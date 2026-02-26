# LGAUTH01 - Authentication Service

## 1. Overview

- **Program ID:** LGAUTH01
- **Author:** GENAPP SECURITY TEAM
- **Purpose:** Provides comprehensive user authentication and account management services for the GenApp insurance application.
- **Summary:** LGAUTH01 is a CICS COBOL program that implements a full-featured authentication service with five functions: user authentication (AUTH), password change (CHGPASS), user creation (CREATEU), account locking (LOCKUSER), and account unlocking (UNLKUSER). It uses DB2 for persistent storage of user credentials, password history, and audit logs. Passwords are hashed using SHA-256 (via a companion program LGPWHASH), salted with cryptographically secure random bytes from z/OS ICSF (Integrated Cryptographic Service Facility). The program implements account lockout after failed attempts, password expiry, password history checking, and comprehensive audit logging. Session tokens are generated using ICSF's CSNBRNGL random number generator.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION / 0000-MAIN (lines 192-198)
Entry point that orchestrates three steps:
1. `PERFORM 1000-INITIALIZE`
2. `PERFORM 2000-PROCESS-REQUEST`
3. `PERFORM 9000-RETURN`

#### 1000-INITIALIZE (lines 200-221)
1. Captures EIB transaction/terminal/task info (lines 201-203).
2. Stores COMMAREA address and length (lines 205-206).
3. Validates COMMAREA length against `LENGTH OF AUTH-REQUEST` (line 208). If too short, sets return code `'98'` and jumps to `9000-RETURN`.
4. Copies customer number and username into DB2 host variables (lines 216-217).
5. Clears audit work fields (lines 219-220).

#### 2000-PROCESS-REQUEST (lines 223-247)
EVALUATE TRUE block dispatching on condition names from `AR-FUNCTION`:
- `AR-AUTHENTICATE` -> `3000-AUTHENTICATE-USER`
- `AR-CHANGE-PASSWORD` -> `4000-CHANGE-PASSWORD`
- `AR-CREATE-USER` -> `5000-CREATE-USER`
- `AR-LOCK-USER` -> `6000-LOCK-USER`
- `AR-UNLOCK-USER` -> `7000-UNLOCK-USER`
- `OTHER` -> Sets return code `'99'`, logs invalid function audit.

#### 3000-AUTHENTICATE-USER (lines 252-287)
Authentication flow:
1. Sets audit action to `'USER_LOGIN'`.
2. Calls `3100-GET-USER-SECURITY` to look up the user in DB2.
3. Calls `3200-CHECK-ACCOUNT-STATUS` (locked/suspended/expired checks).
4. Calls `3300-VERIFY-PASSWORD` (hashes input and compares).
5. On success: calls `3400-SUCCESS-PROCESSING` (updates last login, generates session token).
6. On failure: calls `3500-FAILED-PROCESSING` (increments attempt counter, possibly locks account).
7. Logs audit entry.

#### 3100-GET-USER-SECURITY (lines 289-340)
Executes a SELECT from `USER_SECURITY` table to retrieve all user record fields. Looks up by `CUSTOMERNUM` OR `USERNAME`. Handles SQLCODE 100 (not found, returns `'01'`), non-zero (DB2 error, returns `'90'`), and 0 (success).

#### 3200-CHECK-ACCOUNT-STATUS (lines 342-376)
1. If `DB2-ACCOUNT-STATUS = 'L'` (locked): calls `3210-CHECK-LOCKOUT-EXPIRY` to see if the lockout has expired.
2. If `DB2-ACCOUNT-STATUS = 'S'` (suspended): returns `'02'`.
3. Calls `3220-CHECK-PASSWORD-EXPIRY` to check if the password has expired.

#### 3210-CHECK-LOCKOUT-EXPIRY (lines 378-390)
Gets current date/time via CICS ASKTIME/FORMATTIME, compares with `DB2-LOCKOUT-TIME`. If the current date is past the lockout date, auto-unlocks the account via `3211-UNLOCK-ACCOUNT`.

#### 3211-UNLOCK-ACCOUNT (lines 392-416)
Updates `USER_SECURITY` to set status to `'A'` (active), reset login attempts to 0, and clear lockout time.

#### 3220-CHECK-PASSWORD-EXPIRY (lines 418-434)
Computes days since last password change using `FUNCTION INTEGER-OF-DATE`. If greater than `SC-PASSWORD-EXPIRY-DAYS` (90 days), sets `WS-PASSWORD-EXPIRED = 'Y'`. Also computes `AS-DAYS-TO-EXPIRY`.

#### 3300-VERIFY-PASSWORD (lines 436-465)
Calls `LGPWHASH` with function `'VERIFY'`, passing the entered password, stored salt, and stored hash. If LGPWHASH LINK fails, returns `'90'`. If hash verification fails, returns `'01'` (invalid credentials).

#### 3400-SUCCESS-PROCESSING (lines 467-495)
1. Gets current timestamp, formats it.
2. Updates `USER_SECURITY`: sets `LAST_LOGIN`, resets `LOGIN_ATTEMPTS` to 0.
3. Copies previous `DB2-LAST-LOGIN` to `AS-LAST-LOGIN` in the response.
4. Calls `3410-GENERATE-SESSION-TOKEN`.

#### 3410-GENERATE-SESSION-TOKEN (lines 501-535)
Calls z/OS ICSF `CSNBRNGL` (Random Number Generate Long) to produce 48 cryptographically secure random bytes. On success, converts to hex token. On failure, sets return code `'90'`. Then calls `3412-SET-TOKEN-EXPIRY`.

#### 3411-CONVERT-TOKEN-TO-HEX (lines 540-564)
Iterates over the first 32 bytes of ICSF random output, converting each byte to a 2-character hex representation using `FUNCTION ORD`, division by 16, and a hex lookup table. Produces a 64-character hex session token stored in `AS-SESSION-TOKEN`.

#### 3412-SET-TOKEN-EXPIRY (lines 566-580)
Computes the token expiry time by adding `SC-SESSION-TIMEOUT` (3600 seconds, converted to hundredths) to the current ABSTIME. Formats as a timestamp string in `AS-EXPIRY-TIME`.

#### 3500-FAILED-PROCESSING (lines 582-612)
1. Increments `DB2-LOGIN-ATTEMPTS`.
2. Updates `USER_SECURITY` with the new attempt count.
3. Computes remaining attempts (`SC-MAX-LOGIN-ATTEMPTS - DB2-LOGIN-ATTEMPTS`).
4. If attempts >= max (3), calls `3510-LOCK-ACCOUNT`.

#### 3510-LOCK-ACCOUNT (lines 614-647)
Computes a lockout expiry time (current time + `SC-LOCKOUT-DURATION` = 1800 seconds). Updates `USER_SECURITY` with status `'L'` and the lockout timestamp. Sets return code `'02'`.

#### 4000-CHANGE-PASSWORD (lines 652-718)
Password change flow:
1. Gets user security record (`3100-GET-USER-SECURITY`).
2. Verifies current password (`3300-VERIFY-PASSWORD`).
3. Validates new password via `LGPWHASH` with `'VALIDATE'` function.
4. Checks password history (`4050-CHECK-PASSWORD-HISTORY`).
5. Hashes new password (`4100-HASH-NEW-PASSWORD`).
6. Saves current password to history (`4150-SAVE-PASSWORD-HISTORY`).
7. Updates password in DB2 (`4200-UPDATE-PASSWORD`).

#### 4050-CHECK-PASSWORD-HISTORY (lines 723-756)
1. Hashes the new password with the current salt and compares with the current hash.
2. Queries `PASSWORD_HISTORY` table to check if the new password hash matches any of the last `PP-HISTORY-COUNT` (5) entries.

#### 4100-HASH-NEW-PASSWORD (lines 758-775)
Two-step process: first calls LGPWHASH with `'GENSALT'` to generate a new salt, then calls again with `'HASH'` to hash the password with the new salt.

#### 4150-SAVE-PASSWORD-HISTORY (lines 780-805)
1. Increments all existing history sequence numbers by 1.
2. Deletes entries where sequence exceeds `PP-HISTORY-COUNT`.
3. Inserts the current (old) password hash as sequence 1.

#### 4200-UPDATE-PASSWORD (lines 807-833)
Updates `USER_SECURITY` with the new password hash, salt, and current date as `PASSWORD_DATE` and `MODIFIED_DATE`.

#### 5000-CREATE-USER (lines 838-876)
1. Validates the password via LGPWHASH `'VALIDATE'`.
2. Hashes the password (`4100-HASH-NEW-PASSWORD`).
3. Inserts the new user record (`5100-INSERT-USER-RECORD`).

#### 5100-INSERT-USER-RECORD (lines 878-907)
Inserts a row into `USER_SECURITY` with all fields, setting status to `'A'` (active), login attempts to 0, and lockout time to NULL.

#### 6000-LOCK-USER (lines 912-958)
Administrative lock: updates `USER_SECURITY` to set status `'L'` with a lockout timestamp. Logs audit.

#### 7000-UNLOCK-USER (lines 963-1002)
Administrative unlock: updates `USER_SECURITY` to set status `'A'`, reset login attempts to 0, and clear lockout time. Logs audit.

#### 8000-AUDIT-LOG (lines 1007-1050)
Inserts a row into the `AUDIT_LOG` table. Respects `SC-AUDIT-LEVEL`: skips logging if `SC-AUDIT-NONE`, or if `SC-AUDIT-FAIL` and the result is `'00'` (success).

#### WRITE-ERROR-MESSAGE (lines 1055-1076)
Formats error data and calls `LGSTSQ` program to write to a temporary storage queue.

#### 9000-RETURN (lines 1081-1086)
Copies `WS-RETURN-CODE` and `WS-ERROR-MSG` to the response structure (`AS-RETURN-CODE`, `AS-ERROR-MESSAGE`), then issues `EXEC CICS RETURN`.

### Control Flow Summary
```
0000-MAIN
  -> 1000-INITIALIZE (validate commarea, load user identifiers)
  -> 2000-PROCESS-REQUEST
     EVALUATE AR-FUNCTION:
       'AUTH    ' -> 3000-AUTHENTICATE-USER
         -> 3100-GET-USER-SECURITY (DB2 SELECT)
         -> 3200-CHECK-ACCOUNT-STATUS
            -> 3210-CHECK-LOCKOUT-EXPIRY
               -> 3211-UNLOCK-ACCOUNT (auto-unlock if expired)
            -> 3220-CHECK-PASSWORD-EXPIRY
         -> 3300-VERIFY-PASSWORD (LINK LGPWHASH)
         -> 3400-SUCCESS-PROCESSING (update login, generate token)
            -> 3410-GENERATE-SESSION-TOKEN (CALL CSNBRNGL)
               -> 3411-CONVERT-TOKEN-TO-HEX
               -> 3412-SET-TOKEN-EXPIRY
         -> 3500-FAILED-PROCESSING (increment attempts)
            -> 3510-LOCK-ACCOUNT (if max attempts exceeded)
         -> 8000-AUDIT-LOG
       'CHGPASS ' -> 4000-CHANGE-PASSWORD
         -> 3100-GET-USER-SECURITY
         -> 3300-VERIFY-PASSWORD
         -> LGPWHASH VALIDATE new password
         -> 4050-CHECK-PASSWORD-HISTORY
         -> 4100-HASH-NEW-PASSWORD (GENSALT + HASH)
         -> 4150-SAVE-PASSWORD-HISTORY
         -> 4200-UPDATE-PASSWORD
         -> 8000-AUDIT-LOG
       'CREATEU ' -> 5000-CREATE-USER
         -> LGPWHASH VALIDATE
         -> 4100-HASH-NEW-PASSWORD
         -> 5100-INSERT-USER-RECORD
         -> 8000-AUDIT-LOG
       'LOCKUSER' -> 6000-LOCK-USER (UPDATE, audit)
       'UNLKUSER' -> 7000-UNLOCK-USER (UPDATE, audit)
  -> 9000-RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

#### WS-HEADER (line 40)
| Field | PIC | Role |
|---|---|---|
| `WS-EYECATCHER` | X(16) | Value `'LGAUTH01----WS'` for debugging |
| `WS-TRANSID` | X(4) | EIB transaction ID |
| `WS-TERMID` | X(4) | EIB terminal ID |
| `WS-TASKNUM` | 9(7) | EIB task number |
| `WS-ADDR-DFHCOMMAREA` | POINTER | Address of DFHCOMMAREA |
| `WS-CALEN` | S9(8) COMP | COMMAREA length |
| `WS-RESP` | 9(8) COMP | CICS RESP code |
| `WS-RESP2` | 9(8) COMP | CICS RESP2 code |

#### Time/Date Variables (lines 59-62)
| Variable | PIC | Role |
|---|---|---|
| `WS-ABSTIME` | S9(15) COMP-3 | CICS absolute time |
| `WS-FORMATTED-TIME` | X(26) | ISO-style timestamp for DB2 |
| `WS-DATE` | X(10) | Formatted date |
| `WS-TIME` | X(8) | Formatted time |

#### WS-WORK-VARS (lines 67-78)
| Field | PIC | Role |
|---|---|---|
| `WS-RETURN-CODE` | X(2) VALUE '00' | Internal return code |
| `WS-ERROR-MSG` | X(100) | Error message text |
| `WS-CUSTOMER-NUM-CHAR` | X(10) | Customer number as character (unused) |
| `WS-DATE-WORK` | X(10) | Working date field for DB2 updates |
| `WS-ATTEMPTS-REMAINING` | 9(2) | Remaining login attempts |
| `WS-LOCKOUT-EXPIRED` | X(1) VALUE 'N' | Flag: lockout period expired? |
| `WS-PASSWORD-EXPIRED` | X(1) VALUE 'N' | Flag: password expired? |
| `WS-DAYS-SINCE-CHANGE` | 9(3) | Days since last password change |
| `WS-USER-FOUND` | X(1) VALUE 'N' | Flag: user found in DB2? |
| `WS-HISTORY-MATCH` | X(1) VALUE 'N' | Flag: new password matches history? |
| `WS-HISTORY-COUNT` | 9(2) VALUE 0 | Count of history matches |

#### DB2-USER-SECURITY (lines 83-95)
Host variables matching the `USER_SECURITY` table columns: customer number, username, password hash, salt, hash algorithm, password date, last login, login attempts, account status, lockout time, created date, modified date.

#### DB2-PASSWORD-HISTORY (lines 100-104)
Host variables for the `PASSWORD_HISTORY` table: customer number, sequence, password hash, created date.

#### HASH-COMMAREA (lines 109-116)
Communication area for calling `LGPWHASH`: function code (8), password (64), salt (32), hash (128), return code (2), error message (100), filler (598).

#### ICSF-RANDOM-PARMS (lines 122-131)
Parameters for calling `CSNBRNGL`: return code, reason code, exit data, rule array (`'RANDOM'`), seed, output length (48), output buffer (48 bytes).

#### WS-TOKEN-WORK (lines 136-149)
Fields for hex conversion of session token: seed, counter, character lookup table (62 alphanumeric chars), hex table (`'0123456789ABCDEF'`), nibble values, byte index.

#### AUDIT-WORK (lines 154-158)
| Field | PIC | Role |
|---|---|---|
| `AW-ACTION` | X(20) | Audit action name (e.g., `'USER_LOGIN'`) |
| `AW-RESULT` | X(2) | Audit result code |
| `AW-ERROR-CODE` | X(10) | Machine-readable error code |
| `AW-DETAILS` | X(200) | Human-readable audit details |

### LINKAGE SECTION (lines 181-185)
- `COPY LGSECUR` appears twice: once at line 182 (outside DFHCOMMAREA, possibly a data area definition) and once at line 185 (within DFHCOMMAREA). The LGSECUR copybook defines:
  - `SECURITY-CONFIG`: Hash algorithm, salt length, password length limits, expiry days, max attempts, lockout duration, session timeout, audit level.
  - `PASSWORD-POLICY`: Complexity requirements, history count.
  - `AUTH-REQUEST`: Function code (with 88-level conditions), customer number, username, password, new password, client IP, user agent, session ID.
  - `AUTH-RESPONSE`: Return code (with 88-level conditions), session token, expiry time, last login, attempts remaining, days to expiry, error message.
  - `USER-SECURITY-REC`, `PASSWORD-HISTORY`, `AUDIT-LOG-REC`: Record structures.

### Condition Names (88-levels)
From LGSECUR copybook:

**AR-FUNCTION conditions:**
| Name | Value | Meaning |
|---|---|---|
| `AR-AUTHENTICATE` | `'AUTH    '` | Authenticate user |
| `AR-CHANGE-PASSWORD` | `'CHGPASS '` | Change password |
| `AR-CREATE-USER` | `'CREATEU '` | Create new user |
| `AR-LOCK-USER` | `'LOCKUSER'` | Lock user account |
| `AR-UNLOCK-USER` | `'UNLKUSER'` | Unlock user account |

**AS-RETURN-CODE conditions:**
| Name | Value | Meaning |
|---|---|---|
| `AS-SUCCESS` | `'00'` | Operation successful |
| `AS-INVALID-CREDS` | `'01'` | Invalid credentials |
| `AS-ACCOUNT-LOCKED` | `'02'` | Account locked |
| `AS-PASSWORD-EXPIRED` | `'03'` | Password expired |
| `AS-POLICY-VIOLATION` | `'04'` | Password policy violation |
| `AS-SYSTEM-ERROR` | `'90'` | System/DB2 error |
| `AS-NOT-AUTHORIZED` | `'99'` | Not authorized / invalid function |

**SC-AUDIT-LEVEL conditions:**
| Name | Value | Meaning |
|---|---|---|
| `SC-AUDIT-ALL` | `'A'` | Log all events |
| `SC-AUDIT-FAIL` | `'F'` | Log failures only |
| `SC-AUDIT-NONE` | `'N'` | No audit logging |

**USR-ACCOUNT-STATUS conditions:**
| Name | Value | Meaning |
|---|---|---|
| `USR-ACTIVE` | `'A'` | Active account |
| `USR-LOCKED` | `'L'` | Locked account |
| `USR-SUSPENDED` | `'S'` | Suspended account |
| `USR-EXPIRED` | `'E'` | Expired account |

## 4. Input/Output Behavior

### Input (AUTH-REQUEST in COMMAREA)
| Field | PIC | Description |
|---|---|---|
| `AR-FUNCTION` | X(8) | Function to execute |
| `AR-CUSTOMER-NUM` | 9(10) | Customer number |
| `AR-USERNAME` | X(32) | Username |
| `AR-PASSWORD` | X(64) | Current password |
| `AR-NEW-PASSWORD` | X(64) | New password (for CHGPASS) |
| `AR-CLIENT-IP` | X(15) | Client IP address (for audit) |
| `AR-USER-AGENT` | X(100) | Client user agent (for audit) |
| `AR-SESSION-ID` | X(32) | Session ID |

### Output (AUTH-RESPONSE in COMMAREA)
| Field | PIC | Description |
|---|---|---|
| `AS-RETURN-CODE` | X(2) | Result code |
| `AS-SESSION-TOKEN` | X(64) | Generated session token (AUTH only) |
| `AS-EXPIRY-TIME` | X(26) | Token expiry timestamp |
| `AS-LAST-LOGIN` | X(26) | Previous login timestamp |
| `AS-ATTEMPTS-REMAINING` | 9(2) | Remaining login attempts |
| `AS-DAYS-TO-EXPIRY` | 9(3) | Days until password expires |
| `AS-ERROR-MESSAGE` | X(100) | Error description |

### DB2 Tables Accessed
| Table | Operations | Purpose |
|---|---|---|
| `USER_SECURITY` | SELECT, UPDATE, INSERT | User credential and account management |
| `PASSWORD_HISTORY` | SELECT (COUNT), UPDATE, DELETE, INSERT | Password reuse prevention |
| `AUDIT_LOG` | INSERT | Security event audit trail |

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| `LGPWHASH` | EXEC CICS LINK (lines 442, 674, 730, 762, 770, 844) | Password hashing, verification, validation, salt generation |
| `LGSTSQ` | EXEC CICS LINK (line 1071) | Error message logging to TSQ |
| `CSNBRNGL` | CALL (line 507) | z/OS ICSF cryptographic random number generation |

## 5. Algorithms and Techniques

### SQL Statements

**1. SELECT from USER_SECURITY** (lines 290-318):
```sql
SELECT CUSTOMERNUM, USERNAME, PASSWORD_HASH, SALT, HASH_ALGORITHM,
       PASSWORD_DATE, LAST_LOGIN, LOGIN_ATTEMPTS, ACCOUNT_STATUS,
       LOCKOUT_TIME, CREATED_DATE, MODIFIED_DATE
INTO :DB2-CUSTOMERNUM-INT, :DB2-USERNAME, :DB2-PASSWORD-HASH,
     :DB2-SALT, :DB2-HASH-ALGORITHM, :DB2-PASSWORD-DATE,
     :DB2-LAST-LOGIN, :DB2-LOGIN-ATTEMPTS, :DB2-ACCOUNT-STATUS,
     :DB2-LOCKOUT-TIME, :DB2-CREATED-DATE, :DB2-MODIFIED-DATE
FROM USER_SECURITY
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
  OR  USERNAME = :DB2-USERNAME
```

**2. UPDATE USER_SECURITY - auto-unlock** (lines 393-400):
```sql
UPDATE USER_SECURITY
SET ACCOUNT_STATUS = 'A', LOGIN_ATTEMPTS = 0,
    LOCKOUT_TIME = NULL, MODIFIED_DATE = :WS-DATE-WORK
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**3. UPDATE USER_SECURITY - successful login** (lines 481-487):
```sql
UPDATE USER_SECURITY
SET LAST_LOGIN = :WS-FORMATTED-TIME, LOGIN_ATTEMPTS = 0,
    MODIFIED_DATE = :WS-DATE-WORK
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**4. UPDATE USER_SECURITY - failed login** (lines 590-595):
```sql
UPDATE USER_SECURITY
SET LOGIN_ATTEMPTS = :DB2-LOGIN-ATTEMPTS,
    MODIFIED_DATE = :WS-DATE-WORK
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**5. UPDATE USER_SECURITY - lock account** (lines 629-635):
```sql
UPDATE USER_SECURITY
SET ACCOUNT_STATUS = 'L', LOCKOUT_TIME = :WS-FORMATTED-TIME,
    MODIFIED_DATE = :WS-DATE-WORK
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**6. SELECT COUNT from PASSWORD_HISTORY** (lines 741-748):
```sql
SELECT COUNT(*) INTO :WS-HISTORY-COUNT
FROM PASSWORD_HISTORY
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
  AND PASSWORD_HASH = :HASH-HASH
  AND SEQUENCE <= :PP-HISTORY-COUNT
```

**7. UPDATE PASSWORD_HISTORY - increment sequences** (lines 781-785):
```sql
UPDATE PASSWORD_HISTORY
SET SEQUENCE = SEQUENCE + 1
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**8. DELETE from PASSWORD_HISTORY - prune old entries** (lines 787-791):
```sql
DELETE FROM PASSWORD_HISTORY
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
  AND SEQUENCE > :PP-HISTORY-COUNT
```

**9. INSERT into PASSWORD_HISTORY** (lines 798-804):
```sql
INSERT INTO PASSWORD_HISTORY
(CUSTOMERNUM, SEQUENCE, PASSWORD_HASH, CREATED_DATE)
VALUES (:DB2-CUSTOMERNUM-INT, 1, :DB2-PASSWORD-HASH, :WS-DATE-WORK)
```

**10. UPDATE USER_SECURITY - change password** (lines 813-820):
```sql
UPDATE USER_SECURITY
SET PASSWORD_HASH = :HASH-HASH, SALT = :HASH-SALT,
    PASSWORD_DATE = :WS-DATE-WORK, MODIFIED_DATE = :WS-DATE-WORK
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**11. INSERT into USER_SECURITY** (lines 884-894):
```sql
INSERT INTO USER_SECURITY
(CUSTOMERNUM, USERNAME, PASSWORD_HASH, SALT, HASH_ALGORITHM,
 PASSWORD_DATE, LAST_LOGIN, LOGIN_ATTEMPTS, ACCOUNT_STATUS,
 LOCKOUT_TIME, CREATED_DATE, MODIFIED_DATE)
VALUES (:DB2-CUSTOMERNUM-INT, :AR-USERNAME, :HASH-HASH,
        :HASH-SALT, :SC-HASH-ALGORITHM, :WS-DATE-WORK,
        NULL, 0, 'A', NULL, :WS-DATE-WORK, :WS-DATE-WORK)
```

**12. UPDATE USER_SECURITY - admin lock** (lines 928-934):
```sql
UPDATE USER_SECURITY
SET ACCOUNT_STATUS = 'L', LOCKOUT_TIME = :WS-FORMATTED-TIME,
    MODIFIED_DATE = :WS-DATE-WORK
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**13. UPDATE USER_SECURITY - admin unlock** (lines 971-978):
```sql
UPDATE USER_SECURITY
SET ACCOUNT_STATUS = 'A', LOGIN_ATTEMPTS = 0,
    LOCKOUT_TIME = NULL, MODIFIED_DATE = :WS-DATE-WORK
WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
```

**14. INSERT into AUDIT_LOG** (lines 1038-1046):
```sql
INSERT INTO AUDIT_LOG
(TIMESTAMP, CUSTOMER_NUM, USERNAME, ACTION, RESULT,
 CLIENT_IP, USER_AGENT, ERROR_CODE, DETAILS)
VALUES (:AL-TIMESTAMP, :AL-CUSTOMER-NUM, :AL-USERNAME,
        :AL-ACTION, :AL-RESULT, :AL-CLIENT-IP,
        :AL-USER-AGENT, :AL-ERROR-CODE, :AL-DETAILS)
```

### CICS Commands
- `EXEC CICS ASKTIME ABSTIME(WS-ABSTIME)` -- Used extensively to get current time (lines 379, 419, 468, 567, 585, 615, 793, 808, 879, 915, 966, 1016).
- `EXEC CICS FORMATTIME` -- Formats absolute time to various date/time representations (lines 380, 420, 469, 569, 586, 617, 794, 809, 880, 916, 967, 1017, 1059).
- `EXEC CICS LINK Program('LGPWHASH')` -- Calls password hashing service (lines 442, 674, 730, 762, 770, 844).
- `EXEC CICS LINK Program('LGSTSQ')` -- Writes error messages to TSQ (line 1071).
- `EXEC CICS RETURN` -- Returns to caller (line 1085).

### Error Handling
- Return codes propagated through `WS-RETURN-CODE`, copied to `AS-RETURN-CODE` at exit.
- Descriptive error messages stored in `WS-ERROR-MSG` / `AS-ERROR-MESSAGE`.
- Machine-readable error codes in `AW-ERROR-CODE` for audit trail.
- DB2 errors logged via `WRITE-ERROR-MESSAGE` to TSQ.
- LGPWHASH failures detected via `WS-RESP` and `HASH-RETURN-CODE`.

### Notable Logic Patterns
- **Cryptographic session tokens**: Uses z/OS ICSF CSNBRNGL for 48 random bytes, then converts 32 bytes to a 64-char hex string using byte-by-byte conversion with high/low nibble extraction (lines 543-563).
- **Password history rotation**: Uses an incrementing sequence approach -- existing entries get their sequence bumped, entries exceeding the history count are pruned, and the current password is inserted as sequence 1 (lines 781-805).
- **Auto-unlock on lockout expiry**: During authentication, if the account is locked but the lockout period has expired, the account is automatically unlocked (lines 378-416).
- **Deliberate vague error messages**: Returns "Invalid credentials" for both user-not-found and wrong-password cases, preventing user enumeration attacks (lines 322, 460).
- **Configurable audit levels**: Audit logging can be set to ALL, FAIL-only, or NONE via `SC-AUDIT-LEVEL`.

## 6. Analysis

### Overall Role
LGAUTH01 is the central authentication and user management service for the GenApp application. It is architecturally separate from the insurance business logic programs (LGxxxx01 series), providing a dedicated security layer. It represents a significant modernization of the GenApp codebase, adding proper password hashing, account lockout, and audit capabilities.

### Dependencies
- **DB2 tables**: `USER_SECURITY`, `PASSWORD_HISTORY`, `AUDIT_LOG`.
- **Program `LGPWHASH`**: Password hashing service (hash, verify, validate, generate salt).
- **Program `LGSTSQ`**: Error message logging.
- **z/OS ICSF**: `CSNBRNGL` callable service for cryptographic random number generation.
- **Copybook `LGSECUR`**: Defines all security-related data structures, configuration, and request/response formats.

### Known Issues or Limitations
1. **Duplicate COPY LGSECUR**: The LINKAGE SECTION (lines 182 and 185) includes `COPY LGSECUR` twice -- once at the 01 level outside DFHCOMMAREA and once inside DFHCOMMAREA. This creates duplicate data names which could cause ambiguity in some compilers.
2. **OR in user lookup SQL**: The SELECT in `3100-GET-USER-SECURITY` (line 317) uses `WHERE CUSTOMERNUM = ... OR USERNAME = ...`. If the customer number and username belong to different users, this could return the wrong record or SQLCODE -811 (multiple rows).
3. **No transaction management**: The program does not use `EXEC CICS SYNCPOINT` or `ROLLBACK`. If a multi-step operation (like password change) partially fails, DB2's implicit unit of work may leave data in an inconsistent state.
4. **Password expiry check during login**: The password expiry check (section 3220) sets return code `'03'` but does not prevent the authentication from proceeding in all cases -- the check happens before password verification.
5. **WS-CUSTOMER-NUM-CHAR unused**: Declared at line 70 but never referenced.
6. **No CBL CICS SQL option**: Line 1 has `CBL CICS('SP,EDF')` but does not include `SQL` processing, unlike other DB2 programs that use `PROCESS SQL`. The SQL precompiler may need to be invoked separately.
7. **Lockout expiry uses date comparison only**: Line 386 compares `WS-DATE-WORK > DB2-LOCKOUT-TIME(1:10)`, comparing only the date portion (10 chars). The time portion of the lockout is ignored, meaning lockout periods shorter than a day are effectively rounded up to midnight.

### Relationship to Other Programs
- **Called by**: UI/controller programs that need authentication services.
- **Calls**: LGPWHASH (password hashing), LGSTSQ (error logging), CSNBRNGL (ICSF random number generation).
- **Architecturally separate from**: The LGxPOL01/LGxPDB01/LGxPVS01 insurance business logic programs. LGAUTH01 operates on its own set of DB2 tables (USER_SECURITY, PASSWORD_HISTORY, AUDIT_LOG) rather than the POLICY/ENDOWMENT/HOUSE/MOTOR tables.
- **Companion copybook**: LGSECUR defines the complete interface contract for this program.
