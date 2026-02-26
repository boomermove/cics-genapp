# LGACDB01_SECURE.cbl - Add Customer Details (Secure Version)

## 1. Overview

- **Program ID:** LGACDB01 (same program ID as the original, intended as a secure replacement)
- **Purpose:** Secure version of the customer creation program that replaces hardcoded credentials with proper authentication via a dedicated authentication service (LGAUTH01).
- **Summary:** This is a refactored version of LGACDB01 that adds proper security handling for new customer accounts. Instead of passing a hardcoded password hash to LGACDB02, this version generates a temporary password based on the current timestamp, creates a user account through the LGAUTH01 authentication program, and then inserts the customer record into the DB2 CUSTOMER table with explicit SYNCPOINT management for transactional integrity. It uses a structured paragraph numbering scheme (1000/2000/3000/4000/9000) for clarity.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 131-140)

Entry point. Sequentially performs `1000-INIT`, `2000-PROCESS`, and `9000-RETURN`.

### 0000-MAIN (lines 133-140)

Orchestrates the three major phases: initialization, processing, and return.

### 1000-INIT (lines 145-163)

1. Captures EIB fields (`EIBTRNID`, `EIBTRMID`, `EIBTASKN`) into working storage.
2. Sets the commarea pointer and saves commarea length.
3. Validates commarea length is at least 32,767 bytes; returns `'98'` if too short.
4. Validates `CA-REQUEST-ID` equals `'01ACUS'`; returns `'99'` if not a customer-add request.

### 2000-PROCESS (lines 168-186)

1. Links to `LGACVS01` to write the customer to VSAM with the full 32,767-byte commarea.
2. Checks CICS response; if not normal, sets return code `'90'` and writes error.
3. If `CA-RETURN-CODE` is `'00'`, proceeds to create the secure account and insert the DB2 record.

### 3000-CREATE-SECURE-ACCOUNT (lines 191-196)

Orchestrates three sub-paragraphs: generate customer number, generate default password, create user account.

### 3100-GENERATE-CUSTOMER-NUMBER (lines 198-214)

1. Enqueues on `WS-GENACUSTNUM-NAME` to serialize access.
2. Reads the named counter `GENACUSTNUM` via `EXEC CICS READCOUNTER`.
3. Moves the counter value to `CA-CUSTOMER-NUM`.
4. Dequeues the resource.

### 3200-GENERATE-DEFAULT-PASSWORD (lines 216-224)

1. Gets the current absolute time via `EXEC CICS ASKTIME`.
2. Computes a temporary password from the timestamp (using `FUNCTION INTEGER`).
3. Moves it to `WS-DEFAULT-PASSWORD`.

### 3300-CREATE-USER-ACCOUNT (lines 226-259)

1. Populates the `WS-AUTH-REQUEST` structure: function `'CREATEU '`, customer number, generated username (first name + "." + last name), and default password.
2. Links to `LGAUTH01` to create the user account.
3. On failure (`AS-RETURN-CODE` not `'00'`), sets return code `'90'` and writes error.

### 4000-INSERT-CUSTOMER-RECORD (lines 264-311)

1. Moves all customer fields from commarea to DB2 host variables.
2. Executes the INSERT into the CUSTOMER table.
3. On SQL failure: sets return code `'90'`, writes error, performs `SYNCPOINT ROLLBACK`.
4. On success: sets return code `'00'`, performs `SYNCPOINT` (commit).
5. Returns to caller.

### WRITE-ERROR-MESSAGE (lines 316-338)

Formats the timestamp, populates error message fields, and links to `LGSTSQ` to write the error to a temporary storage queue.

### 9000-RETURN (lines 343-346)

Issues `EXEC CICS RETURN`.

### Control Flow

```
MAINLINE -> 0000-MAIN
  -> 1000-INIT (validate commarea)
  -> 2000-PROCESS
       -> LINK LGACVS01 (VSAM write)
       -> 3000-CREATE-SECURE-ACCOUNT
            -> 3100-GENERATE-CUSTOMER-NUMBER (ENQ/READCOUNTER/DEQ)
            -> 3200-GENERATE-DEFAULT-PASSWORD
            -> 3300-CREATE-USER-ACCOUNT (LINK LGAUTH01)
       -> 4000-INSERT-CUSTOMER-RECORD (SQL INSERT + SYNCPOINT)
  -> 9000-RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-HEADER` (line 38) | Group | Debug eyecatcher, EIB values, commarea pointer |
| `WS-PROG` (line 50) | `PIC X(8)` | Program name constant `'LGACDB01'` |
| `LGAC-RETRY-TIMES` (line 51) | `PIC 9` | Retry limit (value 9, declared but not used in the code) |
| `WS-RESP` / `WS-RESP2` (lines 56-57) | `PIC 9(8) COMP` | CICS response codes |
| `WS-ABSTIME` (line 62) | `PIC S9(15) COMP-3` | Absolute time for password generation |
| `ERROR-MSG` (line 69) | Group | Error message with date, time, function, SQLCODE, RESP, RESP2, task number |
| `WS-GENACUSTNUM-NAME` (line 88) | `PIC X(16)` | Named counter name `'GENACUSTNUM'` |
| `WS-GENACUSTNUM-VALUE` / `-D` (lines 89-91) | `PIC 9(10)` / `PIC S9(10) COMP` | Counter value and its COMP redefinition |
| `WS-AUTH-REQUEST` (line 112) | Group | Authentication request copybook structure |
| `WS-AUTH-RESPONSE` (line 114) | Group | Authentication response copybook structure |
| `WS-SECURITY-WORK` (line 117) | Group | Default and temporary password work areas |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 123-124): Defined via `COPY LGCMAREA`. Note: there is a duplicate `COPY LGCMAREA` at line 122 which appears to be a coding error (two 01-level items both trying to use the same copybook).

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGSECUR` (line 33) | `COPY LGSECUR` | Security configuration constants |
| `SQLCA` (line 98) | `EXEC SQL INCLUDE SQLCA` | DB2 SQL communications area |
| `LGPOLICY` (line 99) | `EXEC SQL INCLUDE LGPOLICY` | Policy/customer length definitions and DB2 structures |
| `DB2-CUSTOMER` (lines 105, 107) | `COPY DB2-CUSTOMER` | DB2 customer table host variables (used twice for two record areas) |
| `AUTH-REQUEST` (line 113) | `COPY AUTH-REQUEST` | Authentication request structure (function, customer num, username, password, etc.) |
| `AUTH-RESPONSE` (line 115) | `COPY AUTH-RESPONSE` | Authentication response structure (return code, error message, etc.) |
| `LGCMAREA` (lines 122, 124) | `COPY LGCMAREA` | Commarea field definitions |

### Condition Names (88-levels)

None explicitly defined in this source.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Must be `'01ACUS'` for customer add.
- `CA-FIRST-NAME`, `CA-LAST-NAME`, `CA-DOB`, `CA-HOUSE-NAME`, `CA-HOUSE-NUM`, `CA-POSTCODE`, `CA-PHONE-MOBILE`, `CA-PHONE-HOME`, `CA-EMAIL-ADDRESS`: Customer details.

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'00'` success, `'90'` SQL or auth error, `'98'` short commarea, `'99'` invalid request ID.
- `CA-CUSTOMER-NUM`: Populated with the generated customer number.

### Files Accessed
- **DB2 CUSTOMER table** -- INSERT (line 278)
- **VSAM KSDSCUST** -- via LINK to LGACVS01 (WRITE)

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGACVS01` (line 170) | Write customer to VSAM file |
| `LGAUTH01` (line 245) | Create secure user account with proper authentication |
| `LGSTSQ` (line 333) | Write error messages to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

**INSERT customer (lines 277-298):**
```sql
INSERT INTO CUSTOMER
  ( CUSTOMERNUMBER, FIRSTNAME, LASTNAME, DATEOFBIRTH,
    HOUSENAME, HOUSENUMBER, POSTCODE,
    PHONEMOBILE, PHONEHOME, EMAILADDRESS )
VALUES ( :DB2-CUSTOMERNUM-INT,
         :DB2-FIRSTNAME, :DB2-LASTNAME, :DB2-DATEOFBIRTH,
         :DB2-HOUSENAME, :DB2-HOUSENUMBER, :DB2-POSTCODE,
         :DB2-PHONEMOBILE, :DB2-PHONEHOME, :DB2-EMAILADDRESS )
```

### CICS Commands
- `EXEC CICS RETURN` -- Return to caller
- `EXEC CICS LINK Program('LGACVS01')` -- VSAM customer write
- `EXEC CICS ENQ RESOURCE(WS-GENACUSTNUM-NAME)` -- Serialize counter access
- `EXEC CICS READCOUNTER` -- Read named counter value
- `EXEC CICS DEQ RESOURCE(WS-GENACUSTNUM-NAME)` -- Release counter serialization
- `EXEC CICS ASKTIME` -- Get current time for password generation
- `EXEC CICS LINK Program('LGAUTH01')` -- Create user account
- `EXEC CICS SYNCPOINT` -- Commit DB2 changes on success
- `EXEC CICS SYNCPOINT ROLLBACK` -- Rollback on SQL failure
- `EXEC CICS FORMATTIME` -- Format time for error messages

### Error Handling
- Explicit SYNCPOINT/ROLLBACK around the DB2 INSERT ensures atomicity.
- Each error path sets `CA-RETURN-CODE` and writes an error message to TSQ.
- Authentication failures from LGAUTH01 are captured and logged.
- ENQ/DEQ around the named counter prevents concurrent allocation of the same customer number.

### Notable Logic Patterns
- **Structured paragraph naming:** Uses a numeric scheme (1000, 2000, 3000, 3100, 3200, 3300, 4000, 9000) for clear hierarchical control flow.
- **Dynamic password generation (line 220-221):** Computes a temporary password from the system time using `FUNCTION INTEGER(WS-ABSTIME / 1000000)`. The COMPUTE with string concatenation `'TMP' + ...` is unusual for COBOL and may be a coding error or pseudo-code intention.
- **Username construction (lines 231-235):** Uses `STRING...DELIMITED BY SPACE` to build `firstname.lastname` format.

## 6. Analysis

### Overall Role
This is the secure replacement for LGACDB01. It adds proper authentication handling through LGAUTH01 rather than directly inserting a hardcoded password into CUSTOMER_SECURE. It also adds transactional integrity with SYNCPOINT management and serialized counter access.

### Dependencies
- CICS named counter `GENACUSTNUM`
- DB2 CUSTOMER table
- Programs: LGACVS01, LGAUTH01, LGSTSQ
- Copybooks: LGSECUR, LGPOLICY, DB2-CUSTOMER, AUTH-REQUEST, AUTH-RESPONSE, LGCMAREA, SQLCA

### Known Issues or Limitations
1. **Duplicate COPY LGCMAREA in LINKAGE** (lines 122-124): Two 01-level items both include `COPY LGCMAREA`, which would cause compilation issues -- the first copy would define fields at the 01 level before `DFHCOMMAREA`.
2. **Password generation logic** (line 220-221): The `COMPUTE` statement attempts to concatenate string `'TMP'` with an integer using `+`, which is not valid COBOL arithmetic. This would likely cause a compile error.
3. **`LGAC-RETRY-TIMES`** (line 51) is declared but never referenced.
4. **No call to LGACDB02:** Unlike the original LGACDB01, this version does not call LGACDB02 to insert into CUSTOMER_SECURE; it relies on LGAUTH01 to handle security record creation.
5. **Overwrites DFHCOMMAREA** (line 243): The auth request is moved to DFHCOMMAREA, potentially destroying the original customer data before the DB2 insert at 4000-INSERT-CUSTOMER-RECORD. The program moves from CA-fields (which are in DFHCOMMAREA) to DB2 host variables in paragraph 4000, but by then the commarea has been overwritten with auth request data.

### Relationship to Other Programs
- **Replaces:** LGACDB01 (original version with hardcoded password)
- **Calls:** LGACVS01 (VSAM write), LGAUTH01 (authentication), LGSTSQ (error logging)
- **Called by:** Presumably LGACUS01 or a similar business-logic program
