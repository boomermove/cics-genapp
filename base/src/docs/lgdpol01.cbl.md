# LGDPOL01 - Delete Policy Business Logic

## 1. Overview

- **Program ID:** LGDPOL01
- **Purpose:** Business logic layer for deleting insurance policy records.
- **Summary:** LGDPOL01 is a CICS COBOL program that serves as the business logic tier for policy delete operations. It validates the COMMAREA, upper-cases the request ID, checks that it is a recognized delete request type, and delegates the actual deletion to LGDPDB01 (the DB2 data access program). The program supports deletion of Endowment, Motor, House, and Commercial policy types.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION (lines 73-131)
1. **Lines 79-83:** Initializes `WS-HEADER` and captures EIB runtime info.
2. **Lines 90-94:** Checks for missing COMMAREA; abends with `'LGCA'` if `EIBCALEN` is zero.
3. **Lines 97-99:** Sets return code to `'00'`, saves EIBCALEN, stores COMMAREA address.
4. **Lines 102-105:** Checks COMMAREA minimum length against `WS-CA-HEADER-LEN` (28 bytes). Returns `'98'` if too short.
5. **Line 112:** Upper-cases `CA-REQUEST-ID` using `FUNCTION UPPER-CASE`.
6. **Lines 114-125:** Validates `CA-REQUEST-ID`:
   - If NOT one of `'01DEND'`, `'01DMOT'`, `'01DHOU'`, `'01DCOM'`, sets return code `'99'`.
   - Otherwise, performs `DELETE-POLICY-DB2-INFO`, then checks `CA-RETURN-CODE` -- if greater than 0, returns immediately (error from LGDPDB01).
7. **Line 128:** Returns to caller.

#### DELETE-POLICY-DB2-INFO (lines 134-141)
Issues `EXEC CICS LINK PROGRAM(LGDPDB01) Commarea(DFHCOMMAREA) LENGTH(32500)` to delegate to the DB2 data access program.

#### WRITE-ERROR-MESSAGE (lines 148-156)
Formats time/date and writes error message and commarea to TSQ via common error handling.

### Control Flow Summary
```
MAINLINE
  -> Initialize
  -> Validate COMMAREA present
  -> Validate COMMAREA length >= 28
  -> Upper-case CA-REQUEST-ID
  -> Validate request ID is a known delete type
     -> If unknown: return code '99'
     -> If known: DELETE-POLICY-DB2-INFO
        -> LINK to LGDPDB01
        -> If error: RETURN immediately
  -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | PIC | Role |
|---|---|---|
| `WS-HEADER` (line 23) | Group | Runtime debug info with eyecatcher `'LGDPOL01------WS'` |
| `WS-TRANSID` (line 25) | X(4) | EIB transaction ID |
| `WS-TERMID` (line 26) | X(4) | EIB terminal ID |
| `WS-TASKNUM` (line 27) | 9(7) | EIB task number |
| `WS-ADDR-DFHCOMMAREA` (line 30) | POINTER | Address of DFHCOMMAREA |
| `WS-CALEN` (line 31) | S9(4) COMP | Saved EIBCALEN |
| `WS-CA-HEADER-LEN` (line 52) | S9(4) COMP VALUE +28 | Minimum COMMAREA length |
| `LGDPDB01` (line 54) | X(8) VALUE 'LGDPDB01' | Name of DB2 data access program |

### ERROR-MSG Structure (lines 39-44)
Contains: date, time, program name `LGDPOL01`, and a 21-character variable message field.

### LINKAGE SECTION
- **DFHCOMMAREA** (line 63): Uses `COPY LGCMAREA`. Comment on lines 61-62 notes the need to allow for max CICS COMMAREA size of 32500 bytes.

### Copybooks Included
| Copybook | Location | Purpose |
|---|---|---|
| `LGERR` (line 36) | WORKING-STORAGE | Common error handling fields |
| `LGCMAREA` (line 64) | LINKAGE SECTION | COMMAREA structure |
| `LGERRPRC` (line 161) | PROCEDURE DIVISION | Common error handling procedures |

### Condition Names (88-levels)
None defined directly in this program.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Must be one of `'01DEND'`, `'01DMOT'`, `'01DHOU'`, `'01DCOM'` (case-insensitive due to upper-casing on line 112).
- `CA-CUSTOMER-NUM`, `CA-POLICY-NUM`: Passed through to LGDPDB01.
- `EIBCALEN`: Must be at least 28 bytes.

### Output (COMMAREA)
- `CA-RETURN-CODE`:
  - `'00'`: Success.
  - `'98'`: COMMAREA too short.
  - `'99'`: Unrecognized request ID.
  - Other codes from LGDPDB01 (`'90'` for DB2 error).

### Files Accessed
None directly. All data access is delegated to LGDPDB01.

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| LGDPDB01 | EXEC CICS LINK (line 136) | DB2 data access for policy deletion |
| LGSTSQ | EXEC CICS LINK (via LGERRPRC) | Error message logging |

## 5. Algorithms and Techniques

### SQL Statements
None. This is a business logic program with no direct DB2 access.

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` (line 93): Abend on missing COMMAREA.
- `EXEC CICS RETURN` (lines 104, 123, 128): Return to caller at various points.
- `EXEC CICS LINK PROGRAM(LGDPDB01) Commarea(DFHCOMMAREA) LENGTH(32500)` (line 136): Delegate to DB2 layer.

### Error Handling
- Missing COMMAREA: Writes error, abends.
- Short COMMAREA: Returns `'98'`.
- Invalid request: Returns `'99'`.
- DB2 errors: Detected by checking `CA-RETURN-CODE > 0` after the LINK; returns immediately.

### Notable Logic Patterns
- **Upper-case conversion**: Line 112 uses `FUNCTION UPPER-CASE(CA-REQUEST-ID)` to normalize the input, making the validation case-insensitive. This is unique among the GenApp programs -- the update programs do not upper-case the request ID.
- **Early return on error**: After calling LGDPDB01, if `CA-RETURN-CODE > 0`, the program returns immediately (line 123) rather than falling through.
- **Compound IF condition**: The request ID validation uses a single compound IF with four NOT EQUAL conditions (lines 114-117) rather than an EVALUATE statement.

## 6. Analysis

### Overall Role
LGDPOL01 is the business logic layer in a three-tier delete chain:
```
Caller -> LGDPOL01 (validation/routing) -> LGDPDB01 (DB2 delete) -> LGDPVS01 (VSAM delete)
```

### Dependencies
- **Program LGDPDB01**: Required for DB2 operations.
- **Program LGSTSQ**: Required for error logging.
- **Copybooks**: LGCMAREA, LGERR, LGERRPRC.

### Known Issues or Limitations
1. **Minimal validation**: Unlike LGUPOL01, this program does not validate COMMAREA length beyond the 28-byte header. Since delete operations only need the header fields (request ID, customer number, policy number), this is appropriate.
2. **No customer/policy number validation**: The program does not verify that customer and policy numbers are non-zero or otherwise valid.
3. **Return code check uses '> 0'**: Line 122 uses `CA-RETURN-CODE > 0` which works because `CA-RETURN-CODE` is `PIC 9(2)`. However, if the return code were ever set to a non-numeric value, this could cause issues.

### Relationship to Other Programs
- **Called by**: Higher-level programs (likely controller/UI programs handling delete requests).
- **Calls**: LGDPDB01 (DB2 delete), LGSTSQ (error logging).
- **Sibling programs**: LGIPOL01 (Inquire), LGAPOL01 (Add), LGUPOL01 (Update) -- all policy business logic programs.
