# LGUCUS01 - Update Customer (Business Logic)

## 1. Overview

- **Program ID:** LGUCUS01
- **Purpose:** Business-logic layer for customer update operations.
- **Summary:** LGUCUS01 is the business-logic orchestrator for customer updates. It receives a COMMAREA from a front-end or service interface, validates it (including checking the request ID), and delegates the actual DB2 update to the data-access program LGUCDB01 via `EXEC CICS LINK`. This program contains no SQL statements itself. It adds a request-ID validation gate (`'01UCUS'`) that is not present in the inquiry business-logic programs.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 78-118)

1. **Initialization** (lines 84-88): Initializes `WS-HEADER` and captures CICS runtime context (`EIBTRNID`, `EIBTRMID`, `EIBTASKN`).
2. **COMMAREA validation** (lines 94-98): If `EIBCALEN` equals zero, logs an error and issues `EXEC CICS ABEND ABCODE('LGCA') NODUMP`.
3. **Return code initialization** (lines 100-103): Sets `CA-RETURN-CODE` to `'00'`, `CA-NUM-POLICIES` to `'00'`, saves COMMAREA length and address.
4. **Request ID validation** (lines 105-108): Checks that `CA-REQUEST-ID` equals `'01UCUS'`. If not, sets return code `'99'` and jumps to `END-PROGRAM` via `GO TO`.
5. **Delegation** (line 111): Performs `UPDATE-CUSTOMER-INFO`.

### END-PROGRAM (lines 114-115)

Issues `EXEC CICS RETURN`.

### UPDATE-CUSTOMER-INFO (lines 121-128)

Executes `EXEC CICS LINK Program(LGUCDB01) Commarea(DFHCOMMAREA) LENGTH(32500)` to invoke the DB2 data-access program.

### WRITE-ERROR-MESSAGE (lines 134-142)

Formats time/date and writes error message and COMMAREA to TSQ via common error handling routines.

### Control Flow

```
MAINLINE -> validate COMMAREA -> check request ID
         -> UPDATE-CUSTOMER-INFO (LINK to LGUCDB01) -> END-PROGRAM (RETURN)

         (if invalid request ID) -> END-PROGRAM (RETURN with '99')
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-EYECATCHER` | X(16) | `'LGUCUS01------WS'` for dump identification |
| `WS-TRANSID` | X(4) | CICS transaction ID |
| `WS-TERMID` | X(4) | CICS terminal ID |
| `WS-TASKNUM` | 9(7) | CICS task number |
| `WS-ADDR-DFHCOMMAREA` | POINTER | COMMAREA address |
| `WS-CALEN` | S9(4) COMP | Saved EIBCALEN |
| `WS-CA-HEADER-LEN` | S9(4) COMP | Header length = 28 (line 52) -- note: different name from other programs |
| `WS-REQUIRED-CA-LEN` | S9(4) COMP | Calculated minimum length (not used) |
| `WS-VARY-FIELD` (level 49) | group | VARCHAR working area: `WS-VARY-LEN` (S9(4) COMP) + `WS-VARY-CHAR` (X(3900)) |
| `LGUCDB01` | X(8) | Program name literal `'LGUCDB01'` (line 61) |

### ERROR-MSG Structure (lines 39-44)

Simple error message with date, time, program name `' LGUCUS01'`, and a 21-character variable area.

### LINKAGE SECTION

- `DFHCOMMAREA`: Via `COPY LGCMAREA` (line 69).

### Copybooks Included

| Copybook | Method | Purpose |
|---|---|---|
| `LGERR` | `COPY LGERR` (line 36) | Common error handling working-storage |
| `LGCMAREA` | `COPY LGCMAREA` (line 69) | COMMAREA structure |
| `LGERRPRC` | `COPY LGERRPRC` (line 147) | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input

- **DFHCOMMAREA fields consumed:**
  - `CA-REQUEST-ID` (X(6)): Must be `'01UCUS'` for the request to be processed.
  - `EIBCALEN`: COMMAREA length (checked for zero).
  - All customer detail fields are passed through to LGUCDB01.

### Output

- **DFHCOMMAREA fields modified:**
  - `CA-RETURN-CODE`: `'00'` initially; `'99'` if request ID is invalid; other codes set by LGUCDB01.
  - `CA-NUM-POLICIES`: Initialized to `'00'`.

- **Return codes (set by this program):**
  - `'99'`: Invalid request ID (not `'01UCUS'`).
  - Other return codes come from LGUCDB01.

### Files/Tables Accessed

None directly. All DB2 access is delegated to LGUCDB01.

### Other Programs Called

| Program | Method | Purpose |
|---|---|---|
| LGUCDB01 | `EXEC CICS LINK` (line 123) | Execute DB2 UPDATE and VSAM sync |
| LGSTSQ | `EXEC CICS LINK` (via LGERRPRC) | Error logging to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

None. This program contains no SQL.

### CICS Commands

| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ABEND ABCODE('LGCA') NODUMP` | 97 | Terminate on missing COMMAREA |
| `EXEC CICS LINK Program(LGUCDB01)` | 123-126 | Delegate to DB2 data-access program |
| `EXEC CICS RETURN` | 115 | Return to caller |

### Error Handling Approach

- Missing COMMAREA: ABEND `'LGCA'`.
- Invalid request ID: Return code `'99'`, skip processing via `GO TO END-PROGRAM`.
- All other error handling is delegated to LGUCDB01.

### Notable Logic Patterns

- **Request ID gate** (lines 105-108): Unlike the inquiry business-logic programs (LGICUS01, LGIPOL01) which do not validate the request ID, LGUCUS01 explicitly checks for `'01UCUS'`. This provides an additional security/validation layer for update operations.
- **GO TO usage** (line 107): Uses `GO TO END-PROGRAM` to skip processing on invalid request ID, which is an older COBOL pattern. The inquiry programs use `EXEC CICS RETURN` directly instead.
- **WS-VARY-FIELD** (lines 57-59): A level-49 VARCHAR structure is defined (3900 bytes) but never used in this program. It may have been included for potential future use with VARCHAR data updates.
- **No COMMAREA length check** beyond zero: Unlike LGICUS01 which checks minimum length, LGUCUS01 relies on LGUCDB01 for that.

## 6. Analysis

### Overall Role

LGUCUS01 is the business-logic tier in the GenApp customer update flow. It sits between the front-end and the data-access layer (LGUCDB01), adding request-ID validation as a processing gate. This is more restrictive than the inquiry programs, reflecting the higher-risk nature of update operations.

### Dependencies

- **LGUCDB01** must be available as a CICS program.
- **LGSTSQ** for error logging.
- **Copybooks:** LGERR, LGERRPRC, LGCMAREA.
- Note: Unlike most other programs, LGUCUS01 does not include the LGPOLICY copybook.

### Known Issues or Limitations

- The `LGUCDB01` variable (line 61) is defined but the LINK uses the name as a literal in the Program() parameter.
- `WS-CA-HEADER-LEN` (line 52) is defined as 28 (different from the 18 used in LGICUS01) but is never actually used for any length check.
- `WS-REQUIRED-CA-LEN` (line 53) is defined but never populated or checked.
- `WS-VARY-FIELD` (lines 57-59) is defined but never used.
- No minimum COMMAREA length validation beyond the zero check.
- The `CA-NUM-POLICIES` field is initialized to `'00'` (line 101), which is carried over from the inquiry pattern but is not meaningful for an update operation.
- Uses `GO TO` (line 107), which is generally considered poor style in structured COBOL.

### Relationship to Other Programs

- **Called by:** Front-end screen handlers or service interfaces that need to update customer data.
- **Calls:** LGUCDB01 (DB2 data access + VSAM sync), LGSTSQ (error logging).
- **Parallel programs:** LGICUS01 (customer inquire BL), LGIPOL01 (policy inquire BL) follow the same delegation pattern without the request-ID gate.
- **Downstream chain:** LGUCUS01 -> LGUCDB01 -> LGUCVS01 (three programs involved in a customer update).
