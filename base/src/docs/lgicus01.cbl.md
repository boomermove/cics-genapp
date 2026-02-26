# LGICUS01 - Inquire Customer (Business Logic)

## 1. Overview

- **Program ID:** LGICUS01
- **Purpose:** Business-logic layer for customer inquiry operations.
- **Summary:** LGICUS01 acts as the business-logic orchestrator for customer inquiries. It receives a COMMAREA from a front-end or calling program, validates it, initializes return fields, and then delegates the actual DB2 data retrieval to the data-access program LGICDB01 via `EXEC CICS LINK`. It does not contain any SQL statements itself. Its primary responsibilities are COMMAREA validation, error handling, and routing to the appropriate data-access program.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 73-114)

1. **Initialization** (lines 75-79): Initializes `WS-HEADER` and captures CICS runtime context (`EIBTRNID`, `EIBTRMID`, `EIBTASKN`).
2. **COMMAREA validation** (lines 83-87): If `EIBCALEN` equals zero, logs an error message and issues `EXEC CICS ABEND ABCODE('LGCA') NODUMP`.
3. **Return code initialization** (lines 89-92): Sets `CA-RETURN-CODE` to `'00'`, `CA-NUM-POLICIES` to `'00'`, saves COMMAREA length and address.
4. **COMMAREA length check** (lines 98-103): Computes minimum required length as `WS-CUSTOMER-LEN + WS-CA-HEADERTRAILER-LEN`. If `EIBCALEN` is less than this, sets return code `'98'` and returns.
5. **Delegation** (line 105): Performs `GET-CUSTOMER-INFO`.

### MAINLINE-END (lines 110-111)

Issues `EXEC CICS RETURN` to pass control back to the caller.

### GET-CUSTOMER-INFO (lines 116-124)

Executes `EXEC CICS LINK Program(LGICDB01) Commarea(DFHCOMMAREA) LENGTH(32500)` to invoke the DB2 data-access program. The entire COMMAREA is passed through so LGICDB01 can populate customer fields directly.

### WRITE-ERROR-MESSAGE (lines 130-138)

1. Calls `LGERR-FORMAT-TIME` to get current date/time.
2. Moves formatted date/time into `EM-DATE` and `EM-TIME`.
3. Calls `LGERR-WRITE-MSG` and `LGERR-LOG-COMMAREA` to log the error.

### Control Flow

```
MAINLINE -> validate COMMAREA -> check length -> GET-CUSTOMER-INFO (LINK to LGICDB01) -> MAINLINE-END (RETURN)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-EYECATCHER` | X(16) | `'LGICUS01------WS'` for dump identification |
| `WS-TRANSID` | X(4) | CICS transaction ID |
| `WS-TERMID` | X(4) | CICS terminal ID |
| `WS-TASKNUM` | 9(7) | CICS task number |
| `WS-ADDR-DFHCOMMAREA` | POINTER | Address of DFHCOMMAREA |
| `WS-CALEN` | S9(4) COMP | Saved EIBCALEN |
| `LGICDB01` | X(8) | Program name literal `'LGICDB01'` (line 48) -- defined but not referenced by this name in the LINK |
| `WS-CA-HEADERTRAILER-LEN` | S9(4) COMP | Fixed overhead = 18 bytes |
| `WS-REQUIRED-CA-LEN` | S9(4) | Calculated minimum length |

### ERROR-MSG Structure (lines 41-46)

Simple error message with date, time, program identifier `' LGICUS01'`, and a 21-character variable message area.

### LINKAGE SECTION

- `DFHCOMMAREA`: Mapped via `COPY LGCMAREA` (line 65), providing `CA-REQUEST-ID`, `CA-RETURN-CODE`, `CA-CUSTOMER-NUM`, and all customer/policy sub-structures.

### Copybooks Included

| Copybook | Method | Purpose |
|---|---|---|
| `LGERR` | `COPY LGERR` (line 38) | Common error handling working-storage fields |
| `LGPOLICY` | `COPY LGPOLICY` (line 56) | Policy length constants and DB2 structures |
| `LGCMAREA` | `COPY LGCMAREA` (line 65) | COMMAREA structure for DFHCOMMAREA |
| `LGERRPRC` | `COPY LGERRPRC` (line 143) | Common error handling procedures |

### Condition Names (88-levels)

None defined in this program.

## 4. Input/Output Behavior

### Input

- **DFHCOMMAREA fields consumed:**
  - `CA-CUSTOMER-NUM` (PIC 9(10)): Customer number (passed through to LGICDB01).
  - `EIBCALEN`: COMMAREA length.

### Output

- **DFHCOMMAREA fields modified:**
  - `CA-RETURN-CODE`: Set to `'00'` initially; may be changed to `'98'` if COMMAREA is too short. Further values set by LGICDB01.
  - `CA-NUM-POLICIES`: Initialized to `'00'`.
  - All customer detail fields are populated by LGICDB01 upon return.

- **Return codes (set by this program):**
  - `'98'`: COMMAREA too short.
  - Other return codes come from LGICDB01.

### Files/Tables Accessed

None directly. All DB2 access is delegated to LGICDB01.

### Other Programs Called

| Program | Method | Purpose |
|---|---|---|
| LGICDB01 | `EXEC CICS LINK` (line 118) | Retrieve customer details from DB2 |
| LGSTSQ | `EXEC CICS LINK` (via LGERRPRC) | Write error messages to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

None. This program contains no SQL.

### CICS Commands

| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ABEND ABCODE('LGCA') NODUMP` | 86 | Terminate when no COMMAREA received |
| `EXEC CICS RETURN` | 102, 111 | Return to caller |
| `EXEC CICS LINK Program(LGICDB01)` | 118-121 | Delegate to DB2 data-access program |
| `EXEC CICS ASKTIME` | (via LGERRPRC) | Get current time for error logging |
| `EXEC CICS FORMATTIME` | (via LGERRPRC) | Format date/time for error logging |
| `EXEC CICS LINK PROGRAM(WS-ERR-LOG-PGM)` | (via LGERRPRC) | Call LGSTSQ for error logging |

### Error Handling Approach

- Missing COMMAREA triggers ABEND `'LGCA'`.
- Short COMMAREA returns code `'98'` without calling the data-access program.
- The `WRITE-ERROR-MESSAGE` paragraph delegates to the common LGERR/LGERRPRC copybook routines.

### Notable Logic Patterns

- The LENGTH parameter on the LINK to LGICDB01 is hardcoded to 32500 (line 120), which matches the maximum COMMAREA size defined in LGCMAREA.
- The working-storage variable `LGICDB01` (line 48) is defined as a PIC X(8) literal but the LINK command uses the program name as a literal string in the LINK statement rather than referencing this variable.

## 6. Analysis

### Overall Role

LGICUS01 is the business-logic tier in the GenApp customer inquiry flow. It sits between the front-end (screen handler or service interface) and the data-access layer (LGICDB01). Its role is to validate inputs and orchestrate the call to the data-access program. This follows the standard GenApp three-tier pattern: presentation -> business logic -> data access.

### Dependencies

- **LGICDB01** must be available as a CICS program for the LINK call.
- **LGSTSQ** must be available for error logging.
- **Copybooks:** LGERR, LGERRPRC, LGPOLICY, LGCMAREA.
- Caller must provide a COMMAREA of at least 90 bytes.

### Known Issues or Limitations

- The program defines a working-storage variable `LGICDB01` (line 48) with value `'LGICDB01'` but does not use it in the `EXEC CICS LINK` statement, where the program name is specified as a literal. This is inconsistent but functionally harmless.
- `CA-NUM-POLICIES` is initialized to `'00'` (line 90) even though this is a customer inquiry, not a policy listing. This field is part of the shared COMMAREA and is being defensively cleared.
- No request-ID validation is performed (unlike LGUCUS01 which checks for `'01UCUS'`).

### Relationship to Other Programs

- **Called by:** Front-end programs or service interfaces that need customer data.
- **Calls:** LGICDB01 (DB2 data access), LGSTSQ (error logging).
- **Parallel programs:** LGIPOL01 (policy inquire business logic), LGUCUS01 (customer update business logic) follow the same delegation pattern.
