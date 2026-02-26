# LGIPOL01 - Inquire Policy (Business Logic)

## 1. Overview

- **Program ID:** LGIPOL01
- **Purpose:** Business-logic layer for policy inquiry operations.
- **Summary:** LGIPOL01 is the business-logic orchestrator for all policy inquiries. It receives a COMMAREA from a front-end or service interface, validates it, and delegates the actual DB2 data retrieval to the data-access program LGIPDB01 via `EXEC CICS LINK`. The program itself contains no SQL or policy-type routing logic; all of that complexity resides in LGIPDB01. LGIPOL01's role is to validate the COMMAREA, initialize return codes, and pass the request through.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 65-94)

1. **Initialization** (lines 67-71): Initializes `WS-HEADER` and captures CICS runtime context.
2. **COMMAREA validation** (lines 74-78): If `EIBCALEN` equals zero, logs an error and issues `EXEC CICS ABEND ABCODE('LGCA') NODUMP`.
3. **Return code initialization** (lines 81-83): Sets `CA-RETURN-CODE` to `'00'`, saves COMMAREA length and address.
4. **Delegation** (lines 86-89): Executes `EXEC CICS LINK Program(LGIPDB01) Commarea(DFHCOMMAREA) Length(32500)` to invoke the DB2 data-access program.
5. **Return** (line 91): Issues `EXEC CICS RETURN`.

### MAINLINE-EXIT (lines 93-94)

Standard exit paragraph.

### WRITE-ERROR-MESSAGE (lines 101-109)

Formats time/date and writes error message and COMMAREA to TSQ via common error handling routines.

### Control Flow

```
MAINLINE -> validate COMMAREA -> LINK to LGIPDB01 -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-EYECATCHER` | X(16) | `'LGIPOL01------WS'` for dump identification |
| `WS-TRANSID` | X(4) | CICS transaction ID |
| `WS-TERMID` | X(4) | CICS terminal ID |
| `WS-TASKNUM` | 9(7) | CICS task number |
| `WS-ADDR-DFHCOMMAREA` | POINTER | COMMAREA address |
| `WS-CALEN` | S9(4) COMP | Saved EIBCALEN |
| `LGIPDB01` | X(8) | Program name literal `'LGIPDB01'` (line 48) |

### ERROR-MSG Structure (lines 41-46)

Simple error message with date, time, program name `' LGIPOL01'`, and a 21-character variable area.

### LINKAGE SECTION

- `DFHCOMMAREA`: Via `COPY LGCMAREA` (line 58).

### Copybooks Included

| Copybook | Method | Purpose |
|---|---|---|
| `LGERR` | `COPY LGERR` (line 38) | Common error handling working-storage |
| `LGPOLICY` | `COPY LGPOLICY` (line 50) | Policy length constants and DB2 structures |
| `LGCMAREA` | `COPY LGCMAREA` (line 58) | COMMAREA structure |
| `LGERRPRC` | `COPY LGERRPRC` (line 114) | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input

- **DFHCOMMAREA fields consumed:**
  - `EIBCALEN`: COMMAREA length (checked for zero).
  - All policy-related fields are passed through to LGIPDB01 without inspection.

### Output

- **DFHCOMMAREA fields modified:**
  - `CA-RETURN-CODE`: Initialized to `'00'`; modified by LGIPDB01 upon return.
  - All other modifications are made by LGIPDB01.

### Files/Tables Accessed

None directly. All DB2 access is delegated to LGIPDB01.

### Other Programs Called

| Program | Method | Purpose |
|---|---|---|
| LGIPDB01 | `EXEC CICS LINK` (line 86) | Retrieve policy details from DB2 |
| LGSTSQ | `EXEC CICS LINK` (via LGERRPRC) | Error logging to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

None. This program contains no SQL.

### CICS Commands

| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ABEND ABCODE('LGCA') NODUMP` | 77 | Terminate on missing COMMAREA |
| `EXEC CICS LINK Program(LGIPDB01)` | 86-89 | Delegate to DB2 data-access program |
| `EXEC CICS RETURN` | 91 | Return to caller |

### Error Handling Approach

- Missing COMMAREA triggers ABEND `'LGCA'` with error logging.
- No COMMAREA length check is performed beyond the zero check; length validation is left to LGIPDB01.
- Error messages are logged via the common LGERR/LGERRPRC copybook routines.

### Notable Logic Patterns

- This is the simplest business-logic program in the GenApp system. Unlike LGICUS01 (which checks minimum COMMAREA length) or LGUCUS01 (which validates the request ID), LGIPOL01 performs only a zero-length check before delegating.
- The LENGTH parameter on the LINK is hardcoded to 32500.
- The working-storage variable `LGIPDB01` (line 48) is defined but the LINK uses the literal in the Program() parameter.

## 6. Analysis

### Overall Role

LGIPOL01 is the business-logic tier in the GenApp policy inquiry flow. It is the thinnest of the business-logic programs, acting almost purely as a pass-through to the data-access layer. Its primary value is as an architectural layer that separates the front-end from the data-access program and provides a consistent entry point.

### Dependencies

- **LGIPDB01** must be available as a CICS program.
- **LGSTSQ** for error logging.
- **Copybooks:** LGERR, LGERRPRC, LGPOLICY, LGCMAREA.

### Known Issues or Limitations

- No COMMAREA length validation (beyond zero check). If the COMMAREA is too short, the error will only be caught by LGIPDB01, making debugging slightly harder.
- No request ID validation. Invalid request IDs are passed through to LGIPDB01, which returns code `'99'`.
- The `LGIPDB01` variable (line 48) is unused in the actual LINK call.

### Relationship to Other Programs

- **Called by:** Front-end screen handlers or service interfaces that need policy data.
- **Calls:** LGIPDB01 (DB2 data access), LGSTSQ (error logging).
- **Parallel programs:** LGICUS01 (customer inquire BL), LGUCUS01 (customer update BL) follow the same pattern.
- **Sibling data-access programs:** LGICDB01 (customer inquire), LGUCDB01 (customer update).
