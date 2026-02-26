# LGUPOL01 - Update Policy Business Logic

## 1. Overview

- **Program ID:** LGUPOL01
- **Purpose:** Business logic layer for updating insurance policy records (Endowment, House, and Motor types).
- **Summary:** LGUPOL01 is a CICS COBOL program that serves as the business logic tier for policy update operations. It validates the incoming COMMAREA, checks which policy type is being updated based on the `CA-REQUEST-ID` field, validates that the COMMAREA is large enough for that policy type, and then delegates the actual data update to LGUPDB01 (the DB2 data access program). The program acts as a gatekeeper, ensuring data integrity before any database operations occur.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION (lines 78-145)
1. **Lines 84-88:** Initializes `WS-HEADER` and captures EIB runtime info (transaction ID, terminal ID, task number).
2. **Lines 94-98:** Checks for missing COMMAREA. If `EIBCALEN` is zero, writes an error message and abends with code `'LGCA'`.
3. **Lines 100-102:** Sets `CA-RETURN-CODE` to `'00'`, saves EIBCALEN, stores COMMAREA address.
4. **Lines 108-136:** EVALUATE block that dispatches on `CA-REQUEST-ID`:
   - `'01UEND'` (Endowment): Required length = 28 (header) + 124 (endowment) = 152 bytes.
   - `'01UHOU'` (House): Required length = 28 (header) + 130 (house) = 158 bytes.
   - `'01UMOT'` (Motor): Required length = 28 (header) + 137 (motor) = 165 bytes.
   - `OTHER`: Sets return code `'99'` (unrecognized request).
   - For each recognized type, if EIBCALEN is less than the required length, sets return code `'98'` and returns immediately.
5. **Line 138:** Performs `UPDATE-POLICY-DB2-INFO` to call the DB2 layer.
6. **Lines 141-142:** Returns to caller.

#### UPDATE-POLICY-DB2-INFO (lines 150-157)
Issues `EXEC CICS LINK Program(LGUPDB01) Commarea(DFHCOMMAREA) LENGTH(32500)` to delegate to the DB2 data access program.

#### WRITE-ERROR-MESSAGE (lines 163-171)
Formats time/date and writes error message and commarea to TSQ using common error handling procedures.

### Control Flow Summary
```
MAINLINE
  -> Initialize
  -> Validate COMMAREA present
  -> EVALUATE CA-REQUEST-ID:
     '01UEND' -> Check length >= 152
     '01UHOU' -> Check length >= 158
     '01UMOT' -> Check length >= 165
     OTHER    -> Return code '99'
  -> UPDATE-POLICY-DB2-INFO
     -> LINK to LGUPDB01
  -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | PIC | Role |
|---|---|---|
| `WS-HEADER` (line 23) | Group | Runtime debug info with eyecatcher `'LGUPOL01------WS'` |
| `WS-TRANSID` (line 25) | X(4) | EIB transaction ID |
| `WS-TERMID` (line 26) | X(4) | EIB terminal ID |
| `WS-TASKNUM` (line 27) | 9(7) | EIB task number |
| `WS-ADDR-DFHCOMMAREA` (line 30) | POINTER | Address of DFHCOMMAREA |
| `WS-CALEN` (line 31) | S9(4) COMP | Saved EIBCALEN value |
| `WS-POLICY-LENGTHS` (line 38) | Group | Expected data lengths per policy type |
| `WS-FULL-ENDOW-LEN` (line 39) | S9(4) COMP VALUE +124 | Endowment policy data length |
| `WS-FULL-HOUSE-LEN` (line 40) | S9(4) COMP VALUE +130 | House policy data length |
| `WS-FULL-MOTOR-LEN` (line 41) | S9(4) COMP VALUE +137 | Motor policy data length |
| `WS-COMMAREA-LENGTHS` (line 56) | Group | COMMAREA header length (28) and required length accumulator |
| `LGUPDB01` (line 60) | X(8) VALUE 'LGUPDB01' | Name of DB2 data access program |

### ERROR-MSG Structure (lines 44-49)
Simpler than the DB2 program's version -- contains date, time, program name `LGUPOL01`, and a 21-character variable message field.

### LINKAGE SECTION
- **DFHCOMMAREA** (line 68): Uses `COPY LGCMAREA`. Standard GenApp COMMAREA.

### Copybooks Included
| Copybook | Location | Purpose |
|---|---|---|
| `LGERR` (line 36) | WORKING-STORAGE | Common error handling fields |
| `LGCMAREA` (line 69) | LINKAGE SECTION | COMMAREA structure |
| `LGERRPRC` (line 176) | PROCEDURE DIVISION | Common error handling procedures |

### Condition Names (88-levels)
None defined directly in this program.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Identifies the operation and policy type (`'01UEND'`, `'01UHOU'`, `'01UMOT'`).
- `EIBCALEN`: Used for length validation against the minimum required for the policy type.
- All policy data fields in the COMMAREA are passed through to LGUPDB01.

### Output (COMMAREA)
- `CA-RETURN-CODE`:
  - `'00'`: Success (or passthrough from LGUPDB01).
  - `'98'`: COMMAREA too short for the requested policy type.
  - `'99'`: Unrecognized request ID.
  - Other codes may be set by LGUPDB01 (`'01'`, `'02'`, `'90'`).

### Files Accessed
None directly. All data access is delegated to LGUPDB01.

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| LGUPDB01 | EXEC CICS LINK (line 152) | DB2 data access for policy update |
| LGSTSQ | EXEC CICS LINK (via LGERRPRC) | Error message logging |

## 5. Algorithms and Techniques

### SQL Statements
None. This is a business logic program with no direct DB2 access.

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` (line 97): Abend on missing COMMAREA.
- `EXEC CICS RETURN` (lines 115, 123, 131, 142): Return to caller.
- `EXEC CICS LINK Program(LGUPDB01) Commarea(DFHCOMMAREA) LENGTH(32500)` (line 152): Delegate to DB2 access layer.

### Error Handling
- Missing COMMAREA: Writes error message, then abends.
- Short COMMAREA: Sets return code `'98'` and returns immediately -- no abend.
- Invalid request ID: Sets return code `'99'` but still proceeds to call LGUPDB01 (see known issues).

### Notable Logic Patterns
- **EVALUATE dispatch with length validation**: Each policy type has a known minimum COMMAREA length. The program computes the required length by adding the header length (28) to the type-specific data length.
- **Separation of concerns**: The program contains no data access logic -- it is purely a validation and routing layer.

## 6. Analysis

### Overall Role
LGUPOL01 is the middle tier in a three-layer architecture for policy updates:
```
Caller -> LGUPOL01 (business logic/validation) -> LGUPDB01 (DB2 access) -> LGUPVS01 (VSAM access)
```
It validates input before delegating to the data access layer.

### Dependencies
- **Program LGUPDB01**: Required for all data operations.
- **Program LGSTSQ**: Required for error logging.
- **Copybooks**: LGCMAREA, LGERR, LGERRPRC.

### Known Issues or Limitations
1. **Proceeds to LGUPDB01 on unrecognized request**: When `CA-REQUEST-ID` matches `OTHER` (line 134-135), the program sets return code `'99'` but does NOT return -- it falls through to line 138 and calls `UPDATE-POLICY-DB2-INFO` anyway. This could cause unexpected behavior in LGUPDB01.
2. **WS-REQUIRED-CA-LEN not reset between evaluations**: Since it uses `ADD` (not `MOVE`), if the EVALUATE logic were ever restructured to allow fall-through, accumulated lengths could be incorrect. However, since only one WHEN clause matches, this is not an active bug.
3. **No validation of policy number or customer number**: The program validates COMMAREA length but does not check whether `CA-CUSTOMER-NUM` or `CA-POLICY-NUM` contain valid data.
4. **Typo in comment**: Line 106 says "chec commarea length" (should be "check").

### Relationship to Other Programs
- **Called by**: Higher-level UI/controller programs (likely LGAPOL01 pattern, or a BMS map handler).
- **Calls**: LGUPDB01 (DB2 update), LGSTSQ (error logging).
- **Sibling programs**: LGIPOL01 (Inquire Policy), LGAPOL01 (Add Policy), LGDPOL01 (Delete Policy).
