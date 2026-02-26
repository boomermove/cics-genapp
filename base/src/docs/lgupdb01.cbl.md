# LGUPDB01 - Update Policy DB2 Data Access

## 1. Overview

- **Program ID:** LGUPDB01
- **Purpose:** Updates policy records in DB2 tables (POLICY, ENDOWMENT, HOUSE, and MOTOR) for the GenApp insurance application.
- **Summary:** LGUPDB01 is a DB2 data access program that implements optimistic concurrency control for policy updates. It opens a cursor on the POLICY table with `SELECT FOR UPDATE`, fetches the current row, compares the LASTCHANGED timestamp from the database with the value in the COMMAREA, and only proceeds with the update if they match. Based on the request type (`01UEND`, `01UHOU`, `01UMOT`), it updates the corresponding policy-type-specific table (ENDOWMENT, HOUSE, or MOTOR), then updates the POLICY table's common fields and sets a new CURRENT TIMESTAMP. After DB2 operations, it calls LGUPVS01 to synchronize the VSAM copy of the data.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION (lines 157-214)
1. **Lines 163-168:** Initializes `WS-HEADER` and captures EIB transaction, terminal, and task info.
2. **Lines 171-172:** Initializes DB2 host variables (`DB2-POLICY`, `DB2-IN-INTEGERS`).
3. **Lines 178-182:** Checks for missing COMMAREA; abends with `'LGCA'` if `EIBCALEN` is zero.
4. **Lines 185-194:** Sets return code to `'00'`, converts customer/policy numbers to DB2 integer format, saves them in error message fields.
5. **Line 202:** Performs `UPDATE-POLICY-DB2-INFO` -- the main update logic.
6. **Lines 204-207:** Links to program `LGUPVS01` to update the VSAM copy, passing the COMMAREA with length 225.
7. **Lines 210-211:** Returns to caller via `EXEC CICS RETURN`.

#### FETCH-DB2-POLICY-ROW (lines 223-234)
Fetches a single row from the `POLICY_CURSOR` into host variables: `DB2-ISSUEDATE`, `DB2-EXPIRYDATE`, `DB2-LASTCHANGED`, `DB2-BROKERID-INT` (with indicator), `DB2-BROKERSREF` (with indicator), and `DB2-PAYMENT-INT` (with indicator).

#### UPDATE-POLICY-DB2-INFO (lines 246-356)
The core update logic:
1. **Lines 249-265:** Opens `POLICY_CURSOR`. On SQLCODE -913 (deadlock/timeout) or other errors, sets return code `'90'` and returns.
2. **Line 268:** Fetches the first (and expected only) row.
3. **Lines 270-353:** If SQLCODE = 0 (row found):
   - **Lines 273-342:** Compares `CA-LASTCHANGED` with `DB2-LASTCHANGED` (optimistic concurrency check):
     - **If match (lines 278-337):** Uses EVALUATE on `CA-REQUEST-ID` to dispatch to type-specific update routines (`UPDATE-ENDOW-DB2-INFO`, `UPDATE-HOUSE-DB2-INFO`, `UPDATE-MOTOR-DB2-INFO`). If the type-specific update succeeds, updates the POLICY table itself (lines 313-321) and retrieves the newly assigned CURRENT TIMESTAMP (lines 324-329). On SQLCODE failure, performs a `SYNCPOINT ROLLBACK` and sets return code `'90'`.
     - **If no match (lines 339-342):** Sets return code `'02'` (timestamp mismatch / stale data).
   - **Lines 344-352:** If SQLCODE = 100 (no row found), sets return code `'01'`. Otherwise, sets `'90'`.
4. **Line 355:** Closes the cursor via `CLOSE-PCURSOR`.

#### CLOSE-PCURSOR (lines 357-376)
Closes `POLICY_CURSOR`. Handles SQLCODE 0 (success), -501 (cursor not open -- treated as success but returns immediately), and other errors (return code `'90'`).

#### UPDATE-ENDOW-DB2-INFO (lines 382-413)
Updates the ENDOWMENT table: sets `WITHPROFITS`, `EQUITIES`, `MANAGEDFUND`, `FUNDNAME`, `TERM`, `SUMASSURED`, `LIFEASSURED` for the matching `POLICYNUMBER`.

#### UPDATE-HOUSE-DB2-INFO (lines 419-449)
Updates the HOUSE table: sets `PROPERTYTYPE`, `BEDROOMS`, `VALUE`, `HOUSENAME`, `HOUSENUMBER`, `POSTCODE` for the matching `POLICYNUMBER`.

#### UPDATE-MOTOR-DB2-INFO (lines 455-490)
Updates the MOTOR table: sets `MAKE`, `MODEL`, `VALUE`, `REGNUMBER`, `COLOUR`, `CC`, `YEAROFMANUFACTURE`, `PREMIUM`, `ACCIDENTS` for the matching `POLICYNUMBER`.

#### WRITE-ERROR-MESSAGE (lines 496-506)
Saves SQLCODE, formats time/date, writes error message and commarea to TSQ via common error handling.

### Control Flow Summary
```
MAINLINE
  -> Initialize
  -> Validate COMMAREA
  -> UPDATE-POLICY-DB2-INFO
     -> OPEN POLICY_CURSOR
     -> FETCH-DB2-POLICY-ROW
     -> Compare timestamps (optimistic concurrency)
     -> EVALUATE CA-REQUEST-ID:
        '01UEND' -> UPDATE-ENDOW-DB2-INFO
        '01UHOU' -> UPDATE-HOUSE-DB2-INFO
        '01UMOT' -> UPDATE-MOTOR-DB2-INFO
     -> UPDATE POLICY table
     -> SELECT new LASTCHANGED timestamp
     -> CLOSE-PCURSOR
  -> LINK to LGUPVS01 (VSAM update)
  -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | PIC | Role |
|---|---|---|
| `WS-HEADER` (line 22) | Group | Debug/runtime info: eyecatcher, transaction/terminal/task IDs |
| `WS-RETRY` (line 31) | X | Retry flag (declared but not used in logic) |
| `WS-COMMAREA-LENGTHS` (line 58) | Group | Header length (28) and required CA length calculation |
| `WS-VARY-FIELD` (line 64) | Group | VARCHAR-compatible field (49-level with length + data), unused |
| `DB2-IN-INTEGERS` (line 81) | Group | DB2 integer host variables for all numeric COMMAREA fields |
| `IND-BROKERID` (line 104) | S9(4) COMP | Null indicator for BROKERID column |
| `IND-BROKERSREF` (line 105) | S9(4) COMP | Null indicator for BROKERSREFERENCE column |
| `IND-PAYMENT` (line 106) | S9(4) COMP | Null indicator for PAYMENT column |
| `LGUPVS01` (line 107) | X(8) VALUE 'LGUPVS01' | Name of VSAM update program to LINK to |

### LINKAGE SECTION
- **DFHCOMMAREA** (line 145): Included via `EXEC SQL INCLUDE LGCMAREA`. Contains the standard GenApp COMMAREA structure.

### Copybooks / INCLUDEs
| Copybook | Location | Purpose |
|---|---|---|
| `LGERR` (line 36) | WORKING-STORAGE | Common error handling work fields |
| `LGPOLICY` (line 98) | WORKING-STORAGE (SQL INCLUDE) | DB2 host variable structure for POLICY table columns |
| `SQLCA` (line 115) | WORKING-STORAGE (SQL INCLUDE) | DB2 SQL Communications Area (SQLCODE, etc.) |
| `LGCMAREA` (line 147) | LINKAGE (SQL INCLUDE) | COMMAREA structure |
| `LGERRPRC` (line 511) | PROCEDURE DIVISION | Common error handling procedures |

### Cursor Definition
**POLICY_CURSOR** (lines 123-138): Declared `WITH HOLD` and `FOR UPDATE OF` five columns. Selects `ISSUEDATE`, `EXPIRYDATE`, `LASTCHANGED`, `BROKERID`, `BROKERSREFERENCE` from POLICY where `CUSTOMERNUMBER` and `POLICYNUMBER` match.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Determines policy type (`'01UEND'`, `'01UHOU'`, `'01UMOT'`).
- `CA-CUSTOMER-NUM`: Customer number (converted to `DB2-CUSTOMERNUM-INT`).
- `CA-POLICY-NUM`: Policy number (converted to `DB2-POLICYNUM-INT`).
- `CA-LASTCHANGED`: Timestamp for optimistic concurrency comparison.
- `CA-ISSUE-DATE`, `CA-EXPIRY-DATE`, `CA-BROKERID`, `CA-BROKERSREF`: Common policy fields.
- Type-specific fields (e.g., `CA-E-WITH-PROFITS`, `CA-H-PROPERTY-TYPE`, `CA-M-MAKE`, etc.).

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'00'` = success, `'01'` = not found, `'02'` = timestamp mismatch, `'90'` = DB2 error.
- `CA-LASTCHANGED`: Updated with the new `CURRENT TIMESTAMP` value after a successful update.

### DB2 Tables Accessed
| Table | Operations |
|---|---|
| POLICY | SELECT FOR UPDATE (via cursor), UPDATE (WHERE CURRENT OF), SELECT (to retrieve new timestamp) |
| ENDOWMENT | UPDATE (by POLICYNUMBER) |
| HOUSE | UPDATE (by POLICYNUMBER) |
| MOTOR | UPDATE (by POLICYNUMBER) |

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| LGUPVS01 | EXEC CICS LINK (line 204) | Update VSAM policy record after DB2 update |
| LGSTSQ | EXEC CICS LINK (via LGERRPRC) | Error message logging |

## 5. Algorithms and Techniques

### SQL Statements

**1. OPEN POLICY_CURSOR** (line 251):
```sql
OPEN POLICY_CURSOR
```

**2. FETCH from cursor** (lines 226-233):
```sql
FETCH POLICY_CURSOR
INTO :DB2-ISSUEDATE, :DB2-EXPIRYDATE, :DB2-LASTCHANGED,
     :DB2-BROKERID-INT INDICATOR :IND-BROKERID,
     :DB2-BROKERSREF INDICATOR :IND-BROKERSREF,
     :DB2-PAYMENT-INT INDICATOR :IND-PAYMENT
```

**3. UPDATE ENDOWMENT** (lines 389-401):
```sql
UPDATE ENDOWMENT
SET WITHPROFITS = :CA-E-WITH-PROFITS, EQUITIES = :CA-E-EQUITIES,
    MANAGEDFUND = :CA-E-MANAGED-FUND, FUNDNAME = :CA-E-FUND-NAME,
    TERM = :DB2-E-TERM-SINT, SUMASSURED = :DB2-E-SUMASSURED-INT,
    LIFEASSURED = :CA-E-LIFE-ASSURED
WHERE POLICYNUMBER = :DB2-POLICYNUM-INT
```

**4. UPDATE HOUSE** (lines 427-437):
```sql
UPDATE HOUSE
SET PROPERTYTYPE = :CA-H-PROPERTY-TYPE, BEDROOMS = :DB2-H-BEDROOMS-SINT,
    VALUE = :DB2-H-VALUE-INT, HOUSENAME = :CA-H-HOUSE-NAME,
    HOUSENUMBER = :CA-H-HOUSE-NUMBER, POSTCODE = :CA-H-POSTCODE
WHERE POLICYNUMBER = :DB2-POLICYNUM-INT
```

**5. UPDATE MOTOR** (lines 464-478):
```sql
UPDATE MOTOR
SET MAKE = :CA-M-MAKE, MODEL = :CA-M-MODEL, VALUE = :DB2-M-VALUE-INT,
    REGNUMBER = :CA-M-REGNUMBER, COLOUR = :CA-M-COLOUR,
    CC = :DB2-M-CC-SINT, YEAROFMANUFACTURE = :CA-M-MANUFACTURED,
    PREMIUM = :DB2-M-PREMIUM-INT, ACCIDENTS = :DB2-M-ACCIDENTS-INT
WHERE POLICYNUMBER = :DB2-POLICYNUM-INT
```

**6. UPDATE POLICY** (lines 313-321):
```sql
UPDATE POLICY
SET ISSUEDATE = :CA-ISSUE-DATE, EXPIRYDATE = :CA-EXPIRY-DATE,
    LASTCHANGED = CURRENT TIMESTAMP,
    BROKERID = :DB2-BROKERID-INT, BROKERSREFERENCE = :CA-BROKERSREF
WHERE CURRENT OF POLICY_CURSOR
```

**7. SELECT new timestamp** (lines 324-329):
```sql
SELECT LASTCHANGED INTO :CA-LASTCHANGED
FROM POLICY WHERE POLICYNUMBER = :DB2-POLICYNUM-INT
```

**8. CLOSE POLICY_CURSOR** (line 361):
```sql
CLOSE POLICY_CURSOR
```

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA')` -- Abend on missing COMMAREA.
- `EXEC CICS LINK Program(LGUPVS01)` -- Call VSAM update program.
- `EXEC CICS RETURN` -- Return to caller.
- `EXEC CICS SYNCPOINT ROLLBACK` -- Roll back DB2 changes on POLICY UPDATE failure.

### Error Handling
- SQLCODE is checked after every SQL operation.
- SQLCODE 0 = success, 100 = not found (return code `'01'`), -913 = deadlock/timeout (return code `'90'`), other = error (`'90'`).
- On POLICY UPDATE failure, a `SYNCPOINT ROLLBACK` is performed to undo the type-specific update.

### Notable Logic Patterns
- **Optimistic concurrency control**: The `LASTCHANGED` timestamp comparison prevents lost updates. If another transaction modified the row between the user's read and this update, the timestamps won't match and return code `'02'` is returned.
- **EVALUATE dispatch**: `CA-REQUEST-ID` determines which policy-type table to update.
- **Cursor WITH HOLD / FOR UPDATE**: Ensures the row is locked for the duration of the update sequence.

## 6. Analysis

### Overall Role
LGUPDB01 is the DB2 data access layer for policy updates. It is called by the business logic layer (LGUPOL01) and in turn calls the VSAM data access layer (LGUPVS01), forming a three-tier update chain: business logic -> DB2 -> VSAM.

### Dependencies
- **DB2 tables**: POLICY, ENDOWMENT, HOUSE, MOTOR.
- **Programs**: LGUPVS01 (VSAM update), LGSTSQ (error logging).
- **Copybooks**: LGCMAREA, LGPOLICY, SQLCA, LGERR, LGERRPRC.
- **CICS DB2 attachment**: The program requires `PROCESS SQL` and DB2 connectivity.

### Known Issues or Limitations
1. **No COMMAREA length validation**: Unlike LGUPOL01, this program does not validate that the COMMAREA is large enough for the specific policy type being updated.
2. **CLOSE-PCURSOR overwrites return code**: Line 366 sets `CA-RETURN-CODE` to `'00'` on successful cursor close, potentially overwriting a `'02'` (timestamp mismatch) return code set earlier.
3. **WS-VARY-FIELD unused**: The VARCHAR-compatible field is declared but never used.
4. **WS-RETRY unused**: Declared but no retry logic is implemented.
5. **FETCH includes PAYMENT indicator** (line 232) but the PAYMENT column is not in the SELECT list of the cursor definition (lines 125-129). This is a bug -- the FETCH has 6 INTO targets but the cursor SELECT only has 5 columns.
6. **VSAM update always called**: LGUPVS01 is called (line 204) even if the DB2 update failed, which could lead to inconsistency.

### Relationship to Other Programs
- **Called by**: LGUPOL01 (Update Policy business logic) via `EXEC CICS LINK`.
- **Calls**: LGUPVS01 (Update Policy VSAM) via `EXEC CICS LINK`.
- **Sibling programs**: LGIPDB01 (Inquire Policy DB2), LGAPDB01 (Add Policy DB2), LGDPDB01 (Delete Policy DB2).
