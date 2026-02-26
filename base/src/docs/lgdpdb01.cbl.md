# LGDPDB01 - Delete Policy DB2 Data Access

## 1. Overview

- **Program ID:** LGDPDB01
- **Purpose:** Deletes a policy record from the DB2 POLICY table and its cascaded child records (Endowment, House, Motor, Commercial).
- **Summary:** LGDPDB01 is a DB2 data access program that deletes a row from the POLICY table based on customer number and policy number. It relies on DB2 foreign key CASCADE DELETE constraints to automatically remove the corresponding row from the policy-type-specific table (ENDOWMENT, HOUSE, MOTOR, or COMMERCIAL). After the DB2 deletion, it calls LGDPVS01 to delete the corresponding VSAM record, keeping both data stores in sync.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION (lines 108-175)
1. **Lines 114-118:** Initializes `WS-HEADER`, captures EIB runtime info.
2. **Line 122:** Initializes DB2 integer host variables.
3. **Lines 128-132:** Checks for missing COMMAREA; abends with `'LGCA'` if `EIBCALEN` is zero.
4. **Lines 135-137:** Sets return code to `'00'`, saves EIBCALEN, stores COMMAREA address.
5. **Lines 140-143:** Checks COMMAREA is at least as long as the header (28 bytes). Returns `'98'` if too short.
6. **Lines 146-150:** Converts `CA-CUSTOMER-NUM` and `CA-POLICY-NUM` to DB2 integer format. Saves them in error message fields.
7. **Lines 157-169:** Validates `CA-REQUEST-ID` against known delete request types (`'01DEND'`, `'01DHOU'`, `'01DCOM'`, `'01DMOT'`). If recognized, performs `DELETE-POLICY-DB2-INFO` and then links to LGDPVS01 for VSAM deletion. If not recognized, sets return code `'99'`.
8. **Line 172:** Returns to caller.

#### DELETE-POLICY-DB2-INFO (lines 183-201)
1. **Lines 186-191:** Executes:
   ```sql
   DELETE FROM POLICY
   WHERE (CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT
     AND  POLICYNUMBER = :DB2-POLICYNUM-INT)
   ```
2. **Lines 195-199:** If SQLCODE is not 0, sets return code `'90'`, writes error message, and returns. Note the comment on lines 193-194 mentions treating SQLCODE 100 as success, but the code does NOT implement this -- any non-zero SQLCODE results in `'90'`.

#### WRITE-ERROR-MESSAGE (lines 208-218)
Saves SQLCODE, formats time/date, writes error message and commarea to TSQ.

### Control Flow Summary
```
MAINLINE
  -> Initialize
  -> Validate COMMAREA (present, minimum length)
  -> Convert customer/policy numbers to DB2 integers
  -> Validate CA-REQUEST-ID against known delete types
  -> DELETE-POLICY-DB2-INFO
     -> DELETE FROM POLICY WHERE ...
     -> On failure: error '90', RETURN
  -> LINK to LGDPVS01 (VSAM delete)
  -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | PIC | Role |
|---|---|---|
| `WS-HEADER` (line 25) | Group | Runtime debug info with eyecatcher `'LGDPDB01------WS'` |
| `WS-CALEN` (line 33) | S9(4) COMP | Saved EIBCALEN |
| `DB2-IN-INTEGERS` (line 78) | Group | DB2 integer host variables |
| `DB2-CUSTOMERNUM-INT` (line 79) | S9(9) COMP | Customer number in DB2 INTEGER format |
| `DB2-POLICYNUM-INT` (line 80) | S9(9) COMP | Policy number in DB2 INTEGER format |
| `LGDPVS01` (line 56) | X(8) VALUE 'LGDPVS01' | Name of VSAM delete program |
| `WS-CA-HEADER-LEN` (line 63) | S9(4) COMP VALUE +28 | Minimum COMMAREA header length |

### ERROR-MSG Structure (lines 41-53)
Contains: date, time, program name `LGDPDB01`, customer number, policy number, SQL request description, and SQLCODE.

### LINKAGE SECTION
- **DFHCOMMAREA** (line 96): Included via `EXEC SQL INCLUDE LGCMAREA`.

### Copybooks / INCLUDEs
| Copybook | Location | Purpose |
|---|---|---|
| `LGERR` (line 38) | WORKING-STORAGE | Common error handling fields |
| `SQLCA` (line 88) | WORKING-STORAGE (SQL INCLUDE) | DB2 SQL Communications Area |
| `LGCMAREA` (line 98) | LINKAGE (SQL INCLUDE) | COMMAREA structure |
| `LGERRPRC` (line 223) | PROCEDURE DIVISION | Common error handling procedures |

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Must be one of `'01DEND'`, `'01DHOU'`, `'01DCOM'`, `'01DMOT'`.
- `CA-CUSTOMER-NUM`: Customer number identifying the policy owner.
- `CA-POLICY-NUM`: Policy number identifying the specific policy.

### Output (COMMAREA)
- `CA-RETURN-CODE`:
  - `'00'`: Success (delete completed or row not found via foreign key cascade).
  - `'90'`: DB2 error during delete.
  - `'98'`: COMMAREA too short.
  - `'99'`: Unrecognized request ID.

### DB2 Tables Accessed
| Table | Operation | Details |
|---|---|---|
| POLICY | DELETE | By CUSTOMERNUMBER and POLICYNUMBER. Foreign key cascading deletes the child table row. |

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| LGDPVS01 | EXEC CICS LINK (line 165) | Delete VSAM policy record |
| LGSTSQ | EXEC CICS LINK (via LGERRPRC) | Error message logging |

## 5. Algorithms and Techniques

### SQL Statements

**DELETE FROM POLICY** (lines 187-191):
```sql
DELETE FROM POLICY
WHERE (CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT
  AND  POLICYNUMBER  = :DB2-POLICYNUM-INT)
```

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` (line 131): Abend on missing COMMAREA.
- `EXEC CICS RETURN` (lines 142, 172, 198): Return to caller.
- `EXEC CICS LINK PROGRAM(LGDPVS01) Commarea(DFHCOMMAREA) LENGTH(32500)` (line 165): Call VSAM delete program.

### Error Handling
- Missing COMMAREA: Write error, abend `'LGCA'`.
- Short COMMAREA: Return code `'98'`.
- SQL error: Return code `'90'`, write error message, return immediately.
- Unrecognized request: Return code `'99'`.

### Notable Logic Patterns
- **Foreign key cascade**: The program only deletes from the POLICY table. The comment on lines 180-182 explains that foreign key definitions propagate the delete to the appropriate policy-type table (ENDOWMENT, HOUSE, MOTOR, or COMMERCIAL).
- **Request ID validation**: Unlike the update programs, LGDPDB01 validates the request ID itself (rather than using an EVALUATE dispatch), since all delete types use the same SQL statement.
- **Simple single-statement delete**: No cursors or multi-step operations needed.

## 6. Analysis

### Overall Role
LGDPDB01 is the DB2 data access layer for policy deletions. It is called by LGDPOL01 (business logic) and calls LGDPVS01 (VSAM) to maintain data consistency:
```
LGDPOL01 -> LGDPDB01 -> LGDPVS01
```

### Dependencies
- **DB2 POLICY table**: Must exist with foreign key CASCADE DELETE constraints to child tables.
- **Programs**: LGDPVS01, LGSTSQ.
- **Copybooks**: LGCMAREA, SQLCA, LGERR, LGERRPRC.
- **CICS DB2 attachment**: Requires `PROCESS SQL`.

### Known Issues or Limitations
1. **Comment contradicts code**: Lines 193-194 say "Treat SQLCODE 0 and SQLCODE 100 (record not found) as successful," but line 195 checks `IF SQLCODE NOT EQUAL 0` -- meaning SQLCODE 100 IS treated as an error (return code `'90'`), contrary to the comment's intent. This could cause problems if a delete is attempted on a non-existent record.
2. **VSAM delete called even on DB2 failure scenario**: If the request ID is recognized but the DB2 delete returns SQLCODE 0, the VSAM delete proceeds. However, if DB2 delete fails, line 198 returns before reaching the LINK to LGDPVS01, which is the correct behavior.
3. **No SYNCPOINT ROLLBACK**: Unlike LGUPDB01, this program does not perform a rollback on failure, relying on the implicit rollback at task end.
4. **Request ID validation includes '01DCOM'**: The Commercial policy type is accepted here, unlike the update path which does not support Commercial.

### Relationship to Other Programs
- **Called by**: LGDPOL01 (Delete Policy business logic) via `EXEC CICS LINK`.
- **Calls**: LGDPVS01 (Delete Policy VSAM) via `EXEC CICS LINK`.
- **Sibling programs**: LGIPDB01 (Inquire), LGAPDB01 (Add), LGUPDB01 (Update) -- all policy DB2 data access.
