# LGAPOL01.cbl - Add Policy (Business Logic)

## 1. Overview

- **Program ID:** LGAPOL01
- **Purpose:** Business logic layer for adding a new insurance policy. Validates the request and delegates to the DB2 data-access program LGAPDB01.
- **Summary:** LGAPOL01 is the business-logic entry point for policy creation. Like its customer counterpart LGACUS01, it is a thin orchestration layer that validates the incoming commarea (checking for presence and minimum length) and then links to LGAPDB01 to perform the actual DB2 inserts and VSAM writes. It handles all four policy types (Endowment, House, Motor, Commercial) by passing the request through unchanged.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 76-125)

1. **Initialization (lines 82-87):** Initializes `WS-HEADER`, captures EIB fields, stores commarea length.
2. **Commarea validation (lines 94-112):**
   - If `EIBCALEN` is zero, writes error and abends with `'LGCA'`.
   - Sets `CA-RETURN-CODE` to `'00'`.
   - Computes required length: `WS-CA-HEADER-LEN` (28 bytes, reflecting the policy header which includes the 10-byte policy number field).
   - If commarea is shorter than required, returns `'98'`.
3. **Delegate to DB2 program (lines 117-120):**
   ```cobol
   EXEC CICS Link Program(LGAPDB01)
        Commarea(DFHCOMMAREA)
        LENGTH(32500)
   END-EXEC.
   ```
4. **Return (line 122):** Returns to caller.

### WRITE-ERROR-MESSAGE (lines 132-140)

Formats time/date, writes error to TSQ via common error handling copybook procedures.

### MAINLINE-EXIT (lines 124-125)

Standard section exit.

### Control Flow

```
MAINLINE -> validate commarea
         -> LINK LGAPDB01
         -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-HEADER` (line 25) | Group | Debug eyecatcher `'LGAPOL01------WS'`, EIB fields, commarea pointer |
| `ERROR-MSG` (line 41) | Group | Simple error message with date, time, program name, variable text |
| `LGAPDB01` (line 48) | `PIC X(8)` | Constant `'LGAPDB01'` -- the DB2 program to link to |
| `WS-COMMAREA-LENGTHS` (line 54) | Group | Header length (28) and computed required length |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 66): Defined via `Copy LGCMAREA`. Full commarea with all policy type overlays.

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGERR` (line 38) | `COPY LGERR` | Common error handling working storage |
| `LGCMAREA` (line 67) | `Copy LGCMAREA` | Commarea field definitions |
| `LGERRPRC` (line 145) | `COPY LGERRPRC` | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Policy add request type (`'01AEND'`, `'01AHOU'`, `'01AMOT'`, `'01ACOM'`). Not validated here -- delegated to LGAPDB01.
- `CA-CUSTOMER-NUM`: The customer to associate the policy with.
- All policy-specific fields as defined in LGCMAREA.

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'00'` success, `'98'` short commarea. Other codes from LGAPDB01: `'70'` (customer not found), `'80'` (VSAM error from LGAPVS01), `'90'` (SQL error), `'99'` (unknown request type).
- `CA-POLICY-NUM`: Populated by LGAPDB01 with the assigned policy number.
- `CA-LASTCHANGED`: Populated by LGAPDB01 with the timestamp.

### Files Accessed

None directly. All data access is delegated to LGAPDB01 and LGAPVS01.

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGAPDB01` (line 117) | DB2 policy insert and VSAM write orchestration |
| `LGSTSQ` (via LGERRPRC) | Error logging to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

None. This program contains no embedded SQL or `PROCESS SQL` directive.

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` -- Abend on missing commarea
- `EXEC CICS RETURN` -- Return to caller
- `EXEC CICS Link Program(LGAPDB01)` -- Delegate to DB2 access layer

### Error Handling
- Missing commarea triggers abend `'LGCA'` with error message to TSQ.
- Short commarea returns `'98'`.
- All other error handling is delegated to LGAPDB01 (SQL errors, referential integrity, VSAM errors).

### Notable Logic Patterns
- **Minimal validation:** The business logic layer only checks for commarea presence and minimum header length (28 bytes). It does not check `CA-REQUEST-ID` or type-specific minimum lengths; that is left to LGAPDB01.
- **Pass-through architecture:** The entire commarea is forwarded unchanged to LGAPDB01 with a fixed length of 32,500 bytes.

## 6. Analysis

### Overall Role
LGAPOL01 is the business-logic facade for policy creation. In the GenApp layered architecture, front-end programs call LGAPOL01, which delegates to LGAPDB01 for data access. This separation allows the data-access layer to be maintained independently.

### Dependencies
- Programs: LGAPDB01, LGSTSQ
- Copybooks: LGERR, LGERRPRC, LGCMAREA

### Known Issues or Limitations
1. **No LGPOLICY copybook:** Unlike LGAPDB01, this program does not include LGPOLICY, so it does not have access to the type-specific length constants (`WS-FULL-ENDOW-LEN`, etc.). Its commarea validation is therefore limited to the 28-byte header check.
2. **No request ID validation:** Does not validate that `CA-REQUEST-ID` is a recognized add-policy request before calling LGAPDB01.
3. **Hardcoded commarea length:** The value 32,500 passed to LGAPDB01 is a magic number.

### Relationship to Other Programs
- **Called by:** Front-end programs, CICS web service interfaces
- **Calls:** LGAPDB01 (DB2 data access), LGSTSQ (error logging)
- **Peer:** LGACUS01 (same pattern for customer add business logic)
- **Related:** LGUPOL01 (update policy), LGDPOL01 (delete policy), LGIPOL01 (inquire policy)
