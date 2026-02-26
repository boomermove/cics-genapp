# LGACUS01.cbl - Add Customer (Business Logic)

## 1. Overview

- **Program ID:** LGACUS01
- **Purpose:** Business logic layer for adding a new customer. Validates the incoming request and delegates database operations to LGACDB01.
- **Summary:** LGACUS01 is the business-logic program for customer creation in the GenApp insurance application. It receives a commarea from the front-end or service interface, validates the commarea length, initializes return codes, and then links to LGACDB01 to perform the actual DB2 insert and associated operations (VSAM write, security record creation). This program contains no direct database access; it serves as a thin orchestration layer that ensures the request is well-formed before passing it to the data-access layer.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 73-121)

1. **Initialization (lines 79-83):** Initializes `WS-HEADER`, captures EIB fields (`EIBTRNID`, `EIBTRMID`, `EIBTASKN`).
2. **Commarea validation (lines 90-110):**
   - If `EIBCALEN` is zero, writes error message and abends with `'LGCA'`.
   - Sets `CA-RETURN-CODE` to `'00'` and `CA-NUM-POLICIES` to `'00'`.
   - Computes required commarea length: `WS-CA-HEADER-LEN` (18) + `WS-CUSTOMER-LEN` (72) = 90 bytes.
   - If commarea is too short, sets return code `'98'` and returns.
3. **Business processing (line 114):** Performs `INSERT-CUSTOMER`.
4. **Return (line 118):** Returns to caller.

### INSERT-CUSTOMER (lines 127-135)

Simply links to LGACDB01 with the full commarea (length 32,500):
```cobol
EXEC CICS LINK Program(LGACDB01)
     Commarea(DFHCOMMAREA)
     LENGTH(32500)
END-EXEC.
```

### WRITE-ERROR-MESSAGE (lines 141-149)

Formats time/date via `LGERR-FORMAT-TIME`, then writes the error and commarea to TSQ using `LGERR-WRITE-MSG` and `LGERR-LOG-COMMAREA`.

### MAINLINE-EXIT (lines 120-121)

Standard section exit.

### Control Flow

```
MAINLINE -> validate commarea
         -> INSERT-CUSTOMER -> LINK LGACDB01
         -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-HEADER` (line 23) | Group | Debug eyecatcher `'LGACUS01------WS'`, EIB fields, commarea pointer |
| `ERROR-MSG` (line 39) | Group | Error message with date, time, program name, and variable text |
| `WS-COMMAREA-LENGTHS` (line 50) | Group | Header length (18) and computed required length |
| `LGACDB01` (line 53) | `PIC X(8)` | Constant `'LGACDB01'` -- the DB2 program to link to |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 64): Defined via `COPY LGCMAREA`. Contains `CA-REQUEST-ID`, `CA-RETURN-CODE`, `CA-CUSTOMER-NUM`, and the full customer/policy structure.

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGERR` (line 36) | `COPY LGERR` | Common error handling working storage fields |
| `LGPOLICY` (line 56) | `COPY LGPOLICY` | Policy length definitions (provides `WS-CUSTOMER-LEN`) |
| `LGCMAREA` (line 65) | `COPY LGCMAREA` | Commarea field definitions |
| `LGERRPRC` (line 154) | `COPY LGERRPRC` | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Expected to be `'01ACUS'` (not checked here; checked by caller or downstream).
- Full customer details: `CA-FIRST-NAME`, `CA-LAST-NAME`, `CA-DOB`, etc.

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'00'` success, `'98'` short commarea. Other codes may be set by LGACDB01.
- `CA-CUSTOMER-NUM`: Populated by LGACDB01 with the new customer number.
- `CA-NUM-POLICIES`: Initialized to `'00'` (zero policies for a new customer).

### Files Accessed
None directly. All data access is delegated to LGACDB01.

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGACDB01` (line 129) | Perform DB2 INSERT, VSAM WRITE, and security record creation |
| `LGSTSQ` (via LGERRPRC) | Write error messages to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

None. This program has no `PROCESS SQL` directive and contains no embedded SQL.

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` -- Abend on missing commarea
- `EXEC CICS RETURN` -- Return to caller
- `EXEC CICS LINK Program(LGACDB01)` -- Delegate to DB2 access program

### Error Handling
- Missing commarea triggers abend `'LGCA'` after writing error message.
- Short commarea returns code `'98'` without abend.
- Errors from LGACDB01 pass through via the shared commarea `CA-RETURN-CODE`.

### Notable Logic Patterns
- **Thin delegation pattern:** The program does almost nothing itself -- it validates the commarea and immediately delegates to LGACDB01. This separates business logic concerns from data access concerns, even though this particular business logic layer is minimal.

## 6. Analysis

### Overall Role
LGACUS01 is the business-logic entry point for the customer-add operation. In the GenApp architecture, front-end programs (like LGTESTC1 or the CICS web service interface) call LGACUS01, which validates the request and delegates to LGACDB01. This layering allows the data-access program to be swapped (e.g., to LGACDB01_SECURE) without changing the business interface.

### Dependencies
- Programs: LGACDB01, LGSTSQ
- Copybooks: LGERR, LGERRPRC, LGPOLICY, LGCMAREA

### Known Issues or Limitations
1. **No request ID validation:** Unlike LGACDB01_SECURE, this program does not check `CA-REQUEST-ID`. It relies on downstream programs or the caller to ensure the correct request type.
2. **`CA-NUM-POLICIES` initialization (line 98):** Sets the policy count to `'00'` which is correct for a new customer but hard-codes the value rather than deriving it.
3. **Pass-through design:** The program adds very little value beyond commarea validation; the business logic layer could potentially be combined with the data access layer for simplicity.

### Relationship to Other Programs
- **Called by:** Front-end programs (e.g., LGTESTC1, CICS web services)
- **Calls:** LGACDB01 (DB2 data access), LGSTSQ (error logging)
- **Peer:** LGACUS01 is the "add" counterpart to inquiry (LGICUS01), update (LGUCUS01), and delete programs
