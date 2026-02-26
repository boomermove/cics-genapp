# LGAPVS01.cbl - Add Policy VSAM Record

## 1. Overview

- **Program ID:** LGAPVS01
- **Purpose:** Write a new policy record to the VSAM KSDS file `KSDSPOLY`.
- **Summary:** LGAPVS01 is the VSAM data-access program for policy records. It receives policy data via the commarea, maps the relevant fields into a compact 64-byte working-storage record structure (`WF-Policy-Info`), and writes it to the `KSDSPOLY` KSDS file with a 21-byte composite key (request type + customer number + policy number). The program handles all four policy types (Commercial, Endowment, House, Motor) by using REDEFINES to map type-specific fields. It is called by LGAPDB01 after the DB2 policy insert.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 90-149)

1. **Save commarea length (line 93):** Moves `EIBCALEN` to `WS-Commarea-Len`.
2. **Build VSAM record key (lines 95-97):**
   - Extracts the 4th character of `CA-Request-ID` into `WF-Request-ID` (the type indicator: `'C'`, `'E'`, `'H'`, or `'M'`).
   - Moves `CA-Policy-Num` and `CA-Customer-Num` into the key structure.
3. **Map type-specific data (lines 99-128):** Uses `EVALUATE WF-Request-ID`:
   - `'C'` (Commercial): Maps postcode, status, customer name.
   - `'E'` (Endowment): Maps with-profits, equities, managed fund, fund name, life assured.
   - `'H'` (House): Maps property type, bedrooms, value, postcode, house name.
   - `'M'` (Motor): Maps make, model, value, registration number.
   - `Other`: Clears `WF-Policy-Data` to spaces.
4. **VSAM WRITE (lines 131-137):**
   ```cobol
   Exec CICS Write File('KSDSPOLY')
        From(WF-Policy-Info)
        Length(64)
        Ridfld(WF-Policy-Key)
        KeyLength(21)
        RESP(WS-RESP)
   End-Exec.
   ```
5. **Error handling (lines 138-143):** If response is not normal, sets return code `'80'`, writes error, and returns.

### A-EXIT (lines 147-149)

Standard exit with `EXIT` and `GOBACK`.

### WRITE-ERROR-MESSAGE (lines 154-167)

Formats time/date, populates customer number, policy number, RESP, and RESP2 into error message fields, then writes to TSQ.

### Control Flow

```
MAINLINE -> Build key from commarea
         -> EVALUATE type -> map fields
         -> WRITE File('KSDSPOLY')
         -> If error: WRITE-ERROR-MESSAGE -> RETURN
         -> A-EXIT (GOBACK)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-RESP` / `WS-RESP2` (lines 18-19) | `PIC S9(8) COMP` | CICS response codes |
| `WS-Comm-Len` (line 20) | `PIC S9(8) COMP` | Declared but unused |
| `WS-STARTCODE` (line 21) | `PIC XX` | Declared but unused |
| `WS-SYSID` (line 22) | `PIC X(4)` | Declared but unused |
| `WS-Commarea-Len` (line 23) | `PIC S9(4) COMP` | Stores EIBCALEN |
| `WF-Policy-Info` (line 25) | Group | The complete 64-byte VSAM record |
| `WF-Policy-Key` (line 26) | Group (21 bytes) | Composite key: type (1) + customer (10) + policy (10) |
| `WF-Policy-Data` (line 30) | `PIC X(43)` | Type-specific policy data (43 bytes) |
| `WF-C-Policy-Data` (line 31) | REDEFINES | Commercial: postcode (8) + status (4) + customer (31) |
| `WF-E-Policy-Data` (line 35) | REDEFINES | Endowment: with-profits (1) + equities (1) + managed fund (1) + fund name (10) + life assured (30) |
| `WF-H-Policy-Data` (line 41) | REDEFINES | House: property type (15) + bedrooms (3) + value (8) + postcode (8) + house name (9) |
| `WF-M-Policy-Data` (line 47) | REDEFINES | Motor: make (15) + model (15) + value (6) + reg number (7) |
| `ERROR-MSG` (line 58) | Group | Error message with policy/customer numbers, file name, RESP/RESP2 |
| `Eyecatcher` (line 76) | `PIC X(16)` | Constant `'Program LGAPVS01'` for debugging |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 82): Defined via `Copy LGCMAREA`. Full commarea with all policy overlays.

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGERR` (line 56) | `COPY LGERR` | Common error handling working storage |
| `LGCMAREA` (line 83) | `Copy LGCMAREA` | Commarea field definitions |
| `LGERRPRC` (line 172) | `COPY LGERRPRC` | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-Request-ID`: Used to extract the policy type character at position 4 (`'C'`/`'E'`/`'H'`/`'M'`).
- `CA-Customer-Num`: 10-byte customer number for the key.
- `CA-Policy-Num`: 10-byte policy number for the key.
- Type-specific policy fields from the commarea overlays.

### Output (COMMAREA)
- `CA-RETURN-CODE`: Set to `'80'` on VSAM write error. Not modified on success.

### Files Accessed
- **VSAM KSDSPOLY** -- WRITE (line 131). Key-Sequenced Data Set for policy records. Composite key: 21 bytes (type + customer num + policy num). Record length: 64 bytes.

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGSTSQ` (via LGERRPRC) | Write error messages to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

None. This is a pure VSAM program.

### CICS Commands
- `EXEC CICS Write File('KSDSPOLY')` -- Write policy record to VSAM KSDS
- `EXEC CICS RETURN` -- Return to caller (error path only)

### Error Handling
- VSAM write errors detected by checking `WS-RESP` against `DFHRESP(NORMAL)`.
- On error: sets return code `'80'`, writes error to TSQ, returns to caller.
- Unlike LGACVS01, this program does NOT abend on VSAM error -- it simply returns with the error code, allowing the caller (LGAPDB01) to handle the situation.

### Notable Logic Patterns
- **EVALUATE for polymorphic record mapping:** The four REDEFINES of `WF-Policy-Data` and the EVALUATE provide a form of polymorphism, mapping different commarea overlays to the correct VSAM record structure based on policy type.
- **Compact VSAM record:** The 64-byte VSAM record stores only a subset of the full policy data (key fields plus the most important type-specific fields), unlike the DB2 tables which store complete details.
- **Key extraction from request ID:** Uses `CA-Request-ID(4:1)` to get the policy type character, which works because the request ID format is `'01Axxx'` where the 4th character indicates the type.

## 6. Analysis

### Overall Role
LGAPVS01 provides the VSAM data-access layer for policy records, maintaining a compact KSDS mirror of the DB2 policy data. The VSAM file likely supports fast key-based lookups by policy type + customer + policy number without requiring DB2 access.

### Dependencies
- VSAM file `KSDSPOLY` must be defined and available in the CICS region.
- Commarea must contain valid policy data at expected offsets.
- Copybooks: LGERR, LGERRPRC, LGCMAREA

### Known Issues or Limitations
1. **No commarea validation:** Like LGACVS01, this program does not validate `EIBCALEN` or commarea presence. It trusts the caller (LGAPDB01) to have validated.
2. **Data loss in VSAM record:** The 43-byte `WF-Policy-Data` area cannot hold all fields stored in DB2. For example, Motor stores make + model + value + reg but omits colour, CC, year, premium, and accidents. Commercial stores only postcode + status + customer, omitting address, geolocation, perils, premiums, etc.
3. **Unused variables:** `WS-Comm-Len`, `WS-STARTCODE`, `WS-SYSID` are declared but unused.
4. **GOBACK after EXIT** (line 149): The `GOBACK` is unreachable.
5. **No abend on error:** Unlike LGACVS01, this program does not abend on VSAM error, which means a VSAM write failure will not trigger UOW backout of the preceding DB2 inserts. The DB2 data and VSAM data may become inconsistent.

### Relationship to Other Programs
- **Called by:** LGAPDB01 (policy creation DB2 access program)
- **Peer:** LGACVS01 (same pattern for customer VSAM writes)
- **Related:** LGUPVS01 (update policy VSAM), LGDPVS01 (delete policy VSAM)
