# LGUPVS01 - Update Policy VSAM Record

## 1. Overview

- **Program ID:** LGUPVS01
- **Purpose:** Updates a policy record in a VSAM KSDS file named `KSDSPOLY`.
- **Summary:** LGUPVS01 is a low-level CICS COBOL program that updates VSAM policy records to keep them in sync with DB2. It extracts the policy type from the COMMAREA's request ID, maps the relevant COMMAREA fields into a local working-storage structure (`WF-Policy-Info`), reads the existing VSAM record with an update lock, then rewrites it with the new data. The program supports four policy types: Commercial (C), Endowment (E), House (H), and Motor (M), each with different field mappings.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION (lines 93-168)
1. **Line 96:** Saves `EIBCALEN` into `WS-Commarea-Len`.
2. **Lines 98-100:** Extracts the 4th character of `CA-Request-ID` into `WF-Request-ID` (this is the policy type indicator), and copies customer/policy numbers into the working-storage key structure.
3. **Lines 102-131:** EVALUATE block on `WF-Request-ID`:
   - `'C'` (Commercial): Maps `CA-B-Postcode`, `CA-B-Status`, `CA-B-Customer` to working-storage fields.
   - `'E'` (Endowment): Maps `CA-E-WITH-PROFITS`, `CA-E-EQUITIES`, `CA-E-MANAGED-FUND`, `CA-E-FUND-NAME`, `CA-E-LIFE-ASSURED`.
   - `'H'` (House): Maps `CA-H-PROPERTY-TYPE`, `CA-H-BEDROOMS`, `CA-H-VALUE`, `CA-H-POSTCODE`, `CA-H-HOUSE-NAME`.
   - `'M'` (Motor): Maps `CA-M-MAKE`, `CA-M-MODEL`, `CA-M-VALUE`, `CA-M-REGNUMBER`.
   - `OTHER`: Clears `WF-Policy-Data` to spaces.
4. **Line 133:** Re-copies `CA-Policy-Num` into `WF-Policy-Num` (appears redundant with line 99).
5. **Lines 135-142:** Issues `EXEC CICS READ FILE('KSDSPOLY') INTO(WS-FileIn) RIDFLD(WF-Policy-Key) KEYLENGTH(21) UPDATE`. The key is 21 bytes: 1-byte type + 10-byte customer number + 10-byte policy number.
6. **Lines 143-149:** On READ failure: sets return code `'81'`, writes error, abends with `'LGV3'`.
7. **Lines 151-155:** Issues `EXEC CICS REWRITE FILE('KSDSPOLY') FROM(WF-Policy-Info) LENGTH(WS-Commarea-LenF)` where `WS-Commarea-LenF` is a fixed 64 bytes.
8. **Lines 156-162:** On REWRITE failure: sets return code `'82'`, writes error, abends with `'LGV4'`.

#### A-EXIT (lines 166-168)
Normal exit point with `EXIT` and `GOBACK`.

#### WRITE-ERROR-MESSAGE (lines 173-186)
Formats time, populates customer number, policy number, and CICS response codes into the error message, then writes to TSQ.

### Control Flow Summary
```
MAINLINE
  -> Extract policy type from CA-Request-ID(4:1)
  -> EVALUATE policy type:
     'C' -> Map commercial fields
     'E' -> Map endowment fields
     'H' -> Map house fields
     'M' -> Map motor fields
  -> READ KSDSPOLY (with Update lock)
     -> On failure: error '81', ABEND 'LGV3'
  -> REWRITE KSDSPOLY
     -> On failure: error '82', ABEND 'LGV4'
  -> A-EXIT (GOBACK)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | PIC | Role |
|---|---|---|
| `WS-RESP` (line 18) | S9(8) COMP | CICS response code |
| `WS-RESP2` (line 19) | S9(8) COMP | CICS response2 code |
| `WS-Commarea-Len` (line 23) | S9(4) COMP VALUE 64 | Length for READ operation |
| `WS-Commarea-LenF` (line 24) | S9(4) COMP VALUE 64 | Fixed length for REWRITE (64 bytes) |
| `WF-Policy-Info` (line 26) | Group (64 bytes total) | The record structure written to VSAM |
| `WF-Policy-Key` (line 27) | Group (21 bytes) | Composite key: type (1) + customer (10) + policy (10) |
| `WF-Policy-Data` (line 31) | X(43) | Policy-type-specific data area with REDEFINES |
| `WS-FileIn` (line 54) | X(1024) | Buffer for reading existing VSAM record |
| `Eyecatcher` (line 79) | X(16) VALUE 'Program LGUPVS01' | Debug eyecatcher |

### WF-Policy-Data REDEFINES
The 43-byte `WF-Policy-Data` field has four overlapping layouts:
- **WF-C-Policy-Data**: Postcode (8), Status (4), Customer (31) -- Commercial.
- **WF-E-Policy-Data**: With-Profits (1), Equities (1), Managed-Fund (1), Fund-Name (10), Life-Assured (30) -- Endowment.
- **WF-H-Policy-Data**: Property-Type (15), Bedrooms (3), Value (8), Postcode (8), House-Name (9) -- House.
- **WF-M-Policy-Data**: Make (15), Model (15), Value (6), Reg-Number (7) -- Motor.

### ERROR-MSG Structure (lines 61-76)
Contains: date, time, program name `LGUPVS01`, policy number, customer number, operation description `Re-write KSDSPOLY`, RESP and RESP2 codes.

### LINKAGE SECTION
- **DFHCOMMAREA** (line 85): Uses `COPY LGCMAREA`.

### Copybooks Included
| Copybook | Location | Purpose |
|---|---|---|
| `LGERR` (line 59) | WORKING-STORAGE | Common error handling fields |
| `LGCMAREA` (line 86) | LINKAGE SECTION | COMMAREA structure |
| `LGERRPRC` (line 191) | PROCEDURE DIVISION | Common error handling procedures |

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-Request-ID`: Characters 4:1 determine the policy type (C/E/H/M).
- `CA-Customer-Num`: Customer number (part of VSAM key).
- `CA-Policy-Num`: Policy number (part of VSAM key).
- Type-specific fields from the COMMAREA.

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'81'` on READ failure, `'82'` on REWRITE failure. Not set on success.

### Files Accessed
| File | Type | Operation | Key |
|---|---|---|---|
| `KSDSPOLY` | VSAM KSDS | READ (with Update) | WF-Policy-Key (21 bytes) |
| `KSDSPOLY` | VSAM KSDS | REWRITE | (implicit, same record) |

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| LGSTSQ | EXEC CICS LINK (via LGERRPRC) | Error logging |

## 5. Algorithms and Techniques

### CICS Commands
1. **`EXEC CICS READ FILE('KSDSPOLY') INTO(WS-FileIn) RIDFLD(WF-Policy-Key) KEYLENGTH(21) UPDATE`** (line 135): Reads and locks the VSAM record.
2. **`EXEC CICS REWRITE FILE('KSDSPOLY') FROM(WF-Policy-Info) LENGTH(WS-Commarea-LenF)`** (line 151): Rewrites the record with new data.
3. **`EXEC CICS ABEND ABCODE('LGV3')` / `ABCODE('LGV4')`**: Abend codes for READ/REWRITE failures.

### Error Handling
Same pattern as LGUCVS01: check `WS-RESP` against `DFHRESP(NORMAL)`, set return code, write error message, abend.

### Notable Logic Patterns
- **EVALUATE-based field mapping**: The program maps only the relevant COMMAREA fields based on policy type, using REDEFINES on the working-storage policy data area.
- **Composite VSAM key**: The 21-byte key consists of the type indicator (extracted from position 4 of `CA-Request-ID`), customer number, and policy number.
- **Fixed record length**: The REWRITE always uses 64 bytes (`WS-Commarea-LenF`), regardless of the policy type.

## 6. Analysis

### Overall Role
LGUPVS01 is the VSAM data access layer for policy updates. It is the final step in the three-tier update chain:
```
LGUPOL01 (business logic) -> LGUPDB01 (DB2) -> LGUPVS01 (VSAM)
```
It maintains a VSAM copy of policy data, likely used for faster sequential access or as a backup/audit trail.

### Dependencies
- **VSAM file `KSDSPOLY`**: Must be defined and accessible.
- **Valid COMMAREA**: Must contain correct policy type indicator, customer number, policy number, and type-specific data.
- **Copybooks**: LGCMAREA, LGERR, LGERRPRC.

### Known Issues or Limitations
1. **No COMMAREA validation**: The program does not check `EIBCALEN` against minimum requirements. It assumes the caller (LGUPDB01) has already validated.
2. **Data subset**: The VSAM record only contains a subset of the full COMMAREA data (43 bytes of policy-specific data). Some fields (e.g., House's HOUSE-NUMBER, Motor's COLOUR/CC/MANUFACTURED/PREMIUM/ACCIDENTS) are not stored in VSAM.
3. **Commercial type handling**: The 'C' type maps `CA-B-Postcode`, `CA-B-Status`, and `CA-B-Customer`, but LGUPDB01 does not handle Commercial updates (no `'01UCOM'` in its EVALUATE). The VSAM record would still be updated if somehow called with type 'C'.
4. **ABEND on all failures**: Any VSAM error causes an abend rather than a graceful return, which is problematic since this is called after a successful DB2 update.
5. **Redundant MOVE**: Line 133 copies `CA-Policy-Num` to `WF-Policy-Num` again (already done on line 99).
6. **No success return code**: Return code is not explicitly set to `'00'` on success.

### Relationship to Other Programs
- **Called by**: LGUPDB01 (Update Policy DB2) via `EXEC CICS LINK`.
- **Calls**: LGSTSQ (error logging via LGERRPRC).
- **Sibling programs**: LGAPVS01 (Add Policy VSAM), LGIPVS01 (Inquire Policy VSAM), LGDPVS01 (Delete Policy VSAM).
