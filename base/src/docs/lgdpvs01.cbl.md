# LGDPVS01 - Delete Policy VSAM Record

## 1. Overview

- **Program ID:** LGDPVS01
- **Purpose:** Deletes a policy record from a VSAM KSDS file named `KSDSPOLY`.
- **Summary:** LGDPVS01 is a low-level CICS COBOL program that deletes a policy record from the VSAM key-sequenced data set `KSDSPOLY`. It constructs a 21-byte composite key from the request ID type, customer number, and policy number, then issues a CICS DELETE command. This program is called by LGDPDB01 after the corresponding DB2 delete has been performed, ensuring both data stores remain synchronized.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION (lines 68-93)
1. **Line 71:** Saves `EIBCALEN` into `WS-Commarea-Len`.
2. **Lines 73-75:** Extracts the 4th character of `CA-Request-ID` as `WF-Request-ID` (policy type indicator), copies `CA-Policy-Num` and `CA-Customer-Num` into the working-storage key structure.
3. **Lines 77-81:** Issues `EXEC CICS DELETE FILE('KSDSPOLY') RIDFLD(WF-Policy-Key) KEYLENGTH(21)`.
4. **Lines 82-87:** If the DELETE response is not NORMAL, captures `EIBRESP2`, sets return code `'81'`, writes error message, and returns (no abend, unlike the update VSAM programs).

#### A-EXIT (lines 91-93)
Normal exit point with `EXIT` and `GOBACK`.

#### WRITE-ERROR-MESSAGE (lines 98-111)
Formats time, populates customer number, policy number, and CICS response codes, then writes error message and commarea to TSQ.

### Control Flow Summary
```
MAINLINE
  -> Save EIBCALEN
  -> Build composite key (type + customer + policy)
  -> DELETE KSDSPOLY record by key
     -> On failure: error '81', RETURN
  -> A-EXIT (GOBACK)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | PIC | Role |
|---|---|---|
| `WS-RESP` (line 18) | S9(8) COMP | CICS response code |
| `WS-RESP2` (line 19) | S9(8) COMP | CICS response2 code |
| `WS-Comm-Len` (line 20) | S9(8) COMP | General length (unused) |
| `WS-STARTCODE` (line 21) | XX | Start code (initialized to spaces, unused) |
| `WS-SYSID` (line 22) | X(4) | System ID (initialized to spaces, unused) |
| `WS-Commarea-Len` (line 23) | S9(4) COMP | Receives EIBCALEN |
| `WF-Policy-Info` (line 25) | Group | Policy key structure |
| `WF-Policy-Key` (line 26) | Group (21 bytes) | Composite key: type (1) + customer (10) + policy (10) |
| `Eyecatcher` (line 54) | X(16) VALUE 'Program LGDPVS01' | Debug eyecatcher |

### WF-Policy-Info Structure
Unlike the update counterpart (LGUPVS01), the delete program's `WF-Policy-Info` only contains the key fields -- no policy data area is needed since the record is being deleted.

### ERROR-MSG Structure (lines 36-51)
Contains: date, time, program name `LGDPVS01`, policy number, customer number, operation description `Delete file KSDSPOLY`, RESP and RESP2 codes.

### LINKAGE SECTION
- **DFHCOMMAREA** (line 60): Uses `COPY LGCMAREA`.

### Copybooks Included
| Copybook | Location | Purpose |
|---|---|---|
| `LGERR` (line 34) | WORKING-STORAGE | Common error handling fields |
| `LGCMAREA` (line 61) | LINKAGE SECTION | COMMAREA structure |
| `LGERRPRC` (line 116) | PROCEDURE DIVISION | Common error handling procedures |

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-Request-ID`: Character at position 4 (1-byte) is the policy type indicator used as part of the VSAM key.
- `CA-Customer-Num`: 10-byte customer number (part of VSAM key).
- `CA-Policy-Num`: 10-byte policy number (part of VSAM key).

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'81'` on DELETE failure. Not set on success.

### Files Accessed
| File | Type | Operation | Key |
|---|---|---|---|
| `KSDSPOLY` | VSAM KSDS | DELETE | WF-Policy-Key (21 bytes: type + customer + policy) |

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| LGSTSQ | EXEC CICS LINK (via LGERRPRC) | Error logging |

## 5. Algorithms and Techniques

### CICS Commands
1. **`EXEC CICS DELETE FILE('KSDSPOLY') RIDFLD(WF-Policy-Key) KEYLENGTH(21) RESP(WS-RESP)`** (line 77): Deletes the VSAM record by its composite key.
2. **`EXEC CICS RETURN`** (line 86): Returns on error.

### Error Handling
- Checks `WS-RESP` against `DFHRESP(NORMAL)`.
- On failure: captures EIBRESP2, sets return code `'81'`, writes error, returns.
- Unlike the update VSAM programs (LGUCVS01, LGUPVS01), this program does NOT abend on failure -- it returns with an error code. This is a design improvement.

### Notable Logic Patterns
- **Simpler than update programs**: Only one CICS file command (DELETE) is needed, versus the READ-for-UPDATE + REWRITE pattern used by update programs.
- **Same composite key structure**: Uses the same 21-byte key format as LGUPVS01: `type(1) + customer(10) + policy(10)`.

## 6. Analysis

### Overall Role
LGDPVS01 is the VSAM data access layer for policy deletions, the final step in the delete chain:
```
LGDPOL01 (business logic) -> LGDPDB01 (DB2 delete) -> LGDPVS01 (VSAM delete)
```

### Dependencies
- **VSAM file `KSDSPOLY`**: Must be defined and accessible.
- **Valid COMMAREA**: Must contain the correct type indicator, customer number, and policy number.
- **Copybooks**: LGCMAREA, LGERR, LGERRPRC.

### Known Issues or Limitations
1. **No COMMAREA validation**: Like other low-level VSAM programs, it does not validate EIBCALEN.
2. **No success return code**: Does not explicitly set `CA-RETURN-CODE` to `'00'` on success.
3. **VSAM record-not-found treated as error**: If the VSAM record doesn't exist (RESP = NOTFND), it is treated as an error with return code `'81'`. Since this is called after a DB2 delete, it might be appropriate to treat NOTFND as success (the record is already gone).
4. **Unused variables**: `WS-Comm-Len`, `WS-STARTCODE`, and `WS-SYSID` are declared but never used.

### Relationship to Other Programs
- **Called by**: LGDPDB01 (Delete Policy DB2) via `EXEC CICS LINK`.
- **Calls**: LGSTSQ (error logging via LGERRPRC).
- **Sibling programs**: LGAPVS01 (Add Policy VSAM), LGIPVS01 (Inquire Policy VSAM), LGUPVS01 (Update Policy VSAM).
