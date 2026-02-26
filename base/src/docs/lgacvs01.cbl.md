# LGACVS01.cbl - Add Customer VSAM Record

## 1. Overview

- **Program ID:** LGACVS01
- **Purpose:** Write a new customer record to the VSAM KSDS file `KSDSCUST`.
- **Summary:** LGACVS01 is a VSAM data-access program that writes a customer record to the KSDS (Key-Sequenced Data Set) file `KSDSCUST`. It receives the customer data via the commarea (LGCMAREA structure), writes 225 bytes starting from `CA-Customer-Num` as the record with the customer number as the key, and handles any VSAM write errors by setting return code `'80'` and abending with code `'LGV0'`. This program is called by LGACDB01 as part of the customer creation workflow to maintain a VSAM mirror of the DB2 customer data.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 60-84)

1. **Save commarea length (line 63):** Moves `EIBCALEN` to `WS-Commarea-Len`.
2. **VSAM WRITE (lines 65-71):**
   ```cobol
   Exec CICS Write File('KSDSCUST')
        From(CA-Customer-Num)
        Length(CUSTOMER-RECORD-SIZE)
        Ridfld(CA-Customer-Num)
        KeyLength(10)
        RESP(WS-RESP)
   End-Exec.
   ```
   Writes 225 bytes starting from `CA-Customer-Num` (which includes the customer number and all customer detail fields) using the 10-byte customer number as the KSDS key.
3. **Error handling (lines 72-78):** If response is not normal, captures `EIBRESP2`, sets return code `'80'`, writes error, abends with `'LGV0'`, and returns.

### A-EXIT (lines 82-84)

Standard exit with `EXIT` and `GOBACK`.

### WRITE-ERROR-MESSAGE (lines 89-101)

1. Calls `LGERR-FORMAT-TIME` to get current timestamp.
2. Moves formatted date/time to error message fields.
3. Populates VSAM-specific error fields: customer number, RESP code, RESP2 code.
4. Writes error via `LGERR-WRITE-MSG` and logs commarea via `LGERR-LOG-COMMAREA`.

### Control Flow

```
MAINLINE -> WRITE File('KSDSCUST')
         -> If error: WRITE-ERROR-MESSAGE -> ABEND -> RETURN
         -> A-EXIT (GOBACK)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-RESP` (line 18) | `PIC S9(8) COMP` | CICS response code from WRITE |
| `WS-RESP2` (line 19) | `PIC S9(8) COMP` | CICS RESP2 value |
| `WS-Comm-Len` (line 20) | `PIC S9(8) COMP` | Declared but unused |
| `WS-STARTCODE` (line 21) | `PIC XX` | Declared but unused |
| `WS-SYSID` (line 22) | `PIC X(4)` | Declared but unused |
| `WS-Commarea-Len` (line 23) | `PIC S9(4) COMP` | Stores EIBCALEN |
| `ERROR-MSG` (line 31) | Group | Error message with VSAM-specific fields (customer number, RESP, RESP2, file name) |
| `CUSTOMER-RECORD-SIZE` (line 46) | `PIC S9(4) BINARY` | Record length constant: 225 bytes |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 52): Defined via `Copy LGCMAREA`. The write starts at `CA-Customer-Num` (offset within the commarea).

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGERR` (line 28) | `COPY LGERR` | Common error handling working storage |
| `LGCMAREA` (line 53) | `Copy LGCMAREA` | Commarea field definitions |
| `LGERRPRC` (line 106) | `COPY LGERRPRC` | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-Customer-Num`: The 10-byte customer number, used both as the record key and the start of the record data.
- Customer detail fields following `CA-Customer-Num` in the commarea (first name, last name, DOB, address, etc.) comprise the rest of the 225-byte record.

### Output (COMMAREA)
- `CA-RETURN-CODE`: Set to `'80'` on VSAM write error. Not modified on success.

### Files Accessed
- **VSAM KSDSCUST** -- WRITE (line 65). Key-Sequenced Data Set for customer records. Key is `CA-Customer-Num` (10 bytes), record length is 225 bytes.

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGSTSQ` (via LGERRPRC) | Write error messages to temporary storage queue |

## 5. Algorithms and Techniques

### SQL Statements

None. This is a pure VSAM program with no DB2 access.

### CICS Commands
- `EXEC CICS Write File('KSDSCUST')` -- Write customer record to VSAM KSDS
- `EXEC CICS ABEND ABCODE('LGV0') NODUMP` -- Abend on VSAM write failure
- `EXEC CICS RETURN` -- Return to caller (after abend path)

### Error Handling
- VSAM write errors are detected by checking `WS-RESP` against `DFHRESP(NORMAL)`.
- On error: captures `EIBRESP2` for diagnostic detail, sets return code `'80'`, writes error message to TSQ, then abends with `'LGV0'`.
- The abend causes CICS to back out any recoverable resources in the current unit of work.
- After the abend, a `RETURN` is coded but would not execute if the abend is successful.

### Notable Logic Patterns
- **Direct commarea-to-file write:** The record written to VSAM starts at `CA-Customer-Num` within the commarea, meaning the first 225 bytes of the commarea from that point are written directly. This avoids copying to a separate record buffer.
- **Abend on error:** Unlike programs that simply return with an error code, this program abends to trigger UOW backout, ensuring consistency with the DB2 insert performed by the caller (LGACDB01).

## 6. Analysis

### Overall Role
LGACVS01 provides the VSAM data-access layer for customer records, maintaining a KSDS mirror of the DB2 CUSTOMER table. In the GenApp architecture, both DB2 and VSAM stores are updated during customer creation for redundancy or to support different access patterns.

### Dependencies
- VSAM file `KSDSCUST` must be defined and available in the CICS region.
- Commarea must contain a valid customer number and details at the expected offsets.
- Copybooks: LGERR, LGERRPRC, LGCMAREA

### Known Issues or Limitations
1. **No commarea validation:** Unlike most other programs in this suite, LGACVS01 does not check `EIBCALEN` for zero or minimum length. It relies entirely on the caller (LGACDB01) to have validated the commarea.
2. **Unused variables:** `WS-Comm-Len`, `WS-STARTCODE`, and `WS-SYSID` are declared but never used.
3. **Hardcoded record size:** The 225-byte `CUSTOMER-RECORD-SIZE` is hardcoded rather than derived from the copybook structures, creating a maintenance risk if the record layout changes.
4. **GOBACK after EXIT:** The `GOBACK` at line 84 is unreachable because `EXIT` precedes it and the normal flow would have already returned via CICS LINK return mechanics.

### Relationship to Other Programs
- **Called by:** LGACDB01 (customer creation), LGACDB01_SECURE (secure customer creation)
- **Peer:** LGAPVS01 (same pattern for policy VSAM writes)
- **Related:** LGUCVS01 (update customer VSAM), LGDPVS01 (delete customer VSAM)
