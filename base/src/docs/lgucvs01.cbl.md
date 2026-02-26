# LGUCVS01 - Update Customer VSAM Record

## 1. Overview

- **Program ID:** LGUCVS01
- **Purpose:** Updates a customer record in a VSAM KSDS (Key-Sequenced Data Set) file named `KSDSCUST`.
- **Summary:** LGUCVS01 is a low-level CICS COBOL program that performs a VSAM READ-for-UPDATE followed by a REWRITE operation on a customer record. It receives a communication area (COMMAREA) containing the customer number as a key and the updated customer data. The program reads the existing record to obtain an update lock, then rewrites it with the new data from the COMMAREA. On failure, it writes diagnostic error messages to a temporary storage queue and abends with specific abend codes.

## 2. Functional Breakdown

### PROCEDURE DIVISION Structure

#### MAINLINE SECTION (lines 60-98)
This is the primary entry point. The control flow is:

1. **Line 63:** Copies `EIBCALEN` (length of the incoming COMMAREA) into `WS-Commarea-Len`.
2. **Lines 65-72:** Issues `EXEC CICS READ` on file `KSDSCUST` with the `Update` option. This reads the current record into `WS-Customer-Area` and places an exclusive lock on it. The key used is `CA-Customer-Num` with a key length of 10.
3. **Lines 73-79:** If the READ fails (response is not NORMAL), sets return code `'81'`, performs `WRITE-ERROR-MESSAGE`, issues `EXEC CICS ABEND ABCODE('LGV1')`, and returns.
4. **Lines 81-85:** Issues `EXEC CICS REWRITE` on file `KSDSCUST`, writing the updated data from `CA-Customer-Num` (the start of the COMMAREA policy/customer data area) with a length of 225 bytes.
5. **Lines 86-92:** If the REWRITE fails, sets return code `'82'`, performs `WRITE-ERROR-MESSAGE`, issues `EXEC CICS ABEND ABCODE('LGV2')`, and returns.

#### A-EXIT (lines 96-98)
Normal exit point. Contains `EXIT` and `GOBACK` statements.

#### WRITE-ERROR-MESSAGE (lines 103-115)
Error logging procedure:
1. Calls `LGERR-FORMAT-TIME` to capture current date/time.
2. Moves formatted date and time into `EM-DATE` and `EM-TIME`.
3. Populates error-specific fields: `EM-Cusnum` (customer number), `EM-RespRC` (CICS response code), `EM-Resp2RC` (CICS response2 code).
4. Calls `LGERR-WRITE-MSG` to write the error message to a TSQ.
5. Calls `LGERR-LOG-COMMAREA` to log the first 90 bytes of the COMMAREA.

#### Copybook LGERRPRC (included at line 120)
Provides the common error handling procedures: `LGERR-FORMAT-TIME`, `LGERR-WRITE-MSG`, `LGERR-LOG-COMMAREA`.

### Control Flow Summary
```
MAINLINE SECTION
  -> READ KSDSCUST (with Update lock)
     -> On failure: error '81', ABEND 'LGV1'
  -> REWRITE KSDSCUST
     -> On failure: error '82', ABEND 'LGV2'
  -> A-EXIT (GOBACK)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | PIC | Role |
|---|---|---|
| `WS-RESP` (line 18) | S9(8) COMP | Stores CICS RESP code from file operations |
| `WS-RESP2` (line 19) | S9(8) COMP | Stores EIBRESP2 on error |
| `WS-Comm-Len` (line 20) | S9(8) COMP | General communication length (unused in code) |
| `WS-STARTCODE` (line 21) | XX | Start code (initialized to spaces, unused) |
| `WS-SYSID` (line 22) | X(4) | System ID (initialized to spaces, unused) |
| `WS-Commarea-Len` (line 23) | S9(4) COMP | Receives EIBCALEN for READ length |
| `WS-Customer-Area` (line 24) | X(1024) | Buffer for reading the existing customer record |
| `CUSTOMER-RECORD-SIZE` (line 46) | S9(4) BINARY VALUE 0225 | Fixed record size (225 bytes) for REWRITE |

### ERROR-MSG Structure (lines 31-44)
A formatted error message containing date, time, program name `LGUCVS01`, customer number, operation description, and CICS response codes.

### LINKAGE SECTION
- **DFHCOMMAREA** (line 52): Defined via `COPY LGCMAREA` (line 53). Contains the standard GenApp COMMAREA with `CA-REQUEST-ID`, `CA-RETURN-CODE`, `CA-CUSTOMER-NUM`, and the customer/policy-specific data area.

### Copybooks Included
| Copybook | Location | Purpose |
|---|---|---|
| `LGERR` (line 29) | WORKING-STORAGE | Common error handling work fields (`WS-ERR-WORK-FIELDS`, `CA-ERROR-MSG`, `WS-ERR-LOG-PGM`) |
| `LGCMAREA` (line 53) | LINKAGE SECTION | COMMAREA structure defining request/response fields |
| `LGERRPRC` (line 120) | PROCEDURE DIVISION | Common error handling procedures (`LGERR-FORMAT-TIME`, `LGERR-WRITE-MSG`, `LGERR-LOG-COMMAREA`) |

### Condition Names (88-levels)
None defined directly in this program. The LGCMAREA copybook may contain condition names on `CA-REQUEST-ID` and `CA-RETURN-CODE`.

## 4. Input/Output Behavior

### Input (COMMAREA)
- **CA-Customer-Num** (PIC 9(10)): The VSAM record key identifying the customer.
- The COMMAREA data starting at `CA-Customer-Num` contains the full updated customer record (225 bytes).
- `EIBCALEN`: Used as the initial read length.

### Output (COMMAREA)
- **CA-RETURN-CODE**: Set to `'81'` on READ failure, `'82'` on REWRITE failure. Not explicitly set on success (remains as passed in).

### Files Accessed
| File | Type | Operation | Purpose |
|---|---|---|---|
| `KSDSCUST` | VSAM KSDS | READ (with Update) | Lock existing customer record |
| `KSDSCUST` | VSAM KSDS | REWRITE | Write updated customer data |

### Programs Called
| Program | Method | Purpose |
|---|---|---|
| `LGSTSQ` | EXEC CICS LINK (via LGERRPRC) | Write error messages to a temporary storage queue |

## 5. Algorithms and Techniques

### CICS Commands Used
1. **`EXEC CICS READ FILE('KSDSCUST') INTO(...) RIDFLD(CA-Customer-Num) KEYLENGTH(10) UPDATE`** (line 65): Reads and locks the record for update.
2. **`EXEC CICS REWRITE FILE('KSDSCUST') FROM(CA-Customer-Num) LENGTH(CUSTOMER-RECORD-SIZE)`** (line 81): Writes the updated record back, releasing the lock.
3. **`EXEC CICS ABEND ABCODE('LGV1') NODUMP`** (line 77): Abends on READ failure.
4. **`EXEC CICS ABEND ABCODE('LGV2') NODUMP`** (line 90): Abends on REWRITE failure.
5. **`EXEC CICS RETURN`** (lines 78, 91): Returns control to the calling program after abend.
6. **`EXEC CICS ASKTIME` / `EXEC CICS FORMATTIME`** (via LGERRPRC): Gets current timestamp for error messages.
7. **`EXEC CICS LINK PROGRAM('LGSTSQ')`** (via LGERRPRC): Writes error data to TSQ.

### Error Handling Approach
- Each VSAM operation checks `WS-RESP` against `DFHRESP(NORMAL)`.
- On failure, the program captures `EIBRESP2`, sets a specific return code (`'81'` or `'82'`), writes a diagnostic message, abends with a unique abend code, and returns.
- The error message includes timestamp, program name, customer number, and CICS response codes.

### Notable Logic Patterns
- The READ-for-UPDATE / REWRITE pattern is the standard VSAM update sequence. The READ with `Update` option acquires an exclusive lock that is released by the REWRITE.
- The program rewrites from `CA-Customer-Num` (the start of customer data in the COMMAREA), not from `WS-Customer-Area`, meaning the caller provides the complete new record content.

## 6. Analysis

### Overall Role in the Application Architecture
LGUCVS01 is a low-level data access program in the GenApp insurance application. It is called by higher-level business logic programs (such as LGUCUS01) to persist customer updates to the VSAM data store. It handles only the VSAM file I/O portion of the customer update operation.

### Dependencies
- **VSAM file `KSDSCUST`**: Must be defined and accessible in the CICS region.
- **COMMAREA**: Must contain a valid customer number and updated record data (at least 225 bytes).
- **Program `LGSTSQ`**: Must be available for error logging.
- **Copybooks**: `LGCMAREA`, `LGERR`, `LGERRPRC`.

### Known Issues or Limitations
1. **No commarea length validation**: The program does not check whether `EIBCALEN` is large enough to contain a valid customer record before proceeding with the READ and REWRITE. A short COMMAREA could cause unexpected behavior.
2. **No success return code**: The program never explicitly sets `CA-RETURN-CODE` to `'00'` on success. It relies on the caller having pre-set it.
3. **ABEND on error**: The program abends (with NODUMP) on any file operation failure rather than returning gracefully with an error code, which could disrupt the calling program's flow.
4. **Unused variables**: `WS-Comm-Len`, `WS-STARTCODE`, and `WS-SYSID` are declared but never used.

### Relationship to Other Programs
- **Called by**: LGUCUS01 (Update Customer business logic program) via `EXEC CICS LINK`.
- **Calls**: LGSTSQ (error logging) via the LGERRPRC copybook.
- **Parallel to**: LGACVS01 (Add Customer VSAM), LGICVS01 (Inquire Customer VSAM) -- these are sibling programs for different CRUD operations on the same VSAM file.
