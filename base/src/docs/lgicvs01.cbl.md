# LGICVS01 - Customer Inquire (VSAM Random Lookup)

## 1. Overview

- **Program ID:** LGICVS01
- **Purpose:** Return a random customer number from the VSAM KSDS Customer dataset.
- **Summary:** LGICVS01 generates a random customer number within a known range and reads the corresponding (or next) record from the VSAM KSDS file `KSDSCUST`. The valid customer number range (low and high bounds) is maintained in a CICS Temporary Storage Queue (TSQ) named `GENACNTL`. The program can be invoked either as a CICS transaction (receiving input from the terminal) or via `EXEC CICS LINK` from another program. Results are returned either by screen display or through the COMMAREA.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 93-228)

1. **Initialization** (lines 95-121): Clears `WS-RECV`, then uses three `EXEC CICS ASSIGN` commands to determine `SYSID`, `STARTCODE`, and `Invokingprog`. Based on these, determines whether the program was started via terminal input (`WS-FLAG = 'R'`) or via LINK/DPL (`WS-FLAG = 'C'`). For terminal invocation, receives data via `EXEC CICS RECEIVE`; for LINK invocation, copies the COMMAREA.
2. **Default range setup** (lines 123-127): Initializes `WS-Cust-Low` and `WS-Cust-High` to `0001000001` and sets all TSQ flags (`WS-FLAG-TSQE`, `WS-FLAG-TSQH`, `WS-FLAG-TSQL`) to `'Y'`.
3. **TSQ reading with ENQ/DEQ** (lines 129-191):
   - Acquires an exclusive lock on the TSQ via `EXEC CICS ENQ Resource(STSQ-NAME)`.
   - Reads the first item from TSQ `GENACNTL`. If successful, iterates through remaining items looking for entries starting with `'LOW CUSTOMER'` and `'HIGH CUSTOMER'` to extract the customer number range.
   - If any control entries are missing, writes them to the TSQ.
   - Releases the lock via `EXEC CICS DEQ`.
4. **Random number generation** (lines 193-196): Uses `FUNCTION RANDOM(EIBTASKN)` seeded with the task number to compute a random customer number between `WS-Cust-Low` and `WS-Cust-High` using `FUNCTION INTEGER`.
5. **VSAM read** (lines 199-209): Reads from file `KSDSCUST` using the random number as a key with `GTEQ` (greater than or equal) positioning and `KeyLength(F10)` (10-byte key). If the read is successful, the actual customer number from the record replaces the random number.
6. **Output** (lines 211-225): If invoked from a terminal (`WS-FLAG = 'R'`), sends the result via `EXEC CICS SEND TEXT`. If invoked via LINK (`WS-FLAG = 'C'`), populates the COMMAREA with the result.
7. **Return** (line 224): Issues `EXEC CICS RETURN`.

### A-EXIT (lines 227-229)

Exit paragraph with `GOBACK`.

### Control Flow

```
MAINLINE -> Determine invocation mode -> ENQ -> Read TSQ range -> DEQ
         -> Generate random number -> Read VSAM GTEQ -> Output result -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-FLAG-TSQE` | X | Flag: `'Y'` if TSQ needs header entry written |
| `WS-FLAG-TSQH` | X | Flag: `'Y'` if TSQ needs HIGH CUSTOMER entry written |
| `WS-FLAG-TSQL` | X | Flag: `'Y'` if TSQ needs LOW CUSTOMER entry written |
| `WS-FLAG` | X | Invocation mode: `'R'` = terminal receive, `'C'` = COMMAREA/LINK |
| `WS-RANDOM-Seed` | S9(4) COMP | Random seed (unused directly; EIBTASKN is used) |
| `WS-RANDOM-Number` | 9(8) COMP | Computed random customer number |
| `WS-RESP` | S9(8) COMP | CICS command response code |
| `WS-STARTCODE` | XX | CICS start code (e.g., `'D'` for DPL) |
| `WS-SYSID` | X(4) | CICS system ID |
| `WS-Invokeprog` | X(8) | Name of invoking program |
| `WS-Cust-Low` | S9(10) | Low bound of customer number range |
| `WS-Cust-High` | S9(10) | High bound of customer number range |
| `WS-Cust-Number` | X(10) | Customer number (defined but not used) |
| `STSQ-NAME` | X(8) | TSQ name = `'GENACNTL'` |
| `F82` | S9(4) COMP | Length constant = 225 for VSAM read |
| `F10` | S9(4) COMP | Key length constant = 10 |

### READ-MSG Structure (lines 40-47)

An 80-byte area with REDEFINES to parse TSQ entries:
- First REDEFINES: extracts `READ-CUST-LOW` (PIC 9(10)) at position 14.
- Second REDEFINES: extracts `READ-CUST-HIGH` (PIC 9(10)) at position 15.

### WRITE-MSG Structure (lines 54-61)

Output message containing the control header `'**** GENAPP CNTL'`, low customer text/value, and high customer text/value.

### CA-AREA (lines 69-71)

A 225-byte area for VSAM record data with `CA-CUSTOMER-NUM` (X(10)) at the start.

### LINKAGE SECTION

- `DFHCOMMAREA`: Contains `COMMA-DATA` with `Comma-Data-H` (X(14)), `Comma-Data-High` (9(10)), and filler.

### Copybooks Included

None. This program does not use COPY or INCLUDE statements.

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input

- **Terminal mode:** Transaction data received via `EXEC CICS RECEIVE` into `WS-RECV`.
- **LINK mode:** COMMAREA passed via DFHCOMMAREA. The COMMAREA content is copied to `WS-COMMAREA`.

### Output

- **Terminal mode:** Result displayed via `EXEC CICS SEND TEXT` showing `WRITE-MSG-H` (14 bytes: `'HIGH CUSTOMER='` followed by the customer number).
- **LINK mode:** COMMAREA populated with `COMMA-Data-H` = `Write-Msg-H` and `COMMA-Data-High` = `Write-Msg-High`.

### Files Accessed

| Resource | Type | Operation |
|---|---|---|
| `KSDSCUST` | VSAM KSDS | READ with GTEQ (generic key) |
| `GENACNTL` | CICS TSQ | READQ TS (read), WRITEQ TS (write) |

### Other Programs Called

None.

## 5. Algorithms and Techniques

### SQL Statements

None. This program does not access DB2.

### CICS Commands

| Command | Line(s) | Purpose |
|---|---|---|
| `EXEC CICS ASSIGN SYSID` | 97-99 | Get current system ID |
| `EXEC CICS ASSIGN STARTCODE` | 101-103 | Determine how program was started |
| `EXEC CICS ASSIGN Invokingprog` | 105-107 | Get name of invoking program |
| `EXEC CICS RECEIVE` | 114-117 | Receive terminal input |
| `EXEC CICS ENQ Resource(STSQ-NAME)` | 129-131 | Acquire exclusive lock on TSQ |
| `EXEC CICS READQ TS` | 132-136, 140-144 | Read items from TSQ |
| `EXEC CICS WRITEQ TS` | 163-168, 172-177, 181-186 | Write control entries to TSQ |
| `EXEC CICS DEQ Resource(STSQ-NAME)` | 189-191 | Release lock on TSQ |
| `EXEC CICS Read File('KSDSCUST')` | 199-206 | Read VSAM KSDS file with GTEQ |
| `EXEC CICS SEND TEXT` | 212-217 | Display result on terminal |
| `EXEC CICS RETURN` | 224-225 | Return to CICS |

### Error Handling Approach

- Error handling is minimal. The program checks `WS-RESP` against `DFHRESP(NORMAL)` for TSQ and VSAM operations but does not log errors or return error codes.
- If the VSAM read fails or returns a record with a different key type, the output shows `'Policy Bad='` with values of 13 (lines 123-125), which appears to be a debugging artifact.

### Notable Logic Patterns

- **ENQ/DEQ serialization** (lines 129-191): The TSQ read/write is wrapped in ENQ/DEQ to prevent concurrent updates to the control range.
- **Random number generation** (lines 193-196): Uses the intrinsic function `FUNCTION RANDOM(EIBTASKN)` seeded by the task number to generate a pseudo-random number in the range [WS-Cust-Low, WS-Cust-High].
- **GTEQ read** (line 205): The VSAM read uses GTEQ so that if the exact random key does not exist, the next higher record is returned.
- **TSQ self-initialization** (lines 162-187): If the TSQ does not exist or is missing entries, the program creates/populates it automatically.

## 6. Analysis

### Overall Role

LGICVS01 provides a random customer number lookup capability, which is useful for testing, demonstration, and load generation purposes. It reads from the VSAM KSDS customer file rather than DB2, making it suitable for scenarios where quick random access is needed without database overhead.

### Dependencies

- **VSAM file `KSDSCUST`** must be defined and accessible with a 10-byte key.
- **TSQ `GENACNTL`** should exist with LOW CUSTOMER and HIGH CUSTOMER entries for meaningful random ranges. If absent, defaults to `0001000001`.
- No copybooks or other programs are required.

### Known Issues or Limitations

- The variable `WS-Cust-Number` (line 51) is defined but never used.
- The variable `WS-RANDOM-Seed` (line 28) is defined but never used (EIBTASKN is used directly as the seed).
- Error handling is weak: a failed VSAM read results in displaying `'Policy Bad='` with the value 13, which is not a meaningful error indicator.
- The REDEFINES on `READ-MSG` for parsing LOW and HIGH customer entries rely on fixed string positions, which is fragile.
- When invoked from a terminal, the transaction ID is consumed from the first 5 bytes of input (`WS-RECV-TRANID`), and 5 is subtracted from the length (line 120).
- The default range (`0001000001` to `0001000001`) means that if the TSQ has no entries, only one customer number will ever be generated.

### Relationship to Other Programs

- **Standalone:** This program operates independently and does not call or get called by the standard business-logic/data-access programs in the GenApp flow.
- **Parallel:** LGIPVS01 follows the same pattern but for policy records using the `KSDSPOLY` file.
- **Related:** LGICDB01/LGICUS01 handle customer inquiries via DB2 rather than VSAM.
