# LGIPVS01 - Inquire Policy (VSAM Random Lookup)

## 1. Overview

- **Program ID:** LGIPVS01
- **Purpose:** Return a random policy/customer number from the VSAM KSDS Policy file, filtered by policy type.
- **Summary:** LGIPVS01 reads the VSAM KSDS file `KSDSPOLY` to find a policy record matching a given policy type. The caller supplies a policy type character and optionally a partial key (customer number). The program performs a generic key read with GTEQ positioning to find the first matching record. It can be invoked either from a terminal (transaction) or via `EXEC CICS LINK`, and returns the result accordingly. This program is the policy-file counterpart to LGICVS01, which performs the same function against the customer file.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 75-148)

1. **Initialization** (lines 77-105): Clears `WS-RECV` and uses three `EXEC CICS ASSIGN` commands to determine `SYSID`, `STARTCODE`, and `Invokingprog`. Determines invocation mode:
   - If `STARTCODE` starts with `'D'` or `Invokingprog` is non-spaces: sets `WS-FLAG = 'C'` (COMMAREA mode), copies COMMAREA to `WS-COMMAREA`, and calculates key length as 10 (11 - 1).
   - Otherwise: sets `WS-FLAG = 'R'` (terminal receive mode), receives terminal input, copies data to `WS-COMMAREA`, and subtracts 6 from the receive length.
2. **Key setup** (lines 107-109): Initializes `CA-Area` to spaces. Extracts the policy type character from `WS-Commarea(1:1)` into `Part-Key-Type` and the numeric portion from `WS-Commarea(2:WS-RECV-LEN)` into `Part-Key-Num`.
3. **VSAM read** (lines 111-119): Reads file `KSDSPOLY` using `PART-KEY` as a generic key with `GTEQ` positioning, `KeyLength(F11)` (11 bytes), reading into `CA-AREA` with length `F64` (64 bytes).
4. **Result validation** (lines 121-128): Checks if the returned record's policy type matches `Part-Key-Type` and if `WS-RESP` is normal. If not, sets output to `'Policy Bad='` with error indicator values of 13.
5. **Output** (lines 130-141): If terminal mode, sends result via `EXEC CICS SEND TEXT`. If COMMAREA mode, populates `COMMA-Data` with the text and key.
6. **Return** (lines 143-144): Issues `EXEC CICS RETURN`.

### A-EXIT (lines 146-148)

Exit paragraph with `GOBACK`.

### Control Flow

```
MAINLINE -> Determine invocation mode -> Build key -> Read VSAM GTEQ
         -> Validate result -> Output (SEND or COMMAREA) -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-FLAG` | X | Invocation mode: `'R'` = terminal, `'C'` = COMMAREA |
| `WS-RESP` | S9(8) COMP | CICS response code |
| `WS-STARTCODE` | XX | How the program was started |
| `WS-SYSID` | X(4) | CICS system ID |
| `WS-Invokeprog` | X(8) | Name of invoking program |
| `WS-COMMAREA` | X(80) | Working copy of input data |
| `WS-RECV` / `WS-RECV-TRANID` / `WS-RECV-DATA` | group | Terminal receive buffer |
| `WS-RECV-LEN` | S9(4) COMP | Length of received data (initial 80) |
| `F64` | S9(4) COMP | VSAM read length constant = 64 |
| `F11` | S9(4) COMP | Key length constant = 11 |

### WRITE-MSG Structure (lines 38-44)

Output message: `'Policy Key='` followed by the composite key (type + customer number + policy number) plus 48 bytes of filler.

### PART-KEY (lines 46-48)

Partial key for VSAM generic read: `PART-KEY-Type` (X) + `PART-KEY-Num` (9(10)).

### CA-AREA (lines 49-53)

64-byte area to receive VSAM record data: `CA-POLICY-TYPE` (X), `CA-CUSTOMER-NUM` (X(10)), `CA-POLICY-NUM` (X(10)), plus 43 bytes of filler.

### LINKAGE SECTION

- `DFHCOMMAREA`: Contains `COMMA-DATA` with `Comma-Data-Text` (X(11)), `Comma-Data-Key` (X(21)), and filler.

### Copybooks Included

None. This program has no COPY or INCLUDE statements.

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input

- **Terminal mode:** Transaction ID + space + policy type character + optional customer number.
- **LINK mode:** COMMAREA with policy type in first byte and optional partial key in subsequent bytes.

### Output

- **Terminal mode:** Screen display of `'Policy Key='` followed by the composite key (type + customer number + policy number), or `'Policy Bad='` with error indicators.
- **LINK mode:** COMMAREA populated with `Comma-Data-Text` = `Write-Msg-Text` and `Comma-Data-Key` = `Write-Msg-Key`.

### Files Accessed

| Resource | Type | Operation |
|---|---|---|
| `KSDSPOLY` | VSAM KSDS | READ with GTEQ, Generic key |

### Other Programs Called

None.

## 5. Algorithms and Techniques

### SQL Statements

None. This program does not access DB2.

### CICS Commands

| Command | Line(s) | Purpose |
|---|---|---|
| `EXEC CICS ASSIGN SYSID` | 79-81 | Get system ID |
| `EXEC CICS ASSIGN STARTCODE` | 83-85 | Determine start method |
| `EXEC CICS ASSIGN Invokingprog` | 87-89 | Get invoking program name |
| `EXEC CICS RECEIVE` | 98-101 | Receive terminal input |
| `EXEC CICS Read File('KSDSPOLY')` | 111-119 | Read policy VSAM file with generic GTEQ |
| `EXEC CICS SEND TEXT` | 131-136 | Display result on terminal |
| `EXEC CICS RETURN` | 143-144 | Return to CICS |

### Error Handling Approach

- Very minimal. If the VSAM read returns a non-matching policy type or a non-normal response, the output shows `'Policy Bad='` with `WRITE-Msg-CustNum` and `WRITE-Msg-PolNum` both set to 13. No error logging is performed.
- No COMMAREA validation (no zero-length check, no ABEND).

### Notable Logic Patterns

- **Generic key read** (lines 111-119): Uses `Generic` and `GTEQ` options with an 11-byte key to find the first record at or after the specified key, allowing partial key matching by policy type.
- **Policy type validation** (lines 121-122): After the GTEQ read, the actual record's policy type is compared against the requested type to ensure the read did not skip past all records of the desired type.
- **Dual invocation mode** (lines 90-105): The program supports both terminal-initiated and program-initiated invocation, determined by `STARTCODE` and `Invokingprog`.

## 6. Analysis

### Overall Role

LGIPVS01 provides a VSAM-based policy lookup utility, useful for finding policy records by type and partial key. Like its counterpart LGICVS01, it operates outside the normal DB2-based business logic flow and is primarily used for testing or demonstration purposes.

### Dependencies

- **VSAM file `KSDSPOLY`** must be defined and accessible with an 11-byte key (1 byte type + 10 byte number).

### Known Issues or Limitations

- No error logging or structured error handling. Failed reads produce the cryptic message `'Policy Bad='` with values of 13.
- No COMMAREA validation (no zero-length check).
- The `WS-FLAG-TSQE`, `WS-FLAG-TSQH`, `WS-FLAG-TSQL` variables present in LGICVS01 are absent here -- this program does not manage any TSQ-based configuration range.
- Variables `WS-RANDOM-Seed` (line 24) and `WS-RANDOM-Number` (line 25) are defined but never used. Unlike LGICVS01, this program does not generate random keys.
- `MSGEND` (line 55) is defined but never used.
- In COMMAREA mode, the receive length calculation (lines 94-96) sets `WS-RECV-LEN` to 11 and then subtracts 1, yielding 10. This effectively uses the first 11 bytes (1 type + 10 number) of the input as the key.

### Relationship to Other Programs

- **Standalone:** Operates independently of the business-logic/data-access program pairs.
- **Counterpart:** LGICVS01 performs the same function for customer VSAM records.
- **Related:** LGIPDB01/LGIPOL01 handle policy inquiries via DB2.
