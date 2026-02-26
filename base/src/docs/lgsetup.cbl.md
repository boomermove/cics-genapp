# lgsetup.cbl - GenApp Environment Setup and Counter Initialization

## 1. Overview

- **Program ID:** LGSETUP
- **Purpose:** Initializes or resets the GenApp runtime environment by deleting and recreating TSQs and named counters used for statistics tracking and customer number generation.

LGSETUP is a setup/administration utility designed to be run as a CICS transaction (likely manually or during application deployment). It deletes the `GENAERRS`, `GENASTRT`, `GENASTAT`, and `GENACNTL` TSQs, writes fresh control records to `GENACNTL` with low/high customer number boundaries, and then deletes and redefines all named counters in the `GENA` pool. This includes the `GENACUSTNUM` counter (used for customer number generation) and approximately 38 additional counters used by the statistics subsystem (LGASTAT1/LGWEBST5). The high customer number can be passed as terminal input.

## 2. Functional Breakdown

### MAINLINE SECTION (line 126)

The program executes linearly with no sub-paragraphs or PERFORM statements.

**Lines 128-135 - Terminal Input:**
Issues `EXEC CICS RECEIVE` to read up to 80 bytes of terminal input. If the received data is longer than 5 characters (i.e., has data beyond the transaction ID), the extra portion is parsed as the `LastCustNum` value, overriding the default of 11.

**Lines 138-152 - TSQ Cleanup:**
Deletes four TSQs in sequence, ignoring errors:
- `GENAERRS` (error log queue)
- `GENASTRT` (startup timestamp)
- `GENASTAT` (statistics data)
- `GENACNTL` (control parameters - customer boundaries)

**Lines 154-176 - GENACNTL Initialization:**
Writes three records to the `GENACNTL` TSQ:
1. Header: `'**** GENAPP CNTL'` (20 bytes)
2. Low customer: `'LOW CUSTOMER='` + `FrstCustNum` (23 bytes)
3. High customer: `'HIGH CUSTOMER='` + `LastCustNum` (24 bytes)

**Lines 179-187 - GENACUSTNUM Counter:**
Deletes and redefines the `GENACUSTNUM` counter in the `GENA` pool with the value of `LastCustNum`. This counter is used to generate the next customer number.

**Lines 189-516 - Statistics Counters:**
Deletes and redefines 38 named counters, all initialized to 0. These counters track success/failure counts for every operation type:
- `GENA01ICUS00`/`99` - Inquire Customer success/error
- `GENA01ACUS00`/`99` - Add Customer success/error
- `GENA01IMOT00`/`99` - Inquire Motor success/error
- `GENA01AMOT00`/`99` - Add Motor success/error
- `GENA01DMOT00`/`99` - Delete Motor success/error
- `GENA01UMOT00`/`99` - Update Motor success/error
- `GENA01IEND00`/`99` - Inquire Endowment success/error
- `GENA01AEND00`/`99` - Add Endowment success/error
- `GENA01DEND00`/`99` - Delete Endowment success/error
- `GENA01UEND00`/`99` - Update Endowment success/error
- `GENA01IHOU00`/`99` - Inquire House success/error
- `GENA01AHOU00`/`99` - Add House success/error
- `GENA01DHOU00`/`99` - Delete House success/error
- `GENA01UHOU00`/`99` - Update House success/error
- `GENA01ICOM00`/`99` - Inquire Commercial success/error
- `GENA01ACOM00`/`99` - Add Commercial success/error
- `GENA01DCOM00`/`99` - Delete Commercial success/error
- `GENA01UCUS00`/`99` - Update Customer success/error

**Lines 520-525 - Terminal Output:**
Sends the high customer number string to the terminal using `EXEC CICS SEND TEXT`.

**Lines 527-528 - Return:**
Issues `EXEC CICS RETURN`.

#### A-EXIT (line 530)
Standard exit with `EXIT` and `GOBACK`.

### Control Flow
```
Entry -> RECEIVE terminal input -> Delete TSQs -> Write GENACNTL
      -> Delete/Define GENACUSTNUM counter
      -> Delete/Define 38 statistics counters
      -> SEND TEXT to terminal -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `WS-FLAG-TSQE/H/L`, `WS-FLAG` | 20-23 | Flags (defined but unused in procedure) |
| `WS-RESP` | 24 | CICS response code |
| `WS-RECV` | 25-27 | Terminal receive buffer: 5-byte TRANID + 74-byte data |
| `WS-RECV-LEN` | 28 | Receive length, initialized to 80 |
| `READ-MSG` / REDEFINES | 30-37 | Message parsing with `READ-CUST-LOW` (offset 13) and `READ-CUST-HIGH` (offset 14) |
| `WS-Cust-Low/High/Number` | 39-41 | Customer number work fields (unused in procedure) |
| `WRITE-MSG` | 43-50 | TSQ write buffer with header, low/high customer values |
| `FrstCustNum` | 52 | First customer number, default `+0000001` |
| `LastCustNum` | 53 | Last customer number, default `+0000011` (COMP) |
| `GENAcount` | 54 | Counter name `'GENACUSTNUM'` |
| `GENApool` | 55 | Counter pool `'GENA'` |
| `STSQ-STRT/STAT/ERRS` | 56-58 | TSQ names: `'GENASTRT'`, `'GENASTAT'`, `'GENAERRS'` |
| `GENACNT100`-`GENACNTI99` | 59-94 | 38 named counter identifiers for statistics |
| `STSQ` / STSQ-NAME | 95-99 | Control TSQ name `'GENACNTL'` with redefine for extension |
| `CA-AREA` | 102-104 | 82-byte area with customer number (unused) |
| `MSGEND` | 106 | End message (unused) |

### LINKAGE SECTION
- **DFHCOMMAREA** (line 115): Contains `Comma-Data-H` (X(14)), `Comma-Data-High` (9(10)), and filler. This is a minimal commarea, though the program primarily uses terminal input rather than the commarea.

### Copybooks
None.

### Condition Names
None defined.

## 4. Input/Output Behavior

### Input
- **Terminal input** (line 128): The transaction ID plus an optional customer number. If data beyond the 5-byte transaction ID is present, it overrides the default `LastCustNum` value of 11.

### Output
- **GENACNTL TSQ**: Three records written: header, low customer boundary, high customer boundary.
- **GENA pool counters**: `GENACUSTNUM` initialized to `LastCustNum`; 38 statistics counters initialized to 0.
- **Terminal**: Sends the high customer text (24 chars) to the terminal.

### Files Accessed
| Resource | Type | Operation |
|---|---|---|
| `GENAERRS` | CICS TSQ | DELETE |
| `GENASTRT` | CICS TSQ | DELETE |
| `GENASTAT` | CICS TSQ | DELETE |
| `GENACNTL` | CICS TSQ | DELETE, WRITE (3 records) |
| `GENACUSTNUM` + 38 others | CICS Named Counter (GENA pool) | DELETE, DEFINE |

### Other Programs Called
None.

## 5. Algorithms and Techniques

### SQL Statements
None.

### CICS Commands
| Command | Lines | Purpose |
|---|---|---|
| `EXEC CICS RECEIVE` | 128 | Read terminal input for customer number parameter |
| `EXEC CICS DeleteQ TS` | 138-152 | Delete four TSQs |
| `EXEC CICS WRITEQ TS` | 157-176 | Write three GENACNTL records |
| `EXEC CICS Delete Counter` | 179+ (repeated) | Delete existing named counters |
| `EXEC CICS Define Counter` | 183+ (repeated) | Create named counters with initial values |
| `EXEC CICS SEND TEXT` | 520 | Display high customer number on terminal |
| `EXEC CICS RETURN` | 527 | Return to CICS |

### Error Handling
All CICS commands use `RESP(WS-RESP)` but the response is never checked. Errors (such as deleting a non-existent TSQ or counter) are silently ignored. This is intentional since the delete operations are meant to clean up resources that may or may not exist.

### Notable Logic Patterns
- **Delete-before-Define pattern**: Every counter is first deleted then defined. This ensures a clean reset regardless of prior state.
- **Repetitive counter setup**: The 38 counter delete/define pairs (lines 189-516) follow an identical pattern. This could have been implemented with a loop but is instead coded as inline repetition.

## 6. Analysis

### Overall Role
LGSETUP is a deployment/administration utility that initializes the GenApp runtime environment. It must be run before the application can function properly, as it creates the named counters used by the statistics subsystem (LGASTAT1/LGWEBST5) and the customer number counter used by the customer creation workflow (LGACUS01).

### Dependencies
- **CICS Named Counter Server**: The `GENA` pool must be defined in the CICS system.
- **Terminal access**: Must be run from a CICS terminal to receive the optional customer number parameter and display output.

### Known Issues or Limitations
1. **Highly repetitive code**: The 38 counter delete/define sequences (lines 189-516) are nearly identical and could be refactored into a loop with a table of counter names. This makes maintenance error-prone.
2. **No error checking**: None of the `RESP` values are checked. If a counter Define fails (e.g., because the pool doesn't exist), the program continues silently.
3. **Hardcoded defaults**: `FrstCustNum` is always 1 and cannot be overridden via input. `LastCustNum` defaults to 11, which is very low for production use.
4. **Unused variables**: `WS-FLAG-TSQE/H/L`, `WS-FLAG`, `WS-Cust-Low/High/Number`, `READ-CUST-LOW/HIGH`, `CA-AREA`, and `MSGEND` are defined but never used.
5. **GOBACK after EXIT** (line 532): The `GOBACK` statement after `EXIT` in `A-EXIT` is unreachable because `EXEC CICS RETURN` has already ended the task.

### Relationship to Other Programs
- **LGASTAT1**: Increments the counters that LGSETUP creates.
- **LGWEBST5**: Reads the counters that LGSETUP creates.
- **LGTESTC1**: Reads/writes the `GENACNTL` TSQ to track customer number boundaries.
- **LGACUS01** (and similar): Uses the `GENACUSTNUM` counter for customer number generation.
