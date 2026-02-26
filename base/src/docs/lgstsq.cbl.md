# lgstsq.cbl - TSQ/TDQ Message Logger

## 1. Overview

- **Program ID:** LGSTSQ
- **Purpose:** Writes diagnostic and error messages to both a CICS Transient Data Queue (TDQ) and a CICS Temporary Storage Queue (TSQ), serving as the centralized logging facility for GenApp.

LGSTSQ is a dual-purpose logging program. It can be invoked either directly from a terminal (as a transaction) or via `EXEC CICS LINK` from another program. When linked from a program, it receives the message via the commarea; when run from a terminal, it reads the message from terminal input. The message is written to both the `CSMT` TDQ (the standard CICS system message log) and a GenApp-specific TSQ (default `GENAERRS`, but configurable via a `Q=nnnn` prefix in the message). If invoked from a terminal, it also sends a blank acknowledgment back to the screen.

## 2. Functional Breakdown

### MAINLINE SECTION (line 55)

**Lines 57-58 - Initialization:**
Clears `WRITE-MSG` and `WS-RECV` to spaces.

**Lines 60-62 - System ID:**
Issues `EXEC CICS ASSIGN SYSID(WRITE-MSG-SYSID)` to capture the 4-character CICS system ID into the first field of the write buffer.

**Lines 64-66 - Invoking Program Check:**
Issues `EXEC CICS ASSIGN INVOKINGPROG(WS-INVOKEPROG)` to determine if this program was called via LINK from another program.

**Lines 68-80 - Input Source Selection:**
If `WS-INVOKEPROG` is not spaces (i.e., called via LINK):
- Sets `WS-FLAG` to `'C'` (called)
- Moves the commarea data (`COMMA-DATA`, 90 bytes) to `WRITE-MSG-MSG`
- Sets `WS-RECV-LEN` to `EIBCALEN`

Otherwise (terminal invocation):
- Issues `EXEC CICS RECEIVE` to read terminal input
- Sets `WS-FLAG` to `'R'` (received)
- Moves `WS-RECV-DATA` to `WRITE-MSG-MSG`
- Subtracts 5 from `WS-RECV-LEN` (removing the transaction ID length)

**Lines 82-88 - Queue Name Parsing:**
Sets default TSQ name to `'GENAERRS'`. If the message starts with `'Q='`, extracts the next 4 characters as a custom queue name suffix (replacing the last 4 chars of `STSQ-NAME`), shifts the remaining message forward, and adjusts the length by subtracting 7.

**Line 90 - Length Adjustment:**
Adds 5 to `WS-RECV-LEN` to account for the SYSID prefix and separator in the write buffer.

**Lines 94-99 - TDQ Write:**
Writes the message to the `CSMT` Transient Data Queue using `EXEC CICS WRITEQ TD`. This sends the message to the CICS system log.

**Lines 105-111 - TSQ Write:**
Writes the same message to the GenApp TSQ (default `GENAERRS`) using `EXEC CICS WRITEQ TS` with `NOSUSPEND` (do not wait if storage is unavailable).

**Lines 113-119 - Terminal Acknowledgment:**
If `WS-FLAG` is `'R'` (terminal invocation), sends a single space character to the terminal with `EXEC CICS SEND TEXT` to clear/acknowledge.

**Lines 121-122 - Return:**
Issues `EXEC CICS RETURN`.

#### A-EXIT (line 124)
Standard exit with `EXIT` and `GOBACK`.

### Control Flow
```
Entry -> Clear buffers -> Get SYSID -> Check invoking program
      -> If LINKED: use commarea; else RECEIVE from terminal
      -> Parse Q= prefix for custom queue name
      -> WRITEQ TD to CSMT
      -> WRITEQ TS to GENAERRS (or custom)
      -> If terminal: SEND TEXT acknowledgment
      -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `WS-FLAG` | 17 | Invocation mode: `'C'`=called via LINK, `'R'`=terminal receive |
| `WS-RESP` | 18 | CICS response code |
| `WS-INVOKEPROG` | 19 | Name of calling program (spaces if terminal) |
| `WS-RECV` | 20-23 | Terminal receive buffer: 5-byte TRANID + 74-byte data |
| `WS-RECV-LEN` | 23 | Receive buffer length, initialized to 80 |
| `WRITE-MSG` | 25-28 | Output buffer: 4-byte SYSID + space + 90-byte message |
| `WRITE-MSG-REST` (via REDEFINES) | 32 | Message without the first 12 bytes (used for Q= shifting) |
| `STSQ` / `STSQ-NAME` | 33-37 | TSQ name (8 bytes), with `STSQ-EXT` redefine for last 4 chars |
| `TEMPO` | 39 | 90-byte temp buffer for message shifting |
| `STDQ-NAME` | 40 | TDQ name `'CSMT'` (CICS system message transient data queue) |

### LINKAGE SECTION
- **DFHCOMMAREA** (line 47): Simple structure with `COMMA-DATA` PIC X(90) - a single 90-byte message field.

### Copybooks
None.

### Condition Names
None defined.

## 4. Input/Output Behavior

### Input
Two invocation modes:
1. **Via EXEC CICS LINK** (from another program): Message passed in `COMMA-DATA` (90 bytes) of the commarea.
2. **Via terminal**: Message read from terminal input after the 5-byte transaction ID.

Optional `Q=nnnn` prefix in the message to redirect TSQ output to `GENAnnnn` instead of `GENAERRS`.

### Output
| Destination | Description |
|---|---|
| `CSMT` TDQ | Message prefixed with CICS SYSID, written to CICS system log |
| `GENAERRS` TSQ (or `GENAnnnn`) | Same message written to GenApp error/diagnostic TSQ |
| Terminal | Single space character sent as acknowledgment (terminal mode only) |

### Files Accessed
| Resource | Type | Operation |
|---|---|---|
| `CSMT` | CICS TDQ | WRITEQ TD |
| `GENAERRS` (or custom) | CICS TSQ | WRITEQ TS (NOSUSPEND) |

### Other Programs Called
None.

## 5. Algorithms and Techniques

### SQL Statements
None.

### CICS Commands
| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ASSIGN SYSID` | 60 | Get CICS system ID for message prefix |
| `EXEC CICS ASSIGN INVOKINGPROG` | 64 | Determine if called via LINK |
| `EXEC CICS RECEIVE` | 73 | Read terminal input (terminal mode only) |
| `EXEC CICS WRITEQ TD Queue('CSMT')` | 94 | Write to CICS system log TDQ |
| `EXEC CICS WRITEQ TS Queue(STSQ-NAME) NOSUSPEND` | 105 | Write to GenApp TSQ without waiting |
| `EXEC CICS SEND TEXT` | 114 | Send acknowledgment to terminal |
| `EXEC CICS RETURN` | 121 | Return to caller |

### Error Handling
- All CICS commands use `RESP(WS-RESP)` but responses are not checked. This is a logging program, so failures during logging are silently ignored to avoid cascading failures.
- The `NOSUSPEND` option on WRITEQ TS ensures the program does not hang if TSQ storage is exhausted; the write is simply discarded.

### Notable Logic Patterns
- **Dual-mode invocation**: The program detects its invocation context using `EXEC CICS ASSIGN INVOKINGPROG` and adapts behavior accordingly. This allows the same program to serve as both a callable subroutine and a standalone terminal transaction.
- **Queue name routing**: The `Q=nnnn` prefix mechanism allows callers to direct messages to different TSQs without code changes, providing flexible message categorization.
- **Message format**: All output messages are prefixed with the 4-character CICS SYSID, enabling identification of the originating system in a multi-system (sysplex) environment.

## 6. Analysis

### Overall Role
LGSTSQ is the centralized logging service for the entire GenApp application. It is called by virtually every other GenApp program (via `EXEC CICS LINK Program('LGSTSQ')`) to record error messages, diagnostic information, and operational events. By writing to both the CICS system log (CSMT TDQ) and a GenApp-specific TSQ, it provides both real-time system-level visibility and application-specific log browsing.

### Dependencies
- **CICS TDQ `CSMT`**: Must be defined (this is a standard CICS system resource).
- **CICS TSQ storage**: Requires available temporary storage for TSQ writes.

### Known Issues or Limitations
1. **No response checking**: If WRITEQ TD or WRITEQ TS fails, the program has no fallback. Logging failures are invisible.
2. **Fixed message size**: The commarea is limited to 90 bytes, which may truncate longer error messages from callers.
3. **Missing END-IF for terminal send** (line 113): The `If WS-FLAG = 'R'` block lacks an explicit `END-IF`. The period after the `SEND TEXT` and the subsequent `EXEC CICS RETURN` mean the return executes unconditionally, but the structure is fragile.
4. **GOBACK unreachable** (line 126): The `GOBACK` after `EXIT` in `A-EXIT` is never reached because `EXEC CICS RETURN` terminates the task.
5. **Queue name default 'GENAERRS'**: The 4-character extension slot (`STSQ-EXT`) means custom queue names are limited to `GENA` + 4 characters.

### Relationship to Other Programs
- **Called by**: Nearly all GenApp programs, including `LGPWHASH`, `LGICUS01`, `LGACUS01`, `LGIPOL01`, `LGAPOL01`, `LGDPOL01`, `LGUPOL01`, and others via `EXEC CICS LINK Program('LGSTSQ')`.
- **LGSETUP**: Deletes the `GENAERRS` TSQ during environment reset.
- **Can be run independently**: As a terminal transaction for ad-hoc message logging.
