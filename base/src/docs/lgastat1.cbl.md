# lgastat1.cbl - GenApp Statistics Event Processor

## 1. Overview

- **Program ID:** LGASTAT1
- **Purpose:** Processes GenApp transaction events for statistics/monitoring by capturing event data from CICS EP (Event Processing) containers or a commarea, normalizing counter names, and incrementing named counters in a shared counter pool.

LGASTAT1 is a CICS event processing adapter. It is triggered either by a CICS Event Processing emission (via EP channels and containers) or called directly with a commarea. The program extracts a request identifier and return code, normalizes certain counter names, and increments a named counter in the `GENA` pool. It also initializes a startup timestamp TSQ (`GENASTRT`) on first invocation. This program acts as the statistics ingestion point that feeds the counters read by the `LGWEBST5` statistics collector.

## 2. Functional Breakdown

### MAINLINE SECTION (line 69)

The entire logic resides in a single section with no sub-paragraphs.

**Lines 72-77 - Initialization:**
Initializes `WS-HEADER`, captures EIB fields (`EIBTRNID`, `EIBTRMID`, `EIBTASKN`, `EIBCALEN`).

**Lines 79-87 - Event Container Retrieval:**
Attempts to read two CICS EP data containers:
- `DFHEP.DATA.00001` into `WS-Data-Req` (6 bytes - the request type)
- `DFHEP.DATA.00002` into `WS-Data-RC` (2 bytes - the return code)

**Lines 89-99 - Data Source Selection:**
If the container read succeeded (`DFHRESP(NORMAL)`), uses the container data. Otherwise, falls back to the commarea: if `EIBCALEN` is zero, returns immediately; otherwise uses `CA-REQUEST-ID` and `CA-RETURN-CODE` from the LGCMAREA copybook.

**Lines 101-120 - Startup Timestamp:**
Reads the `GENASTRT` TSQ. If it does not exist (`QIDERR`), gets the current time/date and writes a startup record to the TSQ. This records when the statistics subsystem first started.

**Lines 122-127 - Counter Name Normalization:**
Normalizes certain request IDs to canonical forms:
- `'02ACUS'` is mapped to `'01ACUS'` (line 123)
- `'02ICOM'`, `'03ICOM'`, `'05ICOM'` are all mapped to `'01ICOM'` (lines 124-126)
- If the return code (`GENAType`) is not `'00'`, it is forced to `'99'` (line 127)

This means all error codes are collapsed to `'99'`, creating a success/failure binary for counting.

**Lines 129-133 - Counter Increment:**
Issues `EXEC CICS GET Counter(GENAcount) Pool(GENApool)` to atomically read and increment the named counter. The counter name is constructed as `'GENA' + GENAcounter + GENAtype` (e.g., `'GENA01ICUS00'` for a successful customer inquiry).

**Line 135 - Return:**
Issues `EXEC CICS RETURN`.

#### MAINLINE-EXIT (line 137)
Standard exit paragraph.

### Control Flow
```
Entry -> Initialize EIB fields
      -> Try EP containers
      -> If OK: use container data; else use commarea
      -> Check/create GENASTRT TSQ
      -> Normalize counter names
      -> Increment named counter
      -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `WS-ABSTIME` | 16 | Absolute time for timestamp |
| `WS-TIME`, `WS-DATE` | 17-18 | Formatted time and date |
| `WS-QAREA` (with `WS-Area-D`, `WS-Area-T`) | 19-22 | TSQ record: 8-char date + 2 spaces + 6-char time |
| `WS-Qname` | 23 | TSQ name `'GENASTRT'` for startup timestamp |
| `WS-CONTname` | 24 | Container name `'DFHEP.CCECONTEXT'` (not used in procedure) |
| `WS-CHANname1` | 26 | EP container `'DFHEP.DATA.00001'` for request type |
| `WS-CHANname2` | 27 | EP container `'DFHEP.DATA.00002'` for return code |
| `WS-Data-Req` | 28 | 6-byte request type from container |
| `WS-Data-RC` | 29 | 2-byte return code from container |
| `GENAcount` | 38-42 | 16-byte counter name: `'GENA'` + `GENAcounter`(6) + `GENAtype`(2) + spaces(4) |
| `Trancount` | 43 | Counter value returned by GET Counter |
| `LGACDB01`, `LGACVS01` | 49-50 | Program names (defined but not used in procedure) |
| `ATRANID` | 51 | Transaction ID `'DSC1'` (defined but not used in procedure) |
| `GENApool` | 52 | Counter pool name `'GENA'` |
| `STSQ-name` | 53 | TSQ name `'STEWSTEW'` (defined but not used) |
| `GENAOTHER` | 54 | Value `'GENAOTHER'` (defined but not used) |

### LINKAGE SECTION
- **DFHCOMMAREA** (line 61): Uses `COPY LGCMAREA` which provides `CA-REQUEST-ID` (X(6)), `CA-RETURN-CODE` (9(2)), `CA-CUSTOMER-NUM` (9(10)), and the full 32,500-byte commarea structure.

### Copybooks
| Copybook | Line | Purpose |
|---|---|---|
| `LGCMAREA` | 62 | Standard GenApp commarea structure with request ID, return code, customer data, and policy data |

### Condition Names
None defined explicitly in this program. The LGCMAREA copybook does not define 88-levels.

## 4. Input/Output Behavior

### Input
The program accepts data from two possible sources:
1. **CICS EP Containers** (preferred): `DFHEP.DATA.00001` (request type, 6 bytes) and `DFHEP.DATA.00002` (return code, 2 bytes). These are populated by the CICS Event Processing infrastructure.
2. **DFHCOMMAREA** (fallback): `CA-REQUEST-ID` and `CA-RETURN-CODE` from the standard GenApp commarea.

### Output
- **Named Counter**: Increments a counter in the `GENA` pool. Counter name pattern: `GENA` + request-id(6) + return-code(2), e.g., `GENA01ICUS00`.
- **GENASTRT TSQ**: On first invocation, writes a startup timestamp record (date + time).

### Files Accessed
| Resource | Type | Operation |
|---|---|---|
| `GENASTRT` | CICS TSQ | READ (to check existence), WRITE (on first run) |
| `GENA` pool counters | CICS Named Counter | GET (atomic increment) |
| `DFHEP.DATA.00001` | CICS EP Container | GET (read request type) |
| `DFHEP.DATA.00002` | CICS EP Container | GET (read return code) |

### Other Programs Called
None.

## 5. Algorithms and Techniques

### SQL Statements
None.

### CICS Commands
| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS Get Container(WS-CHANname1)` | 79 | Read request type from EP container |
| `EXEC CICS Get Container(WS-CHANname2)` | 84 | Read return code from EP container |
| `EXEC CICS RETURN` | 94 | Early return if no commarea and no EP data |
| `EXEC CICS ReadQ TS Queue(WS-Qname)` | 101 | Check if GENASTRT TSQ exists |
| `EXEC CICS ASKTIME` | 107 | Get current absolute time |
| `EXEC CICS FORMATTIME` | 109 | Format date (DDMMYYYY) and time |
| `EXEC CICS WriteQ TS Queue(WS-Qname)` | 115 | Write startup timestamp to GENASTRT |
| `EXEC CICS Get Counter(GENAcount) Pool(GENApool)` | 129 | Atomically read and increment the named counter |
| `EXEC CICS RETURN` | 135 | Return to caller |

### Error Handling
- Container read failures are handled by checking `WS-RESP` against `DFHRESP(NORMAL)` and falling back to commarea data.
- TSQ `QIDERR` (queue not found) is handled by creating the queue with a startup timestamp.
- No explicit error handling for the `GET Counter` command; errors are silently ignored.

### Notable Logic Patterns
- **Counter name normalization** (lines 122-127): Several request IDs are collapsed to canonical forms. This aggregates related operations (e.g., different stages of customer add or commercial inquiry) into a single counter.
- **Error code binary collapse** (line 127): All non-`'00'` return codes become `'99'`, creating a simple success/error dichotomy for the counters. Each operation type therefore has exactly two counters: one for success (`00`) and one for failure (`99`).

## 6. Analysis

### Overall Role
LGASTAT1 is the statistics ingestion layer in the GenApp monitoring subsystem. It sits between CICS Event Processing and the named counter infrastructure. Each GenApp business transaction emits an event that LGASTAT1 captures and translates into a counter increment. The counters are subsequently read by `LGWEBST5` to produce statistics dashboards.

### Dependencies
- **CICS Event Processing**: For the preferred EP container-based invocation path.
- **CICS Named Counter Server**: The `GENA` counter pool must be defined and available.
- **LGCMAREA copybook**: For the fallback commarea-based invocation.

### Known Issues or Limitations
1. **Unused variables**: `LGACDB01`, `LGACVS01`, `ATRANID`, `STSQ-name`, `GENAOTHER`, and `WS-CONTname` are defined but never referenced in the PROCEDURE DIVISION. This suggests dead code from earlier versions or planned features.
2. **No error handling for GET Counter**: If the named counter does not exist (e.g., `LGSETUP` has not been run), the `GET Counter` will fail silently since `WS-RESP` is checked only after the fact.
3. **Counter name normalization is incomplete**: Only a few specific request IDs (`02ACUS`, `02ICOM`, `03ICOM`, `05ICOM`) are normalized. Other multi-step operations may not be properly aggregated.
4. **Startup TSQ is not idempotent**: If multiple instances start simultaneously, they could all see `QIDERR` and write overlapping startup records.
5. **Missing period after IF block** (line 126): The normalization IF statements use implicit scope termination via periods, which makes the flow harder to reason about.

### Relationship to Other Programs
- **LGWEBST5**: Reads the named counters that LGASTAT1 increments to produce statistics.
- **LGSETUP**: Initializes/resets the named counters in the `GENA` pool that LGASTAT1 increments.
- **All GenApp business programs** (LGICUS01, LGACUS01, LGIPOL01, etc.): Emit CICS events or have their outcomes captured by LGASTAT1.
- **LGCMAREA copybook**: Shared commarea structure used across all GenApp programs.
