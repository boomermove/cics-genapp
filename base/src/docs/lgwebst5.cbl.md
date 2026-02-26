# lgwebst5.cbl - GenApp Statistics Collector and Web Publisher

## 1. Overview

- **Program ID:** LGWEBST5
- **Purpose:** Periodically collects transaction statistics from CICS named counters, computes rates and aggregates, stores the data in TSQs, and prepares a symbol list for web-based statistics display.

LGWEBST5 is a self-scheduling statistics collection program designed for the IBM CICS GenApp application. It runs as a repeating transaction (started via `EXEC CICS START` every 60 seconds) that queries all named counters in the `GENA` pool, aggregates success/error totals across all operation types (customer, motor, endowment, house, commercial operations), computes transaction rates over time intervals, and stores historical and delta rate data in TSQs. It also prepares a `SymbList` structure containing all counter values formatted for use with a CICS web template (`GENAST3TEMP`) to render an HTML statistics page. This program is the reporting companion to `LGASTAT1` (which increments the counters) and `LGSETUP` (which initializes them).

## 2. Functional Breakdown

### MAINLINE SECTION (line 250)

**Lines 252-257 - Initialization:**
Initializes `WS-HEADER`, captures EIB fields (transaction ID, terminal ID, task number, commarea length).

**Lines 259-265 - Time Capture:**
Sets TSQ prefix to `'GENA'`, gets current time via `EXEC CICS ASKTIME` and formats it as MMDDYYYY date and time.

**Line 266 - Rate Interval Calculation:**
Performs `Tran-Rate-Interval` to calculate the elapsed time since the last collection cycle and schedule the next cycle.

**Lines 268-699 - Counter Collection:**
Queries each named counter in the `GENA` pool using `EXEC CICS Query Counter`. For each counter pair (success/error), the program:
1. Reads the counter value
2. Accumulates into aggregate totals (`CountSuccess`, `CountErrors`, `CountInq`, `CountAdd`, `CountUpd`, `CountDel`)
3. Formats the value into the corresponding `SymbList` field (`S3`-`S39`)
4. Calls `Tran-Rate-Counts` to compute and store rate data in TSQs

The counters are organized by operation type and outcome:
| Counter Pair | Operation | Aggregate |
|---|---|---|
| GENACNT100/199 | Inquire Customer | CountInq, CountSuccess/Errors |
| GENACNT200/299 | Add Customer | CountAdd, CountSuccess/Errors |
| GENACNT300/399 | Inquire Motor | CountInq, CountSuccess/Errors |
| GENACNT400/499 | Add Motor | CountAdd, CountSuccess/Errors |
| GENACNT500/599 | Delete Motor | CountDel, CountSuccess/Errors |
| GENACNT600/699 | Update Motor | CountUpd, CountSuccess/Errors |
| GENACNT700/799 | Inquire Endowment | CountInq, CountSuccess/Errors |
| GENACNT800/899 | Add Endowment | CountAdd, CountSuccess/Errors |
| GENACNT900/999 | Delete Endowment | CountDel, CountSuccess/Errors |
| GENACNTA00/A99 | Update Endowment | CountUpd, CountSuccess/Errors |
| GENACNTB00/B99 | Inquire House | CountInq, CountSuccess/Errors |
| GENACNTC00/C99 | Add House | CountAdd, CountSuccess/Errors |
| GENACNTD00/D99 | Delete House | CountDel, CountSuccess/Errors |
| GENACNTE00/E99 | Update House | CountUpd, CountSuccess/Errors |
| GENACNTF00/F99 | Inquire Commercial | CountInq, CountSuccess/Errors |
| GENACNTG00/G99 | Add Commercial | CountAdd, CountSuccess/Errors |
| GENACNTH00/H99 | Delete Commercial | CountDel, CountSuccess/Errors |
| GENACNTI99 | Other/Misc | Standalone |

**Lines 670-699 - Summary Counters:**
Computes and stores aggregate totals:
- `GENAsucces-V` / `GENAerrors-V` - Total success/error counts
- `X00V` TSQ - Total success rate
- `X01V`-`X04V` TSQs - Inquiry, Add, Update, Delete rates

**Lines 703-705 - APPLID:**
Gets the CICS application ID and stores it in `S1` of `SymbList`.

**Lines 708-709 - Return:**
Issues `EXEC CICS RETURN`.

### Tran-Rate-Interval (line 715)

Calculates the time elapsed since the last statistics collection:
1. Reads the previous timestamp from TSQ `GENA000V` (item 1)
2. If not found, defaults to `'120000'` (12:00:00)
3. Deletes and rewrites the TSQ with the current time
4. Converts both old and new timestamps to seconds since midnight (HH*3600 + MM*60 + SS)
5. Computes the interval (`ICountVal = NCountVal - OCountVal`)
6. Writes the interval to item 2 of the TSQ
7. Schedules the next execution: `EXEC CICS START Transid('SSST') After Minutes(1)`

### Tran-Rate-Counts (line 769)

For each counter, maintains a 3-item TSQ containing:
1. **Current value** (`NRateVal`) - the latest counter reading
2. **Previous value** (`ORateVal`) - the reading from last cycle
3. **Delta** (`DRateVal = NRateVal - ORateVal`) - transactions since last cycle

The paragraph reads the old value, deletes the TSQ, then writes all three items.

### A-EXIT (line 711)
Standard exit paragraph.

### Control Flow
```
Entry -> Init -> Get Time -> Tran-Rate-Interval (schedule next run)
      -> For each counter pair:
           Query Counter -> Accumulate totals -> Tran-Rate-Counts
      -> Compute summary counters -> Store rates
      -> Get APPLID -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `CountVal` | 18 | Current counter value from Query Counter |
| `CountSuccess` | 19 | Running total of successful operations |
| `CountErrors` | 20 | Running total of failed operations |
| `CountInq/Add/Upd/Del` | 21-24 | Running totals by operation type |
| `DRateVal/NRateVal/ORateVal` | 25-27 | Delta, new, and old rate values for rate computation |
| `ICountVal/OCountVal/NCountVal` | 28-30 | Interval, old, and new time values in seconds |
| `HHval/MMval/SSval` | 31-33 | Hours, minutes, seconds for time conversion |
| `WS-RESP` | 34 | CICS response code |
| `WS-ABSTIME` | 35 | Absolute time |
| `WS-TSQname` | 48 | Dynamically constructed TSQ name for rate data |
| `WS-TSQdata` | 49 | 9-byte TSQ data buffer |
| `WS-HHMMSS` | 51-54 | Current time split into HH, MM, SS |
| `WS-OLDV` | 63-66 | Previous time split into OLDVHH, OLDVMM, OLDVSS |
| `WS-NEWV` | 67 | Computed interval as string |
| `TCPservice` | 40 | TCP service name `'SSISTAT1'` (not used in procedure) |
| `StatQ` | 56 | Statistics TSQ name `'GENASTAT'` (defined but not used) |
| `TemplateName` | 57 | Web template `'GENAST3TEMP'` (defined but not used in procedure) |
| `WS-APPLID` | 58 | CICS application ID |
| `SymbList` | 76-156 | Large symbol substitution list with APPLID, all 37 counter values, success/error totals, and start time |
| `GENAcount-V` through `GENAerrors-V` | 157-195 | Display-format copies of all counter values |
| `GENAcount` through `GENACNTI99` | 197-233 | Named counter identifiers matching those in LGSETUP |
| `GENApool` | 198 | Counter pool name `'GENA'` |

### LINKAGE SECTION
- **DFHCOMMAREA** (line 239): Simple structure with `Comma-Data-H` (X(14)), `Comma-Data-High` (9(10)), and filler. Not meaningfully used since the program is self-starting.

### Copybooks
None.

### Condition Names
None defined.

## 4. Input/Output Behavior

### Input
- **CICS Named Counters** (GENA pool): Reads all 37+ counters via `EXEC CICS Query Counter`.
- **Rate TSQs** (`GENAxxxxV`): Reads previous counter values to compute deltas.

### Output
- **Rate TSQs** (`GENAxxxxV`): Writes 3 items per counter: current value, previous value, delta.
- **Interval TSQ** (`GENA000V`): Stores current timestamp and computed interval.
- **SymbList structure**: Prepared in-memory for web template rendering (though the actual `EXEC CICS DOCUMENT` or web response is not issued in this code).
- **Self-scheduling**: Starts transaction `SSST` after 1 minute for the next collection cycle.

### Files Accessed
| Resource | Type | Operation |
|---|---|---|
| GENA pool counters (37+) | CICS Named Counter | Query |
| `GENA000V` | CICS TSQ | ReadQ, DeleteQ, WriteQ |
| `GENAxxxxV` (per counter) | CICS TSQ | ReadQ, DeleteQ, WriteQ |

### Other Programs Called
None.

## 5. Algorithms and Techniques

### SQL Statements
None.

### CICS Commands
| Command | Lines | Purpose |
|---|---|---|
| `EXEC CICS ASKTIME` | 260 | Get current time |
| `EXEC CICS FORMATTIME` | 262 | Format time as MMDDYYYY and time |
| `EXEC CICS Query Counter ... Pool` | 268+ (repeated ~37 times) | Read named counter values |
| `EXEC CICS ReadQ TS` | 720, 771 | Read previous values from rate TSQs |
| `EXEC CICS DeleteQ TS` | 729, 778 | Delete rate TSQs before rewrite |
| `EXEC CICS WRITEQ TS` | 734, 756, 783, 789, 796 | Write current, previous, and delta values |
| `EXEC CICS Start Transid('SSST') After Minutes(1)` | 761 | Schedule next collection cycle |
| `EXEC CICS ASSIGN APPLID` | 703 | Get CICS application ID |
| `EXEC CICS RETURN` | 708 | Return to CICS |

### Error Handling
- `Tran-Rate-Interval`: If the previous timestamp TSQ does not exist (`WS-RESP Not = DFHRESP(NORMAL)`), defaults to `'120000'` (noon). This handles the first-run scenario.
- All other CICS commands use `RESP(WS-RESP)` but the response is not checked. Counter query failures or TSQ write failures are silently ignored.

### Notable Logic Patterns
- **Self-scheduling transaction**: The program starts itself as transaction `SSST` every 60 seconds via `EXEC CICS START Transid('SSST') After Minutes(1)` (line 761). This creates a continuous monitoring loop without a persistent long-running task.
- **Delete-then-rewrite pattern**: Rate TSQs are deleted and rewritten each cycle rather than updated in place. This avoids item-count management issues.
- **Time-to-seconds conversion**: Converts HH:MM:SS timestamps to seconds since midnight for interval calculation (lines 742-751).
- **Aggregate computation**: Accumulates totals across operation types into `CountSuccess`, `CountErrors`, `CountInq`, `CountAdd`, `CountUpd`, `CountDel` as it iterates through counters.

## 6. Analysis

### Overall Role
LGWEBST5 is the statistics reporting/aggregation layer in the GenApp monitoring subsystem. It forms the read side of a producer-consumer pattern: LGASTAT1 increments counters (producer), and LGWEBST5 periodically reads and aggregates them (consumer). The collected data is used for web-based dashboards and potentially IBM Business Monitor integration (as noted in the source comments).

### Dependencies
- **CICS Named Counter Server**: All counters must exist in the `GENA` pool (created by LGSETUP).
- **Self-start capability**: The initial invocation must be triggered externally (e.g., by manually starting transaction `SSST` or through CICS startup PLT processing).
- **LGSETUP**: Must have been run to create all named counters before LGWEBST5 can function.

### Known Issues or Limitations
1. **No error handling for counter queries**: If any counter does not exist, the query will fail and `CountVal` retains its previous value, leading to incorrect aggregations.
2. **Negative interval possible**: If the program crosses midnight, `NCountVal - OCountVal` will be negative. There is no wraparound handling.
3. **Template rendering not present**: `TemplateName` (`'GENAST3TEMP'`), `WS-MTYPE`, `WS-CHARSET`, and `WS-TOKEN` are defined but no `EXEC CICS DOCUMENT` or web response commands are issued. The SymbList is built but not consumed within this program.
4. **Highly repetitive code**: The counter query and rate computation blocks are repeated for each counter pair. This could be refactored using a table-driven approach.
5. **Unused variables**: `TCPservice`, `TCPaddress`, `TCPaddrLen`, `Part1`-`Part4`, `StatQ`, `TemplateName`, `WS-TOKEN`, `WS-TEXT`, `WS-MTYPE`, `WS-CHARSET`, `WS-CODE` are defined but never used in the PROCEDURE DIVISION.
6. **Missing period after PROGRAM-ID** (line 11): `PROGRAM-ID. LGWEBST5` lacks a trailing period, though most COBOL compilers accept this.

### Relationship to Other Programs
- **LGASTAT1**: Increments the counters that LGWEBST5 reads.
- **LGSETUP**: Creates and initializes the counters.
- **Self-referencing**: Schedules itself via `EXEC CICS START Transid('SSST')`.
- The SymbList structure suggests integration with a CICS web template (`GENAST3TEMP`) for HTML statistics rendering, though the actual web serving code may reside in another program or pipeline.
