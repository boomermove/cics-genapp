# LGERRPRC.cpy -- Common Error Handling Procedures Copybook

## 1. Overview

**File name:** `LGERRPRC.cpy`
**Copyright:** IBM Corp. 2023
**Purpose:** Provides reusable PROCEDURE DIVISION paragraphs for error logging in GenApp COBOL programs. This copybook contains the executable logic that formats timestamps, writes error messages to a Temporary Storage Queue (TSQ), and logs COMMAREA contents for diagnostics.

This is the procedural companion to `LGERR.cpy`. While `LGERR` defines the data structures in WORKING-STORAGE, `LGERRPRC` defines the paragraphs that operate on those structures. Together they provide a standardized error-handling mechanism used across all GenApp programs.

## 2. Data Structure Breakdown

This copybook defines no data structures -- it contains only PROCEDURE DIVISION paragraphs. It relies on data items defined in `LGERR.cpy` and program-specific `ERROR-MSG` structures.

### Referenced Data Items (from LGERR.cpy)
- `WS-ERR-ABSTIME` -- receives CICS absolute time
- `WS-ERR-DATE` -- receives formatted date
- `WS-ERR-TIME` -- receives formatted time
- `WS-ERR-LOG-PGM` -- name of the TSQ writer program ('LGSTSQ')
- `WS-ERR-RESP` -- receives CICS RESP code
- `CA-ERROR-MSG` -- commarea error dump structure
- `CA-DATA` -- receives first 90 bytes of COMMAREA

### Referenced Data Items (from calling program)
- `ERROR-MSG` -- program-specific error message structure
- `EM-DATE` -- date field within ERROR-MSG
- `EM-TIME` -- time field within ERROR-MSG

## 3. Key Components -- Paragraph Definitions

### 3.1 LGERR-FORMAT-TIME (line 25)

Retrieves the current date and time from CICS and formats them into human-readable strings.

**Logic:**
1. `EXEC CICS ASKTIME ABSTIME(WS-ERR-ABSTIME)` -- gets absolute time
2. `EXEC CICS FORMATTIME ABSTIME(WS-ERR-ABSTIME) MMDDYYYY(WS-ERR-DATE) TIME(WS-ERR-TIME)` -- formats to MM/DD/YYYY and HH:MM:SS

**Post-condition:** `WS-ERR-DATE` and `WS-ERR-TIME` contain the current date and time. The caller must then MOVE these into `EM-DATE` and `EM-TIME` of its own `ERROR-MSG`.

### 3.2 LGERR-WRITE-MSG (line 41)

Writes the program's `ERROR-MSG` structure to the TSQ via `EXEC CICS LINK`.

**Logic:**
1. `EXEC CICS LINK PROGRAM(WS-ERR-LOG-PGM) COMMAREA(ERROR-MSG) LENGTH(LENGTH OF ERROR-MSG) RESP(WS-ERR-RESP)` -- calls LGSTSQ to write the error message

**Pre-condition:** The caller must have populated `ERROR-MSG` with the error details (program name, error code, date, time, descriptive text).

### 3.3 LGERR-LOG-COMMAREA (line 53)

Logs the first 90 bytes of the CICS COMMAREA for diagnostic purposes.

**Logic:**
1. Checks `EIBCALEN > 0` (commarea exists)
2. If `EIBCALEN < 91`, copies only the available bytes: `MOVE DFHCOMMAREA(1:EIBCALEN) TO CA-DATA`
3. If `EIBCALEN >= 91`, copies the first 90 bytes: `MOVE DFHCOMMAREA(1:90) TO CA-DATA`
4. Calls LGSTSQ via `EXEC CICS LINK` to write the `CA-ERROR-MSG` structure (which includes the 'COMMAREA=' prefix)

**Note:** This paragraph is safe to call even when no COMMAREA is passed (EIBCALEN = 0), in which case it does nothing.

### 3.4 LGERR-WRITE-FULL (not explicitly coded but documented in header, line 17)

The header comment at line 17 documents a `LGERR-WRITE-FULL` paragraph that would combine: format time + write error message + log commarea. This appears to be a planned convenience paragraph that calling programs can implement by performing the three individual paragraphs in sequence.

## 4. Usage Context

### Programs That COPY This File
All 20 main application programs include `COPY LGERRPRC` in their PROCEDURE DIVISION, matching the same set that includes `COPY LGERR`:
- Customer programs: `lgacus01.cbl`, `lgucus01.cbl`, `lgicus01.cbl`
- Policy programs: `lgapol01.cbl`, `lgipol01.cbl`, `lgupol01.cbl`, `lgdpol01.cbl`
- DB2 access programs: `lgacdb01.cbl`, `lgacdb02.cbl`, `lgicdb01.cbl`, `lgucdb01.cbl`, `lgapdb01.cbl`, `lgipdb01.cbl`, `lgupdb01.cbl`, `lgdpdb01.cbl`
- VSAM access programs: `lgacvs01.cbl`, `lgucvs01.cbl`, `lgapvs01.cbl`, `lgupvs01.cbl`, `lgdpvs01.cbl`

### Section Placement
Included in the **PROCEDURE DIVISION** of each program, typically near the end, providing common error-handling paragraphs that are PERFORMed from the main logic.

### Typical Calling Pattern
```cobol
PERFORM LGERR-FORMAT-TIME
MOVE WS-ERR-DATE TO EM-DATE
MOVE WS-ERR-TIME TO EM-TIME
MOVE 'Error description' TO EM-TEXT
PERFORM LGERR-WRITE-MSG
PERFORM LGERR-LOG-COMMAREA
```

## 5. Analysis

### Role in Architecture
This copybook provides the **executable half** of the GenApp error-handling framework. By including standardized error-logging paragraphs via COPY, the application ensures consistent error handling across all 20+ programs without code duplication.

### Design Patterns
- **Template method pattern:** Each program provides its own `ERROR-MSG` structure (the variable part), while the logging mechanics (the fixed part) come from this copybook.
- **Defensive programming:** `LGERR-LOG-COMMAREA` checks `EIBCALEN` before accessing `DFHCOMMAREA`, avoiding potential storage violations.
- **Reference-length technique:** Uses `LENGTH OF ERROR-MSG` to dynamically determine the message size, so different programs can have different-sized error messages.
- **Indirection via LINK:** Errors are written via `EXEC CICS LINK` to `LGSTSQ` rather than directly to a TSQ, providing a layer of abstraction for the storage mechanism.

### Dependencies
- Requires `LGERR.cpy` to be copied in WORKING-STORAGE
- Requires the calling program to define `ERROR-MSG` with `EM-DATE` and `EM-TIME` fields
- Requires the `LGSTSQ` program to be available in the CICS region
- References CICS system fields `EIBCALEN` and `DFHCOMMAREA`
