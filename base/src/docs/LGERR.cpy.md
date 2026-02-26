# LGERR.cpy -- Common Error Handling Structures Copybook

## 1. Overview

**File name:** `LGERR.cpy`
**Copyright:** IBM Corp. 2023
**Purpose:** Provides shared working-storage data structures for error logging across all GenApp COBOL programs. This copybook defines the work fields, error message formatting areas, and the name of the error-logging program.

This copybook is designed to be used in conjunction with `LGERRPRC.cpy`, which provides the corresponding PROCEDURE DIVISION paragraphs. Together they form a reusable error-handling framework: `LGERR` supplies the data definitions (included in WORKING-STORAGE), and `LGERRPRC` supplies the executable logic (included in PROCEDURE DIVISION).

## 2. Data Structure Breakdown

### 2.1 WS-ERR-WORK-FIELDS (Level 01, line 22)

Working storage fields for time/date retrieval and CICS response handling.

| Level | Field Name | PIC Clause | VALUE | Purpose |
|-------|-----------|------------|-------|---------|
| 05 | WS-ERR-ABSTIME | S9(8) COMP | +0 | CICS absolute time (binary fullword) |
| 05 | WS-ERR-DATE | X(10) | SPACES | Formatted date (MM/DD/YYYY) |
| 05 | WS-ERR-TIME | X(8) | SPACES | Formatted time (HH:MM:SS) |
| 05 | WS-ERR-RESP | S9(8) COMP | 0 | CICS RESP code from error-log LINK |

### 2.2 CA-ERROR-MSG (Level 01, line 32)

Structure for logging the first 90 bytes of the COMMAREA for diagnostic purposes.

| Level | Field Name | PIC Clause | VALUE | Purpose |
|-------|-----------|------------|-------|---------|
| 05 | CA-EM-PREFIX | X(9) | 'COMMAREA=' | Fixed prefix label identifying this as a commarea dump |
| 05 | CA-DATA | X(90) | SPACES | First 90 bytes of the COMMAREA content |

Total size: 99 bytes.

### 2.3 WS-ERR-LOG-PGM (Level 01, line 39)

| Level | Field Name | PIC Clause | VALUE | Purpose |
|-------|-----------|------------|-------|---------|
| 01 | WS-ERR-LOG-PGM | X(8) | 'LGSTSQ' | Program name for the TSQ error-logging service |

## 3. Key Components

### Constants
- **CA-EM-PREFIX** (line 33): The literal `'COMMAREA='` is written before every commarea dump, making it easy to identify commarea log entries in the TSQ.
- **WS-ERR-LOG-PGM** (line 39): The program `LGSTSQ` is the centralized Temporary Storage Queue (TSQ) writer. All error messages are routed through it via `EXEC CICS LINK`.

### Initial Values
- `WS-ERR-ABSTIME` initialised to +0 (no time retrieved yet)
- `WS-ERR-DATE` and `WS-ERR-TIME` initialised to SPACES
- `WS-ERR-RESP` initialised to 0 (no error)
- `CA-DATA` initialised to SPACES

### No Condition Names or REDEFINES
This copybook contains no 88-level conditions and no REDEFINES.

## 4. Usage Context

### Programs That COPY This File
All 20 main application programs include `COPY LGERR`, including:
- Customer programs: `lgacus01.cbl`, `lgucus01.cbl`, `lgicus01.cbl`
- Policy programs: `lgapol01.cbl`, `lgipol01.cbl`, `lgupol01.cbl`, `lgdpol01.cbl`
- DB2 access programs: `lgacdb01.cbl`, `lgacdb02.cbl`, `lgicdb01.cbl`, `lgucdb01.cbl`, `lgapdb01.cbl`, `lgipdb01.cbl`, `lgupdb01.cbl`, `lgdpdb01.cbl`
- VSAM access programs: `lgacvs01.cbl`, `lgucvs01.cbl`, `lgapvs01.cbl`, `lgupvs01.cbl`, `lgdpvs01.cbl`

### Section Placement
Included in the **WORKING-STORAGE SECTION** of each program, as indicated by the comment on line 10: "COPY LGERR."

### Companion Copybook
Each program that includes `COPY LGERR` in WORKING-STORAGE must also include `COPY LGERRPRC` in its PROCEDURE DIVISION. The LGERRPRC paragraphs reference the fields defined here.

### Prerequisites
As noted in the comments (lines 13-14), each consuming program must define its own `ERROR-MSG` structure containing at minimum `EM-DATE PIC X(8)` and `EM-TIME PIC X(6)` at standard positions, since `LGERRPRC` references these fields.

## 5. Analysis

### Role in Architecture
This copybook is part of a **cross-cutting error-handling framework**. Rather than each program implementing its own error logging, all programs share these common structures and write errors to the same TSQ via the `LGSTSQ` program.

### Design Patterns
- **Shared infrastructure pattern:** Common error-handling data is factored out into a copybook rather than duplicated.
- **Two-part copybook pattern:** Data definitions (`LGERR`) are separated from executable logic (`LGERRPRC`), following COBOL's strict section rules.
- **Centralized logging:** All error output is routed through a single program (`LGSTSQ`), providing a single point of control for error persistence.

### Size Considerations
- `WS-ERR-WORK-FIELDS`: approximately 26 bytes (two COMP fields at 4 bytes each + 10 + 8)
- `CA-ERROR-MSG`: 99 bytes
- `WS-ERR-LOG-PGM`: 8 bytes
- Total overhead per program: approximately 133 bytes of working storage
