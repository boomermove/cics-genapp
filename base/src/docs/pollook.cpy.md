# pollook.cpy -- Generic Lookup Request Copybook

## 1. Overview

**File name:** `pollook.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines a minimal COMMAREA structure for generic lookup requests containing a request ID, customer number, and a large request-specific data area.

This is the simplest of the COMMAREA-variant copybooks. It provides a flat structure with just three fields -- a request identifier, a customer number, and a catch-all data payload. It is used in scenarios where only the request routing information and customer number are needed.

## 2. Data Structure Breakdown

### REQUEST (Level 01, line 6)

| Level | Field Name | PIC Clause | Attributes | Purpose |
|-------|-----------|------------|------------|---------|
| 03 | CA-REQUEST-ID | X(6) | -- | Request type identifier |
| 03 | CA-CUSTOMER-NUM | 9(10) | DISPLAY | Customer number |
| 03 | CA-REQUEST-SPECIFIC | X(32482) | -- | Generic request data area |

### Size Calculation
- `CA-REQUEST-ID`: 6 bytes
- `CA-CUSTOMER-NUM`: 10 bytes
- `CA-REQUEST-SPECIFIC`: 32,482 bytes
- Total: 32,498 bytes

## 3. Key Components

### Relationship to lgcmarea.cpy
This structure mirrors the first three fields of the main COMMAREA in `lgcmarea.cpy` (lines 10-13), but omits the `CA-RETURN-CODE` field. The field names `CA-REQUEST-ID`, `CA-CUSTOMER-NUM`, and `CA-REQUEST-SPECIFIC` are identical to those in `lgcmarea.cpy`.

### No Condition Names, REDEFINES, or VALUE Clauses
This is a pure data layout with no initial values, conditions, or redefinitions.

## 4. Usage Context

### Section Placement
The level 01 `REQUEST` structure is placed in the LINKAGE SECTION when the program receives this data as a COMMAREA from a calling program.

### Data Flow
A calling program passes this structure to identify the request type and customer. The receiving program uses `CA-REQUEST-ID` to determine what operation to perform and `CA-CUSTOMER-NUM` to identify the target customer. The `CA-REQUEST-SPECIFIC` area carries any additional data needed for the operation.

## 5. Analysis

### Role in Architecture
This copybook provides a **lightweight request envelope** for programs that need only the routing information (request ID and customer number) without the overhead of parsing the full COMMAREA structure. It is essentially a "view" of the COMMAREA that exposes only the header fields.

### Design Patterns
- **Minimal interface pattern:** Only the fields needed for routing are explicitly named; everything else is treated as opaque data.
- **Compatible overlay:** The total size (minus the 2-byte return code gap) is designed to be compatible with the main COMMAREA layout.

### Size Considerations
At 32,498 bytes, this structure is 2 bytes smaller than the full COMMAREA (32,500 bytes) because it omits `CA-RETURN-CODE PIC 9(2)`.
