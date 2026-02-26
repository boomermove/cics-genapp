# soavcii.cpy -- SOA Validate Customer Inquiry Input COMMAREA Copybook

## 1. Overview

**File name:** `soavcii.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the input COMMAREA structure for a SOA customer validation or inquiry service. This is a minimal structure containing only a customer number and a filler field.

The filename pattern "soavcii" likely stands for "SOA validate customer inquiry input." This copybook is used to pass a customer number to a validation or lookup service.

## 2. Data Structure Breakdown

### COMMA-DATA (Level 01, line 6)

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | Customer-Number | X(10) | 10 | Customer number (alphanumeric format) |
| 03 | Filler | X(72) | 72 | Reserved/unused space |

Total size: 82 bytes.

## 3. Key Components

### Minimal Input Structure
This is one of the smallest COMMAREA structures in the application. It contains only the customer number needed to perform a customer lookup or validation.

### Customer-Number as PIC X(10)
Note that `Customer-Number` is defined as `PIC X(10)` (alphanumeric), not `PIC 9(10)` (numeric) as in other copybooks. This allows leading spaces or non-numeric characters in the input.

### Naming Convention
The level 01 name `COMMA-DATA` (short for COMMAREA-DATA) and the use of mixed-case field names (`Customer-Number`) suggest this was written for the SOA layer with a different coding style than the core COBOL programs.

### Filler
The 72-byte `Filler` field pads the structure to 82 bytes total. This fixed size may correspond to a CICS container or web service message size requirement.

## 4. Usage Context

### Section Placement
Placed in the LINKAGE SECTION of the SOA customer validation service program, mapped from the incoming web service request.

### Data Flow
An external client sends a customer number via a web service request. The CICS pipeline maps it to this structure. The receiving program uses `Customer-Number` to look up the customer in the database.

## 5. Analysis

### Role in Architecture
Part of the SOA validation layer. This input copybook is paired with `soavcio.cpy` (the output copybook) to define the request/response contract for a customer validation web service.

### Design Patterns
- **Request/response pair:** Input (`soavcii.cpy`) and output (`soavcio.cpy`) are separate copybooks
- **Single-field input:** Only the customer number is needed as input

### Size Considerations
At 82 bytes, this is the smallest COMMAREA structure in the application. The 72-byte filler is relatively large compared to the 10-byte payload, possibly to match a minimum COMMAREA size requirement.
