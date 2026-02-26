# soavcio.cpy -- SOA Validate Customer Inquiry Output COMMAREA Copybook

## 1. Overview

**File name:** `soavcio.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the output COMMAREA structure for the SOA customer validation or inquiry service. Returns a text message and the highest customer number found, used to provide status or validation results to the caller.

The filename pattern "soavcio" stands for "SOA validate customer inquiry output." This is the response counterpart to `soavcii.cpy`.

## 2. Data Structure Breakdown

### COMMA-DATA (Level 01, line 6)

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | Comma-Data-Text | X(14) | 14 | Response text/status message |
| 03 | Comma-Data-High | X(10) | 10 | Highest customer number or result value |
| 03 | FILLER | X(48) | 48 | Reserved/unused space |

Total size: 72 bytes.

## 3. Key Components

### Response Fields
- **Comma-Data-Text** (14 bytes): Contains a descriptive text message, likely something like "CUSTOMER FOUND" or "NOT FOUND    " or a similar status string.
- **Comma-Data-High** (10 bytes): Contains the highest or matching customer number. The name "High" suggests it might return the highest existing customer number, useful for generating the next customer number in add operations.

### Naming Convention
Uses mixed-case hyphenated names (`Comma-Data-Text`, `Comma-Data-High`) consistent with the SOA copybook style.

### No Condition Names, REDEFINES, or VALUE Clauses
Pure data layout with no initial values.

## 4. Usage Context

### Section Placement
Placed in the LINKAGE SECTION or WORKING-STORAGE SECTION of the SOA customer validation program, used to format the response back to the web service caller.

### Data Flow
After the customer validation program performs the lookup (using the input from `soavcii.cpy`), it populates this structure with the result text and customer number, then returns it via the COMMAREA. The CICS web service pipeline maps this back to an XML response.

## 5. Analysis

### Role in Architecture
The output half of the SOA customer validation request/response pair. Together with `soavcii.cpy`, it defines a simple "give me a customer number, I'll validate it" web service contract.

### Design Patterns
- **Request/response pair:** Paired with `soavcii.cpy` for input
- **Text + data response:** Combines a human-readable status message with a machine-readable customer number

### Size Considerations
At 72 bytes, this is even smaller than the input structure (82 bytes). The 48-byte filler ensures a minimum structure size. The asymmetric sizes (82 input vs. 72 output) are unusual but workable since input and output use different COMMAREA definitions.
