# soavpio.cpy -- SOA Validate Policy Inquiry Output COMMAREA Copybook

## 1. Overview

**File name:** `soavpio.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the output COMMAREA structure for the SOA policy validation/inquiry service. Returns a text message and a composite key containing the policy type, customer number, and policy number. Part of the M&A scenario for Lord General Insurance Co.

The filename "soavpio" stands for "SOA validate policy inquiry output."

## 2. Data Structure Breakdown

### COMMA-DATA (Level 01, line 13)

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | Comma-Data-Text | X(11) | 11 | Response text/status message |
| 03 | Comma-Data-Key (group) | -- | 21 | Composite key group |
| 05 | Type | X | 1 | Policy type code |
| 05 | Cust-Num | 9(10) | 10 | Customer number |
| 05 | Pol-Num | 9(10) | 10 | Policy number |
| 03 | FILLER | X(48) | 48 | Reserved/unused space |

Total size: 80 bytes.

## 3. Key Components

### Composite Key Structure
The `Comma-Data-Key` group (line 15) contains a three-part composite key:
1. **Type** -- Policy type code (1 byte)
2. **Cust-Num** -- Customer number (10 digits)
3. **Pol-Num** -- Policy number (10 digits)

This composite key uniquely identifies a specific policy. The group name `Comma-Data-Key` allows the entire 21-byte key to be referenced as a single field for comparison or movement operations.

### M&A Scenario Context
The header comments (lines 7-11) identify this as the "Get valid policy number output comm area" for the Lord General Insurance Co. M&A scenario. The service returns the validated policy number along with the customer and type context.

### Response Text
`Comma-Data-Text` (11 bytes) contains a status message, likely values such as "POLICY FOUND" (truncated to 11 chars) or "NOT FOUND  ".

## 4. Usage Context

### Section Placement
Placed in the LINKAGE SECTION or WORKING-STORAGE SECTION of the SOA policy validation program.

### Data Flow
After the policy validation program finds a valid policy matching the input criteria (from `soavpii.cpy`), it populates this structure with:
1. A status text message
2. The full composite key (type + customer + policy number)
The CICS web service pipeline maps this back to an XML response for the external consumer.

## 5. Analysis

### Role in Architecture
The output half of the SOA policy validation request/response pair, supporting M&A operations. External systems can use this service to discover and validate policy numbers before performing further operations.

### Design Patterns
- **Request/response pair:** Paired with `soavpii.cpy` for input
- **Composite key return:** Returns the full identifying key, not just the policy number, so the caller has all context needed for subsequent operations
- **Group item for key:** `Comma-Data-Key` allows the key to be treated as a single unit or accessed field-by-field

### Size Considerations
At 80 bytes, this structure is compact. The meaningful data (text + key) is only 32 bytes; the remaining 48 bytes are filler.
