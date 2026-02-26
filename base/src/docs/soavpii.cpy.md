# soavpii.cpy -- SOA Validate Policy Inquiry Input COMMAREA Copybook

## 1. Overview

**File name:** `soavpii.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the input COMMAREA structure for a SOA policy validation/inquiry service. This is a minimal structure containing a policy type, customer number, and filler. It is part of the M&A (Mergers & Acquisitions) scenario for Lord General Insurance Co.

The filename "soavpii" stands for "SOA validate policy inquiry input."

## 2. Data Structure Breakdown

### COMMA-DATA (Level 01, line 13)

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | Type | X | 1 | Policy type code (e.g., 'E', 'H', 'M', 'C') |
| 03 | Cust-Num | 9(10) | 10 | Customer number |
| 03 | FILLER | X(79) | 79 | Reserved/unused space |

Total size: 90 bytes.

## 3. Key Components

### M&A Scenario Context
The header comments (lines 7-11) identify this as part of the "M&A Scenario - Lord General Insurance Co." and describe it as a "Get valid policy number input comm area." This suggests the service is used during a merger/acquisition scenario to validate or retrieve policy numbers for a given customer and policy type.

### Input Fields
- **Type** (1 byte): Single character identifying the policy type to look up. Values correspond to the policy type codes used throughout GenApp (E=Endowment, H=House, M=Motor, C=Commercial, etc.)
- **Cust-Num** (10 digits): The customer whose policies should be searched

### Naming Convention
Uses short, mixed-case names (`Type`, `Cust-Num`) without the `CA-` prefix used in the main COMMAREA copybooks.

## 4. Usage Context

### Section Placement
Placed in the LINKAGE SECTION of the SOA policy validation program, mapped from the incoming web service request.

### Data Flow
An external system sends a policy type and customer number. The CICS web service maps the request to this structure. The program looks up valid policies of the specified type for the given customer.

## 5. Analysis

### Role in Architecture
Part of the SOA layer supporting the M&A business scenario. During a merger or acquisition, policy data must be validated and reconciled across companies. This service allows external systems to query for valid policy numbers by type and customer.

### Design Patterns
- **Request/response pair:** Input structure (`soavpii.cpy`) paired with output (`soavpio.cpy`)
- **Type-discriminated lookup:** The `Type` field acts as a filter for the policy search

### Size Considerations
At 90 bytes, this is a small COMMAREA. The 79-byte filler ensures adequate minimum size. The structure is intentionally minimal -- only two meaningful fields are needed to perform the lookup.
