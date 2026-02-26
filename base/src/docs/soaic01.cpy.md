# soaic01.cpy -- SOA Customer Inquiry COMMAREA Copybook

## 1. Overview

**File name:** `soaic01.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the COMMAREA structure for customer inquiry operations when accessed via the SOA (Service-Oriented Architecture) interface. This is a flattened, non-REDEFINES version of the customer portion of the main COMMAREA, sized below 32K for web service (WSIM) compatibility.

This copybook provides a simplified view of customer data, including all customer demographic fields and a large policy data area. It is designed for the SOA/web service layer of GenApp where CICS web services need a clean, flat COMMAREA structure.

## 2. Data Structure Breakdown

### CA (Level 01, line 6)

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | CA-REQUEST-ID | X(6) | 6 | Request type identifier |
| 03 | CA-RETURN-CODE | 9(2) | 2 | Return code |
| 03 | CA-CUSTOMER-NUM | 9(10) | 10 | Customer number |
| 03 | CA-FIRST-NAME | X(10) | 10 | Customer first name |
| 03 | CA-LAST-NAME | X(20) | 20 | Customer last name |
| 03 | CA-DOB | X(10) | 10 | Date of birth |
| 03 | CA-HOUSE-NAME | X(20) | 20 | House name |
| 03 | CA-HOUSE-NUM | X(4) | 4 | House number |
| 03 | CA-POSTCODE | X(8) | 8 | Postal code |
| 03 | CA-NUM-POLICIES | 9(3) | 3 | Number of policies |
| 03 | CA-PHONE-MOBILE | X(20) | 20 | Mobile phone |
| 03 | CA-PHONE-HOME | X(20) | 20 | Home phone |
| 03 | CA-EMAIL-ADDRESS | X(100) | 100 | Email address |
| 03 | CA-POLICY-DATA | X(30000) | 30000 | Policy data payload |

Total size: 30,253 bytes.

### Comment on Line 22
The comment `*    Reduce size to below 32K for WSIM support` explains why `CA-POLICY-DATA` is 30,000 bytes instead of the 32,267 bytes in the main COMMAREA's `CA-CUSTOMER-REQUEST`. This keeps the total under 32K for Web Services Infrastructure for the Mainframe (WSIM) compatibility.

## 3. Key Components

### Differences from lgcmarea.cpy
- All fields are at level 03 (flat structure) -- no REDEFINES
- `CA-POLICY-DATA` is reduced from 32,267 to 30,000 bytes
- Total COMMAREA is 30,253 bytes instead of 32,500 bytes
- The level 01 name is `CA` instead of being unnamed (designed for inclusion under another structure)

### No Condition Names, REDEFINES, or VALUE Clauses
This is a pure flat data layout with no conditions, redefinitions, or initial values.

## 4. Usage Context

### Section Placement
The level 01 `CA` structure is placed in the LINKAGE SECTION of SOA-facing programs. It maps the COMMAREA passed by the CICS web service pipeline.

### Data Flow
CICS Web Services receive a SOAP request, map it to this COMMAREA structure, and LINK to the customer inquiry program. The program populates the customer fields and returns the data, which is then mapped back to a SOAP response.

## 5. Analysis

### Role in Architecture
This copybook is part of the **SOA integration layer**, providing web-service-compatible data structures. The "soa" prefix and "ic" (inquiry customer) suffix in the filename identify its role.

### Design Patterns
- **Flat structure for serialization:** No REDEFINES or complex nesting, making it straightforward to map to/from XML/SOAP.
- **Size-constrained design:** Explicitly reduced to fit within WSIM's 32K limit.
- **Parallel structure:** Field names match the main COMMAREA (`lgcmarea.cpy`) for easy data movement between internal and SOA structures.

### Size Considerations
At 30,253 bytes, this structure is 2,247 bytes smaller than the full COMMAREA. The size reduction is entirely in the `CA-POLICY-DATA` padding field.
