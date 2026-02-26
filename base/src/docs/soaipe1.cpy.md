# soaipe1.cpy -- SOA Endowment Policy Inquiry COMMAREA Copybook

## 1. Overview

**File name:** `soaipe1.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the COMMAREA structure for endowment policy inquiry operations via the SOA interface. Contains common policy header fields plus endowment-specific fields for investment type flags, fund details, term, and life assurance.

The "ipe" in the filename stands for "inquiry policy endowment." This is the SOA/web-service-compatible version of the endowment policy data from the main COMMAREA.

## 2. Data Structure Breakdown

Fields are at level 03 (no level 01 wrapper).

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | CA-REQUEST-ID | X(6) | 6 | Request type identifier |
| 03 | CA-RETURN-CODE | 9(2) | 2 | Return code |
| 03 | CA-CUSTOMER-NUM | 9(10) | 10 | Customer number |
| 03 | CA-POLICY-NUM | 9(10) | 10 | Policy number |
| 03 | CA-ISSUE-DATE | X(10) | 10 | Policy issue date |
| 03 | CA-EXPIRY-DATE | X(10) | 10 | Policy expiry date |
| 03 | CA-LASTCHANGED | X(26) | 26 | Last-changed timestamp |
| 03 | CA-BROKERID | 9(10) | 10 | Broker identifier |
| 03 | CA-BROKERSREF | X(10) | 10 | Broker reference |
| 03 | CA-PAYMENT | 9(6) | 6 | Payment amount |
| 03 | CA-E-WITH-PROFITS | X | 1 | With-profits flag |
| 03 | CA-E-EQUITIES | X | 1 | Equities flag |
| 03 | CA-E-MANAGED-FUND | X | 1 | Managed fund flag |
| 03 | CA-E-FUND-NAME | X(10) | 10 | Fund name |
| 03 | CA-E-TERM | 99 | 2 | Term in years |
| 03 | CA-E-SUM-ASSURED | 9(6) | 6 | Sum assured amount |
| 03 | CA-E-LIFE-ASSURED | X(31) | 31 | Life assured name |
| 03 | CA-E-PADDING-DATA | X(30000) | 30000 | Padding for WSIM compatibility |

Total: 30,152 bytes.

## 3. Key Components

### Endowment-Specific Fields (lines 18-25)
- **Investment type flags:** Three single-character flags (`CA-E-WITH-PROFITS`, `CA-E-EQUITIES`, `CA-E-MANAGED-FUND`) indicate the type of endowment investment
- **Fund details:** `CA-E-FUND-NAME` (10 chars) and `CA-E-TERM` (2-digit years)
- **Assurance:** `CA-E-SUM-ASSURED` (6-digit amount) and `CA-E-LIFE-ASSURED` (31-char name)

### Differences from lgcmarea.cpy
- Flat structure with no REDEFINES
- `CA-E-PADDING-DATA` is 30,000 bytes (vs. 32,348 in the main COMMAREA)
- Includes `CA-PAYMENT` which is part of the common policy header

### Comment on Line 26
`*    Reduce size to below 32K for WSIM support`

## 4. Usage Context

### Section Placement
Included within a level 01 structure in SOA endowment policy programs.

### Data Flow
Used by CICS web services for endowment policy inquiry. The web service maps the SOAP request, calls the back-end program with this COMMAREA, and returns the endowment data via SOAP response.

## 5. Analysis

### Role in Architecture
Part of the SOA layer for endowment (life insurance) policies. Endowment policies are investment-linked life insurance products with a savings component.

### Design Patterns
- **Flag-based configuration:** Three single-character flags define the investment mix
- **Flat serialization-friendly layout**

### Size Considerations
The endowment-specific fields total only 52 bytes. The vast majority of the 30,152-byte structure is padding.
