# soaiph1.cpy -- SOA House Policy Inquiry COMMAREA Copybook

## 1. Overview

**File name:** `soaiph1.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the COMMAREA structure for house (home) policy inquiry operations via the SOA interface. Contains common policy header fields plus house-specific fields for property details.

The "iph" in the filename stands for "inquiry policy house." This is the SOA/web-service-compatible version of the house policy data.

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
| 03 | CA-H-PROPERTY-TYPE | X(15) | 15 | Property type description |
| 03 | CA-H-BEDROOMS | 9(3) | 3 | Number of bedrooms |
| 03 | CA-H-VALUE | 9(8) | 8 | Property value |
| 03 | CA-H-HOUSE-NAME | X(20) | 20 | House name |
| 03 | CA-H-HOUSE-NUMBER | X(4) | 4 | House number |
| 03 | CA-H-POSTCODE | X(8) | 8 | Postcode |
| 03 | CA-H-FILLER | X(30000) | 30000 | Padding for WSIM compatibility |

Total: 30,158 bytes.

## 3. Key Components

### House-Specific Fields (lines 18-24)
- **Property type:** `CA-H-PROPERTY-TYPE` (15 chars) -- e.g., "Detached", "Semi", "Terraced"
- **Property size:** `CA-H-BEDROOMS` (3-digit number)
- **Valuation:** `CA-H-VALUE` (8-digit amount)
- **Address:** `CA-H-HOUSE-NAME` (20 chars), `CA-H-HOUSE-NUMBER` (4 chars), `CA-H-POSTCODE` (8 chars)

### Differences from lgcmarea.cpy
- Flat structure, no REDEFINES
- Includes `CA-PAYMENT` from the common policy header
- `CA-H-FILLER` is 30,000 bytes (vs. 32,342 in the main COMMAREA)

### Comment on Line 25
`*    Reduce size to below 32K for WSIM support`

## 4. Usage Context

### Section Placement
Included within a level 01 structure in SOA house policy programs.

### Data Flow
The CICS web service infrastructure maps SOAP requests for house policy inquiry to this structure, invokes the appropriate back-end program, and maps the results back to XML.

## 5. Analysis

### Role in Architecture
Part of the SOA layer for home/house insurance policies. Home insurance covers property damage, theft, and liability for residential properties.

### Design Patterns
- **Property-centric data model:** Address fields (name, number, postcode) are specific to the insured property, not the customer's address
- **Flat serialization-friendly layout**

### Size Considerations
The house-specific fields total 58 bytes. Combined with the 100-byte common header, the meaningful data is only 158 bytes out of 30,158 total.
