# soaipm1.cpy -- SOA Motor Policy Inquiry COMMAREA Copybook

## 1. Overview

**File name:** `soaipm1.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the COMMAREA structure for motor (vehicle) policy inquiry operations via the SOA interface. Contains common policy header fields plus motor-specific fields for vehicle details, premium, and accident history.

The "ipm" in the filename stands for "inquiry policy motor." This is the SOA/web-service-compatible version of the motor policy data.

## 2. Data Structure Breakdown

Fields are at level 03 (no level 01 wrapper). Note: line 9 has `CA-POLICY-NUM` at level 05 (inconsistent with the surrounding level 03 fields -- likely a minor coding error that does not affect functionality since it is subordinate to an implicit group).

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | CA-REQUEST-ID | X(6) | 6 | Request type identifier |
| 03 | CA-RETURN-CODE | 9(2) | 2 | Return code |
| 03 | CA-CUSTOMER-NUM | 9(10) | 10 | Customer number |
| 05 | CA-POLICY-NUM | 9(10) | 10 | Policy number (note: level 05, not 03) |
| 03 | CA-ISSUE-DATE | X(10) | 10 | Policy issue date |
| 03 | CA-EXPIRY-DATE | X(10) | 10 | Policy expiry date |
| 03 | CA-LASTCHANGED | X(26) | 26 | Last-changed timestamp |
| 03 | CA-BROKERID | 9(10) | 10 | Broker identifier |
| 03 | CA-BROKERSREF | X(10) | 10 | Broker reference |
| 03 | CA-PAYMENT | 9(6) | 6 | Payment amount |
| 03 | CA-M-MAKE | X(15) | 15 | Vehicle manufacturer |
| 03 | CA-M-MODEL | X(15) | 15 | Vehicle model |
| 03 | CA-M-VALUE | 9(6) | 6 | Vehicle value |
| 03 | CA-M-REGNUMBER | X(7) | 7 | Registration/license plate number |
| 03 | CA-M-COLOUR | X(8) | 8 | Vehicle colour |
| 03 | CA-M-CC | 9(4) | 4 | Engine capacity in cc |
| 03 | CA-M-MANUFACTURED | X(10) | 10 | Manufacture date |
| 03 | CA-M-PREMIUM | 9(6) | 6 | Insurance premium |
| 03 | CA-M-ACCIDENTS | 9(6) | 6 | Number of accidents |
| 03 | CA-M-FILLER | X(30000) | 30000 | Padding for WSIM compatibility |

Total: 30,177 bytes.

## 3. Key Components

### Motor-Specific Fields (lines 18-27)
- **Vehicle identification:** Make, model, registration number, colour
- **Vehicle specs:** Value, engine CC, manufacture date
- **Insurance data:** Premium amount, accident count

### Level Numbering Anomaly
On line 9, `CA-POLICY-NUM` is defined at level 05 while all other fields are at level 03. This is likely a typographical error. In practice, since there is no enclosing level 03 group immediately above it, the COBOL compiler may treat it differently depending on the compilation context.

### Differences from lgcmarea.cpy
- Flat structure, no REDEFINES
- `CA-M-FILLER` is 30,000 bytes (vs. 32,323 in the main COMMAREA)
- Includes `CA-PAYMENT` from common policy header

### Comment on Line 28
`*    Reduce size to below 32K for WSIM support`

## 4. Usage Context

### Section Placement
Included within a level 01 structure in SOA motor policy programs.

### Data Flow
CICS web services map SOAP requests to this flat structure for motor policy inquiry, invoke the appropriate back-end, and return the vehicle and policy data as XML.

## 5. Analysis

### Role in Architecture
Part of the SOA layer for motor (automobile) insurance policies. Motor insurance is a core product for the General Insurance application, covering vehicle damage, theft, and third-party liability.

### Design Patterns
- **Comprehensive vehicle data model:** Nine vehicle-specific fields capture all relevant details for motor insurance underwriting
- **Flat serialization-friendly layout**

### Size Considerations
Motor-specific fields total 77 bytes. With the common header, the meaningful data is 177 bytes out of 30,177 total.
