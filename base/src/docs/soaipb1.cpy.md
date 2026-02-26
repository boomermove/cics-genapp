# soaipb1.cpy -- SOA Commercial Policy Inquiry COMMAREA Copybook

## 1. Overview

**File name:** `soaipb1.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the COMMAREA structure for commercial (business) policy inquiry operations via the SOA interface. Contains common policy header fields plus all commercial policy-specific fields including address, geographic coordinates, peril ratings, and premiums.

This copybook provides a flat, web-service-compatible layout for commercial property insurance policy data. The "ipb" in the filename stands for "inquiry policy business/commercial."

## 2. Data Structure Breakdown

The copybook defines fields at level 03 (no level 01 wrapper -- designed to be included under an existing level 01).

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | CA-REQUEST-ID | X(6) | 6 | Request type identifier |
| 03 | CA-RETURN-CODE | 9(2) | 2 | Return code |
| 03 | CA-CUSTOMER-NUM | 9(10) | 10 | Customer number |
| 03 | CA-POLICY-NUM | 9(10) | 10 | Policy number |
| 03 | CA-ISSUE-DATE | X(10) | 10 | Policy issue/start date |
| 03 | CA-EXPIRY-DATE | X(10) | 10 | Policy expiry date |
| 03 | CA-LASTCHANGED | X(26) | 26 | Last-changed timestamp |
| 03 | CA-BROKERID | 9(10) | 10 | Broker identifier |
| 03 | CA-BROKERSREF | X(10) | 10 | Broker reference |
| 03 | CA-B-Address | X(255) | 255 | Business property address |
| 03 | CA-B-Postcode | X(8) | 8 | Business postcode |
| 03 | CA-B-Latitude | X(11) | 11 | Geographic latitude |
| 03 | CA-B-Longitude | X(11) | 11 | Geographic longitude |
| 03 | CA-B-Customer | X(255) | 255 | Customer name |
| 03 | CA-B-PropType | X(255) | 255 | Property type description |
| 03 | CA-B-FirePeril | x(4) | 4 | Fire peril rating |
| 03 | CA-B-FirePremium | x(8) | 8 | Fire premium amount |
| 03 | CA-B-CrimePeril | x(4) | 4 | Crime peril rating |
| 03 | CA-B-CrimePremium | x(8) | 8 | Crime premium amount |
| 03 | CA-B-FloodPeril | x(4) | 4 | Flood peril rating |
| 03 | CA-B-FloodPremium | x(8) | 8 | Flood premium amount |
| 03 | CA-B-WeatherPeril | x(4) | 4 | Weather peril rating |
| 03 | CA-B-WeatherPremium | x(8) | 8 | Weather premium amount |
| 03 | CA-B-Status | x(4) | 4 | Policy status code |
| 03 | CA-B-RejectReason | X(255) | 255 | Rejection reason text |
| 03 | CA-B-FILLER | X(30000) | 30000 | Padding for WSIM compatibility |

Total named data: approximately 1,453 bytes + 30,000 padding = 31,453 bytes.

### Notable PIC Clause Detail
The peril and premium fields (lines 23-31) use lowercase `x` in their PIC clauses (e.g., `PIC x(4)`), which is functionally identical to uppercase `X` but is a stylistic inconsistency with the rest of the copybook. This suggests the commercial policy fields may have been added by a different developer.

## 3. Key Components

### Differences from lgcmarea.cpy
- No REDEFINES -- fully flat structure
- Commercial peril/premium fields use `PIC x()` (alphanumeric) instead of `PIC 9()` (numeric) as in `lgcmarea.cpy`. This means the SOA version treats these as character strings rather than numeric values.
- Missing `CA-PAYMENT` field that exists in the main COMMAREA's policy common section
- `CA-B-FILLER` is 30,000 bytes (vs. 31,298 in the main COMMAREA)

### Comment on Line 34
`*    Reduce size to below 32K for WSIM support` confirms the size reduction purpose.

## 4. Usage Context

### Section Placement
Designed to be included within a level 01 structure in either WORKING-STORAGE or LINKAGE SECTION of SOA commercial policy programs.

### Data Flow
The CICS web service infrastructure maps incoming SOAP requests to this flat structure, invokes the commercial policy inquiry program, and maps the returned data back to XML.

## 5. Analysis

### Role in Architecture
Part of the SOA integration layer for commercial property insurance. The commercial policy is the most complex policy type with four separate peril/premium pairs, geographic coordinates, and extensive text fields.

### Design Patterns
- **Flat structure for XML mapping:** No nesting or REDEFINES
- **Four-peril model:** Fire, crime, flood, and weather perils each with a rating and premium amount
- **Geographic data:** Latitude/longitude fields support location-based risk assessment

### Size Considerations
The commercial policy has the most data fields of any policy type. The named fields total approximately 1,453 bytes before the 30,000-byte filler.
