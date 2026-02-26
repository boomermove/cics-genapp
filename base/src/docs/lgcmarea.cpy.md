# lgcmarea.cpy -- COMMAREA Structure Copybook

## 1. Overview

**File name:** `lgcmarea.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines the Communication Area (COMMAREA) structure used for data exchange between CICS programs in the General Insurance (GenApp) application.

This copybook provides a single, universal COMMAREA layout that supports all application functions: customer inquiry/add/update, customer security operations, and all policy types (endowment, house, motor, commercial, and claims). It achieves this through a set of REDEFINES on a large generic field (`CA-REQUEST-SPECIFIC`, 32,482 bytes), allowing the same memory area to be interpreted differently depending on the request type indicated by `CA-REQUEST-ID`.

## 2. Data Structure Breakdown

The copybook defines fields at level 03 and below (designed to be included under a level 01 in the consuming program). The total COMMAREA size is 6 (request ID) + 2 (return code) + 10 (customer num) + 32,482 (request-specific) = **32,500 bytes**.

### 2.1 Common Header Fields (lines 10-13)

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 03 | CA-REQUEST-ID | X(6) | Identifies the type of request (e.g., inquiry, add, update, delete) |
| 03 | CA-RETURN-CODE | 9(2) | Two-digit numeric return code from the called program |
| 03 | CA-CUSTOMER-NUM | 9(10) | Ten-digit customer number |
| 03 | CA-REQUEST-SPECIFIC | X(32482) | Generic request payload; redefined for each request type |

### 2.2 CA-CUSTOMER-REQUEST (REDEFINES CA-REQUEST-SPECIFIC, line 15)

Used for customer inquiry-all and add-customer operations.

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | CA-FIRST-NAME | X(10) | Customer first name |
| 05 | CA-LAST-NAME | X(20) | Customer last name |
| 05 | CA-DOB | X(10) | Date of birth |
| 05 | CA-HOUSE-NAME | X(20) | House/building name |
| 05 | CA-HOUSE-NUM | X(4) | House number |
| 05 | CA-POSTCODE | X(8) | Postal code |
| 05 | CA-NUM-POLICIES | 9(3) | Number of policies held |
| 05 | CA-PHONE-MOBILE | X(20) | Mobile phone number |
| 05 | CA-PHONE-HOME | X(20) | Home phone number |
| 05 | CA-EMAIL-ADDRESS | X(100) | Email address |
| 05 | CA-POLICY-DATA | X(32267) | Remaining space for policy list data |

Total named fields: 10 + 20 + 10 + 20 + 4 + 8 + 3 + 20 + 20 + 100 + 32,267 = 32,482 bytes.

### 2.3 CA-CUSTSECR-REQUEST (REDEFINES CA-REQUEST-SPECIFIC, line 28)

Used for customer security calls (authentication).

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | CA-CUSTSECR-PASS | X(32) | Customer security password |
| 05 | CA-CUSTSECR-COUNT | X(4) | Login attempt count |
| 05 | CA-CUSTSECR-STATE | X | Account state flag |
| 05 | CA-CUSTSECR-DATA | X(32445) | Remaining security data space |

### 2.4 CA-POLICY-REQUEST (REDEFINES CA-REQUEST-SPECIFIC, line 34)

Used for policy inquiry, update, add, and delete operations. Contains a common policy header and type-specific sub-sections.

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | CA-POLICY-NUM | 9(10) | Ten-digit policy number |
| 05 | CA-POLICY-COMMON | (group) | Common policy header fields |
| 07 | CA-ISSUE-DATE | X(10) | Policy issue date |
| 07 | CA-EXPIRY-DATE | X(10) | Policy expiry date |
| 07 | CA-LASTCHANGED | X(26) | Last-changed timestamp |
| 07 | CA-BROKERID | 9(10) | Broker identifier |
| 07 | CA-BROKERSREF | X(10) | Broker's reference |
| 07 | CA-PAYMENT | 9(6) | Payment amount |
| 05 | CA-POLICY-SPECIFIC | X(32400) | Policy-type-specific data area |

#### 2.4.1 CA-ENDOWMENT (REDEFINES CA-POLICY-SPECIFIC, line 46)

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 07 | CA-E-WITH-PROFITS | X | With-profits flag (Y/N) |
| 07 | CA-E-EQUITIES | X | Equities flag |
| 07 | CA-E-MANAGED-FUND | X | Managed fund flag |
| 07 | CA-E-FUND-NAME | X(10) | Fund name |
| 07 | CA-E-TERM | 99 | Term in years (2 digits) |
| 07 | CA-E-SUM-ASSURED | 9(6) | Sum assured amount |
| 07 | CA-E-LIFE-ASSURED | X(31) | Life assured name |
| 07 | CA-E-PADDING-DATA | X(32348) | Padding to fill 32,400 bytes |

#### 2.4.2 CA-HOUSE (REDEFINES CA-POLICY-SPECIFIC, line 56)

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 07 | CA-H-PROPERTY-TYPE | X(15) | Type of property |
| 07 | CA-H-BEDROOMS | 9(3) | Number of bedrooms |
| 07 | CA-H-VALUE | 9(8) | Property value |
| 07 | CA-H-HOUSE-NAME | X(20) | House name |
| 07 | CA-H-HOUSE-NUMBER | X(4) | House number |
| 07 | CA-H-POSTCODE | X(8) | Postcode |
| 07 | CA-H-FILLER | X(32342) | Padding |

#### 2.4.3 CA-MOTOR (REDEFINES CA-POLICY-SPECIFIC, line 65)

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 07 | CA-M-MAKE | X(15) | Vehicle make |
| 07 | CA-M-MODEL | X(15) | Vehicle model |
| 07 | CA-M-VALUE | 9(6) | Vehicle value |
| 07 | CA-M-REGNUMBER | X(7) | Registration number |
| 07 | CA-M-COLOUR | X(8) | Vehicle colour |
| 07 | CA-M-CC | 9(4) | Engine capacity (cc) |
| 07 | CA-M-MANUFACTURED | X(10) | Manufacture date |
| 07 | CA-M-PREMIUM | 9(6) | Premium amount |
| 07 | CA-M-ACCIDENTS | 9(6) | Number of accidents |
| 07 | CA-M-FILLER | X(32323) | Padding |

#### 2.4.4 CA-COMMERCIAL (REDEFINES CA-POLICY-SPECIFIC, line 77)

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 07 | CA-B-Address | X(255) | Business address |
| 07 | CA-B-Postcode | X(8) | Business postcode |
| 07 | CA-B-Latitude | X(11) | Geographic latitude |
| 07 | CA-B-Longitude | X(11) | Geographic longitude |
| 07 | CA-B-Customer | X(255) | Customer name |
| 07 | CA-B-PropType | X(255) | Property type description |
| 07 | CA-B-FirePeril | 9(4) | Fire peril rating |
| 07 | CA-B-FirePremium | 9(8) | Fire premium |
| 07 | CA-B-CrimePeril | 9(4) | Crime peril rating |
| 07 | CA-B-CrimePremium | 9(8) | Crime premium |
| 07 | CA-B-FloodPeril | 9(4) | Flood peril rating |
| 07 | CA-B-FloodPremium | 9(8) | Flood premium |
| 07 | CA-B-WeatherPeril | 9(4) | Weather peril rating |
| 07 | CA-B-WeatherPremium | 9(8) | Weather premium |
| 07 | CA-B-Status | 9(4) | Policy status |
| 07 | CA-B-RejectReason | X(255) | Rejection reason |
| 07 | CA-B-FILLER | X(31298) | Padding |

#### 2.4.5 CA-CLAIM (REDEFINES CA-POLICY-SPECIFIC, line 96)

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 07 | CA-C-Num | 9(10) | Claim number |
| 07 | CA-C-Date | X(10) | Claim date |
| 07 | CA-C-Paid | 9(8) | Amount paid |
| 07 | CA-C-Value | 9(8) | Claim value |
| 07 | CA-C-Cause | X(255) | Cause of claim |
| 07 | CA-C-Observations | X(255) | Observations/notes |
| 07 | CA-C-FILLER | X(31854) | Padding |

## 3. Key Components

### REDEFINES Chain
The central design is a four-way REDEFINES on `CA-REQUEST-SPECIFIC` (line 13):
1. `CA-CUSTOMER-REQUEST` (line 15) -- customer operations
2. `CA-CUSTSECR-REQUEST` (line 28) -- security/authentication
3. `CA-POLICY-REQUEST` (line 34) -- all policy operations

Within `CA-POLICY-REQUEST`, the field `CA-POLICY-SPECIFIC` (line 44) is itself redefined five ways:
1. `CA-ENDOWMENT` (line 46)
2. `CA-HOUSE` (line 56)
3. `CA-MOTOR` (line 65)
4. `CA-COMMERCIAL` (line 77)
5. `CA-CLAIM` (line 96)

### Constants and Defaults
No VALUE clauses are defined -- all fields are pure data carriers populated at runtime.

### No 88-Level Conditions
This copybook contains no condition names.

## 4. Usage Context

### Programs That COPY This File
This copybook is included by virtually every COBOL program in the application (27 programs), including:
- **Customer programs:** `lgacus01.cbl`, `lgucus01.cbl`, `lgicus01.cbl`
- **Policy programs:** `lgapol01.cbl`, `lgipol01.cbl`, `lgupol01.cbl`, `lgdpol01.cbl`
- **DB2 access programs:** `lgacdb01.cbl`, `lgacdb02.cbl`, `lgicdb01.cbl`, `lgucdb01.cbl`, `lgapdb01.cbl`, `lgipdb01.cbl`, `lgupdb01.cbl`, `lgdpdb01.cbl`
- **VSAM access programs:** `lgacvs01.cbl`, `lgucvs01.cbl`, `lgapvs01.cbl`, `lgupvs01.cbl`, `lgdpvs01.cbl`
- **Test programs:** `lgtestc1.cbl`, `lgtestp1.cbl`, `lgtestp2.cbl`, `lgtestp3.cbl`, `lgtestp4.cbl`
- **Statistics:** `lgastat1.cbl`
- **Secure variant:** `lgacdb01_secure.cbl`

### Section Placement
The fields begin at level 03, so they are included under a level 01 item (typically `DFHCOMMAREA` in the LINKAGE SECTION, or a working-storage copy of the commarea).

### Data Flow
The COMMAREA is passed via `EXEC CICS LINK` or `EXEC CICS XCTL` between the front-end screen-handling programs and the back-end data access programs. The `CA-REQUEST-ID` field tells the receiving program which REDEFINES overlay to use.

## 5. Analysis

### Role in Architecture
This is the **central data contract** of the GenApp application. Every inter-program communication uses this structure. It acts as a universal message envelope: the header (request ID, return code, customer number) is always present, while the body is polymorphic via REDEFINES.

### Design Patterns
- **Union/variant record pattern:** Multiple REDEFINES on the same storage provide type-safe overlays for different business entities.
- **Fixed-size COMMAREA:** The total size is always 32,500 bytes regardless of which overlay is active. This is a CICS convention to avoid length mismatches.
- **Padding fields:** Each policy sub-type includes a FILLER/padding field to ensure the overlay fills exactly 32,400 bytes.

### Size Considerations
- Total COMMAREA: **32,500 bytes** (just under the 32KB CICS COMMAREA practical limit)
- The `CA-REQUEST-SPECIFIC` area is 32,482 bytes
- `CA-POLICY-SPECIFIC` is 32,400 bytes
- Each policy-type overlay pads to exactly 32,400 bytes
