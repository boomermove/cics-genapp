# lgpolicy.cpy -- Policy Details and DB2 Mapping Copybook

## 1. Overview

**File name:** `lgpolicy.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines data structures that map directly to DB2 database table columns for all entity types in the General Insurance application: Customer, Policy, Endowment, House, Motor, Commercial, and Claim. Also provides a centralized set of record-length constants.

This copybook serves as the DB2-to-COBOL data bridge. All field lengths for DB2 table columns are defined here so that if the database schema changes, only this single copybook needs to be updated. The structures are used in SQL `SELECT INTO`, `INSERT`, and `UPDATE` statements within the data-access programs.

## 2. Data Structure Breakdown

### 2.1 WS-POLICY-LENGTHS (Level 01, line 16)

Constants defining the byte lengths of various record types. All are `PIC S9(4) COMP` (binary halfword).

| Level | Field Name | PIC Clause | VALUE | Purpose |
|-------|-----------|------------|-------|---------|
| 03 | WS-CUSTOMER-LEN | S9(4) COMP | +72 | Length of DB2-CUSTOMER record |
| 03 | WS-POLICY-LEN | S9(4) COMP | +72 | Length of DB2-POLICY record |
| 03 | WS-ENDOW-LEN | S9(4) COMP | +52 | Length of endowment-specific fields |
| 03 | WS-HOUSE-LEN | S9(4) COMP | +58 | Length of house-specific fields |
| 03 | WS-MOTOR-LEN | S9(4) COMP | +65 | Length of motor-specific fields (not including padding) |
| 03 | WS-COMM-LEN | S9(4) COMP | +1102 | Length of commercial-specific fields |
| 03 | WS-CLAIM-LEN | S9(4) COMP | +546 | Length of claim-specific fields |
| 03 | WS-FULL-ENDOW-LEN | S9(4) COMP | +124 | Full endowment record (policy common + endowment) |
| 03 | WS-FULL-HOUSE-LEN | S9(4) COMP | +130 | Full house record (policy common + house) |
| 03 | WS-FULL-MOTOR-LEN | S9(4) COMP | +137 | Full motor record (policy common + motor) |
| 03 | WS-FULL-COMM-LEN | S9(4) COMP | +1174 | Full commercial record (policy common + commercial) |
| 03 | WS-FULL-CLAIM-LEN | S9(4) COMP | +618 | Full claim record (policy common + claim) |
| 03 | WS-SUMRY-ENDOW-LEN | S9(4) COMP | +25 | Summary endowment record length |

### 2.2 DB2-CUSTOMER (Level 01, line 31)

Maps to the CUSTOMER table. Total size: 212 bytes.

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | DB2-FIRSTNAME | X(10) | 10 | Customer first name |
| 03 | DB2-LASTNAME | X(20) | 20 | Customer last name |
| 03 | DB2-DATEOFBIRTH | X(10) | 10 | Date of birth |
| 03 | DB2-HOUSENAME | X(20) | 20 | House name |
| 03 | DB2-HOUSENUMBER | X(4) | 4 | House number |
| 03 | DB2-POSTCODE | X(8) | 8 | Postal code |
| 03 | DB2-PHONE-MOBILE | X(20) | 20 | Mobile phone |
| 03 | DB2-PHONE-HOME | X(20) | 20 | Home phone |
| 03 | DB2-EMAIL-ADDRESS | X(100) | 100 | Email address |

### 2.3 DB2-POLICY (Level 01, line 42)

Maps to the POLICY table common header. Total size: 73 bytes.

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | DB2-POLICYTYPE | X | 1 | Policy type code (E=Endowment, H=House, M=Motor, etc.) |
| 03 | DB2-POLICYNUMBER | 9(10) | 10 | Policy number |
| 05 | DB2-ISSUEDATE | X(10) | 10 | Issue date |
| 05 | DB2-EXPIRYDATE | X(10) | 10 | Expiry date |
| 05 | DB2-LASTCHANGED | X(26) | 26 | Last-changed timestamp |
| 05 | DB2-BROKERID | 9(10) | 10 | Broker ID |
| 05 | DB2-BROKERSREF | X(10) | 10 | Broker reference |
| 05 | DB2-PAYMENT | 9(6) | 6 | Payment amount |

The group `DB2-POLICY-COMMON` (level 03, line 45) contains the date, broker, and payment fields (72 bytes).

### 2.4 DB2-ENDOWMENT (Level 01, line 53)

Maps to the ENDOWMENT table.

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | DB2-ENDOW-FIXED (group) | -- | 52 | Fixed endowment fields |
| 05 | DB2-E-WITHPROFITS | X | 1 | With-profits indicator |
| 05 | DB2-E-EQUITIES | X | 1 | Equities indicator |
| 05 | DB2-E-MANAGEDFUND | X | 1 | Managed fund indicator |
| 05 | DB2-E-FUNDNAME | X(10) | 10 | Fund name |
| 05 | DB2-E-TERM | 9(2) | 2 | Term in years |
| 05 | DB2-E-SUMASSURED | 9(6) | 6 | Sum assured |
| 05 | DB2-E-LIFEASSURED | X(31) | 31 | Life assured name |
| 03 | DB2-E-PADDINGDATA | X(32611) | 32611 | Large padding field |

### 2.5 DB2-HOUSE (Level 01, line 64)

Maps to the HOUSE table. Total size: 58 bytes.

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | DB2-H-PROPERTYTYPE | X(15) | 15 | Property type |
| 03 | DB2-H-BEDROOMS | 9(3) | 3 | Number of bedrooms |
| 03 | DB2-H-VALUE | 9(8) | 8 | Property value |
| 03 | DB2-H-HOUSENAME | X(20) | 20 | House name |
| 03 | DB2-H-HOUSENUMBER | X(4) | 4 | House number |
| 03 | DB2-H-POSTCODE | X(8) | 8 | Postcode |

### 2.6 DB2-MOTOR (Level 01, line 72)

Maps to the MOTOR table. Total size: 77 bytes.

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | DB2-M-MAKE | X(15) | 15 | Vehicle make |
| 03 | DB2-M-MODEL | X(15) | 15 | Vehicle model |
| 03 | DB2-M-VALUE | 9(6) | 6 | Vehicle value |
| 03 | DB2-M-REGNUMBER | X(7) | 7 | Registration number |
| 03 | DB2-M-COLOUR | X(8) | 8 | Colour |
| 03 | DB2-M-CC | 9(4) | 4 | Engine cc |
| 03 | DB2-M-MANUFACTURED | X(10) | 10 | Manufacture date |
| 03 | DB2-M-PREMIUM | 9(6) | 6 | Premium |
| 03 | DB2-M-ACCIDENTS | 9(6) | 6 | Accident count |

### 2.7 DB2-COMMERCIAL (Level 01, line 83)

Maps to the COMMERCIAL table. Total size: 1,102 bytes.

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | DB2-B-Address | X(255) | 255 | Business address |
| 03 | DB2-B-Postcode | X(8) | 8 | Business postcode |
| 03 | DB2-B-Latitude | X(11) | 11 | Latitude |
| 03 | DB2-B-Longitude | X(11) | 11 | Longitude |
| 03 | DB2-B-Customer | X(255) | 255 | Customer name |
| 03 | DB2-B-PropType | X(255) | 255 | Property type |
| 03 | DB2-B-FirePeril | 9(4) | 4 | Fire peril rating |
| 03 | DB2-B-FirePremium | 9(8) | 8 | Fire premium |
| 03 | DB2-B-CrimePeril | 9(4) | 4 | Crime peril |
| 03 | DB2-B-CrimePremium | 9(8) | 8 | Crime premium |
| 03 | DB2-B-FloodPeril | 9(4) | 4 | Flood peril |
| 03 | DB2-B-FloodPremium | 9(8) | 8 | Flood premium |
| 03 | DB2-B-WeatherPeril | 9(4) | 4 | Weather peril |
| 03 | DB2-B-WeatherPremium | 9(8) | 8 | Weather premium |
| 03 | DB2-B-Status | 9(4) | 4 | Policy status code |
| 03 | DB2-B-RejectReason | X(255) | 255 | Rejection reason text |

### 2.8 DB2-CLAIM (Level 01, line 101)

Maps to the CLAIM table. Total size: 546 bytes.

| Level | Field Name | PIC Clause | Size | Purpose |
|-------|-----------|------------|------|---------|
| 03 | DB2-C-Num | 9(10) | 10 | Claim number |
| 03 | DB2-C-Date | X(10) | 10 | Claim date |
| 03 | DB2-C-Paid | 9(8) | 8 | Amount paid |
| 03 | DB2-C-Value | 9(8) | 8 | Claim value |
| 03 | DB2-C-Cause | X(255) | 255 | Cause description |
| 03 | DB2-C-Observations | X(255) | 255 | Observations |

## 3. Key Components

### Length Constants
The `WS-POLICY-LENGTHS` structure acts as a centralized registry of record sizes. The "full" lengths (e.g., `WS-FULL-ENDOW-LEN` = 124) represent the combined size of the policy common header (72 bytes) plus the type-specific fields (e.g., 52 for endowment = 124 total).

### No Condition Names or REDEFINES
This copybook uses no 88-level conditions and no REDEFINES -- each DB2 entity is a separate level-01 structure.

### Naming Convention
All DB2-mapped fields use the `DB2-` prefix, making it clear these are host variables for SQL operations. Policy-type fields use a second-level prefix: `DB2-E-` (endowment), `DB2-H-` (house), `DB2-M-` (motor), `DB2-B-` (commercial/business), `DB2-C-` (claim).

## 4. Usage Context

### Programs That COPY This File
Used by 11 programs, primarily the DB2 data-access layer:
- `lgacdb01.cbl`, `lgacdb02.cbl` -- Add customer to DB2
- `lgicdb01.cbl` -- Inquire customer from DB2
- `lgucdb01.cbl` -- Update customer in DB2
- `lgapdb01.cbl` -- Add policy to DB2
- `lgipdb01.cbl` -- Inquire policy from DB2
- `lgupdb01.cbl` -- Update policy in DB2
- `lgicus01.cbl` -- Inquire customer (uses customer structure)
- `lgacus01.cbl` -- Add customer
- `lgipol01.cbl` -- Inquire policy
- `lgacdb01_secure.cbl` -- Secure variant of add customer

### Section Placement
Included in the **WORKING-STORAGE SECTION**. The level-01 structures are used as host variables in embedded SQL statements.

### Data Flow
Programs receive a request via the COMMAREA (`lgcmarea.cpy`), then use the `DB2-*` structures in this copybook as host variables for SQL operations. Data is moved between the COMMAREA fields and the DB2 host variables before/after SQL execution.

## 5. Analysis

### Role in Architecture
This copybook is the **database abstraction layer's data dictionary**. It decouples the COMMAREA format from the DB2 table layout, allowing either to change independently (within reason). The length constants provide a single source of truth for COMMAREA sizing calculations.

### Design Patterns
- **Host variable pattern:** Each DB2 structure maps 1:1 to a database table's column set.
- **Centralized length management:** All record sizes are defined as named constants rather than hard-coded literals.
- **Separation of concerns:** The COMMAREA layout (`lgcmarea.cpy`) and the DB2 layout (`lgpolicy.cpy`) are separate copybooks, even though they describe similar data.

### Size Considerations
- `DB2-ENDOWMENT` includes a large 32,611-byte padding field -- this is unusual and likely exists to support bulk data transfer scenarios.
- The length constants allow programs to pass precisely-sized data areas in CICS LINK calls.
