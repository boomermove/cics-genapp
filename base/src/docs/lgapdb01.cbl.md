# LGAPDB01.cbl - Add Policy (DB2)

## 1. Overview

- **Program ID:** LGAPDB01
- **Purpose:** Insert a new policy record and its associated type-specific record into the appropriate DB2 tables (POLICY plus one of ENDOWMENT, HOUSE, MOTOR, or COMMERCIAL).
- **Summary:** LGAPDB01 is the DB2 data-access program for policy creation in the GenApp insurance application. It handles four types of insurance policies: Endowment, House, Motor, and Commercial. For each, it first inserts a row into the POLICY table (with DB2 generating the policy number via an identity column), retrieves the assigned policy number and timestamp, then inserts the type-specific details into the corresponding sub-table. After the DB2 inserts, it links to LGAPVS01 to write a VSAM copy of the policy record. The program uses SQL error handling with CICS ABEND for rollback of partial inserts.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 141-248)

1. **Initialization (lines 144-154):** Initializes `WS-HEADER`, captures EIB fields, stores commarea length, initializes DB2 host variables.
2. **Commarea validation (lines 160-208):**
   - Abends with `'LGCA'` if no commarea.
   - Sets `CA-RETURN-CODE` to `'00'`.
   - Converts `CA-CUSTOMER-NUM` to `DB2-CUSTOMERNUM-INT`.
   - Uses `EVALUATE CA-REQUEST-ID` to determine policy type and required commarea length:
     - `'01AEND'`: Endowment, requires `WS-CA-HEADER-LEN` + `WS-FULL-ENDOW-LEN` (28 + 124 = 152)
     - `'01AHOU'`: House, requires 28 + 130 = 158
     - `'01AMOT'`: Motor, requires 28 + 137 = 165
     - `'01ACOM'`: Commercial, requires 28 + 1174 = 1202
     - Other: return code `'99'`
   - If commarea too short, returns `'98'`.
3. **INSERT operations (lines 214-241):**
   - Performs `INSERT-POLICY` (common policy table insert).
   - Based on `CA-REQUEST-ID`, performs the appropriate type-specific insert: `INSERT-ENDOW`, `INSERT-HOUSE`, `INSERT-MOTOR`, or `INSERT-COMMERCIAL`.
   - Links to `LGAPVS01` for VSAM write.
4. **Return (line 245):** Returns to caller.

### INSERT-POLICY (lines 256-317)

1. Converts `CA-BROKERID` and `CA-PAYMENT` to integer format.
2. Inserts into POLICY with `DEFAULT` for POLICYNUMBER (identity column) and `CURRENT TIMESTAMP` for LASTCHANGED.
3. Evaluates SQLCODE:
   - `0`: Success, sets return code `'00'`.
   - `-530`: Referential integrity violation (foreign key), sets `'70'`.
   - Other: SQL error, sets `'90'`.
4. Retrieves assigned policy number via `IDENTITY_VAL_LOCAL()`.
5. Retrieves the `LASTCHANGED` timestamp via SELECT.

### INSERT-ENDOW (lines 322-392)

1. Converts term and sum assured to integer format.
2. Calculates `WS-VARY-LEN` (extra bytes beyond required length) for VARCHAR padding data.
3. Two INSERT paths:
   - With VARCHAR `PADDINGDATA` if extra data is present.
   - Without VARCHAR if no extra data.
4. On error: sets `'90'`, writes error, abends with `'LGSQ'` to trigger backout.

### INSERT-HOUSE (lines 397-430)

1. Converts value and bedrooms to integer format.
2. Inserts into HOUSE table with property type, bedrooms, value, house name, house number, postcode.
3. On error: abends with `'LGSQ'`.

### INSERT-MOTOR (lines 435-476)

1. Converts value, CC, premium, and accidents to integer format.
2. Inserts into MOTOR table with make, model, value, registration number, colour, CC, year, premium, accidents.
3. On error: abends with `'LGSQ'`.

### INSERT-COMMERCIAL (lines 481-550)

1. Converts all numeric peril/premium/status fields to integer format.
2. Inserts into COMMERCIAL table with 20 columns covering address, geolocation, customer, property type, four peril/premium pairs, status, and rejection reason.
3. On error: abends with `'LGSQ'`.

### WRITE-ERROR-MESSAGE (lines 556-566)

Standard error logging: saves SQLCODE, formats time, writes to TSQ.

### Control Flow

```
MAINLINE -> validate commarea
         -> EVALUATE request type -> set policy type
         -> INSERT-POLICY (POLICY table)
         -> INSERT-ENDOW/HOUSE/MOTOR/COMMERCIAL (type table)
         -> LINK LGAPVS01 (VSAM write)
         -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-HEADER` (line 25) | Group | Debug eyecatcher `'LGAPDB01------WS'`, EIB fields |
| `ERROR-MSG` (line 41) | Group | Error message with customer/policy numbers, SQL request, SQLCODE |
| `WS-COMMAREA-LENGTHS` (line 59) | Group | Header length (28 for policy requests) and computed required length |
| `WS-VARY-FIELD` (line 65) | Group (49-level) | DB2 VARCHAR structure: length + char data (3900 bytes max) for endowment padding |
| `DB2-IN-INTEGERS` (line 84) | Group | 24 host variables for integer conversion of commarea fields for all policy types |
| `DB2-OUT-INTEGERS` (line 111) | Group | `DB2-POLICYNUM-INT` -- assigned policy number from identity column |
| `DB2-POLICYTYPE` | `PIC X` | From LGPOLICY copybook -- policy type character (E/H/M/C) |
| `LGAPVS01` (line 114) | `PIC X(8)` | Constant for VSAM writer program name |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 129): Defined via `EXEC SQL INCLUDE LGCMAREA`. The full commarea with all policy type overlays (endowment, house, motor, commercial, claim).

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGERR` (line 38) | `COPY LGERR` | Common error handling working storage |
| `LGPOLICY` (line 71) | `EXEC SQL INCLUDE LGPOLICY` | Policy lengths, DB2 table structures |
| `SQLCA` (line 120) | `EXEC SQL INCLUDE SQLCA` | DB2 SQL communications area |
| `LGCMAREA` (line 131) | `EXEC SQL INCLUDE LGCMAREA` | Commarea definitions |
| `LGERRPRC` (line 571) | `COPY LGERRPRC` | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Determines policy type (`'01AEND'`, `'01AHOU'`, `'01AMOT'`, `'01ACOM'`).
- `CA-CUSTOMER-NUM`: Customer to attach policy to.
- `CA-ISSUE-DATE`, `CA-EXPIRY-DATE`, `CA-BROKERID`, `CA-BROKERSREF`, `CA-PAYMENT`: Common policy fields.
- Type-specific fields: endowment (with-profits, equities, fund, term, sum assured, life assured, padding), house (property type, bedrooms, value, address), motor (make, model, value, reg, colour, CC, year, premium, accidents), commercial (address, geo, perils, premiums, status).

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'00'` success, `'70'` referential integrity error, `'80'` VSAM error (from LGAPVS01), `'90'` SQL error, `'98'` short commarea, `'99'` unknown request.
- `CA-POLICY-NUM`: Set to the DB2-assigned policy number.
- `CA-LASTCHANGED`: Set to the DB2-assigned timestamp.

### Files Accessed
- **DB2 POLICY table** -- INSERT (line 263), SELECT (line 312)
- **DB2 ENDOWMENT table** -- INSERT (line 342 or 364)
- **DB2 HOUSE table** -- INSERT (line 404)
- **DB2 MOTOR table** -- INSERT (line 444)
- **DB2 COMMERCIAL table** -- INSERT (line 494)
- **VSAM KSDSPOLY** -- via LINK to LGAPVS01 (WRITE)

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGAPVS01` (line 238) | Write policy record to VSAM KSDSPOLY file |
| `LGSTSQ` (via LGERRPRC) | Error logging to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

**INSERT POLICY (lines 263-283):**
```sql
INSERT INTO POLICY
  ( POLICYNUMBER, CUSTOMERNUMBER, ISSUEDATE, EXPIRYDATE,
    POLICYTYPE, LASTCHANGED, BROKERID, BROKERSREFERENCE, PAYMENT )
VALUES ( DEFAULT, :DB2-CUSTOMERNUM-INT, :CA-ISSUE-DATE, :CA-EXPIRY-DATE,
         :DB2-POLICYTYPE, CURRENT TIMESTAMP, :DB2-BROKERID-INT,
         :CA-BROKERSREF, :DB2-PAYMENT-INT )
```

**GET IDENTITY (lines 303-305):**
```sql
SET :DB2-POLICYNUM-INT = IDENTITY_VAL_LOCAL()
```

**SELECT TIMESTAMP (lines 311-316):**
```sql
SELECT LASTCHANGED
  INTO :CA-LASTCHANGED
  FROM POLICY
  WHERE POLICYNUMBER = :DB2-POLICYNUM-INT
```

**INSERT ENDOWMENT with VARCHAR (lines 341-361):**
```sql
INSERT INTO ENDOWMENT
  ( POLICYNUMBER, WITHPROFITS, EQUITIES, MANAGEDFUND, FUNDNAME,
    TERM, SUMASSURED, LIFEASSURED, PADDINGDATA )
VALUES ( :DB2-POLICYNUM-INT, :CA-E-WITH-PROFITS, :CA-E-EQUITIES,
         :CA-E-MANAGED-FUND, :CA-E-FUND-NAME, :DB2-E-TERM-SINT,
         :DB2-E-SUMASSURED-INT, :CA-E-LIFE-ASSURED, :WS-VARY-FIELD )
```

**INSERT HOUSE (lines 404-420):**
```sql
INSERT INTO HOUSE
  ( POLICYNUMBER, PROPERTYTYPE, BEDROOMS, VALUE,
    HOUSENAME, HOUSENUMBER, POSTCODE )
VALUES ( :DB2-POLICYNUM-INT, :CA-H-PROPERTY-TYPE, :DB2-H-BEDROOMS-SINT,
         :DB2-H-VALUE-INT, :CA-H-HOUSE-NAME, :CA-H-HOUSE-NUMBER,
         :CA-H-POSTCODE )
```

**INSERT MOTOR (lines 444-465):**
```sql
INSERT INTO MOTOR
  ( POLICYNUMBER, MAKE, MODEL, VALUE, REGNUMBER, COLOUR,
    CC, YEAROFMANUFACTURE, PREMIUM, ACCIDENTS )
VALUES ( :DB2-POLICYNUM-INT, :CA-M-MAKE, :CA-M-MODEL,
         :DB2-M-VALUE-INT, :CA-M-REGNUMBER, :CA-M-COLOUR,
         :DB2-M-CC-SINT, :CA-M-MANUFACTURED, :DB2-M-PREMIUM-INT,
         :DB2-M-ACCIDENTS-INT )
```

**INSERT COMMERCIAL (lines 494-539):**
```sql
INSERT INTO COMMERCIAL
  ( PolicyNumber, RequestDate, StartDate, RenewalDate, Address, Zipcode,
    LatitudeN, LongitudeW, Customer, PropertyType,
    FirePeril, FirePremium, CrimePeril, CrimePremium,
    FloodPeril, FloodPremium, WeatherPeril, WeatherPremium,
    Status, RejectionReason )
VALUES ( :DB2-POLICYNUM-INT, :CA-LASTCHANGED, :CA-ISSUE-DATE,
         :CA-EXPIRY-DATE, :CA-B-Address, :CA-B-Postcode,
         :CA-B-Latitude, :CA-B-Longitude, :CA-B-Customer,
         :CA-B-PropType, :DB2-B-FirePeril-Int, :DB2-B-FirePremium-Int,
         :DB2-B-CrimePeril-Int, :DB2-B-CrimePremium-Int,
         :DB2-B-FloodPeril-Int, :DB2-B-FloodPremium-Int,
         :DB2-B-WeatherPeril-Int, :DB2-B-WeatherPremium-Int,
         :DB2-B-Status-Int, :CA-B-RejectReason )
```

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` -- Missing commarea
- `EXEC CICS ABEND ABCODE('LGSQ') NODUMP` -- SQL error on type-specific insert (triggers UOW backout to undo POLICY insert)
- `EXEC CICS RETURN` -- Return to caller
- `EXEC CICS LINK Program(LGAPVS01)` -- VSAM policy write

### Error Handling
- **SQLCODE -530** on POLICY insert: Referential integrity violation (customer does not exist), returns `'70'`.
- **Other SQLCODE errors** on POLICY insert: Returns `'90'`.
- **Type-specific insert errors:** Abend with `'LGSQ'` to cause CICS to back out the POLICY insert that already succeeded, maintaining data consistency.
- All errors write to TSQ before returning or abending.

### Notable Logic Patterns
- **EVALUATE for request routing:** Used twice -- once for commarea length validation (lines 179-202) and once for executing the correct type-specific insert (lines 218-236).
- **VARCHAR handling for endowment** (lines 334-382): Calculates the actual data length for the PADDINGDATA VARCHAR field by subtracting the required length from the actual commarea length. This allows variable-length data to be stored efficiently.
- **Two-phase insert with rollback:** The POLICY insert is done first, then the type-specific insert. If the second fails, ABEND causes backout of both inserts.

## 6. Analysis

### Overall Role
LGAPDB01 is the central DB2 data-access program for policy creation. It is the most complex program in this set, handling four different policy types with their own table structures. It sits between the business-logic layer (LGAPOL01) and the DB2 database.

### Dependencies
- DB2 tables: POLICY, ENDOWMENT, HOUSE, MOTOR, COMMERCIAL
- DB2 identity column on POLICY.POLICYNUMBER
- Programs: LGAPVS01, LGSTSQ
- Copybooks: LGERR, LGERRPRC, LGPOLICY, LGCMAREA, SQLCA
- The CUSTOMER table must have the referenced customer (enforced by DB2 foreign key, SQLCODE -530)

### Known Issues or Limitations
1. **LGAPVS01 called even on type-specific insert error path:** The LINK to LGAPVS01 at line 238 is outside the EVALUATE, so it executes regardless of the EVALUATE's `When Other` path setting `'99'`. However, the type-specific insert abends prevent this from being reached on SQL error.
2. **No SYNCPOINT:** Relies on CICS ABEND for rollback rather than explicit SYNCPOINT ROLLBACK, which is a more aggressive approach.
3. **DB2-C-Policynum-Int and claim-related fields** (lines 106-109): Declared but not used in this add-policy program; likely shared copybook overhead.
4. **Hardcoded commarea length** 32500 passed to LGAPVS01 (line 240).

### Relationship to Other Programs
- **Called by:** LGAPOL01 (business logic layer)
- **Calls:** LGAPVS01 (VSAM write), LGSTSQ (error logging)
- **Peer:** LGACDB01 (same pattern for customer DB2 access)
- **Related:** LGUPDB01 (update policy), LGDPDB01 (delete policy), LGIPDB01 (inquire policy)
