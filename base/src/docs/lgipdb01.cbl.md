# LGIPDB01 - Inquire Policy (DB2 Data Access)

## 1. Overview

- **Program ID:** LGIPDB01
- **Purpose:** Retrieve full details of an individual insurance policy from DB2, supporting Endowment, House, Motor, and Commercial policy types.
- **Summary:** LGIPDB01 is the DB2 data-access program for policy inquiries. It receives a COMMAREA containing customer number, policy number, and a request ID that identifies the policy type. Based on the request ID, it executes the appropriate SQL SELECT (or cursor-based fetch) against the POLICY table joined with the relevant policy-type table (ENDOWMENT, HOUSE, MOTOR, or COMMERCIAL). It supports seven different query modes identified by request IDs: `01IEND`, `01IHOU`, `01IMOT`, `01ICOM`, `02ICOM`, `03ICOM`, and `05ICOM`. The program handles nullable DB2 columns using indicator variables, manages VARCHAR data for endowment padding, and marks the end of policy data with a `'FINAL'` sentinel.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 225-311)

1. **Initialization** (lines 231-240): Initializes `WS-HEADER`, captures CICS context, and initializes all DB2 host variable groups (`DB2-IN-INTEGERS`, `DB2-OUT-INTEGERS`, `DB2-POLICY`).
2. **COMMAREA validation** (lines 246-250): Abends with code `'LGCA'` if no COMMAREA received.
3. **Setup** (lines 253-262): Sets return code to `'00'`, saves COMMAREA length and address, converts customer and policy numbers to DB2 integer format, and saves them in error message fields.
4. **Request routing** (lines 270-305): Converts `CA-REQUEST-ID` to upper case and uses an EVALUATE statement to route to the appropriate paragraph:
   - `'01IEND'` -> `GET-ENDOW-DB2-INFO`
   - `'01IHOU'` -> `GET-HOUSE-DB2-INFO`
   - `'01IMOT'` -> `GET-MOTOR-DB2-INFO`
   - `'01ICOM'` -> `GET-COMMERCIAL-DB2-INFO-1`
   - `'02ICOM'` -> `GET-COMMERCIAL-DB2-INFO-2`
   - `'03ICOM'` -> `GET-COMMERCIAL-DB2-INFO-3`
   - `'05ICOM'` -> `GET-COMMERCIAL-DB2-INFO-5`
   - Other -> return code `'99'`

### End-Program (line 308)

Issues `EXEC CICS RETURN`.

### GET-ENDOW-DB2-INFO (lines 322-427)

Executes a SELECT joining POLICY and ENDOWMENT tables on policy number, filtering by customer number and policy number. Handles nullable columns (BROKERID, BROKERSREFERENCE, PAYMENT, PADDINGDATA) via indicator variables. Calculates required COMMAREA length including variable-length VARCHAR padding data. Copies results to COMMAREA and marks end with `'FINAL'`.

### GET-HOUSE-DB2-INFO (lines 436-518)

Executes a SELECT joining POLICY and HOUSE tables. Retrieves property type, bedrooms, value, house name/number, and postcode. Handles nullable broker/payment fields. Copies results to COMMAREA.

### GET-MOTOR-DB2-INFO (lines 523-616)

Executes a SELECT joining POLICY and MOTOR tables. Retrieves make, model, value, registration number, colour, CC, year of manufacture, premium, and accidents. Handles nullable fields. Copies results to COMMAREA.

### GET-COMMERCIAL-DB2-INFO-1 (lines 622-719)

Single-row SELECT joining POLICY and COMMERCIAL tables by customer number and policy number. Retrieves address, zipcode, latitude, longitude, customer name, property type, peril/premium fields, status, and rejection reason. Converts integer peril/premium fields to display format.

### GET-COMMERCIAL-DB2-INFO-2 (lines 725-824)

Similar to INFO-1 but queries by policy number only (no customer number filter). Also retrieves and returns the CustomerNumber. Used when the caller knows only the policy number.

### GET-COMMERCIAL-DB2-INFO-3 (lines 830-908)

Uses `Cust_Cursor` (declared at lines 85-113) to fetch multiple commercial policy rows for a given customer. Opens cursor, fetches rows in a loop (up to 20 records), writes each to the COMMAREA, then closes the cursor. The record count limit of 20 is enforced by setting SQLCODE to 17 when exceeded (line 903).

### GET-COMMERCIAL-DB2-INFO-3-Cur (lines 858-908)

The cursor fetch paragraph called in a PERFORM loop. Fetches one row from `Cust_Cursor`, converts integer fields, and populates the COMMAREA.

### GET-COMMERCIAL-DB2-INFO-5 (lines 914-985)

Uses `Zip_Cursor` (declared at lines 116-144) to fetch commercial policy rows matching a given zipcode (`CA-B-POSTCODE`). Opens cursor, fetches rows in a loop, and closes cursor. No explicit record count limit.

### GET-COMMERCIAL-DB2-INFO-5-Cur (lines 941-985)

Cursor fetch paragraph for `Zip_Cursor`. Fetches one row and populates the COMMAREA.

### WRITE-ERROR-MESSAGE (lines 991-1001)

Logs SQLCODE, formats time/date, writes error and COMMAREA to TSQ.

### Control Flow

```
MAINLINE -> validate COMMAREA -> EVALUATE CA-REQUEST-ID
   |-> GET-ENDOW-DB2-INFO   (01IEND)
   |-> GET-HOUSE-DB2-INFO   (01IHOU)
   |-> GET-MOTOR-DB2-INFO   (01IMOT)
   |-> GET-COMMERCIAL-DB2-INFO-1 (01ICOM)
   |-> GET-COMMERCIAL-DB2-INFO-2 (02ICOM)
   |-> GET-COMMERCIAL-DB2-INFO-3 (03ICOM) -> loop GET-COMMERCIAL-DB2-INFO-3-Cur
   |-> GET-COMMERCIAL-DB2-INFO-5 (05ICOM) -> loop GET-COMMERCIAL-DB2-INFO-5-Cur
   -> End-Program (RETURN)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-EYECATCHER` | X(16) | `'LGIPDB01------WS'` for dump identification |
| `WS-TRANSID`, `WS-TERMID`, `WS-TASKNUM` | various | CICS runtime context |
| `WS-ADDR-DFHCOMMAREA` | POINTER | COMMAREA address |
| `WS-CALEN` | S9(4) COMP | Saved EIBCALEN |
| `MINUS-ONE` | S9(4) COMP | Constant -1 for null indicator checks |
| `WS-CA-HEADERTRAILER-LEN` | S9(4) COMP | Fixed overhead = 33 bytes |
| `WS-REQUIRED-CA-LEN` | S9(4) | Calculated minimum COMMAREA length |
| `END-POLICY-POS` | S9(4) COMP | Position for `'FINAL'` marker after policy data |
| `ICOM-Record-Count` | S9(4) COMP | Counter for cursor-based commercial queries (max 20) |
| `WS-Request-ID` | X(6) | Upper-cased request ID |
| `DB2-CUSTOMERNUM-INT` | S9(9) COMP | Customer number in DB2 integer format |
| `DB2-POLICYNUM-INT` | S9(9) COMP | Policy number in DB2 integer format |
| `DB2-OUT-INTEGERS` | group | Output integer fields for broker, payment, term, value, CC, premium, accidents, perils |
| `IND-BROKERID`, `IND-BROKERSREF`, `IND-PAYMENT`, `IND-E-PADDINGDATA`, `IND-E-PADDINGDATAL` | S9(4) COMP | SQL null indicator variables |
| `TSAREA` | group | Area with postcode, customer number, policy number (line 21-27) |

### LINKAGE SECTION

- `DFHCOMMAREA`: Via `EXEC SQL INCLUDE LGCMAREA` -- defines all customer, policy, and policy-type sub-structures.
- `ICOM-Record`: PIC X(1202) -- defined but not used in the current code (line 217).

### Copybooks Included

| Copybook | Method | Purpose |
|---|---|---|
| `LGERR` | `COPY LGERR` (line 45) | Common error handling working-storage |
| `SQLCA` | `EXEC SQL INCLUDE SQLCA` (line 148) | DB2 SQL Communications Area |
| `LGPOLICY` | `EXEC SQL INCLUDE LGPOLICY` (line 192) | DB2 structures and length constants |
| `LGCMAREA` | `EXEC SQL INCLUDE LGCMAREA` (line 214) | COMMAREA structure |
| `LGERRPRC` | `COPY LGERRPRC` (line 1006) | Common error handling procedures |

### Condition Names (88-levels)

None defined.

### Declared Cursors

- `Cust_Cursor` (lines 85-113): Insensitive Scroll Cursor joining POLICY and COMMERCIAL on PolicyNumber, filtered by CustomerNumber.
- `Zip_Cursor` (lines 116-144): Insensitive Scroll Cursor joining POLICY and COMMERCIAL on PolicyNumber, filtered by Zipcode.

## 4. Input/Output Behavior

### Input

- **DFHCOMMAREA fields consumed:**
  - `CA-REQUEST-ID` (X(6)): Determines which query to execute.
  - `CA-CUSTOMER-NUM` (9(10)): Customer number.
  - `CA-POLICY-NUM` (9(10)): Policy number.
  - `CA-B-POSTCODE` (X(8)): Zipcode (used only for `05ICOM` queries).
  - `EIBCALEN`: COMMAREA length.

### Output

- **DFHCOMMAREA fields populated (varies by query type):**
  - `CA-RETURN-CODE`: `'00'` (success), `'01'` (not found), `'88'` (cursor close error), `'89'` (cursor open error), `'90'` (SQL error), `'98'` (COMMAREA too short), `'99'` (unknown request ID).
  - `CA-POLICY-COMMON`: Issue date, expiry date, last changed timestamp, broker ID, broker ref, payment.
  - Policy-type-specific fields: `CA-ENDOWMENT`, `CA-HOUSE`, `CA-MOTOR`, or `CA-COMMERCIAL`.
  - End-of-data marker: `'FINAL'` written after the last byte of policy data.

### Files/Tables Accessed

| Resource | Type | Operation |
|---|---|---|
| POLICY (DB2 table) | DB2 | SELECT (always joined) |
| ENDOWMENT (DB2 table) | DB2 | SELECT (for `01IEND`) |
| HOUSE (DB2 table) | DB2 | SELECT (for `01IHOU`) |
| MOTOR (DB2 table) | DB2 | SELECT (for `01IMOT`) |
| COMMERCIAL (DB2 table) | DB2 | SELECT (for `01ICOM`, `02ICOM`, `03ICOM`, `05ICOM`) |

### Other Programs Called

| Program | Method | Purpose |
|---|---|---|
| LGSTSQ | `EXEC CICS LINK` (via LGERRPRC) | Error logging to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

This program contains numerous SQL statements. Key examples:

**Endowment SELECT (lines 325-363):**
```sql
SELECT ISSUEDATE, EXPIRYDATE, LASTCHANGED,
       BROKERID, BROKERSREFERENCE, PAYMENT,
       WITHPROFITS, EQUITIES, MANAGEDFUND, FUNDNAME,
       TERM, SUMASSURED, LIFEASSURED,
       PADDINGDATA, LENGTH(PADDINGDATA)
INTO :DB2-ISSUEDATE, :DB2-EXPIRYDATE, :DB2-LASTCHANGED,
     :DB2-BROKERID-INT INDICATOR :IND-BROKERID,
     :DB2-BROKERSREF INDICATOR :IND-BROKERSREF,
     :DB2-PAYMENT-INT INDICATOR :IND-PAYMENT,
     :DB2-E-WITHPROFITS, :DB2-E-EQUITIES, :DB2-E-MANAGEDFUND,
     :DB2-E-FUNDNAME, :DB2-E-TERM-SINT, :DB2-E-SUMASSURED-INT,
     :DB2-E-LIFEASSURED,
     :DB2-E-PADDINGDATA INDICATOR :IND-E-PADDINGDATA,
     :DB2-E-PADDING-LEN INDICATOR :IND-E-PADDINGDATAL
FROM POLICY, ENDOWMENT
WHERE (POLICY.POLICYNUMBER = ENDOWMENT.POLICYNUMBER
   AND POLICY.CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT
   AND POLICY.POLICYNUMBER = :DB2-POLICYNUM-INT)
```

**Commercial cursor declaration (Cust_Cursor, lines 85-113):**
```sql
DECLARE Cust_Cursor INSENSITIVE SCROLL CURSOR FOR
SELECT CustomerNumber, Policy.PolicyNumber,
       RequestDate, StartDate, RenewalDate,
       Address, Zipcode, LatitudeN, LongitudeW,
       Customer, PropertyType,
       FirePeril, FirePremium, CrimePeril, CrimePremium,
       FloodPeril, FloodPremium, WeatherPeril, WeatherPremium,
       Status, RejectionReason
FROM POLICY, COMMERCIAL
WHERE (POLICY.POLICYNUMBER = Commercial.POLICYNUMBER
   AND Policy.CustomerNumber = :DB2-CUSTOMERNUM-INT)
```

**Zip_Cursor declaration (lines 116-144):** Same as Cust_Cursor but filtered by `Commercial.Zipcode = :CA-B-POSTCODE`.

### CICS Commands

| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ABEND ABCODE('LGCA') NODUMP` | 249 | Terminate on missing COMMAREA |
| `EXEC CICS RETURN` | 308, 387, 484, 578, 686, 790 | Return to caller |

### Error Handling Approach

- Missing COMMAREA: ABEND `'LGCA'`.
- COMMAREA too short: Return code `'98'` with immediate return.
- SQLCODE 0: Success (`'00'`).
- SQLCODE 100: Not found (`'01'`).
- Cursor open failure: Return code `'89'` with error logging and return.
- Cursor close failure: Return code `'88'` with error logging and return.
- Other SQL errors: Return code `'90'` with error logging.
- Unknown request ID: Return code `'99'`.

### Notable Logic Patterns

- **Indicator variables** (lines 198-202): Used for nullable DB2 columns (BROKERID, BROKERSREFERENCE, PAYMENT, PADDINGDATA). Null values are detected by checking indicator against `MINUS-ONE` (-1).
- **VARCHAR handling** (lines 378-381): The endowment PADDINGDATA is a VARCHAR column. Its actual length is retrieved via `LENGTH(PADDINGDATA)` and used to dynamically calculate COMMAREA size and data positioning.
- **FINAL sentinel** (e.g., line 412): `'FINAL'` is written after the last byte of policy data to mark the end, allowing the caller to know where valid data ends.
- **Cursor-based multi-row fetch** (lines 830-908): For request `'03ICOM'`, a cursor is opened and rows are fetched in a PERFORM loop with a hard limit of 20 records (line 902-904), enforced by overwriting SQLCODE with 17.
- **FUNCTION UPPER-CASE** (line 270): Request ID is normalized to upper case before evaluation.

## 6. Analysis

### Overall Role

LGIPDB01 is the most complex data-access program in the GenApp system. It serves as the single DB2 access point for all policy inquiries across four policy types (Endowment, House, Motor, Commercial) and seven query modes. It sits below the business-logic program LGIPOL01 in the application architecture.

### Dependencies

- **DB2 tables:** POLICY, ENDOWMENT, HOUSE, MOTOR, COMMERCIAL must all exist with the expected column structures.
- **LGSTSQ** program for error logging.
- **Copybooks:** LGERR, LGERRPRC, LGPOLICY (via SQL INCLUDE), LGCMAREA (via SQL INCLUDE), SQLCA.
- Must be called with a COMMAREA large enough for the requested policy type.

### Known Issues or Limitations

- `ICOM-Record` (line 217) in the LINKAGE SECTION is defined but never referenced.
- `TSAREA` (lines 21-27) in WORKING-STORAGE is defined but never used.
- The 20-record limit in `GET-COMMERCIAL-DB2-INFO-3` (line 902) is enforced by overwriting SQLCODE with 17, which is a non-standard and potentially confusing technique.
- `GET-COMMERCIAL-DB2-INFO-5` (zipcode cursor) has no record count limit, unlike INFO-3, which could cause problems with large result sets.
- Commercial queries (INFO-3 and INFO-5) overwrite the same COMMAREA fields on each fetch iteration, so only the last row fetched is visible to the caller. This appears to be a design limitation unless the caller is processing the COMMAREA after each LINK return.
- The `WS-CA-HEADERTRAILER-LEN` is 33 in this program (vs. 18 in LGICDB01), reflecting the larger policy COMMAREA header that includes the policy number field.
- Comment on line 723 has a typo: "ploicy" should be "policy".

### Relationship to Other Programs

- **Called by:** LGIPOL01 (policy inquire business logic) via `EXEC CICS LINK`.
- **Calls:** LGSTSQ (error logging).
- **Sibling programs:** LGICDB01 (customer inquire DB2), LGUCDB01 (customer update DB2) follow the same data-access pattern but are simpler.
- **Related:** LGUPDB01 (policy update DB2) would handle the write side for the same tables.
