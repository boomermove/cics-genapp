# LGACDB01.cbl - Add Customer Details (DB2)

## 1. Overview

- **Program ID:** LGACDB01
- **Purpose:** Add a new customer record to the DB2 CUSTOMER table, write the customer to the VSAM KSDSCUST file, and create an initial security record in the CUSTOMER_SECURE table.
- **Summary:** LGACDB01 is the primary DB2 data-access program responsible for inserting a new customer into the system. It obtains a unique customer number from a CICS named counter (GENACUSTNUM), inserts the customer's personal details (name, address, date of birth, phone numbers, email) into the DB2 CUSTOMER table, links to LGACVS01 to write a VSAM copy of the customer record, and links to LGACDB02 to create a default password entry in the CUSTOMER_SECURE table. It is typically called by the business-logic program LGACUS01.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 124-191)

1. **Initialization (lines 130-134):** Initializes `WS-HEADER` and captures CICS EIB fields (`EIBTRNID`, `EIBTRMID`, `EIBTASKN`) into working storage for debugging.
2. **Initialize DB2 host variables (line 139):** Zeroes out `DB2-OUT-INTEGERS`.
3. **Commarea validation (lines 145-164):**
   - If `EIBCALEN` is zero, writes an error message and abends with code `'LGCA'`.
   - Sets `CA-RETURN-CODE` to `'00'` and stores commarea length and address.
   - Computes the required commarea length (`WS-CA-HEADER-LEN` + `WS-CUSTOMER-LEN` = 18 + 72 = 90 bytes). If `EIBCALEN` is less, returns `'98'`.
4. **Core processing (lines 167-185):**
   - `PERFORM Obtain-CUSTOMER-Number` -- gets the next customer number from the named counter.
   - `PERFORM INSERT-CUSTOMER` -- inserts the customer row into DB2.
   - `EXEC CICS LINK Program(LGACVS01)` -- writes the customer to the VSAM KSDSCUST file.
   - Sets up `CDB2AREA` with the customer number, a hardcoded default password (`5732fec825535eeafb8fac50fee3a8aa`), initial count `'0000'`, and state `'N'`, then links to LGACDB02 to insert the security record.
5. **Return (line 188):** Returns to the calling program.

### Obtain-CUSTOMER-Number (lines 195-207)

Uses `EXEC CICS GET COUNTER(GENAcount) POOL(GENApool)` to obtain the next customer number. If the counter is unavailable (`WS-RESP` not `DFHRESP(NORMAL)`), sets `LGAC-NCS` to `'NO'` and initializes the integer to zero (fallback to DB2 identity column). Otherwise stores the counter value in `DB2-CUSTOMERNUM-INT`.

### INSERT-CUSTOMER (lines 211-283)

Two paths based on the flag `LGAC-NCS`:
- **`LGAC-NCS = 'ON'` (lines 218-245):** Inserts with the explicit customer number from the named counter.
- **`LGAC-NCS = 'NO'` (lines 247-279):** Inserts with `DEFAULT` for the customer number column, letting DB2 assign an identity value, then retrieves it via `IDENTITY_VAL_LOCAL()`.

Both paths check `SQLCODE`; on failure set `CA-RETURN-CODE` to `'90'`, write an error, and return. On success, copies `DB2-CUSTOMERNUM-INT` to `CA-CUSTOMER-NUM`.

### WRITE-ERROR-MESSAGE (lines 290-300)

Moves `SQLCODE` to `EM-SQLRC`, calls `LGERR-FORMAT-TIME`, `LGERR-WRITE-MSG`, and `LGERR-LOG-COMMAREA` to log the error to a temporary storage queue.

### Control Flow

```
MAINLINE -> Obtain-CUSTOMER-Number -> INSERT-CUSTOMER
         -> LINK LGACVS01
         -> LINK LGACDB02
         -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-HEADER` (line 25) | Group | Debug eyecatcher, transaction/terminal/task IDs, commarea pointer and length |
| `WS-RESP` (line 36) | `PIC S9(8) COMP` | CICS response code holder |
| `LastCustNum` (line 37) | `PIC S9(8) COMP` | Value returned by CICS named counter |
| `GENAcount` (line 38) | `PIC X(16)` | Named counter name `'GENACUSTNUM'` |
| `GENApool` (line 39) | `PIC X(8)` | Named counter pool name `'GENA'` |
| `LGAC-NCS` (line 71) | `PIC X(2)` | Flag: `'ON'` means use named counter, `'NO'` means use DB2 identity |
| `CDB2AREA` (line 59) | Group | Commarea structure passed to LGACDB02 for security record creation |
| `DB2-CUSTOMERNUM-INT` (line 97) | `PIC S9(9) COMP` | DB2 host variable for customer number |
| `ERROR-MSG` (line 47) | Group | Formatted error message for TSQ logging |
| `WS-COMMAREA-LENGTHS` (line 78) | Group | Header length (18) and computed required length |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 113): Defined via `EXEC SQL INCLUDE LGCMAREA`. Contains `CA-REQUEST-ID`, `CA-RETURN-CODE`, `CA-CUSTOMER-NUM`, and the full customer/policy request structure (32,500 bytes).

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGERR` (line 44) | `COPY LGERR` | Common error handling working storage (time/date fields, error log queue name) |
| `LGPOLICY` (line 84) | `COPY LGPOLICY` | Policy length definitions (`WS-CUSTOMER-LEN`, etc.) and DB2 table structures |
| `SQLCA` (line 105) | `EXEC SQL INCLUDE SQLCA` | DB2 SQL communications area |
| `LGCMAREA` (line 115) | `EXEC SQL INCLUDE LGCMAREA` | Commarea field definitions in LINKAGE |
| `LGERRPRC` (line 305) | `COPY LGERRPRC` | Common error handling procedures (`LGERR-FORMAT-TIME`, `LGERR-WRITE-MSG`, `LGERR-LOG-COMMAREA`) |

### Condition Names (88-levels)

None explicitly defined. The program uses direct comparisons against `DFHRESP(NORMAL)`, `SQLCODE`, and the `LGAC-NCS` flag.

## 4. Input/Output Behavior

### Input (COMMAREA)
- `CA-REQUEST-ID`: Not explicitly checked in this program (checked by caller LGACUS01).
- `CA-FIRST-NAME`, `CA-LAST-NAME`, `CA-DOB`, `CA-HOUSE-NAME`, `CA-HOUSE-NUM`, `CA-POSTCODE`, `CA-PHONE-MOBILE`, `CA-PHONE-HOME`, `CA-EMAIL-ADDRESS`: Customer personal details to insert.

### Output (COMMAREA)
- `CA-RETURN-CODE`: `'00'` on success, `'98'` for short commarea, `'90'` for SQL error.
- `CA-CUSTOMER-NUM`: Set to the newly assigned customer number on successful insert.

### Files Accessed
- **DB2 CUSTOMER table** -- INSERT (line 219 or 248)
- **VSAM KSDSCUST** -- via LINK to LGACVS01 (WRITE)

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGACVS01` (line 170) | Write customer record to VSAM KSDSCUST file |
| `LGACDB02` (line 182) | Insert default password into CUSTOMER_SECURE table |
| `LGSTSQ` (via LGERRPRC) | Write error messages to temporary storage queue |

## 5. Algorithms and Techniques

### SQL Statements

**INSERT with explicit customer number (lines 218-240):**
```sql
INSERT INTO CUSTOMER
  ( CUSTOMERNUMBER, FIRSTNAME, LASTNAME, DATEOFBIRTH,
    HOUSENAME, HOUSENUMBER, POSTCODE,
    PHONEMOBILE, PHONEHOME, EMAILADDRESS )
VALUES ( :DB2-CUSTOMERNUM-INT,
         :CA-FIRST-NAME, :CA-LAST-NAME, :CA-DOB,
         :CA-HOUSE-NAME, :CA-HOUSE-NUM, :CA-POSTCODE,
         :CA-PHONE-MOBILE, :CA-PHONE-HOME, :CA-EMAIL-ADDRESS )
```

**INSERT with DEFAULT (identity column) (lines 247-268):**
```sql
INSERT INTO CUSTOMER
  ( CUSTOMERNUMBER, FIRSTNAME, LASTNAME, DATEOFBIRTH,
    HOUSENAME, HOUSENUMBER, POSTCODE,
    PHONEMOBILE, PHONEHOME, EMAILADDRESS )
VALUES ( DEFAULT,
         :CA-FIRST-NAME, :CA-LAST-NAME, :CA-DOB,
         :CA-HOUSE-NAME, :CA-HOUSE-NUM, :CA-POSTCODE,
         :CA-PHONE-MOBILE, :CA-PHONE-HOME, :CA-EMAIL-ADDRESS )
```

**Retrieve identity value (lines 276-278):**
```sql
SET :DB2-CUSTOMERNUM-INT = IDENTITY_VAL_LOCAL()
```

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` -- Abend if no commarea
- `EXEC CICS RETURN` -- Return control to caller
- `EXEC CICS GET COUNTER(GENAcount) POOL(GENApool)` -- Obtain next customer number
- `EXEC CICS LINK Program(LGACVS01)` -- Call VSAM writer
- `EXEC CICS LINK Program(LGACDB02)` -- Call security record creator

### Error Handling
- SQLCODE checked after each INSERT; non-zero sets return code `'90'` and writes error to TSQ via `LGSTSQ`.
- Named counter failure degrades gracefully to DB2 identity column mode.
- Missing/short commarea detected with specific return codes (`'98'`).

### Notable Logic Patterns
- **Dual-path insert strategy:** The `LGAC-NCS` flag creates two code paths -- one using an externally managed counter for customer numbers, the other using DB2's identity column. This provides resilience if the named counter is unavailable.

## 6. Analysis

### Overall Role
LGACDB01 is the DB2 data-access layer for customer creation. It sits between the business-logic layer (LGACUS01) and the database, and orchestrates writes to both DB2 and VSAM for data consistency.

### Dependencies
- CICS named counter pool `GENA` with counter `GENACUSTNUM`
- DB2 CUSTOMER table with identity column on CUSTOMERNUMBER
- Programs: LGACVS01, LGACDB02, LGSTSQ
- Copybooks: LGERR, LGERRPRC, LGPOLICY, LGCMAREA, SQLCA

### Known Issues or Limitations
1. **Hardcoded default password** (line 177): The password `'5732fec825535eeafb8fac50fee3a8aa'` is hardcoded in the source, which is a security concern.
2. **No transactional coordination:** The DB2 insert, VSAM write, and security insert are not wrapped in a single unit of work with explicit syncpoint. If LGACVS01 or LGACDB02 fails, the DB2 CUSTOMER row is already committed.
3. **Magic numbers:** The commarea length `225` passed to LGACVS01 (line 172) and `32500` to LGACDB02 (line 184) are hardcoded.

### Relationship to Other Programs
- **Called by:** LGACUS01 (business logic layer)
- **Calls:** LGACVS01 (VSAM write), LGACDB02 (security record insert), LGSTSQ (error logging)
- **Replaced by:** LGACDB01_SECURE (secure version with proper authentication instead of hardcoded passwords)
