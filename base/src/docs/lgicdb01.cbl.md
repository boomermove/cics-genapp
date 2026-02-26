# LGICDB01 - Inquire Customer (DB2 Data Access)

## 1. Overview

- **Program ID:** LGICDB01
- **Purpose:** Select customer details from a DB2 CUSTOMER table given a customer number.
- **Summary:** LGICDB01 is the data-access layer program for customer inquiries. It receives a communication area (COMMAREA) containing a customer number, executes a DB2 SELECT statement against the CUSTOMER table, and populates the COMMAREA with the customer's personal details (name, date of birth, address, phone numbers, email). It is not invoked directly by a user transaction but is called via `EXEC CICS LINK` from the business-logic program LGICUS01.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 98-160)

1. **Initialization** (lines 104-108): Initializes `WS-HEADER` and captures CICS runtime information (`EIBTRNID`, `EIBTRMID`, `EIBTASKN`) into working storage for debugging purposes.
2. **COMMAREA validation** (lines 115-119): Checks if `EIBCALEN` is zero. If no COMMAREA was passed, logs an error message and issues `EXEC CICS ABEND ABCODE('LGCA') NODUMP` to terminate the transaction.
3. **Return code initialization** (lines 122-124): Sets `CA-RETURN-CODE` to `'00'`, saves the COMMAREA length, and captures the address of `DFHCOMMAREA`.
4. **DB2 host variable initialization** (line 127): Initializes `DB2-IN-INTEGERS`.
5. **COMMAREA length check** (lines 133-139): Calculates the minimum required length (`WS-CUSTOMER-LEN + WS-CA-HEADERTRAILER-LEN`). If `EIBCALEN` is less than this, sets return code `'98'` and returns to the caller.
6. **Customer number conversion** (lines 142-144): Moves `CA-CUSTOMER-NUM` to `DB2-CUSTOMERNUMBER-INT` (integer format for SQL) and to `EM-CUSNUM` (for error message use).
7. **DB2 query** (line 150): Performs `GET-CUSTOMER-INFO`.
8. **Return** (line 157): Executes `EXEC CICS RETURN`.

### GET-CUSTOMER-INFO (lines 163-201)

1. Executes a `SELECT` statement against the CUSTOMER table using the customer number as the key.
2. Evaluates `SQLCODE`:
   - `0`: Sets return code `'00'` (success).
   - `100`: Sets return code `'01'` (no rows found).
   - `-913`: Sets return code `'01'` (deadlock/timeout, treated as not found).
   - Other: Sets return code `'90'` (error), performs `WRITE-ERROR-MESSAGE`, and returns immediately.

### WRITE-ERROR-MESSAGE (lines 207-217)

1. Moves `SQLCODE` to `EM-SQLRC`.
2. Calls `LGERR-FORMAT-TIME` to get the current date and time.
3. Moves formatted date/time into the error message structure.
4. Calls `LGERR-WRITE-MSG` to write the error message to a TSQ via program LGSTSQ.
5. Calls `LGERR-LOG-COMMAREA` to log the first 90 bytes of the COMMAREA.

### Control Flow

```
MAINLINE -> validate COMMAREA -> check length -> GET-CUSTOMER-INFO -> MAINLINE-END (RETURN)
                                                      |
                                              (on error) -> WRITE-ERROR-MESSAGE -> RETURN
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-EYECATCHER` | X(16) | Storage eyecatcher `'LGICDB01------WS'` for dump analysis |
| `WS-TRANSID` | X(4) | Current CICS transaction ID |
| `WS-TERMID` | X(4) | Current CICS terminal ID |
| `WS-TASKNUM` | 9(7) | Current CICS task number |
| `WS-ADDR-DFHCOMMAREA` | POINTER | Address of the DFHCOMMAREA |
| `WS-CALEN` | S9(4) COMP | Saved copy of `EIBCALEN` |
| `DB2-CUSTOMERNUMBER-INT` | S9(9) COMP | Customer number in DB2 integer format |
| `WS-CA-HEADERTRAILER-LEN` | S9(4) COMP | Fixed header/trailer overhead = 18 bytes |
| `WS-REQUIRED-CA-LEN` | S9(4) | Calculated minimum COMMAREA length |

### ERROR-MSG Structure (lines 40-50)

A formatted error message containing date, time, program name (`LGICDB01`), customer number, SQL request description, and the SQLCODE.

### LINKAGE SECTION

- `DFHCOMMAREA`: Mapped via `EXEC SQL INCLUDE LGCMAREA`, which defines `CA-REQUEST-ID`, `CA-RETURN-CODE`, `CA-CUSTOMER-NUM`, and the customer detail fields (`CA-FIRST-NAME`, `CA-LAST-NAME`, etc.).

### Copybooks Included

| Copybook | Method | Purpose |
|---|---|---|
| `LGERR` | `COPY LGERR` (line 37) | Provides common error handling working-storage fields (`WS-ERR-ABSTIME`, `WS-ERR-DATE`, `WS-ERR-TIME`, `CA-ERROR-MSG`, `WS-ERR-LOG-PGM`) |
| `LGPOLICY` | `COPY LGPOLICY` (line 70) | Provides policy length constants (`WS-CUSTOMER-LEN` = 72) and DB2 data structures |
| `SQLCA` | `EXEC SQL INCLUDE SQLCA` (line 78) | DB2 SQL Communications Area (provides `SQLCODE`) |
| `LGCMAREA` | `EXEC SQL INCLUDE LGCMAREA` (line 89) | COMMAREA structure definition for DFHCOMMAREA |
| `LGERRPRC` | `COPY LGERRPRC` (line 222) | Common error handling procedures (`LGERR-FORMAT-TIME`, `LGERR-WRITE-MSG`, `LGERR-LOG-COMMAREA`) |

### Condition Names (88-levels)

None defined explicitly in this program. Conditions are checked via IF/EVALUATE on `SQLCODE` and `EIBCALEN`.

## 4. Input/Output Behavior

### Input

- **DFHCOMMAREA fields consumed:**
  - `CA-CUSTOMER-NUM` (PIC 9(10)): The customer number to look up.
  - `EIBCALEN`: Length of the COMMAREA passed by the caller.

### Output

- **DFHCOMMAREA fields populated on success (SQLCODE=0):**
  - `CA-RETURN-CODE` = `'00'`
  - `CA-FIRST-NAME` (X(10))
  - `CA-LAST-NAME` (X(20))
  - `CA-DOB` (X(10))
  - `CA-HOUSE-NAME` (X(20))
  - `CA-HOUSE-NUM` (X(4))
  - `CA-POSTCODE` (X(8))
  - `CA-PHONE-MOBILE` (X(20))
  - `CA-PHONE-HOME` (X(20))
  - `CA-EMAIL-ADDRESS` (X(100))

- **Return codes:**
  - `'00'`: Success
  - `'01'`: Customer not found (SQLCODE 100 or -913)
  - `'90'`: SQL error
  - `'98'`: COMMAREA too short

### Files/Tables Accessed

| Resource | Type | Operation |
|---|---|---|
| CUSTOMER (DB2 table) | DB2 | SELECT (read) |

### Other Programs Called

| Program | Method | Purpose |
|---|---|---|
| LGSTSQ | `EXEC CICS LINK` (via LGERRPRC) | Write error messages to a temporary storage queue |

## 5. Algorithms and Techniques

### SQL Statements

**SELECT (lines 165-186):**
```sql
SELECT FIRSTNAME, LASTNAME, DATEOFBIRTH,
       HOUSENAME, HOUSENUMBER, POSTCODE,
       PHONEMOBILE, PHONEHOME, EMAILADDRESS
INTO  :CA-FIRST-NAME, :CA-LAST-NAME, :CA-DOB,
      :CA-HOUSE-NAME, :CA-HOUSE-NUM, :CA-POSTCODE,
      :CA-PHONE-MOBILE, :CA-PHONE-HOME, :CA-EMAIL-ADDRESS
FROM CUSTOMER
WHERE CUSTOMERNUMBER = :DB2-CUSTOMERNUMBER-INT
```

### CICS Commands

| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ABEND ABCODE('LGCA') NODUMP` | 118 | Terminate transaction when no COMMAREA received |
| `EXEC CICS RETURN` | 138, 157, 198 | Return control to caller |
| `EXEC CICS ASKTIME` | (via LGERRPRC) | Get current absolute time for error logging |
| `EXEC CICS FORMATTIME` | (via LGERRPRC) | Format time and date for error logging |
| `EXEC CICS LINK PROGRAM(WS-ERR-LOG-PGM)` | (via LGERRPRC) | Call LGSTSQ to write error/commarea to TSQ |

### Error Handling Approach

- Missing COMMAREA causes an immediate ABEND with code `'LGCA'`.
- Insufficient COMMAREA length returns code `'98'` without processing.
- SQLCODE is evaluated with an EVALUATE block. SQLCODE -913 (deadlock/timeout) is treated the same as "not found" (return code `'01'`).
- On unexpected SQL errors (return code `'90'`), the error is logged to a TSQ and the program returns immediately.

### Notable Logic Patterns

- The SELECT populates host variables that map directly into the COMMAREA structure, so data flows straight from DB2 into the caller's COMMAREA with no intermediate transformation.
- The EVALUATE structure on SQLCODE (lines 188-199) provides a clean three-way branch: success, not-found, and error.

## 6. Analysis

### Overall Role

LGICDB01 is the DB2 data-access module in a two-tier architecture for customer inquiries. It sits below the business-logic layer (LGICUS01) and handles all direct database interaction. This separation follows a common CICS application pattern where business logic and data access are kept in separate programs.

### Dependencies

- **DB2 table CUSTOMER** must exist with columns: CUSTOMERNUMBER, FIRSTNAME, LASTNAME, DATEOFBIRTH, HOUSENAME, HOUSENUMBER, POSTCODE, PHONEMOBILE, PHONEHOME, EMAILADDRESS.
- **LGSTSQ** program must be available for error logging.
- **Copybooks:** LGERR, LGERRPRC, LGPOLICY, LGCMAREA, SQLCA.
- Must be called via `EXEC CICS LINK` with a COMMAREA of at least 90 bytes (18 header + 72 customer data).

### Known Issues or Limitations

- SQLCODE -913 (deadlock/timeout) is silently treated as "not found" (return code `'01'`) rather than triggering a retry or returning a distinct error code. This could mask concurrency problems.
- The comment on line 21 has a typo: "defintions" should be "definitions"; line 23 has "infomation" instead of "information".
- No explicit DB2 COMMIT or ROLLBACK is issued; this relies on CICS syncpoint handling.

### Relationship to Other Programs

- **Called by:** LGICUS01 (customer inquire business logic) via `EXEC CICS LINK`.
- **Calls:** LGSTSQ (error logging).
- **Sibling programs:** LGIPDB01 (policy inquire DB2), LGUCDB01 (customer update DB2) follow the same pattern.
