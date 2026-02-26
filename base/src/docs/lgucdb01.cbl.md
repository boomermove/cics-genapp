# LGUCDB01 - Update Customer (DB2 Data Access)

## 1. Overview

- **Program ID:** LGUCDB01
- **Purpose:** Update customer details in the DB2 CUSTOMER table.
- **Summary:** LGUCDB01 is the data-access layer program for customer updates. It receives a COMMAREA containing the customer number and updated customer details, executes a DB2 UPDATE statement against the CUSTOMER table, and then calls LGUCVS01 via `EXEC CICS LINK` to synchronize the VSAM customer file. This program is called by the business-logic program LGUCUS01.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 97-142)

1. **Initialization** (lines 103-108): Initializes `WS-HEADER`, captures CICS runtime context, and initializes `WS-RETRY` to spaces.
2. **COMMAREA validation** (lines 113-117): If `EIBCALEN` equals zero, logs an error and issues `EXEC CICS ABEND ABCODE('LGCA') NODUMP`.
3. **Setup** (lines 120-127): Sets `CA-RETURN-CODE` to `'00'`, saves COMMAREA length and address, converts `CA-CUSTOMER-NUM` to DB2 integer format, and saves the customer number in the error message field.
4. **Update execution** (line 130): Performs `UPDATE-CUSTOMER-INFO`.
5. **VSAM synchronization** (lines 132-135): Executes `EXEC CICS LINK Program(LGUCVS01) Commarea(DFHCOMMAREA) LENGTH(225)` to update the VSAM copy of the customer data.

### END-PROGRAM (line 139)

Issues `EXEC CICS RETURN`.

### UPDATE-CUSTOMER-INFO (lines 148-177)

1. Sets `EM-SQLREQ` to `' UPDATE CUST  '` for error message context.
2. Executes the DB2 UPDATE statement to set all customer fields.
3. Evaluates SQLCODE:
   - `0`: Success (no action needed, return code remains `'00'`).
   - `100`: No matching row found, sets return code `'01'`.
   - Other: Sets return code `'90'` and performs `WRITE-ERROR-MESSAGE`.

### WRITE-ERROR-MESSAGE (lines 184-194)

Saves SQLCODE, formats time/date, writes error message and COMMAREA to TSQ.

### Control Flow

```
MAINLINE -> validate COMMAREA -> UPDATE-CUSTOMER-INFO (DB2 UPDATE)
         -> LINK to LGUCVS01 (VSAM sync) -> END-PROGRAM (RETURN)
```

Note: The VSAM LINK is executed regardless of whether the DB2 UPDATE succeeded. If the DB2 UPDATE failed with return code `'01'` or `'90'`, the VSAM update is still attempted.

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC | Role |
|---|---|---|
| `WS-EYECATCHER` | X(16) | `'LGUCDB01------WS'` for dump identification |
| `WS-TRANSID` | X(4) | CICS transaction ID |
| `WS-TERMID` | X(4) | CICS terminal ID |
| `WS-TASKNUM` | 9(7) | CICS task number |
| `WS-ADDR-DFHCOMMAREA` | POINTER | COMMAREA address |
| `WS-CALEN` | S9(4) COMP | Saved EIBCALEN |
| `WS-RETRY` | X | Retry flag (initialized to spaces, not actually used) |
| `DB2-CUSTOMERNUM-INT` | S9(9) COMP | Customer number in DB2 integer format |
| `DB2-POLICYNUM-INT` | S9(9) COMP | Policy number (defined but unused in this program) |
| `DB2-BROKERID-INT`, `DB2-PAYMENT-INT`, etc. | various COMP | DB2 integer host variables (defined for other programs via LGPOLICY, unused here) |
| `LGUCVS01` | X(8) | Program name literal `'LGUCVS01'` (line 68) |

### ERROR-MSG Structure (lines 56-66)

Error message with date, time, program name `' LGUCDB01'`, customer number, SQL request description, and SQLCODE.

### LINKAGE SECTION

- `DFHCOMMAREA`: Via `EXEC SQL INCLUDE LGCMAREA` (line 87).

### Copybooks Included

| Copybook | Method | Purpose |
|---|---|---|
| `LGERR` | `COPY LGERR` (line 53) | Common error handling working-storage |
| `LGPOLICY` | `EXEC SQL INCLUDE LGPOLICY` (line 73) | DB2 structures and length constants |
| `SQLCA` | `EXEC SQL INCLUDE SQLCA` (line 77) | DB2 SQL Communications Area |
| `LGCMAREA` | `EXEC SQL INCLUDE LGCMAREA` (line 87) | COMMAREA structure |
| `LGERRPRC` | `COPY LGERRPRC` (line 199) | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input

- **DFHCOMMAREA fields consumed:**
  - `CA-CUSTOMER-NUM` (9(10)): Customer number identifying which row to update.
  - `CA-FIRST-NAME` (X(10)): New first name.
  - `CA-LAST-NAME` (X(20)): New last name.
  - `CA-DOB` (X(10)): New date of birth.
  - `CA-HOUSE-NAME` (X(20)): New house name.
  - `CA-HOUSE-NUM` (X(4)): New house number.
  - `CA-POSTCODE` (X(8)): New postcode.
  - `CA-PHONE-MOBILE` (X(20)): New mobile phone.
  - `CA-PHONE-HOME` (X(20)): New home phone.
  - `CA-EMAIL-ADDRESS` (X(100)): New email address.

### Output

- **DFHCOMMAREA fields modified:**
  - `CA-RETURN-CODE`: `'00'` (success), `'01'` (customer not found), `'90'` (SQL error).

### Files/Tables Accessed

| Resource | Type | Operation |
|---|---|---|
| CUSTOMER (DB2 table) | DB2 | UPDATE |

### Other Programs Called

| Program | Method | Purpose |
|---|---|---|
| LGUCVS01 | `EXEC CICS LINK` (line 132) | Synchronize VSAM customer file after DB2 update |
| LGSTSQ | `EXEC CICS LINK` (via LGERRPRC) | Error logging to TSQ |

## 5. Algorithms and Techniques

### SQL Statements

**UPDATE (lines 151-165):**
```sql
UPDATE CUSTOMER
SET
  FIRSTNAME     = :CA-FIRST-NAME,
  LASTNAME      = :CA-LAST-NAME,
  DATEOFBIRTH   = :CA-DOB,
  HOUSENAME     = :CA-HOUSE-NAME,
  HOUSENUMBER   = :CA-HOUSE-NUM,
  POSTCODE      = :CA-POSTCODE,
  PHONEMOBILE   = :CA-PHONE-MOBILE,
  PHONEHOME     = :CA-PHONE-HOME,
  EMAILADDRESS  = :CA-EMAIL-ADDRESS
WHERE
  CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT
```

### CICS Commands

| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ABEND ABCODE('LGCA') NODUMP` | 116 | Terminate on missing COMMAREA |
| `EXEC CICS LINK Program(LGUCVS01)` | 132-135 | Synchronize VSAM file |
| `EXEC CICS RETURN` | 139 | Return to caller |

### Error Handling Approach

- Missing COMMAREA: ABEND `'LGCA'`.
- SQLCODE 0: Success.
- SQLCODE 100: Not found, return code `'01'`.
- Other SQLCODE: Error, return code `'90'`, error logged to TSQ.
- Unlike LGICDB01, there is no COMMAREA length check. The program assumes the COMMAREA is large enough.

### Notable Logic Patterns

- **DB2-then-VSAM pattern**: The program first updates DB2, then calls LGUCVS01 to update the VSAM copy. This dual-write pattern keeps both data sources in sync.
- **No conditional VSAM update**: The LINK to LGUCVS01 occurs even if the DB2 UPDATE fails (return code `'01'` or `'90'`). This could lead to inconsistencies if the DB2 update fails but VSAM succeeds.
- **COMMAREA length for VSAM call**: The LINK to LGUCVS01 uses LENGTH(225), which is much smaller than the full COMMAREA, passing only the customer data portion.

## 6. Analysis

### Overall Role

LGUCDB01 is the data-access module for customer updates. It occupies the same architectural position as LGICDB01 (customer inquire) but performs write operations. It is unique among the data-access programs in that it also calls another program (LGUCVS01) to maintain VSAM synchronization.

### Dependencies

- **DB2 table CUSTOMER** with columns: CUSTOMERNUMBER, FIRSTNAME, LASTNAME, DATEOFBIRTH, HOUSENAME, HOUSENUMBER, POSTCODE, PHONEMOBILE, PHONEHOME, EMAILADDRESS.
- **LGUCVS01** program for VSAM synchronization.
- **LGSTSQ** for error logging.
- **Copybooks:** LGERR, LGERRPRC, LGPOLICY, LGCMAREA, SQLCA.

### Known Issues or Limitations

- No COMMAREA length validation. If the COMMAREA is too short, the program may read beyond the COMMAREA boundary.
- The VSAM synchronization call to LGUCVS01 is unconditional, executing even after a failed DB2 update. This could cause data inconsistency.
- `WS-RETRY` (line 31) is initialized to spaces but never used, suggesting incomplete retry logic.
- Several DB2 integer host variables (`DB2-POLICYNUM-INT`, `DB2-BROKERID-INT`, `DB2-PAYMENT-INT`, etc., lines 38-48) are defined but unused by this program. They come from the LGPOLICY copybook or the program's own declarations.
- The `LGUCVS01` variable (line 68) is defined as a PIC X(8) literal but is not referenced in the LINK statement (which uses a literal).
- No explicit DB2 COMMIT; relies on CICS syncpoint handling.

### Relationship to Other Programs

- **Called by:** LGUCUS01 (customer update business logic) via `EXEC CICS LINK`.
- **Calls:** LGUCVS01 (VSAM customer update), LGSTSQ (error logging).
- **Sibling programs:** LGICDB01 (customer inquire DB2), LGIPDB01 (policy inquire DB2).
