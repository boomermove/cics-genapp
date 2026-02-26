# LGACDB02.cbl - Add Customer Security Record (DB2)

## 1. Overview

- **Program ID:** LGACDB02
- **Purpose:** Insert a new customer password/security record into the DB2 CUSTOMER_SECURE table.
- **Summary:** LGACDB02 is a DB2 data-access program that creates initial security credentials for a newly added customer. It receives the customer number, a default password (BD5 checksum), a password change count, and a state indicator, then inserts them into the CUSTOMER_SECURE table. It is called by LGACDB01 after the customer record has been created in the CUSTOMER table and VSAM file.

## 2. Functional Breakdown

### MAINLINE SECTION (lines 110-153)

1. **Initialization (lines 116-120):** Initializes `WS-HEADER`, captures EIB fields for debugging.
2. **Commarea validation (lines 126-130):** If `EIBCALEN` is zero, writes error and abends with `'LGCA'`.
3. **Set defaults (lines 133-135):** Sets `D2-RETURN-CODE` to `'00'`, stores commarea length and address.
4. **Request routing (lines 138-147):** Uses `EVALUATE D2-REQUEST-ID`:
   - `'02ACUS'`: Moves customer number and count to DB2 integer host variables, performs `INSERT-CUSTOMER-PASSWORD`.
   - `When Other`: Sets return code `'99'` and returns immediately.
5. **Return (line 150):** Returns to caller.

### INSERT-CUSTOMER-PASSWORD (lines 156-179)

1. Sets the error message SQL request field to `' INSERT SECURITY'`.
2. Executes an INSERT into `CUSTOMER_SECURE` with the customer number, password, state indicator, and password change count.
3. If `SQLCODE` is not 0, sets `D2-RETURN-CODE` to `'98'`, writes error, and returns.

### WRITE-ERROR-MESSAGE (lines 186-196)

Saves `SQLCODE` to `EM-SQLRC`, formats time/date via `LGERR-FORMAT-TIME`, then writes the error and commarea to TSQ via `LGERR-WRITE-MSG` and `LGERR-LOG-COMMAREA`.

### MAINLINE-EXIT (lines 152-153)

Standard section exit.

### Control Flow

```
MAINLINE -> EVALUATE D2-REQUEST-ID
  '02ACUS' -> INSERT-CUSTOMER-PASSWORD -> RETURN
  Other    -> RETURN (code '99')
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | Type | Role |
|---|---|---|
| `WS-HEADER` (line 25) | Group | Debug eyecatcher `'LGACDB02------WS'`, EIB fields |
| `WS-RESP` (line 36) | `PIC S9(8) COMP` | CICS response code |
| `LastCustNum` (line 37) | `PIC S9(8) COMP` | Declared but unused in this program |
| `ERROR-MSG` (line 44) | Group | Error message with date, time, customer number, SQL request, SQLCODE |
| `DB2-CUSTOMERNUM-INT` (line 73) | `PIC S9(9) COMP` | DB2 host variable for customer number |
| `DB2-CUSTOMERCNT-INT` (line 74) | `PIC S9(9) COMP` | DB2 host variable for password change count |

### LINKAGE SECTION

- `DFHCOMMAREA` (line 90): A custom structure (not LGCMAREA) with:
  - `D2-REQUEST-ID` PIC X(6)
  - `D2-RETURN-CODE` PIC 9(2)
  - `D2-CUSTOMER-NUM` PIC 9(10)
  - `D2-CUSTSECR-PASS` PIC X(32) -- the password hash
  - `D2-CUSTSECR-COUNT` PIC X(4) -- password change count
  - `D2-CUSTSECR-STATE` PIC X -- state indicator
  - `D2-CUSTSECR-DATA` PIC X(32445) -- filler/data area

### Copybooks Included

| Copybook | Statement | Purpose |
|---|---|---|
| `LGERR` (line 41) | `COPY LGERR` | Common error handling working storage |
| `LGPOLICY` (line 61) | `COPY LGPOLICY` | Policy/customer length definitions (used for consistency) |
| `SQLCA` (line 82) | `EXEC SQL INCLUDE SQLCA` | DB2 SQL communications area |
| `LGERRPRC` (line 201) | `COPY LGERRPRC` | Common error handling procedures |

### Condition Names (88-levels)

None defined.

## 4. Input/Output Behavior

### Input (COMMAREA -- CDB2AREA structure)
- `D2-REQUEST-ID`: Must be `'02ACUS'` for new customer security record.
- `D2-CUSTOMER-NUM`: The customer number to create security for.
- `D2-CUSTSECR-PASS`: The default password (hash).
- `D2-CUSTSECR-COUNT`: Initial password change count.
- `D2-CUSTSECR-STATE`: State indicator (`'N'` for new).

### Output (COMMAREA)
- `D2-RETURN-CODE`: `'00'` success, `'98'` SQL error, `'99'` unknown request.

### Files Accessed
- **DB2 CUSTOMER_SECURE table** -- INSERT (line 162)

### Other Programs Called
| Program | Purpose |
|---|---|
| `LGSTSQ` (via LGERRPRC) | Write error messages to temporary storage queue |

## 5. Algorithms and Techniques

### SQL Statements

**INSERT security record (lines 161-171):**
```sql
INSERT INTO CUSTOMER_SECURE
  ( customerNumber,
    customerPass,
    state_indicator,
    pass_changes )
VALUES ( :DB2-CUSTOMERNUM-INT,
         :D2-CUSTSECR-PASS,
         :D2-CUSTSECR-STATE,
         :DB2-CUSTOMERCNT-INT )
```

### CICS Commands
- `EXEC CICS ABEND ABCODE('LGCA') NODUMP` -- Abend on missing commarea
- `EXEC CICS RETURN` -- Return to caller (used in multiple places)

### Error Handling
- Missing commarea causes an abend with code `'LGCA'`.
- SQL errors set return code `'98'` and log the error including SQLCODE.
- Unrecognized request IDs return `'99'`.

### Notable Logic Patterns
- **EVALUATE for request routing (line 138):** Although only one request type (`'02ACUS'`) is currently supported, the EVALUATE structure allows for future extension.
- **Commented-out LGCMAREA include (lines 99-102):** The original design used the standard LGCMAREA, but was replaced with a custom commarea specific to security operations.

## 6. Analysis

### Overall Role
LGACDB02 is a single-purpose DB2 access program for creating customer security records. It acts as a specialized data-access layer called by LGACDB01 after the main customer record is created.

### Dependencies
- DB2 CUSTOMER_SECURE table
- Programs: LGSTSQ (error logging)
- Copybooks: LGERR, LGERRPRC, LGPOLICY, SQLCA

### Known Issues or Limitations
1. **`LastCustNum` variable (line 37)** is declared but never used.
2. **No validation of password data:** The program inserts whatever password value it receives without any validation or hashing.
3. **Custom commarea:** Uses a non-standard commarea structure rather than LGCMAREA, creating a tight coupling with LGACDB01's `CDB2AREA` definition.
4. **No SYNCPOINT management:** The program does not issue SYNCPOINT or ROLLBACK; transactional integrity depends on the caller.

### Relationship to Other Programs
- **Called by:** LGACDB01 (original version, via EXEC CICS LINK)
- **Not called by:** LGACDB01_SECURE (which delegates security to LGAUTH01 instead)
- **Peer:** Works alongside LGACVS01 as part of the customer creation workflow
