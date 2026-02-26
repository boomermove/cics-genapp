# LGTESTP4 - Commercial Policy Menu

## 1. Overview

- **Program ID:** LGTESTP4 (line 11)
- **Purpose:** CICS BMS terminal interface for Commercial Policy transactions
- **Summary:** LGTESTP4 is a CICS pseudo-conversational program that presents a 3270 terminal screen (SSMAPP4) allowing users to inquire, add, or delete Commercial insurance policies. It collects user input from the BMS map, builds a COMMAREA request, delegates business logic to back-end programs (LGIPOL01, LGAPOL01, LGDPOL01) via EXEC CICS LINK, and displays results or error messages back to the user. The program is associated with CICS transaction ID `SSP4`. Unlike the other policy menu programs, LGTESTP4 features a sophisticated multi-mode inquiry capability (option 1) that supports lookup by customer+policy, policy only, customer only, or postcode.

## 2. Functional Breakdown

### MAINLINE SECTION (line 30)

The entry point. Checks `EIBCALEN` (line 32) to determine first vs. subsequent invocation:

- **First invocation (EIBCALEN = 0):** Initializes `SSMAPP4I`, `SSMAPP4O`, and `COMM-AREA` (lines 35-37). Sets default zero-filled values for customer number (`ENP4CNOO`) and policy number (`ENP4PNOO`) (lines 38-39). Sets `LOW-VALUES` for multiple peril/premium, status fields on the output map (lines 40-48): `ENP4FPEO`, `ENP4FPRO`, `ENP4CPEO`, `ENP4CPRO`, `ENP4XPEO`, `ENP4XPRO`, `ENP4WPEO`, `ENP4WPRO`, `ENP4STAO`. Then sends the blank SSMAPP4 map with `ERASE` (lines 52-55).
- **Subsequent invocation (EIBCALEN > 0):** Jumps directly to `A-GAIN` (line 33).

### A-GAIN (line 57)

Sets up AID key handling and receives user input:

1. **HANDLE AID** (lines 59-61): Routes CLEAR key to `CLEARIT` and PF3 to `ENDIT`.
2. **HANDLE CONDITION** (lines 62-64): Routes MAPFAIL to `ENDIT`.
3. **RECEIVE MAP** (lines 66-68): Reads user input from the terminal into `SSMAPP4I`.
4. **EVALUATE ENP4OPTO** (lines 71-249): Dispatches based on menu option.

#### Option '1' - Policy Inquiry (lines 73-154)

This is the most complex option, featuring a cascading IF structure (lines 74-120) that determines the inquiry mode based on which fields the user has populated:

**Mode 1: Customer + Policy lookup (lines 74-89)**
- Condition: Both `ENP4CNOO` and `ENP4PNOO` are non-empty (not spaces, not low-values, not zero).
- Sets `CA-REQUEST-ID` to `'01ICOM'` (standard inquire by customer+policy).
- Copies both customer number and policy number to COMMAREA.

**Mode 2: Policy-only lookup (lines 91-98)**
- Condition: Only `ENP4PNOO` is non-empty.
- Sets `CA-REQUEST-ID` to `'02ICOM'` (inquire by policy number only).
- Copies only policy number to COMMAREA.

**Mode 3: Customer-only lookup (lines 100-107)**
- Condition: Only `ENP4CNOO` is non-empty.
- Sets `CA-REQUEST-ID` to `'03ICOM'` (inquire by customer number only).
- Copies only customer number to COMMAREA.

**Mode 4: Postcode lookup (lines 109-116)**
- Condition: `ENP4HPCO` is non-empty.
- Sets `CA-REQUEST-ID` to `'05ICOM'` (inquire by postcode).
- Copies postcode to `CA-B-PostCode` in COMMAREA.

After determining the mode, the program:
- LINKs to `LGIPOL01` with the 32500-byte COMMAREA (lines 122-125).
- If `CA-RETURN-CODE > 0`, goes to `NO-DATA` (lines 126-128).
- On success, moves all returned commercial policy data into map input fields for display (lines 130-149): policy number, customer number, issue date, expiry date, address, postcode, latitude, longitude, customer name, property type, fire peril/premium, crime peril/premium, flood peril/premium, weather peril/premium, status, and reject reason.
- Sends the populated map and goes to `ENDIT-STARTIT` (lines 150-154).

#### Option '2' - Policy Add (lines 156-195)

- Sets `CA-REQUEST-ID` to `'01ACOM'` (add commercial policy).
- Copies all fields from map output into the COMMAREA (lines 157-176): customer number, issue date, expiry date, address, postcode, latitude, longitude, customer name, property type, all four peril/premium pairs (fire, crime, flood, weather), status, and reject reason.
- LINKs to `LGAPOL01` (lines 178-181).
- If `CA-RETURN-CODE > 0`, issues `SYNCPOINT ROLLBACK` and goes to `NO-ADD` (lines 182-185).
- On success, displays the returned customer and policy numbers with message `'New Commercial Policy Inserted'` in `ERP4FLDO` (lines 186-194).

#### Option '3' - Policy Delete (lines 197-234)

- Sets `CA-REQUEST-ID` to `'01DCOM'` (delete commercial policy).
- Copies customer number and policy number from the map (lines 199-200).
- LINKs to `LGDPOL01` (lines 201-204).
- If `CA-RETURN-CODE > 0`, issues `SYNCPOINT ROLLBACK` and goes to `NO-DELETE` (lines 205-208).
- On success, clears all policy detail fields to spaces (lines 210-227) and displays `'Commercial Policy Deleted'` in `ERP4FLDO` (lines 228-229).

#### WHEN OTHER (lines 236-247)

- Displays `'Please enter a valid option'` in `ERP4FLDO`.
- Sets cursor to option field by moving `-1` to `ENP4OPTL` (line 240).
- Sends map with `CURSOR` option.

### Standalone EXEC CICS RETURN (lines 254-255)

Fallthrough return after the EVALUATE (unreachable in normal flow).

### ENDIT-STARTIT (lines 257-261)

Pseudo-conversational return: `EXEC CICS RETURN TRANSID('SSP4') COMMAREA(COMM-AREA)`.

### ENDIT (lines 263-271)

Session termination: sends `MSGEND` text with `ERASE` and `FREEKB`, then returns without TRANSID.

### CLEARIT (lines 273-284)

CLEAR key handler: reinitializes `SSMAPP4I`, sends blank map with `MAPONLY`, returns pseudo-conversationally with `TRANSID('SSP4')`.

### NO-ADD (lines 286-294)

Error handler for failed add. Uses `EVALUATE CA-RETURN-CODE`:
- **When 70:** `'Customer does not exist'` in `ERP4FLDO` (line 289).
- **When Other:** `'Error Adding Commercial Policy'` in `ERP4FLDO` (line 292).

### NO-UPD (lines 296-298)

Error handler for failed update: `'Error Updating Commercial Policy'` in `ERP4FLDO`. Note: this paragraph exists but is never reached because option 4 (update) is not implemented in the EVALUATE (see Analysis section).

### NO-DELETE (lines 300-302)

Error handler for failed delete: `'Error Deleting Commercial Policy'` in `ERP4FLDO`.

### NO-DATA (lines 304-306)

Error handler for inquiry returning no data: `'No data was returned.'` in `ERP4FLDO`.

### ERROR-OUT (lines 308-318)

Common error display routine: sends the map with the error message, reinitializes `SSMAPP4I`, `SSMAPP4O`, and `COMM-AREA`, then goes to `ENDIT-STARTIT`.

### Control Flow Summary

```
Entry -> MAINLINE
  |-- (first time) -> Initialize maps -> SEND MAP -> A-GAIN
  |-- (returning)  -> A-GAIN
       |-> HANDLE AID/CONDITION setup
       |-> RECEIVE MAP
       |-> EVALUATE option
            1 -> determine inquiry mode -> LINK LGIPOL01 -> display -> ENDIT-STARTIT
            2 -> LINK LGAPOL01 -> display confirmation -> ENDIT-STARTIT
            3 -> LINK LGDPOL01 -> display confirmation -> ENDIT-STARTIT
            OTHER -> display error -> ENDIT-STARTIT
       CLEAR -> CLEARIT -> ENDIT-STARTIT
       PF3   -> ENDIT (session end)
       MAPFAIL -> ENDIT (session end)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables

| Variable | PIC / Type | Line | Role |
|----------|-----------|------|------|
| `MSGEND` | `PIC X(24)` | 18-19 | Literal `'Transaction ended'` displayed when the user exits via PF3 |

### COPY SSMAP (line 21)

Includes the COBOL symbolic map generated from the BMS mapset `SSMAP`. This provides the `SSMAPP4I` (input) and `SSMAPP4O` (output) structures for the Commercial Policy screen. Named map fields include:

- **ENP4CNO** (I/O) - Customer Number (10 digits)
- **ENP4PNO** (I/O) - Policy Number (10 digits)
- **ENP4IDA** (I/O) - Issue/Start Date (10 chars, yyyy-mm-dd)
- **ENP4EDA** (I/O) - Expiry Date (10 chars, yyyy-mm-dd)
- **ENP4ADD** (I/O) - Address (25 chars)
- **ENP4HPC** (I/O) - Postcode (8 chars)
- **ENP4LAT** (I/O) - Latitude (11 chars)
- **ENP4LON** (I/O) - Longitude (11 chars)
- **ENP4CUS** (I/O) - Customer Name (25 chars)
- **ENP4PTY** (I/O) - Property Type (25 chars)
- **ENP4FPE** (I/O) - Fire Peril (4 digits)
- **ENP4FPR** (I/O) - Fire Premium (8 digits)
- **ENP4CPE** (I/O) - Crime Peril (4 digits)
- **ENP4CPR** (I/O) - Crime Premium (8 digits)
- **ENP4XPE** (I/O) - Flood Peril (4 digits)
- **ENP4XPR** (I/O) - Flood Premium (8 digits)
- **ENP4WPE** (I/O) - Weather Peril (4 digits)
- **ENP4WPR** (I/O) - Weather Premium (8 digits)
- **ENP4STA** (I/O) - Status (4 digits)
- **ENP4REJ** (I/O) - Reject Reason (25 chars)
- **ENP4OPT** (I/O) - Menu Option (1 char)
- **ERP4FLD** (O) - Error/status message field (40 chars)

Each field has `I` (input), `O` (output), and `L` (length) suffixes as generated by BMS.

### COPY LGCMAREA (line 23)

Included under the `01 COMM-AREA` group (line 22). Provides the standard 32,500-byte shared COMMAREA. Key fields used by this program via the `CA-COMMERCIAL` redefines of `CA-POLICY-SPECIFIC`:

- `CA-REQUEST-ID` (PIC X(6)) - Request type code (`'01ICOM'`, `'02ICOM'`, `'03ICOM'`, `'05ICOM'`, `'01ACOM'`, `'01DCOM'`)
- `CA-RETURN-CODE` (PIC 9(2)) - Return code from back-end programs
- `CA-CUSTOMER-NUM` (PIC 9(10)) - Customer number
- `CA-POLICY-NUM` (PIC 9(10)) - Policy number
- `CA-ISSUE-DATE` (PIC X(10)) - Policy issue/start date
- `CA-EXPIRY-DATE` (PIC X(10)) - Policy expiry date
- `CA-B-Address` (PIC X(255)) - Business address
- `CA-B-Postcode` (PIC X(8)) - Business postcode
- `CA-B-Latitude` (PIC X(11)) - Latitude coordinate
- `CA-B-Longitude` (PIC X(11)) - Longitude coordinate
- `CA-B-Customer` (PIC X(255)) - Customer name
- `CA-B-PropType` (PIC X(255)) - Property type description
- `CA-B-FirePeril` (PIC 9(4)) - Fire peril code
- `CA-B-FirePremium` (PIC 9(8)) - Fire premium amount
- `CA-B-CrimePeril` (PIC 9(4)) - Crime peril code
- `CA-B-CrimePremium` (PIC 9(8)) - Crime premium amount
- `CA-B-FloodPeril` (PIC 9(4)) - Flood peril code
- `CA-B-FloodPremium` (PIC 9(8)) - Flood premium amount
- `CA-B-WeatherPeril` (PIC 9(4)) - Weather peril code
- `CA-B-WeatherPremium` (PIC 9(8)) - Weather premium amount
- `CA-B-Status` (PIC 9(4)) - Policy status
- `CA-B-RejectReason` (PIC X(255)) - Rejection reason text

### LINKAGE SECTION

No explicit LINKAGE SECTION or DFHCOMMAREA. As a CICS transaction entry program, it uses the implicit EIBCALEN check and pseudo-conversational COMMAREA passing.

### Condition Names (88-levels)

None defined. All conditions use explicit comparison operators.

## 4. Input/Output Behavior

### Input (from terminal)

User input is received via BMS map `SSMAPP4` in the `SSMAP` mapset:

- **ENP4OPTO** - Menu option (1=Inquiry, 2=Add, 3=Delete)
- **ENP4CNOO** - Customer number
- **ENP4PNOO** - Policy number
- **ENP4HPCO** - Postcode (used for postcode-based inquiry)
- **ENP4IDAO** - Issue/start date
- **ENP4EDAO** - Expiry date
- **ENP4ADDO** - Address
- **ENP4LATO** - Latitude
- **ENP4LONO** - Longitude
- **ENP4CUSO** - Customer name
- **ENP4PTYO** - Property type
- **ENP4FPEO / ENP4FPRO** - Fire peril/premium
- **ENP4CPEO / ENP4CPRO** - Crime peril/premium
- **ENP4XPEO / ENP4XPRO** - Flood peril/premium
- **ENP4WPEO / ENP4WPRO** - Weather peril/premium
- **ENP4STAO** - Status
- **ENP4REJO** - Reject reason

### Output (to terminal)

Results displayed via the same map. Status/error messages via `ERP4FLDO`:
- `'New Commercial Policy Inserted'`
- `'Commercial Policy Deleted'`
- `'No data was returned.'`
- `'Error Adding Commercial Policy'`
- `'Error Deleting Commercial Policy'`
- `'Error Updating Commercial Policy'`
- `'Customer does not exist'`
- `'Please enter a valid option'`

### COMMAREA (DFHCOMMAREA)

- **Sent to back-end programs:** `CA-REQUEST-ID`, `CA-CUSTOMER-NUM`, `CA-POLICY-NUM`, `CA-B-PostCode`, and all commercial policy fields depending on operation.
- **Received from back-end programs:** `CA-RETURN-CODE`, `CA-POLICY-NUM`, `CA-CUSTOMER-NUM`, and all commercial policy detail fields.
- **COMMAREA length:** 32,500 bytes on all LINK calls.

### Files Accessed

This program does not directly access any files. All data operations are delegated to back-end programs.

### Other Programs Called

| Program | Request ID(s) | Operation | Lines |
|---------|--------------|-----------|-------|
| `LGIPOL01` | `01ICOM`, `02ICOM`, `03ICOM`, `05ICOM` | Inquire commercial policy (4 lookup modes) | 122-125 |
| `LGAPOL01` | `01ACOM` | Add new commercial policy | 178-181 |
| `LGDPOL01` | `01DCOM` | Delete commercial policy | 201-204 |

## 5. Algorithms and Techniques

### SQL Statements

None. Database operations are delegated to the linked programs.

### CICS Commands Used

| Command | Purpose | Lines |
|---------|---------|-------|
| `EXEC CICS SEND MAP ('SSMAPP4') MAPSET ('SSMAP') ERASE` | Initial map display with screen clear | 52-55 |
| `EXEC CICS HANDLE AID CLEAR(CLEARIT) PF3(ENDIT)` | Register AID key handlers | 59-61 |
| `EXEC CICS HANDLE CONDITION MAPFAIL(ENDIT)` | Register MAPFAIL handler | 62-64 |
| `EXEC CICS RECEIVE MAP('SSMAPP4') INTO(SSMAPP4I) MAPSET('SSMAP')` | Receive terminal input | 66-68 |
| `EXEC CICS LINK PROGRAM('LGxPOL01') COMMAREA(COMM-AREA) LENGTH(32500)` | Delegate to back-end programs | 122, 178, 201 |
| `EXEC CICS SEND MAP ('SSMAPP4') FROM(SSMAPP4O) MAPSET ('SSMAP')` | Send data to terminal | 150, 191, 230, 242, 309 |
| `EXEC CICS SEND MAP ('SSMAPP4') ... CURSOR` | Send map with cursor positioning | 242-246 |
| `EXEC CICS SEND MAP ('SSMAPP4') MAPSET ('SSMAP') MAPONLY` | Send blank map (CLEAR handler) | 276-279 |
| `Exec CICS Syncpoint Rollback` | Roll back on add/delete failure | 183, 206 |
| `EXEC CICS RETURN TRANSID('SSP4') COMMAREA(COMM-AREA)` | Pseudo-conversational return | 258-261, 281-284 |
| `EXEC CICS RETURN` | Terminal return (end session) | 254-255, 270-271 |
| `EXEC CICS SEND TEXT FROM(MSGEND) ... ERASE FREEKB` | Display exit message | 264-269 |

### Error Handling Approach

- **HANDLE AID / HANDLE CONDITION:** Traditional CICS exception handling. CLEAR routes to CLEARIT, PF3 and MAPFAIL route to ENDIT.
- **Return code checking:** After each LINK, `CA-RETURN-CODE > 0` triggers the appropriate error paragraph.
- **Syncpoint Rollback:** Issued on failed add (line 183) and delete (line 206) to undo partial changes.
- **Error display:** All error paths converge at `ERROR-OUT`, which sends the error message, reinitializes all working storage, and returns pseudo-conversationally.

### Notable Logic Patterns

- **Cascading IF for inquiry mode selection (lines 74-120):** This is the most distinctive feature of LGTESTP4. Instead of a single inquiry mode, the program uses a prioritized cascade of four IF tests to determine which fields the user has provided and selects the appropriate request ID:
  1. Customer + Policy -> `'01ICOM'`
  2. Policy only -> `'02ICOM'`
  3. Customer only -> `'03ICOM'`
  4. Postcode only -> `'05ICOM'`

  Each IF test validates the field against four conditions: not spaces, not low-values, not 0, and not all-zeros. This is more sophisticated than the other policy menu programs which only support single-mode inquiry.

- **EVALUATE structure** (lines 71-249): Central dispatch for menu options.
- **Pseudo-conversational design:** Uses `EXEC CICS RETURN TRANSID('SSP4') COMMAREA(COMM-AREA)`.
- **LOW-VALUES initialization** (lines 40-48): The commercial policy screen uses `LOW-VALUES` instead of zeros/spaces for initializing numeric peril/premium and status fields, which causes BMS to skip transmitting these fields to the terminal (effectively leaving them blank on first display).

## 6. Analysis

### Overall Role in the Application Architecture

LGTESTP4 is the **presentation layer** (UI) component for Commercial Policy management within the IBM General Insurance Application (GenApp). It is the most feature-rich of the policy menu programs in terms of inquiry capabilities, supporting four different lookup modes. It sits alongside peer programs handling other policy types:

- LGTESTP1 - Motor Policy Menu (transaction SSP1)
- LGTESTP2 - Endowment Policy Menu (transaction SSP2)
- LGTESTP3 - House Policy Menu (transaction SSP3)
- **LGTESTP4 - Commercial Policy Menu (transaction SSP4)**
- LGTESTP5 - Claims Menu (transaction SSP5)

### Dependencies

- **CICS region** with BMS support and transaction definition for `SSP4`
- **BMS mapset `SSMAP`** containing the `SSMAPP4` map definition (defined in `ssmap.bms`, lines 449-600)
- **Copybook `SSMAP`** (COBOL symbolic map generated from BMS source)
- **Copybook `LGCMAREA`** (defined in `lgcmarea.cpy`) for the shared COMMAREA structure
- **Back-end programs:** LGIPOL01, LGAPOL01, LGDPOL01 must be installed in the CICS region
- **3270 terminal** or terminal emulator

### Known Issues or Limitations

1. **No update (option 4) implementation:** The EVALUATE block does not include a `WHEN '4'` case. The BMS map definition in `ssmap.bms` (lines 460-461) has the "4. Policy Update" option commented out. However, the program still contains a `NO-UPD` error handler paragraph (lines 296-298) that is dead code -- it can never be reached. This suggests the update feature was planned but not implemented, or was intentionally removed.

2. **Missing request ID `'04ICOM'`:** The inquiry mode cascade jumps from `'03ICOM'` (customer-only) to `'05ICOM'` (postcode), skipping `'04ICOM'`. This may indicate a removed or unimplemented inquiry mode.

3. **No fallthrough handling in inquiry mode cascade:** If the user enters option 1 without populating any of the four tested fields (customer number, policy number, or postcode), none of the IF conditions are satisfied, `CA-REQUEST-ID` remains uninitialized (or carries its previous value), and the LINK to LGIPOL01 still executes (line 122). This could cause unpredictable behavior.

4. **Field truncation on display:** The COMMAREA fields `CA-B-Address` (255 chars), `CA-B-Customer` (255 chars), and `CA-B-PropType` (255 chars) are much larger than the corresponding screen fields `ENP4ADD` (25 chars), `ENP4CUS` (25 chars), and `ENP4PTY` (25 chars). Data will be truncated during the MOVE operations on display (lines 134, 138, 139), and only partial data can be entered on the add screen (lines 161, 165, 166). Similarly, `CA-B-RejectReason` (255 chars) is truncated to 25 chars on display.

5. **HANDLE AID/CONDITION usage:** Uses the older exception handling style rather than RESP/RESP2, making control flow harder to trace.

6. **No input validation:** No client-side validation of user input before passing to back-end programs.

7. **Hard-coded COMMAREA length:** LENGTH(32500) is hard-coded rather than using `LENGTH OF COMM-AREA`.

8. **Asymmetric field usage in add operation:** The add operation (option 2) reads from `O`-suffix (output) fields (e.g., `ENP4IDAO`, `ENP4CNOO`) while most operations use `O` fields for keys and `I` fields for data. This is consistent within the program but differs slightly from other programs in the suite.

### Relationship to Other Programs

- **Peer programs:** LGTESTP1 (Motor), LGTESTP2 (Endowment), LGTESTP3 (House), LGTESTP5 (Claims) -- all follow a similar structural pattern, but LGTESTP4 has the most complex inquiry logic.
- **Called programs:**
  - `LGIPOL01` - Policy inquiry dispatcher, which routes `xxICOM` requests to commercial policy inquiry logic
  - `LGAPOL01` - Policy add dispatcher, which routes `01ACOM` to commercial policy add logic
  - `LGDPOL01` - Policy delete dispatcher, which routes `01DCOM` to commercial policy delete logic
- **Shared copybooks:** `LGCMAREA` (COMMAREA structure) and `SSMAP` (BMS symbolic map) are shared across all programs in the application.
- **Calling program:** Typically invoked from the main menu or directly via transaction ID `SSP4` at a CICS terminal.
- **Notable absence:** No call to `LGUPOL01` (update), confirming that update functionality was not implemented for commercial policies.
