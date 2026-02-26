# LGTESTP3 - House Policy Menu

## 1. Overview

- **Program ID:** LGTESTP3 (line 11)
- **Purpose:** CICS BMS terminal interface for House Policy transactions
- **Summary:** LGTESTP3 is a CICS pseudo-conversational program that presents a 3270 terminal screen (SSMAPP3) allowing users to inquire, add, delete, or update House insurance policies. It acts as a front-end menu that collects user input from the BMS map, constructs a COMMAREA request, delegates the actual business logic to back-end programs (LGIPOL01, LGAPOL01, LGDPOL01, LGUPOL01) via EXEC CICS LINK, and displays the results or error messages back to the user. The program is associated with CICS transaction ID `SSP3`.

## 2. Functional Breakdown

### MAINLINE SECTION (line 30)

The entry point of the program. It checks `EIBCALEN` (line 32) to determine if this is the first invocation or a subsequent pseudo-conversational return:

- **First invocation (EIBCALEN = 0):** Initializes the input map `SSMAPP3I`, the output map `SSMAPP3O`, and `COMM-AREA` (lines 35-37). Sets default zero-filled values for customer number (`ENP3CNOO`), policy number (`ENP3PNOO`), house value (`ENP3VALO`), and bedrooms (`ENP3BEDO`) on the output map (lines 38-41). Then sends the blank SSMAPP3 map to the terminal with `ERASE` (lines 45-48).
- **Subsequent invocation (EIBCALEN > 0):** Jumps directly to `A-GAIN` (line 33).

### A-GAIN (line 50)

Sets up AID key handling and receives user input:

1. **HANDLE AID** (lines 52-54): Routes CLEAR key to `CLEARIT` and PF3 key to `ENDIT`.
2. **HANDLE CONDITION** (lines 55-57): Routes MAPFAIL condition to `ENDIT`.
3. **RECEIVE MAP** (lines 59-61): Reads user input from the terminal into `SSMAPP3I`.
4. **EVALUATE ENP3OPTO** (lines 64-230): Dispatches based on the user-selected menu option:

#### Option '1' - Policy Inquiry (lines 66-90)
- Sets `CA-REQUEST-ID` to `'01IHOU'` (inquire house policy).
- Copies customer number and policy number from the map output fields into the COMMAREA.
- LINKs to program `LGIPOL01` with a 32500-byte COMMAREA (lines 70-73).
- If `CA-RETURN-CODE > 0`, goes to `NO-DATA` (lines 74-76).
- On success, moves returned policy data (issue date, expiry date, property type, bedrooms, value, house name, house number, postcode) into the map input fields for display (lines 78-85).
- Sends the populated map and goes to `ENDIT-STARTIT` (lines 86-90).

#### Option '2' - Policy Add (lines 92-123)
- Sets `CA-REQUEST-ID` to `'01AHOU'` (add house policy).
- Copies all house policy fields from map input into the COMMAREA, including zeroing out `CA-PAYMENT` and `CA-BROKERID` (lines 95-96).
- LINKs to program `LGAPOL01` (lines 106-109).
- If `CA-RETURN-CODE > 0`, issues `SYNCPOINT ROLLBACK` and goes to `NO-ADD` (lines 110-113).
- On success, displays the returned customer number and policy number along with the message `'New House Policy Inserted'` in `ERP3FLDO` (lines 114-122).

#### Option '3' - Policy Delete (lines 125-153)
- Sets `CA-REQUEST-ID` to `'01DHOU'` (delete house policy).
- LINKs to program `LGDPOL01` (lines 129-132).
- If `CA-RETURN-CODE > 0`, issues `SYNCPOINT ROLLBACK` and goes to `NO-DELETE` (lines 133-136).
- On success, clears all policy detail fields to spaces (lines 138-145) and displays `'House Policy Deleted'` (lines 147-148).

#### Option '4' - Policy Update (lines 155-214)
- This is a two-phase operation:
  - **Phase 1 - Inquire:** Sets `CA-REQUEST-ID` to `'01IHOU'` and LINKs to `LGIPOL01` to retrieve the current policy data (lines 156-165). Displays the data on the map and issues a second `RECEIVE MAP` to let the user modify the fields (lines 167-181).
  - **Phase 2 - Update:** Sets `CA-REQUEST-ID` to `'01UHOU'` and LINKs to `LGUPOL01` with the user-modified data (lines 183-199).
- If the update fails (`CA-RETURN-CODE > 0`), goes to `NO-UPD` (lines 200-202).
- On success, displays `'House Policy Updated'` (lines 207-208).

#### WHEN OTHER (lines 217-228)
- Displays `'Please enter a valid option'` in `ERP3FLDO`.
- Sets cursor position to the option field by moving `-1` to `ENP3OPTL` (line 221).
- Sends map with `CURSOR` option.

### Standalone EXEC CICS RETURN (lines 235-236)

A fallthrough return that executes only if no EVALUATE branch was taken (unreachable in normal flow).

### ENDIT-STARTIT (lines 238-242)

Performs a pseudo-conversational return: issues `EXEC CICS RETURN TRANSID('SSP3') COMMAREA(COMM-AREA)`, which tells CICS to restart transaction SSP3 when the user next presses an AID key, passing the COMMAREA for continuity.

### ENDIT (lines 244-252)

Terminates the session: sends the `MSGEND` text (`'Transaction ended'`) to the terminal with `ERASE` and `FREEKB`, then issues `EXEC CICS RETURN` without a TRANSID (ending the pseudo-conversation).

### CLEARIT (lines 254-265)

Handles the CLEAR key: reinitializes `SSMAPP3I`, sends a blank map (`MAPONLY`), and returns pseudo-conversationally with `TRANSID('SSP3')`.

### NO-ADD (lines 267-275)

Error handler for failed add operations. Uses `EVALUATE CA-RETURN-CODE`:
- **When 70:** Displays `'Customer does not exist'` in `ERP1FLDO` (note: this references the P1 error field, which is a likely copy-paste bug -- see Analysis section).
- **When Other:** Displays `'Error Adding House Policy'` in `ERP1FLDO`.
- Both cases go to `ERROR-OUT`.

### NO-UPD (lines 277-279)

Error handler for failed update: displays `'Error Updating House Policy'` in `ERP3FLDO`.

### NO-DELETE (lines 281-283)

Error handler for failed delete: displays `'Error Deleting House Policy'` in `ERP3FLDO`.

### NO-DATA (lines 285-287)

Error handler for inquiry returning no data: displays `'No data was returned.'` in `ERP3FLDO`.

### ERROR-OUT (lines 289-299)

Common error display routine: sends the map with the error message, reinitializes `SSMAPP3I`, `SSMAPP3O`, and `COMM-AREA`, then goes to `ENDIT-STARTIT` for pseudo-conversational return.

### Control Flow Summary

```
Entry -> MAINLINE
  |-- (first time) -> Initialize maps -> SEND MAP -> A-GAIN
  |-- (returning)  -> A-GAIN
       |-> HANDLE AID/CONDITION setup
       |-> RECEIVE MAP
       |-> EVALUATE option
            1 -> LINK LGIPOL01 -> display results -> ENDIT-STARTIT
            2 -> LINK LGAPOL01 -> display confirmation -> ENDIT-STARTIT
            3 -> LINK LGDPOL01 -> display confirmation -> ENDIT-STARTIT
            4 -> LINK LGIPOL01 -> RECEIVE MAP -> LINK LGUPOL01 -> ENDIT-STARTIT
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

Includes the COBOL symbolic map generated from the BMS mapset `SSMAP`. This provides the `SSMAPP3I` (input) and `SSMAPP3O` (output) structures for the House Policy screen, including all the named map fields:

- **ENP3CNO** (I/O) - Customer Number (10 digits)
- **ENP3PNO** (I/O) - Policy Number (10 digits)
- **ENP3IDA** (I/O) - Issue Date (10 chars, yyyy-mm-dd)
- **ENP3EDA** (I/O) - Expiry Date (10 chars, yyyy-mm-dd)
- **ENP3TYP** (I/O) - Property Type (15 chars)
- **ENP3BED** (I/O) - Bedrooms (3 digits)
- **ENP3VAL** (I/O) - House Value (8 digits)
- **ENP3HNM** (I/O) - House Name (20 chars)
- **ENP3HNO** (I/O) - House Number (4 chars)
- **ENP3HPC** (I/O) - Postcode (8 chars)
- **ENP3OPT** (I/O) - Menu Option (1 char)
- **ERP3FLD** (O) - Error/status message field (40 chars)

Each field has an `I` (input) suffix, an `O` (output) suffix, and an `L` (length) suffix as generated by BMS.

### COPY LGCMAREA (line 23)

Included under the `01 COMM-AREA` group (line 22). Provides the standard 32,500-byte communication area structure shared across all GenApp programs. Key fields used by this program:

- `CA-REQUEST-ID` (PIC X(6)) - Request type code (e.g., `'01IHOU'`, `'01AHOU'`, `'01DHOU'`, `'01UHOU'`)
- `CA-RETURN-CODE` (PIC 9(2)) - Return code from back-end programs
- `CA-CUSTOMER-NUM` (PIC 9(10)) - Customer number
- `CA-POLICY-NUM` (PIC 9(10)) - Policy number (within `CA-POLICY-REQUEST` redefines)
- `CA-ISSUE-DATE` (PIC X(10)) - Policy issue date
- `CA-EXPIRY-DATE` (PIC X(10)) - Policy expiry date
- `CA-PAYMENT` (PIC 9(6)) - Payment amount
- `CA-BROKERID` (PIC 9(10)) - Broker ID
- `CA-BROKERSREF` (PIC X(10)) - Broker's reference
- `CA-H-PROPERTY-TYPE` (PIC X(15)) - House property type
- `CA-H-BEDROOMS` (PIC 9(3)) - Number of bedrooms
- `CA-H-VALUE` (PIC 9(8)) - House value
- `CA-H-HOUSE-NAME` (PIC X(20)) - House name
- `CA-H-HOUSE-NUMBER` (PIC X(4)) - House number
- `CA-H-POSTCODE` (PIC X(8)) - House postcode

### LINKAGE SECTION

There is no explicit LINKAGE SECTION or DFHCOMMAREA definition in this program. As a CICS transaction entry program, it receives the COMMAREA implicitly via the `EIBCALEN` check and the pseudo-conversational RETURN COMMAREA mechanism.

### Condition Names (88-levels)

None are defined in this program. All conditional logic uses explicit comparison operators.

## 4. Input/Output Behavior

### Input (from terminal)

The program receives input via the BMS map `SSMAPP3` in the `SSMAP` mapset. The user provides:

- **ENP3OPTO** - Menu option selection (1=Inquiry, 2=Add, 3=Delete, 4=Update)
- **ENP3CNOO / ENP3CNOI** - Customer number
- **ENP3PNOO / ENP3PNOI** - Policy number
- **ENP3IDAI** - Issue date
- **ENP3EDAI** - Expiry date
- **ENP3TYPI** - Property type
- **ENP3BEDI** - Number of bedrooms
- **ENP3VALI** - House value
- **ENP3HNMI** - House name
- **ENP3HNOI** - House number
- **ENP3HPCI** - Postcode

### Output (to terminal)

Results and status messages are displayed via the same BMS map `SSMAPP3`. Output fields populated include all the policy detail fields listed above (with `I` suffix for display), plus `ERP3FLDO` for status/error messages such as:
- `'New House Policy Inserted'`
- `'House Policy Deleted'`
- `'House Policy Updated'`
- `'No data was returned.'`
- `'Error Adding House Policy'`
- `'Error Updating House Policy'`
- `'Error Deleting House Policy'`
- `'Customer does not exist'`
- `'Please enter a valid option'`

### COMMAREA (DFHCOMMAREA)

- **Sent to back-end programs:** `CA-REQUEST-ID`, `CA-CUSTOMER-NUM`, `CA-POLICY-NUM`, and house-specific fields depending on the operation.
- **Received from back-end programs:** `CA-RETURN-CODE`, `CA-POLICY-NUM` (for add), and all house policy detail fields (for inquiry).
- **COMMAREA length:** 32,500 bytes passed on all LINK calls.

### Files Accessed

This program does not directly access any files (VSAM or DB2). All data access is delegated to the linked back-end programs.

### Other Programs Called

| Program | Request ID | Operation | Lines |
|---------|-----------|-----------|-------|
| `LGIPOL01` | `01IHOU` | Inquire house policy | 70-73, 159-162 |
| `LGAPOL01` | `01AHOU` | Add new house policy | 106-109 |
| `LGDPOL01` | `01DHOU` | Delete house policy | 129-132 |
| `LGUPOL01` | `01UHOU` | Update house policy | 196-199 |

## 5. Algorithms and Techniques

### SQL Statements

None. This program contains no SQL. Database operations are delegated to the linked programs.

### CICS Commands Used

| Command | Purpose | Lines |
|---------|---------|-------|
| `EXEC CICS SEND MAP ('SSMAPP3') MAPSET ('SSMAP') ERASE` | Initial map display, clearing the screen | 45-48 |
| `EXEC CICS HANDLE AID CLEAR(CLEARIT) PF3(ENDIT)` | Register AID key handlers for CLEAR and PF3 | 52-54 |
| `EXEC CICS HANDLE CONDITION MAPFAIL(ENDIT)` | Register error handler for MAPFAIL | 55-57 |
| `EXEC CICS RECEIVE MAP('SSMAPP3') INTO(SSMAPP3I) MAPSET('SSMAP')` | Receive user terminal input | 59-61 |
| `EXEC CICS LINK PROGRAM('LGxPOL01') COMMAREA(COMM-AREA) LENGTH(32500)` | Delegate business operations to back-end | 70, 106, 129, 159, 196 |
| `EXEC CICS SEND MAP ('SSMAPP3') FROM(SSMAPP3O) MAPSET ('SSMAP')` | Send populated map data to terminal | 86, 119, 149, 175, 209, 223, 290 |
| `EXEC CICS SEND MAP ('SSMAPP3') ... CURSOR` | Send map with cursor positioning | 223-227 |
| `EXEC CICS SEND MAP ('SSMAPP3') MAPSET ('SSMAP') MAPONLY` | Send blank map (CLEAR key handler) | 257-260 |
| `Exec CICS Syncpoint Rollback` | Roll back transaction on add/delete failure | 111, 134 |
| `EXEC CICS RETURN TRANSID('SSP3') COMMAREA(COMM-AREA)` | Pseudo-conversational return | 239-242, 262-265 |
| `EXEC CICS RETURN` | Terminal return (end session) | 235-236, 251-252 |
| `EXEC CICS SEND TEXT FROM(MSGEND) ... ERASE FREEKB` | Display exit message | 245-250 |

### Error Handling Approach

- **HANDLE AID / HANDLE CONDITION:** Traditional CICS exception handling (not RESP-based). CLEAR key and PF3 key are intercepted. MAPFAIL condition routes to session termination.
- **Return code checking:** After each LINK call, `CA-RETURN-CODE` is checked. A non-zero value routes to the appropriate error paragraph (NO-DATA, NO-ADD, NO-DELETE, NO-UPD).
- **Syncpoint Rollback:** Issued on failed add (line 111) and delete (line 134) operations to undo any partial database changes.
- **Error display:** All error paths converge at `ERROR-OUT`, which sends the error message to the terminal, reinitializes all working storage, and returns pseudo-conversationally.

### Notable Logic Patterns

- **EVALUATE structure** (lines 64-230): Central dispatching mechanism based on the user's menu option selection.
- **Two-phase update** (option 4, lines 155-214): First inquires the current data, sends it to the screen, receives user modifications via a second RECEIVE MAP, then sends the update request. This is a conversational interaction embedded within the pseudo-conversational flow.
- **Pseudo-conversational design:** Uses `EXEC CICS RETURN TRANSID('SSP3') COMMAREA(COMM-AREA)` to return control to CICS between user interactions, preserving state in the COMMAREA.

## 6. Analysis

### Overall Role in the Application Architecture

LGTESTP3 is the **presentation layer** (UI) component for House Policy management within the IBM General Insurance Application (GenApp). It is one of several menu programs in the system, each handling a different policy type:

- LGTESTP1 - Motor Policy Menu (transaction SSP1)
- LGTESTP2 - Endowment Policy Menu (transaction SSP2)
- **LGTESTP3 - House Policy Menu (transaction SSP3)**
- LGTESTP4 - Commercial Policy Menu (transaction SSP4)

The program follows a strict separation of concerns: all UI interaction is handled here, while business logic and data access are delegated to the LGxPOL01 family of programs via COMMAREA-based LINK calls.

### Dependencies

- **CICS region** with BMS support and transaction definition for `SSP3`
- **BMS mapset `SSMAP`** containing the `SSMAPP3` map definition (defined in `ssmap.bms`)
- **Copybook `SSMAP`** (COBOL symbolic map generated from the BMS source)
- **Copybook `LGCMAREA`** (defined in `lgcmarea.cpy`) for the shared COMMAREA structure
- **Back-end programs:** LGIPOL01, LGAPOL01, LGDPOL01, LGUPOL01 must be installed and available in the CICS region
- **3270 terminal** or terminal emulator for user interaction

### Known Issues or Limitations

1. **Bug in NO-ADD error handler (lines 270, 273):** The error messages are written to `ERP1FLDO` (the Motor Policy screen's error field) instead of `ERP3FLDO` (the House Policy screen's error field). This is a copy-paste error from LGTESTP1. While the map sent is SSMAPP3, the field `ERP1FLDO` belongs to SSMAPP1, so the error message likely will not appear on the correct screen or could cause unexpected behavior.

2. **Option 4 uses conversational RECEIVE within pseudo-conversational program (lines 179-181):** The update operation performs a second `EXEC CICS RECEIVE MAP` in the middle of processing, which temporarily makes the program conversational. This ties up the CICS task while waiting for user input, which is generally considered a poor practice for scalability. A better approach would be to split the update into two pseudo-conversational interactions using COMMAREA state flags.

3. **No input validation:** The program does not validate the format or content of user-entered fields (dates, numeric fields, etc.) before passing them to back-end programs. All validation burden falls on the back-end.

4. **HANDLE AID/CONDITION usage:** The program uses the older `HANDLE AID` and `HANDLE CONDITION` approach rather than the recommended `RESP`/`RESP2` inline error checking. This makes the control flow harder to follow due to implicit GO TO behavior.

5. **No option for listing policies:** There is no menu option to list all house policies for a customer; only single-policy operations are supported.

6. **Hard-coded COMMAREA length:** The LENGTH(32500) is hard-coded on all LINK calls rather than using `LENGTH OF COMM-AREA`, making maintenance more fragile if the COMMAREA structure changes.

### Relationship to Other Programs

- **Peer programs:** LGTESTP1 (Motor), LGTESTP2 (Endowment), LGTESTP4 (Commercial), LGTESTP5 (Claims) -- all follow the same structural pattern.
- **Called programs:** LGIPOL01 (inquiry), LGAPOL01 (add), LGDPOL01 (delete), LGUPOL01 (update) -- these are the generic policy operation dispatchers that route to type-specific handlers based on `CA-REQUEST-ID`.
- **Shared copybooks:** `LGCMAREA` (COMMAREA structure) and `SSMAP` (BMS symbolic map) are shared across all menu and back-end programs.
- **Calling program:** Typically invoked from the main menu or directly via transaction ID `SSP3` entered at a CICS terminal.
