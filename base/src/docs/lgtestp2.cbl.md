# lgtestp2.cbl - Endowment Policy Menu Transaction

## 1. Overview

- **Program ID:** LGTESTP2
- **Purpose:** Provides a BMS terminal menu for performing Endowment (Life) Policy CRUD operations: Inquire, Add, Delete, and Update.

LGTESTP2 is a CICS pseudo-conversational terminal program that presents the `SSMAPP2` map to the user and handles endowment policy inquiry (option 1), add (option 2), delete (option 3), and update (option 4) operations. It communicates with the same shared back-end service programs as LGTESTP1 (`LGIPOL01`, `LGAPOL01`, `LGDPOL01`, `LGUPOL01`) but uses request IDs with the `END` suffix (e.g., `'01IEND'`) to indicate endowment policy type. The transaction uses pseudo-conversational return with transaction ID `SSP2`. Despite the source comments and messages referring to "Life Policy," the data structure names use "Endowment" (`CA-ENDOWMENT`, `CA-E-*` fields).

## 2. Functional Breakdown

### MAINLINE SECTION (line 30)

**Lines 32-33 - Continuation Check:**
If `EIBCALEN > 0`, skip to `A-GAIN` for pseudo-conversational continuation.

**Lines 35-45 - First Invocation:**
Initializes map areas (`SSMAPP2I`, `SSMAPP2O`) and `COMM-AREA`. Sets default customer number (`ENP2CNOO`) and policy number (`ENP2PNOO`) to `'0000000000'`. Sends the `SSMAPP2` map with `ERASE`.

#### A-GAIN (line 47)
Sets up AID handling (CLEAR -> `CLEARIT`, PF3 -> `ENDIT`, MAPFAIL -> `ENDIT`). Receives map input.

**Lines 61-231 - Option Dispatch (EVALUATE ENP2OPTO):**

**Option '1' - Inquire Endowment Policy (lines 63-88):**
1. Sets `CA-REQUEST-ID` to `'01IEND'`
2. Moves customer and policy numbers from map
3. LINKs to `LGIPOL01` with 32,500-byte commarea
4. If error, goes to `NO-DATA`
5. Populates map with endowment-specific fields: issue date, expiry date, fund name, term, sum assured, life assured, with-profits flag, managed-fund flag, equities flag
6. Sends map and goes to `ENDIT-STARTIT`

**Option '2' - Add Endowment Policy (lines 90-123):**
1. Sets `CA-REQUEST-ID` to `'01AEND'`
2. Moves customer number, zeros for payment/broker, and all endowment fields from map to commarea
3. LINKs to `LGAPOL01`
4. On error, performs `Syncpoint Rollback` and goes to `NO-ADD`
5. On success, displays new customer/policy numbers, fund name, and `'New Life Policy Inserted'`

**Option '3' - Delete Endowment Policy (lines 125-153):**
1. Sets `CA-REQUEST-ID` to `'01DEND'`
2. Moves customer and policy numbers
3. LINKs to `LGDPOL01`
4. On error, performs `Syncpoint Rollback` and goes to `NO-DELETE`
5. On success, clears all endowment fields to spaces and displays `'Life Policy Deleted'`

**Option '4' - Update Endowment Policy (lines 155-216):**
1. First retrieves current data by LINKing to `LGIPOL01`
2. Populates and sends the map
3. Receives the modified map back from the user
4. Sets `CA-REQUEST-ID` to `'01UEND'`
5. Moves all updated fields to commarea
6. LINKs to `LGUPOL01`
7. On error, goes to `NO-UPD`
8. On success, displays `'Life Policy Updated'`

**Option OTHER (lines 218-229):**
Displays `'Please enter a valid option'` and repositions cursor.

#### ENDIT-STARTIT (line 239)
Pseudo-conversational return with `TRANSID('SSP2')` and `COMMAREA(COMM-AREA)`.

#### ENDIT (line 245)
Sends "Transaction ended" and returns.

#### CLEARIT (line 255)
Reinitializes map, sends empty, returns with `TRANSID('SSP2')`.

#### NO-ADD (line 268)
Handles add errors: return code 70 = `'Customer does not exist'`, other = `'Error Adding Life Policy'`.

#### NO-UPD, NO-DELETE, NO-DATA (lines 278-288)
Standard error message paragraphs.

#### ERROR-OUT (line 290)
Sends map with error, reinitializes, goes to `ENDIT-STARTIT`.

### Control Flow
```
First invocation: Init -> SEND MAP -> RETURN(SSP2)
Continuation:     RECEIVE MAP -> EVALUATE option
                    1: LINK LGIPOL01 (inquire) -> display -> RETURN(SSP2)
                    2: LINK LGAPOL01 (add) -> display result -> RETURN(SSP2)
                    3: LINK LGDPOL01 (delete) -> display result -> RETURN(SSP2)
                    4: LINK LGIPOL01 -> display -> RECEIVE -> LINK LGUPOL01 -> RETURN(SSP2)
                    Other: error -> RETURN(SSP2)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `MSGEND` | 18 | End-of-transaction message `'Transaction ended'` |
| `SSMAPP2I/O` (from SSMAP) | 21 | BMS map input/output areas for endowment policy screen |
| `COMM-AREA` (from LGCMAREA) | 22-23 | Full GenApp commarea |

### LINKAGE SECTION
None explicitly defined beyond CICS pseudo-conversational state management.

### Copybooks
| Copybook | Line | Purpose |
|---|---|---|
| `SSMAP` | 21 | BMS map definitions for SSMAPP2 (Endowment Policy menu screen layout) |
| `LGCMAREA` | 23 | Standard GenApp commarea with endowment fields (`CA-ENDOWMENT` redefine of `CA-POLICY-SPECIFIC`) |

### Condition Names
None defined.

## 4. Input/Output Behavior

### Input
- **BMS Map `SSMAPP2`**: User enters option and endowment policy fields:
  - `ENP2OPTO` - Menu option (1-4)
  - `ENP2CNOO/I` - Customer number
  - `ENP2PNOO/I` - Policy number
  - `ENP2IDAI` - Issue date
  - `ENP2EDAI` - Expiry date
  - `ENP2FNMI` - Fund name
  - `ENP2TERI` - Term (years)
  - `ENP2SUMI` - Sum assured
  - `ENP2LIFI` - Life assured
  - `ENP2WPRI` - With-profits flag
  - `ENP2MANI` - Managed-fund flag
  - `ENP2EQUI` - Equities flag

### Output
- **BMS Map `SSMAPP2`**: Displays endowment policy data and status messages via `ERP2FLDO`.

### Files Accessed
None directly (all data access via linked programs).

### Other Programs Called
| Program | Line | Purpose |
|---|---|---|
| `LGIPOL01` | 67, 159 | Inquire Policy - retrieves endowment policy data |
| `LGAPOL01` | 105 | Add Policy - inserts new endowment policy |
| `LGDPOL01` | 129 | Delete Policy - removes endowment policy |
| `LGUPOL01` | 198 | Update Policy - modifies endowment policy |

## 5. Algorithms and Techniques

### SQL Statements
None directly (SQL is in the linked programs).

### CICS Commands
| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS SEND MAP ('SSMAPP2')` | 42, 84, 119, 149, 176, 211, 224, 258, 291 | Display BMS map |
| `EXEC CICS RECEIVE MAP('SSMAPP2')` | 56, 180 | Receive user input |
| `EXEC CICS HANDLE AID CLEAR/PF3` | 49 | Set AID key handlers |
| `EXEC CICS HANDLE CONDITION MAPFAIL` | 52 | Handle map failure |
| `EXEC CICS LINK PROGRAM(...)` | 67, 105, 129, 159, 198 | Call back-end service programs |
| `EXEC CICS Syncpoint Rollback` | 110, 134 | Rollback on add/delete failure |
| `EXEC CICS RETURN TRANSID('SSP2')` | 240, 263 | Pseudo-conversational return |
| `EXEC CICS RETURN` | 236, 252 | Terminal return |
| `EXEC CICS SEND TEXT` | 246 | Send end-of-transaction message |

### Error Handling
- Return code checking after each LINK: `CA-RETURN-CODE > 0` triggers error paths.
- `Syncpoint Rollback` on add and delete failures.
- Specific return code 70 handling on add (`'Customer does not exist'`).
- Error messages displayed in `ERP2FLDO` (or `ERP1FLDO` in the `NO-ADD` paragraph -- see issues below).

### Notable Logic Patterns
- **Pseudo-conversational model**: Same pattern as LGTESTC1 and LGTESTP1 but with transaction ID `SSP2`.
- **Two-phase update** (option 4): Inquire first, display for editing, receive changes, then update. Same conversational concern as the other test programs.
- **No double send on delete**: Unlike LGTESTP1, the delete path correctly sends the map only once (line 149).
- **Endowment-specific fields**: Uses the `CA-E-*` fields from the LGCMAREA `CA-ENDOWMENT` redefine, including flags for with-profits, equities, and managed-fund.

## 6. Analysis

### Overall Role
LGTESTP2 is the terminal-based user interface for endowment (life) policy management in GenApp. It follows the same structural pattern as LGTESTP1 (motor) and LGTESTC1 (customer), sharing the same back-end policy programs but using endowment-specific request IDs and data fields. It forms part of the test transaction suite for exercising the GenApp application.

### Dependencies
- **BMS mapset `SSMAP`**: Must be installed with map `SSMAPP2`.
- **LGCMAREA copybook**: Defines the commarea with `CA-ENDOWMENT` redefine.
- **Back-end programs**: `LGIPOL01`, `LGAPOL01`, `LGDPOL01`, `LGUPOL01`.
- **Transaction `SSP2`**: Must be defined in CICS.

### Known Issues or Limitations
1. **Wrong error field in NO-ADD** (lines 271, 274): The `NO-ADD` paragraph writes to `ERP1FLDO` instead of `ERP2FLDO`. `ERP1FLDO` is the error field from the motor policy map (`SSMAPP1`), not the endowment map (`SSMAPP2`). This is a copy-paste bug from LGTESTP1 that could cause the error message to be written to the wrong map field or cause a runtime error.
2. **Option 4 is conversational**: The update option holds the task during user think-time, same issue as the sibling programs.
3. **No null-byte handling**: Does not perform `Inspect Replacing All x'00' by x'40'` like LGTESTC1 does.
4. **Terminology inconsistency**: Source comments say "Endowment Policy Menu" but user-facing messages say "Life Policy" (e.g., `'New Life Policy Inserted'`, `'Life Policy Updated'`). The data fields use the `CA-E-*` (Endowment) prefix.
5. **No WRITE-GENACNTL**: Unlike LGTESTC1, this program does not update the GENACNTL TSQ after adding policies, since policy adds don't affect customer numbering.
6. **Fund name displayed on add success** (line 115): After a successful add, `CA-E-FUND-NAME` is moved back to the map, but this is redundant since the value was set from the map input.

### Relationship to Other Programs
- **LGIPOL01**: Shared policy inquiry program.
- **LGAPOL01**: Shared policy add program.
- **LGDPOL01**: Shared policy delete program.
- **LGUPOL01**: Shared policy update program.
- **LGTESTC1**: Sibling customer menu program.
- **LGTESTP1**: Sibling motor policy menu program (structurally nearly identical).
- **LGASTAT1**: Captures events from the back-end policy programs (IEND/AEND/DEND/UEND counters).
