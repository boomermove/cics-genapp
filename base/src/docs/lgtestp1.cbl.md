# lgtestp1.cbl - Motor Policy Menu Transaction

## 1. Overview

- **Program ID:** LGTESTP1
- **Purpose:** Provides a BMS terminal menu for performing Motor Policy CRUD operations: Inquire, Add, Delete, and Update.

LGTESTP1 is a CICS pseudo-conversational terminal program that presents the `SSMAPP1` map to the user and handles motor policy inquiry (option 1), add (option 2), delete (option 3), and update (option 4) operations. It communicates with back-end service programs (`LGIPOL01`, `LGAPOL01`, `LGDPOL01`, `LGUPOL01`) via `EXEC CICS LINK`, passing data through the standard GenApp commarea (`LGCMAREA`). The transaction uses pseudo-conversational return with transaction ID `SSP1`.

## 2. Functional Breakdown

### MAINLINE SECTION (line 30)

**Lines 32-33 - Continuation Check:**
If `EIBCALEN > 0`, this is a pseudo-conversational continuation; skip to `A-GAIN`.

**Lines 35-50 - First Invocation:**
Initializes map input/output areas (`SSMAPP1I`, `SSMAPP1O`) and `COMM-AREA`. Sets default values for numeric fields:
- `ENP1CNOO` = `'0000000000'` (customer number)
- `ENP1PNOO` = `'0000000000'` (policy number)
- `ENP1VALO` = `'000000'` (vehicle value)
- `ENP1CCO` = `'00000'` (engine CC)
- `ENP1ACCO` = `'000000'` (accidents)
- `ENP1PREO` = `'000000'` (premium)

Sends the `SSMAPP1` map with `ERASE`.

#### A-GAIN (line 52)
Sets up AID key handling (CLEAR -> `CLEARIT`, PF3 -> `ENDIT`, MAPFAIL -> `ENDIT`). Receives map input.

**Lines 66-249 - Option Dispatch (EVALUATE ENP1OPTO):**

**Option '1' - Inquire Motor Policy (lines 68-95):**
1. Sets `CA-REQUEST-ID` to `'01IMOT'`
2. Moves customer and policy numbers from map to commarea
3. LINKs to `LGIPOL01` with 32,500-byte commarea
4. If `CA-RETURN-CODE > 0`, goes to `NO-DATA`
5. Populates map with policy details: issue date, expiry date, make, model, value, registration number, colour, CC, manufactured date, premium, accidents
6. Sends map and goes to `ENDIT-STARTIT`

**Option '2' - Add Motor Policy (lines 97-133):**
1. Sets `CA-REQUEST-ID` to `'01AMOT'`
2. Moves customer number, payment (0), broker ID (0), and all motor-specific fields from map to commarea
3. LINKs to `LGAPOL01`
4. On error, performs `Syncpoint Rollback` and goes to `NO-ADD`
5. On success, displays new customer/policy numbers and `'New Motor Policy Inserted'`

**Option '3' - Delete Motor Policy (lines 135-167):**
1. Sets `CA-REQUEST-ID` to `'01DMOT'`
2. Moves customer and policy numbers to commarea
3. LINKs to `LGDPOL01`
4. On error, performs `Syncpoint Rollback` and goes to `NO-DELETE`
5. On success, clears all policy fields and displays `'Motor Policy Deleted'`
6. Note: The map is sent twice (lines 159-166), which appears to be a bug

**Option '4' - Update Motor Policy (lines 169-234):**
1. First retrieves current data by LINKing to `LGIPOL01` (inquiry)
2. Populates and sends the map with current values
3. Receives the modified map back from the user
4. Sets `CA-REQUEST-ID` to `'01UMOT'`
5. Moves all updated fields from map to commarea
6. LINKs to `LGUPOL01`
7. On error, goes to `NO-UPD`
8. On success, displays `'Motor Policy Updated'`

**Option OTHER (lines 236-247):**
Displays `'Please enter a valid option'` and repositions cursor.

#### ENDIT-STARTIT (line 257)
Pseudo-conversational return with `TRANSID('SSP1')` and `COMMAREA(COMM-AREA)`.

#### ENDIT (line 263)
Sends "Transaction ended" and returns without transaction ID.

#### CLEARIT (line 273)
Reinitializes map, sends empty, returns with `TRANSID('SSP1')`.

#### NO-ADD (line 286)
Handles add errors with specific message for return code 70 (`'Customer does not exist'`).

#### NO-UPD, NO-DELETE, NO-DATA (lines 296-306)
Standard error message paragraphs.

#### ERROR-OUT (line 308)
Sends map with error, reinitializes areas, goes to `ENDIT-STARTIT`.

### Control Flow
```
First invocation: Init -> SEND MAP -> RETURN(SSP1)
Continuation:     RECEIVE MAP -> EVALUATE option
                    1: LINK LGIPOL01 (inquire) -> display -> RETURN(SSP1)
                    2: LINK LGAPOL01 (add) -> display result -> RETURN(SSP1)
                    3: LINK LGDPOL01 (delete) -> display result -> RETURN(SSP1)
                    4: LINK LGIPOL01 -> display -> RECEIVE -> LINK LGUPOL01 -> RETURN(SSP1)
                    Other: error -> RETURN(SSP1)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `MSGEND` | 18 | End-of-transaction message `'Transaction ended'` |
| `SSMAPP1I/O` (from SSMAP) | 21 | BMS map input/output areas for motor policy screen |
| `COMM-AREA` (from LGCMAREA) | 22-23 | Full GenApp commarea |

### LINKAGE SECTION
None explicitly defined beyond what CICS provides for pseudo-conversational state.

### Copybooks
| Copybook | Line | Purpose |
|---|---|---|
| `SSMAP` | 21 | BMS map definitions for SSMAPP1 (Motor Policy menu screen layout) |
| `LGCMAREA` | 23 | Standard GenApp commarea with customer, motor policy, and other data structures |

### Condition Names
None defined.

## 4. Input/Output Behavior

### Input
- **BMS Map `SSMAPP1`**: User enters option and motor policy fields:
  - `ENP1OPTO` - Menu option (1-4)
  - `ENP1CNOO/I` - Customer number
  - `ENP1PNOO/I` - Policy number
  - `ENP1IDAI` - Issue date
  - `ENP1EDAI` - Expiry date
  - `ENP1CMKI` - Vehicle make
  - `ENP1CMOI` - Vehicle model
  - `ENP1VALI` - Vehicle value
  - `ENP1REGI` - Registration number
  - `ENP1COLI` - Colour
  - `ENP1CCI` - Engine CC
  - `ENP1MANI` - Manufacture date
  - `ENP1PREI` - Premium
  - `ENP1ACCI` - Number of accidents

### Output
- **BMS Map `SSMAPP1`**: Displays motor policy data and status messages via `ERP1FLDO`.
- **Commarea**: Passed to back-end programs with populated fields.

### Files Accessed
None directly (all data access via linked programs).

### Other Programs Called
| Program | Line | Purpose |
|---|---|---|
| `LGIPOL01` | 72, 173 | Inquire Policy - retrieves policy data (used for inquire and pre-update fetch) |
| `LGAPOL01` | 115 | Add Policy - inserts new motor policy |
| `LGDPOL01` | 139 | Delete Policy - removes motor policy |
| `LGUPOL01` | 216 | Update Policy - modifies motor policy |

## 5. Algorithms and Techniques

### SQL Statements
None directly (SQL is in the linked programs).

### CICS Commands
| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS SEND MAP ('SSMAPP1')` | 47, 91, 129, 159, 163, 192, 229, 242, 276, 309 | Display BMS map |
| `EXEC CICS RECEIVE MAP('SSMAPP1')` | 61, 196 | Receive user input |
| `EXEC CICS HANDLE AID CLEAR/PF3` | 54 | Set AID key handlers |
| `EXEC CICS HANDLE CONDITION MAPFAIL` | 57 | Handle map failure |
| `EXEC CICS LINK PROGRAM(...)` | 72, 115, 139, 173, 216 | Call back-end service programs |
| `EXEC CICS Syncpoint Rollback` | 120, 144 | Rollback on add/delete failure |
| `EXEC CICS RETURN TRANSID('SSP1')` | 258, 281 | Pseudo-conversational return |
| `EXEC CICS RETURN` | 254, 270 | Terminal return |
| `EXEC CICS SEND TEXT` | 264 | Send end-of-transaction message |

### Error Handling
- Return code checking after each LINK: `CA-RETURN-CODE > 0` triggers error paths.
- `Syncpoint Rollback` on add and delete failures.
- Specific error handling for return code 70 on add (`'Customer does not exist'`).
- Error messages displayed in `ERP1FLDO` field.
- All areas reinitialized after errors before returning to the menu.

### Notable Logic Patterns
- **Pseudo-conversational model**: Same pattern as LGTESTC1 but with transaction ID `SSP1`.
- **Two-phase update** (option 4): Inquire first, display for editing, receive changes, then update.
- **Double map send on delete** (lines 159-166): The map is sent twice after a successful delete. This appears to be a copy-paste bug.
- **Return code 70 special case** (lines 287-294): For add operations, return code 70 specifically indicates the customer does not exist (the policy cannot be created for a non-existent customer).
- **Payment and broker defaults**: On add, `CA-PAYMENT` and `CA-BROKERID` are set to 0, and `CA-BROKERSREF` to spaces (lines 100-102, 202-204).

## 6. Analysis

### Overall Role
LGTESTP1 is the terminal-based user interface for motor policy management in GenApp. It is one of several parallel test menu programs (LGTESTC1 for customers, LGTESTP1 for motor, LGTESTP2 for endowment, plus house and commercial variants). It serves as the presentation/controller layer, delegating all data access to shared back-end policy programs.

### Dependencies
- **BMS mapset `SSMAP`**: Must be installed with map `SSMAPP1`.
- **LGCMAREA copybook**: Defines the commarea structure including `CA-MOTOR` fields.
- **Back-end programs**: `LGIPOL01`, `LGAPOL01`, `LGDPOL01`, `LGUPOL01`.
- **Transaction `SSP1`**: Must be defined in CICS.

### Known Issues or Limitations
1. **Double SEND MAP on delete** (lines 159-166): The map is sent twice after a successful delete. The second send is redundant and likely a copy-paste error.
2. **Option 4 is conversational**: Like LGTESTC1, the update option holds the task during user think-time, which is not truly pseudo-conversational.
3. **No null-byte handling**: Unlike LGTESTC1, this program does not perform `Inspect COMM-AREA Replacing All x'00' by x'40'`. Unkeyed BMS fields may contain nulls that could cause issues in the back-end programs.
4. **No postcode uppercasing**: Unlike LGTESTC1, there is no normalization of input fields.
5. **Policy programs are generic**: `LGIPOL01`, `LGAPOL01`, `LGDPOL01`, `LGUPOL01` are shared across all policy types. The `CA-REQUEST-ID` (e.g., `'01IMOT'`) tells the back-end which policy type to process.
6. **Premium and accidents fields cleared on delete** (line 156 missing): The delete success path clears most fields to spaces but does not explicitly clear `ENP1PREO` and `ENP1ACCO`.

### Relationship to Other Programs
- **LGIPOL01**: Shared policy inquiry program (also used by LGTESTP2, etc.).
- **LGAPOL01**: Shared policy add program.
- **LGDPOL01**: Shared policy delete program.
- **LGUPOL01**: Shared policy update program.
- **LGTESTC1**: Sibling customer menu program.
- **LGTESTP2**: Sibling endowment policy menu program.
- **LGASTAT1**: Captures events from the back-end policy programs.
