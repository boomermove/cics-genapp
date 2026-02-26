# lgtestc1.cbl - Customer Menu Transaction

## 1. Overview

- **Program ID:** LGTESTC1
- **Purpose:** Provides a BMS (Basic Mapping Support) terminal menu for performing Customer CRUD operations: Inquire, Add, and Update.

LGTESTC1 is a CICS pseudo-conversational terminal program that presents the `SSMAPC1` map to the user and handles customer inquiry (option 1), customer add (option 2), and customer update (option 4) operations. It communicates with back-end service programs (`LGICUS01`, `LGACUS01`, `LGUCUS01`) via `EXEC CICS LINK`, passing data through the standard GenApp commarea (`LGCMAREA`). After each successful add operation, it updates the `GENACNTL` TSQ with the new high customer number. The transaction uses the pseudo-conversational model, returning with transaction ID `SSC1` and a commarea to maintain state between user interactions.

## 2. Functional Breakdown

### MAINLINE SECTION (line 53)

**Lines 55-56 - Continuation Check:**
If `EIBCALEN > 0`, this is a continuation (not first invocation), so skip initialization and go directly to `A-GAIN`.

**Lines 58-68 - First Invocation:**
Initializes the map input/output areas (`SSMAPC1I`, `SSMAPC1O`) and `COMM-AREA`. Sets customer number output field `ENT1CNOO` to zeros. Sends the `SSMAPC1` map with `ERASE` to display the initial menu.

#### A-GAIN (line 70)
Sets up AID key handling:
- CLEAR key -> `CLEARIT`
- PF3 -> `ENDIT`
- MAPFAIL condition -> `ENDIT`

Receives the map input from the terminal.

**Lines 84-222 - Option Dispatch (EVALUATE ENT1OPTO):**

**Option '1' - Inquire Customer (lines 86-111):**
1. Sets `CA-REQUEST-ID` to `'01ICUS'`
2. Moves customer number from map to `CA-CUSTOMER-NUM`
3. LINKs to `LGICUS01` with 32,500-byte commarea
4. If `CA-RETURN-CODE > 0`, goes to `NO-DATA`
5. On success, populates map output fields with customer data (first name, last name, DOB, address, phone, email)
6. Sends the updated map and goes to `ENDIT-STARTIT`

**Option '2' - Add Customer (lines 113-146):**
1. Sets `CA-REQUEST-ID` to `'01ACUS'`, customer number to 0
2. Moves all input fields from map to commarea
3. Replaces null bytes (x'00') with spaces (x'40') in commarea
4. Uppercases the postcode
5. LINKs to `LGACUS01` with 32,500-byte commarea
6. If error, performs `Syncpoint Rollback` and goes to `NO-ADD`
7. On success, performs `WRITE-GENACNTL` to update the control TSQ
8. Displays the new customer number and success message

**Option '4' - Update Customer (lines 148-207):**
1. First performs an inquiry (LINKs to `LGICUS01`) to retrieve current data
2. Populates and sends the map with current values
3. Receives the modified map back from the user
4. Sets `CA-REQUEST-ID` to `'01UCUS'`
5. Moves all updated fields from map to commarea
6. Replaces nulls with spaces, uppercases postcode
7. LINKs to `LGUCUS01` with 32,500-byte commarea
8. If error, goes to `NO-UPD`
9. On success, displays confirmation message

**Option OTHER (lines 209-220):**
Displays `'Please enter a valid option'` error message and repositions cursor.

#### ENDIT-STARTIT (line 230)
Returns to CICS with `TRANSID('SSC1')` and `COMMAREA(COMM-AREA)` for pseudo-conversational continuation.

#### ENDIT (line 236)
Sends "Transaction ended" message and returns without a transaction ID (ending the conversation).

#### CLEARIT (line 246)
Reinitializes the map and sends it empty, then returns with `TRANSID('SSC1')` to continue.

#### NO-UPD, NO-ADD, NO-DATA (lines 259-269)
Error message paragraphs that set appropriate messages in `ERRFLDO` and go to `ERROR-OUT`.

#### ERROR-OUT (line 271)
Sends the map with the error message, reinitializes all areas, and goes to `ENDIT-STARTIT`.

#### WRITE-GENACNTL (line 283)
Updates the `GENACNTL` TSQ with the new highest customer number:
1. Acquires an ENQ lock on `STSQ-NAME` for serialization
2. Reads the TSQ sequentially, looking for the `'HIGH CUSTOMER'` record
3. If found, rewrites it with the new customer number
4. If the TSQ does not exist, creates it from scratch with header, low, and high customer records
5. Releases the ENQ lock

### Control Flow
```
First invocation: Init maps -> SEND MAP -> RETURN(SSC1)
Continuation:     RECEIVE MAP -> EVALUATE option
                    1: LINK LGICUS01 -> display data -> RETURN(SSC1)
                    2: LINK LGACUS01 -> WRITE-GENACNTL -> RETURN(SSC1)
                    4: LINK LGICUS01 -> display -> RECEIVE -> LINK LGUCUS01 -> RETURN(SSC1)
                    Other: error msg -> RETURN(SSC1)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `WS-RESP` | 18 | CICS response code |
| `WS-Item-Count` | 19 | TSQ item counter for GENACNTL traversal |
| `WS-FLAG-TSQH` | 20 | Flag indicating whether HIGH CUSTOMER record was found/updated |
| `READ-MSG` / REDEFINES | 21-25 | Buffer for reading GENACNTL TSQ, with `READ-CUST-HIGH` at offset 14 |
| `WS-Cust-High` | 27 | Customer high number work field (not directly used) |
| `WRITE-MSG` | 30-36 | TSQ write buffer with header, low/high customer fields |
| `STSQ` / `STSQ-NAME` | 37-38 | TSQ name `'GENACNTL'` |
| `F24` | 40 | Constant 24 for WRITEQ TS length |
| `MSGEND` | 41 | End of transaction message |
| `SSMAPC1I/O` (from SSMAP) | 44 | BMS map input/output areas |
| `COMM-AREA` (from LGCMAREA) | 45-46 | Full GenApp commarea |

### LINKAGE SECTION
None explicitly defined; `DFHCOMMAREA` is implicitly available for pseudo-conversational state passing.

### Copybooks
| Copybook | Line | Purpose |
|---|---|---|
| `SSMAP` | 44 | BMS map definitions for SSMAPC1 (Customer menu screen layout) |
| `LGCMAREA` | 46 | Standard GenApp commarea with customer and policy data structures |

### Condition Names
None explicitly defined. The LGCMAREA copybook does not define 88-levels.

## 4. Input/Output Behavior

### Input
- **BMS Map `SSMAPC1`**: User enters option number and customer data fields:
  - `ENT1OPTO` - Menu option (1=Inquire, 2=Add, 4=Update)
  - `ENT1CNOO/I` - Customer number (output/input formats)
  - `ENT1FNAI` - First name
  - `ENT1LNAI` - Last name
  - `ENT1DOBI` - Date of birth
  - `ENT1HNMI` - House name
  - `ENT1HNOI` - House number
  - `ENT1HPCI` - Postcode
  - `ENT1HP1I` - Home phone
  - `ENT1HP2I` - Mobile phone
  - `ENT1HMOI` - Email address

### Output
- **BMS Map `SSMAPC1`**: Displays customer data and status messages via `ERRFLDO`.
- **GENACNTL TSQ**: Updated with new high customer number after adds.

### Files Accessed
| Resource | Type | Operation |
|---|---|---|
| `GENACNTL` | CICS TSQ | ReadQ, WriteQ, ReWrite (via WRITE-GENACNTL) |

### Other Programs Called
| Program | Line | Purpose |
|---|---|---|
| `LGICUS01` | 89, 151 | Inquire Customer - retrieves customer data from DB2 |
| `LGACUS01` | 128 | Add Customer - inserts new customer into DB2 |
| `LGUCUS01` | 190 | Update Customer - updates customer record in DB2 |

## 5. Algorithms and Techniques

### SQL Statements
None directly (SQL is in the linked programs).

### CICS Commands
| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS SEND MAP ('SSMAPC1')` | 64, 107, 142, 168, 203, 215, 249, 272 | Display BMS map |
| `EXEC CICS RECEIVE MAP('SSMAPC1')` | 79, 172 | Receive user input from BMS map |
| `EXEC CICS HANDLE AID CLEAR/PF3` | 72 | Set AID key handlers |
| `EXEC CICS HANDLE CONDITION MAPFAIL` | 75 | Handle map failure |
| `EXEC CICS LINK PROGRAM(...)` | 89, 128, 151, 190 | Call back-end service programs |
| `EXEC CICS Syncpoint Rollback` | 133 | Rollback on add failure |
| `EXEC CICS RETURN TRANSID('SSC1')` | 231, 255 | Pseudo-conversational return |
| `EXEC CICS RETURN` | 227, 243 | Terminal return (end conversation) |
| `EXEC CICS SEND TEXT` | 237 | Send end-of-transaction message |
| `EXEC CICS ENQ/DEQ Resource` | 285, 343 | Serialize GENACNTL TSQ access |
| `EXEC CICS ReadQ TS` | 290, 297 | Read GENACNTL TSQ items |
| `EXEC CICS WriteQ TS` | 307, 321, 329, 335 | Write/rewrite GENACNTL records |

### Error Handling
- Back-end program return codes are checked: `CA-RETURN-CODE > 0` triggers error paths.
- `Syncpoint Rollback` is issued on add failures to undo any partial database changes.
- Error messages are displayed in the `ERRFLDO` field on the map.
- The `WRITE-GENACNTL` paragraph uses `EXEC CICS ENQ/DEQ` for serialization to prevent concurrent update conflicts.

### Notable Logic Patterns
- **Pseudo-conversational model**: The program returns to CICS after each interaction with `TRANSID('SSC1')` and a commarea, allowing CICS to free resources between user think-time. On re-entry, `EIBCALEN > 0` indicates continuation.
- **Two-phase update** (option 4): First retrieves current data (inquiry), displays it for editing, receives changes, then performs the update. This is a conversational send-receive-send within one pseudo-conversational cycle.
- **Null byte replacement** (lines 125, 187): `Inspect COMM-AREA Replacing All x'00' by x'40'` converts null bytes to spaces, handling BMS fields that were not entered by the user.
- **Postcode uppercasing** (lines 126-127, 188-189): `Move Function UPPER-CASE(CA-POSTCODE) TO CA-POSTCODE` ensures consistent storage.
- **ENQ/DEQ serialization** (WRITE-GENACNTL): Uses CICS resource-level locking to prevent concurrent TSQ updates.

## 6. Analysis

### Overall Role
LGTESTC1 is the primary terminal-based user interface for customer management in GenApp. It serves as the presentation layer in a three-tier architecture: BMS screen (presentation) -> LGTESTC1 (controller) -> LGICUS01/LGACUS01/LGUCUS01 (data access). It is the "C" in the test transaction suite (C=Customer, P1=Motor Policy, P2=Endowment Policy, P3=House Policy, P4=Commercial Policy).

### Dependencies
- **BMS mapset `SSMAP`**: Must be installed with map `SSMAPC1`.
- **LGCMAREA copybook**: Defines the commarea structure.
- **Back-end programs**: `LGICUS01`, `LGACUS01`, `LGUCUS01` must be available.
- **GENACNTL TSQ**: Should exist (created by LGSETUP), though the program can create it if absent.
- **Transaction `SSC1`**: Must be defined in CICS to enable pseudo-conversational return.

### Known Issues or Limitations
1. **No option 3 (Delete Customer)**: The EVALUATE handles options 1, 2, and 4 but not 3. Customer delete functionality is missing from this menu.
2. **Option 4 is conversational within pseudo-conversational**: The update option performs a SEND MAP, then a RECEIVE MAP within the same task. This means the task holds resources during user think-time for the update, which is not truly pseudo-conversational.
3. **GENACNTL TSQ race condition**: Despite ENQ/DEQ, the logic that creates the TSQ from scratch if the HIGH CUSTOMER item is not found could conflict with other concurrent add operations.
4. **32,500-byte commarea**: The LINK commarea length of 32,500 is large and fixed for all operations, even simple inquiries. This is inefficient but matches the LGCMAREA copybook size.
5. **Missing SSMAP copybook**: The `COPY SSMAP` copybook was not found in the repository, suggesting it may be generated from a BMS map source or located elsewhere.
6. **Error in NO-ADD for return code 70**: The add failure path does not check the specific error condition as thoroughly as `LGTESTP1` does.

### Relationship to Other Programs
- **LGICUS01**: Inquire Customer service program (called for options 1 and 4).
- **LGACUS01**: Add Customer service program (called for option 2).
- **LGUCUS01**: Update Customer service program (called for option 4).
- **LGSETUP**: Creates the GENACNTL TSQ and GENACUSTNUM counter.
- **LGTESTP1/P2**: Sibling test menu programs for Motor and Endowment policies.
- **LGASTAT1**: Captures events from the back-end programs called by LGTESTC1.
