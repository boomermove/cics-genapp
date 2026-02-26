# CICS GenApp - Complete Project Analysis

## 1. What This Project Does

**CICS GenApp** (General Insurance Application) is an IBM sample application that simulates a **general insurance company's policy management system** running on IBM z/OS mainframe using CICS Transaction Server and DB2. It manages **customers** and their **insurance policies** across four product lines:

| Policy Type | Code | Description |
|---|---|---|
| **Motor** | M | Vehicle insurance (make, model, registration, accidents) |
| **Endowment** | E | Life/investment insurance (fund names, sum assured, profits) |
| **House** | H | Property insurance (bedrooms, value, property type) |
| **Commercial** | B | Business property insurance (perils: fire, crime, flood, weather) |

Users interact via **3270 terminal screens** (BMS maps) or **web services** (SOA/SOAP).

---

## 2. Architecture - Three-Tier Design

```
┌─────────────────────────────────────────────────────────────────┐
│                     PRESENTATION LAYER                          │
│  3270 Screens (ssmap.bms)          Web Services (SOA/SOAP)      │
│  ┌──────────┐ ┌──────────┐        ┌──────────────────────┐     │
│  │LGTESTC1  │ │LGTESTP1-4│        │ WSDL/Pipeline (JCL)  │     │
│  │Customer  │ │Policy    │        │ wsaac01, wsaip01, etc│     │
│  │  Menu    │ │  Menus   │        └──────────────────────┘     │
│  └────┬─────┘ └────┬─────┘                                     │
└───────┼─────────────┼──────────────────────────────────────────┘
        │             │
┌───────┼─────────────┼──────────────────────────────────────────┐
│       ▼      BUSINESS LOGIC LAYER                              │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐             │
│  │LGICUS01 │ │LGACUS01 │ │LGUCUS01 │ │         │  Customer   │
│  │Inquire  │ │Add      │ │Update   │ │         │  Operations │
│  └────┬────┘ └────┬────┘ └────┬────┘ └─────────┘             │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐             │
│  │LGIPOL01 │ │LGAPOL01 │ │LGUPOL01 │ │LGDPOL01 │  Policy     │
│  │Inquire  │ │Add      │ │Update   │ │Delete   │  Operations │
│  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘             │
└───────┼──────────┼──────────┼──────────┼───────────────────────┘
        │          │          │          │
┌───────┼──────────┼──────────┼──────────┼───────────────────────┐
│       ▼    DATA ACCESS LAYER ▼          ▼                      │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐             │
│  │LGICDB01 │ │LGACDB01 │ │LGUCDB01 │ │LGDPDB01 │  DB2 Layer  │
│  │LGIPDB01 │ │LGAPDB01 │ │LGUPDB01 │ │         │             │
│  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘             │
│       │          │          │          │                       │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐             │
│  │LGICVS01 │ │LGACVS01 │ │LGUCVS01 │ │LGDPVS01 │  VSAM Layer │
│  │LGIPVS01 │ │LGAPVS01 │ │LGUPVS01 │ │         │             │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘             │
└────────────────────────────────────────────────────────────────┘
        │                                    │
   ┌────▼────┐                          ┌────▼────┐
   │  DB2    │                          │  VSAM   │
   │ Tables  │                          │  KSDS   │
   └─────────┘                          └─────────┘
```

### Supporting Programs

| Program | Purpose |
|---|---|
| **LGAUTH01** | Authentication service (login, password change, account lock/unlock) |
| **LGPWHASH** | Password hashing via z/OS ICSF (SHA-256 + salt) |
| **LGACDB02** | Inserts customer security record into CUSTOMER_SECURE |
| **LGSTSQ** | Centralized error logging to TSQ/TDQ |
| **LGSETUP** | Environment initialization (counters, queues) |
| **LGASTAT1** | Debug/startup statistics collector |
| **LGWEBST5** | Business monitor statistics writer |

---

## 3. Program Call Flows

### Customer Inquiry Flow
```
LGTESTC1 (screen) ──LINK──► LGICUS01 (logic) ──LINK──► LGICDB01 (DB2 SELECT)
```

### Customer Add Flow
```
LGTESTC1 (screen) ──LINK──► LGACUS01 (logic) ──LINK──► LGACDB01 (DB2 INSERT)
                                                            ├──LINK──► LGACVS01 (VSAM WRITE)
                                                            └──LINK──► LGACDB02 (security INSERT)
```

### Customer Update Flow
```
LGTESTC1 (screen) ──LINK──► LGUCUS01 (logic) ──LINK──► LGUCDB01 (DB2 UPDATE)
                                                            └──LINK──► LGUCVS01 (VSAM REWRITE)
```

### Policy Inquiry Flow
```
LGTESTP[1-4] (screen) ──LINK──► LGIPOL01 (logic) ──LINK──► LGIPDB01 (DB2 SELECT + cursor)
```

### Policy Add Flow
```
LGTESTP[1-4] (screen) ──LINK──► LGAPOL01 (logic) ──LINK──► LGAPDB01 (DB2 INSERT)
                                                                └──LINK──► LGAPVS01 (VSAM WRITE)
```

### Policy Update Flow
```
LGTESTP[1-4] (screen) ──LINK──► LGUPOL01 (logic) ──LINK──► LGUPDB01 (DB2 UPDATE)
                                                                └──LINK──► LGUPVS01 (VSAM REWRITE)
```

### Policy Delete Flow
```
LGTESTP[1-4] (screen) ──LINK──► LGDPOL01 (logic) ──LINK──► LGDPDB01 (DB2 DELETE)
                                                                └──LINK──► LGDPVS01 (VSAM DELETE)
```

### Authentication Flow (Secure version)
```
LGACDB01_SECURE ──LINK──► LGAUTH01 ──LINK──► LGPWHASH (ICSF SHA-256)
                              │                    ├── CSNBRNGL (random salt)
                              │                    └── CSNBOWH  (hash)
                              ├── DB2: USER_SECURITY
                              ├── DB2: PASSWORD_HISTORY
                              └── DB2: AUDIT_LOG
```

---

## 4. Inter-Program Communication

All programs communicate via a **COMMAREA** (Communication Area) defined in `lgcmarea.cpy`. The key routing field is:

| Field | Description |
|---|---|
| `CA-REQUEST-ID` (PIC X(6)) | First 2 chars = operation code, determines which program to call |

Request ID prefixes:

| Prefix | Operation |
|---|---|
| `01` | Inquire Customer |
| `02` | Add Customer |
| `04` | Update Customer |
| `01` + policy type | Inquire Policy |
| `02` + policy type | Add Policy |
| `03` + policy type | Delete Policy |
| `04` + policy type | Update Policy |

---

## 5. Data Dictionaries

### 5.1 DB2 Tables

#### CUSTOMER Table
| Column | Type | Description |
|---|---|---|
| CUSTOMERNUMBER | INTEGER (auto) | Primary key, generated via named counter |
| FIRSTNAME | CHAR(10) | Customer first name |
| LASTNAME | CHAR(20) | Customer last name |
| DATEOFBIRTH | CHAR(10) | Date of birth (yyyy-mm-dd) |
| HOUSENAME | CHAR(20) | House/building name |
| HOUSENUMBER | CHAR(4) | House number |
| POSTCODE | CHAR(8) | Postal code |
| PHONEMOBILE | CHAR(20) | Mobile phone number |
| PHONEHOME | CHAR(20) | Home phone number |
| EMAILADDRESS | CHAR(100) | Email address |

#### CUSTOMER_SECURE Table
| Column | Type | Description |
|---|---|---|
| CUSTOMERNUMBER | INTEGER | FK to CUSTOMER |
| PASSWORD | CHAR(32) | Customer password (legacy) |
| STATE | CHAR(1) | Account state |

#### POLICY Table
| Column | Type | Description |
|---|---|---|
| CUSTOMERNUMBER | INTEGER | FK to CUSTOMER |
| POLICYNUMBER | INTEGER (identity) | Auto-generated policy ID |
| POLICYTYPE | CHAR(1) | E=Endowment, H=House, M=Motor, C=Commercial |
| ISSUEDATE | CHAR(10) | Policy issue date |
| EXPIRYDATE | CHAR(10) | Policy expiry date |
| LASTCHANGED | CHAR(26) | Timestamp of last modification |
| BROKERID | INTEGER | Broker identifier |
| BROKERSREF | CHAR(10) | Broker's reference |
| PAYMENT | INTEGER | Payment amount |

#### ENDOWMENT Table
| Column | Type | Description |
|---|---|---|
| POLICYNUMBER | INTEGER | FK to POLICY |
| WITHPROFITS | CHAR(1) | With profits flag (Y/N) |
| EQUITIES | CHAR(1) | Equities flag (Y/N) |
| MANAGEDFUND | CHAR(1) | Managed fund flag (Y/N) |
| FUNDNAME | CHAR(10) | Fund name |
| TERM | SMALLINT | Term in years |
| SUMASSURED | INTEGER | Sum assured amount |
| LIFEASSURED | CHAR(31) | Name of life assured |

#### HOUSE Table
| Column | Type | Description |
|---|---|---|
| POLICYNUMBER | INTEGER | FK to POLICY |
| PROPERTYTYPE | CHAR(15) | Type of property |
| BEDROOMS | SMALLINT | Number of bedrooms |
| VALUE | INTEGER | Property value |
| HOUSENAME | CHAR(20) | House name |
| HOUSENUMBER | CHAR(4) | House number |
| POSTCODE | CHAR(8) | Postal code |

#### MOTOR Table
| Column | Type | Description |
|---|---|---|
| POLICYNUMBER | INTEGER | FK to POLICY |
| MAKE | CHAR(15) | Vehicle make |
| MODEL | CHAR(15) | Vehicle model |
| VALUE | INTEGER | Vehicle value |
| REGNUMBER | CHAR(7) | Registration number |
| COLOUR | CHAR(8) | Vehicle colour |
| CC | SMALLINT | Engine capacity (cc) |
| MANUFACTURED | CHAR(10) | Manufacture date |
| PREMIUM | INTEGER | Insurance premium |
| ACCIDENTS | INTEGER | Number of accidents |

#### COMMERCIAL Table
| Column | Type | Description |
|---|---|---|
| POLICYNUMBER | INTEGER | FK to POLICY |
| ADDRESS | VARCHAR(255) | Business address |
| POSTCODE | CHAR(8) | Postal code |
| LATITUDE | CHAR(11) | GPS latitude |
| LONGITUDE | CHAR(11) | GPS longitude |
| CUSTOMER | VARCHAR(255) | Customer/business name |
| PROPTYPE | VARCHAR(255) | Property type |
| FIREPERIL | SMALLINT | Fire peril rating |
| FIREPREMIUM | INTEGER | Fire premium |
| CRIMEPERIL | SMALLINT | Crime peril rating |
| CRIMEPREMIUM | INTEGER | Crime premium |
| FLOODPERIL | SMALLINT | Flood peril rating |
| FLOODPREMIUM | INTEGER | Flood premium |
| WEATHERPERIL | SMALLINT | Weather peril rating |
| WEATHERPREMIUM | INTEGER | Weather premium |
| STATUS | SMALLINT | Policy status code |
| REJECTREASON | VARCHAR(255) | Rejection reason |

#### USER_SECURITY Table (Security)
| Column | Type | Description |
|---|---|---|
| CUSTOMERNUM | INTEGER PK | FK to CUSTOMER |
| USERNAME | VARCHAR(32) UNIQUE | Login username |
| PASSWORD_HASH | VARCHAR(128) | SHA-256 hashed password |
| SALT | VARCHAR(32) | Random salt for hashing |
| HASH_ALGORITHM | VARCHAR(8) | Algorithm used (default SHA256) |
| PASSWORD_DATE | DATE | When password was last set |
| LAST_LOGIN | TIMESTAMP | Last successful login |
| LOGIN_ATTEMPTS | SMALLINT | Failed login count (0-99) |
| ACCOUNT_STATUS | CHAR(1) | A=Active, L=Locked, S=Suspended, E=Expired |
| LOCKOUT_TIME | TIMESTAMP | When account was locked |

#### PASSWORD_HISTORY Table
| Column | Type | Description |
|---|---|---|
| CUSTOMERNUM | INTEGER | FK to USER_SECURITY |
| SEQUENCE | SMALLINT (1-10) | History sequence number |
| PASSWORD_HASH | VARCHAR(128) | Previous password hash |
| CREATED_DATE | DATE | When this password was set |

#### AUDIT_LOG Table
| Column | Type | Description |
|---|---|---|
| LOG_ID | INTEGER (identity) | Auto-generated PK |
| TIMESTAMP | TIMESTAMP | Event timestamp |
| CUSTOMER_NUM | INTEGER | Customer involved |
| USERNAME | VARCHAR(32) | Username involved |
| ACTION | VARCHAR(20) | Action performed (AUTH, CHGPASS, etc.) |
| RESULT | CHAR(2) | 00=Success, 01=Invalid creds, 02=Locked, etc. |
| CLIENT_IP | VARCHAR(15) | Client IP address |
| USER_AGENT | VARCHAR(100) | Client user agent |
| ERROR_CODE | VARCHAR(10) | Error code if failed |
| DETAILS | VARCHAR(200) | Additional detail text |

#### SESSION_TOKENS Table
| Column | Type | Description |
|---|---|---|
| SESSION_ID | VARCHAR(64) PK | Session token |
| CUSTOMERNUM | INTEGER | FK to USER_SECURITY |
| USERNAME | VARCHAR(32) | Username |
| CREATED_TIME | TIMESTAMP | Session start |
| EXPIRY_TIME | TIMESTAMP | Session expiry |
| CLIENT_IP | VARCHAR(15) | Client IP |
| LAST_ACTIVITY | TIMESTAMP | Last activity timestamp |
| STATUS | CHAR(1) | A=Active, E=Expired, I=Invalid |

#### SECURITY_CONFIG Table
| Column | Type | Description |
|---|---|---|
| CONFIG_KEY | VARCHAR(50) PK | Configuration parameter name |
| CONFIG_VALUE | VARCHAR(200) | Configuration value |
| DESCRIPTION | VARCHAR(500) | Human-readable description |

### 5.2 VSAM Datasets

| Dataset | File ID | Key | Record Size | Description |
|---|---|---|---|---|
| **KSDSCUST** | Customer KSDS | Customer Number (10 bytes) | 225 bytes | Customer records mirror |
| **KSDSPOLY** | Policy KSDS | Customer# + Policy# (21 bytes) | 64 bytes | Policy records mirror |

### 5.3 COMMAREA Structure (lgcmarea.cpy)

The COMMAREA is a ~32,500 byte area shared between all programs:

| Offset | Field | PIC | Description |
|---|---|---|---|
| 0 | CA-REQUEST-ID | X(6) | Operation routing code |
| 6 | CA-RETURN-CODE | 9(2) | 00=OK, other=error |
| 8 | CA-CUSTOMER-NUM | 9(10) | Customer number |
| 18 | CA-REQUEST-SPECIFIC | X(32482) | Varies by operation (see below) |

**For Customer requests**, CA-REQUEST-SPECIFIC maps to:

| Offset | Field | PIC | Description |
|---|---|---|---|
| +0 | CA-FIRST-NAME | X(10) | First name |
| +10 | CA-LAST-NAME | X(20) | Last name |
| +30 | CA-DOB | X(10) | Date of birth |
| +40 | CA-HOUSE-NAME | X(20) | House name |
| +60 | CA-HOUSE-NUM | X(4) | House number |
| +64 | CA-POSTCODE | X(8) | Postal code |
| +72 | CA-NUM-POLICIES | 9(3) | Number of policies |
| +75 | CA-PHONE-MOBILE | X(20) | Mobile phone |
| +95 | CA-PHONE-HOME | X(20) | Home phone |
| +115 | CA-EMAIL-ADDRESS | X(100) | Email |

**For Policy requests**, CA-REQUEST-SPECIFIC maps to:

| Offset | Field | PIC | Description |
|---|---|---|---|
| +0 | CA-POLICY-NUM | 9(10) | Policy number |
| +10 | CA-ISSUE-DATE | X(10) | Issue date |
| +20 | CA-EXPIRY-DATE | X(10) | Expiry date |
| +30 | CA-LASTCHANGED | X(26) | Last change timestamp |
| +56 | CA-BROKERID | 9(10) | Broker ID |
| +66 | CA-BROKERSREF | X(10) | Broker reference |
| +76 | CA-PAYMENT | 9(6) | Payment amount |
| +82 | CA-POLICY-SPECIFIC | X(32400) | Type-specific data |

---

## 6. Copybook Dependency Map

```
LGCMAREA.cpy ◄── Used by all business logic and data access programs
LGPOLICY.cpy ◄── Used by DB2 data access programs (lengths + DB2 host vars)
LGSECUR.cpy  ◄── Used by LGAUTH01, LGPWHASH, LGACDB01_SECURE
LGERR.cpy    ◄── Used by ~20 programs (error working storage)
LGERRPRC.cpy ◄── Used by ~20 programs (error handling procedures)
SSMAP (BMS)  ◄── Used by LGTESTC1, LGTESTP1-4 (screen I/O)
SOA*.cpy     ◄── Used by web service pipeline programs
```

---

## 7. BMS Screen Map Summary

The application defines **6 screens** in `ssmap.bms`:

| Map | ID | Title | Operations |
|---|---|---|---|
| **SSMAPC1** | SSC1 | Customer Menu | Inquiry, Add, Update |
| **SSMAPP1** | SSP1 | Motor Policy Menu | Inquiry, Add, Delete, Update |
| **SSMAPP2** | SSP2 | Endowment Policy Menu | Inquiry, Add, Delete, Update |
| **SSMAPP3** | SSP3 | House Policy Menu | Inquiry, Add, Delete, Update |
| **SSMAPP4** | SSP4 | Commercial Policy Menu | Inquiry, Add, Delete (no Update) |
| **SSMAPP5** | SSP5 | Claim Menu | Inquiry, Add |

---

## 8. Error Handling

All programs use a centralized error handling pattern via two shared copybooks:

1. **LGERR.cpy** - Defines `ERROR-MSG` working storage and `WS-ERR-LOG-PGM` = `'LGSTSQ'`
2. **LGERRPRC.cpy** - Provides paragraphs:
   - `LGERR-FORMAT-TIME` - Gets and formats current timestamp
   - `LGERR-WRITE-MSG` - Writes error message to TSQ via `EXEC CICS LINK PROGRAM('LGSTSQ')`
   - `LGERR-LOG-COMMAREA` - Logs first 90 bytes of COMMAREA for debugging

Error messages are written to **Temporary Storage Queues** (TSQs) and **Transient Data Queue** CSMT (CICS system log).

---

## 9. Security Architecture

The security subsystem (added post-initial development) provides:

- **SHA-256 password hashing** via z/OS ICSF cryptographic services
- **Random salt generation** per user (32 bytes)
- **Account lockout** after 3 failed attempts (configurable, 30-minute duration)
- **Password expiry** after 90 days
- **Password history** (prevents reuse of last 5 passwords)
- **Audit logging** of all authentication events
- **Session management** with configurable timeouts

---

## 10. Complete Program Inventory

| Program | PROGRAM-ID | Layer | CICS Commands | DB2 SQL | Calls |
|---|---|---|---|---|---|
| LGTESTC1 | LGTESTC1 | Presentation | SEND/RECEIVE MAP, LINK | None | LGICUS01, LGACUS01, LGUCUS01 |
| LGTESTP1 | LGTESTP1 | Presentation | SEND/RECEIVE MAP, LINK | None | LGIPOL01, LGAPOL01, LGDPOL01, LGUPOL01 |
| LGTESTP2 | LGTESTP2 | Presentation | SEND/RECEIVE MAP, LINK | None | LGIPOL01, LGAPOL01, LGDPOL01, LGUPOL01 |
| LGTESTP3 | LGTESTP3 | Presentation | SEND/RECEIVE MAP, LINK | None | LGIPOL01, LGAPOL01, LGDPOL01, LGUPOL01 |
| LGTESTP4 | LGTESTP4 | Presentation | SEND/RECEIVE MAP, LINK | None | LGIPOL01, LGAPOL01, LGDPOL01 |
| LGICUS01 | LGICUS01 | Business Logic | LINK | None | LGICDB01 |
| LGACUS01 | LGACUS01 | Business Logic | LINK | None | LGACDB01 |
| LGUCUS01 | LGUCUS01 | Business Logic | LINK | None | LGUCDB01 |
| LGIPOL01 | LGIPOL01 | Business Logic | LINK | None | LGIPDB01 |
| LGAPOL01 | LGAPOL01 | Business Logic | LINK | None | LGAPDB01 |
| LGUPOL01 | LGUPOL01 | Business Logic | LINK | None | LGUPDB01 |
| LGDPOL01 | LGDPOL01 | Business Logic | LINK | None | LGDPDB01 |
| LGICDB01 | LGICDB01 | Data Access | None | SELECT CUSTOMER | None |
| LGACDB01 | LGACDB01 | Data Access | LINK | INSERT CUSTOMER | LGACVS01, LGACDB02 |
| LGUCDB01 | LGUCDB01 | Data Access | LINK | UPDATE CUSTOMER | LGUCVS01 |
| LGIPDB01 | LGIPDB01 | Data Access | None | SELECT POLICY+type tables | None |
| LGAPDB01 | LGAPDB01 | Data Access | LINK | INSERT POLICY+type tables | LGAPVS01 |
| LGUPDB01 | LGUPDB01 | Data Access | LINK | UPDATE POLICY+type tables | LGUPVS01 |
| LGDPDB01 | LGDPDB01 | Data Access | LINK | DELETE POLICY | LGDPVS01 |
| LGACDB02 | LGACDB02 | Data Access | None | INSERT CUSTOMER_SECURE | None |
| LGICVS01 | LGICVS01 | Data Access | READ FILE | None | None |
| LGACVS01 | LGACVS01 | Data Access | WRITE FILE | None | None |
| LGUCVS01 | LGUCVS01 | Data Access | READ/REWRITE FILE | None | None |
| LGIPVS01 | LGIPVS01 | Data Access | READ FILE | None | None |
| LGAPVS01 | LGAPVS01 | Data Access | WRITE FILE | None | None |
| LGUPVS01 | LGUPVS01 | Data Access | READ/REWRITE FILE | None | None |
| LGDPVS01 | LGDPVS01 | Data Access | DELETE FILE | None | None |
| LGAUTH01 | LGAUTH01 | Security | LINK | SELECT/UPDATE/INSERT multiple | LGPWHASH, LGSTSQ |
| LGPWHASH | LGPWHASH | Security | LINK | None | CSNBRNGL, CSNBOWH (ICSF) |
| LGACDB01_SECURE | LGACDB01 | Data Access | LINK, ENQ/DEQ | INSERT CUSTOMER | LGACVS01, LGAUTH01, LGSTSQ |
| LGSTSQ | LGSTSQ | Utility | WRITEQ TS/TD | None | None |
| LGSETUP | LGSETUP | Utility | DELETEQ TS, DEFINE COUNTER | None | None |
| LGASTAT1 | LGASTAT1 | Utility | ASSIGN, WRITEQ TS | None | None |
| LGWEBST5 | LGWEBST5 | Utility | READQ/WRITEQ TS, GET COUNTER | None | None |
