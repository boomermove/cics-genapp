# security_tables.sql -- Security Database Schema DDL

## 1. Overview

**File name:** `security_tables.sql`
**Copyright:** IBM Corp. 2023
**Purpose:** Contains the complete DB2 DDL (Data Definition Language) for the GenApp security subsystem. Defines five tables, four views, two stored procedures, index definitions, permission grants, and initial configuration data for user authentication, password management, session tracking, and audit logging.

This SQL script creates the database infrastructure that supports the COBOL security structures defined in `LGSECUR.cpy`. It is intended to be run once during initial setup or as part of a migration from the legacy hardcoded-credential system to the new secure authentication model.

## 2. Table Definitions

### 2.1 USER_SECURITY (lines 13-34)

Primary table for user authentication data. Replaces hardcoded credentials.

| Column | Type | Nullable | Default | Purpose |
|--------|------|----------|---------|---------|
| CUSTOMERNUM | INTEGER | NOT NULL | -- | Customer number (PK) |
| USERNAME | VARCHAR(32) | NOT NULL | -- | Unique login username |
| PASSWORD_HASH | VARCHAR(128) | NOT NULL | -- | Hashed password (never plaintext) |
| SALT | VARCHAR(32) | NOT NULL | -- | Cryptographic salt for hashing |
| HASH_ALGORITHM | VARCHAR(8) | NOT NULL | 'SHA256' | Algorithm used for hashing |
| PASSWORD_DATE | DATE | NOT NULL | CURRENT DATE | Date password was last changed |
| LAST_LOGIN | TIMESTAMP | NULL | -- | Last successful login time |
| LOGIN_ATTEMPTS | SMALLINT | NOT NULL | 0 | Failed login attempt counter |
| ACCOUNT_STATUS | CHAR(1) | NOT NULL | 'A' | Account state (A/L/S/E) |
| LOCKOUT_TIME | TIMESTAMP | NULL | -- | When account was locked |
| CREATED_DATE | DATE | NOT NULL | CURRENT DATE | Record creation date |
| MODIFIED_DATE | DATE | NOT NULL | CURRENT DATE | Last modification date |

**Constraints:**
- `PK_USER_SECURITY` -- PRIMARY KEY on CUSTOMERNUM (line 26)
- `UK_USERNAME` -- UNIQUE on USERNAME (line 27)
- `FK_USER_CUSTOMER` -- FOREIGN KEY referencing CUSTOMER(CUSTOMERNUMBER) (lines 28-29)
- `CK_ACCOUNT_STATUS` -- CHECK: ACCOUNT_STATUS IN ('A', 'L', 'S', 'E') (lines 30-31)
  - A = Active, L = Locked, S = Suspended, E = Expired
- `CK_LOGIN_ATTEMPTS` -- CHECK: LOGIN_ATTEMPTS >= 0 AND <= 99 (lines 32-33)

### 2.2 PASSWORD_HISTORY (lines 44-55)

Stores previous password hashes to prevent password reuse.

| Column | Type | Nullable | Default | Purpose |
|--------|------|----------|---------|---------|
| CUSTOMERNUM | INTEGER | NOT NULL | -- | Customer number |
| SEQUENCE | SMALLINT | NOT NULL | -- | History sequence (1-10) |
| PASSWORD_HASH | VARCHAR(128) | NOT NULL | -- | Historical password hash |
| CREATED_DATE | DATE | NOT NULL | CURRENT DATE | When this password was set |

**Constraints:**
- `PK_PASSWORD_HISTORY` -- PRIMARY KEY on (CUSTOMERNUM, SEQUENCE) (lines 49-50)
- `FK_PWHISTORY_USER` -- FOREIGN KEY referencing USER_SECURITY(CUSTOMERNUM) (lines 51-52)
- `CK_SEQUENCE` -- CHECK: SEQUENCE >= 1 AND <= 10 (lines 53-54)

### 2.3 AUDIT_LOG (lines 60-72)

Security event audit trail.

| Column | Type | Nullable | Default | Purpose |
|--------|------|----------|---------|---------|
| LOG_ID | INTEGER | NOT NULL | GENERATED ALWAYS AS IDENTITY | Auto-incrementing primary key |
| TIMESTAMP | TIMESTAMP | NOT NULL | CURRENT TIMESTAMP | Event timestamp |
| CUSTOMER_NUM | INTEGER | NULL | -- | Customer involved (nullable for system events) |
| USERNAME | VARCHAR(32) | NULL | -- | Username involved |
| ACTION | VARCHAR(20) | NOT NULL | -- | Action performed (e.g., 'AUTH', 'CHGPASS') |
| RESULT | CHAR(2) | NOT NULL | -- | Result code (e.g., '00' = success) |
| CLIENT_IP | VARCHAR(15) | NULL | -- | Client IP address |
| USER_AGENT | VARCHAR(100) | NULL | -- | HTTP user agent |
| ERROR_CODE | VARCHAR(10) | NULL | -- | Error code if applicable |
| DETAILS | VARCHAR(200) | NULL | -- | Additional detail text |

**Constraints:**
- `PK_AUDIT_LOG` -- PRIMARY KEY on LOG_ID (line 71)

### 2.4 SESSION_TOKENS (lines 83-98)

Active user session management.

| Column | Type | Nullable | Default | Purpose |
|--------|------|----------|---------|---------|
| SESSION_ID | VARCHAR(64) | NOT NULL | -- | Session token (PK) |
| CUSTOMERNUM | INTEGER | NOT NULL | -- | Owning customer |
| USERNAME | VARCHAR(32) | NOT NULL | -- | Username for the session |
| CREATED_TIME | TIMESTAMP | NOT NULL | CURRENT TIMESTAMP | Session creation time |
| EXPIRY_TIME | TIMESTAMP | NOT NULL | -- | Session expiration time |
| CLIENT_IP | VARCHAR(15) | NULL | -- | Client IP address |
| USER_AGENT | VARCHAR(100) | NULL | -- | HTTP user agent |
| LAST_ACTIVITY | TIMESTAMP | NOT NULL | CURRENT TIMESTAMP | Last activity timestamp |
| STATUS | CHAR(1) | NOT NULL | 'A' | Session status |

**Constraints:**
- `PK_SESSION_TOKENS` -- PRIMARY KEY on SESSION_ID (line 93)
- `FK_SESSION_USER` -- FOREIGN KEY referencing USER_SECURITY(CUSTOMERNUM) (lines 94-95)
- `CK_SESSION_STATUS` -- CHECK: STATUS IN ('A', 'E', 'I') (lines 96-97)
  - A = Active, E = Expired, I = Invalidated

### 2.5 SECURITY_CONFIG (lines 108-115)

Key-value store for configurable security parameters.

| Column | Type | Nullable | Default | Purpose |
|--------|------|----------|---------|---------|
| CONFIG_KEY | VARCHAR(50) | NOT NULL | -- | Configuration parameter name (PK) |
| CONFIG_VALUE | VARCHAR(200) | NOT NULL | -- | Parameter value |
| DESCRIPTION | VARCHAR(500) | NULL | -- | Human-readable description |
| MODIFIED_DATE | DATE | NOT NULL | CURRENT DATE | Last modification date |
| MODIFIED_BY | VARCHAR(32) | NOT NULL | USER | Who last modified it |

**Constraints:**
- `PK_SECURITY_CONFIG` -- PRIMARY KEY on CONFIG_KEY (line 114)

**Initial Data (lines 118-132):**

| CONFIG_KEY | CONFIG_VALUE | DESCRIPTION |
|------------|-------------|-------------|
| HASH_ALGORITHM | SHA256 | Default password hashing algorithm |
| MIN_PASSWORD_LENGTH | 8 | Minimum password length |
| MAX_PASSWORD_LENGTH | 32 | Maximum password length |
| PASSWORD_EXPIRY_DAYS | 90 | Password expiration in days |
| MAX_LOGIN_ATTEMPTS | 3 | Maximum failed login attempts |
| LOCKOUT_DURATION | 1800 | Account lockout duration in seconds |
| SESSION_TIMEOUT | 3600 | Session timeout in seconds |
| AUDIT_LEVEL | A | Audit level (A=All, F=Failures, N=None) |
| REQUIRE_UPPERCASE | Y | Require uppercase in password |
| REQUIRE_LOWERCASE | Y | Require lowercase in password |
| REQUIRE_DIGIT | Y | Require digit in password |
| REQUIRE_SPECIAL | N | Require special character in password |
| PASSWORD_HISTORY_COUNT | 5 | Number of previous passwords to remember |
| COMPLEXITY_SCORE | 60 | Minimum password complexity score |

## 3. Indexes

### USER_SECURITY Indexes (lines 37-39)
| Index Name | Table | Column(s) | Purpose |
|-----------|-------|-----------|---------|
| IX_USER_SECURITY_USERNAME | USER_SECURITY | USERNAME | Fast username lookup during authentication |
| IX_USER_SECURITY_STATUS | USER_SECURITY | ACCOUNT_STATUS | Filter by account status |
| IX_USER_SECURITY_MODIFIED | USER_SECURITY | MODIFIED_DATE | Query recently modified accounts |

### AUDIT_LOG Indexes (lines 75-78)
| Index Name | Table | Column(s) | Purpose |
|-----------|-------|-----------|---------|
| IX_AUDIT_TIMESTAMP | AUDIT_LOG | TIMESTAMP | Time-range queries on audit data |
| IX_AUDIT_CUSTOMER | AUDIT_LOG | CUSTOMER_NUM | Customer-specific audit lookup |
| IX_AUDIT_ACTION | AUDIT_LOG | ACTION | Filter by action type |
| IX_AUDIT_RESULT | AUDIT_LOG | RESULT | Filter by result code |

### SESSION_TOKENS Indexes (lines 101-103)
| Index Name | Table | Column(s) | Purpose |
|-----------|-------|-----------|---------|
| IX_SESSION_CUSTOMER | SESSION_TOKENS | CUSTOMERNUM | Find sessions by customer |
| IX_SESSION_EXPIRY | SESSION_TOKENS | EXPIRY_TIME | Identify expired sessions for cleanup |
| IX_SESSION_STATUS | SESSION_TOKENS | STATUS | Filter active/expired sessions |

## 4. Views

### ACTIVE_USERS (lines 139-143)
Selects CUSTOMERNUM, USERNAME, LAST_LOGIN, ACCOUNT_STATUS, LOGIN_ATTEMPTS, CREATED_DATE from USER_SECURITY where ACCOUNT_STATUS = 'A'. Shows all currently active user accounts.

### LOCKED_ACCOUNTS (lines 146-150)
Selects CUSTOMERNUM, USERNAME, LOCKOUT_TIME, LOGIN_ATTEMPTS, MODIFIED_DATE from USER_SECURITY where ACCOUNT_STATUS = 'L'. Shows all locked accounts for administrative review.

### RECENT_AUDIT (lines 153-158)
Selects LOG_ID, TIMESTAMP, CUSTOMER_NUM, USERNAME, ACTION, RESULT, CLIENT_IP, ERROR_CODE from AUDIT_LOG where TIMESTAMP >= CURRENT TIMESTAMP - 24 HOURS, ordered by TIMESTAMP DESC. Shows the last 24 hours of security events.

### ACTIVE_SESSIONS (lines 161-165)
Selects SESSION_ID, CUSTOMERNUM, USERNAME, CREATED_TIME, EXPIRY_TIME, LAST_ACTIVITY, CLIENT_IP from SESSION_TOKENS where STATUS = 'A' AND EXPIRY_TIME > CURRENT TIMESTAMP. Shows currently valid sessions.

## 5. Stored Procedures

### CLEANUP_EXPIRED_SESSIONS (lines 172-181)
- **Parameters:** None
- **Language:** SQL
- **Logic:** Deletes from SESSION_TOKENS where EXPIRY_TIME < CURRENT TIMESTAMP OR STATUS = 'E'. Then inserts an audit log entry recording the cleanup event with CUSTOMER_NUM=0, USERNAME='SYSTEM', ACTION='CLEANUP_SESSIONS', RESULT='00'.
- **Purpose:** Housekeeping procedure to remove stale session records.

### RESET_LOGIN_ATTEMPTS (lines 184-194)
- **Parameters:** `p_customernum INTEGER` (IN)
- **Language:** SQL
- **Logic:** Updates USER_SECURITY to set LOGIN_ATTEMPTS = 0 and MODIFIED_DATE = CURRENT DATE for the specified customer. Then inserts an audit log entry with ACTION='RESET_ATTEMPTS', RESULT='00'.
- **Purpose:** Administrative procedure to unlock a customer's account by resetting the failed login counter.

## 6. Permissions and Security

### GRANT Statements (lines 197-205)

| Table/View | Grantee | Permissions |
|------------|---------|-------------|
| USER_SECURITY | GENAPP | SELECT, INSERT, UPDATE |
| PASSWORD_HISTORY | GENAPP | SELECT, INSERT |
| AUDIT_LOG | GENAPP | INSERT |
| SESSION_TOKENS | GENAPP | SELECT, INSERT, UPDATE, DELETE |
| SECURITY_CONFIG | GENAPP | SELECT |
| ACTIVE_USERS | GENAPP | SELECT |
| LOCKED_ACCOUNTS | GENAPP | SELECT |
| RECENT_AUDIT | GENAPP | SELECT |
| ACTIVE_SESSIONS | GENAPP | SELECT |

**Security Notes:**
- The `GENAPP` role has **INSERT-only** access to AUDIT_LOG -- application code cannot modify or delete audit records, ensuring audit integrity.
- PASSWORD_HISTORY has no UPDATE or DELETE -- historical records are immutable.
- SECURITY_CONFIG is read-only for the application -- configuration changes require DBA access.
- SESSION_TOKENS has full CRUD -- the application needs to create, read, update, and delete session records.

### Table Comments (lines 208-217)
Each table has a `COMMENT ON TABLE` statement documenting its purpose.

## 7. Entity Relationships

```
CUSTOMER (existing table)
    |
    |  FK_USER_CUSTOMER (CUSTOMERNUM -> CUSTOMERNUMBER)
    v
USER_SECURITY (1:1 with CUSTOMER)
    |
    |-- FK_PWHISTORY_USER (CUSTOMERNUM)
    |   v
    |   PASSWORD_HISTORY (1:N, max 10 per user)
    |
    |-- FK_SESSION_USER (CUSTOMERNUM)
        v
        SESSION_TOKENS (1:N, multiple active sessions)

AUDIT_LOG (standalone, no foreign keys to preserve logging independence)

SECURITY_CONFIG (standalone key-value configuration store)
```

- **USER_SECURITY** has a 1:1 relationship with the existing **CUSTOMER** table via CUSTOMERNUM.
- **PASSWORD_HISTORY** has a many-to-one relationship with **USER_SECURITY** (up to 10 entries per customer, enforced by CK_SEQUENCE).
- **SESSION_TOKENS** has a many-to-one relationship with **USER_SECURITY** (a user can have multiple concurrent sessions).
- **AUDIT_LOG** is intentionally independent -- it uses no foreign keys so that audit records are never affected by cascading deletes or data cleanup.
- **SECURITY_CONFIG** is a standalone configuration table with no relationships.

The script ends with `COMMIT` (line 219) to finalize all DDL and DML operations.
