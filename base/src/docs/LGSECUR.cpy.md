# LGSECUR.cpy -- Security Configuration and Authentication Copybook

## 1. Overview

**File name:** `LGSECUR.cpy`
**Copyright:** IBM Corp. 2023
**Purpose:** Provides comprehensive security-related data structures for the GenApp application, covering security configuration, password policy, authentication request/response messaging, user security records, password history, and audit logging.

This copybook defines six level-01 structures that together implement a complete authentication and authorization subsystem. It covers the full lifecycle: security settings, password policy enforcement, authentication requests and responses, persistent user security records, password history tracking, and audit trail logging.

## 2. Data Structure Breakdown

### 2.1 SECURITY-CONFIG (Level 01, line 17)

Global security parameter settings.

| Level | Field Name | PIC Clause | VALUE | Purpose |
|-------|-----------|------------|-------|---------|
| 05 | SC-HASH-ALGORITHM | X(8) | 'SHA-256 ' | Hashing algorithm for passwords |
| 05 | SC-SALT-LENGTH | 9(2) | 32 | Length of cryptographic salt |
| 05 | SC-MIN-PASSWORD-LENGTH | 9(2) | 08 | Minimum password length |
| 05 | SC-MAX-PASSWORD-LENGTH | 9(2) | 32 | Maximum password length |
| 05 | SC-PASSWORD-EXPIRY-DAYS | 9(3) | 090 | Days until password expires |
| 05 | SC-MAX-LOGIN-ATTEMPTS | 9(2) | 03 | Max failed logins before lockout |
| 05 | SC-LOCKOUT-DURATION | 9(4) | 1800 | Lockout duration in seconds (30 min) |
| 05 | SC-SESSION-TIMEOUT | 9(4) | 3600 | Session timeout in seconds (1 hour) |
| 05 | SC-AUDIT-LEVEL | X(1) | 'A' | Audit level setting |

### 2.2 PASSWORD-POLICY (Level 01, line 34)

Password complexity requirements.

| Level | Field Name | PIC Clause | VALUE | Purpose |
|-------|-----------|------------|-------|---------|
| 05 | PP-REQUIRE-UPPER | X(1) | 'Y' | Require uppercase letters |
| 05 | PP-REQUIRE-LOWER | X(1) | 'Y' | Require lowercase letters |
| 05 | PP-REQUIRE-DIGIT | X(1) | 'Y' | Require numeric digits |
| 05 | PP-REQUIRE-SPECIAL | X(1) | 'N' | Require special characters |
| 05 | PP-HISTORY-COUNT | 9(2) | 05 | Number of previous passwords remembered |
| 05 | PP-COMPLEXITY-SCORE | 9(2) | 60 | Minimum complexity score (0-99) |

### 2.3 AUTH-REQUEST (Level 01, line 45)

Authentication request message structure.

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | AR-FUNCTION | X(8) | Operation code |
| 05 | AR-CUSTOMER-NUM | 9(10) | Customer number |
| 05 | AR-USERNAME | X(32) | Username |
| 05 | AR-PASSWORD | X(64) | Current password |
| 05 | AR-NEW-PASSWORD | X(64) | New password (for change operations) |
| 05 | AR-CLIENT-IP | X(15) | Client IP address |
| 05 | AR-USER-AGENT | X(100) | HTTP user agent string |
| 05 | AR-SESSION-ID | X(32) | Session identifier |

Total size: 325 bytes.

### 2.4 AUTH-RESPONSE (Level 01, line 63)

Authentication response message structure.

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | AS-RETURN-CODE | X(2) | Result code |
| 05 | AS-SESSION-TOKEN | X(64) | Session token for authenticated user |
| 05 | AS-EXPIRY-TIME | X(26) | Session expiry timestamp |
| 05 | AS-LAST-LOGIN | X(26) | Previous login timestamp |
| 05 | AS-ATTEMPTS-REMAINING | 9(2) | Remaining login attempts |
| 05 | AS-DAYS-TO-EXPIRY | 9(3) | Days until password expires |
| 05 | AS-ERROR-MESSAGE | X(100) | Human-readable error message |

Total size: 223 bytes.

### 2.5 USER-SECURITY-REC (Level 01, line 82)

Persistent user security record (maps to DB2 USER_SECURITY table).

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | USR-CUSTOMER-NUM | 9(10) | Customer number (primary key) |
| 05 | USR-USERNAME | X(32) | Unique username |
| 05 | USR-PASSWORD-HASH | X(128) | Hashed password |
| 05 | USR-SALT | X(32) | Cryptographic salt |
| 05 | USR-HASH-ALGORITHM | X(8) | Algorithm used for hashing |
| 05 | USR-PASSWORD-DATE | X(10) | Date password was last set |
| 05 | USR-LAST-LOGIN | X(26) | Last successful login timestamp |
| 05 | USR-LOGIN-ATTEMPTS | 9(2) | Current failed attempt count |
| 05 | USR-ACCOUNT-STATUS | X(1) | Account status code |
| 05 | USR-LOCKOUT-TIME | X(26) | When the account was locked |
| 05 | USR-CREATED-DATE | X(10) | Account creation date |
| 05 | USR-MODIFIED-DATE | X(10) | Last modification date |

Total size: 295 bytes.

### 2.6 PASSWORD-HISTORY (Level 01, line 103)

Password history record for preventing password reuse.

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | PH-CUSTOMER-NUM | 9(10) | Customer number |
| 05 | PH-SEQUENCE | 9(2) | Sequence number (1-10) |
| 05 | PH-PASSWORD-HASH | X(128) | Historical password hash |
| 05 | PH-CREATED-DATE | X(10) | Date this password was set |

Total size: 150 bytes.

### 2.7 AUDIT-LOG-REC (Level 01, line 112)

Audit trail record for security events.

| Level | Field Name | PIC Clause | Purpose |
|-------|-----------|------------|---------|
| 05 | AL-TIMESTAMP | X(26) | Event timestamp |
| 05 | AL-CUSTOMER-NUM | 9(10) | Customer number |
| 05 | AL-USERNAME | X(32) | Username involved |
| 05 | AL-ACTION | X(20) | Action performed |
| 05 | AL-RESULT | X(2) | Result code |
| 05 | AL-CLIENT-IP | X(15) | Client IP address |
| 05 | AL-USER-AGENT | X(100) | User agent string |
| 05 | AL-ERROR-CODE | X(10) | Error code if applicable |
| 05 | AL-DETAILS | X(200) | Additional details |

Total size: 415 bytes.

## 3. Key Components

### Condition Names (88-levels)

**SC-AUDIT-LEVEL conditions (line 27-29):**
| Condition | Value | Meaning |
|-----------|-------|---------|
| SC-AUDIT-ALL | 'A' | Log all authentication events |
| SC-AUDIT-FAIL | 'F' | Log only failed attempts |
| SC-AUDIT-NONE | 'N' | No audit logging |

**AR-FUNCTION conditions (lines 47-51):**
| Condition | Value | Meaning |
|-----------|-------|---------|
| AR-AUTHENTICATE | 'AUTH    ' | Standard authentication request |
| AR-CHANGE-PASSWORD | 'CHGPASS ' | Change password request |
| AR-CREATE-USER | 'CREATEU ' | Create new user account |
| AR-LOCK-USER | 'LOCKUSER' | Lock a user account |
| AR-UNLOCK-USER | 'UNLKUSER' | Unlock a user account |

**AS-RETURN-CODE conditions (lines 65-71):**
| Condition | Value | Meaning |
|-----------|-------|---------|
| AS-SUCCESS | '00' | Authentication successful |
| AS-INVALID-CREDS | '01' | Invalid credentials |
| AS-ACCOUNT-LOCKED | '02' | Account is locked |
| AS-PASSWORD-EXPIRED | '03' | Password has expired |
| AS-POLICY-VIOLATION | '04' | Password policy violation |
| AS-SYSTEM-ERROR | '90' | System error |
| AS-NOT-AUTHORIZED | '99' | Not authorized |

**USR-ACCOUNT-STATUS conditions (lines 92-95):**
| Condition | Value | Meaning |
|-----------|-------|---------|
| USR-ACTIVE | 'A' | Active account |
| USR-LOCKED | 'L' | Locked account |
| USR-SUSPENDED | 'S' | Suspended account |
| USR-EXPIRED | 'E' | Expired account |

### Default Configuration Values
The `SECURITY-CONFIG` and `PASSWORD-POLICY` structures are initialized with sensible defaults: SHA-256 hashing, 8-32 character passwords, 90-day expiry, 3 login attempts, 30-minute lockout, 1-hour session timeout, and mandatory uppercase/lowercase/digit requirements.

## 4. Usage Context

### Programs That COPY This File
- `LGAUTH01.cbl` -- Main authentication program
- `LGPWHASH.cbl` -- Password hashing utility
- `lgacdb01_secure.cbl` -- Secure customer add with authentication

### Section Placement
Included in **WORKING-STORAGE SECTION**. The structures are used both for in-memory configuration and as host variables for DB2 operations against the security tables.

### Data Flow
1. A calling program populates `AUTH-REQUEST` and links to `LGAUTH01`
2. `LGAUTH01` reads `USER-SECURITY-REC` from DB2, validates credentials
3. `LGAUTH01` returns `AUTH-RESPONSE` with success/failure
4. `AUDIT-LOG-REC` is written for every authentication event

## 5. Analysis

### Role in Architecture
This copybook is the foundation of the security subsystem added in 2023 to replace hardcoded credentials. It provides a modern authentication architecture with password hashing, account lockout, session management, and audit logging -- all within a mainframe COBOL/CICS context.

### Design Patterns
- **Configuration as data:** Security parameters are defined as initialized data structures rather than hard-coded values in procedural code.
- **Request/response messaging:** The `AUTH-REQUEST`/`AUTH-RESPONSE` pair implements a clean message-passing interface.
- **88-level state machines:** Account status and return codes use condition names for readable conditional logic.
- **Defense in depth:** Multiple security layers -- password complexity, expiry, lockout, audit logging, session management.

### Size Considerations
The structures are modest in size (largest is `AL-DETAILS` at 415 bytes), in contrast to the large COMMAREA structures. These are designed for targeted security operations rather than bulk data transfer.
