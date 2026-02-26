# LGPWHASH.cbl - Password Hashing Utility

## 1. Overview

- **Program ID:** LGPWHASH
- **Author:** GENAPP SECURITY TEAM
- **Date Written:** 2023
- **Purpose:** Provides secure password hashing and validation services for the GenApp authentication subsystem.

LGPWHASH is a CICS-based utility program that offers four cryptographic functions: hashing a password with a salt using SHA-256 (via z/OS ICSF), verifying a password against a stored hash, generating a cryptographically secure salt using ICSF random number generation, and validating a password against a configurable password policy. It is designed to be called via `EXEC CICS LINK` from other GenApp programs that need authentication services. The program relies on the z/OS Integrated Cryptographic Service Facility (ICSF) callable services CSNBOWH (One-Way Hash Generate) and CSNBRNGL (Random Number Generate Long) for its cryptographic operations, ensuring compliance with NIST FIPS 180-4.

## 2. Functional Breakdown

### MAINLINE SECTION (line 177)

#### 0000-MAIN (line 179)
Entry point. Calls `1000-INITIALIZE`, then `2000-PROCESS-REQUEST`, then `9000-RETURN`. This is a straightforward sequential orchestration.

#### 1000-INITIALIZE (line 185)
Captures CICS environment information (transaction ID, terminal ID, task number from EIB fields). Validates that the commarea length (`EIBCALEN`) is at least as large as `DFHCOMMAREA`. If not, sets return code `'98'` with error message `'Invalid commarea length'` and short-circuits directly to `9000-RETURN` via `GO TO`. Copies `COMM-FUNCTION` to `WS-FUNCTION`.

#### 2000-PROCESS-REQUEST (line 199)
Dispatches to the appropriate function handler using an `EVALUATE` on `WS-FUNCTION`:
- `'HASH    '` --> `3000-HASH-PASSWORD`
- `'VERIFY  '` --> `4000-VERIFY-PASSWORD`
- `'GENSALT '` --> `5000-GENERATE-SALT`
- `'VALIDATE'` --> `6000-VALIDATE-PASSWORD`
- `OTHER` --> Sets return code `'99'` with `'Invalid function requested'`

#### 3000-HASH-PASSWORD (line 218)
Hashes a password with a salt. If `COMM-SALT` is spaces, it auto-generates a salt by performing `5000-GENERATE-SALT`. Concatenates `COMM-PASSWORD` and `COMM-SALT` (delimited by space) into `WS-INPUT-STRING`, computes the combined length using `FUNCTION TRIM`, then calls `7000-COMPUTE-HASH`. The resulting hex hash is moved to `COMM-HASH`.

#### 4000-VERIFY-PASSWORD (line 240)
Verifies a password against a stored hash. Concatenates `COMM-PASSWORD` and `COMM-SALT` into `WS-INPUT-STRING`, computes the hash via `7000-COMPUTE-HASH`, then compares `WS-HASH-OUTPUT` with `COMM-HASH`. If they match, return code is `'00'`; otherwise `'01'` with message `'Password verification failed'`.

#### 5000-GENERATE-SALT (line 265)
Generates a 32-byte cryptographic salt using the ICSF CSNBRNGL callable service. Initializes the ICSF random number generation parameters, then issues `CALL 'CSNBRNGL'`. On failure (non-zero `ICSF-RNG-RC`), sets return code `'90'` and logs the error. On success, calls `5100-CONVERT-SALT-TO-HEX`.

#### 5100-CONVERT-SALT-TO-HEX (line 300)
Converts the first 16 bytes of binary random output from ICSF into a 32-character hexadecimal string stored in `COMM-SALT`. Iterates byte-by-byte using `FUNCTION ORD` to get the ordinal value, splits into high and low nibbles via `DIVIDE BY 16`, and looks up hex characters from `WS-HEX-TABLE`.

#### 6000-VALIDATE-PASSWORD (line 329)
Validates a password against the configurable password policy defined in the `LGSECUR` copybook. Checks minimum length (`SC-MIN-PASSWORD-LENGTH`, default 8) and maximum length (`SC-MAX-PASSWORD-LENGTH`, default 32). Then iterates through each character via `6100-ANALYZE-CHARACTER` to check for uppercase, lowercase, digits, and special characters. Validates presence of required character classes (`PP-REQUIRE-UPPER`, `PP-REQUIRE-LOWER`, `PP-REQUIRE-DIGIT`) and checks the accumulated complexity score against `PP-COMPLEXITY-SCORE` (default 60). Returns `'04'` for any policy violation.

#### 6100-ANALYZE-CHARACTER (line 381)
Classifies a single character using an `EVALUATE TRUE` structure:
- Uppercase A-Z: sets `WS-HAS-UPPER`, adds 4 to complexity score
- Lowercase a-z: sets `WS-HAS-LOWER`, adds 2 to complexity score
- Digits 0-9: sets `WS-HAS-DIGIT`, adds 3 to complexity score
- Other (special): sets `WS-HAS-SPECIAL`, adds 6 to complexity score

#### 7000-COMPUTE-HASH (line 403)
Computes a SHA-256 hash using the ICSF CSNBOWH callable service. Sets up the ICSF parameters (rule array `'SHA-256 '` and `'ONLY    '`), copies the input text and length, then issues `CALL 'CSNBOWH'`. On failure, sets return code `'90'` and logs the error. On success, calls `7100-CONVERT-HASH-TO-HEX`.

#### 7100-CONVERT-HASH-TO-HEX (line 445)
Converts the 32-byte binary SHA-256 hash into a 64-character hexadecimal string in `WS-HASH-OUTPUT`. Uses the same nibble-splitting and hex-table lookup technique as `5100-CONVERT-SALT-TO-HEX`, but iterates over 32 bytes instead of 16.

#### WRITE-ERROR-MESSAGE (line 474)
Formats a timestamped error message using `EXEC CICS ASKTIME` and `EXEC CICS FORMATTIME`, populates the `ERROR-MSG` structure, and calls the `LGSTSQ` program via `EXEC CICS LINK` to write the error to a TSQ.

#### 9000-RETURN (line 500)
Copies `WS-RETURN-CODE` and `WS-ERROR-MSG` to the commarea fields `COMM-RETURN-CODE` and `COMM-ERROR-MSG`, then issues `EXEC CICS RETURN`.

### Control Flow Summary
```
Entry -> 1000-INITIALIZE -> 2000-PROCESS-REQUEST -> [dispatch] -> 9000-RETURN
                                                      |
                            HASH -> 3000 -> (5000 if no salt) -> 7000 -> 7100
                            VERIFY -> 4000 -> 7000 -> 7100
                            GENSALT -> 5000 -> 5100
                            VALIDATE -> 6000 -> 6100 (loop)
```

## 3. Key Components and Logic

### WORKING-STORAGE Variables
| Variable | Line | Role |
|---|---|---|
| `WS-HEADER` (01) | 39 | Eyecatcher and CICS environment fields (transaction, terminal, task) |
| `WS-ABSTIME`, `WS-DATE`, `WS-TIME` | 51-54 | Time/date fields for error message formatting |
| `WS-WORK-VARS` | 59 | Working function code, return code (init `'00'`), error message, counters |
| `WS-PASSWORD-WORK` | 72 | Password validation tracking: length, character class flags (`WS-HAS-UPPER`, etc.), complexity score |
| `WS-HASH-WORK` | 85 | Hash generation: input string (200 chars), input length, hash output (128 chars), hex table |
| `ICSF-HASH-PARMS` | 98 | CSNBOWH interface: RC, RS, rule array (`SHA-256`, `ONLY`), text, chain data, hash value (32 bytes) |
| `ICSF-RANDOM-PARMS` | 118 | CSNBRNGL interface: RC, RS, rule (`RANDOM`), seed, output (32 bytes) |
| `WS-HEX-CONVERSION` | 132 | Byte-to-hex conversion work fields: byte value, nibbles, hex pair, index |
| `ERROR-MSG` | 142 | Formatted error message structure with date, time, program name, codes |

### LINKAGE SECTION Structures
- **LGSECUR copybook** (line 161): Provides `SECURITY-CONFIG`, `PASSWORD-POLICY`, `AUTH-REQUEST`, `AUTH-RESPONSE`, `USER-SECURITY-REC`, `PASSWORD-HISTORY`, and `AUDIT-LOG-REC` structures. LGPWHASH uses `SC-MIN-PASSWORD-LENGTH`, `SC-MAX-PASSWORD-LENGTH`, `PP-REQUIRE-UPPER`, `PP-REQUIRE-LOWER`, `PP-REQUIRE-DIGIT`, and `PP-COMPLEXITY-SCORE` from this copybook.
- **DFHCOMMAREA** (line 163): Custom 932-byte commarea with `COMM-FUNCTION` (8), `COMM-PASSWORD` (64), `COMM-SALT` (32), `COMM-HASH` (128), `COMM-RETURN-CODE` (2), `COMM-ERROR-MSG` (100), and `COMM-FILLER` (598).

### Copybooks
| Copybook | Line | Purpose |
|---|---|---|
| `LGSECUR` | 161 | Security configuration, password policy rules, authentication structures |

### Condition Names (88-levels)
From the LGSECUR copybook:
- `SC-AUDIT-ALL` / `SC-AUDIT-FAIL` / `SC-AUDIT-NONE` (lines 27-29 of LGSECUR): Audit level settings
- `AR-AUTHENTICATE`, `AR-CHANGE-PASSWORD`, etc.: Authentication request function codes
- `AS-SUCCESS` (`'00'`), `AS-INVALID-CREDS` (`'01'`), `AS-ACCOUNT-LOCKED` (`'02'`), `AS-PASSWORD-EXPIRED` (`'03'`), `AS-POLICY-VIOLATION` (`'04'`), `AS-SYSTEM-ERROR` (`'90'`), `AS-NOT-AUTHORIZED` (`'99'`): Response codes
- `USR-ACTIVE`, `USR-LOCKED`, `USR-SUSPENDED`, `USR-EXPIRED`: Account status values

## 4. Input/Output Behavior

### Input (via DFHCOMMAREA)
| Field | PIC | Description |
|---|---|---|
| `COMM-FUNCTION` | X(8) | Function to execute: `'HASH'`, `'VERIFY'`, `'GENSALT'`, `'VALIDATE'` |
| `COMM-PASSWORD` | X(64) | Password to hash, verify, or validate |
| `COMM-SALT` | X(32) | Salt for hashing/verification (spaces triggers auto-generation for HASH) |
| `COMM-HASH` | X(128) | For VERIFY: the stored hash to compare against |

### Output (via DFHCOMMAREA)
| Field | PIC | Description |
|---|---|---|
| `COMM-RETURN-CODE` | X(2) | `'00'`=success, `'01'`=verify failed, `'04'`=policy violation, `'90'`=ICSF error, `'98'`=bad commarea, `'99'`=invalid function |
| `COMM-ERROR-MSG` | X(100) | Descriptive error message when non-zero return |
| `COMM-SALT` | X(32) | Generated salt (for HASH with no salt, or GENSALT) |
| `COMM-HASH` | X(128) | Computed hash (for HASH function) |

### Files Accessed
None directly. All cryptographic operations use ICSF callable services.

### Other Programs Called
| Program | Line | Purpose |
|---|---|---|
| `LGSTSQ` | 490 | Error logging - writes formatted error messages to a TSQ via `EXEC CICS LINK` |

## 5. Algorithms and Techniques

### SQL Statements
None.

### ICSF Callable Services
1. **CSNBOWH** (One-Way Hash Generate, line 413): Computes SHA-256 hash. Rule array: `'SHA-256 '` + `'ONLY    '`. Processes concatenated password+salt text. Returns 32-byte binary hash.
2. **CSNBRNGL** (Random Number Generate Long, line 271): Generates 32 bytes of cryptographically secure random data. Rule: `'RANDOM  '`. Used for salt generation.

### CICS Commands
| Command | Line | Purpose |
|---|---|---|
| `EXEC CICS ASKTIME` | 477 | Get absolute time for error timestamps |
| `EXEC CICS FORMATTIME` | 478 | Format time as MM/DD/YY and HH:MM:SS |
| `EXEC CICS LINK Program('LGSTSQ')` | 490 | Write error messages to TSQ |
| `EXEC CICS RETURN` | 504 | Return control to calling program |

### Error Handling
- Commarea length validation at entry (return code `'98'`)
- Invalid function detection (return code `'99'`)
- ICSF return code checking after every CALL, with return code `'90'` and descriptive STRING-built error messages
- Errors are logged to the LGSTSQ program which writes to a TSQ

### Notable Logic Patterns
- **Binary-to-hex conversion** (paragraphs 5100 and 7100): Uses `FUNCTION ORD` to get byte ordinal, `DIVIDE BY 16` to split into high/low nibbles, then table lookup from `WS-HEX-TABLE` (`'0123456789ABCDEF'`). The +1/-1 adjustments account for COBOL's 1-based `ORD` function.
- **Auto-salt generation**: The HASH function automatically generates a salt if `COMM-SALT` is spaces (line 219-221).
- **EVALUATE TRUE for character classification** (line 382): Classifies characters by range comparison and assigns weighted complexity scores (uppercase=4, lowercase=2, digit=3, special=6).

## 6. Analysis

### Overall Role
LGPWHASH is a security utility service in the GenApp architecture. It provides the cryptographic foundation for user authentication by offering password hashing, verification, salt generation, and policy validation as discrete, callable functions. It acts as a server-side service invoked via `EXEC CICS LINK` by authentication-related programs.

### Dependencies
- **z/OS ICSF**: Requires ICSF to be active and configured for CSNBOWH and CSNBRNGL. Without ICSF, all hashing and salt generation will fail.
- **LGSECUR copybook**: Provides password policy configuration constants.
- **LGSTSQ program**: Required for error logging.

### Known Issues or Limitations
1. **Password concatenation uses SPACE delimiter** (lines 224-227, 242-245): The `STRING ... DELIMITED BY SPACE` concatenation means passwords or salts containing internal spaces will be truncated at the first space. This could cause hash mismatches for passwords with spaces.
2. **Password length calculation** (line 330): `MOVE LENGTH OF COMM-PASSWORD TO WS-PASSWORD-LENGTH` always returns 64 (the PIC length), not the trimmed length of the actual password. The validation at line 335 against `SC-MIN-PASSWORD-LENGTH` will therefore never fail for "too short" since the length is always 64.
3. **LGSECUR in LINKAGE SECTION** (line 161): The security configuration structures from LGSECUR are defined in the LINKAGE SECTION but are not passed via the commarea. References to `SC-MIN-PASSWORD-LENGTH`, `PP-REQUIRE-UPPER`, etc. in the PROCEDURE DIVISION will reference uninitialized/unaddressable storage unless the COBOL compiler places them appropriately.
4. **Hash output size**: `WS-HASH-OUTPUT` is 128 bytes but SHA-256 produces a 32-byte hash (64 hex chars). The field is oversized but functional.
5. **No salt uniqueness guarantee**: While ICSF RNG provides cryptographic randomness, there is no duplicate salt check.

### Relationship to Other Programs
- Called by authentication programs in the GenApp system that need to hash or verify passwords.
- Calls `LGSTSQ` for error logging, which is the standard GenApp error/message logging facility.
- Uses the `LGSECUR` copybook which is shared across the GenApp security subsystem.
