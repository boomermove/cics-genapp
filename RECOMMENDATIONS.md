# Recommendations for Improving the CICS GenApp Codebase

## Critical (Blocking Issues)

### 1. Security Tables Missing from DB2 Schema
The `LGAUTH01` and `LGPWHASH` programs reference tables (`USER_SECURITY`, `PASSWORD_HISTORY`, `AUDIT_LOG`) that are defined in `security_tables.sql` but **not included in `db2cre.jcl`** - the actual DB2 provisioning job. These programs will fail at runtime with table-not-found errors. The existing `customer_secure` table in `db2cre.jcl` uses a 32-byte password field (MD5 format), while the new auth module expects 128 bytes (SHA-256). These schemas are incompatible.

**Fix:** Integrate `security_tables.sql` DDL into the `db2cre.jcl` build job and create a migration path from the legacy `customer_secure` table.

### 2. Hardcoded Password Hash in Production Code
`lgacdb01.cbl:177-178` contains a hardcoded MD5 hash:
```cobol
move '5732fec825535eeafb8fac50fee3a8aa'
                    To  D2-CUSTSECR-PASS.
```
This same hash also appears in `db2cre.jcl` sample data. Every new customer gets the same password. The secure version (`lgacdb01_secure.cbl`) fixes this by generating dynamic temporary passwords, but the non-secure version is still the one referenced by the build job.

**Fix:** Replace `lgacdb01.cbl` with `lgacdb01_secure.cbl` in the compilation pipeline, or remove the hardcoded credential entirely.

### 3. Security Programs Not in Compilation Job
`LGAUTH01.cbl` and `LGPWHASH.cbl` are not listed in `cobol.jcl`. They will never be compiled or deployed, making the entire authentication subsystem dead code.

**Fix:** Add both programs to the COBOL compilation JCL.

---

## High Priority (Reliability & Correctness)

### 4. Missing RESP Checking on CICS LINK Calls
Multiple business-logic programs issue `EXEC CICS LINK` without a `RESP` clause, meaning if the called program fails to load or abends, execution continues with undefined results:

| Program | Line | Missing RESP on LINK to |
|---|---|---|
| `lgapol01.cbl` | ~117 | LGAPDB01 |
| `lgacus01.cbl` | ~129 | LGACDB01 |
| `lgipol01.cbl` | ~86 | LGIPDB01 |
| `lgdpol01.cbl` | ~136 | LGDPDB01 |
| `lgucus01.cbl` | ~similar | LGUCDB01 |
| `lgupol01.cbl` | ~similar | LGUPDB01 |

The secure version (`lgacdb01_secure.cbl`) demonstrates the correct pattern with `RESP(WS-RESP)` and explicit checking. All LINK calls should follow that model.

### 5. Possible COMMAREA Size Check Bug
`lgacdb01_secure.cbl:154-157`:
```cobol
IF WS-CALEN < 32767
    MOVE '98' TO CA-RETURN-CODE
    PERFORM 9000-RETURN
END-IF
```
This rejects any COMMAREA smaller than 32,767 bytes, but callers typically pass shorter areas. Compare to the correct pattern in `lgacdb01.cbl` which checks against a defined minimum (`WS-REQUIRED-CA-LEN`). This may be a logic bug preventing the secure version from working.

### 6. Inconsistent SQLCODE Error Handling
INSERT operations handle SQL errors differently across programs:
- `lgapdb01.cbl` uses `EVALUATE SQLCODE` checking for specific codes (-530 FK violation)
- `lgacdb01.cbl` uses simple `IF SQLCODE NOT EQUAL 0`
- `lgucdb01.cbl` and `lgdpdb01.cbl` have their own variations

**Fix:** Standardize SQL error handling into a shared pattern (similar to what LGERRPRC.cpy does for CICS errors). Create a SQL error handling copybook.

### 7. No Input Validation at Business Logic Boundary
While all SQL uses parameterized queries (safe from injection), no program validates business data before processing:
- Dates aren't validated for format (yyyy-mm-dd)
- Numeric fields aren't range-checked
- Postcodes, email addresses, phone numbers aren't validated
- Policy types aren't validated before routing

Invalid data silently flows through to DB2, where it either gets stored incorrectly or triggers cryptic SQL errors.

---

## Medium Priority (Test Coverage & Maintainability)

### 8. Zero Test Coverage for Security Subsystem
`LGAUTH01` (1,089 lines) and `LGPWHASH` (508 lines) have **no test programs and no WSIM scenarios**. Functions untested include: authentication, password change, account lockout, password expiry, password history validation, and audit logging.

**Fix:** Create a dedicated `LGTESTAUTH` program that exercises each authentication function, and add WSIM scenarios for the login flow.

### 9. No Error Path Testing
All existing WSIM test scenarios test only the happy path. No scenarios deliberately trigger:
- Invalid customer/policy numbers
- Duplicate key violations
- DB2 or VSAM unavailability
- COMMAREA size errors
- Concurrent access conflicts

### 10. Missing Test Coverage for Specific Operations
| Gap | Impact |
|---|---|
| Customer Delete | No delete operation exists at all (no option 3 in LGTESTC1) |
| Commercial Policy Update | No WSIM scenario (`ssp4u1.txt` missing), screen option commented out |
| Claims (SSMAPP5) | BMS screen defined but no backing COBOL program exists |

### 11. Magic Numbers and Code Smells
`lgipdb01.cbl:902-904`:
```cobol
If ICOM-Record-Count > 20
  Move 17 To SQLCODE
End-If
```
Hardcoded limit of 20 records and faking SQLCODE=17 to break out of a cursor loop. This should use a named constant and a proper loop-control variable instead of corrupting SQLCODE.

### 12. Unused Variables
`lgacdb01_secure.cbl:51` defines `LGAC-RETRY-TIMES PIC 9 VALUE 9` which is never referenced anywhere in the program.

---

## Architectural Recommendations (Longer Term)

### 13. Eliminate VSAM/DB2 Dual Storage
Every write operation updates both DB2 and VSAM (via paired LINK calls to DB and VS programs). This doubles I/O, requires two-phase commit coordination, and creates consistency risks. The VSAM files (`KSDSCUST`, `KSDSPOLY`) appear to be used only by the `LGICVS01`/`LGIPVS01` random-record lookup programs.

**Fix:** Migrate random-record lookup logic to DB2 (e.g., `SELECT ... ORDER BY RAND()` or counter-based approach) and decommission VSAM programs entirely.

### 14. Replace TSQ-Based Error Logging
Errors are written to Temporary Storage Queues via `LGSTSQ`, which are volatile and have limited query capability. Consider structured logging to DB2 or an external logging facility.

### 15. Modernize Counter Management
Customer numbers use named counters (`EXEC CICS READCOUNTER`) requiring a Named Counter Server. Policy numbers already use DB2 `IDENTITY` columns. Standardize on DB2 identity columns for all auto-generated IDs.

### 16. Add RESTful API Layer
The SOA copybooks (`soaic01.cpy`, `soaip*.cpy`) suggest web service capability exists, but the architecture is primarily 3270-based. CICS TS supports JSON/REST natively - the existing business logic programs could be exposed as REST APIs with minimal wrapper programs.

### 17. Update Documentation
`Reference.md`, `Building.md`, and `Installation.md` don't mention the security subsystem (`LGAUTH01`, `LGPWHASH`, `LGSECUR.cpy`, security tables). Document the authentication architecture and migration strategy.

### 18. Compiler Options
The current `cobol.jcl` uses minimal options. Consider adding:
- `OPT(2)` for optimization
- `SSRANGE` for runtime bounds checking (at least in test)
- `FLAG(W)` to surface all compiler warnings

---

## Summary

| Priority | Count | Key Theme |
|---|---|---|
| **Critical** | 3 | Security infrastructure not wired into build/deploy |
| **High** | 4 | Missing error handling, input validation, possible bug |
| **Medium** | 5 | Test gaps, code smells, dead code |
| **Architectural** | 6 | Dual storage, logging, APIs, documentation |

The most impactful immediate action is completing the security integration: add the security DDL to `db2cre.jcl`, add `LGAUTH01`/`LGPWHASH` to `cobol.jcl`, replace the hardcoded password, and fix the COMMAREA check in the secure version. This unblocks the entire authentication subsystem that has been built but never connected.
