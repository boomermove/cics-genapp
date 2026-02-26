# polloo2.cpy -- Customer Policy Lookup Request Copybook

## 1. Overview

**File name:** `polloo2.cpy`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines a COMMAREA structure for looking up the policies associated with a customer. This copybook provides an array of policy number/type pairs and a data area for returning policy details.

This is a compact copybook used in customer-to-policy lookup operations where the application needs to retrieve a list of up to five policies for a given customer.

## 2. Data Structure Breakdown

### REQUEST (Level 01, line 6)

| Level | Field Name | PIC Clause | Attributes | Purpose |
|-------|-----------|------------|------------|---------|
| 03 | CA-CUSPOL-REQUEST | (group) | -- | Group item for the policy lookup request |
| 05 | CA-POLICIES | (group) | OCCURS 5 TIMES | Array of 5 policy entries |
| 07 | CA-POLICY-NUMBER | 9(10) | DISPLAY | Policy number (10 digits) |
| 07 | CA-POLICY-TYPE | X(1) | DISPLAY | Policy type code (single character) |
| 03 | CA-POLICY-DATA | X(32427) | -- | Remaining data area for policy details |

### Size Calculation
- Each `CA-POLICIES` entry: 10 + 1 = 11 bytes
- Array of 5: 11 x 5 = 55 bytes
- `CA-POLICY-DATA`: 32,427 bytes
- Total: 55 + 32,427 = 32,482 bytes

Note: The total of 32,482 bytes matches exactly the size of `CA-REQUEST-SPECIFIC` in `lgcmarea.cpy`, indicating this structure is designed to overlay that field.

## 3. Key Components

### OCCURS Clause
The `CA-POLICIES` array (line 8) allows up to 5 policy references to be returned per customer lookup. Each entry contains:
- `CA-POLICY-NUMBER`: The 10-digit policy identifier
- `CA-POLICY-TYPE`: A single character identifying the policy type (e.g., 'E' for endowment, 'H' for house, 'M' for motor)

Both fields use the explicit `DISPLAY` usage, confirming standard character-format storage.

### No Condition Names, REDEFINES, or VALUE Clauses
This copybook is purely a data layout with no initial values, conditions, or redefinitions.

## 4. Usage Context

### Section Placement
The level 01 `REQUEST` structure would be placed in the LINKAGE SECTION or WORKING-STORAGE SECTION, depending on whether it is received as a COMMAREA or used as a local work area.

### Data Flow
This structure is used to pass a list of policy references between programs. The calling program populates the customer number (from a higher-level structure), and the called program fills in the `CA-POLICIES` array with up to 5 matching policy numbers and types.

## 5. Analysis

### Role in Architecture
This copybook supports the **customer-policy association** feature. When a user inquires on a customer, the system retrieves the customer's policies and returns them in this array structure. The 5-entry limit suggests a display-oriented design matching the screen capacity.

### Design Patterns
- **Fixed-size array:** OCCURS 5 TIMES provides a bounded result set, typical of CICS screen-driven applications.
- **Overlay-compatible sizing:** The 32,482-byte total matches the COMMAREA's `CA-REQUEST-SPECIFIC` field exactly.

### Size Considerations
The 32,427-byte `CA-POLICY-DATA` filler ensures the structure fills the full COMMAREA request area. Most of this space is unused padding in a typical lookup scenario.
