# ssmap.bms -- BMS Screen Map Definitions

## 1. Overview

**File name:** `ssmap.bms`
**Copyright:** IBM Corp. 2011, 2020
**Purpose:** Defines all 3270 terminal screen maps (BMS mapsets) for the General Insurance application's CICS terminal interface. Contains six screen definitions: one customer menu and five policy-type menus (motor, endowment, house, commercial, and claim).

This BMS source defines the mapset `SSMAP` containing maps `SSMAPC1` (customer), `SSMAPP1` (motor), `SSMAPP2` (endowment), `SSMAPP3` (house), `SSMAPP4` (commercial), and `SSMAPP5` (claim). Each map is a 24x80 character screen with labeled input fields, menu options, and an error message area.

### Mapset-Level Attributes (line 6-7)
- **TYPE=MAP** -- Generates a physical map
- **MODE=INOUT** -- Supports both input and output
- **LANG=COBOL** -- Generates COBOL copybooks
- **STORAGE=AUTO** -- Automatic storage allocation
- **TIOAPFX=YES** -- Include TIOA prefix
- **EXTATT=MAPONLY** -- Extended attributes in map only
- **CTRL=(FREEKB)** -- Free keyboard after send

## 2. Screen Definitions

### 2.1 SSMAPC1 -- Customer Screen (line 13-106)

**Title:** "General Insurance Customer Menu" (row 1, col 12)
**Screen ID:** 'SSC1' (row 1, col 1)

#### Menu Options
| Row | Col | Text |
|-----|-----|------|
| 4 | 8 | "1. Cust Inquiry" |
| 5 | 8 | "2. Cust Add" |
| 6 | 8 | (blank -- no option 3) |
| 7 | 8 | "4. Cust Update" |

#### Input Fields

| Name | Row | Col | Length | Attributes | Justify | Initial | Purpose |
|------|-----|-----|--------|------------|---------|---------|---------|
| ENT1CNO | 4 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | -- | Customer number (initial cursor) |
| ENT1FNA | 5 | 50 | 10 | NORM,UNPROT,FSET | -- | ' ' | First name |
| ENT1LNA | 6 | 50 | 20 | NORM,UNPROT,FSET | -- | ' ' | Last name |
| ENT1DOB | 7 | 50 | 10 | NORM,UNPROT,FSET | -- | ' ' | Date of birth (yyyy-mm-dd) |
| ENT1HNM | 8 | 50 | 20 | NORM,UNPROT,FSET | -- | ' ' | House name |
| ENT1HNO | 9 | 50 | 4 | NORM,UNPROT,FSET | -- | ' ' | House number |
| ENT1HPC | 10 | 50 | 8 | NORM,UNPROT,FSET | -- | ' ' | Postcode |
| ENT1HP1 | 11 | 50 | 20 | NORM,UNPROT,FSET | -- | ' ' | Home phone |
| ENT1HP2 | 12 | 50 | 20 | NORM,UNPROT,FSET | -- | ' ' | Mobile phone |
| ENT1HMO | 13 | 50 | 27 | NORM,UNPROT,FSET | -- | ' ' | Email address |
| ENT1OPT | 22 | 24 | 1 | NORM,NUM,UNPROT,FSET | -- | ' ' | Option selection (MUSTENTER) |

#### Error/Message Field
| Name | Row | Col | Length | Attributes | Purpose |
|------|-----|-----|--------|------------|---------|
| ERRFLD | 24 | 8 | 40 | BRT,ASKIP,PROT | Error message display |

### 2.2 SSMAPP1 -- Motor Policy Screen (line 112-231)

**Title:** "General Insurance Motor Policy Menu" (row 1, col 12)
**Screen ID:** 'SSP1' (row 1, col 1)

#### Menu Options
| Row | Col | Text |
|-----|-----|------|
| 4 | 8 | "1. Policy Inquiry" |
| 5 | 8 | "2. Policy Add" |
| 6 | 8 | "3. Policy Delete" |
| 7 | 8 | "4. Policy Update" |

#### Input Fields

| Name | Row | Col | Length | Attributes | Justify | Purpose |
|------|-----|-----|--------|------------|---------|---------|
| ENP1PNO | 4 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Policy number (initial cursor) |
| ENP1CNO | 5 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Customer number |
| ENP1IDA | 6 | 50 | 10 | NORM,UNPROT,FSET | -- | Issue date (yyyy-mm-dd) |
| ENP1EDA | 7 | 50 | 10 | NORM,UNPROT,FSET | -- | Expiry date (yyyy-mm-dd) |
| ENP1CMK | 8 | 50 | 20 | NORM,UNPROT,FSET | -- | Car make |
| ENP1CMO | 9 | 50 | 20 | NORM,UNPROT,FSET | -- | Car model |
| ENP1VAL | 10 | 50 | 6 | NORM,UNPROT,FSET | RIGHT,ZERO | Car value |
| ENP1REG | 11 | 50 | 7 | NORM,UNPROT,FSET | -- | Registration number |
| ENP1COL | 12 | 50 | 8 | NORM,UNPROT,FSET | -- | Car colour |
| ENP1CC | 13 | 50 | 8 | NORM,UNPROT,FSET | RIGHT,ZERO | Engine CC |
| ENP1MAN | 14 | 50 | 10 | NORM,UNPROT,FSET | -- | Manufacture date |
| ENP1ACC | 15 | 50 | 6 | NORM,UNPROT,FSET | RIGHT,ZERO | No. of accidents |
| ENP1PRE | 16 | 50 | 6 | NORM,UNPROT,FSET | RIGHT,ZERO | Policy premium |
| ENP1OPT | 22 | 24 | 1 | NORM,NUM,UNPROT,FSET | -- | Option selection (MUSTENTER) |

#### Error Field
| Name | Row | Col | Length | Attributes | Purpose |
|------|-----|-----|--------|------------|---------|
| ERP1FLD | 24 | 8 | 40 | BRT,ASKIP,PROT | Error message display |

### 2.3 SSMAPP2 -- Endowment Policy Screen (line 237-340)

**Title:** "General Insurance Endowment Policy Menu" (row 1, col 12)
**Screen ID:** 'SSP2' (row 1, col 1)

#### Menu Options
Same four options as motor: Inquiry, Add, Delete, Update.

#### Input Fields

| Name | Row | Col | Length | Attributes | Justify | Purpose |
|------|-----|-----|--------|------------|---------|---------|
| ENP2PNO | 4 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Policy number |
| ENP2CNO | 5 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Customer number |
| ENP2IDA | 6 | 50 | 10 | NORM,UNPROT,FSET | -- | Issue date |
| ENP2EDA | 7 | 50 | 10 | NORM,UNPROT,FSET | -- | Expiry date |
| ENP2FNM | 8 | 50 | 10 | NORM,UNPROT,FSET | -- | Fund name |
| ENP2TER | 9 | 50 | 2 | NORM,UNPROT,FSET | -- | Term |
| ENP2SUM | 10 | 50 | 6 | NORM,UNPROT,FSET | RIGHT,ZERO | Sum assured |
| ENP2LIF | 11 | 50 | 25 | NORM,UNPROT,FSET | RIGHT,ZERO | Life assured |
| ENP2WPR | 12 | 50 | 1 | NORM,UNPROT,FSET | -- | With profits flag |
| ENP2EQU | 13 | 50 | 1 | NORM,UNPROT,FSET | -- | Equities flag |
| ENP2MAN | 14 | 50 | 1 | NORM,UNPROT,FSET | -- | Managed funds flag |
| ENP2OPT | 22 | 24 | 1 | NORM,NUM,UNPROT,FSET | -- | Option selection (MUSTENTER) |

#### Error Field
| Name | Row | Col | Length | Attributes | Purpose |
|------|-----|-----|--------|------------|---------|
| ERP2FLD | 24 | 8 | 40 | BRT,ASKIP,PROT | Error message display |

### 2.4 SSMAPP3 -- House Policy Screen (line 346-443)

**Title:** "General Insurance House Policy Menu" (row 1, col 12)
**Screen ID:** 'SSP3' (row 1, col 1)

#### Menu Options
Same four options: Inquiry, Add, Delete, Update.

#### Input Fields

| Name | Row | Col | Length | Attributes | Justify | Purpose |
|------|-----|-----|--------|------------|---------|---------|
| ENP3PNO | 4 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Policy number |
| ENP3CNO | 5 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Customer number |
| ENP3IDA | 6 | 50 | 10 | NORM,UNPROT,FSET | -- | Issue date |
| ENP3EDA | 7 | 50 | 10 | NORM,UNPROT,FSET | -- | Expiry date |
| ENP3TYP | 8 | 50 | 15 | NORM,UNPROT,FSET | -- | Property type |
| ENP3BED | 9 | 50 | 3 | NORM,UNPROT,FSET | RIGHT,ZERO | Bedrooms |
| ENP3VAL | 10 | 50 | 8 | NORM,UNPROT,FSET | RIGHT,ZERO | House value |
| ENP3HNM | 11 | 50 | 20 | NORM,UNPROT,FSET | -- | House name |
| ENP3HNO | 12 | 50 | 4 | NORM,UNPROT,FSET | -- | House number |
| ENP3HPC | 13 | 50 | 8 | NORM,UNPROT,FSET | -- | Postcode |
| ENP3OPT | 22 | 24 | 1 | NORM,NUM,UNPROT,FSET | -- | Option selection (MUSTENTER) |

#### Error Field
| Name | Row | Col | Length | Attributes | Purpose |
|------|-----|-----|--------|------------|---------|
| ERP3FLD | 24 | 8 | 40 | BRT,ASKIP,PROT | Error message display |

### 2.5 SSMAPP4 -- Commercial Policy Screen (line 449-600)

**Title:** "General Insurance Commercial Policy Menu" (row 1, col 12)
**Screen ID:** 'SSP4' (row 1, col 1)

#### Menu Options
| Row | Col | Text |
|-----|-----|------|
| 4 | 8 | "1. Policy Inquiry" |
| 5 | 8 | "2. Policy Add" |
| 6 | 8 | "3. Policy Delete" |
| 7 | 8 | (Option 4 "Policy Update" is commented out on lines 460-461) |

#### Input Fields

| Name | Row | Col | Length | Attributes | Justify | Purpose |
|------|-----|-----|--------|------------|---------|---------|
| ENP4PNO | 4 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Policy number |
| ENP4CNO | 5 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Customer number |
| ENP4IDA | 6 | 50 | 10 | NORM,UNPROT,FSET | -- | Start date |
| ENP4EDA | 7 | 50 | 10 | NORM,UNPROT,FSET | -- | Expiry date |
| ENP4ADD | 8 | 50 | 25 | NORM,UNPROT,FSET | -- | Address |
| ENP4HPC | 9 | 50 | 8 | NORM,UNPROT,FSET | -- | Postcode |
| ENP4LAT | 10 | 50 | 11 | NORM,UNPROT,FSET | RIGHT,ZERO | Latitude |
| ENP4LON | 10 | 64 | 11 | NORM,UNPROT,FSET | RIGHT,ZERO | Longitude |
| ENP4CUS | 11 | 50 | 25 | NORM,UNPROT,FSET | -- | Customer name |
| ENP4PTY | 12 | 50 | 25 | NORM,UNPROT,FSET | -- | Property type |
| ENP4FPE | 13 | 50 | 4 | NORM,UNPROT,FSET | RIGHT,ZERO | Fire peril |
| ENP4FPR | 13 | 56 | 8 | NORM,UNPROT,FSET | RIGHT,ZERO | Fire premium |
| ENP4CPE | 14 | 50 | 4 | NORM,UNPROT,FSET | RIGHT,ZERO | Crime peril |
| ENP4CPR | 14 | 56 | 8 | NORM,UNPROT,FSET | RIGHT,ZERO | Crime premium |
| ENP4XPE | 15 | 50 | 4 | NORM,UNPROT,FSET | RIGHT,ZERO | Flood peril |
| ENP4XPR | 15 | 56 | 8 | NORM,UNPROT,FSET | RIGHT,ZERO | Flood premium |
| ENP4WPE | 16 | 50 | 4 | NORM,UNPROT,FSET | RIGHT,ZERO | Weather peril |
| ENP4WPR | 16 | 56 | 8 | NORM,UNPROT,FSET | RIGHT,ZERO | Weather premium |
| ENP4STA | 17 | 50 | 4 | NORM,UNPROT,FSET | RIGHT,ZERO | Status |
| ENP4REJ | 18 | 50 | 25 | NORM,UNPROT,FSET | RIGHT,ZERO | Reject reason |
| ENP4OPT | 22 | 24 | 1 | NORM,NUM,UNPROT,FSET | -- | Option selection (MUSTENTER) |

#### Error Field
| Name | Row | Col | Length | Attributes | Purpose |
|------|-----|-----|--------|------------|---------|
| ERP4FLD | 24 | 8 | 40 | BRT,ASKIP,PROT | Error message display |

### 2.6 SSMAPP5 -- Claim Screen (line 606-686)

**Title:** "General Insurance Policy Claim Menu" (row 1, col 12)
**Screen ID:** 'SSP5' (row 1, col 1)

#### Menu Options
| Row | Col | Text |
|-----|-----|------|
| 4 | 8 | "1. Claim Inquiry" |
| 5 | 8 | "2. Claim Add" |
| 6/7 | 8 | (Delete and Update are commented out, lines 615-618) |

#### Input Fields

| Name | Row | Col | Length | Attributes | Justify | Purpose |
|------|-----|-----|--------|------------|---------|---------|
| ENP5LNO | 4 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Claim number |
| ENP5PNO | 5 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Policy number |
| ENP5CNO | 6 | 50 | 10 | NORM,UNPROT,IC,FSET | RIGHT,ZERO | Customer number |
| ENP5CDA | 7 | 50 | 10 | NORM,UNPROT,FSET | -- | Claim date |
| ENP5PAD | 8 | 50 | 10 | NORM,UNPROT,FSET | RIGHT,ZERO | Paid amount (XINIT zeros) |
| ENP5VAL | 9 | 50 | 10 | NORM,UNPROT,FSET | RIGHT,ZERO | Claim value (XINIT zeros) |
| ENP5CAU | 10 | 50 | 25 | NORM,UNPROT,FSET | -- | Cause |
| ENP5OBS | 11 | 50 | 25 | NORM,UNPROT,FSET | -- | Observations |
| ENP5OPT | 22 | 24 | 1 | NORM,NUM,UNPROT,FSET | -- | Option selection (MUSTENTER) |

#### Error Field
| Name | Row | Col | Length | Attributes | Purpose |
|------|-----|-----|--------|------------|---------|
| ERP5FLD | 24 | 8 | 40 | BRT,ASKIP,PROT | Error message display |

## 3. Field Attributes Analysis

### Attribute Categories

**Protected display fields (labels):**
- `ATTRB=(NORM,ASKIP)` -- Normal intensity, auto-skip (labels and descriptive text)
- `ATTRB=(ASKIP,BRT)` -- Auto-skip, bright (screen ID and title at row 1)

**Unprotected input fields:**
- `ATTRB=(NORM,UNPROT,IC,FSET)` -- Normal, unprotected, initial cursor, field set modified (key number fields)
- `ATTRB=(NORM,UNPROT,FSET)` -- Normal, unprotected, field set modified (data entry fields)
- `ATTRB=(NORM,NUM,UNPROT,FSET)` -- Normal, numeric, unprotected, field set modified (option selection)

**Stopper fields:**
- `ATTRB=(PROT,ASKIP)` -- Protected, auto-skip, length 1 (field delimiters placed after each input field)

**Error message fields:**
- `ATTRB=(BRT,ASKIP,PROT)` -- Bright, auto-skip, protected (error message at row 24)

### FSET Attribute
All input fields use `FSET` (Field Set Modified Data Tag), which causes the field data to always be transmitted back to the program even if the user did not modify it. This simplifies data handling since the program always receives all field values.

### JUSTIFY Attribute
Numeric fields use `JUSTIFY=(RIGHT,ZERO)` to right-justify with zero-fill. This ensures numeric values like policy numbers and amounts are properly formatted.

### VALIDN=(MUSTENTER)
The option selection fields (`ENT1OPT`, `ENP1OPT`, etc.) use `VALIDN=(MUSTENTER)` to require the user to enter a value before the screen can be transmitted.

## 4. Screen Layout (ASCII Representation)

### SSMAPC1 -- Customer Screen
```
+------------------------------------------------------------------------------+
|SSC1       General Insurance Customer Menu                                    |
|                                                                              |
|                                                                              |
|       1. Cust Inquiry       Cust Number  [__________]                        |
|       2. Cust Add           Cust Name :First[__________]                     |
|                                   :Last[____________________]                |
|       4. Cust Update        DOB         [__________]  (yyyy-mm-dd)           |
|                             House Name  [____________________]               |
|                             House Number[____]                               |
|                             Postcode    [________]                           |
|                             Phone: Home [____________________]               |
|                             Phone: Mob  [____________________]               |
|                             Email  Addr [___________________________]        |
|                                                                              |
|...(rows 14-21 empty)                                                         |
|       Select Option [_]                                                      |
|...(row 23 empty)                                                             |
|       [________________________________________]  (error message)            |
+------------------------------------------------------------------------------+
```

### SSMAPP1 -- Motor Policy Screen
```
+------------------------------------------------------------------------------+
|SSP1       General Insurance Motor Policy Menu                                |
|                                                                              |
|                                                                              |
|       1. Policy Inquiry     Policy Number [__________]                       |
|       2. Policy Add         Cust Number   [__________]                       |
|       3. Policy Delete      Issue date    [__________]  (yyyy-mm-dd)         |
|       4. Policy Update      Expiry date   [__________]  (yyyy-mm-dd)         |
|                             Car Make      [____________________]             |
|                             Car Model     [____________________]             |
|                             Car Value     [______]                           |
|                             Registration  [_______]                          |
|                             Car Colour    [________]                         |
|                             CC            [________]                         |
|                             Manufacture Date[__________] (yyyy-mm-dd)        |
|                             No. of Accidents[______]                         |
|                             Policy Premium  [______]                         |
|...(rows 17-21 empty)                                                         |
|       Select Option [_]                                                      |
|...(row 23 empty)                                                             |
|       [________________________________________]  (error message)            |
+------------------------------------------------------------------------------+
```

### SSMAPP4 -- Commercial Policy Screen
```
+------------------------------------------------------------------------------+
|SSP4       General Insurance Commercial Policy Menu                           |
|                                                                              |
|                                                                              |
|       1. Policy Inquiry     Policy Number [__________]                       |
|       2. Policy Add         Cust Number   [__________]                       |
|       3. Policy Delete      Start date    [__________]  (yyyy-mm-dd)         |
|                             Expiry date   [__________]  (yyyy-mm-dd)         |
|                             Address       [_________________________]        |
|                             Postcode      [________]                         |
|                             Latitude/Longitude [___________] [___________]   |
|                             Customer Name [_________________________]        |
|                             Property Type [_________________________]        |
|                             Fire Peril/Prem [____] [________]                |
|                             Crime Peril/Prem[____] [________]                |
|                             Flood Peril/Prem[____] [________]                |
|                             Weather Peril/Prem[____] [________]              |
|                             Status        [____]                             |
|                             Reject Reason [_________________________]        |
|...(rows 19-21 empty)                                                         |
|       Select Option [_]                                                      |
|...(row 23 empty)                                                             |
|       [________________________________________]  (error message)            |
+------------------------------------------------------------------------------+
```

## 5. Usage by COBOL Programs

The BMS mapset `SSMAP` is used by the test/screen programs that handle the 3270 terminal interface:
- `lgtestc1.cbl` -- Customer screen handler (uses SSMAPC1)
- `lgtestp1.cbl` -- Motor policy screen handler (uses SSMAPP1)
- `lgtestp2.cbl` -- Endowment policy screen handler (uses SSMAPP2)
- `lgtestp3.cbl` -- House policy screen handler (uses SSMAPP3)
- `lgtestp4.cbl` -- Commercial policy screen handler (uses SSMAPP4)

These programs use `EXEC CICS SEND MAP` and `EXEC CICS RECEIVE MAP` to display and read the screens. The BMS assembler generates a COBOL copybook with field names matching the named DFHMDF entries (e.g., `ENT1CNOI` for input, `ENT1CNOO` for output of the `ENT1CNO` field).

### Map/Program Correspondence
| Map | Screen | COBOL Program |
|-----|--------|--------------|
| SSMAPC1 | Customer Menu | lgtestc1.cbl |
| SSMAPP1 | Motor Policy | lgtestp1.cbl |
| SSMAPP2 | Endowment Policy | lgtestp2.cbl |
| SSMAPP3 | House Policy | lgtestp3.cbl |
| SSMAPP4 | Commercial Policy | lgtestp4.cbl |
| SSMAPP5 | Claim | (likely lgtestp5.cbl or handled within lgtestp4.cbl) |

### Termination
The mapset ends with `DFHMSD TYPE=FINAL` (line 687) and `END` (line 688).
