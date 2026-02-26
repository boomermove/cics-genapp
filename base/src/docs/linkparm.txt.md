# linkparm.txt -- DB2 Precompiler Link Parameters

## 1. Overview

**File name:** `linkparm.txt`
**Purpose:** Provides the link-edit (binder) input control statement for including the DB2 Call Level Interface (CLI) module during the COBOL program link-edit step. This file contains a single directive that tells the binder to include the DB2 precompiler support module.

This file is used during the build process for COBOL programs that contain embedded SQL. It is referenced by the JCL compile/link procedure to ensure the DB2 interface module is included in the final load module.

## 2. Content Breakdown

### Line 1: `INCLUDE SYSLIB(DSNCLI)`

| Component | Value | Purpose |
|-----------|-------|---------|
| Statement | INCLUDE | Binder directive to include an external module |
| Library | SYSLIB | The system library DD name (resolved at JCL execution time) |
| Member | DSNCLI | DB2 Call Level Interface module |

**DSNCLI** is the IBM DB2 for z/OS Call Level Interface stub module. It provides the runtime linkage between the COBOL program's embedded SQL statements (which the DB2 precompiler has converted to CALL statements) and the actual DB2 engine.

### Usage in Build Process

This file is referenced in `base/cntl/cobol.jcl` at line 79:
```jcl
//         DD DISP=SHR,DSN=<SOURCEX>(LINKPARM)
```

The JCL concatenates this file into the binder's SYSIN input, causing the binder to resolve and include the DSNCLI module from the SYSLIB library (typically the DB2 SDSNLOAD dataset).

### Upload Process

As documented in `base/src/README.md`, this file is uploaded to the mainframe with:
```
put linkparm.txt linkparm
```

This transfers the file from the local development environment to the mainframe source PDS as member LINKPARM, where it is consumed by the JCL build process.

### Why This File Exists
Without `INCLUDE SYSLIB(DSNCLI)`, any COBOL program with embedded SQL would fail at link-edit time with unresolved external references to DB2 functions. Every DB2-enabled COBOL program in GenApp (the `lg*db*.cbl` programs) requires this binder directive.
