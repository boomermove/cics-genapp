       PROCESS SQL
      ******************************************************************
      *                                                                *
      * (C) Copyright IBM Corp. 2011, 2021                             *
      *                                                                *
      *                     UPDATE customer details                    *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGUCDB01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'LGUCDB01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.
           03 WS-RETRY                 PIC X.

      * Host variables for input to DB2 integer types
      * Any values specified in SQL stmts must be defined her
      * available to SQL pre-compiler
       01 DB2-IN-INTEGERS.
          03 DB2-CUSTOMERNUM-INT       PIC S9(9) COMP.
          03 DB2-POLICYNUM-INT         PIC S9(9) COMP.
          03 DB2-BROKERID-INT          PIC S9(9) COMP.
          03 DB2-PAYMENT-INT           PIC S9(9) COMP.
          03 DB2-E-TERM-SINT           PIC S9(4) COMP.
          03 DB2-E-SUMASSURED-INT      PIC S9(9) COMP.
          03 DB2-H-BEDROOMS-SINT       PIC S9(4) COMP.
          03 DB2-H-VALUE-INT           PIC S9(9) COMP.
          03 DB2-M-VALUE-INT           PIC S9(9) COMP.
          03 DB2-M-CC-SINT             PIC S9(4) COMP.
          03 DB2-M-PREMIUM-INT         PIC S9(9) COMP.
          03 DB2-M-ACCIDENTS-INT       PIC S9(9) COMP.

      *----------------------------------------------------------------*
      * Common error handling copybook                                 *
      *----------------------------------------------------------------*
           COPY LGERR.

      * Error Message structure (program-specific format)
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGUCDB01'.
           03 EM-VARIABLE.
             05 FILLER                 PIC X(6)  VALUE ' CNUM='.
             05 EM-CUSNUM              PIC X(10)  VALUE SPACES.
             05 EM-SQLREQ              PIC X(16) VALUE SPACES.
             05 FILLER                 PIC X(9)  VALUE ' SQLCODE='.
             05 EM-SQLRC               PIC +9(5) USAGE DISPLAY.

       77  LGUCVS01                    Pic X(8) Value 'LGUCVS01'.
      *----------------------------------------------------------------*
      *    DB2 CONTROL
      *----------------------------------------------------------------*
           EXEC SQL
             INCLUDE LGPOLICY
           END-EXEC.
      * SQLCA DB2 communications area
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
           EXEC SQL
             INCLUDE LGCMAREA
           END-EXEC.


      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE WS-HEADER.
      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
           MOVE SPACES   TO WS-RETRY.
      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('LGCA') NODUMP END-EXEC
           END-IF

      * initialize commarea return code to zero
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.

      * Convert commarea customer to DB2 integer format
           MOVE CA-CUSTOMER-NUM TO DB2-CUSTOMERNUM-INT
      * and save in error msg field incase required
           MOVE CA-CUSTOMER-NUM TO EM-CUSNUM

      *    Call procedure to update required tables
           PERFORM UPDATE-CUSTOMER-INFO.

           EXEC CICS LINK Program(LGUCVS01)
                Commarea(DFHCOMMAREA)
                LENGTH(225)
           END-EXEC.

      * Return to caller
       END-PROGRAM.
           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      * Update row in Customer table                                   *
      *================================================================*
       UPDATE-CUSTOMER-INFO.

           MOVE ' UPDATE CUST  ' TO EM-SQLREQ
             EXEC SQL
               UPDATE CUSTOMER
                 SET
                   FIRSTNAME     = :CA-FIRST-NAME,
                   LASTNAME      = :CA-LAST-NAME,
                   DATEOFBIRTH   = :CA-DOB,
                   HOUSENAME     = :CA-HOUSE-NAME,
                   HOUSENUMBER   = :CA-HOUSE-NUM,
                   POSTCODE      = :CA-POSTCODE,
                   PHONEMOBILE   = :CA-PHONE-MOBILE,
                   PHONEHOME     = :CA-PHONE-HOME,
                   EMAILADDRESS  = :CA-EMAIL-ADDRESS
                 WHERE
                     CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT
             END-EXEC

           IF SQLCODE NOT EQUAL 0
      *      Non-zero SQLCODE from UPDATE statement
             IF SQLCODE EQUAL 100
               MOVE '01' TO CA-RETURN-CODE
             ELSE
               MOVE '90' TO CA-RETURN-CODE
      *        Write error message to TD QUEUE(CSMT)
               PERFORM WRITE-ERROR-MESSAGE
             END-IF
           END-IF.
           EXIT.


      *================================================================*
      * Procedure to write error message to Queues                     *
      *   Uses common error handling from LGERR/LGERRPRC copybooks     *
      *================================================================*
       WRITE-ERROR-MESSAGE.
      * Save SQLCODE in message
           MOVE SQLCODE TO EM-SQLRC
      * Format time and date
           PERFORM LGERR-FORMAT-TIME
           MOVE WS-ERR-DATE TO EM-DATE
           MOVE WS-ERR-TIME TO EM-TIME
      * Write error message and commarea to TSQ
           PERFORM LGERR-WRITE-MSG
           PERFORM LGERR-LOG-COMMAREA
           .

      *----------------------------------------------------------------*
      * Common error handling procedures from copybook                 *
      *----------------------------------------------------------------*
           COPY LGERRPRC.
