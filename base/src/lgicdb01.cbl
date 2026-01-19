       PROCESS SQL
      ******************************************************************
      *                                                                *
      * (C) Copyright IBM Corp. 2011, 2021                             *
      *                                                                *
      *                    Inquire Customer                            *
      *                                                                *
      * Select customer details from DB2 table                         *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGICDB01.
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
                                        VALUE 'LGICDB01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.

      *----------------------------------------------------------------*
      * Common error handling copybook                                 *
      *----------------------------------------------------------------*
           COPY LGERR.

      * Error Message structure (program-specific format)
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGICDB01'.
           03 EM-VARIABLE.
             05 FILLER                 PIC X(6)  VALUE ' CNUM='.
             05 EM-CUSNUM              PIC X(10)  VALUE SPACES.
             05 EM-SQLREQ              PIC X(16) VALUE SPACES.
             05 FILLER                 PIC X(9)  VALUE ' SQLCODE='.
             05 EM-SQLRC               PIC +9(5) USAGE DISPLAY.

      *----------------------------------------------------------------*
      * Fields to be used to calculate if commarea is large enough
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.

      *----------------------------------------------------------------*
      * Definitions required by SQL statement                          *
      *   DB2 datatypes to COBOL equivalents                           *
      *     SMALLINT    :   PIC S9(4) COMP                             *
      *     INTEGER     :   PIC S9(9) COMP                             *
      *     DATE        :   PIC X(10)                                  *
      *     TIMESTAMP   :   PIC X(26)                                  *
      *----------------------------------------------------------------*
      * Host variables for input to DB2 integer types
       01  DB2-IN-INTEGERS.
           03 DB2-CUSTOMERNUMBER-INT   PIC S9(9) COMP.

           COPY LGPOLICY.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    DB2 CONTROL
      *----------------------------------------------------------------*
      * SQLCA DB2 communications area
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
      *
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
      *----------------------------------------------------------------*

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

      * initialize DB2 host variables
           INITIALIZE DB2-IN-INTEGERS.

      *----------------------------------------------------------------*
      * Process incoming commarea                                      *
      *----------------------------------------------------------------*
      * check commarea length - meets minimum requirement
           MOVE WS-CUSTOMER-LEN        TO WS-REQUIRED-CA-LEN
           ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN
      * if less set error return code and return to caller
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF

      * Convert commarea customer number to DB2 integer format
           MOVE CA-CUSTOMER-NUM TO DB2-CUSTOMERNUMBER-INT
      * and save in error msg field incase required
           MOVE CA-CUSTOMER-NUM TO EM-CUSNUM

      *----------------------------------------------------------------*
      * Obtain details from DB2                                        *
      *----------------------------------------------------------------*
      *    Call routine to issue SQL to obtain info from DB2
           PERFORM GET-CUSTOMER-INFO.

      *----------------------------------------------------------------*
      * END PROGRAM and return to caller                               *
      *----------------------------------------------------------------*
       MAINLINE-END.

           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       GET-CUSTOMER-INFO.

           EXEC SQL
               SELECT FIRSTNAME,
                      LASTNAME,
                      DATEOFBIRTH,
                      HOUSENAME,
                      HOUSENUMBER,
                      POSTCODE,
                      PHONEMOBILE,
                      PHONEHOME,
                      EMAILADDRESS
               INTO  :CA-FIRST-NAME,
                     :CA-LAST-NAME,
                     :CA-DOB,
                     :CA-HOUSE-NAME,
                     :CA-HOUSE-NUM,
                     :CA-POSTCODE,
                     :CA-PHONE-MOBILE,
                     :CA-PHONE-HOME,
                     :CA-EMAIL-ADDRESS
               FROM CUSTOMER
               WHERE CUSTOMERNUMBER = :DB2-CUSTOMERNUMBER-INT
           END-EXEC.

           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
             When 100
               MOVE '01' TO CA-RETURN-CODE
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.

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
