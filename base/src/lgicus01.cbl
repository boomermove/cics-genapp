      ******************************************************************
      *                                                                *
      * (C) Copyright IBM Corp. 2011, 2020                             *
      *                                                                *
      *                    Inquire Customer                            *
      *                                                                *
      *   To obtain Customer's details from database.                  *
      *                                                                *
      * Customer Inquire Business logic                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGICUS01.
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
                                        VALUE 'LGICUS01------WS'.
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
           03 FILLER                   PIC X(9)  VALUE ' LGICUS01'.
           03 EM-VARIABLE              PIC X(21) VALUE SPACES.

       01 LGICDB01                  PIC X(8) Value 'LGICDB01'.

      *----------------------------------------------------------------*
      * Fields to be used to calculate if commarea is large enough
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.

           COPY LGPOLICY.
      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
             COPY LGCMAREA.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
      *
           INITIALIZE WS-HEADER.
      *
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('LGCA') NODUMP END-EXEC
           END-IF

           MOVE '00' TO CA-RETURN-CODE
           MOVE '00' TO CA-NUM-POLICIES
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.

      *----------------------------------------------------------------*
      * Process incoming commarea                                      *
      *----------------------------------------------------------------*
      * check commarea length
           MOVE WS-CUSTOMER-LEN        TO WS-REQUIRED-CA-LEN
           ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF

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

           EXEC CICS LINK Program(LGICDB01)
               Commarea(DFHCOMMAREA)
               LENGTH(32500)
           END-EXEC


           EXIT.

      *================================================================*
      * Procedure to write error message to Queues                     *
      *   Uses common error handling from LGERR/LGERRPRC copybooks     *
      *================================================================*
       WRITE-ERROR-MESSAGE.
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
