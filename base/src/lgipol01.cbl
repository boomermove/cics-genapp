      ******************************************************************
      *                                                                *
      * (C) Copyright IBM Corp. 2011, 2020                             *
      *                                                                *
      *                    Inquire Policy                              *
      *                                                                *
      *  Business logic for policy inquire                             *
      *   To obtain full details of an individual policy:              *
      *     Endowment, House or Motor                                  *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGIPOL01.
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
                                        VALUE 'LGIPOL01------WS'.
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

      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGIPOL01'.
           03 EM-VARIABLE              PIC X(21) VALUE SPACES.

       01 LGIPDB01                     PIC x(8) Value 'LGIPDB01'.
      *----------------------------------------------------------------*
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
      *
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
      *

           EXEC CICS LINK Program(LGIPDB01)
               Commarea(DFHCOMMAREA)
               Length(32500)
           END-EXEC.

           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*

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
