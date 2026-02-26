      ******************************************************************
      *                                                                *
      * (C) Copyright IBM Corp. 2011, 2020                             *
      *                                                                *
      *                    Update Policy - VSAM                        *
      *                                                                *
      * VSAM KSDS Policy record Update                                 *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGUPVS01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                   PIC S9(8) COMP.
       01  WS-RESP2                  PIC S9(8) COMP.
       01  WS-Comm-Len               PIC S9(8) COMP.
       01  WS-STARTCODE              PIC XX Value spaces.
       01  WS-SYSID                  PIC X(4) Value spaces.
       01  WS-Commarea-Len           PIC S9(4) COMP Value 64.
       01  WS-Commarea-LenF          PIC S9(4) COMP Value 64.
      ******************************
       01  WF-Policy-Info.
         03  WF-Policy-Key.
           05  WF-Request-ID           Pic X.
           05  WF-Customer-Num         Pic X(10).
           05  WF-Policy-Num           Pic X(10).
         03 WF-Policy-Data             Pic X(43).
         03 WF-C-Policy-Data Redefines WF-Policy-Data.
           05  WF-B-Postcode           Pic X(8).
           05  WF-B-Status             Pic 9(4).
           05  WF-B-Customer           Pic X(31).
         03 WF-E-Policy-Data Redefines WF-Policy-Data.
           05  WF-E-WITH-PROFITS       Pic X.
           05  WF-E-EQUITIES           Pic X.
           05  WF-E-MANAGED-FUND       Pic X.
           05  WF-E-FUND-NAME          Pic X(10).
           05  WF-E-LIFE-ASSURED       Pic X(30).
         03 WF-H-Policy-Data Redefines WF-Policy-Data.
           05  WF-H-PROPERTY-TYPE      Pic X(15).
           05  WF-H-BEDROOMS           Pic 999.
           05  WF-H-VALUE              Pic 9(8).
           05  WF-H-POSTCODE           Pic X(8).
           05  WF-H-HOUSE-NAME         Pic X(9).
         03 WF-M-Policy-Data Redefines WF-Policy-Data.
           05  WF-M-MAKE               Pic X(15).
           05  WF-M-MODEL              Pic X(15).
           05  WF-M-VALUE              Pic 9(6).
           05  WF-M-REGNUMBER          Pic X(7).
      *
       01  WS-FileIn                   Pic X(1024) value Spaces.
      ******************************
      *----------------------------------------------------------------*
      * Common error handling copybook                                 *
      *----------------------------------------------------------------*
           COPY LGERR.
      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGUPVS01'.
           03 EM-VARIABLE.
             05 FILLER                 PIC X(6)  VALUE ' PNUM='.
             05 EM-POLNUM              PIC X(10)  VALUE SPACES.
             05 FILLER                 PIC X(6)  VALUE ' CNUM='.
             05 EM-CUSNUM              PIC X(10)  VALUE SPACES.
             05 FILLER                 PIC X(20)
                                        Value ' Re-write  KSDSPOLY '.
             05 FILLER                 PIC X(6)  VALUE ' RESP='.
             05 EM-RESPRC              PIC +9(5) USAGE DISPLAY.
             05 FILLER                 PIC X(7)  VALUE ' RESP2='.
             05 EM-RESP2RC             PIC +9(5) USAGE DISPLAY.

      ******************************
       77 Eyecatcher               PIC X(16)
                                      Value 'Program LGUPVS01'.
      *****************************************************************
      *    L I N K A G E     S E C T I O N
      *****************************************************************
       LINKAGE SECTION.
       01  DFHCOMMAREA.
         Copy LGCMAREA.

      *----------------------------------------------------------------*
      *****************************************************************
       PROCEDURE DIVISION.

      *---------------------------------------------------------------*
       MAINLINE SECTION.
      *
      *---------------------------------------------------------------*
           Move EIBCALEN To WS-Commarea-Len.
      *---------------------------------------------------------------*
           Move CA-Request-ID(4:1) To WF-Request-ID
           Move CA-Policy-Num      To WF-Policy-Num
           Move CA-Customer-Num    To WF-Customer-Num

           Evaluate WF-Request-ID

             When 'C'
               Move CA-B-Postcode  To WF-B-Postcode
               Move CA-B-Status    To WF-B-Status
               Move CA-B-Customer  To WF-B-Customer

             When 'E'
               Move CA-E-WITH-PROFITS To  WF-E-WITH-PROFITS
               Move CA-E-EQUITIES     To  WF-E-EQUITIES
               Move CA-E-MANAGED-FUND To  WF-E-MANAGED-FUND
               Move CA-E-FUND-NAME    To  WF-E-FUND-NAME
               Move CA-E-LIFE-ASSURED To  WF-E-LIFE-ASSURED

             When 'H'
               Move CA-H-PROPERTY-TYPE To  WF-H-PROPERTY-TYPE
               Move CA-H-BEDROOMS      To  WF-H-BEDROOMS
               Move CA-H-VALUE         To  WF-H-VALUE
               Move CA-H-POSTCODE      To  WF-H-POSTCODE
               Move CA-H-HOUSE-NAME    To  WF-H-HOUSE-NAME

             When 'M'
               Move CA-M-MAKE          To  WF-M-MAKE
               Move CA-M-MODEL         To  WF-M-MODEL
               Move CA-M-VALUE         To  WF-M-VALUE
               Move CA-M-REGNUMBER     To  WF-M-REGNUMBER

             When Other
               Move Spaces To WF-Policy-Data
           End-Evaluate

           Move CA-Policy-Num      To WF-Policy-Num
      *---------------------------------------------------------------*
           Exec CICS Read File('KSDSPOLY')
                     Into(WS-FileIn)
                     Length(WS-Commarea-Len)
                     Ridfld(WF-Policy-Key)
                     KeyLength(21)
                     RESP(WS-RESP)
                     Update
           End-Exec.
           If WS-RESP Not = DFHRESP(NORMAL)
             Move EIBRESP2 To WS-RESP2
             MOVE '81' TO CA-RETURN-CODE
             PERFORM WRITE-ERROR-MESSAGE
             EXEC CICS ABEND ABCODE('LGV3') NODUMP END-EXEC
             EXEC CICS RETURN END-EXEC
           End-If.
      *---------------------------------------------------------------*
           Exec CICS ReWrite File('KSDSPOLY')
                     From(WF-Policy-Info)
                     Length(WS-Commarea-LenF)
                     RESP(WS-RESP)
           End-Exec.
           If WS-RESP Not = DFHRESP(NORMAL)
             Move EIBRESP2 To WS-RESP2
             MOVE '82' TO CA-RETURN-CODE
             PERFORM WRITE-ERROR-MESSAGE
             EXEC CICS ABEND ABCODE('LGV4') NODUMP END-EXEC
             EXEC CICS RETURN END-EXEC
           End-If.

      *---------------------------------------------------------------*

       A-EXIT.
           EXIT.
           GOBACK.
      *================================================================*
      * Procedure to write error message to Queues                     *
      *   Uses common error handling from LGERR/LGERRPRC copybooks     *
      *================================================================*
       WRITE-ERROR-MESSAGE.
      * Format time and date
           PERFORM LGERR-FORMAT-TIME
           MOVE WS-ERR-DATE TO EM-DATE
           MOVE WS-ERR-TIME TO EM-TIME
      * Populate VSAM-specific error fields
           Move CA-Customer-Num To EM-Cusnum
           Move CA-Policy-Num   To EM-POLNUM
           Move WS-RESP         To EM-RespRC
           Move WS-RESP2        To EM-Resp2RC
      * Write error message and commarea to TSQ
           PERFORM LGERR-WRITE-MSG
           PERFORM LGERR-LOG-COMMAREA
           .

      *----------------------------------------------------------------*
      * Common error handling procedures from copybook                 *
      *----------------------------------------------------------------*
           COPY LGERRPRC.
