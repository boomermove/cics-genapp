       CBL CICS('SP,EDF')
      *================================================================*
      * Licensed Materials - Property of IBM                          *
      * GENAPP - CICS GenApp - Add Customer Details (SECURE VERSION)  *
      * (C) Copyright IBM Corp. 2023. All Rights Reserved             *
      * US Government Users Restricted Rights - Use, duplication or    *
      * disclosure restricted by GSA ADP Schedule Contract with IBM    *
      * Corp                                                           *
      *================================================================*
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGACDB01.
       AUTHOR. GENAPP TEAM.
       DATE-WRITTEN. 2023.
       
      *================================================================*
      * Program: LGACDB01 - Add Customer Details (SECURE VERSION)     *
      * Purpose: Secure version with proper authentication            *
      *          Replaces hardcoded credentials with secure auth      *
      *================================================================*
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *----------------------------------------------------------------*
      * Security configuration                                         *
      *----------------------------------------------------------------*
       COPY LGSECUR.
      
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
       01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16) 
                                       VALUE 'LGACDB01----WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-ADDR-DFHCOMMAREA      POINTER.
           03 WS-CALEN                 PIC S9(8) COMP.
      
      *----------------------------------------------------------------*
      * Constants                                                      *
      *----------------------------------------------------------------*
       77 WS-PROG                      PIC X(8) VALUE 'LGACDB01'.
       77 LGAC-RETRY-TIMES             PIC 9 VALUE 9.
      
      *----------------------------------------------------------------*
      * CICS SEND RECEIVE PROCESSING                                   *
      *----------------------------------------------------------------*
       01 WS-RESP                      PIC 9(8) COMP.
       01 WS-RESP2                     PIC 9(8) COMP.
      
      *----------------------------------------------------------------*
      * Variables for time/date processing                             *
      *----------------------------------------------------------------*
       01  WS-ABSTIME                  PIC S9(15) COMP-3.
       01  WS-TIME                     PIC X(6).
       01  WS-DATE                     PIC X(8).
      
      *----------------------------------------------------------------*
      * Error Message structure                                        *
      *----------------------------------------------------------------*
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGACDB01'.
           03 EM-FUNCTION              PIC X(10) VALUE SPACES.
           03 FILLER                   PIC X(2)  VALUE SPACES.
           03 EM-SQLRC                 PIC +9(5) VALUE ZERO.
           03 FILLER                   PIC X(2)  VALUE SPACES.
           03 EM-RESP                  PIC 9(8)  VALUE ZERO.
           03 FILLER                   PIC X(1)  VALUE SPACES.
           03 EM-RESP2                 PIC 9(8)  VALUE ZERO.
           03 FILLER                   PIC X(1)  VALUE SPACES.
           03 EM-TASKNUM               PIC 9(7)  VALUE ZERO.
           03 FILLER                   PIC X(90) VALUE ALL SPACES.
      
      *----------------------------------------------------------------*
      * Named counter fields                                           *
      *----------------------------------------------------------------*
       01  WS-GENACUSTNUM-NAME         PIC X(16) VALUE 'GENACUSTNUM    '.
       01  WS-GENACUSTNUM-VALUE        PIC 9(10) VALUE ZERO.
       01  WS-GENACUSTNUM-VALUE-D      REDEFINES WS-GENACUSTNUM-VALUE
                                       PIC S9(10) COMP.
       01  WS-CUSTOMER-NUM-REC.
           05  WS-CUSTOMER-NUM         PIC 9(10).
      
      *----------------------------------------------------------------*
      * SQL communication area                                         *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE LGPOLICY END-EXEC.
      
      *----------------------------------------------------------------*
      * Host variables for DB2 Customers table                        *
      *----------------------------------------------------------------*
       01  DB2-CUST-REC.
           COPY DB2-CUSTOMER.
       01  WS-CUSTOMER-REC.
           COPY DB2-CUSTOMER.
      
      *----------------------------------------------------------------*
      * Authentication variables                                       *
      *----------------------------------------------------------------*
       01  WS-AUTH-REQUEST.
           COPY AUTH-REQUEST.
       01  WS-AUTH-RESPONSE.
           COPY AUTH-RESPONSE.
      
       01  WS-SECURITY-WORK.
           05  WS-DEFAULT-PASSWORD     PIC X(64) VALUE SPACES.
           05  WS-TEMP-PASSWORD        PIC X(16) VALUE SPACES.
      
       LINKAGE SECTION.
       COPY LGCMAREA.
       01  DFHCOMMAREA.
           COPY LGCMAREA.
      
       PROCEDURE DIVISION.
      
      *================================================================*
      * Main process                                                   *
      *================================================================*
       MAINLINE SECTION.
       
       0000-MAIN.
      
           PERFORM 1000-INIT
           
           PERFORM 2000-PROCESS
           
           PERFORM 9000-RETURN
           .
      
      *----------------------------------------------------------------*
      * Initialize                                                     *
      *----------------------------------------------------------------*
       1000-INIT.
      
           MOVE EIBTRNID    TO WS-TRANSID
           MOVE EIBTRMID    TO WS-TERMID
           MOVE EIBTASKN    TO WS-TASKNUM
           
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA
           MOVE EIBCALEN TO WS-CALEN
           
           IF WS-CALEN < 32767
               MOVE '98' TO CA-RETURN-CODE
               PERFORM 9000-RETURN
           END-IF
           
           IF CA-REQUEST-ID NOT = '01ACUS'
               MOVE '99' TO CA-RETURN-CODE
               PERFORM 9000-RETURN
           END-IF
           .
           
      *----------------------------------------------------------------*
      * Main Process                                                   *
      *----------------------------------------------------------------*
       2000-PROCESS.
      
           EXEC CICS LINK Program('LGACVS01')
                     Commarea(DFHCOMMAREA)
                     LENGTH(32767)
                     RESP(WS-RESP)
           END-EXEC
           
           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               PERFORM 9000-RETURN
           END-IF
           
           IF CA-RETURN-CODE = '00'
               PERFORM 3000-CREATE-SECURE-ACCOUNT
               PERFORM 4000-INSERT-CUSTOMER-RECORD
           END-IF
           .
           
      *----------------------------------------------------------------*
      * Create secure user account                                     *
      *----------------------------------------------------------------*
       3000-CREATE-SECURE-ACCOUNT.
      
           PERFORM 3100-GENERATE-CUSTOMER-NUMBER
           PERFORM 3200-GENERATE-DEFAULT-PASSWORD
           PERFORM 3300-CREATE-USER-ACCOUNT
           .
      
       3100-GENERATE-CUSTOMER-NUMBER.
           
           EXEC CICS ENQ RESOURCE(WS-GENACUSTNUM-NAME) 
                     RESP(WS-RESP)
           END-EXEC
           
           EXEC CICS READCOUNTER COUNTER(WS-GENACUSTNUM-NAME)
                     VALUE(WS-GENACUSTNUM-VALUE-D)
                     RESP(WS-RESP)
           END-EXEC
           
           MOVE WS-GENACUSTNUM-VALUE TO CA-CUSTOMER-NUM
           
           EXEC CICS DEQ RESOURCE(WS-GENACUSTNUM-NAME)
                     RESP(WS-RESP)
           END-EXEC
           .
      
       3200-GENERATE-DEFAULT-PASSWORD.
           
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           
           COMPUTE WS-TEMP-PASSWORD = 
               'TMP' + FUNCTION INTEGER(WS-ABSTIME / 1000000)
           
           MOVE WS-TEMP-PASSWORD TO WS-DEFAULT-PASSWORD
           .
      
       3300-CREATE-USER-ACCOUNT.
           
           MOVE 'CREATEU ' TO AR-FUNCTION
           MOVE CA-CUSTOMER-NUM TO AR-CUSTOMER-NUM
           
           STRING CA-FIRST-NAME DELIMITED BY SPACE
                  '.' DELIMITED BY SIZE
                  CA-LAST-NAME DELIMITED BY SPACE
                  INTO AR-USERNAME
           END-STRING
           
           MOVE WS-DEFAULT-PASSWORD TO AR-PASSWORD
           MOVE SPACES TO AR-NEW-PASSWORD
           MOVE SPACES TO AR-CLIENT-IP
           MOVE SPACES TO AR-USER-AGENT
           MOVE SPACES TO AR-SESSION-ID
           
           MOVE WS-AUTH-REQUEST TO DFHCOMMAREA
           
           EXEC CICS LINK Program('LGAUTH01')
                     Commarea(DFHCOMMAREA)
                     LENGTH(LENGTH OF WS-AUTH-REQUEST)
                     RESP(WS-RESP)
           END-EXEC
           
           MOVE DFHCOMMAREA TO WS-AUTH-RESPONSE
           
           IF AS-RETURN-CODE NOT = '00'
               MOVE '90' TO CA-RETURN-CODE
               MOVE AS-ERROR-MESSAGE TO ERROR-MSG(40:100)
               PERFORM WRITE-ERROR-MESSAGE
               PERFORM 9000-RETURN
           END-IF
           .
      
      *----------------------------------------------------------------*
      * Insert customer record into DB2 customer table               *
      *----------------------------------------------------------------*
       4000-INSERT-CUSTOMER-RECORD.
      
           MOVE CA-CUSTOMER-NUM    TO DB2-CUSTOMERNUM-INT
           MOVE CA-FIRST-NAME      TO DB2-FIRSTNAME
           MOVE CA-LAST-NAME       TO DB2-LASTNAME
           MOVE CA-DOB             TO DB2-DATEOFBIRTH
           MOVE CA-HOUSE-NAME      TO DB2-HOUSENAME
           MOVE CA-HOUSE-NUM       TO DB2-HOUSENUMBER
           MOVE CA-POSTCODE        TO DB2-POSTCODE
           MOVE CA-PHONE-MOBILE    TO DB2-PHONEMOBILE
           MOVE CA-PHONE-HOME      TO DB2-PHONEHOME
           MOVE CA-EMAIL-ADDRESS   TO DB2-EMAILADDRESS
      
           EXEC SQL
               INSERT INTO CUSTOMER
               ( CUSTOMERNUMBER,
                 FIRSTNAME,
                 LASTNAME,
                 DATEOFBIRTH,
                 HOUSENAME,
                 HOUSENUMBER,
                 POSTCODE,
                 PHONEMOBILE,
                 PHONEHOME,
                 EMAILADDRESS )
               VALUES ( :DB2-CUSTOMERNUM-INT,
                        :DB2-FIRSTNAME,
                        :DB2-LASTNAME,
                        :DB2-DATEOFBIRTH,
                        :DB2-HOUSENAME,
                        :DB2-HOUSENUMBER,
                        :DB2-POSTCODE,
                        :DB2-PHONEMOBILE,
                        :DB2-PHONEHOME,
                        :DB2-EMAILADDRESS )
           END-EXEC
           
           IF SQLCODE NOT EQUAL 0
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           ELSE
               MOVE '00' TO CA-RETURN-CODE
               EXEC CICS SYNCPOINT END-EXEC
           END-IF
           
           PERFORM 9000-RETURN
           .
           
      *----------------------------------------------------------------*
      * Error Message Write                                            *
      *----------------------------------------------------------------*
       WRITE-ERROR-MESSAGE.
      
           MOVE SQLCODE TO EM-SQLRC
           
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYY(WS-DATE) DATESEP
                     TIME(WS-TIME) TIMESEP
           END-EXEC
           
           MOVE WS-DATE TO EM-DATE
           MOVE WS-TIME TO EM-TIME
           MOVE WS-PROG TO EM-FUNCTION
           MOVE WS-RESP TO EM-RESP
           MOVE WS-RESP2 TO EM-RESP2
           MOVE WS-TASKNUM TO EM-TASKNUM
           
           EXEC CICS LINK Program('LGSTSQ')
                     Commarea(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
                     RESP(WS-RESP)
           END-EXEC
           .
      
      *----------------------------------------------------------------*
      * Return to calling program                                      *
      *----------------------------------------------------------------*
       9000-RETURN.
      
           EXEC CICS RETURN END-EXEC
           .
      
       END PROGRAM LGACDB01.