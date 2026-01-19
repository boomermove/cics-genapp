       CBL CICS('SP,EDF')
      *================================================================*
      * Licensed Materials - Property of IBM                          *
      * GENAPP - CICS GenApp Authentication Service                   *
      * (C) Copyright IBM Corp. 2023. All Rights Reserved             *
      * US Government Users Restricted Rights - Use, duplication or    *
      * disclosure restricted by GSA ADP Schedule Contract with IBM    *
      * Corp                                                           *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAUTH01.
       AUTHOR. GENAPP SECURITY TEAM.
       DATE-WRITTEN. 2023.

      *================================================================*
      * Program: LGAUTH01 - Authentication Service                    *
      * Purpose: Provides secure user authentication services         *
      *          replacing hardcoded password authentication          *
      * Functions:                                                     *
      *   AUTH     - Authenticate user credentials                    *
      *   CHGPASS  - Change user password                            *
      *   CREATEU  - Create new user account                         *
      *   LOCKUSER - Lock user account                               *
      *   UNLKUSER - Unlock user account                             *
      * Security: Uses z/OS ICSF for cryptographic operations         *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Common definitions                                              *
      *----------------------------------------------------------------*
       01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16) VALUE 'LGAUTH01----WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X(1).
           03 WS-ADDR-DFHCOMMAREA      POINTER.
           03 WS-CALEN                 PIC S9(8) COMP.
           03 WS-RESP                  PIC 9(8) COMP.
           03 WS-RESP2                 PIC 9(8) COMP.

      *----------------------------------------------------------------*
      * SQL Communication Area                                         *
      *----------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA END-EXEC.

      *----------------------------------------------------------------*
      * Time and date                                                  *
      *----------------------------------------------------------------*
       01  WS-ABSTIME                  PIC S9(15) COMP-3.
       01  WS-FORMATTED-TIME           PIC X(26).
       01  WS-DATE                     PIC X(10).
       01  WS-TIME                     PIC X(8).

      *----------------------------------------------------------------*
      * Working variables                                              *
      *----------------------------------------------------------------*
       01  WS-WORK-VARS.
           05  WS-RETURN-CODE          PIC X(2) VALUE '00'.
           05  WS-ERROR-MSG            PIC X(100).
           05  WS-CUSTOMER-NUM-CHAR    PIC X(10).
           05  WS-DATE-WORK            PIC X(10).
           05  WS-ATTEMPTS-REMAINING   PIC 9(2).
           05  WS-LOCKOUT-EXPIRED      PIC X(1) VALUE 'N'.
           05  WS-PASSWORD-EXPIRED     PIC X(1) VALUE 'N'.
           05  WS-DAYS-SINCE-CHANGE    PIC 9(3).
           05  WS-USER-FOUND           PIC X(1) VALUE 'N'.
           05  WS-HISTORY-MATCH        PIC X(1) VALUE 'N'.
           05  WS-HISTORY-COUNT        PIC 9(2) VALUE 0.

      *----------------------------------------------------------------*
      * Database host variables                                        *
      *----------------------------------------------------------------*
       01  DB2-USER-SECURITY.
           05  DB2-CUSTOMERNUM-INT     PIC S9(9) COMP.
           05  DB2-USERNAME            PIC X(32).
           05  DB2-PASSWORD-HASH       PIC X(128).
           05  DB2-SALT                PIC X(32).
           05  DB2-HASH-ALGORITHM      PIC X(8).
           05  DB2-PASSWORD-DATE       PIC X(10).
           05  DB2-LAST-LOGIN          PIC X(26).
           05  DB2-LOGIN-ATTEMPTS      PIC S9(4) COMP.
           05  DB2-ACCOUNT-STATUS      PIC X(1).
           05  DB2-LOCKOUT-TIME        PIC X(26).
           05  DB2-CREATED-DATE        PIC X(10).
           05  DB2-MODIFIED-DATE       PIC X(10).

      *----------------------------------------------------------------*
      * Password history host variables                                *
      *----------------------------------------------------------------*
       01  DB2-PASSWORD-HISTORY.
           05  DB2-HIST-CUSTOMERNUM    PIC S9(9) COMP.
           05  DB2-HIST-SEQUENCE       PIC S9(4) COMP.
           05  DB2-HIST-PASSWORD-HASH  PIC X(128).
           05  DB2-HIST-CREATED-DATE   PIC X(10).

      *----------------------------------------------------------------*
      * Password hashing communication area                            *
      *----------------------------------------------------------------*
       01  HASH-COMMAREA.
           05  HASH-FUNCTION           PIC X(8).
           05  HASH-PASSWORD           PIC X(64).
           05  HASH-SALT               PIC X(32).
           05  HASH-HASH               PIC X(128).
           05  HASH-RETURN-CODE        PIC X(2).
           05  HASH-ERROR-MSG          PIC X(100).
           05  HASH-FILLER             PIC X(598).

      *----------------------------------------------------------------*
      * ICSF Interface - CSNBRNGL (Random Number Generate Long)       *
      * Uses z/OS ICSF for cryptographically secure random bytes      *
      *----------------------------------------------------------------*
       01  ICSF-RANDOM-PARMS.
           05  ICSF-RNG-RC             PIC S9(8) COMP VALUE 0.
           05  ICSF-RNG-RS             PIC S9(8) COMP VALUE 0.
           05  ICSF-RNG-EXIT-LEN       PIC S9(8) COMP VALUE 0.
           05  ICSF-RNG-EXIT-DATA      PIC X(4) VALUE SPACES.
           05  ICSF-RNG-RULE-COUNT     PIC S9(8) COMP VALUE 1.
           05  ICSF-RNG-RULE-ARRAY     PIC X(8) VALUE 'RANDOM  '.
           05  ICSF-RNG-SEED           PIC X(8) VALUE SPACES.
           05  ICSF-RNG-OUTPUT-LEN     PIC S9(8) COMP VALUE 48.
           05  ICSF-RNG-OUTPUT         PIC X(48).

      *----------------------------------------------------------------*
      * Session token work area                                        *
      *----------------------------------------------------------------*
       01  WS-TOKEN-WORK.
           05  WS-TOKEN-SEED           PIC 9(15).
           05  WS-TOKEN-COUNTER        PIC 9(2).
           05  WS-TOKEN-CHAR           PIC X(1).
           05  WS-TOKEN-CHARS          PIC X(62) VALUE
               'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'.
           05  WS-TOKEN-INDEX          PIC 9(2).
           05  WS-BYTE-VALUE           PIC 9(3) COMP.
           05  WS-HEX-TABLE            PIC X(16)
               VALUE '0123456789ABCDEF'.
           05  WS-HIGH-NIBBLE          PIC 9(1) COMP.
           05  WS-LOW-NIBBLE           PIC 9(1) COMP.
           05  WS-HEX-PAIR             PIC X(2).
           05  WS-BYTE-INDEX           PIC 9(2) COMP.

      *----------------------------------------------------------------*
      * Audit logging                                                  *
      *----------------------------------------------------------------*
       01  AUDIT-WORK.
           05  AW-ACTION               PIC X(20).
           05  AW-RESULT               PIC X(2).
           05  AW-ERROR-CODE           PIC X(10).
           05  AW-DETAILS              PIC X(200).

      *----------------------------------------------------------------*
      * Error message                                                  *
      *----------------------------------------------------------------*
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGAUTH01'.
           03 EM-FUNCTION              PIC X(10) VALUE SPACES.
           03 FILLER                   PIC X(2)  VALUE SPACES.
           03 EM-SQLRC                 PIC +9(5) VALUE ZERO.
           03 FILLER                   PIC X(2)  VALUE SPACES.
           03 EM-RESP                  PIC 9(8)  VALUE ZERO.
           03 FILLER                   PIC X(1)  VALUE SPACES.
           03 EM-RESP2                 PIC 9(8)  VALUE ZERO.
           03 FILLER                   PIC X(1)  VALUE SPACES.
           03 EM-TASKNUM               PIC 9(7)  VALUE ZERO.

      *----------------------------------------------------------------*
      * Commarea                                                       *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       COPY LGSECUR.

       01  DFHCOMMAREA.
           COPY LGSECUR.

       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
      * Main processing                                                *
      *----------------------------------------------------------------*
       MAINLINE SECTION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-REQUEST
           PERFORM 9000-RETURN
           .

       1000-INITIALIZE.
           MOVE EIBTRNID    TO WS-TRANSID
           MOVE EIBTRMID    TO WS-TERMID
           MOVE EIBTASKN    TO WS-TASKNUM

           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA
           MOVE EIBCALEN TO WS-CALEN

           IF WS-CALEN < LENGTH OF AUTH-REQUEST
              MOVE '98' TO WS-RETURN-CODE
              MOVE 'Invalid commarea length' TO WS-ERROR-MSG
              MOVE 'INVALID_COMMAREA' TO AW-ERROR-CODE
              MOVE 'Commarea length check failed' TO AW-DETAILS
              GO TO 9000-RETURN
           END-IF

           MOVE AR-CUSTOMER-NUM TO DB2-CUSTOMERNUM-INT
           MOVE AR-USERNAME TO DB2-USERNAME

           MOVE SPACES TO AW-ERROR-CODE
           MOVE SPACES TO AW-DETAILS
           .

       2000-PROCESS-REQUEST.
           EVALUATE TRUE
               WHEN AR-AUTHENTICATE
                   PERFORM 3000-AUTHENTICATE-USER
               WHEN AR-CHANGE-PASSWORD
                   PERFORM 4000-CHANGE-PASSWORD
               WHEN AR-CREATE-USER
                   PERFORM 5000-CREATE-USER
               WHEN AR-LOCK-USER
                   PERFORM 6000-LOCK-USER
               WHEN AR-UNLOCK-USER
                   PERFORM 7000-UNLOCK-USER
               WHEN OTHER
                   MOVE '99' TO WS-RETURN-CODE
                   MOVE 'Invalid authentication function' TO WS-ERROR-MSG
                   MOVE 'INVALID_FUNC' TO AW-ERROR-CODE
                   STRING 'Unknown function: ' DELIMITED BY SIZE
                          AR-FUNCTION DELIMITED BY SPACE
                          INTO AW-DETAILS
                   END-STRING
                   MOVE 'INVALID_FUNCTION' TO AW-ACTION
                   MOVE '99' TO AW-RESULT
                   PERFORM 8000-AUDIT-LOG
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * Authenticate user                                              *
      *----------------------------------------------------------------*
       3000-AUTHENTICATE-USER.
           MOVE 'USER_LOGIN' TO AW-ACTION
           MOVE 'N' TO WS-USER-FOUND

           PERFORM 3100-GET-USER-SECURITY
           IF WS-RETURN-CODE NOT = '00'
              MOVE WS-RETURN-CODE TO AW-RESULT
              PERFORM 8000-AUDIT-LOG
              GO TO 3000-EXIT
           END-IF

           MOVE 'Y' TO WS-USER-FOUND

           PERFORM 3200-CHECK-ACCOUNT-STATUS
           IF WS-RETURN-CODE NOT = '00'
              MOVE WS-RETURN-CODE TO AW-RESULT
              PERFORM 8000-AUDIT-LOG
              GO TO 3000-EXIT
           END-IF

           PERFORM 3300-VERIFY-PASSWORD
           IF WS-RETURN-CODE = '00'
              PERFORM 3400-SUCCESS-PROCESSING
              MOVE '00' TO AW-RESULT
              MOVE SPACES TO AW-ERROR-CODE
              MOVE 'Authentication successful' TO AW-DETAILS
           ELSE
              PERFORM 3500-FAILED-PROCESSING
              MOVE WS-RETURN-CODE TO AW-RESULT
           END-IF

           PERFORM 8000-AUDIT-LOG

       3000-EXIT.
           EXIT
           .

       3100-GET-USER-SECURITY.
           EXEC SQL
               SELECT CUSTOMERNUM,
                      USERNAME,
                      PASSWORD_HASH,
                      SALT,
                      HASH_ALGORITHM,
                      PASSWORD_DATE,
                      LAST_LOGIN,
                      LOGIN_ATTEMPTS,
                      ACCOUNT_STATUS,
                      LOCKOUT_TIME,
                      CREATED_DATE,
                      MODIFIED_DATE
               INTO :DB2-CUSTOMERNUM-INT,
                    :DB2-USERNAME,
                    :DB2-PASSWORD-HASH,
                    :DB2-SALT,
                    :DB2-HASH-ALGORITHM,
                    :DB2-PASSWORD-DATE,
                    :DB2-LAST-LOGIN,
                    :DB2-LOGIN-ATTEMPTS,
                    :DB2-ACCOUNT-STATUS,
                    :DB2-LOCKOUT-TIME,
                    :DB2-CREATED-DATE,
                    :DB2-MODIFIED-DATE
               FROM USER_SECURITY
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
                 OR  USERNAME = :DB2-USERNAME
           END-EXEC

           IF SQLCODE = 100
              MOVE '01' TO WS-RETURN-CODE
              MOVE 'Invalid credentials' TO WS-ERROR-MSG
              MOVE 'USER_NOT_FOUND' TO AW-ERROR-CODE
              MOVE 'User lookup returned no results' TO AW-DETAILS
           ELSE
              IF SQLCODE NOT = 0
                 MOVE '90' TO WS-RETURN-CODE
                 MOVE 'Authentication service error' TO WS-ERROR-MSG
                 MOVE 'DB2_ERROR' TO AW-ERROR-CODE
                 STRING 'SQLCODE=' DELIMITED BY SIZE
                        SQLCODE DELIMITED BY SIZE
                        ' during user lookup' DELIMITED BY SIZE
                        INTO AW-DETAILS
                 END-STRING
                 PERFORM WRITE-ERROR-MESSAGE
              ELSE
                 MOVE 'Y' TO WS-USER-FOUND
              END-IF
           END-IF
           .

       3200-CHECK-ACCOUNT-STATUS.
           IF DB2-ACCOUNT-STATUS = 'L'
              PERFORM 3210-CHECK-LOCKOUT-EXPIRY
              IF WS-LOCKOUT-EXPIRED = 'N'
                 MOVE '02' TO WS-RETURN-CODE
                 MOVE 'Account is temporarily locked' TO WS-ERROR-MSG
                 MOVE 'ACCOUNT_LOCKED' TO AW-ERROR-CODE
                 MOVE 'Account locked due to failed attempts' TO AW-DETAILS
                 GO TO 3200-EXIT
              END-IF
           END-IF

           IF DB2-ACCOUNT-STATUS = 'S'
              MOVE '02' TO WS-RETURN-CODE
              MOVE 'Account is suspended' TO WS-ERROR-MSG
              MOVE 'ACCOUNT_SUSPENDED' TO AW-ERROR-CODE
              MOVE 'Account administratively suspended' TO AW-DETAILS
              GO TO 3200-EXIT
           END-IF

           PERFORM 3220-CHECK-PASSWORD-EXPIRY
           IF WS-PASSWORD-EXPIRED = 'Y'
              MOVE '03' TO WS-RETURN-CODE
              MOVE 'Password has expired' TO WS-ERROR-MSG
              MOVE 'PASSWORD_EXPIRED' TO AW-ERROR-CODE
              STRING 'Password expired after ' DELIMITED BY SIZE
                     WS-DAYS-SINCE-CHANGE DELIMITED BY SIZE
                     ' days' DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
           END-IF

       3200-EXIT.
           EXIT
           .

       3210-CHECK-LOCKOUT-EXPIRY.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP('-') TIMESEP(':')
                     YYDDD DDMMYYYY DATE(WS-DATE-WORK)
                     TIME(WS-TIME)
           END-EXEC

           IF WS-DATE-WORK > DB2-LOCKOUT-TIME(1:10)
              MOVE 'Y' TO WS-LOCKOUT-EXPIRED
              PERFORM 3211-UNLOCK-ACCOUNT
           END-IF
           .

       3211-UNLOCK-ACCOUNT.
           EXEC SQL
               UPDATE USER_SECURITY
               SET ACCOUNT_STATUS = 'A',
                   LOGIN_ATTEMPTS = 0,
                   LOCKOUT_TIME = NULL,
                   MODIFIED_DATE = :WS-DATE-WORK
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           IF SQLCODE NOT = 0
              MOVE '90' TO WS-RETURN-CODE
              MOVE 'Failed to unlock account' TO WS-ERROR-MSG
              MOVE 'UNLOCK_FAILED' TO AW-ERROR-CODE
              STRING 'SQLCODE=' DELIMITED BY SIZE
                     SQLCODE DELIMITED BY SIZE
                     ' during auto-unlock' DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              PERFORM WRITE-ERROR-MESSAGE
           ELSE
              MOVE 0 TO DB2-LOGIN-ATTEMPTS
              MOVE 'A' TO DB2-ACCOUNT-STATUS
           END-IF
           .

       3220-CHECK-PASSWORD-EXPIRY.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP DDMMYYYY DATE(WS-DATE-WORK)
           END-EXEC

           COMPUTE WS-DAYS-SINCE-CHANGE =
               FUNCTION INTEGER-OF-DATE(FUNCTION NUMVAL(WS-DATE-WORK)) -
               FUNCTION INTEGER-OF-DATE(FUNCTION NUMVAL(DB2-PASSWORD-DATE))

           IF WS-DAYS-SINCE-CHANGE > SC-PASSWORD-EXPIRY-DAYS
              MOVE 'Y' TO WS-PASSWORD-EXPIRED
           END-IF

           COMPUTE AS-DAYS-TO-EXPIRY =
               SC-PASSWORD-EXPIRY-DAYS - WS-DAYS-SINCE-CHANGE
           .

       3300-VERIFY-PASSWORD.
           MOVE 'VERIFY  ' TO HASH-FUNCTION
           MOVE AR-PASSWORD TO HASH-PASSWORD
           MOVE DB2-SALT TO HASH-SALT
           MOVE DB2-PASSWORD-HASH TO HASH-HASH

           EXEC CICS LINK Program('LGPWHASH')
                     Commarea(HASH-COMMAREA)
                     LENGTH(LENGTH OF HASH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
              MOVE '90' TO WS-RETURN-CODE
              MOVE 'Authentication service error' TO WS-ERROR-MSG
              MOVE 'HASH_SVC_ERROR' TO AW-ERROR-CODE
              STRING 'LGPWHASH LINK failed RESP=' DELIMITED BY SIZE
                     WS-RESP DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              PERFORM WRITE-ERROR-MESSAGE
           ELSE
              IF HASH-RETURN-CODE NOT = '00'
                 MOVE '01' TO WS-RETURN-CODE
                 MOVE 'Invalid credentials' TO WS-ERROR-MSG
                 MOVE 'INVALID_PASSWORD' TO AW-ERROR-CODE
                 MOVE 'Password verification failed' TO AW-DETAILS
              END-IF
           END-IF
           .

       3400-SUCCESS-PROCESSING.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP('-') TIMESEP(':')
                     YYDDD DDMMYYYY DATE(WS-DATE-WORK)
                     TIME(WS-TIME)
           END-EXEC

           STRING WS-DATE-WORK DELIMITED BY SPACE
                  'T' DELIMITED BY SIZE
                  WS-TIME DELIMITED BY SPACE
                  INTO WS-FORMATTED-TIME
           END-STRING

           EXEC SQL
               UPDATE USER_SECURITY
               SET LAST_LOGIN = :WS-FORMATTED-TIME,
                   LOGIN_ATTEMPTS = 0,
                   MODIFIED_DATE = :WS-DATE-WORK
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           IF SQLCODE NOT = 0
              PERFORM WRITE-ERROR-MESSAGE
           END-IF

           MOVE DB2-LAST-LOGIN TO AS-LAST-LOGIN
           PERFORM 3410-GENERATE-SESSION-TOKEN
           .

      *----------------------------------------------------------------*
      * Generate session token using ICSF CSNBRNGL                    *
      * Uses cryptographically secure random number generation        *
      *----------------------------------------------------------------*
       3410-GENERATE-SESSION-TOKEN.
           MOVE 0 TO ICSF-RNG-RC
           MOVE 0 TO ICSF-RNG-RS
           MOVE 48 TO ICSF-RNG-OUTPUT-LEN
           MOVE SPACES TO ICSF-RNG-OUTPUT

           CALL 'CSNBRNGL' USING
               ICSF-RNG-RC
               ICSF-RNG-RS
               ICSF-RNG-EXIT-LEN
               ICSF-RNG-EXIT-DATA
               ICSF-RNG-RULE-COUNT
               ICSF-RNG-RULE-ARRAY
               ICSF-RNG-SEED
               ICSF-RNG-OUTPUT-LEN
               ICSF-RNG-OUTPUT
           END-CALL

           IF ICSF-RNG-RC NOT = 0
              MOVE '90' TO WS-RETURN-CODE
              MOVE 'Token generation failed' TO WS-ERROR-MSG
              MOVE 'ICSF_RNG_ERROR' TO AW-ERROR-CODE
              STRING 'CSNBRNGL RC=' DELIMITED BY SIZE
                     ICSF-RNG-RC DELIMITED BY SIZE
                     ' RS=' DELIMITED BY SIZE
                     ICSF-RNG-RS DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              PERFORM WRITE-ERROR-MESSAGE
           ELSE
              PERFORM 3411-CONVERT-TOKEN-TO-HEX
           END-IF

           PERFORM 3412-SET-TOKEN-EXPIRY
           .

      *----------------------------------------------------------------*
      * Convert 32 random bytes to 64-char hex token                  *
      *----------------------------------------------------------------*
       3411-CONVERT-TOKEN-TO-HEX.
           MOVE SPACES TO AS-SESSION-TOKEN

           PERFORM VARYING WS-BYTE-INDEX FROM 1 BY 1
                   UNTIL WS-BYTE-INDEX > 32
               MOVE FUNCTION ORD(ICSF-RNG-OUTPUT(WS-BYTE-INDEX:1))
                    TO WS-BYTE-VALUE
               SUBTRACT 1 FROM WS-BYTE-VALUE

               DIVIDE WS-BYTE-VALUE BY 16
                   GIVING WS-HIGH-NIBBLE
                   REMAINDER WS-LOW-NIBBLE

               ADD 1 TO WS-HIGH-NIBBLE
               ADD 1 TO WS-LOW-NIBBLE

               MOVE WS-HEX-TABLE(WS-HIGH-NIBBLE:1)
                    TO WS-HEX-PAIR(1:1)
               MOVE WS-HEX-TABLE(WS-LOW-NIBBLE:1)
                    TO WS-HEX-PAIR(2:1)

               COMPUTE WS-TOKEN-COUNTER = (WS-BYTE-INDEX - 1) * 2 + 1
               MOVE WS-HEX-PAIR TO AS-SESSION-TOKEN(WS-TOKEN-COUNTER:2)
           END-PERFORM
           .

       3412-SET-TOKEN-EXPIRY.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           COMPUTE WS-ABSTIME = WS-ABSTIME + (SC-SESSION-TIMEOUT * 100)
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP('-') TIMESEP(':')
                     YYDDD DDMMYYYY DATE(WS-DATE-WORK)
                     TIME(WS-TIME)
           END-EXEC

           STRING WS-DATE-WORK DELIMITED BY SPACE
                  'T' DELIMITED BY SIZE
                  WS-TIME DELIMITED BY SPACE
                  INTO AS-EXPIRY-TIME
           END-STRING
           .

       3500-FAILED-PROCESSING.
           ADD 1 TO DB2-LOGIN-ATTEMPTS

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP DDMMYYYY DATE(WS-DATE-WORK)
           END-EXEC

           EXEC SQL
               UPDATE USER_SECURITY
               SET LOGIN_ATTEMPTS = :DB2-LOGIN-ATTEMPTS,
                   MODIFIED_DATE = :WS-DATE-WORK
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           COMPUTE WS-ATTEMPTS-REMAINING =
               SC-MAX-LOGIN-ATTEMPTS - DB2-LOGIN-ATTEMPTS
           MOVE WS-ATTEMPTS-REMAINING TO AS-ATTEMPTS-REMAINING

           MOVE 'LOGIN_FAILED' TO AW-ERROR-CODE
           STRING 'Failed attempt ' DELIMITED BY SIZE
                  DB2-LOGIN-ATTEMPTS DELIMITED BY SIZE
                  ' of ' DELIMITED BY SIZE
                  SC-MAX-LOGIN-ATTEMPTS DELIMITED BY SIZE
                  INTO AW-DETAILS
           END-STRING

           IF DB2-LOGIN-ATTEMPTS >= SC-MAX-LOGIN-ATTEMPTS
              PERFORM 3510-LOCK-ACCOUNT
           END-IF
           .

       3510-LOCK-ACCOUNT.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           COMPUTE WS-ABSTIME = WS-ABSTIME + (SC-LOCKOUT-DURATION * 100)
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP('-') TIMESEP(':')
                     YYDDD DDMMYYYY DATE(WS-DATE-WORK)
                     TIME(WS-TIME)
           END-EXEC

           STRING WS-DATE-WORK DELIMITED BY SPACE
                  'T' DELIMITED BY SIZE
                  WS-TIME DELIMITED BY SPACE
                  INTO WS-FORMATTED-TIME
           END-STRING

           EXEC SQL
               UPDATE USER_SECURITY
               SET ACCOUNT_STATUS = 'L',
                   LOCKOUT_TIME = :WS-FORMATTED-TIME,
                   MODIFIED_DATE = :WS-DATE-WORK
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           MOVE '02' TO WS-RETURN-CODE
           MOVE 'Account locked due to excessive login attempts'
                TO WS-ERROR-MSG
           MOVE 'ACCOUNT_LOCKED' TO AW-ERROR-CODE
           STRING 'Locked after ' DELIMITED BY SIZE
                  SC-MAX-LOGIN-ATTEMPTS DELIMITED BY SIZE
                  ' failed attempts until ' DELIMITED BY SIZE
                  WS-FORMATTED-TIME DELIMITED BY SPACE
                  INTO AW-DETAILS
           END-STRING
           .

      *----------------------------------------------------------------*
      * Change password                                                *
      *----------------------------------------------------------------*
       4000-CHANGE-PASSWORD.
           MOVE 'CHANGE_PASSWORD' TO AW-ACTION

           PERFORM 3100-GET-USER-SECURITY
           IF WS-RETURN-CODE NOT = '00'
              MOVE WS-RETURN-CODE TO AW-RESULT
              PERFORM 8000-AUDIT-LOG
              GO TO 4000-EXIT
           END-IF

           PERFORM 3300-VERIFY-PASSWORD
           IF WS-RETURN-CODE NOT = '00'
              MOVE 'CURRENT_PW_INVALID' TO AW-ERROR-CODE
              MOVE 'Current password verification failed' TO AW-DETAILS
              MOVE WS-RETURN-CODE TO AW-RESULT
              PERFORM 8000-AUDIT-LOG
              GO TO 4000-EXIT
           END-IF

           MOVE 'VALIDATE' TO HASH-FUNCTION
           MOVE AR-NEW-PASSWORD TO HASH-PASSWORD

           EXEC CICS LINK Program('LGPWHASH')
                     Commarea(HASH-COMMAREA)
                     LENGTH(LENGTH OF HASH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF HASH-RETURN-CODE NOT = '00'
              MOVE HASH-RETURN-CODE TO WS-RETURN-CODE
              MOVE HASH-ERROR-MSG TO WS-ERROR-MSG
              MOVE 'POLICY_VIOLATION' TO AW-ERROR-CODE
              MOVE HASH-ERROR-MSG TO AW-DETAILS
              MOVE WS-RETURN-CODE TO AW-RESULT
              PERFORM 8000-AUDIT-LOG
              GO TO 4000-EXIT
           END-IF

           PERFORM 4050-CHECK-PASSWORD-HISTORY
           IF WS-HISTORY-MATCH = 'Y'
              MOVE '04' TO WS-RETURN-CODE
              MOVE 'Cannot reuse recent password' TO WS-ERROR-MSG
              MOVE 'PASSWORD_REUSE' TO AW-ERROR-CODE
              STRING 'Password matches one of last '
                     DELIMITED BY SIZE
                     PP-HISTORY-COUNT DELIMITED BY SIZE
                     ' passwords' DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              MOVE WS-RETURN-CODE TO AW-RESULT
              PERFORM 8000-AUDIT-LOG
              GO TO 4000-EXIT
           END-IF

           PERFORM 4100-HASH-NEW-PASSWORD
           PERFORM 4150-SAVE-PASSWORD-HISTORY
           PERFORM 4200-UPDATE-PASSWORD

           MOVE '00' TO WS-RETURN-CODE
           MOVE SPACES TO AW-ERROR-CODE
           MOVE 'Password changed successfully' TO AW-DETAILS
           MOVE '00' TO AW-RESULT
           PERFORM 8000-AUDIT-LOG

       4000-EXIT.
           EXIT
           .

      *----------------------------------------------------------------*
      * Check if new password matches any in history                  *
      *----------------------------------------------------------------*
       4050-CHECK-PASSWORD-HISTORY.
           MOVE 'N' TO WS-HISTORY-MATCH

           MOVE 'HASH    ' TO HASH-FUNCTION
           MOVE AR-NEW-PASSWORD TO HASH-PASSWORD
           MOVE DB2-SALT TO HASH-SALT

           EXEC CICS LINK Program('LGPWHASH')
                     Commarea(HASH-COMMAREA)
                     LENGTH(LENGTH OF HASH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF HASH-HASH = DB2-PASSWORD-HASH
              MOVE 'Y' TO WS-HISTORY-MATCH
              GO TO 4050-EXIT
           END-IF

           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-HISTORY-COUNT
               FROM PASSWORD_HISTORY
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
                 AND PASSWORD_HASH = :HASH-HASH
                 AND SEQUENCE <= :PP-HISTORY-COUNT
           END-EXEC

           IF WS-HISTORY-COUNT > 0
              MOVE 'Y' TO WS-HISTORY-MATCH
           END-IF

       4050-EXIT.
           EXIT
           .

       4100-HASH-NEW-PASSWORD.
           MOVE 'GENSALT ' TO HASH-FUNCTION
           MOVE AR-NEW-PASSWORD TO HASH-PASSWORD

           EXEC CICS LINK Program('LGPWHASH')
                     Commarea(HASH-COMMAREA)
                     LENGTH(LENGTH OF HASH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           MOVE 'HASH    ' TO HASH-FUNCTION

           EXEC CICS LINK Program('LGPWHASH')
                     Commarea(HASH-COMMAREA)
                     LENGTH(LENGTH OF HASH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC
           .

      *----------------------------------------------------------------*
      * Save current password to history before changing              *
      *----------------------------------------------------------------*
       4150-SAVE-PASSWORD-HISTORY.
           EXEC SQL
               UPDATE PASSWORD_HISTORY
               SET SEQUENCE = SEQUENCE + 1
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           EXEC SQL
               DELETE FROM PASSWORD_HISTORY
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
                 AND SEQUENCE > :PP-HISTORY-COUNT
           END-EXEC

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP DDMMYYYY DATE(WS-DATE-WORK)
           END-EXEC

           EXEC SQL
               INSERT INTO PASSWORD_HISTORY
               (CUSTOMERNUM, SEQUENCE, PASSWORD_HASH, CREATED_DATE)
               VALUES
               (:DB2-CUSTOMERNUM-INT, 1, :DB2-PASSWORD-HASH,
                :WS-DATE-WORK)
           END-EXEC
           .

       4200-UPDATE-PASSWORD.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP DDMMYYYY DATE(WS-DATE-WORK)
           END-EXEC

           EXEC SQL
               UPDATE USER_SECURITY
               SET PASSWORD_HASH = :HASH-HASH,
                   SALT = :HASH-SALT,
                   PASSWORD_DATE = :WS-DATE-WORK,
                   MODIFIED_DATE = :WS-DATE-WORK
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           IF SQLCODE NOT = 0
              MOVE '90' TO WS-RETURN-CODE
              MOVE 'Failed to update password' TO WS-ERROR-MSG
              MOVE 'UPDATE_FAILED' TO AW-ERROR-CODE
              STRING 'SQLCODE=' DELIMITED BY SIZE
                     SQLCODE DELIMITED BY SIZE
                     ' during password update' DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              PERFORM WRITE-ERROR-MESSAGE
           END-IF
           .

      *----------------------------------------------------------------*
      * Create new user                                                *
      *----------------------------------------------------------------*
       5000-CREATE-USER.
           MOVE 'CREATE_USER' TO AW-ACTION

           MOVE 'VALIDATE' TO HASH-FUNCTION
           MOVE AR-PASSWORD TO HASH-PASSWORD

           EXEC CICS LINK Program('LGPWHASH')
                     Commarea(HASH-COMMAREA)
                     LENGTH(LENGTH OF HASH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF HASH-RETURN-CODE NOT = '00'
              MOVE HASH-RETURN-CODE TO WS-RETURN-CODE
              MOVE HASH-ERROR-MSG TO WS-ERROR-MSG
              MOVE 'POLICY_VIOLATION' TO AW-ERROR-CODE
              MOVE HASH-ERROR-MSG TO AW-DETAILS
              MOVE WS-RETURN-CODE TO AW-RESULT
              PERFORM 8000-AUDIT-LOG
              GO TO 5000-EXIT
           END-IF

           PERFORM 4100-HASH-NEW-PASSWORD
           PERFORM 5100-INSERT-USER-RECORD

           IF WS-RETURN-CODE = '00'
              MOVE SPACES TO AW-ERROR-CODE
              STRING 'User created: ' DELIMITED BY SIZE
                     AR-USERNAME DELIMITED BY SPACE
                     INTO AW-DETAILS
              END-STRING
           END-IF

           MOVE WS-RETURN-CODE TO AW-RESULT
           PERFORM 8000-AUDIT-LOG

       5000-EXIT.
           EXIT
           .

       5100-INSERT-USER-RECORD.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP DDMMYYYY DATE(WS-DATE-WORK)
           END-EXEC

           EXEC SQL
               INSERT INTO USER_SECURITY
               (CUSTOMERNUM, USERNAME, PASSWORD_HASH, SALT,
                HASH_ALGORITHM, PASSWORD_DATE, LAST_LOGIN,
                LOGIN_ATTEMPTS, ACCOUNT_STATUS, LOCKOUT_TIME,
                CREATED_DATE, MODIFIED_DATE)
               VALUES
               (:DB2-CUSTOMERNUM-INT, :AR-USERNAME, :HASH-HASH,
                :HASH-SALT, :SC-HASH-ALGORITHM, :WS-DATE-WORK,
                NULL, 0, 'A', NULL, :WS-DATE-WORK, :WS-DATE-WORK)
           END-EXEC

           IF SQLCODE NOT = 0
              MOVE '90' TO WS-RETURN-CODE
              MOVE 'Failed to create user record' TO WS-ERROR-MSG
              MOVE 'INSERT_FAILED' TO AW-ERROR-CODE
              STRING 'SQLCODE=' DELIMITED BY SIZE
                     SQLCODE DELIMITED BY SIZE
                     ' during user insert' DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              PERFORM WRITE-ERROR-MESSAGE
           END-IF
           .

      *----------------------------------------------------------------*
      * Lock user account                                              *
      *----------------------------------------------------------------*
       6000-LOCK-USER.
           MOVE 'LOCK_USER' TO AW-ACTION

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP('-') TIMESEP(':')
                     YYDDD DDMMYYYY DATE(WS-DATE-WORK)
                     TIME(WS-TIME)
           END-EXEC

           STRING WS-DATE-WORK DELIMITED BY SPACE
                  'T' DELIMITED BY SIZE
                  WS-TIME DELIMITED BY SPACE
                  INTO WS-FORMATTED-TIME
           END-STRING

           EXEC SQL
               UPDATE USER_SECURITY
               SET ACCOUNT_STATUS = 'L',
                   LOCKOUT_TIME = :WS-FORMATTED-TIME,
                   MODIFIED_DATE = :WS-DATE-WORK
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           IF SQLCODE = 0
              MOVE '00' TO WS-RETURN-CODE
              MOVE '00' TO AW-RESULT
              MOVE SPACES TO AW-ERROR-CODE
              STRING 'Account locked for user ' DELIMITED BY SIZE
                     AR-USERNAME DELIMITED BY SPACE
                     INTO AW-DETAILS
              END-STRING
           ELSE
              MOVE '90' TO WS-RETURN-CODE
              MOVE 'Failed to lock user account' TO WS-ERROR-MSG
              MOVE '90' TO AW-RESULT
              MOVE 'LOCK_FAILED' TO AW-ERROR-CODE
              STRING 'SQLCODE=' DELIMITED BY SIZE
                     SQLCODE DELIMITED BY SIZE
                     ' during account lock' DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              PERFORM WRITE-ERROR-MESSAGE
           END-IF

           PERFORM 8000-AUDIT-LOG
           .

      *----------------------------------------------------------------*
      * Unlock user account                                            *
      *----------------------------------------------------------------*
       7000-UNLOCK-USER.
           MOVE 'UNLOCK_USER' TO AW-ACTION

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP DDMMYYYY DATE(WS-DATE-WORK)
           END-EXEC

           EXEC SQL
               UPDATE USER_SECURITY
               SET ACCOUNT_STATUS = 'A',
                   LOGIN_ATTEMPTS = 0,
                   LOCKOUT_TIME = NULL,
                   MODIFIED_DATE = :WS-DATE-WORK
               WHERE CUSTOMERNUM = :DB2-CUSTOMERNUM-INT
           END-EXEC

           IF SQLCODE = 0
              MOVE '00' TO WS-RETURN-CODE
              MOVE '00' TO AW-RESULT
              MOVE SPACES TO AW-ERROR-CODE
              STRING 'Account unlocked for user ' DELIMITED BY SIZE
                     AR-USERNAME DELIMITED BY SPACE
                     INTO AW-DETAILS
              END-STRING
           ELSE
              MOVE '90' TO WS-RETURN-CODE
              MOVE 'Failed to unlock user account' TO WS-ERROR-MSG
              MOVE '90' TO AW-RESULT
              MOVE 'UNLOCK_FAILED' TO AW-ERROR-CODE
              STRING 'SQLCODE=' DELIMITED BY SIZE
                     SQLCODE DELIMITED BY SIZE
                     ' during account unlock' DELIMITED BY SIZE
                     INTO AW-DETAILS
              END-STRING
              PERFORM WRITE-ERROR-MESSAGE
           END-IF

           PERFORM 8000-AUDIT-LOG
           .

      *----------------------------------------------------------------*
      * Audit logging                                                  *
      *----------------------------------------------------------------*
       8000-AUDIT-LOG.
           IF SC-AUDIT-NONE
              GO TO 8000-EXIT
           END-IF

           IF SC-AUDIT-FAIL AND AW-RESULT = '00'
              GO TO 8000-EXIT
           END-IF

           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     DATESEP('-') TIMESEP(':')
                     YYDDD DDMMYYYY DATE(WS-DATE-WORK)
                     TIME(WS-TIME)
           END-EXEC

           STRING WS-DATE-WORK DELIMITED BY SPACE
                  'T' DELIMITED BY SIZE
                  WS-TIME DELIMITED BY SPACE
                  INTO AL-TIMESTAMP
           END-STRING

           MOVE AR-CUSTOMER-NUM TO AL-CUSTOMER-NUM
           MOVE AR-USERNAME TO AL-USERNAME
           MOVE AW-ACTION TO AL-ACTION
           MOVE AW-RESULT TO AL-RESULT
           MOVE AR-CLIENT-IP TO AL-CLIENT-IP
           MOVE AR-USER-AGENT TO AL-USER-AGENT
           MOVE AW-ERROR-CODE TO AL-ERROR-CODE
           MOVE AW-DETAILS TO AL-DETAILS

           EXEC SQL
               INSERT INTO AUDIT_LOG
               (TIMESTAMP, CUSTOMER_NUM, USERNAME, ACTION,
                RESULT, CLIENT_IP, USER_AGENT, ERROR_CODE, DETAILS)
               VALUES
               (:AL-TIMESTAMP, :AL-CUSTOMER-NUM, :AL-USERNAME,
                :AL-ACTION, :AL-RESULT, :AL-CLIENT-IP,
                :AL-USER-AGENT, :AL-ERROR-CODE, :AL-DETAILS)
           END-EXEC

       8000-EXIT.
           EXIT
           .

      *----------------------------------------------------------------*
      * Error handling                                                 *
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
           MOVE AR-FUNCTION TO EM-FUNCTION
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
      * Return to caller                                               *
      *----------------------------------------------------------------*
       9000-RETURN.
           MOVE WS-RETURN-CODE TO AS-RETURN-CODE
           MOVE WS-ERROR-MSG TO AS-ERROR-MESSAGE

           EXEC CICS RETURN END-EXEC
           .

       END PROGRAM LGAUTH01.
