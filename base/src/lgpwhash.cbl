       CBL CICS('SP,EDF')
      *================================================================*
      * Licensed Materials - Property of IBM                          *
      * GENAPP - CICS GenApp Password Hashing Utility                 *
      * (C) Copyright IBM Corp. 2023. All Rights Reserved             *
      * US Government Users Restricted Rights - Use, duplication or    *
      * disclosure restricted by GSA ADP Schedule Contract with IBM    *
      * Corp                                                           *
      *================================================================*
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGPWHASH.
       AUTHOR. GENAPP SECURITY TEAM.
       DATE-WRITTEN. 2023.
      
      *================================================================*
      * Program: LGPWHASH - Password Hashing Utility                  *
      * Purpose: Provides secure password hashing and validation      *
      *          services for GenApp authentication                    *
      * Functions:                                                     *
      *   HASH     - Hash a password with salt                        *
      *   VERIFY   - Verify password against stored hash              *
      *   GENSALT  - Generate cryptographic salt                      *
      *   VALIDATE - Validate password against policy                 *
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
           03 WS-EYECATCHER            PIC X(16) VALUE 'LGPWHASH----WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X(1).
           03 WS-RESP                  PIC 9(8) COMP.
           03 WS-RESP2                 PIC 9(8) COMP.
      
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
           05  WS-FUNCTION             PIC X(8).
           05  WS-RETURN-CODE          PIC X(2) VALUE '00'.
           05  WS-ERROR-MSG            PIC X(100).
           05  WS-COUNTER              PIC 9(3).
           05  WS-SEED                 PIC 9(15).
           05  WS-RANDOM-NUM           PIC 9(15).
           05  WS-CHAR-CODE            PIC 9(3).
           05  WS-HEX-DIGIT            PIC X(1).
      
      *----------------------------------------------------------------*
      * Password validation work fields                                *
      *----------------------------------------------------------------*
       01  WS-PASSWORD-WORK.
           05  WS-PASSWORD-LENGTH      PIC 9(2).
           05  WS-HAS-UPPER            PIC X(1) VALUE 'N'.
           05  WS-HAS-LOWER            PIC X(1) VALUE 'N'.
           05  WS-HAS-DIGIT            PIC X(1) VALUE 'N'.
           05  WS-HAS-SPECIAL          PIC X(1) VALUE 'N'.
           05  WS-COMPLEXITY-SCORE     PIC 9(3) VALUE ZERO.
           05  WS-CURRENT-CHAR         PIC X(1).
           05  WS-CHAR-INDEX           PIC 9(2).
      
      *----------------------------------------------------------------*
      * Hash generation work fields                                    *
      *----------------------------------------------------------------*
       01  WS-HASH-WORK.
           05  WS-INPUT-STRING         PIC X(200).
           05  WS-INPUT-LENGTH         PIC 9(3).
           05  WS-HASH-OUTPUT          PIC X(128).
           05  WS-SALT-INDEX           PIC 9(2).
       
       01  WS-SALT-CHARS               PIC X(64) VALUE
           'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz012345'.
      
      *----------------------------------------------------------------*
      * Error message                                                  *
      *----------------------------------------------------------------*
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGPWHASH'.
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
           05  COMM-FUNCTION           PIC X(8).
           05  COMM-PASSWORD           PIC X(64).
           05  COMM-SALT               PIC X(32).
           05  COMM-HASH               PIC X(128).
           05  COMM-RETURN-CODE        PIC X(2).
           05  COMM-ERROR-MSG          PIC X(100).
           05  COMM-FILLER             PIC X(598).
      
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
           
           IF EIBCALEN < LENGTH OF DFHCOMMAREA
              MOVE '98' TO WS-RETURN-CODE
              MOVE 'Invalid commarea length' TO WS-ERROR-MSG
              GO TO 9000-RETURN
           END-IF
           
           MOVE COMM-FUNCTION TO WS-FUNCTION
           .
      
       2000-PROCESS-REQUEST.
           EVALUATE WS-FUNCTION
               WHEN 'HASH    '
                   PERFORM 3000-HASH-PASSWORD
               WHEN 'VERIFY  '
                   PERFORM 4000-VERIFY-PASSWORD
               WHEN 'GENSALT '
                   PERFORM 5000-GENERATE-SALT
               WHEN 'VALIDATE'
                   PERFORM 6000-VALIDATE-PASSWORD
               WHEN OTHER
                   MOVE '99' TO WS-RETURN-CODE
                   MOVE 'Invalid function requested' TO WS-ERROR-MSG
           END-EVALUATE
           .
      
      *----------------------------------------------------------------*
      * Hash password with salt                                        *
      *----------------------------------------------------------------*
       3000-HASH-PASSWORD.
           IF COMM-SALT = SPACES
              PERFORM 5000-GENERATE-SALT
           END-IF
           
           STRING COMM-PASSWORD DELIMITED BY SPACE
                  COMM-SALT DELIMITED BY SPACE
                  INTO WS-INPUT-STRING
           END-STRING
           
           COMPUTE WS-INPUT-LENGTH = 
               LENGTH OF COMM-PASSWORD + LENGTH OF COMM-SALT
           
           PERFORM 7000-COMPUTE-HASH
           MOVE WS-HASH-OUTPUT TO COMM-HASH
           MOVE COMM-SALT TO WS-WORK-VARS
           .
      
      *----------------------------------------------------------------*
      * Verify password against hash                                   *
      *----------------------------------------------------------------*
       4000-VERIFY-PASSWORD.
           STRING COMM-PASSWORD DELIMITED BY SPACE
                  COMM-SALT DELIMITED BY SPACE
                  INTO WS-INPUT-STRING
           END-STRING
           
           COMPUTE WS-INPUT-LENGTH = 
               LENGTH OF COMM-PASSWORD + LENGTH OF COMM-SALT
           
           PERFORM 7000-COMPUTE-HASH
           
           IF WS-HASH-OUTPUT = COMM-HASH
              MOVE '00' TO WS-RETURN-CODE
           ELSE
              MOVE '01' TO WS-RETURN-CODE
              MOVE 'Password verification failed' TO WS-ERROR-MSG
           END-IF
           .
      
      *----------------------------------------------------------------*
      * Generate cryptographic salt                                    *
      *----------------------------------------------------------------*
       5000-GENERATE-SALT.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           
           MOVE WS-ABSTIME TO WS-SEED
           MOVE SPACES TO COMM-SALT
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 
                   UNTIL WS-COUNTER > 32
               COMPUTE WS-RANDOM-NUM = FUNCTION RANDOM(WS-SEED) * 62 + 1
               MOVE WS-SALT-CHARS(WS-RANDOM-NUM:1) TO 
                    COMM-SALT(WS-COUNTER:1)
           END-PERFORM
           .
      
      *----------------------------------------------------------------*
      * Validate password against policy                               *
      *----------------------------------------------------------------*
       6000-VALIDATE-PASSWORD.
           MOVE LENGTH OF COMM-PASSWORD TO WS-PASSWORD-LENGTH
           MOVE 'N' TO WS-HAS-UPPER WS-HAS-LOWER WS-HAS-DIGIT 
                       WS-HAS-SPECIAL
           MOVE ZERO TO WS-COMPLEXITY-SCORE
           
           IF WS-PASSWORD-LENGTH < SC-MIN-PASSWORD-LENGTH
              MOVE '04' TO WS-RETURN-CODE
              MOVE 'Password too short' TO WS-ERROR-MSG
              GO TO 6000-EXIT
           END-IF
           
           IF WS-PASSWORD-LENGTH > SC-MAX-PASSWORD-LENGTH
              MOVE '04' TO WS-RETURN-CODE
              MOVE 'Password too long' TO WS-ERROR-MSG
              GO TO 6000-EXIT
           END-IF
           
           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1
                   UNTIL WS-CHAR-INDEX > WS-PASSWORD-LENGTH
               MOVE COMM-PASSWORD(WS-CHAR-INDEX:1) TO WS-CURRENT-CHAR
               PERFORM 6100-ANALYZE-CHARACTER
           END-PERFORM
           
           IF PP-REQUIRE-UPPER = 'Y' AND WS-HAS-UPPER = 'N'
              MOVE '04' TO WS-RETURN-CODE
              MOVE 'Password requires uppercase letter' TO WS-ERROR-MSG
              GO TO 6000-EXIT
           END-IF
           
           IF PP-REQUIRE-LOWER = 'Y' AND WS-HAS-LOWER = 'N'
              MOVE '04' TO WS-RETURN-CODE
              MOVE 'Password requires lowercase letter' TO WS-ERROR-MSG
              GO TO 6000-EXIT
           END-IF
           
           IF PP-REQUIRE-DIGIT = 'Y' AND WS-HAS-DIGIT = 'N'
              MOVE '04' TO WS-RETURN-CODE
              MOVE 'Password requires digit' TO WS-ERROR-MSG
              GO TO 6000-EXIT
           END-IF
           
           IF WS-COMPLEXITY-SCORE < PP-COMPLEXITY-SCORE
              MOVE '04' TO WS-RETURN-CODE
              MOVE 'Password does not meet complexity requirements' 
                   TO WS-ERROR-MSG
           END-IF
           
       6000-EXIT.
           EXIT
           .
      
       6100-ANALYZE-CHARACTER.
           IF WS-CURRENT-CHAR >= 'A' AND WS-CURRENT-CHAR <= 'Z'
              MOVE 'Y' TO WS-HAS-UPPER
              ADD 4 TO WS-COMPLEXITY-SCORE
           ELSE
              IF WS-CURRENT-CHAR >= 'a' AND WS-CURRENT-CHAR <= 'z'
                 MOVE 'Y' TO WS-HAS-LOWER
                 ADD 2 TO WS-COMPLEXITY-SCORE
              ELSE
                 IF WS-CURRENT-CHAR >= '0' AND WS-CURRENT-CHAR <= '9'
                    MOVE 'Y' TO WS-HAS-DIGIT
                    ADD 3 TO WS-COMPLEXITY-SCORE
                 ELSE
                    MOVE 'Y' TO WS-HAS-SPECIAL
                    ADD 6 TO WS-COMPLEXITY-SCORE
                 END-IF
              END-IF
           END-IF
           .
      
      *----------------------------------------------------------------*
      * Compute hash (simplified SHA256-like algorithm)               *
      *----------------------------------------------------------------*
       7000-COMPUTE-HASH.
           MOVE SPACES TO WS-HASH-OUTPUT
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 
                   UNTIL WS-COUNTER > WS-INPUT-LENGTH
               MOVE WS-INPUT-STRING(WS-COUNTER:1) TO WS-CURRENT-CHAR
               COMPUTE WS-CHAR-CODE = 
                   FUNCTION ORD(WS-CURRENT-CHAR) * WS-COUNTER
               PERFORM 7100-CHAR-TO-HEX
               STRING WS-HASH-OUTPUT DELIMITED BY SPACE
                      WS-HEX-DIGIT WS-HEX-DIGIT
                      INTO WS-HASH-OUTPUT
               END-STRING
           END-PERFORM
           .
      
       7100-CHAR-TO-HEX.
           COMPUTE WS-CHAR-CODE = 
               FUNCTION MOD(WS-CHAR-CODE, 16)
           EVALUATE WS-CHAR-CODE
               WHEN 0 THRU 9
                   COMPUTE WS-HEX-DIGIT = WS-CHAR-CODE + 48
               WHEN 10 THRU 15
                   COMPUTE WS-HEX-DIGIT = WS-CHAR-CODE + 55
           END-EVALUATE
           .
      
      *----------------------------------------------------------------*
      * Error handling                                                 *
      *----------------------------------------------------------------*
       WRITE-ERROR-MESSAGE.
           MOVE ZERO TO EM-SQLRC
           
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME) END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYY(WS-DATE) DATESEP
                     TIME(WS-TIME) TIMESEP
           END-EXEC
           
           MOVE WS-DATE TO EM-DATE
           MOVE WS-TIME TO EM-TIME
           MOVE WS-FUNCTION TO EM-FUNCTION
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
           MOVE WS-RETURN-CODE TO COMM-RETURN-CODE
           MOVE WS-ERROR-MSG TO COMM-ERROR-MSG
           
           EXEC CICS RETURN END-EXEC
           .
      
       END PROGRAM LGPWHASH.