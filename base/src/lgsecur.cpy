      *================================================================*
      * Licensed Materials - Property of IBM                          *
      * GENAPP - CICS GenApp Security Configuration                    *
      * (C) Copyright IBM Corp. 2023. All Rights Reserved             *
      * US Government Users Restricted Rights - Use, duplication or    *
      * disclosure restricted by GSA ADP Schedule Contract with IBM    *
      * Corp                                                           *
      *================================================================*
      
      *----------------------------------------------------------------*
      * Security Configuration Copybook                               *
      *----------------------------------------------------------------*
       
      *----------------------------------------------------------------*
      * Security Settings                                              *
      *----------------------------------------------------------------*
       01  SECURITY-CONFIG.
           05  SC-HASH-ALGORITHM        PIC X(8) VALUE 'SHA256  '.
           05  SC-SALT-LENGTH          PIC 9(2) VALUE 16.
           05  SC-MIN-PASSWORD-LENGTH  PIC 9(2) VALUE 08.
           05  SC-MAX-PASSWORD-LENGTH  PIC 9(2) VALUE 32.
           05  SC-PASSWORD-EXPIRY-DAYS PIC 9(3) VALUE 090.
           05  SC-MAX-LOGIN-ATTEMPTS   PIC 9(2) VALUE 03.
           05  SC-LOCKOUT-DURATION     PIC 9(4) VALUE 1800.
           05  SC-SESSION-TIMEOUT      PIC 9(4) VALUE 3600.
           05  SC-AUDIT-LEVEL          PIC X(1) VALUE 'A'.
               88  SC-AUDIT-ALL        VALUE 'A'.
               88  SC-AUDIT-FAIL       VALUE 'F'.
               88  SC-AUDIT-NONE       VALUE 'N'.
       
      *----------------------------------------------------------------*
      * Password Policy Structure                                      *
      *----------------------------------------------------------------*
       01  PASSWORD-POLICY.
           05  PP-REQUIRE-UPPER        PIC X(1) VALUE 'Y'.
           05  PP-REQUIRE-LOWER        PIC X(1) VALUE 'Y'.
           05  PP-REQUIRE-DIGIT        PIC X(1) VALUE 'Y'.
           05  PP-REQUIRE-SPECIAL      PIC X(1) VALUE 'N'.
           05  PP-HISTORY-COUNT        PIC 9(2) VALUE 05.
           05  PP-COMPLEXITY-SCORE     PIC 9(2) VALUE 60.
       
      *----------------------------------------------------------------*
      * Authentication Request Structure                               *
      *----------------------------------------------------------------*
       01  AUTH-REQUEST.
           05  AR-FUNCTION             PIC X(8).
               88  AR-AUTHENTICATE     VALUE 'AUTH    '.
               88  AR-CHANGE-PASSWORD  VALUE 'CHGPASS '.
               88  AR-CREATE-USER      VALUE 'CREATEU '.
               88  AR-LOCK-USER        VALUE 'LOCKUSER'.
               88  AR-UNLOCK-USER      VALUE 'UNLKUSER'.
           05  AR-CUSTOMER-NUM         PIC 9(10).
           05  AR-USERNAME             PIC X(32).
           05  AR-PASSWORD             PIC X(64).
           05  AR-NEW-PASSWORD         PIC X(64).
           05  AR-CLIENT-IP            PIC X(15).
           05  AR-USER-AGENT           PIC X(100).
           05  AR-SESSION-ID           PIC X(32).
       
      *----------------------------------------------------------------*
      * Authentication Response Structure                              *
      *----------------------------------------------------------------*
       01  AUTH-RESPONSE.
           05  AS-RETURN-CODE          PIC X(2).
               88  AS-SUCCESS          VALUE '00'.
               88  AS-INVALID-CREDS    VALUE '01'.
               88  AS-ACCOUNT-LOCKED   VALUE '02'.
               88  AS-PASSWORD-EXPIRED VALUE '03'.
               88  AS-POLICY-VIOLATION VALUE '04'.
               88  AS-SYSTEM-ERROR     VALUE '90'.
               88  AS-NOT-AUTHORIZED   VALUE '99'.
           05  AS-SESSION-TOKEN        PIC X(64).
           05  AS-EXPIRY-TIME          PIC X(26).
           05  AS-LAST-LOGIN           PIC X(26).
           05  AS-ATTEMPTS-REMAINING   PIC 9(2).
           05  AS-DAYS-TO-EXPIRY       PIC 9(3).
           05  AS-ERROR-MESSAGE        PIC X(100).
       
      *----------------------------------------------------------------*
      * User Security Record                                          *
      *----------------------------------------------------------------*
       01  USER-SECURITY-REC.
           05  USR-CUSTOMER-NUM        PIC 9(10).
           05  USR-USERNAME            PIC X(32).
           05  USR-PASSWORD-HASH       PIC X(128).
           05  USR-SALT                PIC X(32).
           05  USR-HASH-ALGORITHM      PIC X(8).
           05  USR-PASSWORD-DATE       PIC X(10).
           05  USR-LAST-LOGIN          PIC X(26).
           05  USR-LOGIN-ATTEMPTS      PIC 9(2).
           05  USR-ACCOUNT-STATUS      PIC X(1).
               88  USR-ACTIVE          VALUE 'A'.
               88  USR-LOCKED          VALUE 'L'.
               88  USR-SUSPENDED       VALUE 'S'.
               88  USR-EXPIRED         VALUE 'E'.
           05  USR-LOCKOUT-TIME        PIC X(26).
           05  USR-CREATED-DATE        PIC X(10).
           05  USR-MODIFIED-DATE       PIC X(10).
       
      *----------------------------------------------------------------*
      * Password History Structure                                     *
      *----------------------------------------------------------------*
       01  PASSWORD-HISTORY.
           05  PH-CUSTOMER-NUM         PIC 9(10).
           05  PH-SEQUENCE             PIC 9(2).
           05  PH-PASSWORD-HASH        PIC X(128).
           05  PH-CREATED-DATE         PIC X(10).
       
      *----------------------------------------------------------------*
      * Audit Log Structure                                           *
      *----------------------------------------------------------------*
       01  AUDIT-LOG-REC.
           05  AL-TIMESTAMP            PIC X(26).
           05  AL-CUSTOMER-NUM         PIC 9(10).
           05  AL-USERNAME             PIC X(32).
           05  AL-ACTION               PIC X(20).
           05  AL-RESULT               PIC X(2).
           05  AL-CLIENT-IP            PIC X(15).
           05  AL-USER-AGENT           PIC X(100).
           05  AL-ERROR-CODE           PIC X(10).
           05  AL-DETAILS              PIC X(200).