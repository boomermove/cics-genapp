      *================================================================*
      * Licensed Materials - Property of IBM                          *
      * GENAPP - CICS GenApp Common Error Handling Copybook           *
      * (C) Copyright IBM Corp. 2023. All Rights Reserved             *
      *================================================================*
      *                                                                *
      * Copybook: LGERR - Common Error Handling Structures            *
      * Purpose:  Provides shared error logging structures            *
      *                                                                *
      * Usage:    COPY LGERR.                                         *
      *           Then COPY LGERRPRC in PROCEDURE DIVISION.           *
      *                                                                *
      * Note: Each program defines its own ERROR-MSG structure        *
      *       but must include EM-DATE PIC X(8) and EM-TIME PIC X(6)  *
      *       at the standard positions for LGERRPRC to work.         *
      *                                                                *
      *================================================================*

      *----------------------------------------------------------------*
      * Time and date working storage for error logging               *
      *----------------------------------------------------------------*
       01  WS-ERR-WORK-FIELDS.
           05  WS-ERR-ABSTIME          PIC S9(8) COMP VALUE +0.
           05  WS-ERR-DATE             PIC X(10) VALUE SPACES.
           05  WS-ERR-TIME             PIC X(8) VALUE SPACES.
           05  WS-ERR-RESP             PIC S9(8) COMP VALUE 0.

      *----------------------------------------------------------------*
      * Commarea error logging structure                              *
      * Used to log first 90 bytes of commarea for diagnostics        *
      *----------------------------------------------------------------*
       01  CA-ERROR-MSG.
           05  CA-EM-PREFIX            PIC X(9)  VALUE 'COMMAREA='.
           05  CA-DATA                 PIC X(90) VALUE SPACES.

      *----------------------------------------------------------------*
      * Error log queue name constant                                 *
      *----------------------------------------------------------------*
       01  WS-ERR-LOG-PGM              PIC X(8)  VALUE 'LGSTSQ'.
