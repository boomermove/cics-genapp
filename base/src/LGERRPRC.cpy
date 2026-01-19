      *================================================================*
      * Licensed Materials - Property of IBM                          *
      * GENAPP - CICS GenApp Common Error Handling Procedures         *
      * (C) Copyright IBM Corp. 2023. All Rights Reserved             *
      *================================================================*
      *                                                                *
      * Copybook: LGERRPRC - Common Error Handling Procedures         *
      * Purpose:  Provides shared error logging routines              *
      *                                                                *
      * Requires: COPY LGERR in WORKING-STORAGE SECTION               *
      *           ERROR-MSG with EM-DATE and EM-TIME fields           *
      *                                                                *
      * Provides:                                                      *
      *   LGERR-FORMAT-TIME   - Get and format current time/date      *
      *   LGERR-WRITE-MSG     - Write ERROR-MSG to TSQ                *
      *   LGERR-LOG-COMMAREA  - Log commarea contents to TSQ          *
      *   LGERR-WRITE-FULL    - Format time + write error + log CA    *
      *                                                                *
      *================================================================*

      *----------------------------------------------------------------*
      * Format current time and date into WS-ERR fields               *
      * Caller must move to EM-DATE and EM-TIME as needed             *
      *----------------------------------------------------------------*
       LGERR-FORMAT-TIME.
           EXEC CICS ASKTIME
               ABSTIME(WS-ERR-ABSTIME)
           END-EXEC

           EXEC CICS FORMATTIME
               ABSTIME(WS-ERR-ABSTIME)
               MMDDYYYY(WS-ERR-DATE)
               TIME(WS-ERR-TIME)
           END-EXEC
           .

      *----------------------------------------------------------------*
      * Write error message to temporary storage queue                *
      * Assumes ERROR-MSG is populated by caller                      *
      *----------------------------------------------------------------*
       LGERR-WRITE-MSG.
           EXEC CICS LINK
               PROGRAM(WS-ERR-LOG-PGM)
               COMMAREA(ERROR-MSG)
               LENGTH(LENGTH OF ERROR-MSG)
               RESP(WS-ERR-RESP)
           END-EXEC
           .

      *----------------------------------------------------------------*
      * Log commarea contents (first 90 bytes) to TSQ                 *
      *----------------------------------------------------------------*
       LGERR-LOG-COMMAREA.
           IF EIBCALEN > 0
               IF EIBCALEN < 91
                   MOVE DFHCOMMAREA(1:EIBCALEN) TO CA-DATA
               ELSE
                   MOVE DFHCOMMAREA(1:90) TO CA-DATA
               END-IF

               EXEC CICS LINK
                   PROGRAM(WS-ERR-LOG-PGM)
                   COMMAREA(CA-ERROR-MSG)
                   LENGTH(LENGTH OF CA-ERROR-MSG)
                   RESP(WS-ERR-RESP)
               END-EXEC
           END-IF
           .
