      ******************************************************************
      * Author: ignamiguel
      * Date: 10 july 2016
      * Purpose: Before a file can be used in a COBOL program
      * its organization has to be defined, partly in
      * an ENVIRONMENT DIVISION, where the physical file name and
      * organization is stated, and partly in a DATA DIVISION, which
      * is used to describe the record layout.
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. INDEXED-FILE-CREATOR.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT MYFILE ASSIGN "INOUTFILE"
          ORGANIZATION IS INDEXED
          ACCESS IS RANDOM
          RECORD KEY IS ACCTNO
          STATUS IS FILE-ERROR.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
        FD  MYFILE.
        01  IN-RECORD.
           03  ACCTNO      PIC 99.
           03  AMOUNT      PIC 99999V99.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
        01  FILE-ERROR         PIC XX.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM INDEXED-FILE-CREATOR.
