      ******************************************************************
      * Author: ignamiguel
      * Date: 8th july 2016
      * Purpose: cobol basics
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. COBOL_TUTORIALS.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC A(30).
       01 WS-ID PIC 9(5) VALUE 12345.

       01 S9-3-V9-2 PIC S9(3)V9(2).
       01 PPP999 PIC PPP999.
       01 S9-3-V9-2-NEW PIC S9(3)V9(2) VALUE -123.45.
       01 WS-NEW-NAME PIC A(6) VALUE 'ABCDEF'.
       01 WS-NEW-ID PIC X(5) VALUE 'A121$'.


       01 WS-STUDENT-NAME PIC X(25).
       01 WS-DATE PIC X(10).

       01 WS-OTHER-NAME PIC A(30) VALUE 'ABCDEF'.
       01 WS-OTHER-ID PIC 9(5).
       01 WS-ADDRESS.
        05 WS-HOUSE-NUMBER PIC 9(3).
        05 WS-COUNTRY PIC X(15).
        05 WS-PINCODE PIC 9(6) VALUE 123456.

       01 WS-OLD-X PIC X(10) VALUE 'PEPE 1234'.
       01 WS-NEW-9 REDEFINES WS-OLD-X PIC 9(8).
       01 WS-NEW-A REDEFINES WS-OLD-X PIC A(10).

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR     PIC X(04).
               10 WS-CURRENT-MONTH    PIC X(02).
               10 WS-CURRENT-DAY     PIC X(02).
           05  WS-CURRENT-TIME.
               10 WS-CURRENT-HOUR     PIC  9(2).
               10  WS-CURRENT-MINUTE  PIC  9(2).
               10  WS-CURRENT-SECOND  PIC  9(2).
               10  WS-CURRENT-MS      PIC  9(2).
               10  WS-GMT-SIGN        PIC X(01).
               10  WS-GMT-TIME        PIC X(04).
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            *>PERFORM BASIC-PROCEDURE.
      *      PERFORM USER-INPUT-PROCEDURE.
      *      PERFORM REPLACING-PROCEDURE.
      *      PERFORM REDEFINES-PROCEDURE.
      *      PERFORM IF-EXAMPLE.
             PERFORM Print_Date.

            STOP RUN.
      ** add other procedures here
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       BASIC-PROCEDURE.
            DISPLAY '*---------------------------*'.
            DISPLAY '-- START BASIC-PROCEDURE --'.
            MOVE 'TutorialsPoint' TO WS-NAME.
            DISPLAY "My name is : "WS-NAME.
            DISPLAY "My ID is : "WS-ID.
            DISPLAY "S9-3-V9-2 : "S9-3-V9-2.
            DISPLAY "PPP999 : "PPP999.
            DISPLAY "S9-3-V9-2-NEW : "S9-3-V9-2-NEW.
            DISPLAY "WS-NEW-NAME : "WS-NEW-NAME.
            DISPLAY "WS-NEW-ID : "WS-NEW-ID.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       USER-INPUT-PROCEDURE.
       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
            DISPLAY '*---------------------------*'.
            DISPLAY '-- START USER-INPUT-PROCEDURE --'.
            DISPLAY "Enter student name and hit enter: "
      *     ACCEPT WS-STUDENT-NAME.
            ACCEPT WS-DATE FROM DATE.
            DISPLAY "Name :  " WS-STUDENT-NAME.
            DISPLAY "System Date : " WS-DATE.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       REPLACING-PROCEDURE.
            DISPLAY '*---------------------------*'.
            DISPLAY '-- START REPLACING-PROCEDURE --'.
            INITIALIZE WS-OTHER-NAME, WS-ADDRESS.
            INITIALIZE WS-OTHER-ID REPLACING NUMERIC DATA BY 12345.
            DISPLAY "My name is   : "WS-OTHER-NAME.
            DISPLAY "My ID is     : "WS-OTHER-ID.
            DISPLAY "Address      : "WS-ADDRESS.
            DISPLAY "House Number : "WS-HOUSE-NUMBER.
            DISPLAY "Country      : "WS-COUNTRY.
            DISPLAY "Pincode      : "WS-PINCODE.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       REDEFINES-PROCEDURE.
            DISPLAY '*---------------------------*'.
            DISPLAY '-- START REDEFINES-PROCEDURE --'.
            DISPLAY "WS-OLD-X: " WS-OLD-X.

            MOVE 1234 TO WS-NEW-9.
            DISPLAY "WS-NEW-9: " WS-NEW-9.

            MOVE 'ABCD' TO WS-NEW-A.
            DISPLAY "WS-NEW-A: " WS-NEW-A.

            DISPLAY "WS-OLD-X: " WS-OLD-X.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       IF-EXAMPLE.
            IF 1 = 1 THEN
               DISPLAY "1 = 1 OK"
            ELSE
               DISPLAY "1 NOT = 1"
            END-IF.
       Print_Date.
        DISPLAY FUNCTION CURRENT-DATE.

        PERFORM Print_friendly_date.

       Print_friendly_date.
          MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
          DISPLAY "Today -> "
          WS-CURRENT-DAY "/" WS-CURRENT-MONTH "/"
         WS-CURRENT-YEAR.

         DISPLAY "Now -> " WS-CURRENT-HOUR ":" WS-CURRENT-MINUTE ":"
         WS-CURRENT-SECOND.


       END PROGRAM COBOL_TUTORIALS.
