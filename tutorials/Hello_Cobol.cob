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

       01 WS-NUM1 PIC S9(3)V9(2).
       01 WS-NUM2 PIC PPP999.
       01 WS-NUM3 PIC S9(3)V9(2) VALUE -123.45.
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
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
      *      PERFORM BASIC-PROCEDURE.
      *      PERFORM USER-INPUT-PROCEDURE.
      *      PERFORM REPLACING-PROCEDURE.
      *      PERFORM REDEFINES-PROCEDURE.
            PERFORM IF-EXAMPLE.

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
            DISPLAY "WS-NUM1 : "WS-NUM1.
            DISPLAY "WS-NUM2 : "WS-NUM2.
            DISPLAY "WS-NUM3 : "WS-NUM3.
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
       END PROGRAM COBOL_TUTORIALS.
