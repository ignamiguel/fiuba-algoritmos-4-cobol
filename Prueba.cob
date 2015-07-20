      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PRUEBA.
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

       01 WS-COUNTER PIC 99 VALUE 0.
       01 WS-AUX PIC 99 VALUE 0.
       01 WS-AUX-2 PIC 99 VALUE 0.

       01 WS-REGISTER.
           05 WS-COMPANY OCCURS 10 TIMES.
               10 WS-COMPANY-NAME PIC A(20) VALUE 'COMPANY X'.
               10 WS-YEAR OCCURS 5 TIMES.
                   15 WS-YEAR-NAME PIC A(20) VALUE 'YEAR Y'.
                   15 WS-MONTHS OCCURS 12 TIMES.
                       20 WS-MONTH-NAME PIC A(20) VALUE 'MONTH Z'.
                       20 WS-MONTH-HOURS PIC S999.

      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * Cargo las empresas , los anios y los meses
      **
      *    Cargo las empresas
           MOVE 1 TO WS-COUNTER.
           PERFORM LOAD-COMAPNY WITH TEST AFTER UNTIL WS-COUNTER > 10.

      *    Cargo los anios
           MOVE 1 TO WS-COUNTER.
           PERFORM LOAD-YEARS WITH TEST AFTER UNTIL WS-COUNTER > 10.

      *    Cargo los meses
           MOVE 1 TO WS-COUNTER.
           PERFORM LOAD-YEAR-MONTH UNTIL WS-COUNTER > 10.

      *    Muestro el contenido del registro
           PERFORM PRINT-TABLE.

      *    DISPLAY "ONE-D TABLE : "WS-REGISTER.

           STOP RUN.

      * -- Sub modulos
       LOAD-COMAPNY.
      *    Se debe cargar la tabla empresas
      *    con el contenido del archivo 'empresas'
      *    Simulo la carga con nombres inventados.

           MOVE 'ARCOR' TO WS-COMPANY-NAME(1).
           MOVE 'BAUFEST' TO WS-COMPANY-NAME(2).
           MOVE 'COMODIN' TO WS-COMPANY-NAME(3).
           MOVE 'TENARIS' TO WS-COMPANY-NAME(4).
           MOVE 'VISA' TO WS-COMPANY-NAME(5).
           MOVE 'HP' TO WS-COMPANY-NAME(6).
           MOVE 'TATA' TO WS-COMPANY-NAME(7).
           MOVE 'SOFTTEK' TO WS-COMPANY-NAME(8).
           MOVE 'GOOGLE' TO WS-COMPANY-NAME(9).
           MOVE 'VOLSKWAGEN' TO WS-COMPANY-NAME(10).

           ADD 1 TO WS-COUNTER.

       LOAD-YEARS.
           MOVE '2011' TO WS-YEAR-NAME(WS-COUNTER,1).
           MOVE '2012' TO WS-YEAR-NAME(WS-COUNTER,2).
           MOVE '2013' TO WS-YEAR-NAME(WS-COUNTER,3).
           MOVE '2014' TO WS-YEAR-NAME(WS-COUNTER,4).
           MOVE '2015' TO WS-YEAR-NAME(WS-COUNTER,5).
           ADD 1 TO WS-COUNTER.

       LOAD-YEAR-MONTH.
           MOVE 1 TO WS-AUX.
           PERFORM LOAD-MONTHS UNTIL WS-AUX > 5.
           ADD 1 TO WS-COUNTER.

       LOAD-MONTHS.
           MOVE 'ENERO' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,1).
           MOVE 'FEBRERO' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,2).
           MOVE 'MARZO' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,3).
           MOVE 'ABRIL' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,4).
           MOVE 'MAYO' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,5).
           MOVE 'JUNIO' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,6).
           MOVE 'JULIO' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,7).
           MOVE 'AGOSTO' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,8).
           MOVE 'SEPTIEMBRE' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,9).
           MOVE 'OCTUBRE' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,10).
           MOVE 'NOVIEMBRE' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,11).
           MOVE 'DICIEMBRE' TO WS-MONTH-NAME(WS-COUNTER,WS-AUX,12).

           ADD 1 TO WS-AUX.

       PRINT-TABLE.
           MOVE 1 TO WS-COUNTER.
           PERFORM PRINT-COMAPNY WITH TEST AFTER UNTIL WS-COUNTER > 10.

       PRINT-COMAPNY.
           DISPLAY WS-COMPANY-NAME(WS-COUNTER).
           MOVE 1 TO WS-AUX.
           PERFORM PRINT-YEAR WITH TEST AFTER UNTIL WS-AUX > 5.
           ADD 1 TO WS-COUNTER.

       PRINT-YEAR.
           DISPLAY '   'WS-YEAR-NAME(WS-COUNTER,WS-AUX).
           PERFORM PRINT-MONTHS.
           ADD 1 TO WS-AUX.

       PRINT-MONTHS.
           MOVE 1 TO WS-AUX-2.
           PERFORM PRINT-SINGLE-MONTH UNTIL WS-AUX-2 > 12.


       PRINT-SINGLE-MONTH.
           DISPLAY '       'WS-MONTHS(WS-COUNTER,WS-AUX,WS-AUX-2).
           ADD 1 TO WS-AUX-2.


       END PROGRAM PRUEBA.
