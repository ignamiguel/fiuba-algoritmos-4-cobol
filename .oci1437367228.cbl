       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *-----------------------

       FILE-CONTROL.
        SELECT ACT-TIMES     ASSIGN TO DISK
                             ORGANIZATION IS LINE SEQUENTIAL
                             FILE STATUS IS ACT-TIMES-ESTADO.
        SELECT EMPRESAS  ASSIGN TO DISK
                           ORGANIZATION IS LINE SEQUENTIAL.


        SELECT LISTADO-ESTAD ASSIGN TO PRINTER "LISTADOESTADISTICO.DAT".

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------

       FD LISTADO-ESTAD     LABEL RECORD OMITTED.
       01 LINEA-LISTADO PIC X(70).

       FD ACT-TIMES     LABEL RECORD IS STANDARD
                        VALUE OF FILE-ID IS "TIMES.DAT".
       01 REG-ACT-TIMES.
            03 ACT-TIMES-NUMERO    PIC X(5).
            03 ACT-TIMES-FECHA.
                05 ACT-TIMES-ANIO    PIC 9(4).
                05 ACT-TIMES-MES    PIC 9(2).
                05 ACT-TIMES-DIA    PIC 9(2).
            03 ACT-TIMES-EMPRESA    PIC 9(3).
            03 ACT-TIMES-TAREA        PIC X(4).
            03 ACT-TIMES-HORAS        PIC 9(2)V99.

       FD EMPRESAS     LABEL RECORD IS STANDARD
                       VALUE OF FILE-ID IS "EMPRESAS.DAT".
       01 REG-EMPRESAS.
          03 EMP-EMPRESA    PIC 9(3).
          03 EMP-RAZON        PIC X(25).
          03 EMP-DIRE        PIC X(20).
          03 EMP-TEL        PIC X(20).
          03 EMP-CUIT    PIC 9(11).


       WORKING-STORAGE SECTION.

       77 ACT-TIMES-ESTADO PIC XX.
       77 EMPRESAS-ESTADO PIC XX.

       77 EOF-TIM PIC XX VALUE "NO".
            88 EOF-TIMES VALUE "SI".
       77 EOF-EMP PIC XX VALUE "NO".
            88 EOF-EMPRESAS VALUE "SI".

       01 CLAVE-ACT-TIMES.
            03 CLAVE-ACT-TIMES-NUMERO     PIC 9(5).
            03 CLAVE-ACT-TIMES-FECHA    PIC 9(8).


       01 LINEA-A-ESCRIBIR PIC 9(2) VALUE 1.
       01 HORAS-MES PIC 9(2)V99.
       01 HORAS-ANIO PIC 9(2)V99.
       01 HORAS-POR-FECHA PIC 9(2)V99.

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


       01 SUBINDICE PIC 9(2) VALUE 1.

       01 WS-COUNTER PIC 99 VALUE 0.
       01 WS-AUX PIC 99 VALUE 0.
       01 WS-AUX-2 PIC 99 VALUE 0.       
       
       01 WS-REGISTER.
           05 WS-COMPANY OCCURS 10 TIMES.
               10 WS-COMPANY-CODE PIC 9(3).
               10 WS-COMPANY-NAME PIC A(20).
               10 WS-YEAR OCCURS 5 TIMES.
                   15 WS-YEAR-NAME PIC A(20).
                   15 WS-MONTHS OCCURS 12 TIMES.
                       20 WS-MONTH-NAME PIC A(15).
                       20 WS-MONTH-HOURS PIC S999.


       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       COMIENZO.

       PERFORM INICIO.
       PERFORM CARGAR-TABLAS.
       PERFORM CARGAR-DATOS.
       PERFORM PRINT-TABLE.
       PERFORM FIN.

       STOP RUN.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       INICIO.
           OPEN INPUT EMPRESAS. 
           OPEN INPUT ACT-TIMES.
      *  OPEN OUTPUT LISTADO-ESTAD.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*

       CARGAR-TABLAS.
           PERFORM LEER-EMPRESAS.
           MOVE 1 TO SUBINDICE.        
           PERFORM CARGAR-EMPRESAS UNTIL EOF-EMPRESAS.

       CARGAR-EMPRESAS.
           MOVE EMP-EMPRESA TO WS-COMPANY-CODE(SUBINDICE).
           MOVE EMP-RAZON TO WS-COMPANY-NAME(SUBINDICE).
           PERFORM LOAD-YEARS.
           PERFORM LOAD-YEAR-MONTH.
           ADD  1 TO SUBINDICE.
           PERFORM LEER-EMPRESAS.
        
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LEER-EMPRESAS.
           READ EMPRESAS AT END MOVE "SI" TO EOF-EMP.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       LEER-TIMES.
           READ ACT-TIMES
               AT END MOVE "SI" TO EOF-TIM.
           MOVE ACT-TIMES-FECHA TO CLAVE-ACT-TIMES-FECHA.
           MOVE ACT-TIMES-NUMERO TO CLAVE-ACT-TIMES-NUMERO.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*

       LOAD-YEARS.
           MOVE '2011' TO WS-YEAR-NAME(SUBINDICE,1).
           MOVE '2012' TO WS-YEAR-NAME(SUBINDICE,2).
           MOVE '2013' TO WS-YEAR-NAME(SUBINDICE,3).
           MOVE '2014' TO WS-YEAR-NAME(SUBINDICE,4).
           MOVE '2015' TO WS-YEAR-NAME(SUBINDICE,5).
          
       LOAD-YEAR-MONTH.
           MOVE 1 TO WS-AUX.
           PERFORM LOAD-MONTHS UNTIL WS-AUX > 5.

       LOAD-MONTHS.
           MOVE 'ENERO' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,1).
           MOVE 'FEBRERO' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,2).
           MOVE 'MARZO' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,3).
           MOVE 'ABRIL' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,4).
           MOVE 'MAYO' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,5).
           MOVE 'JUNIO' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,6).
           MOVE 'JULIO' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,7).
           MOVE 'AGOSTO' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,8).
           MOVE 'SEPTIEMBRE' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,9).
           MOVE 'OCTUBRE' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,10).
           MOVE 'NOVIEMBRE' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,11).
           MOVE 'DICIEMBRE' TO WS-MONTH-NAME(SUBINDICE,WS-AUX,12).

           ADD 1 TO WS-AUX.       
       
       CARGAR-DATOS.
           PERFORM LEER-TIMES.
           PERFORM CARGAR-DAT0-TABLA UNTIL EOF-TIMES.
             
       CARGAR-DAT0-TABLA.
          MOVE 1 TO SUBINDICE.
          PERFORM BC UNTIL ACT-TIMES-EMPRESA = WS-COMPANY-CODE(SUBINDICE).
          PERFORM LEER-TIMES.  
         
       BC.
           

      * PROCESAR-EMPRESAS
      *  SEARCH TAB-EMPRESAS
      *     AT END DISPLAY 'EMPRESA NO ENCONTRADA'
      *     WHEN TAB-EMP-EMPRESA(EMP-INDICE) EQUAL ACT-TIMES-EMPRESA
      *     PERFORM ACTUALIZAR-TABLA-HORAS.
      *     END-SEARCH
      *  PERFORM LEER-TIMES.

      * ACTUALIZAR-TABLA-HORAS.
      *  COMPUTE INDICE = ACT-TIMES-ANIO - 2010.
      *  TAB

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
       
       FIN.
           CLOSE EMPRESAS.
           CLOSE ACT-TIMES.
       
       END PROGRAM TP.
