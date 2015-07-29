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
       01 TABLA-EMPRESAS.
           02 TAB-EMPRESAS OCCURS 100 TIMES INDEXED BY EMP-INDICE.
               03 TAB-EMP-EMPRESA PIC 9(3).
               03 TAB-EMP-RAZON PIC X(25).
               03 TAB-EMP-DIRE PIC X(20).
               03 TAB-EMP-TEL PIC X(20).
               03 TAB-EMP-CUIT PIC 9(11).
       01 TABLA-HORAS.
           02 TAB-HORAS OCCURS 5 TIMES INDEXED BY ANIO-INDICE.
               03 TAB-HORAS-MESES OCCURS 12 TIMES INDEXED BY MES-INDICE.
                   05 TAB-HORAS-CANTIDAD PIC 9(2)V99.





       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       COMIENZO.

       PERFORM INICIO.
       PERFORM CARGAR-TABLAS.
       PERFORM PROCESAR-ESTADISTICAS.


       STOP RUN.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       INICIO.
        OPEN INPUT ACT-TIMES
        OPEN OUTPUT LISTADO-ESTAD.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*

       CARGAR-TABLAS.
        MOVE 1 TO SUBINDICE.
        PERFORM CARGAR-EMPRESAS UNTIL EOF-EMPRESAS.

       CARGAR-EMPRESAS.
        MOVE EMP-EMPRESA TO TAB-EMP-EMPRESA(SUBINDICE).
        MOVE EMP-RAZON TO TAB-EMP-RAZON(SUBINDICE).
        MOVE EMP-DIRE TO TAB-EMP-DIRE(SUBINDICE).
        MOVE EMP-TEL TO TAB-EMP-TEL(SUBINDICE).
        MOVE EMP-CUIT TO TAB-EMP-CUIT(SUBINDICE).
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

       PROCESAR-ESTADISTICAS.
        SET EMP-INDICE TO 1.
        PERFORM PROCESAR-EMPRESAS UNTIL EOF-TIMES
            OR (ACT-TIMES-EMPRESA =



       PROCESAR-EMPRESAS
        SEARCH TAB-EMPRESAS
           AT END DISPLAY 'EMPRESA NO ENCONTRADA'
           WHEN TAB-EMP-EMPRESA(EMP-INDICE) EQUAL ACT-TIMES-EMPRESA
           PERFORM ACTUALIZAR-TABLA-HORAS.
           END-SEARCH
         PERFORM LEER-TIMES.

       ACTUALIZAR-TABLA-HORAS.
        COMPUTE INDICE = ACT-TIMES-ANIO - 2010.
        TAB


       END PROGRAM TP.
