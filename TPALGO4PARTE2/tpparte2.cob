       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP.
     
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT TIM    ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS SEQUENTIAL
                      RECORD KEY TIM-NFC
                      FILE STATUS IS FS-TIMES.
        SELECT CONSULTORES    ASSIGN TO DISK
                              ORGANIZATION IS INDEXED
                              ACCESS MODE IS RANDOM
                              RECORD KEY CONS-NUMERO
                              FILE STATUS IS FS-CONS.
        SELECT EMPRESAS       ASSIGN TO DISK
                              ORGANIZATION IS INDEXED
                              ACCESS MODE IS RANDOM
                              RECORD KEY EMP-EMPRESA
                              ALTERNATE RECORD KEY IS EMP-CUIT
                              FILE STATUS IS FS-EMP.
        SELECT TARIFAS        ASSIGN TO DISK
                              ORGANIZATION IS INDEXED
                              ACCESS MODE IS SEQUENTIAL
                              RECORD KEY TAR-SVD
                              FILE STATUS IS FS-TAR.
        SELECT PARAMETROS    ASSIGN TO DISK
                            ORGANIZATION IS LINE SEQUENTIAL
                            FILE STATUS IS PARAM-ESTADO.
        SELECT ARCH-ORDENAR    ASSIGN TO DISK
                               FILE STATUS IS FS-ARCH-ORDENAR.


        SELECT LISTADO ASSIGN TO PRINTER "LISTADOdelSORT.DAT".
       DATA DIVISION.
       FILE SECTION.

       FD TIM     LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS "TIMESind2.DAT".
       01 REG-TIMES.
           02 TIM-NFC.
               03 TIM-NUMERO    PIC X(5).
               03 TIM-FECHA.
                   05 TIM-ANIO    PIC 9(4).
                   05 TIM-MES    PIC 9(2).
                   05 TIM-DIA    PIC 9(2).
               03 TIM-CUIT    PIC 9(11).
           02 TIM-TAREA        PIC X(4).
           02 TIM-HORAS        PIC 9(2)V99.
           
       FD CONSULTORES     LABEL RECORD IS STANDARD
                          VALUE OF FILE-ID IS "CONSind2.DAT".
       01 REG-CONSULTORES.
          03 CONS-NUMERO    PIC X(5).
          03 CONS-DNI    PIC 9(8).
          03 CONS-SRT       PIC X(2).
          03 CONS-NOMBRE        PIC X(25).
          03 CONS-DIRE        PIC X(20).
          03 CONS-TEL        PIC X(20).

       FD EMPRESAS     LABEL RECORD IS STANDARD
                      VALUE OF FILE-ID IS "EMPind2.DAT".
       01 REG-EMPRESAS.
          03 EMP-EMPRESA    PIC 9(3).
          03 EMP-RAZON        PIC X(25).
          03 EMP-DIRE        PIC X(20).
          03 EMP-TEL        PIC X(20).
          03 EMP-CUIT    PIC 9(11).

       FD TARIFAS     LABEL RECORD IS STANDARD
                      VALUE OF FILE-ID IS "TARind2.DAT".
       01 REG-TARIFAS.
          03 TAR-SVD.
              05 TAR-SRT PIC X(2).
              05 TAR-VIG-DES PIC 9(8).
          03 TAR-TARIFA        PIC 9(5)V99.

       FD PARAMETROS     LABEL RECORD IS STANDARD
                      VALUE OF FILE-ID IS "PARAMETROS.DAT".
       01 REG-PARAM.
           03 PAR-CUIT-DESDE PIC 9(11).
           03 PAR-CUIT-HASTA PIC 9(11).

       SD ARCH-ORDENAR DATA RECORD IS REG-ORDENAR.
       01 REG-ORDENAR.
           03 ORD-EMP-RAZON    PIC X(25).
           03 ORD-EMP-CUIT     PIC 9(11).
           03 ORD-CONS-NUMERO    PIC X(5).
           03 ORD-CONS-NOMBRE        PIC X(25).
           03 ORD-TIM-FECHA.
                   05 ORD-TIM-ANIO    PIC 9(4).
                   05 ORD-TIM-MES    PIC 9(2).
                   05 ORD-TIM-DIA    PIC 9(2).
           03 ORD-HORAS PIC 9(2)V99.
           03 ORD-IMPORTE PIC 9(7)V99.

       FD LISTADO     LABEL RECORD OMITTED.
       01 LINEA-LISTADO PIC X(70).

       WORKING-STORAGE SECTION.
       77 PARAM-ESTADO PIC XX.
       01 FS-TIMES PIC XX.
           88 OK-TIM VALUE '00'.
           88 NO-TIM VALUE '23'.
           88 EOF-TIM VALUE '10'.
       01 FS-CONS PIC XX.
           88 OK-CONS VALUE '00'.
           88 NO-CONS VALUE '23'.
           88 EOF-CONS VALUE '10'.
       01 FS-EMP PIC XX.
           88 OK-EMP VALUE '00'.
           88 NO-EMP VALUE '23'.
           88 EOF-EMP VALUE '10'.
       01 FS-TAR PIC XX.
           88 OK-TAR VALUE '00'.
           88 NO-TAR VALUE '23'.
           88 EOF-TAR VALUE '10'.
       01 FS-ARCH-ORDENAR PIC XX.
           88 OK-ORD VALUE '00'.
           88 NO-ORD VALUE '23'.
           88 EOF-ORD VALUE '10'.
       01 EOF-ARCH-ORD PIC XX.
           88 EOF-ARCH-ORDENAR VALUE 'SI'.

       01 REG-RELEASE.
           03 REG-RELEASE-EMP-RAZON PIC X(25).
           03 REG-RELEASE-EMP-CUIT PIC 9(11).
           03 REG-RELEASE-CONS-NRO PIC X(5).
           03 REG-RELEASE-CONS-NOMBRE PIC X(25).
           03 REG-RELEASE-TIM-FECHA PIC 9(8).
           03 REG-RELEASE-TIM-HORAS PIC 9(2)V99.
           03 REG-RELEASE-IMPORTE PIC 9(7)V99.

       01 CONS-ANT PIC X(5).
       01 CATEGORIA-SRT PIC  X(2).
       01 EMP-ANT PIC X(25).
       01 FECHA-ANT PIC 9(8).
       01 TARIFA-ANT.
           03 ANT-SRT PIC X(2).
           03 ANT-VIG-DES PIC 9(8).
           03 ANT-TARIFA   PIC 9(5)V99.

       01 ENCABEZADO.
           03 FILLER PIC X(1) VALUE SPACES.
           03 DIA PIC X(2).
           03 FILLER PIC X VALUE '/'.
           03 MES PIC X(2).
           03 FILLER PIC X VALUE '/'.
           03 ANIO PIC X(4).
           03 FILLER PIC X(51) VALUE SPACES.
           03 FILLER PIC X(5) VALUE 'HOJA '.
           03 HOJA PIC 9(3) VALUE 1.
       01 LINEA-VACIA.
           03 FILLER PIC X(70) VALUE SPACES.
       01 DATOS-EMPRESA.
           02 PRIMER-DATO-EMPRESA.
               03 FILLER PIC X(1) VALUE SPACES.
               03 FILLER PIC X(9) VALUE "EMPRESA: ".
               03 DATO-EMPRESA-NOMBRE PIC X(25).
               03 FILLER PIC X(35) VALUE SPACES.
           02 SEGUNDO-DATO-EMPRESA.
               03 FILLER PIC X(1) VALUE SPACES.
               03 FILLER PIC X(6) VALUE "CUIT: ".
               03 DATO-EMPRESA-CUIT PIC 9(11).
               03 FILLER PIC X(52) VALUE SPACES.
       01 ENCABEZADO-TABLA.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(26) VALUE '  FECHA       CONS        '.
           03 FILLER PIC X(32) VALUE '  NOMBRE                  HS    '.
           03 FILLER PIC X(11) VALUE '   IMPORTE '.
       01 BARRA.
           03 FILLER PIC X(70) VALUE ALL "-".
       01 BARRA-TOTAL.
           03 FILLER PIC X(50) VALUE SPACES.
           03 FILLER PIC X(6) VALUE ALL "-".
           03 FILLER PIC X(3) VALUE ALL SPACES.
           03 FILLER PIC X(10) VALUE ALL "-".
       01 LINEA-ORD.
           03 FILLER PIC X(1) VALUE SPACES.
           03 ORD-FECHA.
               05 ORD-DIA PIC 9(2).
               05 FILLER PIC X(1) VALUE '/'.
               05 ORD-MES PIC 9(2).
               05 FILLER PIC X(1) VALUE '/'.
               05 ORD-ANIO PIC 9(4).
               05 FILLER PIC X(3) VALUE SPACES.
           03 ORD-NRO PIC 9(5).
           03 FILLER PIC X(2) VALUE SPACES.
           03 ORD-NOMBRE PIC X(25).
           03 FILLER PIC X(5) VALUE SPACES.
           03 ORD-HS PIC ZZ.99.
           03 FILLER PIC X(3) VALUE SPACES.
           03 ORD-IMP PIC ZZZZZZ9.99.
       01 LINEA-TOTAL-POR-FECHA.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(18) VALUE 'TOTALES POR FECHA:'.
           03 FILLER PIC X(31) VALUE SPACES.
           03 DATO-TOTAL-POR-FECHA-HORAS PIC ZZZ.99.
           03 FILLER PIC X(3) VALUE SPACES.
           03 DATO-TOTAL-POR-FECHA-IMPORTE PIC ZZZZZZZ.99.
           03 FILLER PIC X(3) VALUE SPACES.
           03 AUX-TOTAL-POR-FECHA-HORAS PIC 9(3)V99.
           03 AUX-TOTAL-POR-FECHA-IMPORTE PIC 9(7)V99.
       01 LINEA-TOTAL-POR-EMPRESA.
           03 FILLER PIC X(1) VALUE SPACES.
           03 FILLER PIC X(20) VALUE 'TOTALES POR EMPRESA:'.
           03 FILLER PIC X(28) VALUE SPACES.
           03 DATO-TOTAL-POR-EMP-HORAS PIC ZZZZ.99.
           03 FILLER PIC X(2) VALUE SPACES.
           03 DATO-TOTAL-POR-EMP-IMPORTE PIC ZZZZZZZZ.99.
           03 FILLER PIC X(3) VALUE SPACES.
           03 AUX-TOTAL-POR-EMP-HORAS PIC 9(4)V99.
           03 AUX-TOTAL-POR-EMP-IMPORTE PIC 9(8)V99.
       01 LINEA-TOTAL-GRAL.
           03 FILLER PIC X(14) VALUE 'TOTAL GENERAL:'.
           03 FILLER PIC X(44) VALUE SPACES.
           03 DATO-TOTAL-GRAL-IMPORTE PIC ZZZZZZZZ.99.
           03 FILLER PIC X(3) VALUE SPACES.
           03 AUX-TOTAL-GRAL-IMPORTE PIC 9(8)V99.

       01 LINEA-A-ESCRIBIR PIC 9(2) VALUE 1.
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

       PROCEDURE DIVISION.
       COMIENZO.
            DISPLAY 'Hola mundo'.
            SORT ARCH-ORDENAR ON ASCENDING KEY ORD-EMP-RAZON
                                 ASCENDING KEY ORD-TIM-FECHA
                                 ASCENDING KEY ORD-CONS-NOMBRE
                              INPUT PROCEDURE IS ENTRADA
                              OUTPUT PROCEDURE IS SALIDA.
            STOP RUN.
       ENTRADA SECTION.
      ******************************************************************
       ARMAR-ENTRADA.
        PERFORM INICIO-ENTRADA.
        PERFORM LEER-PARAMETROS.
        PERFORM LEER-TIMES UNTIL (PAR-CUIT-HASTA >= TIM-CUIT
             AND TIM-CUIT >= PAR-CUIT-DESDE) OR EOF-TIM.
        PERFORM PROCESO-TIMES UNTIL FS-TIMES EQUAL '10'.
        PERFORM FIN-ENTRADA.
      ******************************************************************
       SALIDA SECTION.
      ******************************************************************
       ARMAR-SALIDA.
      *   DISPLAY "-------SALIDA-------".
      *   PERFORM LEER-ORDENADO UNTIL EOF-ORD.
        PERFORM INICIO-SALIDA.
        MOVE 0.0 TO DATO-TOTAL-GRAL-IMPORTE.
        PERFORM LEER-ORDENADO.
        PERFORM ESCRIBIR-ENCABEZADO.
        PERFORM PROCESO-ARCH-ORDENAR UNTIL EOF-ARCH-ORDENAR.
        DISPLAY AUX-TOTAL-GRAL-IMPORTE.
        MOVE AUX-TOTAL-GRAL-IMPORTE TO DATO-TOTAL-GRAL-IMPORTE.
        WRITE LINEA-LISTADO FROM LINEA-TOTAL-GRAL.
        PERFORM FIN-SALIDA.
       OTRA SECTION.
      ******************************************************************
       INICIO-ENTRADA.
         OPEN INPUT TIM.
         OPEN INPUT CONSULTORES.
         OPEN INPUT EMPRESAS.
         OPEN INPUT PARAMETROS.
      *   OPEN INPUT TARIFAS.
      ******************************************************************
       FIN-ENTRADA.
         CLOSE TIM.
         CLOSE CONSULTORES.
         CLOSE EMPRESAS.
         CLOSE PARAMETROS.
         CLOSE TARIFAS.
      ******************************************************************
       LEER-PARAMETROS.
         READ PARAMETROS.
      ******************************************************************
       LEER-TIMES.
         READ TIM RECORD.
      ******************************************************************
       LEER-TARIFAS.
         READ TARIFAS RECORD.
      ******************************************************************
       LEER-ORDENADO.
         RETURN ARCH-ORDENAR AT END MOVE "SI" TO EOF-ARCH-ORD.
      *   DISPLAY REG-ORDENAR.
      ******************************************************************
      *********************  METODOS DE ENTRADA  ***********************
      ******************************************************************
       PROCESO-TIMES.
         MOVE TIM-NUMERO TO CONS-ANT.
         PERFORM BUSQUEDA-CON-CONSULTOR.
         PERFORM PROCESO-CONS UNTIL FS-TIMES EQUAL '10'
             OR CONS-ANT NOT EQUAL TIM-NUMERO.
      ******************************************************************
       PROCESO-CONS.
         PERFORM BUSQUEDA-EN-EMPRESAS.
         MOVE TIM-HORAS TO REG-RELEASE-TIM-HORAS.
         MOVE TIM-CUIT TO REG-RELEASE-EMP-CUIT.
         MOVE TIM-NUMERO TO REG-RELEASE-CONS-NRO.
         MOVE TIM-FECHA TO REG-RELEASE-TIM-FECHA.
         PERFORM BUSQUEDA-EN-TARIFAS.
         RELEASE REG-ORDENAR FROM REG-RELEASE.
         MOVE 0 TO TIM-CUIT.
         PERFORM LEER-TIMES UNTIL (PAR-CUIT-HASTA >= TIM-CUIT
             AND TIM-CUIT >= PAR-CUIT-DESDE)OR EOF-TIM.
      ******************************************************************
       BUSQUEDA-CON-CONSULTOR.
         MOVE CONS-ANT TO CONS-NUMERO.
         READ CONSULTORES RECORD.
         IF OK-CONS THEN
             MOVE CONS-NOMBRE TO REG-RELEASE-CONS-NOMBRE
             MOVE CONS-SRT TO CATEGORIA-SRT
         ELSE
             DISPLAY "ERROR BUSQUEDA CON CONSULTOR".
      ******************************************************************
       BUSQUEDA-EN-EMPRESAS.
         MOVE TIM-CUIT TO EMP-CUIT.
         READ EMPRESAS RECORD KEY IS EMP-CUIT.
         IF OK-EMP THEN
             MOVE EMP-RAZON TO REG-RELEASE-EMP-RAZON
         ELSE
             DISPLAY "ERROR BUSQUEDA EN EMPRESAS".
      ******************************************************************
       BUSQUEDA-EN-TARIFAS.
      *   MOVE 15.00 TO REG-RELEASE-IMPORTE.
         OPEN INPUT TARIFAS.
         PERFORM LEER-TARIFAS UNTIL TAR-SRT EQUAL CATEGORIA-SRT.
         PERFORM UNTIL TAR-VIG-DES > REG-RELEASE-TIM-FECHA
             OR TAR-SRT <> CATEGORIA-SRT OR EOF-TAR
             MOVE REG-TARIFAS TO TARIFA-ANT
             PERFORM LEER-TARIFAS
         END-PERFORM.
         COMPUTE REG-RELEASE-IMPORTE = REG-RELEASE-TIM-HORAS
         * ANT-TARIFA.
         DISPLAY REG-RELEASE-TIM-HORAS," // ", ANT-TARIFA
         , " // ", REG-RELEASE-IMPORTE.
         CLOSE TARIFAS.
      ******************************************************************
      *********************  METODOS DE SALIDA  ************************
      ******************************************************************
       INICIO-SALIDA.
         OPEN OUTPUT LISTADO.
      ******************************************************************
       FIN-SALIDA.
         CLOSE LISTADO.
      ******************************************************************
       PROCESO-ARCH-ORDENAR.
         MOVE 1 TO LINEA-A-ESCRIBIR.
         MOVE ORD-EMP-RAZON TO EMP-ANT.
         MOVE ORD-EMP-RAZON TO DATO-EMPRESA-NOMBRE.
         MOVE ORD-EMP-CUIT TO DATO-EMPRESA-CUIT.
         WRITE LINEA-LISTADO FROM PRIMER-DATO-EMPRESA.
         WRITE LINEA-LISTADO FROM SEGUNDO-DATO-EMPRESA.
         WRITE LINEA-LISTADO FROM LINEA-VACIA.
         ADD 3 TO LINEA-A-ESCRIBIR.
      *   PERFORM UNTIL LINEA-A-ESCRIBIR EQUAL 55
      *       WRITE LINEA-LISTADO FROM LINEA-VACIA
      *       ADD 1 TO LINEA-A-ESCRIBIR
      *   END-PERFORM.
         DISPLAY "SALE".
         MOVE 0 TO AUX-TOTAL-POR-EMP-HORAS.
         MOVE 0 TO AUX-TOTAL-POR-EMP-IMPORTE.
         PERFORM PROCESO-POR-CUIT UNTIL EOF-ARCH-ORDENAR
             OR EMP-ANT NOT EQUAL ORD-EMP-RAZON.
         COMPUTE AUX-TOTAL-GRAL-IMPORTE = AUX-TOTAL-GRAL-IMPORTE
         + AUX-TOTAL-POR-EMP-IMPORTE.
         IF LINEA-A-ESCRIBIR > 60 THEN PERFORM SALTO-DE-PAGINA.
         MOVE AUX-TOTAL-POR-EMP-HORAS TO DATO-TOTAL-POR-EMP-HORAS.
         MOVE AUX-TOTAL-POR-EMP-IMPORTE TO DATO-TOTAL-POR-EMP-IMPORTE.
         WRITE LINEA-LISTADO FROM LINEA-TOTAL-POR-EMPRESA.
         ADD 1 TO LINEA-A-ESCRIBIR.
         PERFORM SALTO-DE-PAGINA.
      ******************************************************************
       PROCESO-POR-CUIT.
         MOVE 0 TO AUX-TOTAL-POR-FECHA-HORAS.
         MOVE 0 TO AUX-TOTAL-POR-FECHA-IMPORTE.
         MOVE ORD-TIM-FECHA TO FECHA-ANT.
         IF LINEA-A-ESCRIBIR > 57 THEN PERFORM SALTO-DE-PAGINA.
         WRITE LINEA-LISTADO FROM BARRA.
         WRITE LINEA-LISTADO FROM ENCABEZADO-TABLA.
         WRITE LINEA-LISTADO FROM BARRA.
         ADD 3 TO LINEA-A-ESCRIBIR.
         PERFORM PROCESO-POR-FECHA UNTIL EOF-ARCH-ORDENAR
             OR (EMP-ANT NOT EQUAL ORD-EMP-RAZON)
             OR (FECHA-ANT NOT EQUAL ORD-TIM-FECHA).
         COMPUTE AUX-TOTAL-POR-EMP-HORAS = AUX-TOTAL-POR-EMP-HORAS
         + AUX-TOTAL-POR-FECHA-HORAS.
         COMPUTE AUX-TOTAL-POR-EMP-IMPORTE = AUX-TOTAL-POR-EMP-IMPORTE
         + AUX-TOTAL-POR-FECHA-IMPORTE.
         IF LINEA-A-ESCRIBIR > 58 THEN PERFORM SALTO-DE-PAGINA.
         WRITE LINEA-LISTADO FROM BARRA-TOTAL.
         MOVE AUX-TOTAL-POR-FECHA-HORAS TO DATO-TOTAL-POR-FECHA-HORAS.
         MOVE AUX-TOTAL-POR-FECHA-IMPORTE
         TO DATO-TOTAL-POR-FECHA-IMPORTE.
         WRITE LINEA-LISTADO FROM LINEA-TOTAL-POR-FECHA.
         ADD 2 TO LINEA-A-ESCRIBIR.
      ******************************************************************
       PROCESO-POR-FECHA.
         MOVE ORD-CONS-NUMERO TO ORD-NRO.
         MOVE ORD-CONS-NOMBRE TO ORD-NOMBRE.
         MOVE ORD-TIM-ANIO TO ORD-ANIO.
         MOVE ORD-TIM-MES TO ORD-MES.
         MOVE ORD-TIM-DIA TO ORD-DIA.
         MOVE ORD-HORAS TO ORD-HS.
         MOVE ORD-IMPORTE TO ORD-IMP.
         DISPLAY ORD-IMPORTE," ",ORD-IMP.
         DISPLAY LINEA-ORD.
         IF LINEA-A-ESCRIBIR > 60 THEN PERFORM SALTO-DE-PAGINA.
         WRITE LINEA-LISTADO FROM LINEA-ORD.
         ADD 1 TO LINEA-A-ESCRIBIR.
         COMPUTE AUX-TOTAL-POR-FECHA-HORAS = AUX-TOTAL-POR-FECHA-HORAS
         + ORD-HORAS.
         COMPUTE AUX-TOTAL-POR-FECHA-IMPORTE =
         AUX-TOTAL-POR-FECHA-IMPORTE + ORD-IMPORTE.
         PERFORM LEER-ORDENADO.
      ******************************************************************
       ESCRIBIR-ENCABEZADO.
        MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
        MOVE WS-CURRENT-YEAR TO ANIO.
        MOVE WS-CURRENT-MONTH TO MES.
        MOVE WS-CURRENT-DAY TO DIA.
        MOVE 1 TO LINEA-A-ESCRIBIR.
        WRITE LINEA-LISTADO FROM BARRA.
        WRITE LINEA-LISTADO FROM ENCABEZADO.
        WRITE LINEA-LISTADO FROM LINEA-VACIA.
        ADD 3 TO LINEA-A-ESCRIBIR.
      ******************************************************************
       SALTO-DE-PAGINA.
      *  DISPLAY LINEA-A-ESCRIBIR.
        PERFORM UNTIL LINEA-A-ESCRIBIR EQUAL 61
            WRITE LINEA-LISTADO FROM LINEA-VACIA
            ADD 1 TO LINEA-A-ESCRIBIR
        END-PERFORM.
        ADD 1 TO HOJA.
        PERFORM ESCRIBIR-ENCABEZADO.
       END PROGRAM TP.
