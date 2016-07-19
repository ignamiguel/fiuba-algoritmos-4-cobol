       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CreditCard-Sample.
      * AUTHOR:  nacho.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          *>Cupons
          SELECT Cupon1_file ASSIGN TO '..\files\CUPON1.dat'
          ORGANIZATION IS LINE SEQUENTIAL.

          *> SELECT Cupon2 ASSIGN TO '..\files\CUPON2.dat'
          *> ORGANIZATION IS LINE SEQUENTIAL.

          *> SELECT Cupon3 ASSIGN TO '..\files\CUPON3.dat'
          *> ORGANIZATION IS LINE SEQUENTIAL.

          SELECT SaldoFile ASSIGN TO "..\files\SALDOS.DAT"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS SALD-KEY
          FILE STATUS IS SaldoStatus.

          SELECT TarjetasFile ASSIGN TO "..\files\TARJETAS.DAT"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS TJ-KEY
          FILE STATUS IS TarjetaStatus.

       DATA DIVISION.
       FILE SECTION.

       FD Cupon1_file.
       01 CuponRecord.
         88 EOF-CUPON-1 VALUE HIGH-VALUE.
        03 C1-NRO-TARJ            PIC 9(10).
        03 C1-NRO-CUPON            PIC 9(5).
        03 C1-FECHA-COMPRA            PIC X(10).
        03 C1-IMPORTE            PIC 9(6)V99.

       FD SaldoFile.
       01 SaldoRecord.
         88 EOF-SALDO VALUE HIGH-VALUE.
         02 SALD-KEY.
          04 SALD-NRO-TARJ            PIC 9(10).
          04 SALD-FECHA               PIC X(10).
         02 SALD-IMPORTE             PIC 9(6)V99.

       FD TarjetasFile.
       01 TarjetaRecord.
         88 EOF-TARJETA VALUE HIGH-VALUE.
         02 TJ-KEY.
          03 SeqTJ-NRO-TARJ        PIC 9(10).
         02 SeqTJ-TITULAR           PIC X(30).
         02 SeqTJ-DOCUMENTO         PIC 9(11).

       WORKING-STORAGE SECTION.
       01   SaldoStatus               PIC X(2).
       01   TarjetaStatus               PIC X(2).

       01 WS-CUPON-1.
        03 WS-C1-NRO-TARJ            PIC 9(10).
        03 WS-C1-NRO-CUPON            PIC 9(5).
        03 WS-C1-FECHA-COMPRA            PIC X(10).
        03 WS-C1-IMPORTE            PIC 9(6)V99.

        01 WS-EOF PIC A(1) VALUE 'N'.

       PROCEDURE DIVISION.
       Begin.
          PERFORM OPEN-FILES.
          PERFORM READ-FIRST.
          PERFORM PROCESS-FILES.
          PERFORM CLOSE-FILES.
          STOP RUN.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       OPEN-FILES.
      *   Start open file.
          OPEN INPUT SaldoFile.
          OPEN INPUT TarjetasFile.
          OPEN INPUT Cupon1_file.
      *   End open file.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       READ-FIRST.
      * Start read first.
          READ SaldoFile NEXT RECORD
             AT END SET EOF-SALDO TO TRUE
          END-READ.

          READ TarjetasFile NEXT RECORD
             AT END SET EOF-TARJETA TO TRUE
          END-READ.

          READ Cupon1_file NEXT RECORD
             AT END SET EOF-CUPON-1 TO TRUE
          END-READ.


          *> MOVE 'N' TO WS-EOF.


         *> PERFORM UNTIL WS-EOF='Y'
          *> READ Cupon1_file INTO WS-CUPON-1
             *> AT END MOVE 'Y' TO WS-EOF
             *> *>AT END SET EOF-CUPON-1 TO TRUE
             *> NOT AT END DISPLAY WS-CUPON-1
          *> END-READ
         *> END-PERFORM.

      * End read first.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       PROCESS-FILES.
      * Start process files.

        PERFORM UNTIL EOF-CUPON-1


             DISPLAY "Numero tarjeta -> "C1-NRO-TARJ

             READ Cupon1_file NEXT RECORD
               AT END SET EOF-CUPON-1 TO TRUE
             END-READ
          END-PERFORM.

        *> PERFORM UNTIL EOF-SALDO
             *> *> MOVE VideoCode TO PrnVideoCode
             *> *> MOVE VideoTitle TO PrnVideoTitle
             *> *> MOVE SupplierCode TO PrnSupplierCode
             *> *> DISPLAY  PrnVideoRecord
             *> DISPLAY  SaldoRecord
             *> READ SaldoFile NEXT RECORD
         *> AT END SET EOF-SALDO TO TRUE
             *> END-READ
          *> END-PERFORM.
          *> DISPLAY "".
          *> DISPLAY "Tarjetas:".
          *> PERFORM UNTIL EOF-TARJETA
             *> *> MOVE VideoCode TO PrnVideoCode
             *> *> MOVE VideoTitle TO PrnVideoTitle
             *> *> MOVE SupplierCode TO PrnSupplierCode
             *> *> DISPLAY  PrnVideoRecord
             *> DISPLAY  TarjetaRecord
             *> READ TarjetasFile NEXT RECORD
         *> AT END SET EOF-TARJETA TO TRUE
             *> END-READ
          *> END-PERFORM.
      * End process files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CLOSE-FILES.
      * Start close files.
        CLOSE SaldoFile.
        CLOSE TarjetasFile.
        CLOSE Cupon1_file.
      * End close files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       END PROGRAM CreditCard-Sample.
