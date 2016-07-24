       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CreditCard-Sample.
      * AUTHOR:  nacho.
      * Sequential reading of an indexed file

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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

       01   RequiredSequence         PIC 9.
            88 VideoCodeSequence    VALUE 1.
            88 VideoTitleSequence   VALUE 2.

       01 PrnVideoRecord.
          02 PrnVideoCode           PIC 9(5).
          02 PrnVideoTitle          PIC BBBBX(40).
          02 PrnSupplierCode        PIC BBBB99.

       PROCEDURE DIVISION.
       Begin.
          PERFORM OPEN-FILES.

      *   DISPLAY "Enter key : 1=VideoCode, 2=VideoTitle ->"
      *       WITH NO ADVANCING.

      *    ACCEPT RequiredSequence.

          *> IF VideoTitleSequence
             *> MOVE SPACES TO VideoTitle
             *> START SaldoFile KEY IS GREATER THAN VideoTitle
         *> INVALID KEY  DISPLAY "VIDEO STATUS :- ", VideoStatus
             *> END-START
          *> END-IF

          PERFORM READ-FIRST.
          PERFORM PRINT-FILES.
          PERFORM CLOSE-FILES.
          STOP RUN.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       OPEN-FILES.
      *   Start open file.
          OPEN INPUT SaldoFile.
          OPEN INPUT TarjetasFile.
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
      * End read first.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       PRINT-FILES.
      * Start print files.
        DISPLAY "Saldos:".
        PERFORM UNTIL EOF-SALDO
             *> MOVE VideoCode TO PrnVideoCode
             *> MOVE VideoTitle TO PrnVideoTitle
             *> MOVE SupplierCode TO PrnSupplierCode
             *> DISPLAY  PrnVideoRecord
             DISPLAY  SaldoRecord
             READ SaldoFile NEXT RECORD
         AT END SET EOF-SALDO TO TRUE
             END-READ
          END-PERFORM.
          DISPLAY "".
          DISPLAY "Tarjetas:".
          PERFORM UNTIL EOF-TARJETA
             *> MOVE VideoCode TO PrnVideoCode
             *> MOVE VideoTitle TO PrnVideoTitle
             *> MOVE SupplierCode TO PrnSupplierCode
             *> DISPLAY  PrnVideoRecord
             DISPLAY  TarjetaRecord
             READ TarjetasFile NEXT RECORD
         AT END SET EOF-TARJETA TO TRUE
             END-READ
          END-PERFORM.
      * End print files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CLOSE-FILES.
      * Start close files.
        CLOSE SaldoFile.
        CLOSE TarjetasFile.
      * End close files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       END PROGRAM CreditCard-Sample.
