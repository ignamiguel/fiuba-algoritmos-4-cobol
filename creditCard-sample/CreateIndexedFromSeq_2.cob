
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CreateIndexedFromSeq.
      * AUTHOR.  nacho.
      * Creates an indexed file  from a sequential file.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT SaldoFile ASSIGN TO "..\files\SALDOS.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS RANDOM
          RECORD KEY IS SALD-KEY
          FILE STATUS IS SaldoStatus.

          SELECT SeqSaldoFile ASSIGN TO "..\files\INPUT-SALDOS.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT TarjetasFile ASSIGN TO "..\files\TARJETAS.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS RANDOM
          RECORD KEY IS TJ-KEY
          FILE STATUS IS TarjetaStatus.

          SELECT SeqTarjetasFile ASSIGN TO "..\files\INPUT-TARJETAS.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD SaldoFile.
       01 SaldoRecord.
         02 SALD-KEY.
          04 SALD-NRO-TARJ            PIC 9(10).
          04 SALD-FECHA               PIC X(10).
         02 SALD-IMPORTE             PIC 9(6)V99.

       FD SeqSaldoFile.
       01 SeqSaldoRecord.
          88   EOF-SALDO VALUE HIGH-VALUES.
          02 SeqSALD-KEY.
            04 SeqSALD-NRO-TARJ        PIC 9(10).
            04 SeqSALD-FECHA           PIC X(10).
          02 SeqSALD-IMPORTE         PIC 9(6)V99.

       FD TarjetasFile.
       01 TarjetaRecord.
         02 TJ-KEY.
           03 SeqTJ-NRO-TARJ        PIC 9(10).
         02 SeqTJ-TITULAR           PIC X(30).
         02 SeqTJ-DOCUMENTO         PIC 9(11).

       FD SeqTarjetasFile.
       01 SeqTarjetaRecord.
          88   EOF-TARJETA VALUE HIGH-VALUES.
          02 SeqTJ-KEY.
            03 SeqTJ-NRO-TARJ        PIC 9(10).
          02 SeqTJ-TITULAR           PIC X(30).
          02 SeqTJ-DOCUMENTO         PIC 9(11).

       WORKING-STORAGE SECTION.
       01   SaldoStatus               PIC X(2).
       01   TarjetaStatus               PIC X(2).

       PROCEDURE DIVISION.
       Begin.

          PERFORM OPEN-FILES.
          PERFORM READ-FILES.

          PERFORM CREATE-INDEXED-FILES.

          PERFORM CLOSE-FILES.
          STOP RUN.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       OPEN-FILES.
      *   Start open file
          OPEN INPUT SeqSaldoFile.
          OPEN OUTPUT SaldoFile.
          OPEN INPUT SeqTarjetasFile.
          OPEN OUTPUT TarjetasFile.
      *   End open files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       READ-FILES.
      *   Start read files.
          READ SeqSaldoFile
             AT END SET EOF-SALDO TO TRUE
          END-READ.
          READ SeqTarjetasFile
             AT END SET EOF-TARJETA TO TRUE
          END-READ.
      *   End read files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CREATE-INDEXED-FILES.
      *   Start create indexed files.
          PERFORM UNTIL EOF-SALDO
          DISPLAY SeqSaldoRecord
             WRITE SaldoRecord FROM SeqSaldoRecord
             INVALID KEY DISPLAY "SALDO STATUS :- ", SaldoStatus
             END-WRITE
             READ SeqSaldoFile
             AT END SET EOF-SALDO TO TRUE
             END-READ
          END-PERFORM.

          PERFORM UNTIL EOF-TARJETA
          DISPLAY SeqTarjetaRecord
             WRITE TarjetaRecord FROM SeqTarjetaRecord
             INVALID KEY DISPLAY "TARJETA STATUS :- ", TarjetaStatus
             END-WRITE
             READ SeqTarjetasFile
             AT END SET EOF-TARJETA TO TRUE
             END-READ
          END-PERFORM.
      *   End create indexed files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       CLOSE-FILES.
      *   Start close files.
          CLOSE SaldoFile, SeqSaldoFile.
          CLOSE TarjetasFile, SeqTarjetasFile.
      *   End close files.
       END PROGRAM CreateIndexedFromSeq.
