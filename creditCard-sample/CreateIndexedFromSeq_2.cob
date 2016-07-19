
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
          88   EndOfFile VALUE HIGH-VALUES.
          02 SeqSALD-NRO-TARJ        PIC 9(10).
          02 SeqSALD-FECHA           PIC X(10).
          02 SeqSALD-IMPORTE         PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       01   SaldoStatus               PIC X(2).

       PROCEDURE DIVISION.
       Begin.
          OPEN INPUT SeqSaldoFile.
          OPEN OUTPUT SaldoFile.

          READ SeqSaldoFile
             AT END SET EndOfFile TO TRUE
          END-READ.

          PERFORM UNTIL EndOfFile
      *    DISPLAY SeqSaldoRecord
             WRITE SaldoRecord FROM SeqSaldoRecord
             INVALID KEY DISPLAY "VIDEO STATUS :- ", SaldoStatus
             END-WRITE
             READ SeqSaldoFile
             AT END SET EndOfFile TO TRUE
             END-READ
          END-PERFORM.

          CLOSE SaldoFile, SeqSaldoFile.
          STOP RUN.
       END PROGRAM CreateIndexedFromSeq.
