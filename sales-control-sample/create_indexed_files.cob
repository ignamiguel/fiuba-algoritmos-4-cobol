
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CreateIndexedFromSeq.
      * AUTHOR.  nacho.
      * Creates indexed files from a sequential file.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          *> Output indexed files
          SELECT VentasFile ASSIGN TO "..\files\ventas_indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS RANDOM
          RECORD KEY IS Ventas-key
          FILE STATUS IS VentasStatus.

          SELECT SeqVentasFile ASSIGN TO "..\files\ventas-input.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT ComercioFile ASSIGN TO "..\files\comercios_indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS RANDOM
          RECORD KEY IS Comercio-key
          FILE STATUS IS ComercioStatus.

          SELECT SeqComercioFile ASSIGN TO
          "..\files\comercios-input.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT LimiteVentaFile ASSIGN TO
          "..\files\limite_venta_indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS RANDOM
          RECORD KEY IS LimiteVenta-key
          FILE STATUS IS LimiteVentaStatus.

          SELECT SeqLimiteVentaFile ASSIGN TO
          "..\files\limite-venta-input.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD VentasFile.
       01 VentasRecord.
         02 Ventas-key.
          04 Ventas-comercio-num         PIC 9(6).
          04 Ventas-moneda               PIC 9(1).
          04 Ventas-fecha                PIC X(8).
         02 Ventas-importe               PIC 9(3)V99.

       FD SeqVentasFile.
       01 SeqVentaRecord.
          88   EOF-Ventas VALUE HIGH-VALUES.
          02 FILLER                      PIC X(20).


       FD ComercioFile.
       01 ComercioRecord.
         02 Comercio-key.
           03 Comercio-num               PIC 9(6).
         02 Comercio-razon-social        PIC X(30).
         02 Comercio-direccion           PIC X(20).
         02 Comercio-cod-rubro           PIC 9(4).
         02 Comercio-limite-venta        PIC X(1).

       FD SeqComercioFile.
       01 SeqTarjetaRecord.
          88   EOF-Comercio VALUE HIGH-VALUES.
          02 FILLER                      PIC X(61).

       FD LimiteVentaFile.
       01 LimiteVentaRecord.
         02 LimiteVenta-key.
           03 LimiteVenta-letra          PIC X(1).
           03 LimiteVenta-fecha-desde    PIC 9(8).
           03 LimtieVenta-fecha-hasta    PIC 9(8).
         02 LimiteVenta-valor            PIC 9(6)V99.

       FD SeqLimiteVentaFile.
       01 SeqLimiteVentaRecord.
          88   EOF-LimiteVenta VALUE HIGH-VALUES.
          02 FILLER                      PIC X(25).

       WORKING-STORAGE SECTION.
       01   VentasStatus                 PIC X(2).
       01   ComercioStatus               PIC X(2).
       01   LimiteVentaStatus            PIC X(2).

       PROCEDURE DIVISION.
       Begin.

          PERFORM Open_files.
          PERFORM Read_files.

          PERFORM Create_indexed_files.

          PERFORM Close_files.
          STOP RUN.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Open_files.
          OPEN INPUT SeqVentasFile.
          OPEN OUTPUT VentasFile.
          OPEN INPUT SeqComercioFile.
          OPEN OUTPUT ComercioFile.
          OPEN INPUT SeqLimiteVentaFile.
          OPEN OUTPUT LimiteVentaFile.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Read_files.
          READ SeqVentasFile
             AT END SET EOF-Ventas TO TRUE
          END-READ.
          READ SeqComercioFile
             AT END SET EOF-Comercio TO TRUE
          END-READ.
          READ SeqLimiteVentaFile
             AT END SET EOF-LimiteVenta TO TRUE
          END-READ.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Create_indexed_files.
          PERFORM UNTIL EOF-Ventas
          DISPLAY SeqVentaRecord
             WRITE VentasRecord FROM SeqVentaRecord
             INVALID KEY DISPLAY "Venta STATUS :- ", VentasStatus
             END-WRITE
             READ SeqVentasFile
             AT END SET EOF-Ventas TO TRUE
             END-READ
          END-PERFORM.

          PERFORM UNTIL EOF-Comercio
          DISPLAY SeqTarjetaRecord
             WRITE ComercioRecord FROM SeqTarjetaRecord
             INVALID KEY DISPLAY "Comercio STATUS :- ", ComercioStatus
             END-WRITE
             READ SeqComercioFile
             AT END SET EOF-Comercio TO TRUE
             END-READ
          END-PERFORM.

          PERFORM UNTIL EOF-LimiteVenta
          DISPLAY SeqLimiteVentaRecord
             WRITE LimiteVentaRecord FROM SeqLimiteVentaRecord
             INVALID KEY DISPLAY "LimiteVenta STATUS :- ",
             LimiteVentaStatus
             END-WRITE
             READ SeqLimiteVentaFile
             AT END SET EOF-LimiteVenta TO TRUE
             END-READ
          END-PERFORM.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Close_files.
          CLOSE VentasFile, SeqVentasFile.
          CLOSE ComercioFile, SeqComercioFile.
          CLOSE LimiteVentaFile, SeqLimiteVentaFile.
       END PROGRAM CreateIndexedFromSeq.
