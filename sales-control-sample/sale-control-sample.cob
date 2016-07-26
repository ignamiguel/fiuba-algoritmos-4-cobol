       IDENTIFICATION DIVISION.
       PROGRAM-ID. SaleControl-sample.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT VentasFile ASSIGN TO "..\files\ventas-indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS Ventas-key
          FILE STATUS IS VentasFS.

          SELECT RubrosFile ASSIGN TO "..\files\rubros.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT ComerciosFile ASSIGN TO
          "..\files\comercios-indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS Comercio-key
          FILE STATUS IS ComercioFS.

          SELECT LimiteVentaFile ASSIGN TO
          "..\files\limite-venta-indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS LimiteVenta-key
          FILE STATUS IS LimiteVentasFS.


       DATA DIVISION.
       FILE SECTION.
       FD VentasFile.
       01 VentasRecord.
            88 Ventas-EOF              VALUE HIGH-VALUE.
         02 Ventas-key.
          04 Ventas-comercio-num         PIC 9(6).
          04 Ventas-moneda               PIC 9(1).
          04 Ventas-fecha                PIC X(8).
         02 Ventas-importe               PIC 9(3)V99.

       FD ComerciosFile.
       01 ComercioRecord.
            88  Comercios-EOF            VALUE HIGH-VALUE.
         02 Comercio-key.
           03 Comercio-num               PIC 9(6).
         02 Comercio-razon-social        PIC X(30).
         02 Comercio-direccion           PIC X(20).
         02 Comercio-cod-rubro           PIC 9(4).
         02 Comercio-limite-venta        PIC X(1).

       FD RubrosFile.
       01 RubrosRecord.
           88 Rubros-EOF                 VALUE HIGH-VALUE.
         02 Rubro-codigo                 PIC 9(4).
         02 Rubro-descripcion            PIC X(30).
         02 Rubro-descuento              PIC 9(2).
         02 Rubro-descuento              PIC 9(2).
         02 Rubro-campo2                 PIC 9(2).
         02 Rubro-campo3                 PIC 9(2).
         02 Rubro-campo4                 PIC 9(2).

       FD LimiteVentaFile.
       01 LimiteVentaRecord.
         02 LimiteVenta-key.
           03 LimiteVenta-letra          PIC X(1).
           03 LimiteVenta-fecha-desde    PIC 9(8).
           03 LimtieVenta-fecha-hasta    PIC 9(8).
         02 LimiteVenta-valor            PIC 9(6)V99.


       WORKING-STORAGE SECTION.
       01 VentasFS                       PIC X(2).
       01 LimiteVentasFS                 PIC X(2).
          88 LimiteVenta_success         VALUE "00".
       01 ComercioFS                     PIC X(2).
          88 comercio_success            VALUE "00".

       01 Comercio_valid                 PIC X(1).
          88  commerce_ok                VALUE HIGH-VALUE.
          88  commerce_invalid           VALUE LOW-VALUE.

       PROCEDURE DIVISION.

       Main.
           PERFORM Open_files.
           PERFORM Read_files.
           PERFORM Load_tables.
           PERFORM Process_files.
           PERFORM Close_files.
           STOP RUN.

       Open_files.
         OPEN INPUT VentasFile.
         OPEN INPUT RubrosFile.
         OPEN INPUT ComerciosFile.

       Read_files.
          READ VentasFile NEXT RECORD
             AT END SET Ventas-EOF TO TRUE
          END-READ.
          DISPLAY VentasFS.
          DISPLAY VentasRecord.

          READ RubrosFile NEXT RECORD
             AT END SET Rubros-EOF TO TRUE
          END-READ.

          READ ComerciosFile NEXT RECORD
             AT END SET Comercios-EOF TO TRUE
          END-READ.

       Load_tables.
       *> To do...

       Process_files.
          PERFORM UNTIL Ventas-EOF
              PERFORM Process_sale

              READ VentasFile NEXT RECORD
                 AT END SET Ventas-EOF TO TRUE
              END-READ

          END-PERFORM.

       Process_sale.
          PERFORM Validate_commerce.

          IF commerce_ok
             PERFORM Evalute_sale
          END-IF.

       Validate_commerce.
       *> Set search filter
         MOVE Ventas-comercio-num TO Comercio-key.

         DISPLAY Comercio-key.
         STOP RUN.

       *> Search in commerce master file
         START ComerciosFile KEY IS EQUAL TO Comercio-key
         END-START.

         *> ComercioFS = 00
         IF comercio_success
            MOVE HIGH-VALUES TO Comercio_valid
            READ ComerciosFile NEXT RECORD
            AT END SET Comercios-EOF TO TRUE
            END-READ
         ELSE
            DISPLAY "commerce not found"
            MOVE LOW-VALUES  TO Comercio_valid
            STOP RUN
         END-IF.

       Evalute_sale.
       *> Set parameters
       *> ...

       *> Get limits
       *> CALL "getlimit" USING BY CONTENT XXX,
                      *> BY REFERNCE
                       *> BY REFERENCE XXX.
           PERFORM Get_limits.

       *> Decide if it must be stored

       Get_limits.
       *> Set search filter
          MOVE Comercio-limite-venta TO LimiteVenta-letra.

          MOVE "20160701" TO LimiteVenta-fecha-desde.
          MOVE "20160801" TO LimtieVenta-fecha-hasta.

          START LimiteVentaFile KEY IS EQUAL TO LimiteVenta-key
          END-START.

          IF LimiteVenta_success
             DISPLAY "limite found!!!"
          ELSE
              DISPLAY "limite venta not found"
          END-IF.
       Close_files.
         CLOSE VentasFile.
         CLOSE RubrosFile.
         CLOSE ComerciosFile.

       END PROGRAM SaleControl-sample.
