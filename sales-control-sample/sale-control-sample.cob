       IDENTIFICATION DIVISION.
       PROGRAM-ID. SaleControl-sample.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT VentasFile ASSIGN TO "..\files\ventas_indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS Ventas-key
          FILE STATUS IS VentasFS.

          SELECT RubrosFile ASSIGN TO "..\files\rubros.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT ComerciosFile ASSIGN TO
          "..\files\comercios_indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS Comercio-key
          FILE STATUS IS ComercioFS.

          SELECT LimiteVentaFile ASSIGN TO
          "..\files\limite_venta_indexed.dat"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS LimiteVenta-key
          FILE STATUS IS LimiteVentaFS.

          SELECT WorkFile ASSIGN TO "..\files\work_file.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

          SELECT OutputReportFile ASSIGN TO "..\files\sales_report.dat"
          ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD VentasFile.
       01 VentasRecord.
           88  Ventas-EOF                VALUE HIGH-VALUE.
         02 Ventas-key.
          04 Ventas-comercio-num         PIC 9(6).
          04 Ventas-moneda               PIC 9(1).
          04 Ventas-fecha                PIC X(8).
         02 Ventas-nro-card              PIC 9(4).
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
            88 LimiteVenta-EOF           VALUE HIGH-VALUES.
         02 LimiteVenta-key.
           03 LimiteVenta-letra          PIC X(1).
           03 LimiteVenta-fecha-desde    PIC 9(8).
           03 LimiteVenta-fecha-hasta    PIC 9(8).
         02 LimiteVenta-valor            PIC 9(6)V99.

       SD WorkFile.
       01 SortRecord.
           88 WorkFile-EOF               VALUE HIGH-VALUE.
         02 sort-key                     PIC X(6).
         02 FILLER                       PIC X(64).

       FD OutputReportFile.
       01 OutputReportRecord.
         02 FILLER                       PIC X(70).

       WORKING-STORAGE SECTION.
       01 VentasFS                       PIC X(2).
       01 LimiteVentaFS                  PIC X(2).
          88 LimiteVenta_success         VALUE "00".
       01 ComercioFS                     PIC X(2).
          88 comercio_success            VALUE "00".

       01 Comercio_valid                 PIC X(1).
          88  commerce_ok                VALUE HIGH-VALUE.
          88  commerce_invalid           VALUE LOW-VALUE.

       01 Rubros_table.
         02 Rubros_element OCCURS 3 TIMES INDEXED BY I.
           03 rubros_code                PIC 9(4).
           03 rubros_description         PIC X(30).
           03 rubros_scount              PIC X(2).
           03 rubros_campo2              PIC X(2).
           03 rubros_campo3              PIC X(2).
           03 rubros_campo4              PIC X(2).


       01 WS-CURRENT-DATE-FIELDS.
        02 WS-DATE-YEAR                  PIC X(4).
        02 WS-DATE-MONTH                 PIC X(2).
        02 WS-DATE-DAY                   PIC X(2).
        02 WS-TIME-HOUR                  PIC X(2).
        02 WS-TIME-MINUTE                PIC X(2).

       01 page_num                       PIC 9(2).

       01 header_line_1.
         02 FILLER                       PIC X(7) VALUE "Fecha: ".
         02 header_line_date_day         PIC X(2).
         02 FILLER                       PIC X(1) VALUE "/".
         02 header_line_date_month       PIC X(2).
         02 FILLER                       PIC X(1) VALUE "/".
         02 header_line_date_year        PIC X(4).
         02 FILLER                       PIC X(45) VALUE ALL SPACES.
         02 FILLER                       PIC X(6) VALUE "Hoja: ".
         02 header_line_page_num         PIC X(2).

       01 header_line_2.
         02 FILLER                       PIC X(70) VALUE
         "           Total de Compras de los Comercios por Rubro".

       01 header_line_3.
         02 FILLER                       PIC X(70) VALUE
         "           -------------------------------------------".

       01 empty_line.
         02 FILLER                       PIC X(70) VALUE ALL SPACES.


       PROCEDURE DIVISION.

       Main.
            SORT WorkFile ON DESCENDING KEY sort-key
                              INPUT PROCEDURE IS Input_Process
                              OUTPUT PROCEDURE IS Output_Process.


           STOP RUN.
       Input_Process SECTION.
        PERFORM Open_files.
           PERFORM Read_files.
           PERFORM Load_tables.
           PERFORM Process_files.
           PERFORM Close_files.

       EXIT SECTION.
       Output_Process SECTION.
         OPEN OUTPUT OutputReportFile.
         INITIALIZE page_num.
         PERFORM Print_header.

         PERFORM Get_record_from_sort_file.

         PERFORM UNTIL WorkFile-EOF

            MOVE sort_rubro TO ws_rubro
            PERFORM Copy_rubro_detals

            PERFORM UNTIL sort-rubro <> ws_rubro
            END-PERFORM

            WRITE OutputReportRecord FROM SortRecord

            PERFORM Get_record_from_sort_file
         END-PERFORM.
         CLOSE OutputReportFile.
       EXIT SECTION.

       Copy_rubro_detals.
       *> To do...

       Get_record_from_sort_file.
          RETURN WorkFile AT END SET WorkFile-EOF TO TRUE.

       Print_header.
          ADD 1 TO page_num.
          MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
          MOVE WS-DATE-DAY TO header_line_date_day.
          MOVE WS-DATE-MONTH TO header_line_date_month.
          MOVE WS-DATE-YEAR TO header_line_date_year.
          MOVE page_num TO header_line_page_num.
          WRITE OutputReportRecord FROM header_line_1.
          WRITE OutputReportRecord FROM empty_line.
          WRITE OutPutReportRecord FROM header_line_2.
          WRITE OutputReportRecord FROM header_line_3.

       Open_files.
         OPEN INPUT VentasFile.
         *> file status = 35
         *> not found
         *>DISPLAY VentasFS.
         OPEN INPUT RubrosFile.
         OPEN INPUT ComerciosFile.
         OPEN INPUT LimiteVentaFile.

       Read_files.
          READ VentasFile NEXT RECORD
             AT END SET Ventas-EOF TO TRUE
          END-READ.

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

          *> Decide if record must be stored
          IF (Ventas-importe > LimiteVenta-valor) AND
             Ventas-fecha >= LimiteVenta-fecha-desde AND
             Ventas-fecha <= LimiteVenta-fecha-hasta THEN

             DISPLAY VentasRecord
             MOVE VentasRecord TO SortRecord
             RELEASE SortRecord

          END-IF.

       Get_limits.
          *> Set search filter
          MOVE Comercio-limite-venta TO LimiteVenta-letra.
          *> Force since date and upto date parameters
          MOVE "20160701" TO LimiteVenta-fecha-desde.
          MOVE "20160801" TO LimiteVenta-fecha-hasta.

          START LimiteVentaFile KEY IS EQUAL TO LimiteVenta-key
          END-START.

          IF LimiteVenta_success
             READ LimiteVentaFile NEXT RECORD
                AT END SET LimiteVenta-EOF TO TRUE
             END-READ
          ELSE
              *> To do error handeling
              DISPLAY "limite venta not found"
          END-IF.

       Close_files.
         CLOSE VentasFile.
         CLOSE RubrosFile.
         CLOSE ComerciosFile.
         CLOSE LimiteVentaFile.

       END PROGRAM SaleControl-sample.
