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
        03 C1-NRO-CUPON           PIC 9(5).
        03 C1-FECHA-COMPRA.
          06 C1-FILLER            PIC X(2).
          06 C1-DAY               PIC X(2).
          06 C1-MONTH             PIC X(2).
          06 C1-YEAR              PIC X(4).
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
           03 TJ-NRO-TARJ        PIC 9(10).
         02 TJ-TITULAR           PIC X(30).
         02 TJ-DOCUMENTO         PIC 9(11).

       WORKING-STORAGE SECTION.
       01   VideoStatus              PIC X(2).
       01   SaldoStatus              PIC X(2).
       01   TarjetaStatus            PIC X(2).
       01   WS-CreditCardValid       PIC X(1).
          88 CC-VALID VALUE HIGH-VALUE.
          88 CC-INVALID VALUE LOW-VALUE.
       01   WS-Saldo-amount          PIC 9(10)V99.
       01   WS-total-amount          PIC 9(10)V99.
       01   WS-cupon-counter         PIC 9(2).

       01 WS-C1.
        03 WS-nro-tarjeta            PIC 9(10).
        03 WS-C1-NRO-CUPON            PIC 9(5).
        03 WS-C1-FECHA-COMPRA            PIC X(10).
        03 WS-C1-IMPORTE            PIC 9(6)V99.

       01 WS-TJ-KEY.
         03 WS-TJ-NRO-TARJ              PIC 9(10).

       01 WS-SALD-KEY.
          02 WS-SALD-NRO-TARJ            PIC 9(10).
          02 WS-SALD-FECHA               PIC X(10).


       PROCEDURE DIVISION.
       Begin.
          PERFORM Open_All_Files.
          PERFORM Read_Sequential_Files.
          PERFORM Process_All_Files.
          PERFORM Close_All_Files.
          STOP RUN.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Open_All_Files.
          OPEN INPUT SaldoFile.
          OPEN INPUT TarjetasFile.
          OPEN INPUT Cupon1_file.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Read_Sequential_Files.
          READ Cupon1_file NEXT RECORD
             AT END SET EOF-CUPON-1 TO TRUE
          END-READ.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Process_All_Files.
         PERFORM UNTIL EOF-CUPON-1
             DISPLAY "Processing CC -> " C1-NRO-TARJ
             PERFORM Process-CreditCard

          END-PERFORM.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Process-CreditCard.
          PERFORM Check_CreditCard.

          IF CC-VALID
                DISPLAY "VALID CC"
                PERFORM Print_CreditCard_Details
                PERFORM Print_Saldo
                PERFORM Process_All_Cupons_For_CC
                PERFORM Print_Amounts
          ELSE
                *>DISPLAY "INVALID CC move to next record"
                PERFORM Move_to_Next_CC
          END-IF.

      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Print_Amounts.
           DISPLAY "------------------------------------".
           DISPLAY "Total de la tarjeta: " WS-total-amount.
           COMPUTE WS-Saldo-amount = FUNCTION NUMVAL(WS-Saldo-amount)
           END-COMPUTE
           COMPUTE WS-total-amount = WS-total-amount + WS-Saldo-amount.
           DISPLAY "Saldo final: " WS-total-amount.
           DISPLAY "------------------------------------".
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Process_All_Cupons_For_CC.
         DISPLAY "------------------------------------".
         DISPLAY "Cupones".
         MOVE 1 TO WS-cupon-counter.
         MOVE 0 TO WS-total-amount.

         *> Backup value into another variable so content
         *> can be updated when reading next record.
         MOVE C1-NRO-TARJ TO WS-nro-tarjeta.

         PERFORM UNTIL C1-NRO-TARJ <> WS-nro-tarjeta
            PERFORM Print_Cupon_Details

            MOVE C1-IMPORTE TO WS-C1-IMPORTE
            COMPUTE WS-C1-IMPORTE = FUNCTION NUMVAL(WS-C1-IMPORTE)
            END-COMPUTE
            COMPUTE WS-total-amount = (WS-total-amount + WS-C1-IMPORTE)

            READ Cupon1_file NEXT RECORD
             AT END SET EOF-CUPON-1 TO TRUE
            END-READ

            ADD 1 TO WS-cupon-counter

         END-PERFORM.
         DISPLAY "------------------------------------".
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Print_Cupon_Details.
         DISPLAY "[" WS-cupon-counter "]".
         DISPLAY "   Nro Cupon: " C1-NRO-CUPON.
         DISPLAY "   Fecha compra: " C1-DAY "/" C1-MONTH "/"C1-YEAR.
         DISPLAY "   Importe: " C1-IMPORTE.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Move_to_Next_CC.
        MOVE C1-NRO-TARJ TO WS-nro-tarjeta.
        PERFORM UNTIL C1-NRO-TARJ <> WS-nro-tarjeta
             READ Cupon1_file NEXT RECORD
              AT END SET EOF-CUPON-1 TO TRUE
             END-READ
        END-PERFORM.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Check_CreditCard.

        MOVE C1-NRO-TARJ TO TJ-NRO-TARJ.

        START TarjetasFile KEY IS EQUAL TO TJ-KEY
         *>INVALID KEY DISPLAY "Invalid CC Key :- ", TarjetaStatus
         *>NOT INVALID KEY DISPLAY "Tarjeta Pointer Updated "TarjetaStatus
        END-START.

        IF TarjetaStatus = "00"
           *>DISPLAY "HIGH-VALUE TO WS-CreditCardValid"
           MOVE HIGH-VALUE TO WS-CreditCardValid
           READ TarjetasFile NEXT RECORD
              AT END SET EOF-TARJETA TO TRUE
           END-READ
        ELSE
           *>DISPLAY "LOW-VALUE TO WS-CreditCardValid"
           MOVE LOW-VALUE TO WS-CreditCardValid
        END-If.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Print_CreditCard_Details.
        DISPLAY "------------------------------------".
        DISPLAY "CC Details".
        DISPLAY "Titular: " TJ-TITULAR.
        DISPLAY "Documento: " TJ-DOCUMENTO.
        DISPLAY "Nro Tarjeta: " TJ-NRO-TARJ.
        DISPLAY "------------------------------------".
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Print_Saldo.
         DISPLAY "------------------------------------".
         MOVE C1-NRO-TARJ TO SALD-NRO-TARJ.
         MOVE "  10062016" TO SALD-FECHA.

         START SaldoFile KEY IS EQUAL TO SALD-KEY
          *>INVALID KEY DISPLAY "Invalid Saldo Key :- ", SaldoStatus
          *>NOT INVALID KEY DISPLAY "Saldo Pointer Updated :- "SaldoStatus
         END-START.

        IF SaldoStatus = "00"
           READ SaldoFile NEXT RECORD
              AT END SET EOF-SALDO TO TRUE
           END-READ
           MOVE SALD-IMPORTE TO WS-Saldo-amount
           DISPLAY "Saldo anterior: " WS-Saldo-amount
        ELSE
           MOVE 0 TO WS-Saldo-amount
           DISPLAY "Saldo anterior: 0,00"
        END-IF.
        DISPLAY "------------------------------------".
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       Close_All_Files.
      * Start close files.
         CLOSE SaldoFile.
         CLOSE TarjetasFile.
         CLOSE Cupon1_file.
      * End close files.
      *-----------------------------------------------------------*
      *-----------------------------------------------------------*
       END PROGRAM CreditCard-Sample.
