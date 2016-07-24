       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkcc.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT TarjetasFile ASSIGN TO "..\files\TARJETAS.DAT"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS TJ-KEY
          FILE STATUS IS TarjetaStatus.

       DATA DIVISION.
       FILE SECTION.

       FD TarjetasFile.
       01 TarjetaRecord.
         88 EOF-TARJETA VALUE HIGH-VALUE.
         02 TJ-KEY.
           03 TJ-NRO-TARJ        PIC 9(10).
         02 TJ-TITULAR           PIC X(30).
         02 TJ-DOCUMENTO         PIC 9(11).

       WORKING-STORAGE SECTION.
       01   TarjetaStatus            PIC X(2).

       LINKAGE SECTION.
       01 LS-STUDENT-ID PIC 9(4).
       01 LS-STUDENT-NAME PIC A(15).

       01  LS-CreditCardValid       PIC X(1).
          88 CC-VALID VALUE HIGH-VALUE.
          88 CC-INVALID VALUE LOW-VALUE.

       01 LS-TJ-KEY-INPUT.
         03 LS-TJ-NRO-TARJ-INPUT  PIC 9(10).

        01 LS-TarjetaRecord.
         02 LS-TJ-KEY.
           03 LS-TJ-NRO-TARJ        PIC 9(10).
         02 LS-TJ-TITULAR           PIC X(30).
         02 LS-TJ-DOCUMENTO         PIC 9(11).

       PROCEDURE DIVISION USING LS-TJ-KEY-INPUT,
                                LS-CreditCardValid,
                                LS-TarjetaRecord.
          DISPLAY 'In checkcc'.
          OPEN INPUT TarjetasFile.

          MOVE LS-TJ-KEY-INPUT TO TJ-NRO-TARJ.

         START TarjetasFile KEY IS EQUAL TO TJ-KEY
          *>INVALID KEY DISPLAY "Invalid CC Key :- ", TarjetaStatus
          *>NOT INVALID KEY DISPLAY "CC Pointer Updated "TarjetaStatus
         END-START.

          IF TarjetaStatus = "00"
            *>DISPLAY "HIGH-VALUE TO WS-CreditCardValid"
            MOVE HIGH-VALUE TO LS-CreditCardValid
            READ TarjetasFile NEXT RECORD
              AT END SET EOF-TARJETA TO TRUE
            END-READ
            MOVE TarjetaRecord TO LS-TarjetaRecord
          ELSE
            *>DISPLAY "LOW-VALUE TO WS-CreditCardValid"
            MOVE LOW-VALUE TO LS-CreditCardValid
          END-IF.
          CLOSE TarjetasFile.
       EXIT PROGRAM.
