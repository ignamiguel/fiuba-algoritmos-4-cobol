       IDENTIFICATION DIVISION.
       PROGRAM-ID. mytable.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
         05 WS-A OCCURS 3 TIMES INDEXED BY I.
         10 WS-B PIC A(2).
         10 WS-C OCCURS 2 TIMES INDEXED BY K.
            15 WS-D PIC X(3).

       01 WS-LETTERS.
       05 WS-LetterArray PIC X(1) OCCURS 18 TIMES INDEXED BY J.

       01 WS-SRCH PIC A(1) VALUE 'M'.

       01 WS-ORDERED-TABLE.
       05 WS-RECORD OCCURS 6 TIMES
          ASCENDING KEY IS WS-NUM INDEXED BY P.
         10 WS-NUM PIC 9(2).
         10 WS-NAME PIC A(3).

       PROCEDURE DIVISION.
           *>PERFORM Table_Index_Access.

           *>PERFORM Table_Iteration.

           *>PERFORM Table_Simple_Search.

           PERFORM Binary_Search.

           STOP RUN.

       Table_Index_Access.
          MOVE '12ABCDEF34GHIJKL56MNOPQR' TO WS-TABLE.
          SET I J TO 1.
          DISPLAY WS-C(I,J).
          SET I J UP BY 1.
          DISPLAY WS-C(I,J).

       Table_Iteration.
          MOVE '12ABCDEF34GHIJKL56MNOPQR' TO WS-TABLE.
          SET I, J TO 1.
          PERFORM Show_content VARYING I FROM 1 BY 1 UNTIL I > 3.

       Table_Simple_Search.
          *> Search
          MOVE 'ABCDEFGHIJKLMNOPQR' TO WS-LETTERS.
           SET J TO 1.
           SEARCH WS-LetterArray
               AT END DISPLAY 'M NOT FOUND IN TABLE'
            WHEN WS-LetterArray(J)=WS-SRCH
               DISPLAY 'LETTER M FOUND IN TABLE'
            END-SEARCH.

       Show_content.
        DISPLAY WS-A(I).
        *>DISPLAY " ".

       Binary_Search.
         MOVE '12ABC34DEF56GHI78JKL90MNO99PQR' TO WS-ORDERED-TABLE.

         PERFORM Show_content_2 VARYING P FROM 1 BY 1
         UNTIL P > 6.


         SEARCH ALL WS-RECORD
           AT END DISPLAY 'RECORD NOT FOUND'
           WHEN WS-NUM(P)=99
            DISPLAY 'RECORD FOUND '
            DISPLAY WS-NUM(I)
            DISPLAY WS-NAME(I)
         END-SEARCH.

       Show_content_2.
          DISPLAY WS-NUM(P).

       END PROGRAM mytable.
