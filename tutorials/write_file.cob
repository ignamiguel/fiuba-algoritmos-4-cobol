       IDENTIFICATION DIVISION.
       PROGRAM-ID. Write_File.

       ENVIRONMENT DIVISION.
          INPUT-OUTPUT SECTION.
          FILE-CONTROL.
          SELECT Student_File ASSIGN TO "..\files\write_output.dat"
             ORGANIZATION IS SEQUENTIAL
             ACCESS IS SEQUENTIAL
             FILE STATUS IS FileStatus.

       DATA DIVISION.
          FILE SECTION.
          FD Student_File.
          01 Student_Record.
             05 Student_id PIC 9(5).
             05 Student_name PIC A(25).
             05 Student_class PIC X(3).

          WORKING-STORAGE SECTION.
          01 WS-STUDENT.
             05 WS-Student_id PIC 9(5).
             05 WS-Student_name PIC A(25).
             05 WS-Student_class PIC X(3).

         01 FileStatus          PIC X(2).

       PROCEDURE DIVISION.
          OPEN OUTPUT Student_File.

             MOVE 1000 TO Student_id.
             MOVE 'Tim' TO Student_name.
             MOVE '10' TO Student_class.

             DISPLAY Student_Record.

             WRITE Student_Record
             END-WRITE.

          CLOSE Student_File.
       STOP RUN.
       END PROGRAM Write_File.
