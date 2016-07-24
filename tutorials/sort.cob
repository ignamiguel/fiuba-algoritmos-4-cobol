       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       ENVIRONMENT DIVISION.
          INPUT-OUTPUT SECTION.
          FILE-CONTROL.
             SELECT Input_file ASSIGN TO '..\files\sort_input.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
             SELECT Output_file ASSIGN TO '..\files\sort_output.dat'
             ORGANIZATION IS LINE SEQUENTIAL.
             SELECT Work_file ASSIGN TO '..\files\sort_work.dat'
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
          FILE SECTION.
          FD Input_file.
             01 INPUT-STUDENT.
                05 STUDENT-ID-I PIC 9(5).
                05 STUDENT-NAME-I PIC A(25).
          FD Output_file.
             01 OUTPUT-STUDENT.
                05 STUDENT-ID-O PIC 9(5).
                05 STUDENT-NAME-O PIC A(25).
          SD Work_file.
             01 WORK-STUDENT.
                05 STUDENT-ID-W PIC 9(5).
                05 STUDENT-NAME-W PIC A(25).

       PROCEDURE DIVISION.
          SORT Work_file ON ASCENDING KEY STUDENT-NAME-O
          USING Input_file GIVING Output_file.
          DISPLAY 'Sort Successful'.
       STOP RUN.
