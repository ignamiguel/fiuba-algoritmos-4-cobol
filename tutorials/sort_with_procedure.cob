       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MaleSort.
      * AUTHOR.  Michael Coughlan.
      *  Uses the the SORT and an INPUT PROCEDURE to read
      *  the student masterfile (sorted on ascending Student Id)
      *  and from it to produce a file containing only the records of
      *  male students sorted on ascending student name.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT StudentFile ASSIGN TO
           "../files/students_duplicates.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MaleStudentFile ASSIGN TO
           "../files/_male_students.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FemaleStudentFile ASSIGN TO
           "../files/_female_students.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT WorkFile ASSIGN TO "../files/WORK.TMP"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD StudentFile.
       01 StudentRec      PIC X(30).
          88 EndOfFile    VALUE HIGH-VALUES.

       FD MaleStudentFile.
       01 MaleStudentRec  PIC X(30).

       FD FemaleStudentFile.
       01 FemaleStudentRecord   PIC X(30).

       SD WorkFile.
       01 WorkRec.
          *>02 FILLER             PIC 9(7).
          02 WStudentId         PIC 9(7).
          02 WStudentName       PIC X(10).
          02 FILLER             PIC X(12).
          02 WGender            PIC X.
             88 Is_Male_Student     VALUE "M".
             88 Is_Female_Student   VALUE "F".

       PROCEDURE DIVISION.
       Begin.
          *> Get only male students
          PERFORM Process_Male.

          *> Get only female students
          PERFORM Process_Female.

          STOP RUN.

       Process_Male.
          SORT WorkFile ON ASCENDING KEY WStudentName
                           ASCENDING KEY WStudentId
               INPUT PROCEDURE IS Get_Male_Students
               GIVING MaleStudentFile.

       Process_Female.
           SORT WorkFile ON ASCENDING KEY WStudentName
                            DESCENDING KEY WStudentId
               INPUT PROCEDURE IS Get_Female_Students
               GIVING FemaleStudentFile.

       Get_Male_Students.
          OPEN INPUT StudentFile

          READ StudentFile
             AT END SET EndOfFile TO TRUE
          END-READ

          PERFORM UNTIL EndOfFile
             MOVE StudentRec TO WorkRec

             IF Is_Male_Student
                RELEASE WorkRec
             END-IF

             READ StudentFile
               AT END SET EndOfFile TO TRUE
             END-READ

          END-PERFORM.

          DISPLAY "Done male!".

          CLOSE StudentFile.

       Get_Female_Students.
          OPEN INPUT StudentFile

          READ StudentFile
             AT END SET EndOfFile TO TRUE
          END-READ

          PERFORM UNTIL EndOfFile
             MOVE StudentRec TO WorkRec

             IF Is_Female_Student
                RELEASE WorkRec
             END-IF

             READ StudentFile
               AT END SET EndOfFile TO TRUE
             END-READ

          END-PERFORM.

          DISPLAY "Done female!".

          CLOSE StudentFile.
       END PROGRAM MaleSort.
