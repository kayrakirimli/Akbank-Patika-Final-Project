       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PBEG006.
       AUTHOR.        Kayra KIRIMLI.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT-FILE   ASSIGN TO OUTFILE
                             STATUS OUT-ST.
           SELECT INP-FILE   ASSIGN TO INPFILE
                             STATUS INP-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           03 OUT-FILLER-F       PIC X(6) VALUE "FUNC: ".
           03 OUT-FUNC-TYPE      PIC X(1).
           03 OUT-FILLER-ID      PIC X(4) VALUE "ID: ".
           03 OUT-ID             PIC 9(8).
           03 OUT-FILLER-RC      PIC X(4) VALUE "RC: ".
           03 OUT-RC             PIC 9(2).
           03 OUT-FILLER-DESC    PIC X(7) VALUE "DESC: ".    
           03 OUT-DESC           PIC X(30).
           03 OUT-FILLER-DATA    PIC X(7) VALUE "DATA: ".    
           03 OUT-DATA.
              05 OUT-FNAME-FROM  PIC X(15).
              05 OUT-FNAME-TO    PIC X(15).
              05 OUT-LNAME-FROM  PIC X(15).
              05 OUT-LNAME-TO    PIC X(15).


      *
       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           05 INP-FUNC-TYPE      PIC X(1).
           05 INP-ID             PIC X(5).
           05 INP-DVZ            PIC X(3).
           

      *
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 WS-PBEGIDX            PIC X(7) VALUE 'PBEGIDX'.
           05 OUT-ST             PIC 9(2).
              88 OUT-OK                   VALUE 00 97.
           05 INP-ST             PIC 9(2).
              88 INP-OK                   VALUE 00 97.
              88 INP-EOF                  VALUE 10.
           05 WS-FUNC-TYPE       PIC X(1).
              88 WS-VALID-FUNC            VALUE 'W' 'R' 'U' 'D'.
           05 WS-SUBAREA.
              07 WS-SUB-FUNC     PIC 9(1).
                 88 WS-FUNC-WRITE           VALUE 'W'.
                 88 WS-FUNC-READ            VALUE 'R'.
                 88 WS-FUNC-UPDATE          VALUE 'U'.
                 88 WS-FUNC-DELETE          VALUE 'D'.
              07 WS-SUB-ID      PIC 9(5).
              07 WS-SUB-DVZ     PIC 9(3). 
              07 WS-SUB-RC      PIC 9(2).
              07 WS-SUB-DATA    PIC X(50).
              07 WS-SUB-DSC     PIC X(30).

      *------------------
       PROCEDURE DIVISION.
      *------------------
        00000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS UNTIL INP-EOF.
           PERFORM H999-PROGRAM-EXIT.
         00000-END. EXIT.
         
        H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           IF (NOT OUT-OK)
               DISPLAY "OUT-FILE OPEN ERROR" OUT-ST
               MOVE OUT-ST TO RETURN-CODE 
               PERFORM H999-PROGRAM-EXIT
             END-IF.
           IF (NOT INP-OK)
              DISPLAY "INP-FILE OPEN ERROR" INP-ST
              MOVE INP-ST TO RETURN-CODE 
              PERFORM H999-PROGRAM-EXIT
           END-IF.
        H100-END. EXIT.

        H200-PROCESS.
           READ INP-FILE AT END SET INP-EOF TO TRUE
             IF (INP-EOF)
               PERFORM H999-PROGRAM-EXIT 
             END-IF.
            IF (NOT INP-OK)
                DISPLAY "INP-FILE READ ERROR" INP-ST
                MOVE INP-ST TO RETURN-CODE 
                PERFORM H999-PROGRAM-EXIT
                END-IF.
            MOVE INP-FUNC-TYPE TO WS-FUNC-TYPE.
            EVALUATE WS-FUNC-TYPE 
                  WHEN 'W'
                     SET WS-FUNC-WRITE TO TRUE
                  WHEN 'R'
                     SET WS-FUNC-READ TO TRUE
                  WHEN 'U'
                     SET WS-FUNC-UPDATE TO TRUE
                  WHEN 'D'
                     SET WS-FUNC-DELETE TO TRUE
                  WHEN OTHER
                     DISPLAY "INVALID FUNCTION TYPE" WS-FUNC-TYPE
                     MOVE 99 TO RETURN-CODE
                     PERFORM H999-PROGRAM-EXIT
            END-EVALUATE.
            MOVE SPACES TO WS-SUB-DATA.
            CALL WS-PBEGIDX USING WS-SUBAREA 
            PERFORM H300-WRITE-OUT-REC.
        H200-END. EXIT.

        H300-WRITE-OUT-REC.
           MOVE WS-FUNC-TYPE TO OUT-FUNC-TYPE.
           MOVE WS-SUB-ID TO OUT-ID.
           MOVE WS-SUB-RC TO OUT-RC.
           MOVE WS-SUB-DSC TO OUT-DESC.
           IF (WS-SUB-DATA NOT = SPACES)
              MOVE WS-SUB-DATA TO OUT-DATA
            END-IF.
           MOVE 'F TYPE: ' TO OUT-FILLER-F.
           MOVE ' ID: '    TO OUT-FILLER-ID.
           MOVE ' RC: '    TO OUT-FILLER-RC.
           MOVE ' DESC: '  TO OUT-FILLER-DESC.
           MOVE ' DATA: '  TO OUT-FILLER-DATA.
           WRITE OUT-REC.
           IF (NOT OUT-OK)
              DISPLAY "OUT-FILE WRITE ERROR" OUT-ST
              MOVE OUT-ST TO RETURN-CODE 
              PERFORM H999-PROGRAM-EXIT
           END-IF.
        H300-END. EXIT.

         H999-PROGRAM-EXIT.
             CLOSE INP-FILE.
             CLOSE OUT-FILE.
             STOP RUN.
         H999-END. EXIT.
      *

