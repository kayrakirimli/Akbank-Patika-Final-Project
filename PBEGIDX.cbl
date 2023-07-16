       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PBEG006.
       AUTHOR.        Kayra KIRIMLI.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN TO IDXFILE
                             ORGANIZATION IS INDEXED 
                             ACCESS MODE IS RANDOM
                             RECORD KEY IS IDX-KEY
                             STATUS IDX-ST.
       DATA DIVISION. 
       FILE SECTION. 
       FD  IDX-FILE.
       01  IDX-REC.
           03  IDX-KEY.
               05 IDX-ID             PIC S9(05) COMP-3.
               05 IDX-DVZ            PIC S9(03) COMP.
           03  IDX-FULL-NAME.
               05 IDX-FNAME          PIC X(15).
               05 IDX-LNAME          PIC X(15).
           03  IDX-DATE              PIC S9(7)  COMP-3.
           03  IDX-BALANCE           PIC S9(15) COMP-3.
          
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           03 IDX-ST                  PIC 9(2).
              88 IDX-OK                        VALUE 00 97.
              88 IDX-NOT-FOUND                 VALUE 23.
           
           03 WS-INDEX-I              PIC 9(2).
           03 WS-INDEX-J              PIC 9(2).
           03 WS-UPDATED-FNAME.
              05 WS-UPDATED-NAME      PIC X(15).
              05 WS-UPDATED-SURNAME   PIC X(15).
             
       LINKAGE SECTION. 
       01  WS-SUB-AREA.
           05 WS-SUB-FUNC              PIC X(1).
           05 WS-SUB-ID                PIC 9(5).
           05 WS-SUB-DVZ               PIC 9(3).
           05 WS-SUB-RC                PIC 9(2).
           05 WS-SUB-DESC              PIC X(30).
           05 WS-SUB-DATA              PIC X(60).
        
       PROCEDURE DIVISION USING WS-SUB-AREA.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS.
           PERFORM H300-CLOSE-FILES.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN I-O IDX-FILE.
             IF (NOT IDX-OK)
                   DISPLAY "IDX-FILE OPEN ERROR"
                   DISPLAY IDX-ST
                   PERFORM H999-PROGRAM-EXIT.
       H100-END. EXIT.

       H200-PROCESS.
           EVALUATE TRUE 
             WHEN WS-SUB-FUNC = 'W'
                  PERFORM H210-WRITE-DATA
             WHEN WS-SUB-FUNC = 'R'
                  PERFORM H220-READ-DATA
             WHEN WS-SUB-FUNC = 'U'
                  PERFORM H230-UPDATE-DATA
             WHEN WS-SUB-FUNC = 'D'
                  PERFORM H240-DELETE-DATA
             WHEN OTHER
                  MOVE -1 TO WS-SUB-RC
                  MOVE 'INVALID FUNCTION' TO WS-SUB-DESC
                  PERFORM H999-PROGRAM-EXIT
             END-EVALUATE.
         H200-END. EXIT.

         H210-WRITE-DATA.
             MOVE WS-SUB-ID          TO IDX-ID.
             MOVE WS-SUB-DVZ         TO IDX-DVZ.
             MOVE 'KAYRA          '  TO IDX-FNAME.
             MOVE 'KIRIMLI        '  TO IDX-LNAME.
             MOVE 20000508           TO IDX-DATE.
             MOVE 1000               TO IDX-BALANCE.
               WRITE IDX-REC
                  INVALID KEY 
                     MOVE IDX-ST TO WS-SUB-RC
                     MOVE 'IDX-FILE WRITE ERROR' TO WS-SUB-DESC
                     PERFORM H999-PROGRAM-EXIT
               END-WRITE.
               MOVE IDX-FULL-NAME TO WS-SUB-DATA.
               MOVE 'WRITE SUCCESSFUL' TO WS-SUB-DESC.
         H210-END. EXIT.

       H220-READ-DATA.
             MOVE WS-SUB-ID          TO IDX-ID.
             MOVE WS-SUB-DVZ         TO IDX-DVZ.
             READ IDX-FILE
                INVALID KEY 
                      MOVE IDX-ST TO WS-SUB-RC
                      MOVE 'IDX-FILE READ ERROR' TO WS-SUB-DESC
                      PERFORM H999-PROGRAM-EXIT
             END-READ.
             MOVE IDX-FULL-NAME TO WS-SUB-DATA.
             MOVE 'READ SUCCESSFUL' TO WS-SUB-DESC.
         H220-END. EXIT.

         H230-UPDATE-DATA.
               PERFORM H235-UPDATE-FNAME.
               MOVE WS-UPDATED-NAME TO IDX-FNAME.
               MOVE WS-UPDATED-SURNAME TO IDX-LNAME.
               REWRITE IDX-REC
                  INVALID KEY 
                     MOVE IDX-ST TO WS-SUB-RC
                     MOVE 'IDX-FILE REWRITE ERROR' TO WS-SUB-DESC
                     PERFORM H999-PROGRAM-EXIT
               END-REWRITE.
               MOVE IDX-FULL-NAME TO WS-SUB-DATA.
               MOVE 'UPDATE SUCCESSFUL' TO WS-SUB-DESC.
         H230-END. EXIT.

         H235-UPDATE-FNAME.
               MOVE 1 TO WS-INDEX-I.
               MOVE 1 TO WS-INDEX-J.
               PERFORM UNTIL WS-INDEX-I > LENGTH OF IDX-FNAME
                  IF (IDX-FNAME(WS-INDEX-I:1) NOT = ' ')
                        MOVE IDX-FNAME(WS-INDEX-I:1) 
                                TO WS-UPDATED-NAME(WS-INDEX-J:1)
                        ADD 1 TO WS-INDEX-J
                  END-IF
                  ADD 1 TO WS-INDEX-I
               END-PERFORM.
               MOVE 1 TO WS-INDEX-I.
               MOVE 1 TO WS-INDEX-J.
               PERFORM UNTIL WS-INDEX-I > LENGTH OF IDX-LNAME
                  IF (IDX-LNAME(WS-INDEX-I:1) NOT = ' ')
                        MOVE IDX-LNAME(WS-INDEX-I:1) 
                                TO WS-UPDATED-SURNAME(WS-INDEX-J:1)
                        ADD 1 TO WS-INDEX-J
                  END-IF
                  ADD 1 TO WS-INDEX-I
               END-PERFORM.
         H235-END. EXIT.

         H240-DELETE-DATA.
               MOVE WS-SUB-ID          TO IDX-ID.
               MOVE WS-SUB-DVZ         TO IDX-DVZ.
               DELETE IDX-FILE
                  INVALID KEY 
                     MOVE IDX-ST TO WS-SUB-RC
                     MOVE 'IDX-FILE DELETE ERROR' TO WS-SUB-DESC
                     PERFORM H999-PROGRAM-EXIT
               END-DELETE.
               MOVE 'DELETE SUCCESSFUL' TO WS-SUB-DATA.
               MOVE 'OK' TO WS-SUB-DESC.
         H240-END. EXIT.

         H300-CLOSE-FILES.
               CLOSE IDX-FILE.
         H300-END. EXIT.

         H999-PROGRAM-EXIT.
               PERFORM H300-CLOSE-FILES.
               EXIT PROGRAM.
         H999-END. EXIT.

           