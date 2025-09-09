       IDENTIFICATION DIVISION.
       PROGRAM-ID. EPIC1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN "users.txt"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD USER-FILE.
       01 USER-RECORD.
           05 USER-NAME     PIC X(20).
           05 USER-PASSWORD PIC X(12).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS PIC XX.
           88 FS-SUCCESS        VALUE "00".
           88 FS-FILE-NOT-FOUND VALUE "35".
       01 WS-USERNAME    PIC X(20).
       01 WS-PASSWORD    PIC X(12).
       01 WS-PASS-LENGTH PIC 9.
       01 WS-CHOICE      PIC 9.
       01 END-FLAG       PIC X VALUE 'N'.
       01 FOUND-FLAG     PIC X VALUE 'N'.

       01 USER-COUNT PIC 9 VALUE 0.

       PROCEDURE DIVISION.
           PERFORM MAIN-PROCEDURE
           STOP RUN.
       MAIN-PROCEDURE.
           DISPLAY "-*-----------------------*-"
           DISPLAY "-*- InSchool Navigation -*-"
           DISPLAY "-*-----------------------*-"
           DISPLAY " "
           DISPLAY "[1] Log in"
           DISPLAY "[2] Create Account"
           DISPLAY " "
           DISPLAY "-*- Input your selection:"

           *> Get User Input
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               PERFORM LOGIN-PROCEDURE
           ELSE IF WS-CHOICE = 2
               PERFORM CREATE-ACCOUNT-PROCEDURE
           ELSE
               DISPLAY "Error: Invalid Choice Selected."
               DISPLAY "[!] Terminating Program."
               STOP RUN
           END-IF
           EXIT.
       LOGIN-PROCEDURE.
           *> Open the "user.txt" file
           OPEN INPUT USER-FILE

           *> Handle failing to open the file
           IF NOT FS-SUCCESS
               DISPLAY "Failed to open user file. Status: " WS-FILE-STATUS
               DISPLAY "You cannot login at this time."
               DISPLAY "Either no accounts have been created, or the user file is misplaced."
               CLOSE USER-FILE
               EXIT
           ELSE
               DISPLAY "[DEBUG] File Status: " WS-FILE-STATUS
           END-IF

           *> Accept user input
           DISPLAY " "
           DISPLAY "-*- Login into your account"
           DISPLAY " "
           DISPLAY "Username: "
           ACCEPT WS-USERNAME
           DISPLAY "Password: "
           ACCEPT WS-PASSWORD

           *> *** Attempt Login

           *> Reset Variables in-case they have been set in a previous loop
           MOVE 'N' TO END-FLAG
           MOVE 'N' TO FOUND-FLAG

           PERFORM UNTIL END-FLAG = 'Y' OR FOUND-FLAG = 'Y'
               *> Current line is set to USER-RECORD, which gives us access to the line's `USER-NAME` and `USER-PASSWORD`
               READ USER-FILE INTO USER-RECORD
                   AT END MOVE 'Y' TO END-FLAG
               END-READ

               DISPLAY "[DEBUG] Reading File: "
               DISPLAY " - Username:" USER-NAME
               DISPLAY " - Password:" USER-PASSWORD
               IF WS-USERNAME = USER-NAME
                   IF WS-PASSWORD = USER-PASSWORD
                       DISPLAY "Login Successful"
                       MOVE 'Y' TO FOUND-FLAG
                   ELSE
                       DISPLAY "Password Incorrect"
                   END-IF
               END-IF
           END-PERFORM

           IF FOUND-FLAG = 'N'
               DISPLAY "Login credentials are invalid."
           END-IF
           CLOSE USER-FILE
           EXIT.

       CREATE-ACCOUNT-PROCEDURE.
           OPEN I-O USER-FILE
           PERFORM UNTIL END-FLAG = 'Y'
               READ USER-FILE INTO USER-RECORD
                   AT END MOVE 'Y' TO END-FLAG
                   NOT AT END ADD 1 TO USER-COUNT
               END-READ
           END-PERFORM
           CLOSE USER-FILE
           IF USER-COUNT > 5
               DISPLAY "User limit reached."
               STOP RUN

           ELSE
               DISPLAY "Username: "
               ACCEPT WS-USERNAME
               DISPLAY "Password: "
               ACCEPT WS-PASSWORD


               OPEN EXTEND USER-FILE

               MOVE WS-USERNAME TO USER-NAME
               MOVE WS-PASSWORD TO USER-PASSWORD
               WRITE USER-RECORD
               CLOSE USER-FILE
           END-IF

           CLOSE USER-FILE
           EXIT.
