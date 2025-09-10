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
       01 WS-PASSWORD-IN PIC X(13).
       01 WS-PASS-LENGTH PIC 99.
       01 WS-CHOICE      PIC 9.
       01 END-FLAG       PIC X VALUE 'N'.
       01 FOUND-FLAG     PIC X VALUE 'N'.

       01 PROGRAM-STATUS PIC 9 VALUE 0.
           88 PROGRAM-RUNNING VALUE "0".
           88 PROGRAM-EXIT-READY VALUE "1".

       01 LOGGED_IN_STATUS PIC 9 VALUE 0.
           88 SIGNED_OUT VALUE 0.
           88 SIGNED_IN VALUE 1.



       01 USER-COUNT PIC 9 VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY "-*------------------------*-"
           DISPLAY "-*- Welcome to InCollege -*-"
           DISPLAY "-*------------------------*-"
           PERFORM UNTIL PROGRAM-EXIT-READY
               IF SIGNED_IN
                   PERFORM NAVIGATION-LOGGED-IN-PROCEDURE
               ELSE
                   PERFORM NAVIGATION-LOGGED-OUT-PROCEDURE
               END-IF
           END-PERFORM
           STOP RUN.

       NAVIGATION-LOGGED-OUT-PROCEDURE.
           DISPLAY "Choose an operation:"
           DISPLAY "[1] Log in"
           DISPLAY "[2] Create Account"
           DISPLAY "[3] Exit"
           DISPLAY " "
           DISPLAY "Your selection: " WITH NO ADVANCING

           *> Get User Input
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               PERFORM LOGIN-PROCEDURE
           ELSE IF WS-CHOICE = 2
               PERFORM CREATE-ACCOUNT-PROCEDURE
           ELSE IF WS-CHOICE = 3
               MOVE 1 TO PROGRAM-STATUS
           ELSE
               DISPLAY "[Error]: Invalid Choice Selected."
           END-IF
           .

       NAVIGATION-LOGGED-IN-PROCEDURE.
           DISPLAY "-*------------------------*-"
           DISPLAY "-*- InCollege Navigation -*-"
           DISPLAY "-*------------------------*-"
           DISPLAY " "
           DISPLAY "[1] Search for a job"
           DISPLAY "[2] Find someone you know"
           DISPLAY "[3] Learn a new skill"
           DISPLAY " "
           DISPLAY "Your selection: "  WITH NO ADVANCING

           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               DISPLAY "[Search for a job] is under construction."
           ELSE IF WS-CHOICE = 2
               DISPLAY "[Find someone you know] is under construction."
           ELSE IF WS-CHOICE = 3
               PERFORM LEARN-A-SKILL-PROCEDURE
           ELSE
               DISPLAY "[Error]: Invalid Choice Selected."
               PERFORM NAVIGATION-LOGGED-IN-PROCEDURE
           END-IF
           .
       LEARN-A-SKILL-PROCEDURE.
           PERFORM UNTIL WS-CHOICE = '0'
               DiSPLAY "-- Learn a skill --"
               DISPLAY "[0] Return to previous level"
               DISPLAY "[1] Learn Example Skill 1"
               DISPLAY "[2] Learn Example Skill 2"
               DISPLAY "[3] Learn Example Skill 3"
               DISPLAY "[4] Learn Example Skill 4"
               DISPLAY "[5] Learn Example Skill 5"
               DISPLAY " "
               DISPLAY "Your selection: "  WITH NO ADVANCING

               ACCEPT WS-CHOICE

               IF WS-CHOICE = 0 then
                   *> End program execution like in the sample output (For now)
                   STOP RUN
               ELSE IF WS-CHOICE LESS THAN OR EQUAL TO 5
                   DISPLAY "[Example Skill " WS-CHOICE "] is under construction."
               ELSE
                   DISPLAY "Invalid Selection"
               END-IF
           END-PERFORM
           .

       LOGIN-PROCEDURE.
           *> Open the "user.txt" file
           OPEN INPUT USER-FILE

           *> Handle failing to open the file
           IF NOT FS-SUCCESS
               DISPLAY "Failed to open user file. Status: " WS-FILE-STATUS
               DISPLAY "You cannot login at this time."
               DISPLAY "Either no accounts have been created, or the user file is misplaced."
               CLOSE USER-FILE
               PERFORM NAVIGATION-LOGGED-OUT-PROCEDURE
               EXIT
           *> ELSE
           *>    DISPLAY "[DEBUG] File Status: " WS-FILE-STATUS
           END-IF

           *> Accept user input
           DISPLAY " "
           DISPLAY "Enter your account credentials:"
           DISPLAY "Username: "  WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY "Password: "  WITH NO ADVANCING
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

               *> DISPLAY " "
               *> DISPLAY "[DEBUG] Reading File: "
               *> DISPLAY " - Username: " USER-NAME
               *> DISPLAY " - Password: " USER-PASSWORD
               IF WS-USERNAME = USER-NAME
                   IF WS-PASSWORD = USER-PASSWORD
                       MOVE 'Y' TO FOUND-FLAG
                   END-IF
               END-IF
           END-PERFORM
           CLOSE USER-FILE

           IF FOUND-FLAG = 'N'
               DISPLAY "[Error]: Incorrect username/password, please try again"
           ELSE IF FOUND-FLAG = 'Y'
               MOVE 1 TO LOGGED_IN_STATUS
               DISPLAY "[!] You have successfully logged in"
               DISPLAY "Welcome, " FUNCTION TRIM(WS-USERNAME TRAILING) "!"
               PERFORM NAVIGATION-LOGGED-IN-PROCEDURE
           END-IF
           .

       CREATE-ACCOUNT-PROCEDURE.
           *> Check the amount of users added
           OPEN INPUT USER-FILE
           IF FS-FILE-NOT-FOUND
               OPEN OUTPUT USER-FILE
               MOVE WS-USERNAME TO USER-NAME
               MOVE WS-PASSWORD TO USER-PASSWORD
               WRITE USER-RECORD
               CLOSE USER-FILE
               OPEN INPUT USER-FILE
           END-IF

           MOVE 0 TO USER-COUNT
           MOVE 'N' TO END-FLAG
           PERFORM UNTIL END-FLAG = 'Y'
               READ USER-FILE INTO USER-RECORD
               AT END MOVE 'Y' TO END-FLAG
               NOT AT END ADD 1 TO USER-COUNT
               END-READ
           END-PERFORM

           CLOSE USER-FILE
           DISPLAY "USER COUNT: " USER-COUNT

           IF USER-COUNT > 5
               DISPLAY "[!]: All permitted accounts have been created, please come back later"
           ELSE
               DISPLAY "-*- Enter new account credentials"
               DISPLAY "Username: " WITH NO ADVANCING
               ACCEPT WS-USERNAME
               DISPLAY "Password: " WITH NO ADVANCING
               ACCEPT WS-PASSWORD-IN
               MOVE WS-PASSWORD-IN TO WS-PASSWORD

               COMPUTE WS-PASS-LENGTH = FUNCTION LENGTH(FUNCTION TRIM(WS-PASSWORD-IN TRAILING))

               IF WS-PASS-LENGTH IS GREATER THAN 12 OR WS-PASS-LENGTH IS LESS THAN 8
                   DISPLAY "[!] Password must have a length between 8 to 12 characters"
               ELSE
                   OPEN EXTEND USER-FILE

                   MOVE WS-USERNAME TO USER-NAME
                   MOVE WS-PASSWORD TO USER-PASSWORD
                   WRITE USER-RECORD

                   CLOSE USER-FILE
               END-IF
           END-IF
           .
