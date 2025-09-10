>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OutFile   ASSIGN TO "data/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT AcctFile  ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-ACCT.
           SELECT InFile    ASSIGN TO "data/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-IN.

       DATA DIVISION.
       FILE SECTION.
       FD  OutFile.
       01  OUT-REC                         PIC X(240).

       FD  AcctFile.
       01  ACCT-REC.
           05 AR-USER                      PIC X(20).
           05 AR-PASS                      PIC X(20).

       FD  InFile.
       01  IN-REC                          PIC X(240).

       WORKING-STORAGE SECTION.
       77  FS-ACCT                         PIC XX     VALUE SPACES.
       77  FS-IN                           PIC XX     VALUE SPACES.

       01  IN-EOF-FLAG                     PIC 9      VALUE 0.
           88  IN-AT-EOF                              VALUE 1.
           88  IN-NOT-EOF                             VALUE 0.

       01  LINE-MSG                        PIC X(240) VALUE SPACES.
       01  LAST-LINE                       PIC X(240) VALUE SPACES.

       01  RAW-SEL                         PIC X(12)  VALUE SPACES.
       77  MAIN-SEL                        PIC 99     VALUE 0.
       77  NAV-SEL                         PIC 99     VALUE 0.

       01  U-IN                            PIC X(20)  VALUE SPACES.
       01  P-IN                            PIC X(20)  VALUE SPACES.

       01  U-NORM                          PIC X(20)  VALUE SPACES.
       01  P-NORM                          PIC X(20)  VALUE SPACES.

       77  LOGIN-OK                        PIC 9      VALUE 0.
           88  LOGGED-IN                              VALUE 1.
           88  NOT-LOGGED                             VALUE 0.

       77  ACCT-COUNT                      PIC 9      VALUE 0.
       01  ACCT-TABLE.
           05 ACCT-SLOT OCCURS 5 TIMES.
              10 T-USER                    PIC X(20).
              10 T-PASS                    PIC X(20).

       77  PW-LEN                          PIC 99     VALUE 0.
       77  PW-HAS-UP                       PIC 9      VALUE 0.
       77  PW-HAS-DG                       PIC 9      VALUE 0.
       77  PW-HAS-SP                       PIC 9      VALUE 0.
       77  PW-VALID                        PIC 9      VALUE 0.

       77  I                               PIC 99     VALUE 0.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM BOOT
           PERFORM LOAD-ACCOUNTS

           MOVE "InCollege CLI : Welcome!" TO LINE-MSG
           PERFORM SAY

           PERFORM UNTIL LOGGED-IN
              PERFORM SHOW-MAIN
              PERFORM READ-MAIN
              EVALUATE TRUE
                 WHEN MAIN-SEL = 1
                    PERFORM LOGIN-FLOW
                 WHEN MAIN-SEL = 2
                    PERFORM REGISTER-FLOW
                 WHEN OTHER
                    MOVE "Invalid option. Choose 1 or 2." TO LINE-MSG
                    PERFORM SAY
              END-EVALUATE
           END-PERFORM

           PERFORM DASHBOARD
           PERFORM SHUTDOWN
           STOP RUN.

       *> ------------------------------ *
       *> Startup / Shutdown             *
       *> ------------------------------ *
       BOOT.
           OPEN OUTPUT OutFile
           OPEN INPUT  AcctFile
           IF FS-ACCT = "35"
              OPEN OUTPUT AcctFile
              CLOSE AcctFile
              MOVE SPACES TO FS-ACCT
              OPEN INPUT AcctFile
           END-IF

           OPEN INPUT  InFile
           IF FS-IN = "35"
              MOVE "ERROR: Missing input file: data/InCollege-Input.txt" TO LINE-MSG
              PERFORM SAY
              PERFORM HALT-PROGRAM
           END-IF
           .

       SHUTDOWN.
           CLOSE AcctFile
           CLOSE InFile
           CLOSE OutFile
           .

       *> ------------------------------ *
       *> Utility routines               *
       *> ------------------------------ *
       SAY.
           DISPLAY LINE-MSG
           MOVE LINE-MSG TO OUT-REC
           WRITE OUT-REC
           .

       READ-NEXT.
           IF IN-AT-EOF
              PERFORM HALT-PROGRAM
           END-IF
           READ InFile
              AT END
                 SET IN-AT-EOF TO TRUE
                 PERFORM HALT-PROGRAM
              NOT AT END
                 MOVE FUNCTION TRIM(IN-REC) TO LAST-LINE
           END-READ
           .

       HALT-PROGRAM.
           MOVE "--- END OF EXECUTION ---" TO LINE-MSG
           PERFORM SAY
           PERFORM SHUTDOWN
           STOP RUN
           .

       UCASE-TRIM-USER.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(U-NORM)) TO U-NORM
           .

       *> ------------------------------ *
       *> Account file handling          *
       *> ------------------------------ *
       LOAD-ACCOUNTS.
           MOVE 0 TO ACCT-COUNT
           PERFORM UNTIL 1 = 2
              READ AcctFile
                 AT END EXIT PERFORM
              END-READ
              IF AR-USER NOT = SPACES
                 ADD 1 TO ACCT-COUNT
                 IF ACCT-COUNT <= 5
                    MOVE AR-USER TO T-USER(ACCT-COUNT)
                    MOVE AR-PASS TO T-PASS(ACCT-COUNT)
                 END-IF
              END-IF
           END-PERFORM
           .

       APPEND-ACCOUNT.
           CLOSE AcctFile
           OPEN EXTEND AcctFile
           MOVE T-USER(ACCT-COUNT) TO AR-USER
           MOVE T-PASS(ACCT-COUNT) TO AR-PASS
           WRITE ACCT-REC
           CLOSE AcctFile
           OPEN INPUT AcctFile
           .

       *> ------------------------------ *
       *> Main menu                      *
       *> ------------------------------ *
       SHOW-MAIN.
           MOVE "1) Log in"             TO LINE-MSG PERFORM SAY
           MOVE "2) Create account"     TO LINE-MSG PERFORM SAY
           MOVE "Select an option:"     TO LINE-MSG PERFORM SAY
           .

       READ-MAIN.
           PERFORM READ-NEXT
           MOVE LAST-LINE TO RAW-SEL
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(RAW-SEL)) TO MAIN-SEL
           .

       *> ------------------------------ *
       *> Registration                   *
       *> ------------------------------ *
       REGISTER-FLOW.
           IF ACCT-COUNT >= 5
              MOVE "Account limit reached (5). Please try later." TO LINE-MSG
              PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           MOVE "Enter a username:" TO LINE-MSG
           PERFORM SAY
           PERFORM READ-NEXT
           MOVE LAST-LINE TO U-IN

           IF FUNCTION LENGTH(FUNCTION TRIM(U-IN)) = 0
              MOVE "Username cannot be empty." TO LINE-MSG
              PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           MOVE "Enter a password (8 to 12, include uppercase, digit, special):" TO LINE-MSG
           PERFORM SAY
           PERFORM READ-NEXT
           MOVE LAST-LINE TO P-IN

           PERFORM CHECK-PASSWORD
           IF PW-VALID = 0
              MOVE "Password does not meet complexity rules." TO LINE-MSG
              PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(U-IN) TO U-NORM
           PERFORM UCASE-TRIM-USER

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCT-COUNT
              IF FUNCTION UPPER-CASE(FUNCTION TRIM(T-USER(I))) = U-NORM
                 MOVE "That username is already taken. Please log in." TO LINE-MSG
                 PERFORM SAY
                 EXIT PARAGRAPH
              END-IF
           END-PERFORM

           ADD 1 TO ACCT-COUNT
           MOVE FUNCTION TRIM(U-IN) TO T-USER(ACCT-COUNT)
           MOVE FUNCTION TRIM(P-IN) TO T-PASS(ACCT-COUNT)
           PERFORM APPEND-ACCOUNT

           MOVE "Account created! You can log in now." TO LINE-MSG
           PERFORM SAY
           .

       CHECK-PASSWORD.
           MOVE 0 TO PW-HAS-UP PW-HAS-DG PW-HAS-SP PW-VALID
           MOVE FUNCTION LENGTH(FUNCTION TRIM(P-IN)) TO PW-LEN
           IF PW-LEN < 8 OR PW-LEN > 12
              EXIT PARAGRAPH
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PW-LEN
              EVALUATE TRUE
                 WHEN P-IN(I:1) >= "A" AND P-IN(I:1) <= "Z"
                    MOVE 1 TO PW-HAS-UP
                 WHEN P-IN(I:1) >= "0" AND P-IN(I:1) <= "9"
                    MOVE 1 TO PW-HAS-DG
                 WHEN P-IN(I:1) >= "a" AND P-IN(I:1) <= "z"
                    CONTINUE
                 WHEN OTHER
                    MOVE 1 TO PW-HAS-SP
              END-EVALUATE
           END-PERFORM

           IF PW-HAS-UP = 1 AND PW-HAS-DG = 1 AND PW-HAS-SP = 1
              MOVE 1 TO PW-VALID
           END-IF
           .

       *> ------------------------------ *
       *> Login                          *
       *> ------------------------------ *
       LOGIN-FLOW.
           PERFORM UNTIL LOGGED-IN
              MOVE "Username:" TO LINE-MSG
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE LAST-LINE TO U-IN

              MOVE "Password:" TO LINE-MSG
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE LAST-LINE TO P-IN

              PERFORM VERIFY-CREDS
              IF LOGIN-OK = 1
                 SET LOGGED-IN TO TRUE
                 MOVE "Login successful." TO LINE-MSG
                 PERFORM SAY
              ELSE
                 MOVE "Incorrect credentials. Try again." TO LINE-MSG
                 PERFORM SAY
              END-IF
           END-PERFORM
           .

       VERIFY-CREDS.
           MOVE 0 TO LOGIN-OK
           MOVE FUNCTION TRIM(U-IN) TO U-NORM
           MOVE FUNCTION TRIM(P-IN) TO P-NORM
           PERFORM UCASE-TRIM-USER

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCT-COUNT
              IF FUNCTION UPPER-CASE(FUNCTION TRIM(T-USER(I))) = U-NORM
                 AND FUNCTION TRIM(T-PASS(I)) = P-NORM
                 MOVE 1 TO LOGIN-OK
                 EXIT PERFORM
              END-IF
           END-PERFORM
           .

       *> ------------------------------ *
       *> Logged-in dashboard            *
       *> ------------------------------ *
       DASHBOARD.
           PERFORM UNTIL 1 = 2
              MOVE "1) Search jobs (coming soon)" TO LINE-MSG PERFORM SAY
              MOVE "2) Find people (coming soon)" TO LINE-MSG PERFORM SAY
              MOVE "3) Learn a new skill"         TO LINE-MSG PERFORM SAY
              MOVE "Choose an option:"            TO LINE-MSG PERFORM SAY

              PERFORM READ-NEXT
              MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO NAV-SEL

              EVALUATE TRUE
                 WHEN NAV-SEL = 1
                    MOVE "Jobs module is under development." TO LINE-MSG
                    PERFORM SAY
                 WHEN NAV-SEL = 2
                    MOVE "People finder is under development." TO LINE-MSG
                    PERFORM SAY
                 WHEN NAV-SEL = 3
                    PERFORM SKILL-MENU
                 WHEN OTHER
                    MOVE "Please pick 1, 2, or 3." TO LINE-MSG
                    PERFORM SAY
              END-EVALUATE
           END-PERFORM
           .

       SKILL-MENU.
           PERFORM UNTIL 1 = 2
              MOVE "Learn a New Skill" TO LINE-MSG PERFORM SAY
              MOVE "1) Alpha"          TO LINE-MSG PERFORM SAY
              MOVE "2) Beta"           TO LINE-MSG PERFORM SAY
              MOVE "3) Gamma"          TO LINE-MSG PERFORM SAY
              MOVE "4) Delta"          TO LINE-MSG PERFORM SAY
              MOVE "5) Epsilon"        TO LINE-MSG PERFORM SAY
              MOVE "6) Back"           TO LINE-MSG PERFORM SAY
              MOVE "Your selection:"   TO LINE-MSG PERFORM SAY

              PERFORM READ-NEXT
              MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO NAV-SEL

              EVALUATE TRUE
                 WHEN NAV-SEL >= 1 AND NAV-SEL <= 5
                    MOVE "That skill content is not available yet." TO LINE-MSG
                    PERFORM SAY
                 WHEN NAV-SEL = 6
                    EXIT PARAGRAPH
                 WHEN OTHER
                    MOVE "Valid options: 1 to 6." TO LINE-MSG
                    PERFORM SAY
              END-EVALUATE
           END-PERFORM
           .
