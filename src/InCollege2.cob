>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OutFile         ASSIGN TO "data/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-OUT.

           SELECT AcctFile        ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-ACCT.

           SELECT InFile          ASSIGN TO "data/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-IN.

           *> Profiles: fixed-length sequential records (824 bytes)
           SELECT ProfileFile     ASSIGN TO "data/InCollege-Profiles.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS FS-PROFILE.

           SELECT TempProfileFile ASSIGN TO "data/InCollege-Profiles.tmp"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS FS-TMP.

       DATA DIVISION.
       FILE SECTION.

       FD  OutFile
           RECORD CONTAINS 240 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  OUT-REC                         PIC X(240).

       FD  AcctFile.
       01  ACCT-REC.
           05 AR-USER                      PIC X(20).
           05 AR-PASS                      PIC X(20).

       *> Fixed-length profile record: 824 characters total
       FD  ProfileFile
           RECORD CONTAINS 824 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  PROFILE-REC.
           05 PR-USER                      PIC X(20).
           05 PR-FNAME                     PIC X(20).
           05 PR-LNAME                     PIC X(20).
           05 PR-SCHOOL                    PIC X(30).
           05 PR-MAJOR                     PIC X(30).
           05 PR-GRADYR                    PIC X(4).
           05 PR-ABOUT                     PIC X(100).
           05 PR-EXPERIENCE-TABLE OCCURS 3 TIMES.
              10 PR-EXP-TITLE              PIC X(30).
              10 PR-EXP-COMPANY            PIC X(30).
              10 PR-EXP-DATES              PIC X(20).
              10 PR-EXP-DESC               PIC X(50).
           05 PR-EDUCATION-TABLE OCCURS 3 TIMES.
              10 PR-EDU-DEGREE             PIC X(30).
              10 PR-EDU-SCHOOL             PIC X(30).
              10 PR-EDU-YEARS              PIC X(10).

       FD  TempProfileFile
           RECORD CONTAINS 824 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  TEMP-REC.
           05 TP-USER                      PIC X(20).
           05 TP-FNAME                     PIC X(20).
           05 TP-LNAME                     PIC X(20).
           05 TP-SCHOOL                    PIC X(30).
           05 TP-MAJOR                     PIC X(30).
           05 TP-GRADYR                    PIC X(4).
           05 TP-ABOUT                     PIC X(100).
           05 TP-EXPERIENCE-TABLE OCCURS 3 TIMES.
              10 TP-EXP-TITLE              PIC X(30).
              10 TP-EXP-COMPANY            PIC X(30).
              10 TP-EXP-DATES              PIC X(20).
              10 TP-EXP-DESC               PIC X(50).
           05 TP-EDUCATION-TABLE OCCURS 3 TIMES.
              10 TP-EDU-DEGREE             PIC X(30).
              10 TP-EDU-SCHOOL             PIC X(30).
              10 TP-EDU-YEARS              PIC X(10).

       FD  InFile.
       01  IN-REC                          PIC X(240).

       WORKING-STORAGE SECTION.
       77  FS-OUT                          PIC XX     VALUE SPACES.
       77  FS-ACCT                         PIC XX     VALUE SPACES.
       77  FS-PROFILE                      PIC XX     VALUE SPACES.
       77  FS-TMP                          PIC XX     VALUE SPACES.
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

       01  GRAD-YR-STR                     PIC X(4)   VALUE SPACES.
       77  YEAR-VALID                      PIC 9      VALUE 0.
       77  YEAR-LEN                        PIC 99     VALUE 0.
       77  YEAR-NUM                        PIC 9(4)   VALUE 0.
       01  YEAR-RAW                        PIC X(16)  VALUE SPACES.

       77  I                               PIC 99     VALUE 0.
       77  EXPERIENCE-COUNT                PIC 9      VALUE 0.
       77  EDUCATION-COUNT                 PIC 9      VALUE 0.

       01  PROMPT-TEXT                     PIC X(240) VALUE SPACES.

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

       01  CURRENT-USER                    PIC X(20)  VALUE SPACES.

       01  I-DISPLAY                       PIC 99     VALUE 0.
       01  E-DISPLAY                       PIC 99     VALUE 0.
       77  PROFILE-FOUND                   PIC 9      VALUE 0.
       77  REPLACED-FLAG                   PIC 9      VALUE 0.

       01  FULL-NAME                       PIC X(120) VALUE SPACES.

       *> Stable NEW buffer so READs never clobber inputs
       01  NEW-PROFILE.
           05 NP-USER                      PIC X(20).
           05 NP-FNAME                     PIC X(20).
           05 NP-LNAME                     PIC X(20).
           05 NP-SCHOOL                    PIC X(30).
           05 NP-MAJOR                     PIC X(30).
           05 NP-GRADYR                    PIC X(4).
           05 NP-ABOUT                     PIC X(100).
           05 NP-EXPERIENCE-TABLE OCCURS 3 TIMES.
              10 NP-EXP-TITLE              PIC X(30).
              10 NP-EXP-COMPANY            PIC X(30).
              10 NP-EXP-DATES              PIC X(20).
              10 NP-EXP-DESC               PIC X(50).
           05 NP-EDUCATION-TABLE OCCURS 3 TIMES.
              10 NP-EDU-DEGREE             PIC X(30).
              10 NP-EDU-SCHOOL             PIC X(30).
              10 NP-EDU-YEARS              PIC X(10).

       PROCEDURE DIVISION.
       MAIN.
           PERFORM BOOT
           PERFORM LOAD-ACCOUNTS

           MOVE "Welcome to InCollege!" TO LINE-MSG
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

       *> ---------------- Startup / Shutdown ----------------
       BOOT.
           OPEN OUTPUT OutFile

           OPEN INPUT  AcctFile
           IF FS-ACCT = "35"
              OPEN OUTPUT AcctFile
              CLOSE AcctFile
              MOVE SPACES TO FS-ACCT
              OPEN INPUT AcctFile
           END-IF

           OPEN INPUT  ProfileFile
           IF FS-PROFILE = "35"
              OPEN OUTPUT ProfileFile
              CLOSE ProfileFile
              MOVE SPACES TO FS-PROFILE
              OPEN INPUT ProfileFile
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
           CLOSE ProfileFile
           CLOSE TempProfileFile
           CLOSE InFile
           CLOSE OutFile
           .

       *> ---------------- Utilities ----------------
       SAY.
           DISPLAY LINE-MSG
           MOVE LINE-MSG TO OUT-REC
           WRITE OUT-REC
           .

       SAY-LABEL-VALUE.
           *> expects PROMPT-TEXT = label, LAST-LINE = value
           MOVE SPACES TO LINE-MSG
           STRING
              FUNCTION TRIM(PROMPT-TEXT) DELIMITED BY SIZE
              " "                        DELIMITED BY SIZE
              FUNCTION TRIM(LAST-LINE)   DELIMITED BY SIZE
              INTO LINE-MSG
           END-STRING
           PERFORM SAY
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
           MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO LINE-MSG
           PERFORM SAY
           PERFORM SHUTDOWN
           STOP RUN
           .

       UCASE-TRIM-USER.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(U-NORM)) TO U-NORM
           .

       *> ---------------- Accounts ----------------
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

       *> ---------------- Menus ----------------
       SHOW-MAIN.
           MOVE "1. Log In"             TO LINE-MSG PERFORM SAY
           MOVE "2. Create New Account" TO LINE-MSG PERFORM SAY
           MOVE "Enter your choice:"    TO LINE-MSG PERFORM SAY
           .

       READ-MAIN.
           PERFORM READ-NEXT
           MOVE LAST-LINE TO RAW-SEL
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(RAW-SEL)) TO MAIN-SEL
           .

       DASHBOARD.
           PERFORM UNTIL 1 = 2
              MOVE "1. Create/Edit My Profile" TO LINE-MSG PERFORM SAY
              MOVE "2. View My Profile"        TO LINE-MSG PERFORM SAY
              MOVE "3. Search for User"        TO LINE-MSG PERFORM SAY
              MOVE "4. Learn a New Skill"      TO LINE-MSG PERFORM SAY
              MOVE "Enter your choice:"        TO LINE-MSG PERFORM SAY

              PERFORM READ-NEXT

              *> Ignore empty inputs instead of complaining
              IF FUNCTION LENGTH(FUNCTION TRIM(LAST-LINE)) = 0
                 CONTINUE
              ELSE
                 MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO NAV-SEL

                 EVALUATE TRUE
                    WHEN NAV-SEL = 1  PERFORM CREATE-EDIT-FLOW
                    WHEN NAV-SEL = 2  PERFORM VIEW-PROFILE
                    WHEN NAV-SEL = 3  MOVE "(Search coming soon)" TO LINE-MSG PERFORM SAY
                    WHEN NAV-SEL = 4  PERFORM SKILL-MENU
                    WHEN OTHER        MOVE "Please pick 1, 2, 3, or 4." TO LINE-MSG PERFORM SAY
                 END-EVALUATE
              END-IF
           END-PERFORM
           .

       SAY-HELLO.
           MOVE SPACES TO LINE-MSG
           STRING
              "Welcome, " DELIMITED BY SIZE
              FUNCTION TRIM(CURRENT-USER) DELIMITED BY SIZE
              "!" DELIMITED BY SIZE
              INTO LINE-MSG
           END-STRING
           PERFORM SAY
           .

       *> ---------------- Registration / Login ----------------
       REGISTER-FLOW.
           IF ACCT-COUNT >= 5
              MOVE "Account limit reached (5). Please try later." TO LINE-MSG
              PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           MOVE "Please enter your username:" TO LINE-MSG
           PERFORM SAY
           PERFORM READ-NEXT
           MOVE LAST-LINE TO U-IN

           IF FUNCTION LENGTH(FUNCTION TRIM(U-IN)) = 0
              MOVE "Username cannot be empty." TO LINE-MSG
              PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           MOVE "Please enter your password:" TO LINE-MSG
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

       LOGIN-FLOW.
           MOVE "Please enter your username:" TO LINE-MSG PERFORM SAY
           PERFORM READ-NEXT
           MOVE LAST-LINE TO U-IN

           MOVE "Please enter your password:" TO LINE-MSG PERFORM SAY
           PERFORM READ-NEXT
           MOVE LAST-LINE TO P-IN

           PERFORM VERIFY-CREDS
           IF LOGIN-OK = 1
              SET LOGGED-IN TO TRUE
              MOVE "You have successfully logged in." TO LINE-MSG PERFORM SAY
              MOVE FUNCTION TRIM(U-IN) TO CURRENT-USER
              PERFORM SAY-HELLO
           ELSE
              MOVE "Incorrect credentials. Try again." TO LINE-MSG PERFORM SAY
           END-IF
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

       *> ---------------- Create/Edit (UPSERT with NEW buffer) ----------------
       CREATE-EDIT-FLOW.
           MOVE "--- Create/Edit Profile ---" TO LINE-MSG PERFORM SAY

           *> First Name (required)
           MOVE SPACES TO PR-FNAME
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(PR-FNAME)) > 0
              MOVE "Enter First Name:" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-FNAME
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-FNAME)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> Last Name (required)
           MOVE SPACES TO PR-LNAME
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(PR-LNAME)) > 0
              MOVE "Enter Last Name:" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-LNAME
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-LNAME)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> University/College (required)
           MOVE SPACES TO PR-SCHOOL
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(PR-SCHOOL)) > 0
              MOVE "Enter University/College Attended:" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-SCHOOL
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-SCHOOL)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> Major (required)
           MOVE SPACES TO PR-MAJOR
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(PR-MAJOR)) > 0
              MOVE "Enter Major:" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-MAJOR
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-MAJOR)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> Graduation Year (validated against RAW, not X(4))
           MOVE "Enter Graduation Year (YYYY):" TO PROMPT-TEXT
           PERFORM PROMPT-AND-READ
           PERFORM CHECK-YEAR
           MOVE GRAD-YR-STR TO PR-GRADYR

           *> About (optional)
           MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO PROMPT-TEXT
           PERFORM PROMPT-AND-READ
           IF FUNCTION LENGTH(FUNCTION TRIM(LAST-LINE)) > 0
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-ABOUT
           ELSE
              MOVE SPACES TO PR-ABOUT
           END-IF

           *> Experience entries (up to 3)
           MOVE SPACES TO PR-EXP-TITLE(1) PR-EXP-TITLE(2) PR-EXP-TITLE(3)
           MOVE 0 TO EXPERIENCE-COUNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
              MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO LINE-MSG
              PERFORM SAY
              PERFORM READ-NEXT

              IF FUNCTION LENGTH(FUNCTION TRIM(LAST-LINE)) = 0
                 EXIT PERFORM
              END-IF
              IF FUNCTION UPPER-CASE(FUNCTION TRIM(LAST-LINE)) = "DONE"
                 EXIT PERFORM
              END-IF

              *> FIRST FIELD LABEL: Title
              MOVE I TO I-DISPLAY
              MOVE SPACES TO LINE-MSG
              STRING "Experience #" I-DISPLAY " - Title:" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              *> Use the previously entered line as the Title if user typed it already
              *> Otherwise, read now
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-EXP-TITLE(I))) = 0
                 MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-TITLE(I)
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-EXP-TITLE(I))) = 0
                 PERFORM READ-NEXT
                 MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-TITLE(I)
              END-IF

              *> Company
              MOVE SPACES TO LINE-MSG
              STRING "Experience #" I-DISPLAY " - Company/Organization:" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-COMPANY(I)

              *> Dates
              MOVE SPACES TO LINE-MSG
              STRING "Experience #" I-DISPLAY " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-DATES(I)

              *> Description (optional)
              MOVE SPACES TO LINE-MSG
              STRING "Experience #" I-DISPLAY " - Description (optional, max 100 chars, blank to skip):"
                 DELIMITED BY SIZE INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-DESC(I)

              ADD 1 TO EXPERIENCE-COUNT
           END-PERFORM

           *> Education entries (up to 3)
           MOVE SPACES TO PR-EDU-DEGREE(1) PR-EDU-DEGREE(2) PR-EDU-DEGREE(3)
           MOVE 0 TO EDUCATION-COUNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
              MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO LINE-MSG
              PERFORM SAY
              PERFORM READ-NEXT

              IF FUNCTION LENGTH(FUNCTION TRIM(LAST-LINE)) = 0
                 EXIT PERFORM
              END-IF
              IF FUNCTION UPPER-CASE(FUNCTION TRIM(LAST-LINE)) = "DONE"
                 EXIT PERFORM
              END-IF

              *> FIRST FIELD LABEL: Degree
              MOVE I TO E-DISPLAY
              MOVE SPACES TO LINE-MSG
              STRING "Education #" E-DISPLAY " - Degree:" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              *> Use the previously entered line as Degree if present
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-EDU-DEGREE(I))) = 0
                 MOVE FUNCTION TRIM(LAST-LINE) TO PR-EDU-DEGREE(I)
              END-IF
              IF FUNCTION LENGTH(FUNCTION TRIM(PR-EDU-DEGREE(I))) = 0
                 PERFORM READ-NEXT
                 MOVE FUNCTION TRIM(LAST-LINE) TO PR-EDU-DEGREE(I)
              END-IF

              *> University/College
              MOVE SPACES TO LINE-MSG
              STRING "Education #" E-DISPLAY " - University/College:" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EDU-SCHOOL(I)

              *> Years
              MOVE SPACES TO LINE-MSG
              STRING "Education #" E-DISPLAY " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EDU-YEARS(I)

              ADD 1 TO EDUCATION-COUNT
           END-PERFORM

           *> Tie profile to logged-in user (normalized)
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO PR-USER
           IF FUNCTION LENGTH(FUNCTION TRIM(PR-USER)) = 0
              MOVE "No active user - cannot save profile." TO LINE-MSG PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           *> Copy to NEW buffer (stable), then UPSERT
           PERFORM MOVE-PR-TO-NEW
           PERFORM UPSERT-PROFILE

           MOVE "Profile saved successfully!" TO LINE-MSG PERFORM SAY
           .

       *> ---------------- UPSERT: replace if exists, else append ----------------
       UPSERT-PROFILE.
           MOVE 0 TO REPLACED-FLAG

           CLOSE ProfileFile
           CLOSE TempProfileFile
           OPEN INPUT  ProfileFile
           OPEN OUTPUT TempProfileFile

           PERFORM UNTIL 1 = 2
              READ ProfileFile
                 AT END EXIT PERFORM
              END-READ

              IF FUNCTION UPPER-CASE(FUNCTION TRIM(PR-USER))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(NP-USER))
                 MOVE 1 TO REPLACED-FLAG
                 PERFORM MOVE-NEW-TO-TP
                 WRITE TEMP-REC
              ELSE
                 PERFORM MOVE-PR-TO-TP
                 WRITE TEMP-REC
              END-IF
           END-PERFORM

           IF REPLACED-FLAG = 0
              PERFORM MOVE-NEW-TO-TP
              WRITE TEMP-REC
           END-IF

           CLOSE ProfileFile
           CLOSE TempProfileFile

           *> Copy temp back to main file (truncate and rewrite)
           OPEN OUTPUT ProfileFile
           CLOSE ProfileFile
           OPEN OUTPUT ProfileFile
           OPEN INPUT  TempProfileFile

           PERFORM UNTIL 1 = 2
              READ TempProfileFile
                 AT END EXIT PERFORM
              END-READ
              PERFORM MOVE-TP-TO-PR
              WRITE PROFILE-REC
           END-PERFORM

           CLOSE TempProfileFile
           CLOSE ProfileFile
           OPEN INPUT ProfileFile
           .

       *> -------- move helpers --------
       MOVE-PR-TO-NEW.
           MOVE PR-USER   TO NP-USER
           MOVE PR-FNAME  TO NP-FNAME
           MOVE PR-LNAME  TO NP-LNAME
           MOVE PR-SCHOOL TO NP-SCHOOL
           MOVE PR-MAJOR  TO NP-MAJOR
           MOVE PR-GRADYR TO NP-GRADYR
           MOVE PR-ABOUT  TO NP-ABOUT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
              MOVE PR-EXP-TITLE(I)   TO NP-EXP-TITLE(I)
              MOVE PR-EXP-COMPANY(I) TO NP-EXP-COMPANY(I)
              MOVE PR-EXP-DATES(I)   TO NP-EXP-DATES(I)
              MOVE PR-EXP-DESC(I)    TO NP-EXP-DESC(I)
              MOVE PR-EDU-DEGREE(I)  TO NP-EDU-DEGREE(I)
              MOVE PR-EDU-SCHOOL(I)  TO NP-EDU-SCHOOL(I)
              MOVE PR-EDU-YEARS(I)   TO NP-EDU-YEARS(I)
           END-PERFORM
           .

       MOVE-NEW-TO-TP.
           MOVE NP-USER   TO TP-USER
           MOVE NP-FNAME  TO TP-FNAME
           MOVE NP-LNAME  TO TP-LNAME
           MOVE NP-SCHOOL TO TP-SCHOOL
           MOVE NP-MAJOR  TO TP-MAJOR
           MOVE NP-GRADYR TO TP-GRADYR
           MOVE NP-ABOUT  TO TP-ABOUT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
              MOVE NP-EXP-TITLE(I)   TO TP-EXP-TITLE(I)
              MOVE NP-EXP-COMPANY(I) TO TP-EXP-COMPANY(I)
              MOVE NP-EXP-DATES(I)   TO TP-EXP-DATES(I)
              MOVE NP-EXP-DESC(I)    TO TP-EXP-DESC(I)
              MOVE NP-EDU-DEGREE(I)  TO TP-EDU-DEGREE(I)
              MOVE NP-EDU-SCHOOL(I)  TO TP-EDU-SCHOOL(I)
              MOVE NP-EDU-YEARS(I)   TO TP-EDU-YEARS(I)
           END-PERFORM
           .

       MOVE-PR-TO-TP.
           MOVE PR-USER   TO TP-USER
           MOVE PR-FNAME  TO TP-FNAME
           MOVE PR-LNAME  TO TP-LNAME
           MOVE PR-SCHOOL TO TP-SCHOOL
           MOVE PR-MAJOR  TO TP-MAJOR
           MOVE PR-GRADYR TO TP-GRADYR
           MOVE PR-ABOUT  TO TP-ABOUT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
              MOVE PR-EXP-TITLE(I)   TO TP-EXP-TITLE(I)
              MOVE PR-EXP-COMPANY(I) TO TP-EXP-COMPANY(I)
              MOVE PR-EXP-DATES(I)   TO TP-EXP-DATES(I)
              MOVE PR-EXP-DESC(I)    TO TP-EXP-DESC(I)
              MOVE PR-EDU-DEGREE(I)  TO TP-EDU-DEGREE(I)
              MOVE PR-EDU-SCHOOL(I)  TO TP-EDU-SCHOOL(I)
              MOVE PR-EDU-YEARS(I)   TO TP-EDU-YEARS(I)
           END-PERFORM
           .

       MOVE-TP-TO-PR.
           MOVE TP-USER   TO PR-USER
           MOVE TP-FNAME  TO PR-FNAME
           MOVE TP-LNAME  TO PR-LNAME
           MOVE TP-SCHOOL TO PR-SCHOOL
           MOVE TP-MAJOR  TO PR-MAJOR
           MOVE TP-GRADYR TO PR-GRADYR
           MOVE TP-ABOUT  TO PR-ABOUT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
              MOVE TP-EXP-TITLE(I)   TO PR-EXP-TITLE(I)
              MOVE TP-EXP-COMPANY(I) TO PR-EXP-COMPANY(I)
              MOVE TP-EXP-DATES(I)   TO PR-EXP-DATES(I)
              MOVE TP-EXP-DESC(I)    TO PR-EXP-DESC(I)
              MOVE TP-EDU-DEGREE(I)  TO PR-EDU-DEGREE(I)
              MOVE TP-EDU-SCHOOL(I)  TO PR-EDU-SCHOOL(I)
              MOVE TP-EDU-YEARS(I)   TO PR-EDU-YEARS(I)
           END-PERFORM
           .

       *> ---------------- Validation helpers ----------------
       CHECK-YEAR.
           MOVE 0 TO YEAR-VALID
           PERFORM UNTIL YEAR-VALID = 1
              MOVE FUNCTION TRIM(LAST-LINE) TO YEAR-RAW
              MOVE FUNCTION LENGTH(FUNCTION TRIM(YEAR-RAW)) TO YEAR-LEN

              IF YEAR-LEN = 4
                 AND YEAR-RAW(1:1) >= "0" AND YEAR-RAW(1:1) <= "9"
                 AND YEAR-RAW(2:1) >= "0" AND YEAR-RAW(2:1) <= "9"
                 AND YEAR-RAW(3:1) >= "0" AND YEAR-RAW(3:1) <= "9"
                 AND YEAR-RAW(4:1) >= "0" AND YEAR-RAW(4:1) <= "9"
              THEN
                 MOVE FUNCTION NUMVAL(YEAR-RAW(1:4)) TO YEAR-NUM
                 IF YEAR-NUM >= 1900 AND YEAR-NUM <= 2100
                    MOVE YEAR-RAW(1:4) TO GRAD-YR-STR
                    MOVE 1 TO YEAR-VALID
                 END-IF
              END-IF

              IF YEAR-VALID = 0
                 MOVE "Required, must be a valid 4-digit year (1900-2100), e.g., 2025" TO LINE-MSG
                 PERFORM SAY
                 MOVE "Enter Graduation Year (YYYY):" TO PROMPT-TEXT
                 PERFORM PROMPT-AND-READ
              END-IF
           END-PERFORM
           .

       PROMPT-AND-READ.
           MOVE PROMPT-TEXT TO LINE-MSG
           PERFORM SAY
           PERFORM READ-NEXT
           .

       *> ---------------- View Profile (one line per label) ----------------
       VIEW-PROFILE.
           MOVE 0 TO PROFILE-FOUND
           MOVE "--- Your Profile ---" TO LINE-MSG PERFORM SAY

           OPEN INPUT ProfileFile
           PERFORM UNTIL 1 = 2
              READ ProfileFile
                 AT END EXIT PERFORM
              END-READ

              IF FUNCTION UPPER-CASE(FUNCTION TRIM(PR-USER))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
                 MOVE 1 TO PROFILE-FOUND

                 MOVE SPACES TO FULL-NAME
                 STRING "Name: " DELIMITED BY SIZE
                        FUNCTION TRIM(PR-FNAME) DELIMITED BY SIZE
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(PR-LNAME) DELIMITED BY SIZE
                        INTO FULL-NAME
                 END-STRING
                 MOVE FULL-NAME TO LINE-MSG PERFORM SAY

                 MOVE "University:" TO PROMPT-TEXT
                 MOVE FUNCTION TRIM(PR-SCHOOL) TO LAST-LINE
                 PERFORM SAY-LABEL-VALUE

                 MOVE "Major:" TO PROMPT-TEXT
                 MOVE FUNCTION TRIM(PR-MAJOR) TO LAST-LINE
                 PERFORM SAY-LABEL-VALUE

                 MOVE "Graduation Year:" TO PROMPT-TEXT
                 MOVE FUNCTION TRIM(PR-GRADYR) TO LAST-LINE
                 PERFORM SAY-LABEL-VALUE

                 IF PR-ABOUT NOT = SPACES
                    MOVE "About Me:" TO PROMPT-TEXT
                    MOVE FUNCTION TRIM(PR-ABOUT) TO LAST-LINE
                    PERFORM SAY-LABEL-VALUE
                 END-IF

                 MOVE "Experience:" TO LINE-MSG PERFORM SAY
                 PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                    IF PR-EXP-TITLE(I) NOT = SPACES
                       MOVE "  Title:" TO PROMPT-TEXT
                       MOVE FUNCTION TRIM(PR-EXP-TITLE(I)) TO LAST-LINE
                       PERFORM SAY-LABEL-VALUE

                       MOVE "  Company:" TO PROMPT-TEXT
                       MOVE FUNCTION TRIM(PR-EXP-COMPANY(I)) TO LAST-LINE
                       PERFORM SAY-LABEL-VALUE

                       MOVE "  Dates:" TO PROMPT-TEXT
                       MOVE FUNCTION TRIM(PR-EXP-DATES(I)) TO LAST-LINE
                       PERFORM SAY-LABEL-VALUE

                       IF PR-EXP-DESC(I) NOT = SPACES
                          MOVE "  Description:" TO PROMPT-TEXT
                          MOVE FUNCTION TRIM(PR-EXP-DESC(I)) TO LAST-LINE
                          PERFORM SAY-LABEL-VALUE
                       END-IF
                    END-IF
                 END-PERFORM

                 MOVE "Education:" TO LINE-MSG PERFORM SAY
                 PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                    IF PR-EDU-DEGREE(I) NOT = SPACES
                       MOVE "  Degree:" TO PROMPT-TEXT
                       MOVE FUNCTION TRIM(PR-EDU-DEGREE(I)) TO LAST-LINE
                       PERFORM SAY-LABEL-VALUE

                       MOVE "  University:" TO PROMPT-TEXT
                       MOVE FUNCTION TRIM(PR-EDU-SCHOOL(I)) TO LAST-LINE
                       PERFORM SAY-LABEL-VALUE

                       MOVE "  Years:" TO PROMPT-TEXT
                       MOVE FUNCTION TRIM(PR-EDU-YEARS(I)) TO LAST-LINE
                       PERFORM SAY-LABEL-VALUE
                    END-IF
                 END-PERFORM

                 EXIT PERFORM
              END-IF
           END-PERFORM
           CLOSE ProfileFile

           IF PROFILE-FOUND = 0
              MOVE "No profile found for this user yet." TO LINE-MSG PERFORM SAY
           END-IF
           .

       *> ---------------- Skills ----------------
       SKILL-MENU.
           PERFORM UNTIL 1 = 2
              MOVE "Learn a New Skill (coming soon)" TO LINE-MSG PERFORM SAY
              EXIT PARAGRAPH
           END-PERFORM
           .
