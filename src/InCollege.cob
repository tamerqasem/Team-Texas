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
           SELECT ReqFile         ASSIGN TO "data/InCollege-Requests.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-REQ.

           SELECT TempReqFile     ASSIGN TO "data/InCollege-Requests.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS FS-REQ.

           SELECT ConnectionsFile ASSIGN TO "data/InCollege-Connections.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS  IS FS-CONNEC.
           SELECT JobFile ASSIGN TO "data/InCollege-Jobs.dat"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS FS-JOB.

           SELECT ApplicationFile ASSIGN TO "data/InCollege-Applications.dat"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS  IS FS-APP.



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

       FD ConnectionsFile.
       01  CONNECTIONS-REC.
           05 CR-USER                      PIC X(20).
           05 CR-CONNEC-NAME               PIC X(20).

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

       *> NEW: Jobs file (line sequential)
       FD  JobFile.
       01  JOB-REC.
           05 JOB-ID         PIC 9(5).
           05 JOB-TITLE      PIC X(50).
           05 JOB-DESC       PIC X(200).
           05 JOB-EMPLOYER   PIC X(50).
           05 JOB-LOCATION   PIC X(50).
           05 JOB-SALARY     PIC X(30).
           05 JOB-POSTER     PIC X(20).

       FD  ApplicationFile.
       01  APP-REC.
           05 APP-USER       PIC X(20).
           05 APP-JOB-ID     PIC 9(5).
           05 APP-JOB-TITLE  PIC X(50).
           05 APP-EMPLOYER   PIC X(50).
           05 APP-LOCATION   PIC X(50).


       FD  InFile.
       01  IN-REC                          PIC X(240).
       FD  ReqFile.
       01  REQ-REC.
           05 REQ-SENDER                  PIC X(20).   *> UPPER-CASE username
           05 REQ-RECIP                   PIC X(20).   *> UPPER-CASE username
              FD  TempReqFile.

       01  TEMP-REQ-REC.
           05 TREQ-SENDER                 PIC X(20).
           05 TREQ-RECIP                  PIC X(20).


       WORKING-STORAGE SECTION.
       77  FS-OUT                          PIC XX     VALUE SPACES.
       77  FS-ACCT                         PIC XX     VALUE SPACES.
       77  FS-PROFILE                      PIC XX     VALUE SPACES.
       77  FS-TMP                          PIC XX     VALUE SPACES.
       77  FS-IN                           PIC XX     VALUE SPACES.

       77  FS-REQ                          PIC XX     VALUE SPACES.

       77  FS-CONNEC                       PIC XX     VALUE SPACES.
       77  FS-APP                           PIC XX     VALUE SPACES.

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
       01  C-NORM                          PIC X(20)  VALUE SPACES.

       01  GRAD-YR-STR                     PIC X(4)   VALUE SPACES.
       77  YEAR-VALID                      PIC 9      VALUE 0.
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
       01  SEARCH-NAME                     PIC X(120) VALUE SPACES.
       01  SEARCH-NAME-U                   PIC X(120) VALUE SPACES.
       01  FULLNAME-U                      PIC X(120) VALUE SPACES.

       77  YEAR-LEN                        PIC 99     VALUE 0.
       77  YEAR-NUM                        PIC 9(4)   VALUE 0.
       01  YEAR-RAW                        PIC X(16)  VALUE SPACES.


       77  REQ-FOUND                      PIC 9      VALUE 0.
       01  TARGET-USER                    PIC X(20)  VALUE SPACES.
       01  TARGET-NAME                    PIC X(120) VALUE SPACES.
       77  SUB-SEL                        PIC 99     VALUE 0.
       77  CONNEC-SEL                     PIC 99     VALUE 0.
       01  CONNEC-NAME                    PIC X(20)  VALUE SPACES.
       01  CONNEC-LIST-NAMES.
           05 CLN-NAME PIC X(20) OCCURS 99 TIMES VALUE SPACES.


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

           77  FS-JOB           PIC XX     VALUE SPACES.

           *> NEW: Job posting scratch/input fields
           77  JOB-ID-SEQ       PIC 9(5)   VALUE 0.
           01  JOB-TITLE-IN     PIC X(50)  VALUE SPACES.
           01  JOB-DESC-IN      PIC X(200) VALUE SPACES.
           01  JOB-EMP-IN       PIC X(50)  VALUE SPACES.
           01  JOB-LOC-IN       PIC X(50)  VALUE SPACES.
           01  JOB-SAL-IN       PIC X(30)  VALUE SPACES.

*> ----- [ BROWSE + DETAILS WORKING-STORAGE] -----
       77  JOB-COUNT          PIC 9(3)    VALUE 0.
       77  JOB-SEL            PIC 9(3)    VALUE 0.
       01  JOB-ID-CHOICE      PIC 9(5)    VALUE 0.
       01  JOB-ID-MAP.
           05 JOB-ID-SLOT     PIC 9(5) OCCURS 200 TIMES VALUE 0.


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
              MOVE "ERROR: Missing input file: data/InCollege-Input.txt" TO LINE-MSG PERFORM SAY
              MOVE "Check if you are in the correct directory, or create the file." TO LINE-MSG PERFORM SAY
              PERFORM HALT-PROGRAM
           END-IF

           OPEN INPUT  ReqFile
           IF FS-REQ = "35"
              OPEN OUTPUT ReqFile
              CLOSE ReqFile
              MOVE SPACES TO FS-REQ
              OPEN INPUT ReqFile
           END-IF

           OPEN INPUT JobFile
           IF FS-JOB = "35"
              OPEN OUTPUT JobFile
              CLOSE JobFile
              MOVE SPACES TO FS-JOB
              OPEN INPUT JobFile
           END-IF

           OPEN INPUT ApplicationFile
           IF FS-APP = "35"
              OPEN OUTPUT ApplicationFile
              CLOSE ApplicationFile
              MOVE SPACES TO FS-APP
              OPEN INPUT ApplicationFile
           END-IF

           OPEN INPUT  ConnectionsFile
           IF FS-CONNEC = "35"
              OPEN OUTPUT ConnectionsFile
              CLOSE ConnectionsFile
              MOVE SPACES TO FS-CONNEC
              OPEN INPUT ConnectionsFile
           END-IF
           .

       SHUTDOWN.
           CLOSE AcctFile
           CLOSE ProfileFile
           CLOSE TempProfileFile
           CLOSE InFile
           CLOSE OutFile
           CLOSE ReqFile
           CLOSE ConnectionsFile
           CLOSE JobFile
           CLOSE ApplicationFile
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
              MOVE "1. Create/Edit My Profile"            TO LINE-MSG PERFORM SAY
              MOVE "2. View My Profile"                   TO LINE-MSG PERFORM SAY
              MOVE "3. Find someone you know"             TO LINE-MSG PERFORM SAY
              MOVE "4. View My Pending Connection Requests" TO LINE-MSG PERFORM SAY
              MOVE "5. Learn a New Skill"                 TO LINE-MSG PERFORM SAY
              MOVE "6. View My Network"                  TO LINE-MSG PERFORM SAY
              MOVE "7. Search for a job"            TO LINE-MSG PERFORM SAY
              MOVE "Enter your choice:"                   TO LINE-MSG PERFORM SAY

              PERFORM READ-NEXT
              IF FUNCTION LENGTH(FUNCTION TRIM(LAST-LINE)) = 0
                 CONTINUE
              ELSE
                 MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO NAV-SEL
                 EVALUATE TRUE
                    WHEN NAV-SEL = 1  PERFORM CREATE-EDIT-FLOW
                    WHEN NAV-SEL = 2  PERFORM VIEW-PROFILE
                    WHEN NAV-SEL = 3  PERFORM FIND-SOMEONE
                    WHEN NAV-SEL = 4  PERFORM VIEW-PENDING-REQUESTS
                    WHEN NAV-SEL = 5  PERFORM SKILL-MENU
                    WHEN NAV-SEL = 6  PERFORM VIEW-NETWORK
                    WHEN NAV-SEL = 7  PERFORM JOB-MENU
                    WHEN OTHER        MOVE "Please pick 1, 2, 3, 4, 5, or 6." TO LINE-MSG PERFORM SAY
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
       SEND-REQUEST.
           *> Normalize current and target usernames
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO U-NORM
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(TARGET-USER))  TO P-NORM

           IF FUNCTION LENGTH(FUNCTION TRIM(U-NORM)) = 0
              MOVE "You must be logged in to send requests." TO LINE-MSG PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           IF U-NORM = P-NORM
              MOVE "You cannot send a request to yourself." TO LINE-MSG PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           *> Duplicate pending check (you -> them)
           MOVE 0 TO REQ-FOUND
           OPEN INPUT ReqFile
           PERFORM UNTIL 1 = 2
              READ ReqFile AT END EXIT PERFORM END-READ
              IF FUNCTION TRIM(REQ-SENDER) = U-NORM
                 AND FUNCTION TRIM(REQ-RECIP)  = P-NORM
                 MOVE 1 TO REQ-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM
           CLOSE ReqFile

           IF REQ-FOUND = 1
              MOVE "You have already sent this user a connection request." TO LINE-MSG PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           *> Inverse pending check (them -> you)
           MOVE 0 TO REQ-FOUND
           OPEN INPUT ReqFile
           PERFORM UNTIL 1 = 2
              READ ReqFile AT END EXIT PERFORM END-READ
              IF FUNCTION TRIM(REQ-SENDER) = P-NORM
                 AND FUNCTION TRIM(REQ-RECIP)  = U-NORM
                 MOVE 1 TO REQ-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM
           CLOSE ReqFile

           IF REQ-FOUND = 1
              MOVE "This user has already sent you a connection request." TO LINE-MSG PERFORM SAY
              EXIT PARAGRAPH
           END-IF

           *> Append new pending request
           CLOSE ReqFile
           OPEN EXTEND ReqFile
              MOVE U-NORM TO REQ-SENDER
              MOVE P-NORM TO REQ-RECIP
              WRITE REQ-REC
           CLOSE ReqFile
           OPEN INPUT ReqFile

           *> Confirmation
           IF FUNCTION LENGTH(FUNCTION TRIM(TARGET-NAME)) > 0
              MOVE SPACES TO LINE-MSG
              STRING "Connection request sent to " FUNCTION TRIM(TARGET-NAME) "."
                 INTO LINE-MSG
              END-STRING
           ELSE
              MOVE SPACES TO LINE-MSG
              STRING "Connection request sent to user " FUNCTION TRIM(TARGET-USER) "."
                 INTO LINE-MSG
              END-STRING
           END-IF
           PERFORM SAY
           .
       VIEW-PENDING-REQUESTS.
           MOVE "--- Pending Connection Requests ---" TO LINE-MSG PERFORM SAY
           MOVE "Select a user to Accept/Reject:" TO LINE-MSG PERFORM SAY
           MOVE 0 TO CONNEC-SEL
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO U-NORM

           INITIALIZE CONNEC-LIST-NAMES. *> Temporary Variable. This will store connection username.
           OPEN INPUT ReqFile
           PERFORM UNTIL 1 = 2
              *> Loop through Request File and find matching users.
              READ ReqFile AT END EXIT PERFORM END-READ
              IF FUNCTION TRIM(REQ-RECIP) = U-NORM
                 ADD 1 TO CONNEC-SEL *> essentially just `i++`


                 *> Try to resolve sender to full name
                 MOVE SPACES TO FULL-NAME
                 MOVE 0 TO PROFILE-FOUND

                 CLOSE ProfileFile
                 OPEN INPUT ProfileFile
                 PERFORM UNTIL 1 = 2
                    READ ProfileFile AT END EXIT PERFORM END-READ
                    IF FUNCTION UPPER-CASE(FUNCTION TRIM(PR-USER)) = FUNCTION TRIM(REQ-SENDER)
                       MOVE 1 TO PROFILE-FOUND
                       STRING FUNCTION TRIM(PR-FNAME) " " FUNCTION TRIM(PR-LNAME)
                          INTO FULL-NAME
                       END-STRING
                       EXIT PERFORM
                    END-IF
                 END-PERFORM
                 CLOSE ProfileFile

                 *> Add username to CONNEC-LIST-NAME array.
                 MOVE REQ-SENDER TO CLN-NAME(CONNEC-SEL)

                 *> Change the name we print depending on if the user has created a profile yet.
                 *> Format example `01. FirstName LastName`
                 IF PROFILE-FOUND = 1
                    MOVE SPACES TO LINE-MSG
                    STRING " " CONNEC-SEL ". " FUNCTION TRIM(FULL-NAME) INTO LINE-MSG
                    END-STRING
                 ELSE
                    MOVE SPACES TO LINE-MSG
                    STRING " " CONNEC-SEL ". " FUNCTION TRIM(REQ-SENDER) INTO LINE-MSG
                    END-STRING
                 END-IF


                 PERFORM SAY
              END-IF
           END-PERFORM
           CLOSE ReqFile

           IF CONNEC-SEL = 0
               MOVE " [ No requests found ]" TO LINE-MSG PERFORM SAY
           END-IF
           MOVE " 00. Return to Home Page" TO LINE-MSG PERFORM SAY

           *> User Takes selection
           PERFORM READ-NEXT
           IF FUNCTION LENGTH(FUNCTION TRIM(LAST-LINE)) = 0
               *> Invalid input
               CONTINUE
           ELSE
               *> Transfer user input into NAV-SEL
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO NAV-SEL


               IF NAV-SEL = 0
                   CONTINUE
               ELSE IF NAV-SEL <= CONNEC-SEL
                   *> Set selected username to CONNECT-NAME
                   MOVE CLN-NAME(NAV-SEL) TO CONNEC-NAME

                   MOVE SPACES TO LINE-MSG
                   STRING " You selected (" NAV-SEL "): " CONNEC-NAME INTO LINE-MSG PERFORM SAY

                   MOVE SPACES TO LINE-MSG
                   STRING "   01. Accept '" FUNCTION TRIM(CONNEC-NAME) "'" INTO LINE-MSG PERFORM SAY
                   STRING "   02. Reject '" FUNCTION TRIM(CONNEC-NAME) "'" INTO LINE-MSG PERFORM SAY
                   MOVE   "   00. Return to Home Page" TO LINE-MSG PERFORM SAY

                   PERFORM READ-NEXT
                   IF FUNCTION LENGTH(FUNCTION TRIM(LAST-LINE)) = 0
                       *> Invalid input
                       CONTINUE
                   ELSE
                       MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO NAV-SEL
                       EVALUATE TRUE
                           WHEN NAV-SEL = 1
                               PERFORM ACCEPT-CONNECTION-REQUEST
                           WHEN NAV-SEL = 2
                               PERFORM REJECT-CONNECTION-REQUEST
                           WHEN NAV-SEL = 0
                               CONTINUE
                           WHEN OTHER
                               MOVE "Invalid Input. Returning Home." TO LINE-MSG PERFORM SAY
                       END-EVALUATE
                   END-IF

               ELSE
                   MOVE "Invalid Input. Returning Home." TO LINE-MSG PERFORM SAY
               END-IF

           END-IF

           MOVE "-----------------------------------" TO LINE-MSG PERFORM SAY
           .
       *> Takes `CONNEC-NAME` as string input
       ACCEPT-CONNECTION-REQUEST.
      *> 1. Remove user from the pending request table.
      *> 2. Add them to the connections table (doubly).

      *> --- PART 1: REMOVE THE PENDING REQUEST ---
      *> This is the same logic as the REJECT paragraph. It copies all
      *> records except the accepted one to a temp file, then overwrites
      *> the original.

      *> Normalize the names once for efficient comparison.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO U-NORM.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CONNEC-NAME)) TO C-NORM.

      *> Open original file for reading and temp file for writing.
           OPEN INPUT ReqFile.
           OPEN OUTPUT TempReqFile.

           PERFORM UNTIL 1 = 2
               READ ReqFile
                   AT END EXIT PERFORM
               END-READ

      *> Check if the current record is the one being accepted.
               IF (FUNCTION TRIM(REQ-SENDER) = FUNCTION TRIM(C-NORM)) AND
                  (FUNCTION TRIM(REQ-RECIP)  = FUNCTION TRIM(U-NORM))
      *> This is the record to remove, so do nothing.
                   CONTINUE
               ELSE
      *> This is a record to keep, write it to the temp file.
                   WRITE TEMP-REQ-REC FROM REQ-REC
               END-IF
           END-PERFORM.

           CLOSE ReqFile.
           CLOSE TempReqFile.

      *> Now, overwrite the original ReqFile with the temp file.
           OPEN OUTPUT ReqFile.
           OPEN INPUT TempReqFile.

           PERFORM UNTIL 1 = 2
               READ TempReqFile
                   AT END EXIT PERFORM
               END-READ
               WRITE REQ-REC FROM TEMP-REQ-REC
           END-PERFORM.

           CLOSE ReqFile.
           CLOSE TempReqFile.

      *> --- PART 2: ADD THE NEW CONNECTION (DOUBLY) ---
      *> The file was opened at BOOT, so we must CLOSE it first.
           CLOSE ConnectionsFile.
           OPEN EXTEND ConnectionsFile.

      *> Check file status after OPEN. "00" means success.
           IF FS-CONNEC = "00"
      *> Write the first record: CURRENT-USER is connected to CONNEC-NAME
               MOVE U-NORM TO CR-USER
               MOVE C-NORM TO CR-CONNEC-NAME
               WRITE CONNECTIONS-REC

      *> Check status after first WRITE, if successful, write second record
               IF FS-CONNEC = "00"
                  MOVE C-NORM TO CR-USER
                  MOVE U-NORM TO CR-CONNEC-NAME
                  WRITE CONNECTIONS-REC

      *> Check status after second WRITE for final confirmation
                  IF FS-CONNEC = "00"
      *> --- PART 3: PROVIDE USER FEEDBACK (SUCCESS) ---
                     MOVE SPACES TO LINE-MSG
                     STRING "You are now connected with "
                        FUNCTION TRIM(CONNEC-NAME) "." INTO LINE-MSG
                     PERFORM SAY
                  ELSE
                     DISPLAY "ERROR: Failed to write second connection. Status: " FS-CONNEC
                  END-IF
               ELSE
                  DISPLAY "ERROR: Failed to write first connection. Status: " FS-CONNEC
               END-IF

               CLOSE ConnectionsFile
           ELSE
               DISPLAY "ERROR: Could not open ConnectionsFile. Status: " FS-CONNEC
           END-IF
       .


       REJECT-CONNECTION-REQUEST.
           *> Remove user from the pending requests table by rewriting the file
           *> without the rejected record.

           *> Normalize the names once for efficient comparison inside the loop.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO U-NORM.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CONNEC-NAME)) TO C-NORM.

           *> Open original file for reading and temp file for writing.
           OPEN INPUT ReqFile.
           OPEN OUTPUT TempReqFile.

           PERFORM UNTIL 1 = 2
               READ ReqFile
                   AT END EXIT PERFORM
               END-READ

               *> Check if the current record is the one to be rejected.
               IF (FUNCTION TRIM(REQ-SENDER) = FUNCTION TRIM(C-NORM)) AND
                  (FUNCTION TRIM(REQ-RECIP)  = FUNCTION TRIM(U-NORM))
               *> This is the record to reject, so do nothing.
                   CONTINUE
               ELSE
                   *> This is a record to keep, so write it to the temp file.
                   WRITE TEMP-REQ-REC FROM REQ-REC
               END-IF
           END-PERFORM.

           *> Close both files to save changes.
           CLOSE ReqFile.
           CLOSE TempReqFile.

           *> Now, overwrite the original ReqFile with the contents of the
           *> filtered TempReqFile.
           OPEN OUTPUT ReqFile.
           OPEN INPUT TempReqFile.

           PERFORM UNTIL 1 = 2
               READ TempReqFile
                   AT END EXIT PERFORM
               END-READ
               WRITE REQ-REC FROM TEMP-REQ-REC
           END-PERFORM.

           CLOSE ReqFile.
           CLOSE TempReqFile.

           *> Provide feedback to the user.
           MOVE SPACES TO LINE-MSG
           STRING "Request from '" FUNCTION TRIM(CONNEC-NAME) "' has been rejected." INTO LINE-MSG
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

           *> Experience entries (up to 3) — gate per entry
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

              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-TITLE(I)
              MOVE I TO I-DISPLAY

              MOVE SPACES TO LINE-MSG
              STRING "Experience #" I-DISPLAY " - Company/Organization:" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-COMPANY(I)

              MOVE SPACES TO LINE-MSG
              STRING "Experience #" I-DISPLAY " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-DATES(I)

              MOVE SPACES TO LINE-MSG
              STRING "Experience #" I-DISPLAY " - Description (optional, max 100 chars, blank to skip):"
                 DELIMITED BY SIZE INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EXP-DESC(I)

              ADD 1 TO EXPERIENCE-COUNT
           END-PERFORM

           *> Education entries (up to 3) — gate per entry
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

              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EDU-DEGREE(I)
              MOVE I TO E-DISPLAY

              MOVE SPACES TO LINE-MSG
              STRING "Education #" E-DISPLAY " - University/College:" DELIMITED BY SIZE
                 INTO LINE-MSG
              END-STRING
              PERFORM SAY
              PERFORM READ-NEXT
              MOVE FUNCTION TRIM(LAST-LINE) TO PR-EDU-SCHOOL(I)

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
              DISPLAY-PR.
           MOVE SPACES TO FULL-NAME
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(PR-FNAME) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(PR-LNAME) DELIMITED BY SIZE
                  INTO FULL-NAME
           END-STRING
           MOVE FULL-NAME TO LINE-MSG PERFORM SAY

           MOVE "University:"   TO PROMPT-TEXT
           MOVE FUNCTION TRIM(PR-SCHOOL) TO LAST-LINE
           PERFORM SAY-LABEL-VALUE

           MOVE "Major:"        TO PROMPT-TEXT
           MOVE FUNCTION TRIM(PR-MAJOR)  TO LAST-LINE
           PERFORM SAY-LABEL-VALUE

           MOVE "Graduation Year:" TO PROMPT-TEXT
           MOVE FUNCTION TRIM(PR-GRADYR) TO LAST-LINE
           PERFORM SAY-LABEL-VALUE

           IF PR-ABOUT NOT = SPACES
              MOVE "About Me:"  TO PROMPT-TEXT
              MOVE FUNCTION TRIM(PR-ABOUT) TO LAST-LINE
              PERFORM SAY-LABEL-VALUE
           END-IF

           MOVE "Experience:" TO LINE-MSG PERFORM SAY
           MOVE 0 TO I-DISPLAY
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
                 ADD 1 TO I-DISPLAY
              END-IF
           END-PERFORM
           IF I-DISPLAY = 0
              MOVE "  None" TO LINE-MSG PERFORM SAY
           END-IF

           MOVE "Education:" TO LINE-MSG PERFORM SAY
           MOVE 0 TO E-DISPLAY
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
                 ADD 1 TO E-DISPLAY
              END-IF
           END-PERFORM
           IF E-DISPLAY = 0
              MOVE "  None" TO LINE-MSG PERFORM SAY
           END-IF
           .

       *> ---------------- View Profile (one line per label) ----------------
              VIEW-PROFILE.
           MOVE 0 TO PROFILE-FOUND
           MOVE "--- Your Profile ---" TO LINE-MSG PERFORM SAY

           CLOSE ProfileFile
           OPEN INPUT ProfileFile

           PERFORM UNTIL 1 = 2
              READ ProfileFile AT END EXIT PERFORM END-READ
              IF FUNCTION UPPER-CASE(FUNCTION TRIM(PR-USER))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER))
                 MOVE 1 TO PROFILE-FOUND
                 PERFORM DISPLAY-PR
                 EXIT PERFORM
              END-IF
           END-PERFORM
           CLOSE ProfileFile

           IF PROFILE-FOUND = 0
              MOVE "No profile found for this user yet." TO LINE-MSG PERFORM SAY
           END-IF
           .

              FIND-SOMEONE.
           MOVE "Enter the full name of the person you are looking for:" TO LINE-MSG
           PERFORM SAY
           PERFORM READ-NEXT
           MOVE FUNCTION TRIM(LAST-LINE) TO SEARCH-NAME
           IF FUNCTION LENGTH(FUNCTION TRIM(SEARCH-NAME)) = 0
              EXIT PARAGRAPH
           END-IF
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(SEARCH-NAME)) TO SEARCH-NAME-U

           MOVE 0 TO PROFILE-FOUND
           CLOSE ProfileFile
           OPEN INPUT ProfileFile

           PERFORM UNTIL 1 = 2
              READ ProfileFile AT END EXIT PERFORM END-READ

              MOVE SPACES TO FULLNAME-U
              STRING FUNCTION UPPER-CASE(FUNCTION TRIM(PR-FNAME))
                     " "
                     FUNCTION UPPER-CASE(FUNCTION TRIM(PR-LNAME))
                     INTO FULLNAME-U
              END-STRING

              IF FULLNAME-U = SEARCH-NAME-U
                 MOVE "--- Found User Profile ---" TO LINE-MSG PERFORM SAY
                 PERFORM DISPLAY-PR
                 *> Offer to send a request to this PR-USER
                 MOVE SPACES TO TARGET-USER
                 MOVE PR-USER TO TARGET-USER

                 MOVE SPACES TO TARGET-NAME
                 STRING FUNCTION TRIM(PR-FNAME) " " FUNCTION TRIM(PR-LNAME)
                        INTO TARGET-NAME
                 END-STRING

                 MOVE "-------------------------" TO LINE-MSG PERFORM SAY
                 MOVE "1. Send Connection Request" TO LINE-MSG PERFORM SAY
                 MOVE "2. Back to Main Menu"       TO LINE-MSG PERFORM SAY
                 MOVE "Enter your choice:"         TO LINE-MSG PERFORM SAY
                 PERFORM READ-NEXT
                 MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO SUB-SEL

                 IF SUB-SEL = 1
                    PERFORM SEND-REQUEST
                 END-IF

                 MOVE 1 TO PROFILE-FOUND
                 EXIT PERFORM
              END-IF
           END-PERFORM

           CLOSE ProfileFile

           IF PROFILE-FOUND = 0
              MOVE "No one by that name could be found." TO LINE-MSG PERFORM SAY
           END-IF
           .

       *> ---------------- Skills ----------------
       SKILL-MENU.
           PERFORM UNTIL 1 = 2
              MOVE "Learn a New Skill (coming soon)" TO LINE-MSG PERFORM SAY
              EXIT PARAGRAPH
           END-PERFORM
           .
    *> ---------------- View network ----------------
    VIEW-NETWORK.
     MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO U-NORM

     MOVE 0 TO PROFILE-FOUND
     CLOSE ConnectionsFile
     OPEN INPUT ConnectionsFile

     MOVE "Your Connections:" TO LINE-MSG PERFORM SAY

     PERFORM UNTIL 1 = 2
         READ ConnectionsFile AT END EXIT PERFORM END-READ
         IF FUNCTION UPPER-CASE(FUNCTION TRIM(CR-USER)) = U-NORM
          IF CR-CONNEC-NAME NOT = SPACES
              *> Lookup full name in ProfileFile
              MOVE SPACES TO FULL-NAME
              CLOSE ProfileFile
              OPEN INPUT ProfileFile
              MOVE 0 TO PROFILE-FOUND
              PERFORM UNTIL 1 = 2
               READ ProfileFile AT END EXIT PERFORM END-READ
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(PR-USER)) = FUNCTION UPPER-CASE(FUNCTION TRIM(CR-CONNEC-NAME))
                MOVE 1 TO PROFILE-FOUND
                STRING FUNCTION TRIM(PR-FNAME) " " FUNCTION TRIM(PR-LNAME)
                    INTO FULL-NAME
                END-STRING
                EXIT PERFORM
               END-IF
              END-PERFORM
              CLOSE ProfileFile

              IF PROFILE-FOUND = 1
               MOVE FULL-NAME TO LINE-MSG
              ELSE
               MOVE FUNCTION TRIM(CR-CONNEC-NAME) TO LINE-MSG
              END-IF
              PERFORM SAY
          END-IF
         END-IF
     END-PERFORM

     CLOSE ConnectionsFile

     IF PROFILE-FOUND = 0
         MOVE "You have no connections at this time." TO LINE-MSG PERFORM SAY
     END-IF

     EXIT PARAGRAPH
     .

     *> ---------------- Jobs: Menu + Posting ----------------
     JOB-MENU.
        MOVE "--- Job Search/Internship Menu ---" TO LINE-MSG
        PERFORM SAY
        MOVE "1. Post a Job/Internship"           TO LINE-MSG
        PERFORM SAY
        MOVE "2. Browse Jobs/Internships"         TO LINE-MSG
        PERFORM SAY
        MOVE "3. View My Applications"            TO LINE-MSG
        PERFORM SAY
        MOVE "4. Back to Main Menu"               TO LINE-MSG
        PERFORM SAY
        MOVE "Enter your choice:"                  TO LINE-MSG
        PERFORM SAY.

        PERFORM READ-NEXT
        MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO SUB-SEL.

        EVALUATE SUB-SEL
           WHEN 1
             PERFORM POST-JOB-FLOW
           WHEN 2
             PERFORM BROWSE-JOB-FLOW
           WHEN 3
             PERFORM VIEW-MY-APPLICATIONS
           WHEN 4
             CONTINUE
           WHEN OTHER
            MOVE "Invalid option." TO LINE-MSG
            PERFORM SAY
        END-EVALUATE.

     POST-JOB-FLOW.
           MOVE "--- Post a New Job/Internship ---" TO LINE-MSG PERFORM SAY

           *> REQUIRED: Job Title
           MOVE SPACES TO JOB-TITLE-IN
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-TITLE-IN)) > 0
              MOVE "Enter Job Title:" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO JOB-TITLE-IN
              IF FUNCTION LENGTH(FUNCTION TRIM(JOB-TITLE-IN)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> REQUIRED: Description
           MOVE SPACES TO JOB-DESC-IN
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-DESC-IN)) > 0
              MOVE "Enter Description (max 200 chars):" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO JOB-DESC-IN
              IF FUNCTION LENGTH(FUNCTION TRIM(JOB-DESC-IN)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> REQUIRED: Employer
           MOVE SPACES TO JOB-EMP-IN
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-EMP-IN)) > 0
              MOVE "Enter Employer Name:" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO JOB-EMP-IN
              IF FUNCTION LENGTH(FUNCTION TRIM(JOB-EMP-IN)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> REQUIRED: Location
           MOVE SPACES TO JOB-LOC-IN
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-LOC-IN)) > 0
              MOVE "Enter Location:" TO PROMPT-TEXT
              PERFORM PROMPT-AND-READ
              MOVE FUNCTION TRIM(LAST-LINE) TO JOB-LOC-IN
              IF FUNCTION LENGTH(FUNCTION TRIM(JOB-LOC-IN)) = 0
                 MOVE "This field is required." TO LINE-MSG PERFORM SAY
              END-IF
           END-PERFORM

           *> OPTIONAL: Salary
           MOVE "Enter Salary (optional, enter 'NONE' to skip):" TO PROMPT-TEXT
           PERFORM PROMPT-AND-READ
           IF FUNCTION UPPER-CASE(FUNCTION TRIM(LAST-LINE)) = "NONE"
              MOVE SPACES TO JOB-SAL-IN
           ELSE
              MOVE FUNCTION TRIM(LAST-LINE) TO JOB-SAL-IN
           END-IF

           *> Determine next JOB-ID
           MOVE 0 TO JOB-ID-SEQ
           CLOSE JobFile
           OPEN INPUT JobFile
           PERFORM UNTIL 1 = 2
              READ JobFile AT END EXIT PERFORM END-READ
              IF JOB-ID > JOB-ID-SEQ
                 MOVE JOB-ID TO JOB-ID-SEQ
              END-IF
           END-PERFORM
           CLOSE JobFile
           ADD 1 TO JOB-ID-SEQ

           *> Append new record
           OPEN EXTEND JobFile
              MOVE JOB-ID-SEQ    TO JOB-ID
              MOVE JOB-TITLE-IN  TO JOB-TITLE
              MOVE JOB-DESC-IN   TO JOB-DESC
              MOVE JOB-EMP-IN    TO JOB-EMPLOYER
              MOVE JOB-LOC-IN    TO JOB-LOCATION
              MOVE JOB-SAL-IN    TO JOB-SALARY
              MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO JOB-POSTER
              WRITE JOB-REC
           CLOSE JobFile
           OPEN INPUT JobFile

           MOVE "Job posted successfully!" TO LINE-MSG PERFORM SAY
           MOVE "----------------------------------" TO LINE-MSG PERFORM SAY
           .

*> ----- [EPIC 7 ADD START – BROWSE + DETAILS IMPLEMENTATION] -----
    BROWSE-JOB-FLOW.
    MOVE "--- Available Job Listings ---" TO LINE-MSG
    PERFORM SAY

    CLOSE JobFile
    OPEN INPUT JobFile

    MOVE 0 TO JOB-COUNT
    INITIALIZE JOB-ID-MAP

    PERFORM UNTIL 1 = 2
        READ JobFile
            AT END
                EXIT PERFORM
        END-READ

        IF JOB-TITLE NOT = SPACES
            ADD 1 TO JOB-COUNT
            MOVE JOB-ID TO JOB-ID-SLOT(JOB-COUNT)
            MOVE SPACES TO LINE-MSG
            STRING
               FUNCTION TRIM(JOB-TITLE) " at " FUNCTION TRIM(JOB-EMPLOYER) 
               " (" FUNCTION TRIM(JOB-LOCATION) ")"
               INTO LINE-MSG
            END-STRING
            PERFORM SAY
        END-IF
    END-PERFORM

    CLOSE JobFile

    IF JOB-COUNT = 0
        MOVE "No jobs currently posted." TO LINE-MSG
        PERFORM SAY
        EXIT PARAGRAPH
    END-IF

    MOVE "-----------------------------" TO LINE-MSG
    PERFORM SAY
    MOVE "Enter job number to view details, or 0 to go back:" TO LINE-MSG
    PERFORM SAY
    PERFORM READ-NEXT
    MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO JOB-SEL

    IF JOB-SEL = 0
        EXIT PARAGRAPH
    END-IF

    IF JOB-SEL > 0 AND JOB-SEL <= JOB-COUNT
        MOVE JOB-ID-SLOT(JOB-SEL) TO JOB-ID-CHOICE
        PERFORM VIEW-JOB-DETAILS
    ELSE
        MOVE "Invalid job number." TO LINE-MSG
        PERFORM SAY
    END-IF

    EXIT PARAGRAPH
    .

    VIEW-JOB-DETAILS.
        MOVE "--- Job Details ---" TO LINE-MSG
        PERFORM SAY

        CLOSE JobFile
        OPEN INPUT JobFile

        PERFORM UNTIL 1 = 2
            READ JobFile
                AT END
                    EXIT PERFORM
            END-READ

            IF JOB-ID = JOB-ID-CHOICE
                MOVE "Title: " TO PROMPT-TEXT
                MOVE FUNCTION TRIM(JOB-TITLE) TO LAST-LINE
                PERFORM SAY-LABEL-VALUE

                MOVE "Description: " TO PROMPT-TEXT
                MOVE FUNCTION TRIM(JOB-DESC) TO LAST-LINE
                PERFORM SAY-LABEL-VALUE

                MOVE "Employer: " TO PROMPT-TEXT
                MOVE FUNCTION TRIM(JOB-EMPLOYER) TO LAST-LINE
                PERFORM SAY-LABEL-VALUE

                MOVE "Location: " TO PROMPT-TEXT
                MOVE FUNCTION TRIM(JOB-LOCATION) TO LAST-LINE
                PERFORM SAY-LABEL-VALUE

                IF JOB-SALARY NOT = SPACES
                    MOVE "Salary: " TO PROMPT-TEXT
                    MOVE FUNCTION TRIM(JOB-SALARY) TO LAST-LINE
                    PERFORM SAY-LABEL-VALUE
                END-IF

                MOVE "-------------------" TO LINE-MSG
                PERFORM SAY
                MOVE "1. Apply for this Job" TO LINE-MSG
                PERFORM SAY
                MOVE "2. Back to Job List" TO LINE-MSG
                PERFORM SAY
                MOVE "Enter your choice:" TO LINE-MSG
                PERFORM SAY

                PERFORM READ-NEXT
                MOVE FUNCTION NUMVAL(FUNCTION TRIM(LAST-LINE)) TO SUB-SEL

                EVALUATE SUB-SEL
                    WHEN 1
                        PERFORM APPLY-FOR-JOB
                    WHEN 2
                        PERFORM BROWSE-JOB-FLOW
                    WHEN OTHER
                        MOVE "Invalid option." TO LINE-MSG
                        PERFORM SAY
                END-EVALUATE

                EXIT PERFORM
            END-IF
        END-PERFORM

        CLOSE JobFile
        EXIT PARAGRAPH
        .

    APPLY-FOR-JOB.
        MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO APP-USER
        MOVE JOB-ID-CHOICE TO APP-JOB-ID
        MOVE FUNCTION TRIM(JOB-TITLE) TO APP-JOB-TITLE
        MOVE FUNCTION TRIM(JOB-EMPLOYER) TO APP-EMPLOYER
        MOVE FUNCTION TRIM(JOB-LOCATION) TO APP-LOCATION

        CLOSE ApplicationFile
        OPEN EXTEND ApplicationFile
        WRITE APP-REC
        CLOSE ApplicationFile
        OPEN INPUT ApplicationFile

        MOVE SPACES TO LINE-MSG
        STRING "Your application for " FUNCTION TRIM(APP-JOB-TITLE) 
               " at " FUNCTION TRIM(APP-EMPLOYER) " has been submitted."
               INTO LINE-MSG
        END-STRING
        PERFORM SAY

        EXIT PARAGRAPH
        .

    VIEW-MY-APPLICATIONS.
        MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(CURRENT-USER)) TO U-NORM
        MOVE "--- Your Job Applications ---" TO LINE-MSG
        PERFORM SAY

        MOVE SPACES TO LINE-MSG
        STRING "Application Summary for " FUNCTION TRIM(CURRENT-USER)
               INTO LINE-MSG
        END-STRING
        PERFORM SAY

        MOVE "------------------------------" TO LINE-MSG
        PERFORM SAY

        CLOSE ApplicationFile
        OPEN INPUT ApplicationFile

        MOVE 0 TO JOB-COUNT
        PERFORM UNTIL 1 = 2
            READ ApplicationFile
                AT END
                    EXIT PERFORM
            END-READ

            IF FUNCTION UPPER-CASE(FUNCTION TRIM(APP-USER)) = U-NORM
                ADD 1 TO JOB-COUNT
                MOVE "Job Title: " TO PROMPT-TEXT
                MOVE FUNCTION TRIM(APP-JOB-TITLE) TO LAST-LINE
                PERFORM SAY-LABEL-VALUE

                MOVE "Employer: " TO PROMPT-TEXT
                MOVE FUNCTION TRIM(APP-EMPLOYER) TO LAST-LINE
                PERFORM SAY-LABEL-VALUE

                MOVE "Location: " TO PROMPT-TEXT
                MOVE FUNCTION TRIM(APP-LOCATION) TO LAST-LINE
                PERFORM SAY-LABEL-VALUE

                MOVE "---" TO LINE-MSG
                PERFORM SAY
            END-IF
        END-PERFORM

        CLOSE ApplicationFile

        MOVE "------------------------------" TO LINE-MSG
        PERFORM SAY

        MOVE SPACES TO LINE-MSG
        STRING "Total Applications: " JOB-COUNT
               INTO LINE-MSG
        END-STRING
        PERFORM SAY

        MOVE "------------------------------" TO LINE-MSG
        PERFORM SAY

        EXIT PARAGRAPH
        .
