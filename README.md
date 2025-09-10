

# InCollege – Week 3 Deliverable (COBOL, Free Source Format)

A command-line simulation of the “InCollege” system written in **GnuCOBOL**.  
All **input** comes from a file, and all **output** is printed to screen **and** written to a log file.  
Implements **Create Account**, **Login**, and a **Dashboard** with a Skills submenu.

---

## ✅ What’s Implemented (Matches Rubric)

- **SOURCE FORMAT FREE** (compile with `-free`)
- **File-based I/O only**  
  - Input from `data/InCollege-Input.txt`  
  - Output printed to console **and** written to `data/InCollege-Output.txt`
- **Flows**
  - **Create account** (caps at 5 accounts)
  - **Login** (case-insensitive username match; exact password match)
  - **Dashboard** (Jobs/People placeholders + Skills submenu)
- **Password policy**
  - Length **8–12**
  - At least **1 uppercase**, **1 digit**, **1 special** (any non-alphanumeric)
- **Persistence**
  - Accounts stored in `data/accounts.dat` (created on first run if missing)
  - New accounts appended

---

## 📂 Project Structure

```

InCollege/
├─ InCollege.cob                  # Main COBOL source code
├─ README.md                      # This file
└─ data/                          # Data and test files
├─ InCollege-Input.txt         # Active input script (copy one test here before run)
├─ InCollege-Output.txt        # Program output log (auto-created each run)
├─ accounts.dat                # Persistent account storage (line sequential, auto-created)
│
├─ InCollege-Input-happy.txt               # Test 1 – happy path
├─ InCollege-Input-badpass.txt             # Test 2 – invalid password
├─ InCollege-Input-duplicate.txt           # Test 3 – duplicate username
├─ InCollege-Input-login-fail-then-success.txt  # Test 4 – login fail then success
├─ InCollege-Input-dashboard-nav.txt       # Test 5 – dashboard navigation
├─ InCollege-Input-acc-limit.txt           # Test 6 – account limit reached
└─ InCollege-Input-setup-5.txt             # Optional – seed 5 accounts

````

---

## 🗃️ Data File Formats

### `data/accounts.dat` (accounts database)
- **Organization:** LINE SEQUENTIAL
- **Record layout (fixed width = 40 chars / 1 line per account):**
  - Columns 1–20: `username` (PIC X(20))
  - Columns 21–40: `password` (PIC X(20))
- Written automatically when registering.  
- Recommendation: create users through the program, not by editing manually.

### `data/InCollege-Input.txt` (input script)
- One line = one user response.
- EOF ends execution.

### `data/InCollege-Output.txt` (output log)
- Contains every line displayed on the screen.

---

## 🛠️ Build & Run

Make sure you have **GnuCOBOL** installed (`cobc`).

```bash
# Compile
cobc -x -free InCollege.cob -o InCollege

# Run (using whichever input you copied as active)
./InCollege
````

---

## 🧪 Test Structure

Each test is an input script (`.txt`) placed in the `data/` folder.
Before running, copy the test you want into `InCollege-Input.txt`:

```bash
cp data/InCollege-Input-happy.txt data/InCollege-Input.txt
./InCollege
```

### Test Cases

1. **Happy Path** – Register → Login → Dashboard → Skills → Back
   File: `data/InCollege-Input-happy.txt`

   ```txt
   2
   newuser
   Newuser1!
   1
   newuser
   Newuser1!
   3
   6
   ```

2. **Invalid Password Rule** – Password too short or missing requirements
   File: `data/InCollege-Input-badpass.txt`

   ```txt
   2
   shortuser
   abc
   ```

3. **Duplicate Username** – Same username registered twice
   File: `data/InCollege-Input-duplicate.txt`

   ```txt
   2
   dupuser
   DupPass1!
   2
   dupuser
   Another1!
   ```

4. **Login Failure then Success** – Wrong password, then correct one
   File: `data/InCollege-Input-login-fail-then-success.txt`

   ```txt
   2
   trialuser
   TrialUser1!
   1
   trialuser
   wrongpass
   trialuser
   TrialUser1!
   ```

5. **Dashboard Navigation** – Explore jobs, people, and skills menus
   File: `data/InCollege-Input-dashboard-nav.txt`

   ```txt
   2
   dashuser
   DashUser1!
   1
   dashuser
   DashUser1!
   1
   2
   3
   6
   ```

6. **Account Limit Reached** – Attempt to create more than 5 accounts
   File: `data/InCollege-Input-acc-limit.txt`

   ```txt
   2
   user1
   UserOne1!
   2
   user2
   UserTwo2!
   2
   user3
   UserThree3!
   2
   user4
   UserFour4!
   2
   user5
   UserFive5!
   2
   user6
   UserSix6!
   ```

7. **Optional Setup – Seed 5 Accounts**
   File: `data/InCollege-Input-setup-5.txt`

   ```txt
   2
   auser1
   AuserOne1!
   2
   auser2
   AuserTwo2!
   2
   auser3
   AuserThree3!
   2
   auser4
   AuserFour4!
   2
   auser5
   AuserFive5!
   ```

---

## 🔎 Expected Output (Happy Path)

For input:

```txt
2
newuser
Newuser1!
1
newuser
Newuser1!
3
6
```

Program output:

```
InCollege CLI : Welcome!
1) Log in
2) Create account
Select an option:
Enter a username:
Enter a password (8 to 12, include uppercase, digit, special):
Account created! You can log in now.
1) Log in
2) Create account
Select an option:
Username:
Password:
Login successful.
1) Search jobs (coming soon)
2) Find people (coming soon)
3) Learn a new skill
Choose an option:
Learn a New Skill
1) Alpha
2) Beta
3) Gamma
4) Delta
5) Epsilon
6) Back
Your selection:
1) Search jobs (coming soon)
2) Find people (coming soon)
3) Learn a new skill
Choose an option:
--- END OF EXECUTION ---
```

---

## 🧭 Grading/Review Checklist

* [x] Free source format (`-free` compiles)
* [x] Input only from file (`InCollege-Input.txt`)
* [x] Output to screen + log file (`InCollege-Output.txt`)
* [x] Create account with validation & duplicate check
* [x] Login success and failure cases
* [x] Password policy enforced (8–12 chars, uppercase, digit, special)
* [x] Persistence with `accounts.dat`
* [x] Dashboard & Skills submenu
* [x] Graceful error if input file missing

---

## 🩺 Troubleshooting

* **Missing input file error:**

  ```
  FATAL: cannot open InCollege-Input.txt status=35
  ```

  Fix:

  ```bash
  mkdir -p data && cp data/InCollege-Input-happy.txt data/InCollege-Input.txt
  ```

* **Encoding warnings:**
  Ensure code is plain ASCII (no curly quotes).

* **accounts.dat issues:**
  Each record is one line = 40 chars (20 username + 20 password, both padded).
  Best practice: let the program create users.

* **Program ends too early:**
  Your test file ended (EOF). Add more lines if needed.

---

## 🧪 Optional Golden Output Comparison

Capture run output and compare with log:

```bash
./InCollege | tee run.out
diff -u run.out data/InCollege-Output.txt
```

---
