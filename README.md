# Epic #3 — View Profile + “Find someone you know”

This epic adds two big post-login features to **InCollege** (COBOL):

1. **View My Profile** – prints the logged-in user’s full profile, nicely labeled.
2. **Find someone you know** – search by **full name** (case-insensitive), and if found, print that user’s full profile using the same display helper.

---

## What’s in this branch

**Branch:** `feature/epic3-profiles-search`
**Main source:** `InCollege.cob`
**Data directory:** `data/`

Key files used at runtime:

* `data/accounts.dat` – username/password pairs (line-sequential).
* `data/InCollege-Input.txt` – scripted input (the app reads all prompts from here).
* `data/InCollege-Output.txt` – transcript of everything displayed (mirrors the console).
* `data/InCollege-Profiles.dat` – fixed-length profile store.
* `data/InCollege-Profiles.tmp` – temp file used for UPSERT (created during save).

---

## Build & run

```bash
# from repo root
cobc -x -free -o InCollege InCollege.cob
./InCollege
```

> The program **reads all user input** from `data/InCollege-Input.txt` and **writes all output** to both the console and `data/InCollege-Output.txt`.
> When input ends, it prints: `--- END_OF_PROGRAM_EXECUTION ---` and exits.

---

## Menus (post-login)

```
1. Create/Edit My Profile
2. View My Profile
3. Find someone you know
4. Learn a New Skill
```

* **2. View My Profile** renders the current user’s profile.
* **3. Find someone you know** prompts for a **full name** (e.g., `Suyog Bam`), matches case-insensitively, and prints that user’s profile if found.

---

## Features implemented in code

* **Reusable profile printer**: `DISPLAY-PR` – one place that formats Name, University, Major, Graduation Year, About, Experience (up to 3), and Education (up to 3).
* **Search flow**: `FIND-SOMEONE`:

  * Prompts for **full name**.
  * Upper-cases and trims input, compares to `PR-FNAME + " " + PR-LNAME` (also upper-cased & trimmed).
  * On a match: prints **“--- Found User Profile ---”** and calls `DISPLAY-PR`.
  * Otherwise: **“No one by that name could be found.”**
* **View flow**: `VIEW-PROFILE`:

  * Scans `InCollege-Profiles.dat` for the logged-in user.
  * Calls `DISPLAY-PR` to render the profile.
* **UPSERT save** for profiles:

  * Writes to a temp file and replaces existing record for the same `PR-USER`, or appends if not present.
* **Validation**:

  * **Password** (at registration): length 8–12, must include **uppercase**, **digit**, and **special** char.
  * **Graduation year**: exactly 4 digits, **1900–2100** inclusive.
* **I/O contract**:

  * Every `DISPLAY` also goes to `data/InCollege-Output.txt` (via `SAY`).
  * All prompts are satisfied from `data/InCollege-Input.txt` (no interactive stdin).

---

## Data model (high level)

* **Accounts**: username (case-insensitive on login), password (case-sensitive).
* **Profiles** (per user; stored upper-cased username):

  * First/Last Name (required)
  * University (required)
  * Major (required)
  * Graduation Year (4-digit validated)
  * About (optional, up to **100** chars)
  * **Experience** (0–3): Title, Company, Dates, Description (50 chars)
  * **Education** (0–3): Degree, School, Years

> Note: The search compares the exact **full name** with a single space between first and last (case-insensitive). Extra internal spaces won’t match.

---

## Quick start test (smoke)

Put this in `data/InCollege-Input.txt` and run:

```
1
newuser
Newuser1!
1
Alex
Rivera
USF
Information Technology
2026
Curious IT student exploring systems and support.
Help Desk Intern
USF IT
Summer 2024
Provided Tier 1 support.
DONE
BS Information Technology
USF
2022-2026
DONE
2
3
Suyog Bam
```

**Expected:**

* Save profile → “Profile saved successfully!”
* **View My Profile** prints Alex’s profile.
* **Find someone you know** finds and prints **Suyog Bam** (assuming `suyog`’s profile already exists in `data/InCollege-Profiles.dat`).

---

## More test blocks (drop into `data/InCollege-Input.txt`)

**A) View only**

```
1
newuser
Newuser1!
2
```

**B) Search not found**

```
1
newuser
Newuser1!
3
No Such Person
```

**C) Year validation (bad → bad → good)**

```
1
newuser
Newuser1!
1
Temp
User
USF
CS
20a5
1800
2027

DONE
DONE
```

> After each invalid year you’ll be reprompted until a valid 4-digit year in 1900–2100 is entered.

---

## File layout (what to submit for grading)

```
.
├── InCollege.cob
├── README.md
└── data/
    ├── accounts.dat
    ├── InCollege-Input.txt
    ├── InCollege-Output.txt
    ├── InCollege-Profiles.dat
    └── InCollege-Profiles.tmp   # generated; include only if your grader asks
```

---

## Known limitations

* **Search** requires an exact **single-space** between first and last name. “Suyog   Bam” (multiple spaces) won’t match.
* Prompts may allow longer typing, but the file fields themselves are fixed width; values are trimmed when saved/printed.

---


## Troubleshooting

* **No output file?** Check write permissions for `data/InCollege-Output.txt`. The program creates it on boot.
* **Input stops early?** Ensure your `data/InCollege-Input.txt` includes enough lines to satisfy every prompt.
* **“No profile found” for your user?** Create/edit the profile first (Menu 1), then view it (Menu 2).

---


