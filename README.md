EPIC 2 – USER PROFILE CREATION (Progress Report)

Completed so far (implemented in code, needs testing):
- Profile Information Capture:
    * First Name (required)
    * Last Name (required)
    * University/College Attended (required)
    * Major (required)
    * Graduation Year (required, 4-digit validation)
    * About Me (optional) → logic partially implemented, optional check needs finalization
- Experience (optional, up to 3 entries):
    * Title
    * Company/Organization
    * Dates
    * Description (optional)
- Profile Persistence:
    * User profile info written to InCollege-Profiles.dat
    * Linked to account login system

Not completed yet (needs development):
- Education (optional, up to 3 entries):
    * Degree
    * University/College
    * Years Attended
- Profile Viewing:
    * Ability for logged-in users to view their saved profile
- Extra Input Validation:
    * Ensure optional fields handle blank input cleanly
    * Handle invalid/random inputs gracefully

Notes:
- Code compiles, but full runtime testing is still required
- Persistence mechanism works, validation needed with multiple users
- Optional "About Me" toggle not fully implemented
