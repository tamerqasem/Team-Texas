# Epic 4 - Connection Request Test Cases

## Overview
This directory contains test cases for Epic 4, which implements connection request functionality in the InCollege application.

## Features Tested
1. **Find someone you know** - Search for users by full name
2. **Send connection request** - Send requests to found users
3. **View pending connection requests** - See requests sent to you
4. **Duplicate request prevention** - Prevent sending duplicate requests
5. **Self-request prevention** - Prevent sending requests to yourself

## Test Case Structure

### Valid Cases
- **SuccessfulConnectionRequestInput.txt** - Basic successful connection request flow
- **ViewMultiplePendingRequestsInput.txt** - View multiple pending requests
- **SearchAndConnectFlowInput.txt** - Complete search and connect workflow
- **NoPendingRequestsInput.txt** - View requests when none exist
- **CompleteConnectionWorkflowInput.txt** - Full workflow with profile creation
- **ViewRequestsWithProfilesInput.txt** - View requests with profile resolution

### Invalid Cases
- **SearchNonExistentUserInput.txt** - Search for user that doesn't exist
- **SendRequestToSelfInput.txt** - Attempt to send request to yourself
- **DuplicateRequestInput.txt** - Attempt to send duplicate request
- **EmptySearchNameInput.txt** - Search with empty name
- **InvalidMenuChoiceInput.txt** - Invalid menu choice in connection flow

## Test Data Requirements

### User Accounts (in data/accounts.dat)
- alice / Password123!
- bob / Password123!
- charlie / Password123!
- david / Password123!
- eve / Password123!
- frank / Password123!

### User Profiles (in data/InCollege-Profiles.dat)
- Alice Johnson (alice) - University of Central Florida, Computer Engineering, 2024
- Bob Smith (bob) - University of Florida, Computer Science, 2025
- Emma Wilson (emma) - Florida State University, Data Science, 2024
- Charlie Brown (charlie) - University of Miami, Business, 2023
- David Wilson (david) - Florida Institute of Technology, Engineering, 2024

### Connection Requests (in data/InCollege-Requests.dat)
- alice -> bob
- charlie -> alice
- david -> bob

## Expected Behaviors

### Valid Scenarios
1. **Successful Connection Request**: User finds another user, sends request, gets confirmation
2. **View Pending Requests**: Shows list of users who sent requests to current user
3. **No Pending Requests**: Shows appropriate message when no requests exist
4. **Profile Resolution**: Shows full names when available, usernames when not

### Invalid Scenarios
1. **Non-existent User**: Returns "No one by that name could be found"
2. **Self-request**: Returns "You cannot send a request to yourself"
3. **Duplicate Request**: Returns "You have already sent this user a connection request"
4. **Empty Search**: Exits search without action
5. **Invalid Menu Choice**: Returns "Please pick 1, 2, 3, 4, or 5"

## Running Tests
1. Ensure COBOL compiler (cobc) is installed and in PATH
2. Compile: `cobc -x InCollege2.cob`
3. Run: `./InCollege2 < TestCaseEpic4/Valid Cases/Inputs/[testfile].txt`
4. Compare output with expected output in corresponding Outputs directory

## Notes
- All usernames are case-insensitive (converted to uppercase)
- Connection requests are stored in data/InCollege-Requests.dat
- Profile lookups are case-insensitive for name matching
- The system prevents both outgoing and incoming duplicate requests
