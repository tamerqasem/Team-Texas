# InCollege
> [!WARNING]
> Don't change this `README.md` for describing a specific **Epic** or **Assignment**.

## How to compile
Open the project in vscode, and press the docker environment by clicking on the "Reopen in Container" pop-up.

After succesfully reopening the project in the docker container, you can compile and run using the following commands:
```bash
cobc src/InCollege.cob -o bin/InCollege -x -free
./bin/InCollege
```

Make sure to put all of your test cases somewhere in the `assignments/Epic [#]/` directory after writing them.
Anything in the `bin/` directory won't be pushed to the repository because of the `.gitignore` setup that currently exists.
