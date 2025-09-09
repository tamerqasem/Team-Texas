# GNU COBOL + Docker + VS Code (Dev Containers)

A cross-platform template for teaching **GNU COBOL** using **Docker** and **VS Code Dev Containers**. 
Works the same on **Windows 11** and **macOS** (Apple Silicon and Intel).

## Quick Start (Students)

**Prerequisites**
- Install **Docker Desktop**: https://www.docker.com/products/docker-desktop/
  - Windows: enable **WSL 2** backend during installation.
- Install **VS Code**: https://code.visualstudio.com/
- In VS Code, install the **Dev Containers** extension (ID: `ms-vscode-remote.remote-containers`).

**Run the template**
1. Clone this repo and open it in VS Code.
2. When prompted, click **“Reopen in Container”**. (Or run *Dev Containers: Reopen in Container* from the Command Palette.)
3. Open `src/hello.cob`.
4. Press **Ctrl+Shift+B** (or **⇧⌘B** on Mac) to **Build**, or run the task **COBOL: Run active file (after build)** from the command palette.
5. The compiled program will be placed in `bin/` and run in the VS Code terminal.

## Features
- Docker image with **Ubuntu 22.04 + GNU COBOL (gnucobol)**.
- VS Code tasks to **build and run the active COBOL file**.
- Default UTF-8 locale configured.
- No extra installs on host OS beyond Docker + VS Code.

## Common Commands (inside the container)
```bash
# Compile and run a COBOL program manually
cobc -x -o bin/hello src/hello.cob
./bin/hello
```

## Troubleshooting
- If VS Code doesn’t prompt to reopen in a container, run **Dev Containers: Reopen in Container** manually.
- On Windows, make sure Docker Desktop is running with **WSL 2** enabled.
- On Apple Silicon (M1/M2/M3), Docker will pull the correct multi-arch Ubuntu image automatically.
- If you see permission issues on `bin/` after pulling from a different OS, run: `sudo chmod -R a+rw bin` (inside the container).

## Folder Structure
```
.devcontainer/         # Dev container config (Dockerfile, devcontainer.json)
.vscode/               # VS Code tasks
src/                   # Your COBOL source files
bin/                   # Build outputs (gitignored)
```

## License
MIT
