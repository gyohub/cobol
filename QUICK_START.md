# 🚀 Quick Start Guide - COBOL Banking System Docker

## Prerequisites

- Docker installed on your system
- Basic familiarity with command line
- **Windows users:** Use Command Prompt or PowerShell
- **Linux/Mac users:** Use Terminal or Bash

## 1. Test Your Setup

**Linux/Mac:**

```bash
./docker/test-docker.sh
```

**Windows:**

```cmd
docker\test-docker.bat
```

This will verify Docker is working and build a test image.

## 2. Build the Development Image

**Linux/Mac:**

```bash
./docker/docker-manage.sh build
```

**Windows:**

```cmd
docker\docker-manage.bat build
```

## 3. Start Development Environment

### Option A: Interactive Mode (Recommended)

**Linux/Mac:**

```bash
./docker/docker-manage.sh run
```

**Windows:**

```cmd
docker\docker-manage.bat run
```

This opens an interactive shell inside the container where you can:

- Run `./compile.sh` to compile your COBOL programs
- Run `./BANKTEST` to test the system
- Run `./BANKMAIN` to use the banking system
- Edit files on your host machine and see changes immediately

### Option B: Background Mode

**Linux/Mac:**

```bash
./docker/docker-manage.sh start
./docker/docker-manage.sh shell
```

**Windows:**

```cmd
docker\docker-manage.bat start
docker\docker-manage.bat shell
```

This starts the container in the background and opens a shell when needed.

## 4. Development Workflow

1. **Edit COBOL files** on your host machine (using your favorite editor)
2. **Compile and test** inside the container:
   ```bash
   ./compile.sh
   ./BANKTEST
   ./BANKMAIN
   ```
3. **Repeat** as needed

## 5. Stop When Done

**Linux/Mac:**

```bash
./docker/docker-manage.sh stop
```

**Windows:**

```cmd
docker\docker-manage.bat stop
```

## 🎯 Key Benefits

- ✅ **No local COBOL installation needed**
- ✅ **Consistent environment across all machines**
- ✅ **Volume mounting for instant file sync**
- ✅ **Isolated development environment**
- ✅ **Easy cleanup and reset**

## 📚 More Information

- **Full documentation:** `DOCKER_README.md`
- **Available commands:** `./docker.sh help` (Linux/Mac) or `docker.bat help` (Windows)
- **Troubleshooting:** See DOCKER_README.md

## 🔧 Common Commands

**Option 1: Using the convenience script (recommended):**

**Linux/Mac:**

```bash
./docker.sh build    # Build Docker image
./docker.sh run      # Interactive development
./docker.sh start    # Background mode
./docker.sh shell    # Open shell in running container
./docker.sh stop     # Stop container
./docker.sh clean    # Remove everything
```

**Windows:**

```cmd
docker.bat build     # Build Docker image
docker.bat run       # Interactive development
docker.bat start     # Background mode
docker.bat shell     # Open shell in running container
docker.bat stop      # Stop container
docker.bat clean     # Remove everything
```

**Option 2: Direct access:**

**Linux/Mac:**
| Command | Purpose |
| --------------------------------- | ------------------------------- |
| `./docker/docker-manage.sh build` | Build Docker image |
| `./docker/docker-manage.sh run` | Interactive development |
| `./docker/docker-manage.sh start` | Background mode |
| `./docker/docker-manage.sh shell` | Open shell in running container |
| `./docker/docker-manage.sh stop` | Stop container |
| `./docker/docker-manage.sh clean` | Remove everything |

**Windows:**
| Command | Purpose |
| ------------------------------ | ------------------------------- |
| `docker\docker-manage.bat build` | Build Docker image |
| `docker\docker-manage.bat run` | Interactive development |
| `docker\docker-manage.bat start` | Background mode |
| `docker\docker-manage.bat shell` | Open shell in running container |
| `docker\docker-manage.bat stop` | Stop container |
| `docker\docker-manage.bat clean` | Remove everything |

Happy COBOL development! 🏦💻
