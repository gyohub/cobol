# COBOL Banking System - Docker Development Environment

This Docker setup provides a complete development environment for the COBOL Banking System with GnuCOBOL compiler and all necessary tools.

## 🐳 Docker Setup

### Prerequisites

- Docker installed on your system
- Docker Compose (optional, but recommended)

### Quick Start

1. **Build the Docker image:**

   ```bash
   ./docker-manage.sh build
   ```

2. **Run interactively (recommended for development):**

   ```bash
   ./docker-manage.sh run
   ```

3. **Or start in background:**
   ```bash
   ./docker-manage.sh start
   ./docker-manage.sh shell
   ```

## 📁 Volume Mounting

The Docker setup mounts your local directory to `/app/cobol-banking` in the container, allowing you to:

- Edit COBOL files on your host machine
- Run compilation and execution inside the container
- See changes immediately reflected in the container

## 🛠️ Available Commands

### Docker Management Script (`./docker-manage.sh`)

| Command   | Description                     |
| --------- | ------------------------------- |
| `build`   | Build the Docker image          |
| `run`     | Run container interactively     |
| `start`   | Start container in background   |
| `stop`    | Stop the running container      |
| `restart` | Restart the container           |
| `logs`    | Show container logs             |
| `shell`   | Open shell in running container |
| `clean`   | Remove container and image      |
| `status`  | Show container status           |
| `help`    | Show help message               |

### Inside the Container

Once inside the container, you can use:

```bash
# Compile all COBOL programs
./compile.sh

# Run the test program
./BANKTEST

# Run the main banking system
./BANKMAIN

# Load sample data
./DATALOADER

# Check GnuCOBOL version
cobc --version
```

## 🔧 Development Workflow

### Method 1: Interactive Development

1. **Start the container:**

   ```bash
   ./docker-manage.sh run
   ```

2. **Inside the container, compile and test:**

   ```bash
   ./compile.sh
   ./BANKTEST
   ./BANKMAIN
   ```

3. **Edit files on your host machine** - changes are immediately available in the container

4. **Recompile and test** as needed

### Method 2: Background Development

1. **Start container in background:**

   ```bash
   ./docker-manage.sh start
   ```

2. **Open shell when needed:**

   ```bash
   ./docker-manage.sh shell
   ```

3. **Work inside the shell, exit when done**

4. **Stop when finished:**
   ```bash
   ./docker-manage.sh stop
   ```

## 📋 Container Details

### Base Image

- **Ubuntu 22.04 LTS** - Stable, well-supported Linux distribution

### Installed Software

- **GnuCOBOL 3.2+** - Modern COBOL compiler
- **Build tools** - gcc, make, etc.
- **Development tools** - vim, nano, git
- **System utilities** - curl, wget, less

### User Account

- **Username:** `coboldev`
- **Home directory:** `/home/coboldev`
- **Working directory:** `/app/cobol-banking`

### Volume Mounts

- **Host directory** → `/app/cobol-banking` (your COBOL project)
- **Persistent volume** → `/app/cobol-banking/data` (for compiled files)

## 🚀 Example Usage

### Complete Development Session

```bash
# 1. Build the image (first time only)
./docker-manage.sh build

# 2. Start interactive session
./docker-manage.sh run

# 3. Inside container:
./compile.sh
./BANKTEST
./BANKMAIN

# 4. Edit files on host machine, then back to container:
./compile.sh
./BANKMAIN
```

### Quick Testing

```bash
# Start container
./docker-manage.sh start

# Run tests
./docker-manage.sh shell
./compile.sh && ./BANKTEST

# Stop when done
./docker-manage.sh stop
```

## 🔍 Troubleshooting

### Container Won't Start

```bash
# Check Docker status
docker ps -a

# Check logs
./docker-manage.sh logs

# Clean and rebuild
./docker-manage.sh clean
./docker-manage.sh build
```

### Permission Issues

The container runs as a non-root user (`coboldev`) with sudo privileges. If you encounter permission issues:

```bash
# Inside container
sudo chown -R coboldev:coboldev /app/cobol-banking
```

### Volume Mount Issues

Ensure your host directory is properly mounted:

```bash
# Check mounted volumes
docker inspect cobol-banking-dev | grep -A 10 "Mounts"
```

## 📊 Performance Notes

- **First build:** May take 2-3 minutes to download Ubuntu and install packages
- **Subsequent runs:** Start in seconds
- **Volume mounting:** Near-instant file synchronization
- **Compilation:** Same speed as native Linux

## 🔒 Security

- Container runs as non-root user
- No unnecessary services exposed
- Minimal attack surface
- Isolated development environment

## 🎯 Benefits

1. **Consistent Environment** - Same COBOL compiler version across all machines
2. **Easy Setup** - No need to install COBOL compiler locally
3. **Isolation** - Development environment doesn't affect host system
4. **Portability** - Works on Windows, macOS, and Linux
5. **Version Control** - Dockerfile ensures reproducible builds

## 📝 Next Steps

After setting up Docker:

1. **Develop your COBOL programs** on your host machine
2. **Compile and test** inside the container
3. **Iterate quickly** with volume mounting
4. **Deploy** using the same Docker image

This setup provides a professional development environment for COBOL programming with modern tooling and best practices!
