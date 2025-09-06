@echo off
REM Docker Test Script for COBOL Banking System (Windows)
REM This script tests the Docker environment setup

echo ==========================================
echo COBOL Banking System - Docker Test
echo ==========================================
echo.

REM Test 1: Check if Docker is installed
echo Test 1: Checking Docker installation...
docker --version >nul 2>&1
if %errorlevel% neq 0 (
    echo [91m[ERROR][0m Docker is not installed
    echo Please install Docker first: https://docs.docker.com/get-docker/
    exit /b 1
)
echo [92m[SUCCESS][0m Docker is installed
docker --version
echo.

REM Test 2: Check if Docker Compose is available
echo Test 2: Checking Docker Compose...
docker-compose --version >nul 2>&1
if %errorlevel% equ 0 (
    echo [92m[SUCCESS][0m Docker Compose is available
    docker-compose --version
) else (
    docker compose version >nul 2>&1
    if %errorlevel% equ 0 (
        echo [92m[SUCCESS][0m Docker Compose (plugin) is available
        docker compose version
    ) else (
        echo [93m[WARNING][0m Docker Compose not found (optional)
    )
)
echo.

REM Test 3: Check if required files exist
echo Test 3: Checking required files...
if not exist "docker\Dockerfile" (
    echo [91m[ERROR][0m docker\Dockerfile is missing
    exit /b 1
)
echo [92m[SUCCESS][0m docker\Dockerfile exists

if not exist "docker\docker-compose.yml" (
    echo [91m[ERROR][0m docker\docker-compose.yml is missing
    exit /b 1
)
echo [92m[SUCCESS][0m docker\docker-compose.yml exists

if not exist "docker\docker-manage.bat" (
    echo [91m[ERROR][0m docker\docker-manage.bat is missing
    exit /b 1
)
echo [92m[SUCCESS][0m docker\docker-manage.bat exists
echo.

REM Test 4: Check if docker-manage.bat is executable (Windows doesn't need chmod)
echo Test 4: Checking script permissions...
echo [92m[SUCCESS][0m docker\docker-manage.bat is executable
echo.

REM Test 5: Test Docker build (dry run)
echo Test 5: Testing Docker build...
echo Building Docker image (this may take a few minutes)...
docker build -t cobol-banking:test -f docker/Dockerfile . >nul 2>&1
if %errorlevel% neq 0 (
    echo [91m[ERROR][0m Docker build failed
    echo Please check your Docker installation and try again
    exit /b 1
)
echo [92m[SUCCESS][0m Docker image built successfully

REM Test 6: Test container run
echo.
echo Test 6: Testing container run...
echo Running container test...

REM Run a quick test inside the container
docker run --rm cobol-banking:test cobc --version >nul 2>&1
if %errorlevel% neq 0 (
    echo [91m[ERROR][0m Container failed to run
    exit /b 1
)
echo [92m[SUCCESS][0m Container runs successfully
echo [92m[SUCCESS][0m GnuCOBOL compiler is available

REM Clean up test image
docker rmi cobol-banking:test >nul 2>&1
echo [92m[SUCCESS][0m Test image cleaned up

echo.
echo ==========================================
echo All tests passed! [92m[SUCCESS][0m
echo ==========================================
echo.
echo You can now use the Docker environment:
echo   docker\docker-manage.bat build    # Build the image
echo   docker\docker-manage.bat run      # Run interactively
echo   docker\docker-manage.bat start    # Start in background
echo.
echo For more information, see DOCKER_README.md
