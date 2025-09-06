@echo off
REM COBOL Banking System Docker Management Script for Windows
REM This script helps you build, run, and manage the Docker container

setlocal enabledelayedexpansion

REM Colors (limited support in Windows batch)
set "RED=[91m"
set "GREEN=[92m"
set "YELLOW=[93m"
set "BLUE=[94m"
set "NC=[0m"

REM Function to print colored output
:print_status
echo %BLUE%[INFO]%NC% %~1
goto :eof

:print_success
echo %GREEN%[SUCCESS]%NC% %~1
goto :eof

:print_warning
echo %YELLOW%[WARNING]%NC% %~1
goto :eof

:print_error
echo %RED%[ERROR]%NC% %~1
goto :eof

REM Function to show usage
:show_usage
echo COBOL Banking System Docker Management
echo ======================================
echo.
echo Usage: %~nx0 [COMMAND]
echo.
echo Commands:
echo   build     - Build the Docker image
echo   run       - Run the container interactively
echo   start     - Start the container in background
echo   stop      - Stop the running container
echo   restart   - Restart the container
echo   logs      - Show container logs
echo   shell     - Open shell in running container
echo   clean     - Remove container and image
echo   status    - Show container status
echo   help      - Show this help message
echo.
echo Examples:
echo   %~nx0 build ^&^& %~nx0 run    # Build and run interactively
echo   %~nx0 start              # Start in background
echo   %~nx0 shell              # Open shell in running container
goto :eof

REM Function to build the Docker image
:build_image
call :print_status "Building COBOL Banking System Docker image..."
docker build -t cobol-banking:latest -f docker/Dockerfile .
if %errorlevel% neq 0 (
    call :print_error "Docker build failed!"
    exit /b 1
)
call :print_success "Docker image built successfully!"
goto :eof

REM Function to run container interactively
:run_container
call :print_status "Starting COBOL Banking System container interactively..."
docker run -it --rm --name cobol-banking-dev -v "%cd%:/app/cobol-banking" -w /app/cobol-banking cobol-banking:latest
goto :eof

REM Function to start container in background
:start_container
call :print_status "Starting COBOL Banking System container in background..."
docker-compose -f docker/docker-compose.yml up -d
if %errorlevel% neq 0 (
    call :print_error "Failed to start container!"
    exit /b 1
)
call :print_success "Container started! Use '%~nx0 shell' to access it."
goto :eof

REM Function to stop container
:stop_container
call :print_status "Stopping COBOL Banking System container..."
docker-compose -f docker/docker-compose.yml down
if %errorlevel% neq 0 (
    call :print_error "Failed to stop container!"
    exit /b 1
)
call :print_success "Container stopped!"
goto :eof

REM Function to restart container
:restart_container
call :print_status "Restarting COBOL Banking System container..."
docker-compose -f docker/docker-compose.yml restart
if %errorlevel% neq 0 (
    call :print_error "Failed to restart container!"
    exit /b 1
)
call :print_success "Container restarted!"
goto :eof

REM Function to show logs
:show_logs
call :print_status "Showing container logs..."
docker-compose -f docker/docker-compose.yml logs -f
goto :eof

REM Function to open shell in running container
:open_shell
call :print_status "Opening shell in running container..."
docker exec -it cobol-banking-dev /bin/bash
goto :eof

REM Function to clean up
:clean_up
call :print_warning "This will remove the container and image. Are you sure? (y/N)"
set /p response=
if /i "%response%"=="y" (
    call :print_status "Stopping and removing container..."
    docker-compose -f docker/docker-compose.yml down
    call :print_status "Removing Docker image..."
    docker rmi cobol-banking:latest
    call :print_success "Cleanup completed!"
) else (
    call :print_status "Cleanup cancelled."
)
goto :eof

REM Function to show status
:show_status
call :print_status "Container status:"
docker-compose -f docker/docker-compose.yml ps
echo.
call :print_status "Docker images:"
docker images | findstr cobol-banking
goto :eof

REM Main script logic
if "%1"=="" goto :help
if "%1"=="help" goto :help
if "%1"=="--help" goto :help
if "%1"=="-h" goto :help
if "%1"=="build" goto :build_image
if "%1"=="run" goto :run_container
if "%1"=="start" goto :start_container
if "%1"=="stop" goto :stop_container
if "%1"=="restart" goto :restart_container
if "%1"=="logs" goto :show_logs
if "%1"=="shell" goto :open_shell
if "%1"=="clean" goto :clean_up
if "%1"=="status" goto :show_status

call :print_error "Unknown command: %1"
echo.
goto :help

:help
call :show_usage
exit /b 0
