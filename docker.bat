@echo off
REM Convenience script to run Docker commands from the root directory (Windows)
REM This script passes all arguments to the docker-manage.bat script

set "SCRIPT_DIR=%~dp0"
set "DOCKER_SCRIPT=%SCRIPT_DIR%docker\docker-manage.bat"

if not exist "%DOCKER_SCRIPT%" (
    echo [91m[ERROR][0m Docker management script not found at %DOCKER_SCRIPT%
    exit /b 1
)

REM Pass all arguments to the docker-manage.bat script
call "%DOCKER_SCRIPT%" %*
