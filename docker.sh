#!/bin/bash

# Convenience script to run Docker commands from the root directory
# This script passes all arguments to the docker-manage.sh script

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKER_SCRIPT="$SCRIPT_DIR/docker/docker-manage.sh"

if [ ! -f "$DOCKER_SCRIPT" ]; then
    echo "Error: Docker management script not found at $DOCKER_SCRIPT"
    exit 1
fi

# Make sure the script is executable
chmod +x "$DOCKER_SCRIPT"

# Pass all arguments to the docker-manage.sh script
exec "$DOCKER_SCRIPT" "$@"
