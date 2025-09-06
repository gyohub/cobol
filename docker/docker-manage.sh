#!/bin/bash

# COBOL Banking System Docker Management Script
# This script helps you build, run, and manage the Docker container

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to show usage
show_usage() {
    echo "COBOL Banking System Docker Management"
    echo "======================================"
    echo ""
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  build     - Build the Docker image"
    echo "  run       - Run the container interactively"
    echo "  start     - Start the container in background"
    echo "  stop      - Stop the running container"
    echo "  restart   - Restart the container"
    echo "  logs      - Show container logs"
    echo "  shell     - Open shell in running container"
    echo "  clean     - Remove container and image"
    echo "  status    - Show container status"
    echo "  help      - Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 build && $0 run    # Build and run interactively"
    echo "  $0 start              # Start in background"
    echo "  $0 shell              # Open shell in running container"
}

# Function to build the Docker image
build_image() {
    print_status "Building COBOL Banking System Docker image..."
    docker build -t cobol-banking:latest -f docker/Dockerfile .
    print_success "Docker image built successfully!"
}

# Function to run container interactively
run_container() {
    print_status "Starting COBOL Banking System container interactively..."
    docker run -it --rm \
        --name cobol-banking-dev \
        -v "$(pwd):/app/cobol-banking" \
        -w /app/cobol-banking \
        cobol-banking:latest
}

# Function to start container in background
start_container() {
    print_status "Starting COBOL Banking System container in background..."
    docker-compose -f docker/docker-compose.yml up -d
    print_success "Container started! Use '$0 shell' to access it."
}

# Function to stop container
stop_container() {
    print_status "Stopping COBOL Banking System container..."
    docker-compose -f docker/docker-compose.yml down
    print_success "Container stopped!"
}

# Function to restart container
restart_container() {
    print_status "Restarting COBOL Banking System container..."
    docker-compose -f docker/docker-compose.yml restart
    print_success "Container restarted!"
}

# Function to show logs
show_logs() {
    print_status "Showing container logs..."
    docker-compose -f docker/docker-compose.yml logs -f
}

# Function to open shell in running container
open_shell() {
    print_status "Opening shell in running container..."
    docker exec -it cobol-banking-dev /bin/bash
}

# Function to clean up
clean_up() {
    print_warning "This will remove the container and image. Are you sure? (y/N)"
    read -r response
    if [[ "$response" =~ ^[Yy]$ ]]; then
        print_status "Stopping and removing container..."
        docker-compose -f docker/docker-compose.yml down
        print_status "Removing Docker image..."
        docker rmi cobol-banking:latest
        print_success "Cleanup completed!"
    else
        print_status "Cleanup cancelled."
    fi
}

# Function to show status
show_status() {
    print_status "Container status:"
    docker-compose -f docker/docker-compose.yml ps
    echo ""
    print_status "Docker images:"
    docker images | grep cobol-banking
}

# Main script logic
case "${1:-help}" in
    build)
        build_image
        ;;
    run)
        run_container
        ;;
    start)
        start_container
        ;;
    stop)
        stop_container
        ;;
    restart)
        restart_container
        ;;
    logs)
        show_logs
        ;;
    shell)
        open_shell
        ;;
    clean)
        clean_up
        ;;
    status)
        show_status
        ;;
    help|--help|-h)
        show_usage
        ;;
    *)
        print_error "Unknown command: $1"
        echo ""
        show_usage
        exit 1
        ;;
esac
