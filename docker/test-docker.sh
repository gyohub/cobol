#!/bin/bash

# Docker Test Script for COBOL Banking System
# This script tests the Docker environment setup

set -e

echo "=========================================="
echo "COBOL Banking System - Docker Test"
echo "=========================================="
echo ""

# Test 1: Check if Docker is installed
echo "Test 1: Checking Docker installation..."
if command -v docker &> /dev/null; then
    echo "✓ Docker is installed"
    docker --version
else
    echo "✗ Docker is not installed"
    echo "Please install Docker first: https://docs.docker.com/get-docker/"
    exit 1
fi
echo ""

# Test 2: Check if Docker Compose is available
echo "Test 2: Checking Docker Compose..."
if command -v docker-compose &> /dev/null; then
    echo "✓ Docker Compose is available"
    docker-compose --version
elif docker compose version &> /dev/null; then
    echo "✓ Docker Compose (plugin) is available"
    docker compose version
else
    echo "⚠ Docker Compose not found (optional)"
fi
echo ""

# Test 3: Check if required files exist
echo "Test 3: Checking required files..."
required_files=("docker/Dockerfile" "docker/docker-compose.yml" "docker/docker-manage.sh")
for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ $file exists"
    else
        echo "✗ $file is missing"
        exit 1
    fi
done
echo ""

# Test 4: Check if docker-manage.sh is executable
echo "Test 4: Checking script permissions..."
if [ -x "docker/docker-manage.sh" ]; then
    echo "✓ docker/docker-manage.sh is executable"
else
    echo "⚠ Making docker/docker-manage.sh executable..."
    chmod +x docker/docker-manage.sh
    echo "✓ docker/docker-manage.sh is now executable"
fi
echo ""

# Test 5: Test Docker build (dry run)
echo "Test 5: Testing Docker build..."
echo "Building Docker image (this may take a few minutes)..."
if docker build -t cobol-banking:test -f docker/Dockerfile . > /dev/null 2>&1; then
    echo "✓ Docker image built successfully"
    
    # Test 6: Test container run
    echo ""
    echo "Test 6: Testing container run..."
    echo "Running container test..."
    
    # Run a quick test inside the container
    if docker run --rm cobol-banking:test cobc --version > /dev/null 2>&1; then
        echo "✓ Container runs successfully"
        echo "✓ GnuCOBOL compiler is available"
    else
        echo "✗ Container failed to run"
        exit 1
    fi
    
    # Clean up test image
    docker rmi cobol-banking:test > /dev/null 2>&1
    echo "✓ Test image cleaned up"
    
else
    echo "✗ Docker build failed"
    echo "Please check your Docker installation and try again"
    exit 1
fi

echo ""
echo "=========================================="
echo "All tests passed! 🎉"
echo "=========================================="
echo ""
echo "You can now use the Docker environment:"
echo "  ./docker/docker-manage.sh build    # Build the image"
echo "  ./docker/docker-manage.sh run      # Run interactively"
echo "  ./docker/docker-manage.sh start    # Start in background"
echo ""
echo "For more information, see DOCKER_README.md"
