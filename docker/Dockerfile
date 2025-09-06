# Dockerfile for COBOL Banking System Development
# Uses Ubuntu with GnuCOBOL compiler
FROM ubuntu:22.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV COBOL_HOME=/usr/local/cobol
ENV PATH=$PATH:$COBOL_HOME/bin

# Install system dependencies and GnuCOBOL
RUN apt-get update && apt-get install -y \
    build-essential \
    vim \
    nano \
    less \
    curl \
    wget \
    git \
    software-properties-common \
    && add-apt-repository universe \
    && apt-get update \
    && apt-get install -y gnucobol4 \
    && rm -rf /var/lib/apt/lists/*

# Create working directory
WORKDIR /app

# Create a non-root user for development
RUN useradd -m -s /bin/bash coboldev && \
    usermod -aG sudo coboldev && \
    echo 'coboldev ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Set up COBOL environment
RUN echo 'export PATH=$PATH:/usr/bin' >> /home/coboldev/.bashrc && \
    echo 'alias ll="ls -la"' >> /home/coboldev/.bashrc && \
    echo 'alias cobc="cobc -std=cobol85"' >> /home/coboldev/.bashrc

# Create directories for the banking system
RUN mkdir -p /app/cobol-banking && \
    chown -R coboldev:coboldev /app

# Switch to non-root user
USER coboldev

# Set working directory
WORKDIR /app/cobol-banking

# Create a simple entry script
RUN echo '#!/bin/bash' > /app/cobol-banking/entrypoint.sh && \
    echo 'echo "=========================================="' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "COBOL Banking System Development Environment"' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "=========================================="' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo ""' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "Available commands:"' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "  ./compile.sh    - Compile all COBOL programs"' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "  ./BANKTEST      - Run test program"' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "  ./BANKMAIN      - Run main banking system"' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "  ./DATALOADER    - Load sample data"' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo ""' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "GnuCOBOL version:"' >> /app/cobol-banking/entrypoint.sh && \
    echo 'cobc --version' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo ""' >> /app/cobol-banking/entrypoint.sh && \
    echo 'echo "Starting interactive shell..."' >> /app/cobol-banking/entrypoint.sh && \
    echo 'exec /bin/bash' >> /app/cobol-banking/entrypoint.sh && \
    chmod +x /app/cobol-banking/entrypoint.sh

# Expose port for potential web interface (future enhancement)
EXPOSE 8080

# Set the entrypoint
ENTRYPOINT ["/app/cobol-banking/entrypoint.sh"]
