#!/bin/bash

# COBOL Banking System Compilation Script
# This script compiles all COBOL programs in the banking system

echo "=========================================="
echo "COBOL Banking System Compilation Script"
echo "=========================================="
echo ""

# Check if cobc is installed
if ! command -v cobc &> /dev/null; then
    echo "Error: GnuCOBOL compiler (cobc) is not installed."
    echo "Please install GnuCOBOL first:"
    echo "  Ubuntu/Debian: sudo apt-get install open-cobol"
    echo "  macOS: brew install gnu-cobol"
    echo "  CentOS/RHEL: sudo yum install open-cobol"
    exit 1
fi

echo "GnuCOBOL compiler found. Starting compilation..."
echo ""

# Compile main banking program
echo "Compiling BANKMAIN.cbl..."
if cobc -x BANKMAIN.cbl; then
    echo "✓ BANKMAIN.cbl compiled successfully"
else
    echo "✗ Failed to compile BANKMAIN.cbl"
    exit 1
fi

# Compile test program
echo "Compiling BANKTEST.cbl..."
if cobc -x BANKTEST.cbl; then
    echo "✓ BANKTEST.cbl compiled successfully"
else
    echo "✗ Failed to compile BANKTEST.cbl"
    exit 1
fi

echo ""
echo "=========================================="
echo "Core programs compiled successfully!"
echo "=========================================="
echo ""
echo "Executable files created:"
echo "  - BANKMAIN (Main banking system)"
echo "  - BANKTEST (Test program)"
echo ""
echo "To test the system:"
echo "  ./BANKTEST"
echo ""
echo "To run the banking system:"
echo "  ./BANKMAIN"
echo ""
echo "Note: Additional modules (TRANSPROC, REPORTGEN, USERINTERFACE)"
echo "have syntax issues that need to be resolved for full functionality."
echo ""
echo "Sample data files:"
echo "  - CUSTOMER.DAT (Customer accounts)"
echo "  - TRANSACT.DAT (Transaction history)"
echo "  - BANKREPORT.TXT (Generated reports)"
