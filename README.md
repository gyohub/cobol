# COBOL Banking System

## Overview

This is a comprehensive COBOL banking system that demonstrates various banking operations including customer management, transaction processing, and report generation. The system showcases modern COBOL programming practices with proper file handling, data validation, and user interaction.

## Project Structure

### Core Programs

- **BANKMAIN.cbl** - Main banking program with integrated menu system
- **TRANSPROC.cbl** - Transaction processing module for deposits, withdrawals, and transfers
- **REPORTGEN.cbl** - Comprehensive report generation module
- **USERINTERFACE.cbl** - Enhanced user interface with detailed menus and validation

### Data Files

- **CUSTOMER.DAT** - Indexed customer account file
- **TRANSACT.DAT** - Sequential transaction log file
- **BANKREPORT.TXT** - Generated report output file

## Features

### Banking Operations

1. **Customer Account Inquiry** - View account details and balances
2. **Deposit Transactions** - Add money to customer accounts
3. **Withdrawal Transactions** - Remove money from customer accounts with balance validation
4. **Transfer Transactions** - Move money between accounts
5. **Customer Management** - Add, update, and deactivate customer accounts

### Data Management

- **Indexed File Access** - Efficient random access to customer records
- **Sequential Transaction Logging** - Complete audit trail of all transactions
- **Data Validation** - Input validation and error handling
- **File Status Checking** - Proper error handling for file operations

### Reporting System

- **Customer Listing Report** - Complete customer account information
- **Transaction History Report** - Detailed transaction log
- **Summary Statistics Report** - Bank-wide statistics and totals
- **Formatted Output** - Professional report formatting with headers and totals

### User Interface

- **Menu-Driven System** - Intuitive navigation through banking operations
- **Input Validation** - Comprehensive validation of user inputs
- **Error Messages** - Clear error messages for invalid operations
- **Confirmation Dialogs** - User confirmation for critical operations

## File Structures

### Customer Record (CUSTOMER.DAT)

```
01 CUSTOMER-RECORD.
   05 CUST-ACCOUNT-NUMBER    PIC 9(10).     - Account number (key)
   05 CUST-NAME             PIC X(30).     - Customer name
   05 CUST-ADDRESS          PIC X(50).     - Customer address
   05 CUST-PHONE            PIC X(15).     - Phone number
   05 CUST-BALANCE          PIC 9(10)V99.  - Account balance
   05 CUST-ACCOUNT-TYPE     PIC X(1).      - S=Savings, C=Checking
   05 CUST-STATUS           PIC X(1).      - A=Active, I=Inactive
   05 CUST-DATE-OPENED      PIC 9(8).      - Date account opened
   05 FILLER                PIC X(20).     - Reserved space
```

### Transaction Record (TRANSACT.DAT)

```
01 TRANSACTION-RECORD.
   05 TRANS-ID              PIC 9(10).     - Transaction ID
   05 TRANS-ACCOUNT-NUMBER  PIC 9(10).     - Account number
   05 TRANS-TYPE            PIC X(1).      - D=Deposit, W=Withdrawal, T=Transfer
   05 TRANS-AMOUNT          PIC 9(10)V99.  - Transaction amount
   05 TRANS-DATE            PIC 9(8).      - Transaction date
   05 TRANS-TIME            PIC 9(6).      - Transaction time
   05 TRANS-DESCRIPTION     PIC X(30).     - Transaction description
   05 FILLER                PIC X(20).     - Reserved space
```

## Sample Data

The system includes sample data with 10 customer accounts and 15 sample transactions demonstrating various banking operations.

## Compilation and Execution

### Compilation

```bash
# Compile main program
cobc -x BANKMAIN.cbl

# Compile transaction processing module
cobc -x TRANSPROC.cbl

# Compile report generation module
cobc -x REPORTGEN.cbl

# Compile user interface module
cobc -x USERINTERFACE.cbl
```

### Execution

```bash
# Run the main banking system
./BANKMAIN

# Generate reports
./REPORTGEN
```

## Key COBOL Concepts Demonstrated

1. **File Organization**

   - Indexed files for random access
   - Sequential files for transaction logging
   - Proper file status handling

2. **Data Validation**

   - Input validation routines
   - Error handling and messaging
   - Business rule enforcement

3. **Program Structure**

   - Modular program design
   - Clear division organization
   - Proper use of working storage

4. **User Interface**

   - Menu-driven navigation
   - Formatted output display
   - Interactive data entry

5. **Report Generation**
   - Professional report formatting
   - Statistical calculations
   - Multi-page report handling

## Business Rules Implemented

1. **Account Validation** - All account numbers must exist before transactions
2. **Balance Validation** - Withdrawals cannot exceed available balance
3. **Amount Validation** - All transaction amounts must be positive
4. **Transfer Validation** - Both source and destination accounts must exist
5. **Status Checking** - Only active accounts can perform transactions

## Error Handling

The system includes comprehensive error handling for:

- Invalid account numbers
- Insufficient funds
- Invalid transaction amounts
- File operation errors
- Data validation failures

## Future Enhancements

Potential enhancements could include:

- Interest calculation for savings accounts
- Monthly statement generation
- Online banking features
- Multi-currency support
- Advanced reporting capabilities
- Database integration
- Web service interfaces

## Technical Notes

- Uses COBOL-85 standard features
- Compatible with GnuCOBOL compiler
- Follows enterprise COBOL programming standards
- Includes proper documentation and comments
- Demonstrates best practices for COBOL development

This banking system serves as an excellent example of COBOL programming for educational purposes and demonstrates the power and flexibility of COBOL for business applications.

