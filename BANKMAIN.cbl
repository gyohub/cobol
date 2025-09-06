       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKMAIN.
       AUTHOR. COBOL Banking System.
       DATE-WRITTEN. 2024.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ACCOUNT-NUMBER
               FILE STATUS IS CUSTOMER-FILE-STATUS.
               
           SELECT TRANSACTION-FILE ASSIGN TO 'TRANSACT.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS TRANSACTION-FILE-STATUS.
               
           SELECT REPORT-FILE ASSIGN TO 'BANKREPORT.TXT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUST-ACCOUNT-NUMBER    PIC 9(10).
           05 CUST-NAME             PIC X(30).
           05 CUST-ADDRESS          PIC X(50).
           05 CUST-PHONE            PIC X(15).
           05 CUST-BALANCE          PIC 9(10)V99.
           05 CUST-ACCOUNT-TYPE     PIC X(1).
           05 CUST-STATUS           PIC X(1).
           05 CUST-DATE-OPENED      PIC 9(8).
           05 FILLER                PIC X(20).
       
       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           05 TRANS-ID              PIC 9(10).
           05 TRANS-ACCOUNT-NUMBER  PIC 9(10).
           05 TRANS-TYPE            PIC X(1).
           05 TRANS-AMOUNT          PIC 9(10)V99.
           05 TRANS-DATE            PIC 9(8).
           05 TRANS-TIME            PIC 9(6).
           05 TRANS-DESCRIPTION     PIC X(30).
           05 FILLER                PIC X(20).
       
       FD REPORT-FILE.
       01 REPORT-LIN               PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 FILE-STATUS-VARIABLES.
           05 CUSTOMER-FILE-STATUS  PIC XX.
           05 TRANSACTION-FILE-STATUS PIC XX.
           05 REPORT-FILE-STATUS    PIC XX.
       
       01 SYSTEM-VARIABLES.
           05 CURRENT-DATE          PIC 9(8).
           05 CURRENT-TIME          PIC 9(6).
           05 MENU-CHOICE           PIC 9(1).
           05 PROGRAM-STATUS        PIC X(1).
               88 CONTINUE-PROGRAM  VALUE 'Y'.
               88 EXIT-PROGRAM      VALUE 'N'.
       
       01 CUSTOMER-WORK-AREA.
           05 WS-ACCOUNT-NUMBER     PIC 9(10).
           05 WS-CUSTOMER-NAME      PIC X(30).
           05 WS-BALANCE            PIC 9(10)V99.
           05 WS-AMOUNT             PIC 9(10)V99.
           05 WS-NEWBAL         PIC 9(10)V99.
       
       01 TRANSACTION-WORK-AREA.
           05 WS-TRANS-ID           PIC 9(10) VALUE 1.
           05 WS-TRANS-TYPE         PIC X(1).
           05 WS-TRANS-AMOUNT       PIC 9(10)V99.
           05 WS-TRANS-DESC         PIC X(30).
       
       01 DISPLAY-VARIABLES.
           05 WS-DISPLAY-BALANCE    PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-AMOUNT     PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-NEWBAL PIC $ZZZ,ZZZ,ZZ9.99.
       
       01 ERROR-MESSAGES.
           05 ERR-INVALID-ACCOUNT   PIC X(50) VALUE 
              'ERROR: Account number not found'.
           05 ERR-INSUFFICIENT-FUNDS PIC X(50) VALUE 
              'ERROR: Insufficient funds for transaction'.
           05 ERR-INVALID-AMOUNT    PIC X(50) VALUE 
              'ERROR: Invalid amount entered'.
           05 ERR-FILE-ERROR        PIC X(50) VALUE 
              'ERROR: File operation failed'.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-PROGRAM
           PERFORM MAIN-MENU-LOOP UNTIL EXIT-PROGRAM
           PERFORM CLOSE-FILES
           STOP RUN.
       
       INITIALIZE-PROGRAM.
           OPEN INPUT CUSTOMER-FILE
           OPEN OUTPUT TRANSACTION-FILE
           OPEN OUTPUT REPORT-FILE
           
           ACCEPT CURRENT-DATE FROM DATE
           ACCEPT CURRENT-TIME FROM TIME
           
           DISPLAY '========================================'
           DISPLAY '    WELCOME TO COBOL BANKING SYSTEM    '
           DISPLAY '========================================'
           DISPLAY ' '
           
           SET CONTINUE-PROGRAM TO TRUE.
       
       MAIN-MENU-LOOP.
           PERFORM DISPLAY-MAIN-MENU
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM CUSTOMER-INQUIRY
               WHEN 2 PERFORM DEPOSIT-TRANSACTION
               WHEN 3 PERFORM WITHDRAWAL-TRANSACTION
               WHEN 4 PERFORM TRANSFER-TRANSACTION
               WHEN 5 PERFORM GENERATE-REPORTS
               WHEN 6 PERFORM ADD-NEW-CUSTOMER
               WHEN 9 SET EXIT-PROGRAM TO TRUE
               WHEN OTHER DISPLAY 'Invalid choice. Please try again.'
           END-EVALUATE.
       
       DISPLAY-MAIN-MENU.
           DISPLAY ' '
           DISPLAY 'MAIN MENU:'
           DISPLAY '1. Customer Account Inquiry'
           DISPLAY '2. Deposit Money'
           DISPLAY '3. Withdraw Money'
           DISPLAY '4. Transfer Money'
           DISPLAY '5. Generate Reports'
           DISPLAY '6. Add New Customer'
           DISPLAY '9. Exit System'
           DISPLAY ' '
           DISPLAY 'Enter your choice (1-6, 9): '.
       
       CUSTOMER-INQUIRY.
           DISPLAY ' '
           DISPLAY 'CUSTOMER ACCOUNT INQUIRY'
           DISPLAY '========================'
           DISPLAY 'Enter account number: '
           ACCEPT WS-ACCOUNT-NUMBER
           
           PERFORM READ-CUSTOMER-RECORD
           IF CUSTOMER-FILE-STATUS = '00'
               PERFORM DISPLAY-CUSTOMER-INFO
           ELSE
               DISPLAY ERR-INVALID-ACCOUNT
           END-IF.
       
       DEPOSIT-TRANSACTION.
           DISPLAY ' '
           DISPLAY 'DEPOSIT TRANSACTION'
           DISPLAY '=================='
           PERFORM GET-ACCOUNT-AND-AMOUNT
           
           IF CUSTOMER-FILE-STATUS = '00'
               ADD WS-AMOUNT TO CUST-BALANCE GIVING WS-NEWBAL
               PERFORM UPDATE-CUSTOMER-BALANCE
               PERFORM RECORD-TRANSACTION
               MOVE 'D' TO WS-TRANS-TYPE
               MOVE 'Deposit Transaction' TO WS-TRANS-DESC
               PERFORM DISPLAY-TRANSACTION-RESULT
           ELSE
               DISPLAY ERR-INVALID-ACCOUNT
           END-IF.
       
       WITHDRAWAL-TRANSACTION.
           DISPLAY ' '
           DISPLAY 'WITHDRAWAL TRANSACTION'
           DISPLAY '====================='
           PERFORM GET-ACCOUNT-AND-AMOUNT
           
           IF CUSTOMER-FILE-STATUS = '00'
               IF WS-AMOUNT <= CUST-BALANCE
                   SUBTRACT WS-AMOUNT FROM CUST-BALANCE GIVING WS-NEWBAL
                   PERFORM UPDATE-CUSTOMER-BALANCE
                   PERFORM RECORD-TRANSACTION
                   MOVE 'W' TO WS-TRANS-TYPE
                   MOVE 'Withdrawal Transaction' TO WS-TRANS-DESC
                   PERFORM DISPLAY-TRANSACTION-RESULT
               ELSE
                   DISPLAY ERR-INSUFFICIENT-FUNDS
               END-IF
           ELSE
               DISPLAY ERR-INVALID-ACCOUNT
           END-IF.
       
       TRANSFER-TRANSACTION.
           DISPLAY ' '
           DISPLAY 'TRANSFER TRANSACTION'
           DISPLAY '==================='
           DISPLAY 'Enter source account number: '
           ACCEPT WS-ACCOUNT-NUMBER
           
           PERFORM READ-CUSTOMER-RECORD
           IF CUSTOMER-FILE-STATUS = '00'
               DISPLAY 'Enter transfer amount: '
               ACCEPT WS-AMOUNT
               
               IF WS-AMOUNT <= WS-BALANCE AND WS-AMOUNT > 0
                   DISPLAY 'Enter destination account number: '
                   ACCEPT WS-ACCOUNT-NUMBER
                   
                   PERFORM READ-CUSTOMER-RECORD
                   IF CUSTOMER-FILE-STATUS = '00'
                       ADD WS-AMOUNT TO WS-BALANCE GIVING WS-NEWBAL
                       PERFORM UPDATE-CUSTOMER-BALANCE
                       PERFORM RECORD-TRANSACTION
                       MOVE 'T' TO WS-TRANS-TYPE
                       MOVE 'Transfer Transaction' TO WS-TRANS-DESC
                       PERFORM DISPLAY-TRANSACTION-RESULT
                   ELSE
                       DISPLAY ERR-INVALID-ACCOUNT
                   END-IF
               ELSE
                   DISPLAY ERR-INSUFFICIENT-FUNDS
               END-IF
           ELSE
               DISPLAY ERR-INVALID-ACCOUNT
           END-IF.
       
       ADD-NEW-CUSTOMER.
           DISPLAY ' '
           DISPLAY 'ADD NEW CUSTOMER'
           DISPLAY '================'
           DISPLAY 'Enter account number: '
           ACCEPT WS-ACCOUNT-NUMBER
           DISPLAY 'Enter customer name: '
           ACCEPT WS-CUSTOMER-NAME
           DISPLAY 'Enter initial balance: '
           ACCEPT WS-BALANCE
           
           MOVE WS-ACCOUNT-NUMBER TO CUST-ACCOUNT-NUMBER
           MOVE WS-CUSTOMER-NAME TO CUST-NAME
           MOVE WS-BALANCE TO CUST-BALANCE
           MOVE 'A' TO CUST-STATUS
           MOVE 'S' TO CUST-ACCOUNT-TYPE
           MOVE CURRENT-DATE TO CUST-DATE-OPENED
           MOVE SPACES TO CUST-ADDRESS
           MOVE SPACES TO CUST-PHONE
           
           WRITE CUSTOMER-RECORD
           IF CUSTOMER-FILE-STATUS = '00'
               DISPLAY 'Customer added successfully!'
           ELSE
               DISPLAY ERR-FILE-ERROR
           END-IF.
       
       GENERATE-REPORTS.
           DISPLAY ' '
           DISPLAY 'GENERATING BANK REPORTS...'
           PERFORM WRITE-REPORT-HEADER
           PERFORM WRITE-CUSTOMER-LISTING
           PERFORM WRITE-TRANSACTION-SUMMARY
           DISPLAY 'Reports generated successfully!'.
       
       GET-ACCOUNT-AND-AMOUNT.
           DISPLAY 'Enter account number: '
           ACCEPT WS-ACCOUNT-NUMBER
           DISPLAY 'Enter amount: '
           ACCEPT WS-AMOUNT
           
           IF WS-AMOUNT <= 0
               DISPLAY ERR-INVALID-AMOUNT
               MOVE 0 TO WS-AMOUNT
           END-IF.
       
       READ-CUSTOMER-RECORD.
           MOVE WS-ACCOUNT-NUMBER TO CUST-ACCOUNT-NUMBER
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE '99' TO CUSTOMER-FILE-STATUS
           END-READ.
       
       UPDATE-CUSTOMER-BALANCE.
           MOVE WS-NEWBAL TO CUST-BALANCE
           REWRITE CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY ERR-FILE-ERROR
           END-REWRITE.
       
       RECORD-TRANSACTION.
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE WS-ACCOUNT-NUMBER TO TRANS-ACCOUNT-NUMBER
           MOVE WS-TRANS-TYPE TO TRANS-TYPE
           MOVE WS-AMOUNT TO TRANS-AMOUNT
           MOVE CURRENT-DATE TO TRANS-DATE
           MOVE CURRENT-TIME TO TRANS-TIME
           MOVE WS-TRANS-DESC TO TRANS-DESCRIPTION
           
           WRITE TRANSACTION-RECORD
           ADD 1 TO WS-TRANS-ID.
       
       DISPLAY-CUSTOMER-INFO.
           MOVE CUST-BALANCE TO WS-DISPLAY-BALANCE
           DISPLAY ' '
           DISPLAY 'CUSTOMER INFORMATION'
           DISPLAY '==================='
           DISPLAY 'Account Number: ' CUST-ACCOUNT-NUMBER
           DISPLAY 'Customer Name: ' CUST-NAME
           DISPLAY 'Current Balance: ' WS-DISPLAY-BALANCE
           DISPLAY 'Account Type: ' CUST-ACCOUNT-TYPE
           DISPLAY 'Status: ' CUST-STATUS.
       
       DISPLAY-TRANSACTION-RESULT.
           MOVE WS-AMOUNT TO WS-DISPLAY-AMOUNT
           MOVE WS-NEWBAL TO WS-DISPLAY-NEWBAL
           DISPLAY ' '
           DISPLAY 'TRANSACTION COMPLETED'
           DISPLAY '===================='
           DISPLAY 'Transaction Amount: ' WS-DISPLAY-AMOUNT
           DISPLAY 'New Balance: ' WS-DISPLAY-NEWBAL.
       
       WRITE-REPORT-HEADER.
           MOVE 'COBOL BANKING SYSTEM - REPORT' TO REPORT-LIN
           WRITE REPORT-LIN
           MOVE 'Generated on: ' TO REPORT-LIN
           WRITE REPORT-LIN
           MOVE '========================================' TO REPORT-LIN
           WRITE REPORT-LIN.
       
       WRITE-CUSTOMER-LISTING.
           MOVE 'CUSTOMER LISTING' TO REPORT-LIN
           WRITE REPORT-LIN
           MOVE '================' TO REPORT-LIN
           WRITE REPORT-LIN.
       
       WRITE-TRANSACTION-SUMMARY.
           MOVE 'TRANSACTION SUMMARY' TO REPORT-LIN
           WRITE REPORT-LIN
           MOVE '===================' TO REPORT-LIN
           WRITE REPORT-LIN.
       
       CLOSE-FILES.
           CLOSE CUSTOMER-FILE
           CLOSE TRANSACTION-FILE
           CLOSE REPORT-FILE
           DISPLAY ' '
           DISPLAY 'Thank you for using COBOL Banking System!'
