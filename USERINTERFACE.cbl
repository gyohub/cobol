       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERINTERFACE.
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
       
       WORKING-STORAGE SECTION.
       01 FILE-STATUS-VARIABLES.
           05 CUSTOMER-FILE-STATUS  PIC XX.
       
       01 USER-INTERFACE-VARIABLES.
           05 UI-MENU-CHOICE        PIC 9(1).
           05 UI-ACCOUNT-NUMBER     PIC 9(10).
           05 UI-CUSTOMER-NAME      PIC X(30).
           05 UI-ADDRESS            PIC X(50).
           05 UI-PHONE              PIC X(15).
           05 UI-INITIAL-BALANCE    PIC 9(10)V99.
           05 UI-ACCOUNT-TYPE       PIC X(1).
           05 UI-AMOUNT             PIC 9(10)V99.
           05 UI-DESTINATION-ACCOUNT PIC 9(10).
           05 UI-CONTINUE-FLAG      PIC X(1).
               88 CONTINUE-PROGRAM  VALUE 'Y'.
               88 EXIT-PROGRAM      VALUE 'N'.
       
       01 DISPLAY-VARIABLES.
           05 WS-DISPLAY-BALANCE    PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-AMOUNT     PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-DATE       PIC X(10).
       
       01 SYSTEM-VARIABLES.
           05 CURRENT-DATE          PIC 9(8).
           05 CURRENT-TIME          PIC 9(6).
       
       01 VALIDATION-FLAGS.
           05 VALID-ACCOUNT         PIC X(1).
               88 ACCOUNT-VALID     VALUE 'Y'.
               88 ACCOUNT-INVALID   VALUE 'N'.
           05 VALID-AMOUNT          PIC X(1).
               88 AMOUNT-VALID      VALUE 'Y'.
               88 AMOUNT-INVALID    VALUE 'N'.
       
       01 ERROR-MESSAGES.
           05 ERR-INVALID-INPUT     PIC X(50) VALUE 
              'ERROR: Invalid input. Please try again.'.
           05 ERR-ACCOUNT-NOT-FOUND PIC X(50) VALUE 
              'ERROR: Account number not found.'.
           05 ERR-INSUFFICIENT-FUNDS PIC X(50) VALUE 
              'ERROR: Insufficient funds for transaction.'.
           05 ERR-INVALID-AMOUNT    PIC X(50) VALUE 
              'ERROR: Invalid amount. Must be greater than 0.'.
       
       PROCEDURE DIVISION.
       MAIN-USER-INTERFACE.
           PERFORM INITIALIZE-UI
           PERFORM MAIN-MENU-LOOP UNTIL EXIT-PROGRAM
           PERFORM CLEANUP-UI
           STOP RUN.
       
       INITIALIZE-UI.
           OPEN INPUT CUSTOMER-FILE
           ACCEPT CURRENT-DATE FROM DATE
           ACCEPT CURRENT-TIME FROM TIME
           
           DISPLAY '========================================'
           DISPLAY '    COBOL BANKING SYSTEM - MAIN MENU    '
           DISPLAY '========================================'
           DISPLAY ' '
           SET CONTINUE-PROGRAM TO TRUE.
       
       MAIN-MENU-LOOP.
           PERFORM DISPLAY-MAIN-MENU
           ACCEPT UI-MENU-CHOICE
           EVALUATE UI-MENU-CHOICE
               WHEN 1 PERFORM CUSTOMER-INQUIRY-MENU
               WHEN 2 PERFORM DEPOSIT-MENU
               WHEN 3 PERFORM WITHDRAWAL-MENU
               WHEN 4 PERFORM TRANSFER-MENU
               WHEN 5 PERFORM CUSTOMER-MANAGEMENT-MENU
               WHEN 6 PERFORM SYSTEM-REPORTS-MENU
               WHEN 9 PERFORM EXIT-CONFIRMATION
               WHEN OTHER PERFORM INVALID-CHOICE-MESSAGE
           END-EVALUATE.
       
       DISPLAY-MAIN-MENU.
           DISPLAY ' '
           DISPLAY 'MAIN MENU:'
           DISPLAY '1. Customer Account Inquiry'
           DISPLAY '2. Deposit Money'
           DISPLAY '3. Withdraw Money'
           DISPLAY '4. Transfer Money'
           DISPLAY '5. Customer Management'
           DISPLAY '6. System Reports'
           DISPLAY '9. Exit System'
           DISPLAY ' '
           DISPLAY 'Enter your choice (1-6, 9): ' WITH NO ADVANCING.
       
       CUSTOMER-INQUIRY-MENU.
           DISPLAY ' '
           DISPLAY 'CUSTOMER ACCOUNT INQUIRY'
           DISPLAY '========================'
           PERFORM GET-ACCOUNT-NUMBER
           
           IF ACCOUNT-VALID
               PERFORM READ-CUSTOMER-RECORD
               IF CUSTOMER-FILE-STATUS = '00'
                   PERFORM DISPLAY-CUSTOMER-DETAILS
               ELSE
                   DISPLAY ERR-ACCOUNT-NOT-FOUND
               END-IF
           END-IF
           
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       DEPOSIT-MENU.
           DISPLAY ' '
           DISPLAY 'DEPOSIT TRANSACTION'
           DISPLAY '=================='
           PERFORM GET-ACCOUNT-NUMBER
           
           IF ACCOUNT-VALID
               PERFORM GET-TRANSACTION-AMOUNT
               IF AMOUNT-VALID
                   PERFORM READ-CUSTOMER-RECORD
                   IF CUSTOMER-FILE-STATUS = '00'
                       PERFORM PROCESS-DEPOSIT
                   ELSE
                       DISPLAY ERR-ACCOUNT-NOT-FOUND
                   END-IF
               END-IF
           END-IF
           
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       WITHDRAWAL-MENU.
           DISPLAY ' '
           DISPLAY 'WITHDRAWAL TRANSACTION'
           DISPLAY '====================='
           PERFORM GET-ACCOUNT-NUMBER
           
           IF ACCOUNT-VALID
               PERFORM GET-TRANSACTION-AMOUNT
               IF AMOUNT-VALID
                   PERFORM READ-CUSTOMER-RECORD
                   IF CUSTOMER-FILE-STATUS = '00'
                       PERFORM PROCESS-WITHDRAWAL
                   ELSE
                       DISPLAY ERR-ACCOUNT-NOT-FOUND
                   END-IF
               END-IF
           END-IF
           
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       TRANSFER-MENU.
           DISPLAY ' '
           DISPLAY 'TRANSFER TRANSACTION'
           DISPLAY '==================='
           DISPLAY 'Source Account:'
           PERFORM GET-ACCOUNT-NUMBER
           
           IF ACCOUNT-VALID
               PERFORM GET-TRANSACTION-AMOUNT
               IF AMOUNT-VALID
                   PERFORM READ-CUSTOMER-RECORD
                   IF CUSTOMER-FILE-STATUS = '00'
                       DISPLAY 'Destination Account:'
                       MOVE UI-ACCOUNT-NUMBER TO UI-DESTINATION-ACCOUNT
                       PERFORM GET-ACCOUNT-NUMBER
                       
                       IF ACCOUNT-VALID
                           PERFORM PROCESS-TRANSFER
                       END-IF
                   ELSE
                       DISPLAY ERR-ACCOUNT-NOT-FOUND
                   END-IF
               END-IF
           END-IF
           
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       CUSTOMER-MANAGEMENT-MENU.
           DISPLAY ' '
           DISPLAY 'CUSTOMER MANAGEMENT'
           DISPLAY '=================='
           DISPLAY '1. Add New Customer'
           DISPLAY '2. Update Customer Information'
           DISPLAY '3. Deactivate Customer Account'
           DISPLAY '4. Back to Main Menu'
           DISPLAY ' '
           DISPLAY 'Enter your choice (1-4): ' WITH NO ADVANCING
           ACCEPT UI-MENU-CHOICE
           
           EVALUATE UI-MENU-CHOICE
               WHEN 1 PERFORM ADD-NEW-CUSTOMER-MENU
               WHEN 2 PERFORM UPDATE-CUSTOMER-MENU
               WHEN 3 PERFORM DEACTIVATE-CUSTOMER-MENU
               WHEN 4 
                   CONTINUE
               WHEN OTHER PERFORM INVALID-CHOICE-MESSAGE
           END-EVALUATE.
       
       ADD-NEW-CUSTOMER-MENU.
           DISPLAY ' '
           DISPLAY 'ADD NEW CUSTOMER'
           DISPLAY '================'
           PERFORM GET-CUSTOMER-DETAILS
           PERFORM CREATE-NEW-CUSTOMER
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       UPDATE-CUSTOMER-MENU.
           DISPLAY ' '
           DISPLAY 'UPDATE CUSTOMER INFORMATION'
           DISPLAY '==========================='
           PERFORM GET-ACCOUNT-NUMBER
           
           IF ACCOUNT-VALID
               PERFORM READ-CUSTOMER-RECORD
               IF CUSTOMER-FILE-STATUS = '00'
                   PERFORM DISPLAY-CUSTOMER-DETAILS
                   PERFORM GET-UPDATE-DETAILS
                   PERFORM UPDATE-CUSTOMER-RECORD
               ELSE
                   DISPLAY ERR-ACCOUNT-NOT-FOUND
               END-IF
           END-IF
           
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       DEACTIVATE-CUSTOMER-MENU.
           DISPLAY ' '
           DISPLAY 'DEACTIVATE CUSTOMER ACCOUNT'
           DISPLAY '==========================='
           PERFORM GET-ACCOUNT-NUMBER
           
           IF ACCOUNT-VALID
               PERFORM READ-CUSTOMER-RECORD
               IF CUSTOMER-FILE-STATUS = '00'
                   PERFORM DISPLAY-CUSTOMER-DETAILS
                   DISPLAY ' '
                   DISPLAY 'Are you sure you want to deactivate this account? (Y/N): ' 
                           WITH NO ADVANCING
                   ACCEPT UI-CONTINUE-FLAG
                   
                   IF UI-CONTINUE-FLAG = 'Y' OR UI-CONTINUE-FLAG = 'y'
                       MOVE 'I' TO CUST-STATUS
                       PERFORM UPDATE-CUSTOMER-RECORD
                       DISPLAY 'Account deactivated successfully!'
                   ELSE
                       DISPLAY 'Account deactivation cancelled.'
                   END-IF
               ELSE
                   DISPLAY ERR-ACCOUNT-NOT-FOUND
               END-IF
           END-IF
           
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       SYSTEM-REPORTS-MENU.
           DISPLAY ' '
           DISPLAY 'SYSTEM REPORTS'
           DISPLAY '=============='
           DISPLAY '1. Generate Customer Report'
           DISPLAY '2. Generate Transaction Report'
           DISPLAY '3. Generate Summary Report'
           DISPLAY '4. Back to Main Menu'
           DISPLAY ' '
           DISPLAY 'Enter your choice (1-4): ' WITH NO ADVANCING
           ACCEPT UI-MENU-CHOICE
           
           EVALUATE UI-MENU-CHOICE
               WHEN 1 PERFORM GENERATE-CUSTOMER-REPORT
               WHEN 2 PERFORM GENERATE-TRANSACTION-REPORT
               WHEN 3 PERFORM GENERATE-SUMMARY-REPORT
               WHEN 4 
                   CONTINUE
               WHEN OTHER PERFORM INVALID-CHOICE-MESSAGE
           END-EVALUATE.
       
       GET-ACCOUNT-NUMBER.
           DISPLAY 'Enter account number: ' WITH NO ADVANCING
           ACCEPT UI-ACCOUNT-NUMBER
           
           IF UI-ACCOUNT-NUMBER > 0
               SET ACCOUNT-VALID TO TRUE
           ELSE
               SET ACCOUNT-INVALID TO TRUE
               DISPLAY ERR-INVALID-INPUT
           END-IF.
       
       GET-TRANSACTION-AMOUNT.
           DISPLAY 'Enter amount: ' WITH NO ADVANCING
           ACCEPT UI-AMOUNT
           
           IF UI-AMOUNT > 0
               SET AMOUNT-VALID TO TRUE
           ELSE
               SET AMOUNT-INVALID TO TRUE
               DISPLAY ERR-INVALID-AMOUNT
           END-IF.
       
       GET-CUSTOMER-DETAILS.
           DISPLAY 'Enter account number: ' WITH NO ADVANCING
           ACCEPT UI-ACCOUNT-NUMBER
           DISPLAY 'Enter customer name: ' WITH NO ADVANCING
           ACCEPT UI-CUSTOMER-NAME
           DISPLAY 'Enter address: ' WITH NO ADVANCING
           ACCEPT UI-ADDRESS
           DISPLAY 'Enter phone number: ' WITH NO ADVANCING
           ACCEPT UI-PHONE
           DISPLAY 'Enter initial balance: ' WITH NO ADVANCING
           ACCEPT UI-INITIAL-BALANCE
           DISPLAY 'Enter account type (S=Savings, C=Checking): ' WITH NO ADVANCING
           ACCEPT UI-ACCOUNT-TYPE.
       
       READ-CUSTOMER-RECORD.
           MOVE UI-ACCOUNT-NUMBER TO CUST-ACCOUNT-NUMBER
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE '99' TO CUSTOMER-FILE-STATUS
           END-READ.
       
       DISPLAY-CUSTOMER-DETAILS.
           MOVE CUST-BALANCE TO WS-DISPLAY-BALANCE
           STRING CUST-DATE-OPENED(1:2) '/' 
                  CUST-DATE-OPENED(3:2) '/' 
                  CUST-DATE-OPENED(5:4)
                  INTO WS-DISPLAY-DATE
           END-STRING
           
           DISPLAY ' '
           DISPLAY 'CUSTOMER INFORMATION'
           DISPLAY '==================='
           DISPLAY 'Account Number: ' CUST-ACCOUNT-NUMBER
           DISPLAY 'Customer Name: ' CUST-NAME
           DISPLAY 'Address: ' CUST-ADDRESS
           DISPLAY 'Phone: ' CUST-PHONE
           DISPLAY 'Current Balance: ' WS-DISPLAY-BALANCE
           DISPLAY 'Account Type: ' CUST-ACCOUNT-TYPE
           DISPLAY 'Status: ' CUST-STATUS
           DISPLAY 'Date Opened: ' WS-DISPLAY-DATE.
       
       PROCESS-DEPOSIT.
           ADD UI-AMOUNT TO CUST-BALANCE
           PERFORM UPDATE-CUSTOMER-RECORD
           MOVE CUST-BALANCE TO WS-DISPLAY-BALANCE
           MOVE UI-AMOUNT TO WS-DISPLAY-AMOUNT
           
           DISPLAY ' '
           DISPLAY 'DEPOSIT COMPLETED SUCCESSFULLY!'
           DISPLAY 'Deposit Amount: ' WS-DISPLAY-AMOUNT
           DISPLAY 'New Balance: ' WS-DISPLAY-BALANCE.
       
       PROCESS-WITHDRAWAL.
           IF UI-AMOUNT <= CUST-BALANCE
               SUBTRACT UI-AMOUNT FROM CUST-BALANCE
               PERFORM UPDATE-CUSTOMER-RECORD
               MOVE CUST-BALANCE TO WS-DISPLAY-BALANCE
               MOVE UI-AMOUNT TO WS-DISPLAY-AMOUNT
               
               DISPLAY ' '
               DISPLAY 'WITHDRAWAL COMPLETED SUCCESSFULLY!'
               DISPLAY 'Withdrawal Amount: ' WS-DISPLAY-AMOUNT
               DISPLAY 'New Balance: ' WS-DISPLAY-BALANCE
           ELSE
               DISPLAY ERR-INSUFFICIENT-FUNDS
           END-IF.
       
       PROCESS-TRANSFER.
           IF UI-AMOUNT <= CUST-BALANCE
               SUBTRACT UI-AMOUNT FROM CUST-BALANCE
               PERFORM UPDATE-CUSTOMER-RECORD
               
               MOVE UI-DESTINATION-ACCOUNT TO CUST-ACCOUNT-NUMBER
               READ CUSTOMER-FILE
                   INVALID KEY
                       DISPLAY 'Destination account not found!'
                       EXIT PARAGRAPH
               END-READ
               
               ADD UI-AMOUNT TO CUST-BALANCE
               PERFORM UPDATE-CUSTOMER-RECORD
               
               MOVE UI-AMOUNT TO WS-DISPLAY-AMOUNT
               
               DISPLAY ' '
               DISPLAY 'TRANSFER COMPLETED SUCCESSFULLY!'
               DISPLAY 'Transfer Amount: ' WS-DISPLAY-AMOUNT
               DISPLAY 'From Account: ' UI-ACCOUNT-NUMBER
               DISPLAY 'To Account: ' UI-DESTINATION-ACCOUNT
           ELSE
               DISPLAY ERR-INSUFFICIENT-FUNDS
           END-IF.
       
       CREATE-NEW-CUSTOMER.
           MOVE UI-ACCOUNT-NUMBER TO CUST-ACCOUNT-NUMBER
           MOVE UI-CUSTOMER-NAME TO CUST-NAME
           MOVE UI-ADDRESS TO CUST-ADDRESS
           MOVE UI-PHONE TO CUST-PHONE
           MOVE UI-INITIAL-BALANCE TO CUST-BALANCE
           MOVE UI-ACCOUNT-TYPE TO CUST-ACCOUNT-TYPE
           MOVE 'A' TO CUST-STATUS
           MOVE CURRENT-DATE TO CUST-DATE-OPENED
           
           WRITE CUSTOMER-RECORD
           IF CUSTOMER-FILE-STATUS = '00'
               DISPLAY 'Customer added successfully!'
           ELSE
               DISPLAY 'Error adding customer. Account may already exist.'
           END-IF.
       
       UPDATE-CUSTOMER-RECORD.
           REWRITE CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY 'Error updating customer record.'
           END-REWRITE.
       
       GET-UPDATE-DETAILS.
           DISPLAY ' '
           DISPLAY 'Enter new customer name (or press Enter to keep current): ' 
                   WITH NO ADVANCING
           ACCEPT UI-CUSTOMER-NAME
           IF UI-CUSTOMER-NAME NOT = SPACES
               MOVE UI-CUSTOMER-NAME TO CUST-NAME
           END-IF
           
           DISPLAY 'Enter new address (or press Enter to keep current): ' 
                   WITH NO ADVANCING
           ACCEPT UI-ADDRESS
           IF UI-ADDRESS NOT = SPACES
               MOVE UI-ADDRESS TO CUST-ADDRESS
           END-IF
           
           DISPLAY 'Enter new phone number (or press Enter to keep current): ' 
                   WITH NO ADVANCING
           ACCEPT UI-PHONE
           IF UI-PHONE NOT = SPACES
               MOVE UI-PHONE TO CUST-PHONE
           END-IF.
       
       GENERATE-CUSTOMER-REPORT.
           DISPLAY 'Generating customer report...'
           CALL 'REPORTGEN' USING 'CUSTOMER'
           DISPLAY 'Customer report generated successfully!'
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       GENERATE-TRANSACTION-REPORT.
           DISPLAY 'Generating transaction report...'
           CALL 'REPORTGEN' USING 'TRANSACTION'
           DISPLAY 'Transaction report generated successfully!'
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       GENERATE-SUMMARY-REPORT.
           DISPLAY 'Generating summary report...'
           CALL 'REPORTGEN' USING 'SUMMARY'
           DISPLAY 'Summary report generated successfully!'
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       EXIT-CONFIRMATION.
           DISPLAY ' '
           DISPLAY 'Are you sure you want to exit? (Y/N): ' WITH NO ADVANCING
           ACCEPT UI-CONTINUE-FLAG
           
           IF UI-CONTINUE-FLAG = 'Y' OR UI-CONTINUE-FLAG = 'y'
               SET EXIT-PROGRAM TO TRUE
           END-IF.
       
       INVALID-CHOICE-MESSAGE.
           DISPLAY ' '
           DISPLAY 'Invalid choice. Please enter a valid option.'
           PERFORM PRESS-ENTER-TO-CONTINUE.
       
       PRESS-ENTER-TO-CONTINUE.
           DISPLAY ' '
           DISPLAY 'Press Enter to continue...'
           ACCEPT UI-CONTINUE-FLAG.
       
       CLEANUP-UI.
           CLOSE CUSTOMER-FILE
           DISPLAY ' '
           DISPLAY 'Thank you for using COBOL Banking System!'
           DISPLAY 'Goodbye!'.
