       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSPROC.
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
       
       WORKING-STORAGE SECTION.
       01 FILE-STATUS-VARIABLES.
           05 CUSTOMER-FILE-STATUS  PIC XX.
           05 TRANSACTION-FILE-STATUS PIC XX.
       
       01 TRANSACTION-PARAMETERS.
           05 TP-ACCOUNT-NUMBER     PIC 9(10).
           05 TP-TRANSACTION-TYPE   PIC X(1).
           05 TP-AMOUNT             PIC 9(10)V99.
           05 TP-DESTINATION-ACCOUNT PIC 9(10).
           05 TP-DESCRIPTION        PIC X(30).
           05 TP-TRANSACTION-ID     PIC 9(10).
       
       01 SYSTEM-VARIABLES.
           05 CURRENT-DATE          PIC 9(8).
           05 CURRENT-TIME          PIC 9(6).
           05 TRANSACTION-STATUS    PIC X(1).
               88 TRANSACTION-SUCCESS VALUE 'S'.
               88 TRANSACTION-FAILED VALUE 'F'.
       
       01 WORK-AREAS.
           05 WS-SOURCE-BALANCE     PIC 9(10)V99.
           05 WS-DEST-BALANCE       PIC 9(10)V99.
           05 WS-NEW-SOURCE-BALANCE PIC 9(10)V99.
           05 WS-NEW-DEST-BALANCE   PIC 9(10)V99.
           05 WS-CUSTOMER-NAME      PIC X(30).
       
       01 ERROR-CODES.
           05 ERR-ACCOUNT-NOT-FOUND PIC X(1) VALUE '1'.
           05 ERR-INSUFFICIENT-FUNDS PIC X(1) VALUE '2'.
           05 ERR-INVALID-AMOUNT    PIC X(1) VALUE '3'.
           05 ERR-FILE-ERROR        PIC X(1) VALUE '4'.
       
       LINKAGE SECTION.
       01 TRANSACTION-RESULT.
           05 TR-STATUS             PIC X(1).
           05 TR-ERROR-CODE         PIC X(1).
           05 TR-NEW-BALANCE        PIC 9(10)V99.
           05 TR-MESSAGE            PIC X(50).
       
       PROCEDURE DIVISION USING TRANSACTION-PARAMETERS TRANSACTION-RESULT.
       MAIN-TRANSACTION-PROCESSING.
           PERFORM INITIALIZE-TRANSACTION
           EVALUATE TP-TRANSACTION-TYPE
               WHEN 'D' PERFORM PROCESS-DEPOSIT
               WHEN 'W' PERFORM PROCESS-WITHDRAWAL
               WHEN 'T' PERFORM PROCESS-TRANSFER
               WHEN OTHER PERFORM INVALID-TRANSACTION-TYPE
           END-EVALUATE
           PERFORM FINALIZE-TRANSACTION.
       
       INITIALIZE-TRANSACTION.
           ACCEPT CURRENT-DATE FROM DATE
           ACCEPT CURRENT-TIME FROM TIME
           SET TRANSACTION-FAILED TO TRUE
           MOVE SPACES TO TR-MESSAGE.
       
       PROCESS-DEPOSIT.
           PERFORM READ-CUSTOMER-RECORD
           IF CUSTOMER-FILE-STATUS = '00'
               IF TP-AMOUNT > 0
                   ADD TP-AMOUNT TO CUST-BALANCE GIVING WS-NEW-SOURCE-BALANCE
                   MOVE WS-NEW-SOURCE-BALANCE TO CUST-BALANCE
                   PERFORM UPDATE-CUSTOMER-RECORD
                   IF CUSTOMER-FILE-STATUS = '00'
                       PERFORM RECORD-TRANSACTION-LOG
                       MOVE WS-NEW-SOURCE-BALANCE TO TR-NEW-BALANCE
                       SET TRANSACTION-SUCCESS TO TRUE
                       MOVE 'Deposit completed successfully' TO TR-MESSAGE
                   ELSE
                       MOVE ERR-FILE-ERROR TO TR-ERROR-CODE
                       MOVE 'Failed to update customer record' TO TR-MESSAGE
                   END-IF
               ELSE
                   MOVE ERR-INVALID-AMOUNT TO TR-ERROR-CODE
                   MOVE 'Invalid deposit amount' TO TR-MESSAGE
               END-IF
           ELSE
               MOVE ERR-ACCOUNT-NOT-FOUND TO TR-ERROR-CODE
               MOVE 'Account not found' TO TR-MESSAGE
           END-IF.
       
       PROCESS-WITHDRAWAL.
           PERFORM READ-CUSTOMER-RECORD
           IF CUSTOMER-FILE-STATUS = '00'
               IF TP-AMOUNT > 0
                   IF TP-AMOUNT <= CUST-BALANCE
                       SUBTRACT TP-AMOUNT FROM CUST-BALANCE 
                           GIVING WS-NEW-SOURCE-BALANCE
                       MOVE WS-NEW-SOURCE-BALANCE TO CUST-BALANCE
                       PERFORM UPDATE-CUSTOMER-RECORD
                       IF CUSTOMER-FILE-STATUS = '00'
                           PERFORM RECORD-TRANSACTION-LOG
                           MOVE WS-NEW-SOURCE-BALANCE TO TR-NEW-BALANCE
                           SET TRANSACTION-SUCCESS TO TRUE
                           MOVE 'Withdrawal completed successfully' TO TR-MESSAGE
                       ELSE
                           MOVE ERR-FILE-ERROR TO TR-ERROR-CODE
                           MOVE 'Failed to update customer record' TO TR-MESSAGE
                       END-IF
                   ELSE
                       MOVE ERR-INSUFFICIENT-FUNDS TO TR-ERROR-CODE
                       MOVE 'Insufficient funds for withdrawal' TO TR-MESSAGE
                   END-IF
               ELSE
                   MOVE ERR-INVALID-AMOUNT TO TR-ERROR-CODE
                   MOVE 'Invalid withdrawal amount' TO TR-MESSAGE
               END-IF
           ELSE
               MOVE ERR-ACCOUNT-NOT-FOUND TO TR-ERROR-CODE
               MOVE 'Account not found' TO TR-MESSAGE
           END-IF.
       
       PROCESS-TRANSFER.
           PERFORM READ-CUSTOMER-RECORD
           IF CUSTOMER-FILE-STATUS = '00'
               MOVE CUST-BALANCE TO WS-SOURCE-BALANCE
               MOVE CUST-NAME TO WS-CUSTOMER-NAME
               
               MOVE TP-DESTINATION-ACCOUNT TO CUST-ACCOUNT-NUMBER
               READ CUSTOMER-FILE
                   INVALID KEY
                       MOVE '99' TO CUSTOMER-FILE-STATUS
               END-READ
               
               IF CUSTOMER-FILE-STATUS = '00'
                   MOVE CUST-BALANCE TO WS-DEST-BALANCE
                   
                   IF TP-AMOUNT > 0 AND TP-AMOUNT <= WS-SOURCE-BALANCE
                       SUBTRACT TP-AMOUNT FROM WS-SOURCE-BALANCE 
                           GIVING WS-NEW-SOURCE-BALANCE
                       ADD TP-AMOUNT TO WS-DEST-BALANCE 
                           GIVING WS-NEW-DEST-BALANCE
                       
                       MOVE TP-ACCOUNT-NUMBER TO CUST-ACCOUNT-NUMBER
                       MOVE WS-NEW-SOURCE-BALANCE TO CUST-BALANCE
                       PERFORM UPDATE-CUSTOMER-RECORD
                       
                       IF CUSTOMER-FILE-STATUS = '00'
                           MOVE TP-DESTINATION-ACCOUNT TO CUST-ACCOUNT-NUMBER
                           MOVE WS-NEW-DEST-BALANCE TO CUST-BALANCE
                           PERFORM UPDATE-CUSTOMER-RECORD
                           
                           IF CUSTOMER-FILE-STATUS = '00'
                               PERFORM RECORD-TRANSFER-LOG
                               MOVE WS-NEW-SOURCE-BALANCE TO TR-NEW-BALANCE
                               SET TRANSACTION-SUCCESS TO TRUE
                               MOVE 'Transfer completed successfully' TO TR-MESSAGE
                           ELSE
                               MOVE ERR-FILE-ERROR TO TR-ERROR-CODE
                               MOVE 'Failed to update destination account' TO TR-MESSAGE
                           END-IF
                       ELSE
                           MOVE ERR-FILE-ERROR TO TR-ERROR-CODE
                           MOVE 'Failed to update source account' TO TR-MESSAGE
                       END-IF
                   ELSE
                       MOVE ERR-INSUFFICIENT-FUNDS TO TR-ERROR-CODE
                       MOVE 'Insufficient funds for transfer' TO TR-MESSAGE
                   END-IF
               ELSE
                   MOVE ERR-ACCOUNT-NOT-FOUND TO TR-ERROR-CODE
                   MOVE 'Destination account not found' TO TR-MESSAGE
               END-IF
           ELSE
               MOVE ERR-ACCOUNT-NOT-FOUND TO TR-ERROR-CODE
               MOVE 'Source account not found' TO TR-MESSAGE
           END-IF.
       
       INVALID-TRANSACTION-TYPE.
           MOVE '9' TO TR-ERROR-CODE
           MOVE 'Invalid transaction type' TO TR-MESSAGE.
       
       READ-CUSTOMER-RECORD.
           MOVE TP-ACCOUNT-NUMBER TO CUST-ACCOUNT-NUMBER
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE '99' TO CUSTOMER-FILE-STATUS
           END-READ.
       
       UPDATE-CUSTOMER-RECORD.
           REWRITE CUSTOMER-RECORD
               INVALID KEY
                   MOVE '99' TO CUSTOMER-FILE-STATUS
           END-REWRITE.
       
       RECORD-TRANSACTION-LOG.
           MOVE TP-TRANSACTION-ID TO TRANS-ID
           MOVE TP-ACCOUNT-NUMBER TO TRANS-ACCOUNT-NUMBER
           MOVE TP-TRANSACTION-TYPE TO TRANS-TYPE
           MOVE TP-AMOUNT TO TRANS-AMOUNT
           MOVE CURRENT-DATE TO TRANS-DATE
           MOVE CURRENT-TIME TO TRANS-TIME
           MOVE TP-DESCRIPTION TO TRANS-DESCRIPTION
           
           WRITE TRANSACTION-RECORD.
       
       RECORD-TRANSFER-LOG.
           PERFORM RECORD-TRANSACTION-LOG
           
           MOVE TP-DESTINATION-ACCOUNT TO TRANS-ACCOUNT-NUMBER
           MOVE 'D' TO TRANS-TYPE
           MOVE 'Transfer Credit' TO TRANS-DESCRIPTION
           
           WRITE TRANSACTION-RECORD.
       
       FINALIZE-TRANSACTION.
           IF TRANSACTION-SUCCESS
               MOVE 'S' TO TR-STATUS
           ELSE
               MOVE 'F' TO TR-STATUS
           END-IF.

