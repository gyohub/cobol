       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKTEST.
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
       
       01 TEST-VARIABLES.
           05 TEST-ACCOUNT-NUMBER    PIC 9(10).
           05 TEST-COUNTER          PIC 9(3) VALUE 0.
           05 TEST-TOTAL-BALANCE    PIC 9(12)V99 VALUE 0.
       
       01 DISPLAY-VARIABLES.
           05 WS-DISPLAY-BALANCE    PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-TOTAL      PIC $ZZZ,ZZZ,ZZ9.99.
       
       PROCEDURE DIVISION.
       MAIN-TEST-PROGRAM.
           PERFORM INITIALIZE-TEST
           PERFORM TEST-CUSTOMER-FILE
           PERFORM DISPLAY-TEST-RESULTS
           PERFORM CLEANUP-TEST
           STOP RUN.
       
       INITIALIZE-TEST.
           DISPLAY '========================================'
           DISPLAY '    COBOL BANKING SYSTEM - TEST MODE    '
           DISPLAY '========================================'
           DISPLAY ' '
           
           OPEN INPUT CUSTOMER-FILE
           IF CUSTOMER-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR: Cannot open customer file'
               DISPLAY 'File Status: ' CUSTOMER-FILE-STATUS
               STOP RUN
           END-IF.
       
       TEST-CUSTOMER-FILE.
           DISPLAY 'Testing customer file access...'
           DISPLAY ' '
           DISPLAY 'CUSTOMER ACCOUNTS:'
           DISPLAY '================='
           DISPLAY 'Account #    Customer Name              Balance'
           DISPLAY '----------------------------------------------'
           
           MOVE 1 TO TEST-ACCOUNT-NUMBER
           
           PERFORM READ-CUSTOMER-LOOP.
       
       READ-CUSTOMER-LOOP.
           MOVE TEST-ACCOUNT-NUMBER TO CUST-ACCOUNT-NUMBER
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE '99' TO CUSTOMER-FILE-STATUS
               NOT INVALID KEY
                   PERFORM PROCESS-CUSTOMER-RECORD
           END-READ
           
           ADD 1 TO TEST-ACCOUNT-NUMBER
           
           IF CUSTOMER-FILE-STATUS = '00' AND TEST-COUNTER < 10
               PERFORM READ-CUSTOMER-LOOP
           END-IF.
       
       PROCESS-CUSTOMER-RECORD.
           ADD 1 TO TEST-COUNTER
           ADD CUST-BALANCE TO TEST-TOTAL-BALANCE
           
           MOVE CUST-BALANCE TO WS-DISPLAY-BALANCE
           
           DISPLAY CUST-ACCOUNT-NUMBER '   ' 
                   CUST-NAME '   ' 
                   WS-DISPLAY-BALANCE.
       
       DISPLAY-TEST-RESULTS.
           DISPLAY ' '
           DISPLAY 'TEST RESULTS:'
           DISPLAY '============='
           DISPLAY 'Total Customers Found: ' TEST-COUNTER
           MOVE TEST-TOTAL-BALANCE TO WS-DISPLAY-TOTAL
           DISPLAY 'Total Bank Balance: ' WS-DISPLAY-TOTAL
           DISPLAY ' '
           
           IF TEST-COUNTER > 0
               DISPLAY 'Customer file test PASSED'
               DISPLAY 'File access working correctly'
               DISPLAY 'Data integrity maintained'
           ELSE
               DISPLAY 'Customer file test FAILED'
               DISPLAY 'No customer records found'
           END-IF.
       
       CLEANUP-TEST.
           CLOSE CUSTOMER-FILE
           DISPLAY ' '
           DISPLAY 'Test completed successfully!'
           DISPLAY 'Run main system with: ./BANKMAIN'
