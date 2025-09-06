       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTGEN.
       AUTHOR. COBOL Banking System.
       DATE-WRITTEN. 2024.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
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
       01 REPORT-LINE               PIC X(132).
       
       WORKING-STORAGE SECTION.
       01 FILE-STATUS-VARIABLES.
           05 CUSTOMER-FILE-STATUS  PIC XX.
           05 TRANSACTION-FILE-STATUS PIC XX.
           05 REPORT-FILE-STATUS    PIC XX.
       
       01 SYSTEM-VARIABLES.
           05 CURRENT-DATE          PIC 9(8).
           05 CURRENT-TIME          PIC 9(6).
           05 REPORT-DATE           PIC X(10).
           05 REPORT-TIME           PIC X(8).
           05 LINE-COUNT            PIC 9(3) VALUE 0.
           05 PAGE-NUMBER           PIC 9(3) VALUE 1.
           05 LINES-PER-PAGE        PIC 9(3) VALUE 60.
       
       01 REPORT-COUNTERS.
           05 TOTAL-CUSTOMERS       PIC 9(5) VALUE 0.
           05 ACTIVE-CUSTOMERS      PIC 9(5) VALUE 0.
           05 TOTAL-DEPOSITS        PIC 9(10)V99 VALUE 0.
           05 TOTAL-WITHDRAWALS     PIC 9(10)V99 VALUE 0.
           05 TOTAL-TRANSFERS       PIC 9(10)V99 VALUE 0.
           05 TOTAL-TRANSACTIONS    PIC 9(5) VALUE 0.
           05 TOTAL-BALANCE         PIC 9(12)V99 VALUE 0.
       
       01 DISPLAY-VARIABLES.
           05 WS-DISPLAY-BALANCE    PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-AMOUNT     PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-TOTAL      PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-DATE       PIC X(10).
           05 WS-DISPLAY-TIME       PIC X(8).
       
       01 REPORT-LINES.
           05 HEADER-LINE-1.
               10 FILLER            PIC X(20) VALUE SPACES.
               10 FILLER            PIC X(30) VALUE 
                  'COBOL BANKING SYSTEM REPORT'.
               10 FILLER            PIC X(82) VALUE SPACES.
           
           05 HEADER-LINE-2.
               10 FILLER            PIC X(20) VALUE SPACES.
               10 FILLER            PIC X(25) VALUE 
                  'Generated on: '.
               10 H2-DATE           PIC X(10).
               10 FILLER            PIC X(5) VALUE ' at '.
               10 H2-TIME           PIC X(8).
               10 FILLER            PIC X(64) VALUE SPACES.
           
           05 HEADER-LINE-3.
               10 FILLER            PIC X(132) VALUE 
                  '========================================'.
           
           05 CUSTOMER-HEADER.
               10 FILLER            PIC X(10) VALUE 'ACCOUNT #'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(20) VALUE 'CUSTOMER NAME'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(10) VALUE 'TYPE'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(8) VALUE 'STATUS'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(15) VALUE 'BALANCE'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(12) VALUE 'DATE OPENED'.
               10 FILLER            PIC X(49) VALUE SPACES.
           
           05 CUSTOMER-DETAIL.
               10 CD-ACCOUNT        PIC 9(10).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 CD-NAME           PIC X(20).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 CD-TYPE           PIC X(10).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 CD-STATUS         PIC X(8).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 CD-BALANCE        PIC $ZZZ,ZZZ,ZZ9.99.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 CD-DATE           PIC X(10).
               10 FILLER            PIC X(49) VALUE SPACES.
           
           05 TRANSACTION-HEADER.
               10 FILLER            PIC X(10) VALUE 'TRANS ID'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(12) VALUE 'ACCOUNT #'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(4) VALUE 'TYPE'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(10) VALUE 'AMOUNT'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(12) VALUE 'DATE'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(8) VALUE 'TIME'.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 FILLER            PIC X(20) VALUE 'DESCRIPTION'.
               10 FILLER            PIC X(44) VALUE SPACES.
           
           05 TRANSACTION-DETAIL.
               10 TD-ID             PIC 9(10).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 TD-ACCOUNT        PIC 9(10).
               10 FILLER            PIC X(4) VALUE SPACES.
               10 TD-TYPE           PIC X(4).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 TD-AMOUNT         PIC $ZZZ,ZZZ,ZZ9.99.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 TD-DATE           PIC X(10).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 TD-TIME           PIC X(8).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 TD-DESCRIPTION    PIC X(20).
               10 FILLER            PIC X(44) VALUE SPACES.
           
           05 SUMMARY-LINE.
               10 FILLER            PIC X(20) VALUE 'SUMMARY STATISTICS:'.
               10 FILLER            PIC X(112) VALUE SPACES.
           
           05 TOTAL-LINE.
               10 TL-DESCRIPTION    PIC X(30).
               10 FILLER            PIC X(2) VALUE SPACES.
               10 TL-COUNT          PIC ZZZ,ZZ9.
               10 FILLER            PIC X(2) VALUE SPACES.
               10 TL-AMOUNT         PIC $ZZZ,ZZZ,ZZ9.99.
               10 FILLER            PIC X(85) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-REPORT-GENERATION.
           PERFORM INITIALIZE-REPORT
           PERFORM GENERATE-CUSTOMER-REPORT
           PERFORM GENERATE-TRANSACTION-REPORT
           PERFORM GENERATE-SUMMARY-REPORT
           PERFORM CLOSE-REPORT-FILES
           STOP RUN.
       
       INITIALIZE-REPORT.
           OPEN INPUT CUSTOMER-FILE
           OPEN INPUT TRANSACTION-FILE
           OPEN OUTPUT REPORT-FILE
           
           ACCEPT CURRENT-DATE FROM DATE
           ACCEPT CURRENT-TIME FROM TIME
           
           PERFORM FORMAT-DATE-TIME
           PERFORM WRITE-REPORT-HEADER.
       
       FORMAT-DATE-TIME.
           MOVE CURRENT-DATE TO WS-DISPLAY-DATE
           MOVE CURRENT-TIME TO WS-DISPLAY-TIME
           
           STRING WS-DISPLAY-DATE(1:2) '/' 
                  WS-DISPLAY-DATE(3:2) '/' 
                  WS-DISPLAY-DATE(5:4)
                  INTO REPORT-DATE
           END-STRING
           
           STRING WS-DISPLAY-TIME(1:2) ':' 
                  WS-DISPLAY-TIME(3:2) ':' 
                  WS-DISPLAY-TIME(5:2)
                  INTO REPORT-TIME
           END-STRING.
       
       WRITE-REPORT-HEADER.
           MOVE REPORT-DATE TO H2-DATE
           MOVE REPORT-TIME TO H2-TIME
           
           WRITE REPORT-LINE FROM HEADER-LINE-1
           WRITE REPORT-LINE FROM HEADER-LINE-2
           WRITE REPORT-LINE FROM HEADER-LINE-3
           WRITE REPORT-LINE FROM SPACES
           ADD 4 TO LINE-COUNT.
       
       GENERATE-CUSTOMER-REPORT.
           WRITE REPORT-LINE FROM 'CUSTOMER ACCOUNT LISTING'
           WRITE REPORT-LINE FROM '========================'
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM CUSTOMER-HEADER
           WRITE REPORT-LINE FROM '----------------------------------------'
           ADD 5 TO LINE-COUNT
           
           PERFORM READ-CUSTOMER-RECORDS UNTIL CUSTOMER-FILE-STATUS NOT = '00'
           
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM SPACES
           ADD 2 TO LINE-COUNT.
       
       READ-CUSTOMER-RECORDS.
           READ CUSTOMER-FILE
               AT END
                   MOVE '99' TO CUSTOMER-FILE-STATUS
               NOT AT END
                   PERFORM PROCESS-CUSTOMER-RECORD
           END-READ.
       
       PROCESS-CUSTOMER-RECORD.
           ADD 1 TO TOTAL-CUSTOMERS
           ADD CUST-BALANCE TO TOTAL-BALANCE
           
           IF CUST-STATUS = 'A'
               ADD 1 TO ACTIVE-CUSTOMERS
           END-IF
           
           MOVE CUST-ACCOUNT-NUMBER TO CD-ACCOUNT
           MOVE CUST-NAME TO CD-NAME
           
           EVALUATE CUST-ACCOUNT-TYPE
               WHEN 'S' MOVE 'SAVINGS' TO CD-TYPE
               WHEN 'C' MOVE 'CHECKING' TO CD-TYPE
               WHEN OTHER MOVE 'UNKNOWN' TO CD-TYPE
           END-EVALUATE
           
           EVALUATE CUST-STATUS
               WHEN 'A' MOVE 'ACTIVE' TO CD-STATUS
               WHEN 'I' MOVE 'INACTIVE' TO CD-STATUS
               WHEN OTHER MOVE 'UNKNOWN' TO CD-STATUS
           END-EVALUATE
           
           MOVE CUST-BALANCE TO CD-BALANCE
           
           STRING CUST-DATE-OPENED(1:2) '/' 
                  CUST-DATE-OPENED(3:2) '/' 
                  CUST-DATE-OPENED(5:4)
                  INTO CD-DATE
           END-STRING
           
           WRITE REPORT-LINE FROM CUSTOMER-DETAIL
           ADD 1 TO LINE-COUNT
           
           IF LINE-COUNT > LINES-PER-PAGE
               PERFORM NEW-PAGE
           END-IF.
       
       GENERATE-TRANSACTION-REPORT.
           WRITE REPORT-LINE FROM 'TRANSACTION HISTORY'
           WRITE REPORT-LINE FROM '=================='
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM TRANSACTION-HEADER
           WRITE REPORT-LINE FROM '----------------------------------------'
           ADD 5 TO LINE-COUNT
           
           PERFORM READ-TRANSACTION-RECORDS UNTIL TRANSACTION-FILE-STATUS NOT = '00'
           
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM SPACES
           ADD 2 TO LINE-COUNT.
       
       READ-TRANSACTION-RECORDS.
           READ TRANSACTION-FILE
               AT END
                   MOVE '99' TO TRANSACTION-FILE-STATUS
               NOT AT END
                   PERFORM PROCESS-TRANSACTION-RECORD
           END-READ.
       
       PROCESS-TRANSACTION-RECORD.
           ADD 1 TO TOTAL-TRANSACTIONS
           
           EVALUATE TRANS-TYPE
               WHEN 'D' ADD TRANS-AMOUNT TO TOTAL-DEPOSITS
               WHEN 'W' ADD TRANS-AMOUNT TO TOTAL-WITHDRAWALS
               WHEN 'T' ADD TRANS-AMOUNT TO TOTAL-TRANSFERS
           END-EVALUATE
           
           MOVE TRANS-ID TO TD-ID
           MOVE TRANS-ACCOUNT-NUMBER TO TD-ACCOUNT
           
           EVALUATE TRANS-TYPE
               WHEN 'D' MOVE 'DEP' TO TD-TYPE
               WHEN 'W' MOVE 'WTH' TO TD-TYPE
               WHEN 'T' MOVE 'TRF' TO TD-TYPE
               WHEN OTHER MOVE 'UNK' TO TD-TYPE
           END-EVALUATE
           
           MOVE TRANS-AMOUNT TO TD-AMOUNT
           
           STRING TRANS-DATE(1:2) '/' 
                  TRANS-DATE(3:2) '/' 
                  TRANS-DATE(5:4)
                  INTO TD-DATE
           END-STRING
           
           STRING TRANS-TIME(1:2) ':' 
                  TRANS-TIME(3:2) ':' 
                  TRANS-TIME(5:2)
                  INTO TD-TIME
           END-STRING
           
           MOVE TRANS-DESCRIPTION TO TD-DESCRIPTION
           
           WRITE REPORT-LINE FROM TRANSACTION-DETAIL
           ADD 1 TO LINE-COUNT
           
           IF LINE-COUNT > LINES-PER-PAGE
               PERFORM NEW-PAGE
           END-IF.
       
       GENERATE-SUMMARY-REPORT.
           WRITE REPORT-LINE FROM SUMMARY-LINE
           WRITE REPORT-LINE FROM '===================='
           WRITE REPORT-LINE FROM SPACES
           ADD 3 TO LINE-COUNT
           
           MOVE 'Total Customers:' TO TL-DESCRIPTION
           MOVE TOTAL-CUSTOMERS TO TL-COUNT
           MOVE 0 TO TL-AMOUNT
           WRITE REPORT-LINE FROM TOTAL-LINE
           ADD 1 TO LINE-COUNT
           
           MOVE 'Active Customers:' TO TL-DESCRIPTION
           MOVE ACTIVE-CUSTOMERS TO TL-COUNT
           MOVE 0 TO TL-AMOUNT
           WRITE REPORT-LINE FROM TOTAL-LINE
           ADD 1 TO LINE-COUNT
           
           MOVE 'Total Transactions:' TO TL-DESCRIPTION
           MOVE TOTAL-TRANSACTIONS TO TL-COUNT
           MOVE 0 TO TL-AMOUNT
           WRITE REPORT-LINE FROM TOTAL-LINE
           ADD 1 TO LINE-COUNT
           
           MOVE 'Total Deposits:' TO TL-DESCRIPTION
           MOVE 0 TO TL-COUNT
           MOVE TOTAL-DEPOSITS TO TL-AMOUNT
           WRITE REPORT-LINE FROM TOTAL-LINE
           ADD 1 TO LINE-COUNT
           
           MOVE 'Total Withdrawals:' TO TL-DESCRIPTION
           MOVE 0 TO TL-COUNT
           MOVE TOTAL-WITHDRAWALS TO TL-AMOUNT
           WRITE REPORT-LINE FROM TOTAL-LINE
           ADD 1 TO LINE-COUNT
           
           MOVE 'Total Transfers:' TO TL-DESCRIPTION
           MOVE 0 TO TL-COUNT
           MOVE TOTAL-TRANSFERS TO TL-AMOUNT
           WRITE REPORT-LINE FROM TOTAL-LINE
           ADD 1 TO LINE-COUNT
           
           MOVE 'Total Bank Balance:' TO TL-DESCRIPTION
           MOVE 0 TO TL-COUNT
           MOVE TOTAL-BALANCE TO TL-AMOUNT
           WRITE REPORT-LINE FROM TOTAL-LINE
           ADD 1 TO LINE-COUNT.
       
       NEW-PAGE.
           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM 'Page ' PAGE-NUMBER
           WRITE REPORT-LINE FROM SPACES
           ADD 1 TO PAGE-NUMBER
           MOVE 0 TO LINE-COUNT.
       
       CLOSE-REPORT-FILES.
           CLOSE CUSTOMER-FILE
           CLOSE TRANSACTION-FILE
           CLOSE REPORT-FILE
           DISPLAY 'Report generation completed successfully!'.

