       IDENTIFICATION DIVISION.
       PROGRAM-ID. MultiLineSQLExample.
 
       ENVIRONMENT DIVISION.
 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
 
 
       01  WS-STUDENT-REC.
       05 WS-STUDENT-ID          PIC 9(5).
       05 WS-STUDENT-NAME        PIC X(25).
       05 WS-STUDENT-ADDRESS.
       10 WS-STREET           PIC X(30).
       10 WS-CITY             PIC X(20).
       10 WS-STATE            PIC XX.
       10 WS-ZIP              PIC 9(5).
       05  CONTINUE-MAX-GALLONS-OTH-SW PIC 9        VALUE 0.
       88  CONTINUE-MAX-GALLONS-OTH             VALUE 0.
 
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-CUSTOMER-ID         PIC 9(5).
       01  WS-CUSTOMER-NAME       PIC X(25).
       01  WS-CUSTOMER-AGE        PIC 9(3).
       01  WS-CONNECTION-STATUS   PIC X(8).
       EXEC SQL END DECLARE SECTION END-EXEC.
        
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
 
 
       EXEC SQL
            CONNECT TO 'your_database'
            USER 'your_username'
            USING 'your_password'
       END-EXEC.
 
 
       EXEC SQL
           WHENEVER SQLERROR GOTO DB-ERROR
       END-EXEC.
 
 
       MOVE 1001 TO WS-CUSTOMER-ID.
       EXEC SQL
           SELECT CUSTOMER_NAME, CUSTOMER_AGE
           INTO :WS-CUSTOMER-NAME, :WS-CUSTOMER-AGE
           FROM CUSTOMER_TABLE
           WHERE CUSTOMER_ID = :WS-CUSTOMER-ID
       END-EXEC.
 
       DISPLAY 'Customer Name: ' WS-CUSTOMER-NAME.
       DISPLAY 'Customer Age: ' WS-CUSTOMER-AGE.
 
 
       MOVE 1002 TO WS-CUSTOMER-ID.
       MOVE 'John Doe' TO WS-CUSTOMER-NAME.
       MOVE 30 TO WS-CUSTOMER-AGE.
       EXEC SQL
           INSERT INTO CUSTOMER_TABLE (CUSTOMER_ID, CUSTOMER_NAME, CUSTOMER_AGE)
           VALUES (:WS-CUSTOMER-ID, :WS-CUSTOMER-NAME, :WS-CUSTOMER-AGE)
       END-EXEC.
 