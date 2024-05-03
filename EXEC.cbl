
IDENTIFICATION DIVISION.
PROGRAM-ID. EXECACTIONSSAMPLES.
AUTHOR. COBOLUSERS.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. IBM-370.
OBJECT-COMPUTER. IBM-370.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-VARIABLE PIC X(10) VALUE 'HELLO'.

PROCEDURE DIVISION.

EXEC SQL
    DISPLAY 'This is an example of COBOL program with EXEC SQL.'
END-EXEC.

EXEC PGM PGM1
    DISPLAY 'Executing Program PGM1'
END-EXEC.

EXEC LINK SUBPROG
    DISPLAY 'Calling Subprogram SUBPROG'
END-EXEC.

EXEC GOTO PARA1
    DISPLAY 'This line will not be executed'
END-EXEC.

EXEC TOOLS
    DISPLAY 'Using COBOL EXEC TOOLS'
END-EXEC.

EXEC CONVERT
    DISPLAY 'Using COBOL EXEC CONVERT'
END-EXEC.

EXEC SQL
        SELECT CUSTOMER_ID, CUSTOMER_NAME, CUSTOMER_AGE
        INTO :CUSTOMER-ID, :CUSTOMER-NAME, :CUSTOMER-AGE
        FROM CUSTOMER_TABLE
        WHERE CUSTOMER_ID = :WS-CUSTOMER-ID
END-EXEC.

IF SQLCODE = 0
     DISPLAY "Customer found:", CUSTOMER-NAME
ELSE
     DISPLAY "Customer not found."
END-IF.

PARA1.
    DISPLAY 'Jumped to PARA1'.


STOP RUN.
