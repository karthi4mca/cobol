000010 S900-000-DUAL-MDUL-DB2-ERR      SECTION.
000020******************************************************************
000030*          S900 - FORMAT ERROR LOG FIELDS FOR CLAIMS DUAL MODULE *
000040*                                                                *
000050*?   THIS SECTION FORMATS THE FIELDS FOR THE G_ERROR_LOG_TB      *
000060*+   THAT ARE TO BE RETURNED TO THE CLAIM CONTROL MODULE =       *
000070*+   PDDC8000, WHERE THEY WILL ACTUALLY BE WRITTEN TO THE TABLE. *
000080*+                                                               *
000090*+   NOTE, TO USE THIS ROUTINE, THE PROGRAM MUST HAVE MEMBERS    *
000100*+   WD999950, W1C56941, WVC4398C, AND WVG5880C COPIED INTO THE  *
000110*+   PROGRAM.                                                    *
000120*+                                                               *
000130*+   NOTE, THIS ROUTINE, EXCEPT FOR THE GOBACK, IS REPEATED IN-  *
000140*+   LINE IN THE CLAIM CONTROL PROGRAM = PDDC8000.               *
000150*+                                                               *
000160******************************************************************
000170 S900-010-START.
000180*
matest*    display 'WD-999-MSG (1):' WD-999-MSG(1)
matest*    display 'WD-999-MSG (2):' WD-999-MSG(2)
matest*    display 'WD-999-MSG (3):' WD-999-MSG(3)
matest*    display 'WD-999-MSG (4):' WD-999-MSG(4)
matest*    display 'WD-999-MSG (5):' WD-999-MSG(5)
matest*    display 'sql code :' SQLCODE
000190     INITIALIZE              W1C56941-G-ERROR-LOG-TB.
000200     MOVE WV-C4398-C-SQL-HARD-ERROR
000210                          TO W1C56941-C-MDUL-RTRN-CD.
000220     MOVE WD-999-MSG (1)  TO W1C56941-C-MDUL-NAM.
000230     MOVE WD-999-MSG (1)  TO W1C56941-G-PROG-NAM.
000240     MOVE WD-999-MSG (2)  TO W1C56941-G-PROG-SECTION-TX.
000250     MOVE WD-999-MSG (3)  TO W1C56941-G-SQL-TABLE-NAM.
000260     MOVE WD-999-MSG (4)  TO W1C56941-G-SQL-FUNCTION-TX.
000270     MOVE WD-999-MSG (5)  TO W1C56941-G-KEY-TX.
000280     MOVE SQLCODE         TO W1C56941-G-SQL-CODE-NUM.
000290     MOVE W1C40541-G-AUD-USER-ID
000300                          TO W1C56941-G-AUD-USER-ID.
000310     MOVE W1C40541-G-AUD-TS
000320                          TO W1C56941-G-AUD-TS.
000330     MOVE SQLCAID         TO W1C56941-G-SQL-AID-TX.
000340     MOVE SQLCABC         TO W1C56941-G-SQL-ABC-TX.
000350     MOVE SQLERRM         TO W1C56941-G-SQL-ERROR-TX.
000360     MOVE SQLERRP         TO W1C56941-G-SQL-ERRORP-TX.
000370     MOVE SQLERRD(1)      TO W1C56941-G-SQL-ERRORD1-TX.
000380     MOVE SQLERRD(2)      TO W1C56941-G-SQL-ERRORD2-TX.
000390     MOVE SQLERRD(3)      TO W1C56941-G-SQL-ERRORD3-TX.
000400     MOVE SQLERRD(4)      TO W1C56941-G-SQL-ERRORD4-TX.
000410     MOVE SQLERRD(5)      TO W1C56941-G-SQL-ERRORD5-TX.
000420     MOVE SQLERRD(6)      TO W1C56941-G-SQL-ERRORD6-TX.
000430     MOVE SQLWARN0        TO W1C56941-G-SQL-WARNING1-TX.
000440     MOVE SQLWARN1        TO W1C56941-G-SQL-WARNING2-TX.
000450     MOVE SQLWARN2        TO W1C56941-G-SQL-WARNING3-TX.
000460     MOVE SQLWARN3        TO W1C56941-G-SQL-WARNING4-TX.
000470     MOVE SQLWARN4        TO W1C56941-G-SQL-WARNING5-TX.
000480     MOVE SQLWARN5        TO W1C56941-G-SQL-WARNING6-TX.
000490     MOVE SQLWARN6        TO W1C56941-G-SQL-WARNING7-TX.
000500     MOVE SQLWARN7        TO W1C56941-G-SQL-WARNING8-TX.
      *    MOVE SQLWARN8        TO W1C56941-G-SQL-WARNING9-TX.          2PHASE
      *    MOVE SQLWARN9        TO W1C56941-G-SQL-WARNING10-TX.         2PHASE
      *    MOVE SQLSTATE        TO W1C56941-G-SQL-STATE-TX.             2PHASE
000540     MOVE WD-999-MSG (6)  TO W1C56941-G-ERR-SVRTY-CD.
000550     MOVE W1C40541-C-BLNG-PROV-ID
000560                          TO W1C56941-P-ID.
000570*
000580***     RETURN TO CLAIM CONTROL
000590*
000600     GOBACK.
000610*
000620 S900-999-EXIT.
000630     EXIT.
000640
000650
000660
000670
000680
000690
