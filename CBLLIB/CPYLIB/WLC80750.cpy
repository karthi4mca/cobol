000010***************************************************************** 00000100
000020*    MEMBER:  WLC80750                                          * 00000200
000030*      NAME:  C-MDUL-COMMAREA.                                  * 00000300
000040* SUBSYSTEM:  C Claims                                          * 00000400
000050*   CREATED:  2002-07-17                                        * 00000500
000060*   UPDATED:  2010-02-18                                        * COR0826
000070*                                                               * 00000700
PML   ****SINCE 2-PHASE ADJUDICATION HAS BEEN IMPLEMENTED, THERE IS ***
PML   ****A COPYBOOK THAT IS A DUPLICATE OF THIS COPYBOOK, AND MUST ***
PML   ****BE MODIFIED IF WLC80750 IS MODIFIED.                      ***
PML   ****     >>>>>>>>>>>   WL380750   <<<<<<<<<<                  ***
PML   ****                                                          ***
PML   ****                                                          ***
PML   ****                                                          ***
000080****CLAIM CONTROL AND CALLED SUBPROGRAM COMMON INTER-PROGRAM  *** 00000800
000090****COMMUNICATION AND WORK AREA.                              *** 00000900
000100***************************************************************** 00001000
000110 01  WL-807-C-MDUL-COMMAREA.                                      00001100
000120                                                                  00001200
000130                                                                  00001300
000140     05  WL-807-SWITCHES.                                         00001400
000150***  SET BY PDDC8130 - CLAIM TYPE ASSIGNMENT                      00001500
000160         10  WL-807-CLAIM-TYPE-EXC-IND        PIC X(01).          00001600
000170             88  WL-807-CLAIM-TYPE-EXC-SET    VALUE 'Y'.          00001700
000180             88  WL-807-FATAL-ADJ-EXC-SET     VALUE 'A'.          00001800
000190             88  WL-807-MEMBER-01-FORCED      VALUE '1'.          00001900
000200                                                                  00002000
000210***  SET BY ???????                                               00002100
000220         10  WL-807-FATAL-HDR-PROV-EDIT-IND     PIC X(01).        00002200
000230             88  WL-807-NO-FATAL-PROV-EDITS     VALUE ' '.        00002300
000240             88  WL-807-FATAL-PROV-EDIT-SET     VALUE 'P'.        00002400
000250                                                                  00002500
000260***  SET BY ??????                                                00002600
000270         10  WL-807-FATAL-HDR-MBR-EDIT-IND      PIC X(01).        00002700
000280             88  WL-807-NO-FATAL-MBR-EDITS      VALUE ' '.        00002800
000290             88  WL-807-FATAL-MEMBER-EDIT-SET   VALUE 'M'.        00002900
000300                                                                  00003000
000310***  SET BY VARIOUS PRICING-RELATED PROGRAMS                      00003100
000320         10  WL-807-FATAL-HDR-PRIC-EDIT-IND     PIC X(01).        00003200
000330             88  WL-807-NO-FATAL-PRICING-EDIT   VALUE ' '.        00003300
000340             88  WL-807-CANT-PRICE-CLM-EDIT-SET VALUE '$'.        00003400
000350                                                                  00003500
000360***  SET BY VARIOUS PRICING-RELATED PROGRAMS                      00003600
000370         10  WL-807-AID-CAT-EDIT-IND          PIC X(01).          00003700
000380             88  WL-807-DONT-DO-AID-CAT-EDIT  VALUE ' '.          00003800
000390             88  WL-807-DO-AID-CAT-EDIT       VALUE 'H' 'D'.      00003900
000400             88  WL-807-DO-HDR-AID-CAT-EDIT   VALUE 'H'.          00004000
000410             88  WL-807-DO-DTL-AID-CAT-EDIT   VALUE 'D'.          00004100
000420                                                                  00004200
000430***  SET BY PDDC8520 - DETAIL PRICING EDITS                       00004300
000440         10  WL-807-HDR-COVERAGE-EXC-IND      PIC X(01).          00004400
000450             88  WL-807-HDR-COVERAGE-EXC-SET  VALUE 'Y'.          00004500
000460                                                                  00004600
000470***  SET BY PDDC8620 - DUR/ACCUMULATION                           00004700
000480         10  WL-807-MAX-BENEFIT-SW            PIC X.              00004800
000490             88  WL-807-MAX-BENEFIT-MET-YES   VALUE 'Y'.          00004900
000500             88  WL-807-MAX-BENEFIT-MET-NO    VALUE 'N'.          00005000
000510                                                                  00005100
000520         10  WL-807-OUT-OF-POCKET-SW          PIC X.              00005200
000530             88  WL-807-OUT-OF-POCKET-MET-YES VALUE 'Y'.          00005300
000540             88  WL-807-OUT-OF-POCKET-MET-NO  VALUE 'N'.          00005400
000550                                                                  00005500
000560         10  WL-807-CUST-UNQ-HDR-SWITCHES.                        00005600
000570***  SET BY PDDC8525 - CUSTOMER-SPECIFIC POST DETAIL PRICING EDITS00005700
000580             15  WL-807-MA-RTS-FEE-SW             PIC X(01).      COR0636
000582                 88  WL-807-NOT-RTS               VALUE ' '.      COR0636
000590                 88  WL-807-FULL-RTS-W-FEE        VALUE 'F'.      COR0636
000590                 88  WL-807-PARTIAL-RTS-W-FEE     VALUE 'P'.      COR0636
000600                 88  WL-807-RTS-WO-FEE            VALUE 'N'.      COR0636
000630                                                                  00006300
000570***  SET BY PDDC8000 - LEGACY PROCESSING VV 3201                  COR0883
000570***  GENERATED FOR SYSTEM G-PARM NUM #1363                        COR0883
000650         10  WL-807-LEGACY-PROCESSING-SW      PIC X(01).          COR0883
000540             88  WL-807-CURRMMIS-TCN-REQUEST  VALUE 'Y'.          COR0883
000520             88  WL-807-CURRMMIS-IS-ACTIVE    VALUE 'Y'.          COR0962
000530             88  WL-807-NEWMMIS-ICN-REQUEST   VALUE 'N'.          COR0883
000550             88  WL-807-NEWMMIS-IS-ACTIVE     VALUE 'N'.          COR0962
000670                                                                  COR0883
000680     05  WL-807-TABLES.                                           00006800
000690***  FILLED BY PDDC8505 - HEADER PRICING EDITS                    00006900
000700         10  WL-807-PLAN-DAW-CODE-TBL.                            00007000
000710             15  WL-807-DAW-CODE-TBL-ENTS   PIC S9(3) COMP-3.     00007100
000720             15  WL-807-PLAN-DAW-CODES      OCCURS 10 TIMES       00007200
000730                                            INDEXED BY            00007300
000740                                            WL-807-DAW-CODE-IDX.  00007400
000750                                                                  00007500
000760                 20  WL-807-PLAN-DAW-CODE   PIC X(1).             00007600
000770                                                                  00007700
000780     05  WL-807-WORKAREAS.                                        00007800
000790***  FILLED BY PDDC8000 - CLAIM CONTROLLER                        00007900
000800         10  WL-807-P7546-CYCLE-DATE          PIC X(10).          00008000
000810***  FILLED BY PDDC8120 - DATE VALIDATION                         00008100
000820         10  WL-807-TCN-YYYY-MM-DD-DATE       PIC X(10).          00008200
000830***  FILLED BY PDDC8520 - DETAIL PRICING EDITS                    00008300
000840         10  WL-807-MAX-DAYS-SUPPLY-LIMIT     PIC S9(4) COMP.     00008400
000850***  FILLED BY PDDC8505 - HEADER PRICING EDITS                    00008500
000860         10  WL-807-MAX-UNITS-LIMIT           PIC S9(8)V9(3)      00008600
000870                                              COMP-3.             00008700
000880                                                                  00008800
000890***  FILLED BY PDDC8620 - DUR/ACCUMULATION                        00008900
000900         10  WL-807-BENEFIT-AMT-REM           PIC S9(7)V99 COMP-3.00009000
000910         10  WL-807-FILL-AFTER-DATE           PIC X(10).          00009100
000920***  FILLED BY PDDC8130, USED AND REINITIALIZED BY PDDC8420       00008900
000930         10  WL-807-BCVRG-BEG-REDEF   REDEFINES
000940                        WL-807-FILL-AFTER-DATE.
000941             15 WL-807-B-CVRG-BEG-POST-DT     PIC X(10).
000942***  FILLED BY PDDC8610 - DUPE CHECK                              00009200
000943         10  WL-807-INITIAL-COPAY-AMT         PIC S9(9)V99 COMP-3.00009300
005570         10  WL-807-R-QTY-PRC-AMT-OMT-IND     PIC X(1).           COR11078
000944                                                                  00009400
000950***********************************************************       00009500
000960***  MOST FIELDS BELOW HOLD DATA THAT HAS BEEN FETCHED  ***       00009600
000970***  FROM DB2 TABLES DURING PROCESSING.                 ***       00009700
000980***********************************************************       00009800
000990                                                                  00009900
001000*-------------------------------*                                 00010000
001010*    CUSTOMER                   *                                 00010100
001020*-------------------------------*                                 00010200
001030***  FILLED BY PDDC8300 - DATA VALIDATION                         00010300
001040     05  WL-807-RCUSTDTB-FIELDS.                                  00010400
001050         10  WL-807-R-MAIL-ORD-IND            PIC X(01).          00010500
001060         10  WL-807-R-STEPCARE-IND            PIC X(01).          00010600
001070         10  WL-807-R-WORKERS-COMP-IND        PIC X(01).          00010700
001080                                                                  00010800
001090*-------------------------------*                                 00010900
001100*    PARTICIPANT DETAIL         *                                 00011000
001110*-------------------------------*                                 00011100
001120***  FILLED BY PDDC8420 - PARTICIPANT ELIGIBILITY                 00011200
001130     05  WL-807-BDTAILTB-FIELDS.                                  00011300
001140         10  WL-807-B-PREV-YR-COPAY-CI        PIC X(01).          00011400
001150         10  WL-807-B-CURR-YR-COPAY-CI        PIC X(01).          00011500
001160         10  WL-807-B-OTHR-YR-COPAY-CI        PIC X(01).          00011600
001170                                                                  00011700
001180*-------------------------------*                                 00011800
001190*    GROUP                      *                                 00011900
001200*-------------------------------*                                 00012000
001210***  FILLED BY PDDC8130 - CLAIM type assignment                   00012100
001220     05  WL-807-RGROUPTB-FIELDS.                                  00012200
001230         10  WL-807-R-GRP-TYP-CD              PIC X(01).          00012300
001240         10  WL-807-R-MAX-COMPOUND-AMT        PIC S9(9)V99        00012400
001250                                                         COMP-3.  00012500
001260         10  WL-807-R-GRP-CLM-FILE-LMT        PIC S9(9)  comp.    00012600
001270         10  WL-807-R-UD-DISP-FEE-AMT         PIC S9(9)V99        00012700
001280                                                         COMP-3.  00012800
001290                                                                  00012900
001300*-------------------------------*                                 00013000
001310*    GROUP PRICING              *                                 00013100
001320*-------------------------------*                                 00013200
001330***  FILLED BY PDDC8530 - PRICING                                 00013300
001340         10  WL-807-R-PAY-LESSER-IND          PIC X(01).          00013400
001350         10  WL-807-R-SALES-TAX-IND           PIC X(01).          00013500
001360                                                                  00013600
001370*-------------------------------*                                 00013700
001380*    NETWORK/GROUP XREF         *                                 00013800
001390*-------------------------------*                                 00013900
001400***  FILLED BY PDDC8420 - PARTICIPANT ELIGIBILITY                 00014000
001410         10  WL-807-R-NETWORK-ID              PIC X(06).          00014100
001420                                                                  00014200
001430*-------------------------------*                                 00014300
001440*    PROVIDER                   *                                 00014400
001450*-------------------------------*                                 00014500
001460***  FILLED BY PDDC8410 - PROVIDER ELIGIBILITY                    00014600
001470     05  WL-807-PROVDRTB-FIELDS.                                  00014700
001480         10  WL-807-P-PROV-PYMT-CD            PIC X(01).          00014800
I2SPL          10  WL-807-P-PHRM-P-ID               PIC S9(09) COMP.
001490                                                                  00014900
001500*-------------------------------*                                 00015000
001510*    PROVIDER DATA              *                                 00015100
001520*-------------------------------*                                 00015200
001530***  FILLED BY PDDC8410 - PROVIDER ELIGIBILITY                    00015300
001540     05  WL-807-PSPECLTB-FIELDS.                                  00015400
001550         10  WL-807-P-SPECL-CD          PIC X(03).                00015500
001550         10  WL-807-PHARM-TYP-CD        PIC X(03).                SPA
001490                                                                  COR0761
001560     05  WL-807-SANC-PRSC-IND                 PIC X(01).          COR0761
001570                                                                  00015700
001580*-------------------------------*                                 00015800
001590*    PROVIDER NETWORK           *                                 00015900
001600*-------------------------------*                                 00016000
001610***  FILLED BY PDDC8410 - PROVIDER ELIGIBILITY                    00016100
001620     05  WL-807-PRVNETTB-FIELDS.                                  00016200
001630         10  WL-807-P-BRND-DISCT-PCT          PIC S9(5)V99        00016300
001640                                                         COMP-3.  00016400
001650         10  WL-807-P-GENR-DISCT-PCT          PIC S9(5)V99        00016500
001660                                                         COMP-3.  00016600
001670*-------------------------------*                                 00016700
001680*    PLAN DETAIL                *                                 00016800
001690*-------------------------------*                                 00016900
001700***  FILLED BY PDDC8505 - HEADER PRICING EDITS                    00017000
001710     05  WL-807-RPLANDTB-FIELDS.                                  00017100
001720         10  WL-807-R-PLAN-TYPE-CD            PIC X(01).          00017200
001730         10  WL-807-R-MAX-BENE-CD             PIC X(01).          00017300
001740         10  WL-807-R-GENR-MAND-IND           PIC X(01).          00017400
001750         10  WL-807-R-EXT-BENE-IND            PIC X(01).          00017500
001760         10  WL-807-R-DESI-DRUG-IND           PIC X(01).          00017600
001770         10  WL-807-R-FORMULARY-TY-CD         PIC X(01).          00017700
001780         10  WL-807-R-DRUG-FMLY-NUM           PIC S9(4)  COMP.    00017800
001790         10  WL-807-R-MAX-FAM-AMT             PIC S9(9)  COMP.    00017900
001800         10  WL-807-R-MAX-INDIV-AMT           PIC S9(9)  COMP.    00018000
001810         10  WL-807-R-DED-INDIV-AMT           PIC S9(9)  COMP.    00018100
001820         10  WL-807-R-DED-FAM-AMT             PIC S9(9)  COMP.    00018200
001830         10  WL-807-R-MAX-FAM-MBR-NUM         PIC S9(4)  COMP.    00018300
001840         10  WL-807-R-MAX-BENE-ACUM-CD        PIC X(11).          00018400
001850         10  WL-807-R-DED-ACUM-CD             PIC X(01).          00018500
001860         10  WL-807-R-FY-BEG-MO-NUM           PIC S9(4)  COMP.    00018600
001870                                                                  00018700
001880*----------------------------------*                              00018800
001890*    PLAN BENEFIT LIMITS - HEADER  *                              00018900
001900*----------------------------------*                              00019000
001910***  FILLED BY PDDC8505 - HEADER PRICING EDITS                    00019100
001920     05  WL-807-RPHLIMTB-FIELDS.                                  00019200
001930         10  WL-807-R-PHL-BUSN-TYP-CD         PIC X(1).           00019300
001940         10  WL-807-R-DAYS-SPLY-LMT           PIC S9(4) COMP.     00019400
001950         10  WL-807-R-GRACE-DAYS-LMT          PIC S9(4) COMP.     00019500
001960         10  WL-807-R-GRACE-PER-PCT           PIC SV9(3) COMP-3.  00019600
001970         10  WL-807-R-MAINT-SPLY-LMT          PIC S9(4) COMP.     00019700
001980         10  WL-807-R-MAX-CLM-UNT-LMT         PIC S9(8)V9(3)      00019800
001990                                                        COMP-3.   00019900
002000         10  WL-807-R-PHL-NUM-RX-LMT          PIC S9(4) COMP.     00020000
002010         10  WL-807-R-PHL-RX-SPAN-CD          PIC X(1).           00020100
002020         10  WL-807-R-PHL-RX-SPN-LMT          PIC S9(4) COMP.     00020200
002030         10  WL-807-R-NUM-REFLL-LMT           PIC S9(4) COMP.     00020300
002040         10  WL-807-R-MAIL-ORD-RX-CD          PIC X(1).           00020400
002050         10  WL-807-R-PHL-EXMT-AGE            PIC S9(4) COMP.     00020500
002060         10  WL-807-R-MAX-COPAY-AMT           PIC S9(7)V99 COMP-3.00020600
002070         10  WL-807-R-ADDL-COPAY-AMT          PIC S9(7)V99 COMP-3.00020700
002080         10  WL-807-R-ADDL-COPAY-PCT          PIC S9V9(4)  COMP-3.00020800
002060         10  WL-807-R-COPAY-BEG-DT            PIC X(10).          00020600
002060         10  WL-807-R-COPAY-END-DT            PIC X(10).          00020600
002090                                                                  00020900
002100                                                                  00021000
002110**************************************************************    00021100
002120*                                                            *    00021200
002130*  THIS WORK AREA IS USED DURING DUR PROCESSING.             *    00021300
002140*  ONLY FOR PDDC8620(DUR PROCESSING), PDDC8622 (DUR EDITS),  *    00021400
002150*  AND PDDC8625 (DUR FILTER).                                *    00021500
002160*                                                            *    00021600
002170**************************************************************    00021700
002180     05  WL-807-DUR-WORK-AREA.                                    00021800
002190         10  WL-807-DUR-HISTORY-KEY.                              00021900
002200           15  WL-807-HIST-C-TCN-NUM     PIC S9(17)V USAGE COMP-3.00022000
002210           15  WL-807-HIST-C-LI-NUM      PIC S9(4)   USAGE COMP.  00022100
002220                                                                  00022200
002230         10  WL-807-DUR-CLAIM-IND             PIC X(01).          00022300
002240           88  WL-807-DUR-HISTORY-CLAIM       VALUE 'H'.          00022400
002250           88  WL-807-DUR-IN-PROCESS-CLAIM    VALUE 'I'.          00022500
002260                                                                  00022600
002270         10  WL-807-DUR-ACTION-CODE           PIC X(01).          00022700
002280           88  WL-807-DUR-EDIT                VALUE 'E'.          00022800
002290           88  WL-807-DUR-ACCUM-SAFE-DUR      VALUE 'S'.          00022900
002300           88  WL-807-DUR-EDIT-AND-SAFE-DUR   VALUE 'B'.          00023000
002310           88  WL-807-DUR-EVALUATE-SAFE-DUR   VALUE 'X'.          00023100
002320                                                                  00023200
002330         10  WL-807-PRO-DUR-INDICATOR         PIC X(01).          00023300
002340           88  WL-807-IP-CLAIM-PROCESSED      VALUE 'I'.          00023400
002350           88  WL-807-EVALUATED-SAFE-DUR      VALUE 'E'.          00023500
002360                                                                  00023600
002370         10  WL-807-DUR-RETURN-CODE           PIC 9(01).          00023700
002380           88  WL-807-DUR-RETURN-OK           VALUE 0.            00023800
002390           88  WL-807-DUR-INVALID-CLAIM-IND   VALUE 1.            00023900
002400           88  WL-807-DUR-INVALID-ACTION      VALUE 2.            00024000
002410                                                                  00024100
002420         10  WL-807-DUR-LATEST-LDOS           PIC S9(05) COMP-3.  00024200
002430                                                                  00024300
002440         10  WL-807-MED-PROFILE-IND           PIC X(01).          00024400
002450             88  WL-807-NO-MED-PROFILE        VALUE LOW-VALUES.   00024500
002460             88  WL-807-MED-PROF-COVERED      VALUE 'C'.          00024600
002470             88  WL-807-MED-PROF-NON-COVERED  VALUE 'N'.          00024700
002480             88  WL-807-MED-PROF-PRIOR-AUTH   VALUE 'A'.          00024800
002490                                                                  00024900
002500         10  WL-807-DUR-MAX-DURATION          PIC  9(03).         00025000
002510             88  WL-807-IGNORE-MAX-DUR        VALUE 000.          00025100
002520                                                                  00025200
002530         10  WL-807-MEDPROF-REQ-SW             PIC X(01).         00025300
002540             88  WL-807-MEDPROF-REQ            VALUE 'Y'.         00025400
002550                                                                  00025500
002560         10  WL-807-PREGNANCY-INDICATOR        PIC X.             00025600
002570                                                                  00025700
002580         10  WL-807-DUR-SEVERITY-CODE          PIC X(01).         00025800
002590         10  WL-807-DUR-PA-SW     PIC 9(01)    VALUE 0.           00025900
002600             88  WL-807-NO-DUR-PA              VALUE 0.           00026000
002610             88  WL-807-HD-DUR-PA              VALUE 1.           00026100
002620             88  WL-807-DD-DUR-PA              VALUE 2.           00026200
002630             88  WL-807-TD-DUR-PA              VALUE 3.           00026300
002640             88  WL-807-ID-DUR-PA              VALUE 4.           00026400
002650                                                                  00026500
002660         10  WL-807-DUR-PA-DENY-SW  PIC 9(01)  VALUE 0.           00026600
002670             88  WL-807-DUR-PA-DENY            VALUE 1.           00026700
002680                                                                  00026800
002690         10  WL-807-HD-DUR-MSG-SW   PIC 9(01)  VALUE 0.           00026900
002700             88  WL-807-HD-DUR-MSG-YES         VALUE 1.           00027000
002710             88  WL-807-HD-DUR-MSG-NO          VALUE 0.           00027100
002720                                                                  00027200
002730         10  WL-807-HIST-DUR-MSG-SW PIC 9(01)  VALUE 0.           00027300
002740             88  WL-807-HIST-DUR-MSG-YES       VALUE 1.           00027400
002750             88  WL-807-HIST-DUR-MSG-NO        VALUE 0.           00027500
002760                                                                  00027600
002770         10  WL-807-DUR-FILTER-DATA.                              00027700
002780             15  WL-807-DUR-CONFLICT-CODE       PIC X(02).        00027800
002790             15  WL-807-DUR-MESSAGE             PIC X(30).        00027900
002800             15  WL-807-DUR-THERA-CLASS         PIC X(03).        00028000
002810             15  WL-807-DUR-EXCEPTION-STATUS    PIC X(01).        00028100
002820             15  WL-807-DUR-ACCUM-DAYS          PIC 9(03) COMP-3. 00028200
002830                                                                  00028300
002840     05  WL-807-EXC-OVERRIDE-AREA.                                00028400
002850         10  WL-807-EXC4010-IND               PIC X(01).          00028500
002860           88  WL-807-EXC4010-POSTED          VALUE 'A'.          00028600
002870                                                                  00028700
002880         10  WL-807-EXC4023-IND               PIC X(01).          00028800
002890           88  WL-807-EXC4023-POSTED          VALUE 'B'.          00028900
002900                                                                  00029000
002910         10  WL-807-EXC4082-IND               PIC X(01).          00029100
002920           88  WL-807-EXC4082-POSTED          VALUE 'C'.          00029200
002930                                                                  00029300
002940         10  WL-807-EXC4086-IND               PIC X(01).          00029400
002950           88  WL-807-EXC4086-POSTED          VALUE 'D'.          00029500
002960                                                                  00029600
002970         10  WL-807-EXC4082-IND               PIC X(01).          00029700
002980           88  WL-807-EXC4088-POSTED          VALUE 'E'.          00029800
002990                                                                  00029900
003000         10  WL-807-EXC4097-IND               PIC X(01).          00030000
003010           88  WL-807-EXC4097-POSTED          VALUE 'F'.          00030100
003020                                                                  00030200
003030         10  WL-807-EXC4099-IND               PIC X(01).          00030300
003040           88  WL-807-EXC4099-POSTED          VALUE 'G'.          00030400
003050                                                                  00030500
003060***********************************************************       00030600
003070***  ORIG-B-SYS-ID IS POPULATED BY PDDC8330, AND IT HOLDS**       00030700
003080***  THE ID FROM THE ORIGINAL CLAIM THAT IS TO BE CREDITED*       00030800
003090***********************************************************       00030900
003100                                                                  00031000
003110     05  WL-807-ORIG-B-SYS-ID           PIC S9(9) COMP            00031100
003120                                        VALUE ZEROES.             00031200
003130                                                                  00031300
003140     05 WL-807-1ST-CYCL-MASS-ADJ-SW     PIC X VALUE 'N'.          00031400
003150        88  WL-807-1ST-CYCL-MASS-ADJ-N        VALUE 'N'.          00031500
003160        88  WL-807-1ST-CYCL-MASS-ADJ-Y        VALUE 'Y'.          00031600
003170                                                                  00031700
003180     05 WL-807-PA-APPLIED-SW            PIC X VALUE ' '.          00031701
003190        88  WL-807-PA-WAS-APPLIED             VALUE 'Y'.          00031702
003200        88  WL-807-PA-NOT-APPLIED             VALUE 'N'.          00031703
003210                                                                  00031704
003220     05 WL-807-PARTIAL-SW               PIC X VALUE 'N'.          00031705
003230        88  WL-807-PARTIAL-NOT-FOUND          VALUE 'N'.          00031706
003240        88  WL-807-PARTIAL-FOUND              VALUE 'Y'.          00031707
003250                                                                  00031708
003260     05  WL-807-A-ID                    PIC S9(11) COMP-3.        00031709
003270                                                                  00031710
003280     05 WL-807-PXREFXTB-P-ALT-ID        PIC X(15).                00031720
003290
003300     05 WL-807-HIST-HDR-ALLOW-AMT       PIC S9(9)V9(2) COMP-3.          20
003310
003320     05 WL-807-HIST-TOT-TPL-AMT         PIC S9(9)V9(2) COMP-3.          20
003330
003340     05 WL-807-UPDATE-RX-SW             PIC X VALUE ' '.          00031701
003350        88  WL-807-UPDATE-RX                  VALUE 'Y'.          00031702
003360                                                                  00031704
003370***  FILLED BY PDDC8000 - POS REFORMATTER                         COR0272
003380     05 WL-807-INDIRECT-340B-SW         PIC X VALUE ' '.          COR0272
003390        88  WL-807-INDIRECT-340B              VALUE 'Y'.          COR0272
003400                                                                  COR0272
003410***  FILLED BY PDDC8420 - PARTICIPANT ELIGIBILITY                 COR1762
003420***  INDICATES WHETHER ELIGIBILITY WOULD HAVE POSTED              COR1762
003430***  MEDICARE B PAY OR DENY EXCEPTIONS                            COR1762
003440     05 WL-807-MEDB-COVERAGE-SW         PIC X VALUE ' '.          COR1762
003450        88  WL-807-MEDB-PAY                   VALUE 'N'.          COR1762
003460        88  WL-807-MEDB-DENY                  VALUE 'Y'.          COR1762
003400                                                                  SPA
003370***  FILLED BY PDDC8730 - PA PROGRAM                              SPA
003470     05  WL-807-INTERNAL-PA-OVRRD-SW       PIC X.                 SPA
003470             88  WL-807-INTERNAL-PA-OVRRD-Y VALUE 'Y'.            SPA
003470             88  WL-807-INTERNAL-PA-OVRRD-N VALUE 'N'.            SPA
003470                                                                  COR1762
003530***  FILLED BY MADC8530 - MASS SPECIFIC PRICING                   COR0429
003540***  ONLY STORED AT THE HEADER LEVEL, BECAUSE THIS VALUE IS       COR0429
003550***  OVERRIDEN FOR COMPOUND CLAIMS                                COR0429
003560     05  WL-807-FED-MAC-PRICE-IND       PIC X(01).                COR0429
003570         88  WL-807-FMAC-PRICE-NOT-FOUND  VALUE 'N' ' '.          COR0429
003580         88  WL-807-FMAC-PRICE-FOUND      VALUE 'Y'.              COR0429
003590                                                                  00036700
003600     05  WL-807-EMERGENCY-CLAIM-SW      PIC X(01).                COR0500
003610         88  WL-807-LOS-NOT-3             VALUE 'N'.              COR0500
003620         88  WL-807-EMERGENCY-CLAIM       VALUE 'I' 'O'.          COR0500
003630         88  WL-807-EMERGENCY-IN-HRS      VALUE 'I'.              COR0500
003640         88  WL-807-EMERGENCY-OUT-OF-HRS  VALUE 'O'.              COR0500
003650         88  WL-807-EMERGENCY-EDIT-POSTED VALUE 'E'.              COR0500
003660                                                                  COR0500
003661     05  WL-807-PTP-NCPDP-NUM           PIC X(15).                COR0377
003662                                                                  COR0500
003661     05  WL-807-MCARE-ELIG-CLAIM-SW     PIC X(01).                COR0763
003470         88  WL-807-1ST-CLAIM             VALUE '1'.              COR0763
003470         88  WL-807-2ND-CLAIM             VALUE '2'.              COR0763
003662                                                                  COR0763
007290     05 WL-807-MCARE-B-CVRG-SW          PIC X(01)  VALUE ' '.     COR0763
007300         88  WL-807-NO-MCARE-B-CVRG       VALUE 'N'.              COR0763
007310         88  WL-807-MCARE-B-CVRG          VALUE 'Y'.              COR0763
003400                                                                  COR0763
007290     05 WL-807-TYPE-17-CVRG-SW          PIC X(01)  VALUE ' '.     COR0763
007300         88  WL-807-NO-TYPE-17-CVRG       VALUE 'N'.              COR0763
007310         88  WL-807-TYPE-17-CVRG          VALUE 'Y'.              COR0763
003400                                                                  COR0763
007290     05 WL-807-MCARE-B-PROD-IND     PIC X(01) VALUE 'N'.          COR0763
007300         88  WL-807-MCARE-B-PROD-YES      VALUE 'Y'.              COR0763
007310         88  WL-807-MCARE-B-PROD-NO       VALUE 'N'.              COR0763
003400                                                                  COR0763
007290     05 WL-807-MCARE-D-PLAN-IND     PIC X(01) VALUE 'N'.          COR0763
007300         88  WL-807-MCARE-D-PLAN-YES      VALUE 'Y'.              COR0763
007310         88  WL-807-MCARE-D-PLAN-NO       VALUE 'N'.              COR0763
003400                                                                  COR0763
007290     05 WL-807-8510-PART-B-SWITCH   PIC X(01) VALUE 'N'.          COR0826
007300         88  WL-807-8510-PART-B-NO        VALUE 'N'.              COR0826
007310         88  WL-807-8510-PART-B-YES       VALUE 'Y'.              COR0826
003640                                                                  COR0962
003650     05 WL-807-HSNO-CLAIM-IND       PIC X(01) VALUE ' '.          COR0962
003650                                                                  COR0962
003650     05 WL-807-HSNO-REL-BEG-DT      PIC X(10) VALUE SPACES.       COR0962
003650                                                                  COR0962
003650     05 WL-807-PAY-TO-MEDICAID      PIC X(10) VALUE SPACES.       COR0962
003650                                                                  COR0962
003650     05 WL-807-BPLAN-COE-COA-CD     PIC X(02) VALUE SPACES.       COR0962
003660                                                                  COR0826
003110     05 WL-807-LEGACY-B-SYS-ID      PIC S9(9) COMP VALUE ZEROES.  00031100
003120                                                                  00031200
003110     05 WL-807-NEWMMIS-B-SYS-ID     PIC S9(9) COMP VALUE ZEROES.  00031100
003120                                                                  00031200
003110     05 WL-807-SUB-SYS-ID-SWITCH    PIC X(01) VALUE ' '.          00031100
007300         88  WL-807-SUB-SYS-ID-PARTIAL    VALUE 'P'.              COR0826
007310         88  WL-807-SUB-SYS-ID-ALL        VALUE 'S'.              COR0826
003120                                                                  00031200
003110     05 WL-807-EC4177-IND-SWITCH    PIC X(01) VALUE ' '.          COR1025
007300         88  WL-807-EC4177-IND-YES        VALUE 'Y'.              COR1025
007310         88  WL-807-EC4177-IND-NO         VALUE 'N'.              COR1025
003120                                                                  00031200
003690***********************************************************       00032000
003700***  THIS INDICATOR IS SPECIFIC TO MEDICARE C/D PLANS   ***       00032100
003700***  WHERE MASSHEALTH IS PRIMARY, ALSO KNOWN AS PSUEDO  ***       00032100
003700***  MEDB FOR COR1050.                                  ***       00032100
003690***********************************************************       00032000
007290     05 WL-807-PLAN007-IND          PIC X(01) VALUE 'N'.          COR1050
007300        88  WL-807-PLAN007-YES           VALUE 'Y'.               COR1050
007310        88  WL-807-PLAN007-NO            VALUE 'N'.               COR1050

007290     05 WL-807-QUAN-PRES-IND        PIC X(01) VALUE 'N'.          CR4825
007300        88  WL-807-QUAN-PRES-IND-YES     VALUE 'Y'.               CR4825
007310        88  WL-807-QUAN-PRES-IND-NO      VALUE 'N'.               CR4825
003400                                                                  COR1050
007290     05 WL-807-BIN-CARR-IND         PIC X(01) VALUE ' '.          COR1050
003400                                                                  COR1050
DEBUG *
DEBUG      05 WL-807-DEBUG-IND            PIC X(01) VALUE 'N'.         
DEBUG         88 WL-807-DEBUG-IS-ON             VALUE 'Y'.
DEBUG         88 WL-807-DEBUG-IS-OFF            VALUE 'N'.
DEBUG *
003110     05 WL-807-EC4764-IND-SWITCH    PIC X(01) VALUE ' '.          COR1068
007300         88  WL-807-EC4764-IND-YES        VALUE 'Y'.              COR1068
007310         88  WL-807-EC4764-IND-NO         VALUE 'N'.              COR1068
003400                                                                  COR1068
007290     05 WL-807-SUBM-DME-NDC         PIC X(11) VALUE ' '.          COR1068
      ***                                                               PML
           05  FILLER                          PIC X(01) VALUE ' '.     PML
                                                                        PML
      ***  SET BY PDDC8300.                                             PML
           05  WL-807-R-2PH-ADJUD-CD           PIC X(01) VALUE ' '.     PML
               88  WL-807-R-2PH-ADJUD-IS-OFF      VALUE 'N' ' '.        PML
               88  WL-807-R-2PH-ADJUD-PLAN        VALUE 'P'.            PML
               88  WL-807-R-2PH-ADJUD-GROUP       VALUE 'G'.            PML
               88  WL-807-R-2PH-ADJUD-IS-ON       VALUE 'P' 'G' 'Y'.    PML
                                                                        PML
      ***  SET BY PDDC8000 TO INDICATE 1ST OR 2ND PASS ADJUDICATION     SR1387
           05  WL-807-2PHASE-PASS-IND           PIC X(01) VALUE ' '.    SR1387
               88  WL-807-2PHASE-1ST-PASS          VALUE '1' ' '.       SR1387
PML            88  WL-807-2PHASE-PLAN1-RANK-99     VALUE '4'.
               88  WL-807-2PHASE-2ND-PASS          VALUE '2'.           SR1387
               88  WL-807-2PHASE-3RD-PASS          VALUE '3'.           SR1387
                                                                        SR1387
      *********INDICATOR TO SHOW IF PARTICIPANT IS IN LTC               MAPOPS3
           05 WL-807-LTC-SW               PIC X(01) VALUE 'N'.          MAPOPS3
              88 WL-807-CLAIM-LTC            VALUE 'Y'.                 MAPOPS3
              88 WL-807-CLAIM-IS-NOT-LTC     VALUE 'N'.                 MAPOPS3
                                                                        SR1387
PML        05 FILLER                      PIC X(02) VALUE SPACE.  
PML   ***  05 FILLER                      PIC X(03) VALUE SPACES.       CR10608
           05 WL-807-OTH-AMT-CLAIMED-GRP.                               CR10608
             10 WL-807-OTH-AMT-DERIVED  OCCURS 3 TIMES                  CR10608
000730                                            INDEXED BY            CR10608
000740                                            WL-807-OTH-AMT-IDX.   CR10608
000750                                                                  CR10608
000760                 20  WL-807-OTH-AMT-DERVD   PIC X(1).             CR10608
DF1066***  SET BY PDDC0426
DF1066     05 WL-807-SVC-LOC-CD-SUBM-IND  PIC X(01) VALUE SPACES.
DF1066        88  WL-807-SVC-LOC-CD-SUBM-Y          VALUE 'Y'.
DF1066        88  WL-807-SVC-LOC-CD-SUBM-N          VALUE 'N'.
DF1066     05 WL-807-LVL-OF-SVC-CD-SUBM-IND  PIC X(01) VALUE SPACES.
DF1066        88  WL-807-LVL-OF-SVC-CD-SUBM-Y       VALUE 'Y'.
DF1066        88  WL-807-LVL-OF-SVC-CD-SUBM-N       VALUE 'N'.
DF3180     05 WL-807-UNIT-OF-MSR-CD-SUBM-IND PIC X(01) VALUE SPACES.
DF3180        88  WL-807-UNIT-OF-MSR-CD-SUBM-Y      VALUE 'Y'.
DF3180        88  WL-807-UNIT-OF-MSR-CD-SUBM-N      VALUE 'N'.
DF3180     05 WL-807-REF-ID-CD-SUBM-IND      PIC X(01) VALUE SPACES.
DF3180        88  WL-807-REF-ID-CD-SUBM-Y           VALUE 'Y'.
DF3180        88  WL-807-REF-ID-CD-SUBM-N           VALUE 'N'.
DF3180     05 WL-807-PH-SVC-TY-CD-SUBM-IND   PIC X(01) VALUE SPACES.
DF3180        88  WL-807-PH-SVC-TY-CD-SUBM-Y        VALUE 'Y'.
DF3180        88  WL-807-PH-SVC-TY-CD-SUBM-N        VALUE 'N'.
DF2834     05 WL-807-DAW-CD-SUBM-IND         PIC X(01) VALUE SPACES.
DF2834        88  WL-807-DAW-CD-SUBM-Y              VALUE 'Y'.
DF2834        88  WL-807-DAW-CD-SUBM-N              VALUE 'N'.
DF3046     05 WL-807-RX-OVRRD-CD-SUBM-IND    PIC X(01) VALUE SPACES.
DF3046        88  WL-807-RX-OVRRD-CD-SUBM-Y         VALUE 'Y'.
DF3046        88  WL-807-RX-OVRRD-CD-SUBM-N         VALUE 'N'.
DF3046     05 WL-807-RX-OVR2-CD-SUBM-IND     PIC X(01) VALUE SPACES.
DF3046        88  WL-807-RX-OVR2-CD-SUBM-Y          VALUE 'Y'.
DF3046        88  WL-807-RX-OVR2-CD-SUBM-N          VALUE 'N'.
DF3046     05 WL-807-RX-OVR3-CD-SUBM-IND     PIC X(01) VALUE SPACES.
DF3046        88  WL-807-RX-OVR3-CD-SUBM-Y          VALUE 'Y'.
DF3046        88  WL-807-RX-OVR3-CD-SUBM-N          VALUE 'N'.
DF1066     05 WL-807-LVL-EFRT-CD-SUBM-IND-GR.
DF1066       10 WL-807-LVL-EFRT-CD-SUBM-IND  OCCURS 9 TIMES
                                          PIC X(01).
DF1066        88  WL-807-LVL-EFRT-CD-SUBM-Y         VALUE 'Y'.
DF1066        88  WL-807-LVL-EFRT-CD-SUBM-N         VALUE 'N'.
DF1165     05 WL-807-PARTIAL-FILL-DOS     PIC X(10) VALUE SPACES.
      ***  SET BY PDDCBENP - PDDC8505
           05 WL-807-MAX-REIMB-AMT        PIC S9(9)V99 COMP-3 VALUE +0.
DF1880     05 WL-807-MAX-REBILL-AMT       PIC S9(9)V99 COMP-3 VALUE +0.
DF2198     05 WL-807-R-MAX-COPAY-WRAP     PIC S9(9)V99 COMP-3 VALUE +0.
      ***  SET BY PDDC8130 FOR USE IN PDDC8000, 8010, 8020
           05  WL-807-R-GROUP-2ND-ID      PIC X(08) VALUE SPACES.       PML
           05  WL-807-R-PLAN-2ND-ID       PIC X(03) VALUE SPACES.       PML
           05  WL-807-R-GROUP-3RD-ID      PIC X(08) VALUE SPACES.       PML
           05  WL-807-R-PLAN-3RD-ID       PIC X(03) VALUE SPACES.       PML
      ***  FILLED IN BY PDDC8410 FOR PRICING NETWORK USE
DF2350     05  WL-807-R-PAY-TO-P-ID       PIC S9(9) COMP.
MDM        05  WL-807-R-RTS-ADMIN-FEE     PIC S9(9)V99 COMP-3 VALUE +0. MDM0823
DF1891     05  WL-807-TY-OF-TM-PER        PIC X(01) VALUE SPACES.
           05  WL-807-R-PAYTO-NETWORK     PIC X(06) VALUE SPACES.       COR3014
           05  WL-807-PHARM-COV-IND       PIC X(01) VALUE SPACES.       DF2952
           05  WL-807-FORCED-SECONDARY    PIC X(01) VALUE SPACES.       DF3071
               88  WL-807-FORCED-SECONDARY-Y        VALUE 'Y'.          DF3071
               88  WL-807-FORCED-SECONDARY-N        VALUE 'N'.          DF3071
           05  WL-807-REJECT-VALID-SW     PIC X(01) VALUE SPACES.       DF3071
               88  WL-807-REJECT-VALID-YES          VALUE 'Y'.          DF3071
               88  WL-807-REJECT-VALID-NO           VALUE 'N'.          DF3071
           05  WL-807-R-CARVEOUT-IND      PIC X(01) VALUE SPACES.       DF3490
               88  WL-807-CARVEOUT-YES              VALUE 'Y'.          DF3490
               88  WL-807-CARVEOUT-NO               VALUE 'N'.          DF3490
DF3701     05  WL-807-SL7031-INDIRECT-HEMO PIC X(01) VALUE SPACE.       RCAMPB
DF3701     05  WL-807-SL7031-DIRECT-PRELAT PIC X(01) VALUE SPACE.       RCAMPB
DF3736     05  WL-807-COPAY-FY-BEG-MO-NUM  PIC 9(2)  VALUE ZEROS.       DF3736
DF3736     05  WL-807-COPAY-TY-OF-TM-PER   PIC X(01) VALUE SPACES.      DF3736   
DF3736     05  WL-807-MAX-BENEFT-AMT       PIC S9(9)V99 COMP-3 VALUE +0.DF3736
DF3736     05  WL-807-B-BENEFIT-TY-CD      PIC X(2)  VALUE SPACES.      DF3736
DF3736     05  WL-807-R-BENE-BEG-DT        PIC X(10) VALUE SPACES.      DF3736
DF3736     05  WL-807-R-BENE-END-DT        PIC X(10) VALUE SPACES.      DF3736
CR4434***  SET BY PDDC8730 - PA OVERRIDE                                CR4434
CR4434     05  WL-807-PROCESS-3708-HIST    PIC X(1)  VALUE 'N'.         CR4434
CR6311     05  WL-807-PROCESS-4689-HIST    PIC X(1)  VALUE 'N'.         CR6311
CR6311     05  WL-807-PROCESS-3702-HIST    PIC X(1)  VALUE 'N'.         CR6311
           05  WL-807-3723-EXC-SW          PIC X(1)  VALUE SPACES.      DF7070
               88  WL-807-3723-EXC-YES              VALUE 'Y'.          DF7070
               88  WL-807-3723-EXC-NO               VALUE 'N'.          DF7070
           05  WL-807-3724-EXC-SW          PIC X(1)  VALUE SPACES.      DF7070
               88  WL-807-3724-EXC-YES              VALUE 'Y'.          DF7070
               88  WL-807-3724-EXC-NO               VALUE 'N'.          DF7070
           05  WL-807-3725-EXC-SW          PIC X(1)  VALUE SPACES.      DF7070
               88  WL-807-3725-EXC-YES              VALUE 'Y'.          DF7070
               88  WL-807-3725-EXC-NO               VALUE 'N'.          DF7070
           05  WL-807-3726-EXC-SW          PIC X(1)  VALUE SPACES.      DF7070
               88  WL-807-3726-EXC-YES              VALUE 'Y'.          DF7070
               88  WL-807-3726-EXC-NO               VALUE 'N'.          DF7070
           05  WL-807-3727-EXC-SW          PIC X(1)  VALUE SPACES.      DF7070
               88  WL-807-3727-EXC-YES              VALUE 'Y'.          DF7070
               88  WL-807-3727-EXC-NO               VALUE 'N'.          DF7070
CR4434*    05 FILLER                      PIC X(39) VALUE SPACES.       CR4434
CR6311*    05 FILLER                      PIC X(06) VALUE SPACES.       DF7070	
DF7070*    05 FILLER                      PIC X(01) VALUE SPACES.       CR11078
           05 WL-807-SCHDL-II-CPY-RESET   PIC X(01) VALUE SPACES.       CR11078
003680                                                                  00031900
003690***********************************************************       00032000
003700***  THE AREA BELOW HOLDS DATA THAT IS CLAIM DETAIL/    ***       00032100
003710***  LINE ITEM-RELATED.  IT OCCURS THE SAME NUMBER OF   ***       00032200
003720***  TIMES AS THERE ARE CLAIM DETAILS AND HOLDS BOTH    ***       00032300
003730***  TABLE AND NON-TABLE RELATED DATA.                  ***       00032400
003740***********************************************************       00032500
003750                                                                  00032600
003760     05  WL-807-LINE-ITEM-RELATED-AREA.                           00032700
003770         10  WL-807-LINE-ITEM-DATA  OCCURS 40 TIMES               00032800
003780                                    INDEXED BY WL-807-LI-IDX.     00032900
003790                                                                  00033000
003800          12 WL-807-LI-SWITCHES.                                  00033100
003810***  SET BY PDDC8440 - PRIOR AUTHORIZATION                        00033200
003820             15  WL-807-PA-MATCH-SW           PIC X(01).          00033300
003830                 88  WL-807-PA-MATCHED        VALUE 'Y'.          00033400
003840                 88  WL-807-PA-NOT-MATCHED    VALUE 'N'.          00033500
003850                                                                  00033600
003860             15  WL-807-ALRGY-IND             PIC X(01).          00033700
003870                 88  WL-807-ALRGY             VALUE 'Y'.          00033800
003880                 88  WL-807-NO-ALRGY          VALUE 'N'.          00033900
003890                                                                  00034000
I2SPL***** SET BY PDDC8520 - SPECIALTY DRUG LOOKUP
I2SPL              15  WL-807-SDRUG-IND       PIC X(01) VALUE ' '.
I2SPL                  88  WL-807-SDRUG-IND-BOTH         VALUE 'X'.
I2SPL                  88  WL-807-SDRUG-IND-PRSC         VALUE 'Y'.
I2SPL                  88  WL-807-SDRUG-IND-PHRM         VALUE 'Z'.
I2SPL****** SET BY PDDC8520 - SPECIALTY DRUG PROVIDER
I2SPL              15  WL-807-SDRUG-PH-IND-SW    PIC X(01) VALUE 'N'.
I2SPL                  88  WL-807-SDRUG-PH-IND-YES      VALUE 'Y'.
I2SPL                  88  WL-807-SDRUG-PH-IND-NO       VALUE 'N'.
I2SPL******* SET BY PDDC8520 SPECIALTY DRUG PRESCRIBER
I2SPL              15  WL-807-SDRUG-PRSC-IND-SW   PIC X(01) VALUE 'N'.
I2SPL                  88  WL-807-SDRUG-PRSC-IND-YES    VALUE 'Y'.
I2SPL                  88  WL-807-SDRUG-PRSC-IND-NO     VALUE 'N'.
003900***  SET BY PDDC8610 - DUPLICATE CHECK                            00034100
003910             15  WL-807-FATAL-DUPLICATE-SW         PIC X(01).     00034200
003920                 88  WL-807-FATAL-DUPE-POSTED-YES  VALUE 'Y'.     00034300
003930                 88  WL-807-FATAL-DUPE-POSTED-NO   VALUE 'N'.     00034400
003940                                                                  00034500
003950***  SET BY VARIOUS                                               00034600
003960             15  WL-807-FATAL-DTL-EDIT-EXC-IND     PIC X(01).     00034700
003970                 88  WL-807-NO-FATAL-DTL-EDITS     VALUE ' '.     00034800
003980                 88  WL-807-FATAL-NDC-EDIT-SET     VALUE 'D'.     00034900
003990                 88  WL-807-CANT-PRICE-LI-EDIT-SET VALUE '$'.     00035000
004000                                                                  00035100
004010***  SET BY PDDC8520 - DETAIL PRICING EDITS                       00035200
004020             15  WL-807-DRUG-COVERAGE-IND         PIC X(01).      00035300
004030                 88  WL-807-DRUG-IS-NOT-COVERED   VALUE 'N' ' '.  00035400
004040                 88  WL-807-DRUG-IS-COVERED       VALUE 'Y'.      00035500
004050                                                                  00035600
004060***  SET BY PDDC8520 - DETAIL PRICING EDITS                       00035700
004070             15  WL-807-CUSTOM-PLAN-REC-IND       PIC X(01).      00035800
004080                 88  WL-807-CUSTOM-PLAN-NOT-FOUND VALUE 'N' ' '.  00035900
004090                 88  WL-807-CUSTOM-PLAN-FOUND     VALUE 'Y'.      00036000
004100                                                                  00036100
004110***  SET BY PDDC8510 - GET DRUG PRICES - CLAIM NDC                00036200
004120***  SET BY PDDC8515 - GET DRUG PRICES - STD PKG NDC              00036300
004130             15  WL-807-STATE-MAC-PRICE-IND       PIC X(01).      00036400
004140                 88  WL-807-SMAC-PRICE-NOT-FOUND  VALUE 'N' ' '.  00036500
004150                 88  WL-807-SMAC-PRICE-FOUND      VALUE 'Y'.      00036600
004160                                                                  00036700
004170***  SET BY PDDC8520 - DETAIL PRICING EDITS                       00036800
004180             15  WL-807-LI-COVERAGE-EXC-IND         PIC X(01).    00036900
004190                 88  WL-807-LI-COVERAGE-EXC-NOT-SET VALUE 'N' ' '.00037000
004200                 88  WL-807-LI-COVERAGE-EXC-SET     VALUE 'Y'.    00037100
004210                                                                  00037200
004220                                                                  00037300
004230          12 WL-807-LI-WORKAREA.                                  00037400
004240***  FILLED BY PDDC8430 - PLAN OVERRIDES                          00037500
004250             15  WL-807-R-STND-PKG-NDC-ID     PIC X(11).          00037600
004260***  FILLED BY PDDC8520 - DETAIL PRICING EDITS                    00037700
004270             15  WL-807-MIN-AGE-LIMIT         PIC S9(4) COMP.     00037800
004280             15  WL-807-MAX-AGE-LIMIT         PIC S9(4) COMP.     00037900
004290             15  WL-807-MAX-NUM-REFILLS-LIMIT PIC S9(4) COMP.     00038000
004300                                                                  00038100
004310***  FILLED BY DUR PROCESSING                                     00038200
004320             15  WL-807-DUR-GRACE-PERIOD    PIC 9(3)     COMP-3.  00038300
004330             15  WL-807-DUR-AMT-LIM-TOT     PIC S9(9)V99 COMP-3.  00038400
004340                                                                  00038500
004350*-------------------------------*                                 00038600
004360*    DRUG MASTER                *                                 00038700
004370*-------------------------------*                                 00038800
004380          12 WL-807-RDDRUGTB-FIELDS.                              00038900
004390***  OVERLAYED BY PDDC8515 - GET DRUG PRICES - STD PKG NDC        00039000
004400             15  WL-807-WHICH-NDC-USED-IND    PIC X(01).          00039100
004410                 88  WL-807-REG-PRICE-NOT-FOUND VALUE ' '.        00039200
004420                 88  WL-807-CLAIM-NDC-DATA      VALUE 'C'.        00039210
004430                 88  WL-807-STND-PKG-NDC-DATA   VALUE 'S'.        00039300
004440                                                                  00039400
004450***  FILLED    BY PDDC8430 - PLAN OVERRIDES                       00039500
004460***  OVERLAYED BY PDDC8515 - GET DRUG PRICES - STD PKG NDC        00039600
004470             15  WL-807-R-DRUG-FM-CD          PIC X(01).          00039700
004472             15  WL-807-R-DRUG-PKG-SZ-AMT     PIC S9(8)V9(3)      COR0636
004474                                                         COMP-3.  COR0636
004476             15  WL-807-R-GENR-MAND-EX-IND    PIC X(01).          COR0636
004478             15  WL-807-R-BRND-GENR-CD        PIC X(01).          COR0656
004478             15  WL-807-R-DRUG-CVRG-IND       PIC X(01).          SPA
004478             15  WL-807-R-DME-EXISTS-IND      PIC X VALUE SPACES. COR0692
                       88  WL-807-R-DME-EXISTS            VALUE 'Y'.    COR0692
004490             15  WL-807-R-DRUG-CAT-CD         PIC X(01).          00039800
004500             15  WL-807-R-DRUG-CLS-CD         PIC X(01).          00039900
004510             15  WL-807-R-DRUG-GENR-IND       PIC X(01).          00040000
004520             15  WL-807-R-HCFA-TERM-DT        PIC X(10).          00040100
004530             15  WL-807-R-GENR-AVAIL-IND      PIC X(01).          00040200
004540             15  WL-807-R-STND-THERA-CD       PIC X(02).          00040300
004550             15  WL-807-R-DRUG-UNT-DOSE-CD    PIC X(01).          00040400
004560             15  WL-807-R-DRUG-ORNG-BK-CD     PIC X(02).          00040500
004570             15  WL-807-R-DRUG-DESI-CD        PIC X(01).          00040600
004580             15  WL-807-R-DRUG-DESI-DT        PIC X(10).          00040700
004590             15  WL-807-R-DRUG-FMLRY-CD       PIC X(01).          00040800
004600             15  WL-807-R-STD-PKG-SZ-CD       PIC X(02).          00040900
004610             15  WL-807-R-MCARE-CVRG-IND      PIC X(01).          00041000
004620             15  WL-807-R-DRUG-HICL-CD        PIC X(06).          COR0632
003440             15  WL-807-PLAN007-STATUS-SW     PIC X(01) VALUE ' '.
                       88  WL-807-PLAN007-PARTB               VALUE 'B'.
                       88  WL-807-PLAN007-COVERED             VALUE 'C'.
                       88  WL-807-PLAN007-PRIMARY             VALUE 'P'.
                       88  WL-807-PLAN007-SECONDARY           VALUE 'S'.
                       88  WL-807-PLAN007-COB                 VALUE 'T'.
                       88  WL-807-PLAN007-IMPLIED             VALUE 'I'.
                   15  WL-807-PLAN008-STATUS-SW     PIC X(01) VALUE ' '.
                       88  WL-807-PLAN008-PARTB               VALUE 'B'.
                       88  WL-807-PLAN008-COVERED             VALUE 'C'.
                       88  WL-807-PLAN008-PRIMARY             VALUE 'P'.
                       88  WL-807-PLAN008-SECONDARY           VALUE 'S'.
                       88  WL-807-PLAN008-COB                 VALUE 'T'.
                       88  WL-807-PLAN008-IMPLIED             VALUE 'I'.
                   15  WL-807-PLAN009-STATUS-SW     PIC X(01) VALUE ' '.
                       88  WL-807-PLAN009-PARTB               VALUE 'B'.
                       88  WL-807-PLAN009-COVERED             VALUE 'C'.
                       88  WL-807-PLAN009-PRIMARY             VALUE 'P'.
                       88  WL-807-PLAN009-SECONDARY           VALUE 'S'.
                       88  WL-807-PLAN009-COB                 VALUE 'T'.
                       88  WL-807-PLAN009-IMPLIED             VALUE 'I'.
                   15  WL-807-MCARE-WRAP-SW         PIC X(01) VALUE ' '.
                       88  WL-807-MCARE-WRAP                  VALUE 'Y'.
                       88  WL-807-NO-MCARE-WRAP               VALUE 'N'.
                   15  WL-807-MCARE-D-PAY-SW        PIC X(01) VALUE ' '.
                       88  WL-807-MCARE-D-PAY                 VALUE 'P'.
                       88  WL-807-MCARE-D-DENY                VALUE 'D'.
007290             15  WL-807-4595-IGNORE-CD       PIC X(01) VALUE ' '. COR0763
007300                 88  WL-807-4595-IGNORE-SUPL           VALUE 'S'. COR0763
007310                 88  WL-807-4595-IGNORE-WRAP           VALUE 'W'. COR0763
004630             15  FILLER                       PIC X(01).          COR0465
004630             15  WL-807-R-AUX-PLAN-STAT-CD    PIC X(01).          COR0465
004640             15  WL-807-R-RATED-GSN-CD        PIC X(01).          00041300
004650             15  WL-807-R-ADULT-DUR-AMT       PIC S9(3)V COMP-3.  00041400
004660             15  WL-807-R-GERI-DUR-AMT        PIC S9(3)V COMP-3.  00041500
004670             15  WL-807-R-PEDI-DUR-AMT        PIC S9(3)V COMP-3.  00041600
004680             15  WL-807-R-HCFA-MARKET-DT      PIC X(10).          00041700
                   15  WL-807-R-PRC-NUM             PIC X(02).
                   15  WL-807-CPY-BRND-GENR-CD      PIC X(01).
                   15  WL-807-CPY-GENR-CALC-CD      PIC X(01).
                   15  WL-807-R-DRUG-ORIG-CLS-CD    PIC X(01).          COR9855
004630             15  FILLER                       PIC X(19).          COR9855
004630*            15  FILLER                       PIC X(20).          COR9855
004690                                                                  00041800
004700*-------------------------------*                                 00041900
004710*    DRUG FORMULARY             *                                 00042000
004720*-------------------------------*                                 00042100
004730***  FILLED BY PDDC8520 - DETAIL PRICING EDITS                    00042200
004740          12 WL-807-RDFMLYTB-FIELDS.                              00042300
004750             15  WL-807-R-DRUG-FMLY-BEG-DT    PIC X(10).          00042400
004760             15  WL-807-R-DRUG-FMLY-END-DT    PIC X(10).          00042500
004770             15  WL-807-R-FMLY-PARM-CD        PIC X(04).          00042600
004780             15  WL-807-R-FMLY-CVRG-IND       PIC X(01).          00042700
004790                                                                  00042800
004800*----------------------------------*                              00042900
004810*    PLAN BENEFIT LIMITS - STANDARD*                              00043000
004820*----------------------------------*                              00043100
004830***  FILLED BY PDDC8520 - DETAIL PRICING EDITS                    00043200
004840          12 WL-807-RPBLMTTB-FIELDS.                              00043300
004850             15  WL-807-R-PBL-TYP-CD          PIC X(1).           00043400
004860             15  WL-807-R-PBL-RNGE-BEG-CD     PIC X(11).          00043500
004870             15  WL-807-R-PBL-RNGE-END-CD     PIC X(11).          00043600
004880             15  WL-807-R-PBL-LMT-STAT-CD     PIC X(1).           00043700
004890             15  WL-807-R-PBL-OVRRD-PA-CD     PIC X(1).           00043800
004900             15  WL-807-R-PBL-MED-CERT-IND    PIC X(1).           00043900
004910                                                                  00044000
004920*----------------------------------*                              00044100
004930*    PLAN BENEFIT LIMITS - CUSTOM  *                              00044200
004940*----------------------------------*                              00044300
004950***  FILLED BY PDDC8520 - DETAIL PRICING EDITS                    00044400
004960          12  WL-807-RPBLCSTB-FIELDS.                             00044500
004970             15  WL-807-R-CUSTOM-ID           PIC S9(9) COMP.     00044600
004980             15  WL-807-R-BRND-GNRIC-CD       PIC X(1).           00044700
004990             15  WL-807-R-MAINT-DOSE-NUM      PIC S9(6)V9(3)      00044800
005000                                                        COMP-3.   00044900
                   15  WL-807-R-MAINT-DOSENUM-SC PIC X(1) VALUE SPACES. MAPOPS3
005010             15  WL-807-R-MAINT-DOSE-CD       PIC X(1).           00045000
005020             15  WL-807-R-PBL-MAX-AGE         PIC S9(4) COMP.     00045100
005030             15  WL-807-R-PBL-MIN-AGE         PIC S9(4) COMP.     00045200
                   15  WL-807-R-COPAY-OVRD-SC       PIC X(1).           MAPOPS3
005040             15  WL-807-R-PBL-AGE-STAT-CD     PIC X(1).           00045300
005050             15  WL-807-R-PBL-SEX-CD          PIC X(1).           00045400
                   15  WL-807-R-PBL-SEX-SC          PIC X(1).           MAPOPS3
005060             15  WL-807-R-PBL-MAX-DOSE-NUM    PIC S9(6)V9(3)      00045500
005070                                                        COMP-3.   00045600
                   15  WL-807-R-PBL-MAX-DOSE-SC PIC X(1)                MAPOPS3
                                                     VALUE SPACES.      MAPOPS3
005080             15  WL-807-R-PBL-MIN-DOSE-NUM    PIC S9(6)V9(3)      00045700
005090                                                        COMP-3.   00045800
                   15  WL-807-R-PBL-MIN-DOSE-SC PIC X(1)                MAPOPS3
                                                     VALUE SPACES.      MAPOPS3
005100             15  WL-807-R-PBL-DAY-SUBM-NUM    PIC S9(4) COMP.     00045900
                   15  WL-807-R-PBL-DAY-SUBM-NUM-SC PIC X(1).           MAPOPS3
                   15  WL-807-R-PBL-DAY-SUBM-EXC    PIC X(4).           MAPOPS3
005110             15  WL-807-R-PBL-DAYS-STAT-CD    PIC X(1).           00046000
005120             15  WL-807-R-PBL-DAYS-ACUM-CD    PIC X(1).           00046100
005130             15  WL-807-R-UNITS-LMT-NUM       PIC S9(6)V9(3)      00046200
005140                                                        COMP-3.   00046300
                   15  WL-807-R-UNITS-LMT-NUM-SC    PIC X(1).           MAPOPS3
005150             15  WL-807-R-UNITS-STAT-CD       PIC X(1).           00046400
005160             15  WL-807-R-MAX-UNIT-ACUM-CD    PIC X(1).           00046500
005170             15  WL-807-R-DUR-UNIT-LMT-NUM    PIC S9(6)V9(3)      00046600
005180                                                        COMP-3.   00046700
005190             15  WL-807-R-DUR-UNIT-STAT-CD    PIC X(1).           00046800
005200             15  WL-807-R-DUR-UNIT-ACUM-CD    PIC X(1).           00046900
005210             15  WL-807-R-DUR-SPAN-NUM        PIC S9(4) COMP.     00047000
005220             15  WL-807-R-DUR-UNIT-SPAN-CD    PIC X(1).           00047100
005230             15  WL-807-R-DUR-CVRG-CD         PIC X(1).           00047200
005240             15  WL-807-R-DAYS-SPLY-NUM       PIC S9(4) COMP.     00047300
                   15  WL-807-R-DAYS-SPLY-NUM-SC    PIC X(1).           MAPOPS3
005250             15  WL-807-R-DAY-SPLY-STAT-CD    PIC X(1).           00047400
005260             15  WL-807-R-DAY-SPLY-ACUM-CD    PIC X(1).           00047500
005270             15  WL-807-R-DAY-SPLY-SPN-NUM    PIC S9(4) COMP.     00047600
005280             15  WL-807-R-DAY-SPLY-SPN-CD     PIC X(1).           00047700
005290             15  WL-807-R-DAY-SPLY-CVRG-CD    PIC X(1).           00047800
005300             15  WL-807-R-PBL-AMT-LMT         PIC S9(9)V9(2)      00047900
005310                                                        COMP-3.   00048000
005320             15  WL-807-R-AMT-LMT-STAT-CD     PIC X(1).           00048100
005330             15  WL-807-R-AMT-LMT-ACUM-CD     PIC X(1).           00048200
005340             15  WL-807-R-AMT-LMT-SPN-NUM     PIC S9(4) COMP.     00048300
005350             15  WL-807-R-AMT-LMT-SPN-CD      PIC X(1).           00048400
005360             15  WL-807-R-AMT-LMT-CVRG-CD     PIC X(1).           00048500
005370             15  WL-807-R-PBL-MAX-RX-NUM      PIC S9(4) COMP.     00048600
                   15  WL-807-R-PBL-MAX-RXNUM-SC   PIC X(1).            MAPOPS3
005380             15  WL-807-R-MAX-RX-STAT-CD      PIC X(1).           00048700
005390             15  WL-807-R-MAX-RX-ACUM-CD      PIC X(1).           00048800
005400             15  WL-807-R-MAX-RX-SPN-NUM      PIC S9(4) COMP.     00048900
005410             15  WL-807-R-MAX-RX-SPN-CD       PIC X(1).           00049000
005420             15  WL-807-R-MAX-RX-CVRG-CD      PIC X(1).           00049100
005430             15  WL-807-R-COPAY-RTL-AMT       PIC S9(7)V9(2)      00049200
005440                                                        COMP-3.   00049300
005450             15  WL-807-R-ADDL-RTL-AMT        PIC S9(7)V9(2)      00049400
005460                                                        COMP-3.   00049500
005470             15  WL-807-R-ADDL-RTL-PCT        PIC S9(1)V9(4)      00049600
005480                                                        COMP-3.   00049700
005490             15  WL-807-R-MAIL-ORD-AMT        PIC S9(7)V9(2)      00049800
005500                                                        COMP-3.   00049900
005510             15  WL-807-R-ADDL-MAIL-AMT       PIC S9(7)V9(2)      00050000
005520                                                        COMP-3.   00050100
005530             15  WL-807-R-ADDL-MAIL-PCT       PIC S9(1)V9(4)      00050200
005540                                                        COMP-3.   00050300
                   15  WL-807-R-MAIL-SC             PIC X(1).           MAPOPS3
005550             15  WL-807-R-RX-LMT-EXMT-IND     PIC X(1).           00050400
005560             15  WL-807-R-REFLL-EXMT-IND      PIC X(1).           00050500
005570             15  WL-807-R-DUPCK-EXMT-IND      PIC X(1).           00050600
005580             15  WL-807-R-MAINT-DRUG-CD       PIC X(1).           00050700
005590             15  WL-807-R-MAX-REFLL-NUM       PIC S9(4) COMP.     00050800
005600             15  WL-807-R-PBL-SUPPLY-CD       PIC X(1).           00050900
005610                                                                  00051000
005620*-------------------------------*                                 00051100
005630*    DRUG PRICING TABLE         *                                 00051200
005640*-------------------------------*                                 00051300
005650***  FILLED    BY PDDC8510 - GET DRUG PRICES - CLAIM NDC          00051400
005660***  OVERLAYED BY PDDC8515 - GET DRUG PRICES - STD PKG NDC        00051500
005670          12 WL-807-RDPRCETB-FIELDS.                              00051600
005680             15  WL-807-REG-AVG-WHLSL-PRICE      PIC S9(6)V9(5)   00051700
005690                                                        COMP-3.   00051800
005700             15  WL-807-REG-AVG-SALE-PRICE       PIC S9(6)V9(5)   00051900
005710                                                        COMP-3.   00052000
005720             15  WL-807-REG-DIRECT-PRICE         PIC S9(6)V9(5)   00052100
005730                                                        COMP-3.   00052200
005740             15  WL-807-REG-FMAC-PRICE           PIC S9(6)V9(5)   00052300
005750                                                        COMP-3.   00052400
005760             15  WL-807-REG-SMAC-PRICE           PIC S9(6)V9(5)   00052500
005770                                                        COMP-3.   00052600
005780             15  WL-807-REG-MEDICAID-AWP         PIC S9(6)V9(5)   00052700
005790                                                        COMP-3.   00052800
005800             15  WL-807-REG-WHLSL-NETPKG-PRICE   PIC S9(6)V9(5)   00052900
005810                                                        COMP-3.   00053000
005820             15  WL-807-REG-WHLSL-NETUNIT-PRICE  PIC S9(6)V9(5)   00053100
005830                                                        COMP-3.   00053200
005820             15  WL-807-REG-NADAC-BRND-PRICE     PIC S9(6)V9(5)   COR6915
005830                                                        COMP-3.   COR6915
005820             15  WL-807-REG-NADAC-GENR-PRICE     PIC S9(6)V9(5)   COR6915
005830                                                        COMP-3.   COR6915
005620*---------------------------------------------------------------* MAPOPS3
005630*    custom plan secondary payer data   FILLED IN BY PDDC8520   * MAPOPS3
005640*---------------------------------------------------------------* MAPOPS3
005670          12 WL-807-2ND-PAYER-FIELDS.                             MAPOPS3
005680             15 WL-807-S-PBL-DAYS-STAT-CD       PIC X(1).         MAPOPS3
                   15 WL-807-S-PBL-DAYS-ACUM-CD       PIC X(1).         MAPOPS3
                   15 WL-807-S-UNITS-STAT-CD          PIC X(1).         MAPOPS3
                   15 WL-807-S-MAX-UNIT-ACUM-CD       PIC X(1).         MAPOPS3
                   15 WL-807-S-DUR-UNIT-LMT-NUM  PIC S9(6)V9(3) COMP-3. MAPOPS3
                   15 WL-807-S-DUR-UNIT-STAT-CD       PIC X(1).         MAPOPS3
                   15 WL-807-S-DUR-CVRG-CD            PIC X(1).         MAPOPS3
                   15 WL-807-S-DAY-SPLY-STAT-CD       PIC X(1).         MAPOPS3
                   15 WL-807-S-DAY-SPLY-ACUM-CD       PIC X(1).         MAPOPS3
                   15 WL-807-S-DAY-SPLY-CVRG-CD       PIC X(1).         MAPOPS3
                   15 WL-807-S-AMT-LMT-STAT-CD        PIC X(1).         MAPOPS3
                   15 WL-807-S-AMT-LMT-ACUM-CD        PIC X(1).         MAPOPS3
                   15 WL-807-S-AMT-LMT-CVRG-CD        PIC X(1).         MAPOPS3
                   15 WL-807-S-MAX-RX-STAT-CD         PIC X(1).         MAPOPS3
                   15 WL-807-S-MAX-RX-ACUM-CD         PIC X(1).         MAPOPS3
                   15 WL-807-S-MAX-RX-CVRG-CD         PIC X(1).         MAPOPS3
                   15 WL-807-S-RX-LMT-EXMT-IND        PIC X(1).         MAPOPS3
                   15 WL-807-S-REFLL-EXMT-IND         PIC X(1).         MAPOPS3
                   15 WL-807-S-DUPCK-EXMT-IND         PIC X(1).         MAPOPS3
                   15 WL-807-S-MAINT-DRUG-CD          PIC X(1).         MAPOPS3
                   15 WL-807-S-PBL-SUPPLY-CD          PIC X(1).         MAPOPS3
                   15 WL-807-S-PBL-DAY-SUBM-NUM       PIC S9(4) COMP.   MAPOPS3
                   15 WL-807-S-PBL-DAY-SUBM-NUM-SC    PIC X(1).         MAPOPS3
                   15 WL-807-S-PBL-DAY-SUBM-EXC   PIC X(4).             MAPOPS3
                   15 WL-807-S-PBL-DAYS-MIN-NUM       PIC S9(4) COMP.   MAPOPS3
                   15 WL-807-S-PBL-DAYS-MIN-NUM-SC    PIC X(1).         MAPOPS3
                   15 WL-807-S-PBL-DAYS-MIN-EXC   PIC X(4).             MAPOPS3
                   15 WL-807-S-UNITS-LMT-NUM     PIC S9(6)V9(3) COMP-3. MAPOPS3
                   15 WL-807-S-UNITS-LMT-NUM-SC       PIC X(1).         MAPOPS3
                   15 WL-807-S-PBL-UNITS-MIN-NUM PIC S9(6)V9(3) COMP-3. MAPOPS3
                   15 WL-807-S-PBL-UNITS-MIN-EXC PIC X(04).             MAPOPS3
                   15 WL-807-S-PBL-UNITS-MIN-SC  PIC X(01).             MAPOPS3
                   15 WL-807-S-PBL-AMT-LMT       PIC S9(9)V9(2) COMP-3. MAPOPS3
                   15 WL-807-S-AMT-LMT-SPN-NUM   PIC S9(4) COMP.        MAPOPS3
                   15 WL-807-S-AMT-LMT-SPN-CD    PIC X(1).              MAPOPS3
                   15 WL-807-S-DAYS-SPLY-NUM     PIC S9(4) COMP.        MAPOPS3
                   15 WL-807-S-DAYS-SPLY-NUM-SC  PIC X(1).              MAPOPS3
                   15 WL-807-S-DAY-SPLY-SPN-NUM  PIC S9(4) COMP.        MAPOPS3
                   15 WL-807-S-DAY-SPLY-SPN-CD   PIC X(1).              MAPOPS3
                   15 WL-807-S-DUR-UNIT-ACUM-CD  PIC X(1).              MAPOPS3
                   15 WL-807-S-DUR-SPAN-NUM      PIC S9(4) COMP.        MAPOPS3
                   15 WL-807-S-DUR-UNIT-SPAN-CD  PIC X(1).              MAPOPS3
                   15 WL-807-S-PBL-MAX-RX-NUM    PIC S9(4) COMP.        MAPOPS3
                   15 WL-807-S-PBL-MAX-RXNUM-SC PIC X(1).               MAPOPS3
                   15 WL-807-S-MAX-RX-SPN-NUM    PIC S9(4) COMP.        MAPOPS3
                   15 WL-807-S-MAX-RX-SPN-CD     PIC X(1).              MAPOPS3
                   15 WL-807-S-PBL-MAX-DOSE-NUM  PIC S9(6)V9(3) COMP-3. MAPOPS3
                   15 WL-807-S-PBL-MAX-DOSE-SC PIC X(1)                 MAPOPS3
                                                     VALUE SPACES.      MAPOPS3
                   15 WL-807-S-PBL-MIN-DOSE-NUM  PIC S9(6)V9(3) COMP-3. MAPOPS3
                   15 WL-807-S-PBL-MIN-DOSE-SC PIC X(1)                 MAPOPS3
                                                     VALUE SPACES.      MAPOPS3
                   15 WL-807-S-MAINT-DOSE-NUM    PIC S9(6)V9(3) COMP-3. MAPOPS3
                   15 WL-807-S-MAINT-DOSENUM-SC PIC X(1)  VALUE SPACES. MAPOPS3
                   15 WL-807-S-MAINT-DOSE-CD     PIC X(1).              MAPOPS3
                   15 WL-807-S-MAX-REFLL-NUM     PIC S9(4) COMP.        MAPOPS3
                   15 WL-807-R-SN-NETWORK-ID     PIC X(06) VALUE SPACES.MAPOPS3
                   15 WL-807-R-SN-NET-PRIM-IND   PIC X(01) VALUE SPACES.MAPOPS3
                   15 WL-807-R-SN-NET-CVRG-CD    PIC X(01) VALUE SPACES.MAPOPS3
                   15 WL-807-R-SN-CLM-EXC        PIC X(04) VALUE SPACES.MAPOPS3
004630             15 WL-807-R-PBL-MAX-SPN-CD    PIC X(01) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MAX-AGE-EXC   PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MAX-AGE-SC    PIC X(1) VALUE SPACES. MAPOPS3
                   15 WL-807-R-PBL-MIN-SPN-CD    PIC X(01) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MIN-AGE-EXC   PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MIN-AGE-SC    PIC X(1) VALUE SPACES. MAPOPS3
      *            15 WL-807-R-PBL-SEX-CD-EXC    PIC X(01) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-SEX-CD-EXC    PIC X(04) VALUE SPACES.CR14166
                   15 WL-807-R-COMM-MBR-IND      PIC X(01) VALUE SPACES.MAPOPS3
                   15 WL-807-R-COMM-MBR-IND-EXC  PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-COMM-MBR-IND-SC   PIC X(01) VALUE SPACE. MAPOPS3
                   15 WL-807-R-NH-MBR-IND        PIC X(01) VALUE SPACES.MAPOPS3
                   15 WL-807-R-NH-MBR-IND-EXC    PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-NH-MBR-IND-SC     PIC X(01) VALUE SPACE. MAPOPS3
                   15 WL-807-R-PBL-DAYS-MIN-NUM  PIC S9(9) USAGE COMP.  MAPOPS3
                   15 WL-807-R-PBL-DAYS-MIN-NUM-SC PIC X(1) VALUE SPACE.MAPOPS3
                   15 WL-807-R-PBL-DAYS-MIN-EXC  PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-UNITS-LMT-NUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-UNITS-LMT-NUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-UNITS-MIN-NUM PIC S9(6)V9(3) COMP-3. MAPOPS3
                   15 WL-807-R-PBL-UNITS-MIN-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-UNITS-MIN-SC  PIC X(01) VALUE SPACE. MAPOPS3
                   15 WL-807-R-PBL-AMT-LMT-EXC   PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-AMT-LMT-SC    PIC X(01) VALUE SPACE. MAPOPS3
                   15 WL-807-S-PBL-AMT-LMT-EXC   PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-PBL-AMT-LMT-SC    PIC X(01) VALUE SPACE. MAPOPS3
                   15 WL-807-R-DAYS-SPLY-NUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-DAYS-SPLY-NUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-DUR-UNIT-ACUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-DUR-UNIT-ACUM-SC PIC X(01) VALUE SPACE.  MAPOPS3
                   15 WL-807-S-DUR-UNIT-ACUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-DUR-UNIT-ACUM-SC PIC X(01) VALUE SPACE.  MAPOPS3
                   15 WL-807-R-PBL-MAX-RXNUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-PBL-MAX-RXNUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MAX-DOSE-EXC  PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-PBL-MAX-DOSE-EXC  PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MIN-DOSE-EXC  PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-PBL-MIN-DOSE-EXC  PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-MAINT-DOSENUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-MAINT-DOSENUM-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MAX-REFLL-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-S-PBL-MAX-REFLL-EXC PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PBL-MAX-REFLL-SC PIC X(01) VALUE SPACE.  MAPOPS3
                   15 WL-807-S-PBL-MAX-REFLL-SC PIC X(01) VALUE SPACE.  MAPOPS3
                   15 WL-807-R-PROC-MOD-1-CD     PIC X(02) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PROC-MOD-2-CD     PIC X(02) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PROC-MOD-3-CD     PIC X(02) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PROC-MOD-4-CD     PIC X(02) VALUE SPACES.MAPOPS3
MDM-I2             15 WL-807-R-PROC-MOD-5-CD     PIC X(02) VALUE SPACES.MAPOPS3
MDM-I2             15 WL-807-R-PROC-MOD-6-CD     PIC X(02) VALUE SPACES.MAPOPS3
MDM-I2             15 WL-807-R-PROC-MOD-7-CD     PIC X(02) VALUE SPACES.MAPOPS3
MDM-I2             15 WL-807-R-PROC-MOD-8-CD     PIC X(02) VALUE SPACES.MAPOPS3
MDM-I2             15 WL-807-R-PROC-MOD-9-CD     PIC X(02) VALUE SPACES.MAPOPS3
MDM-I2             15 WL-807-R-PROC-MOD-10-CD    PIC X(02) VALUE SPACES.MAPOPS3
MDM-I2             15 WL-807-R-PROC-MOD-STAT-CD  PIC X(01) VALUE SPACES.
                   15 WL-807-R-PROC-MOD-EXC      PIC X(04) VALUE SPACES.MAPOPS3
                   15 WL-807-R-PROC-MOD-SC       PIC X(01) VALUE SPACE. MAPOPS3
MDM-I2             15 WL-807-R-VAC-MAX-AGE       PIC S9(4) COMP.
MDM-I2             15 WL-807-R-VAC-MAX-SPN-CD    PIC X(01) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-MAX-AGE-EXC   PIC X(04) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-MAX-AGE-SC    PIC X(1) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-MIN-AGE       PIC S9(4) COMP.
MDM-I2             15 WL-807-R-VAC-MIN-SPN-CD    PIC X(01) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-MIN-AGE-EXC   PIC X(04) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-MIN-AGE-SC    PIC X(1) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-CVRD-EXC      PIC X(04) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-CVRD-SC       PIC X(1) VALUE SPACES.
MDM-I2             15 WL-807-R-VAC-CVRD-IND      PIC X(1) VALUE SPACES.
DF3679             15 WL-807-R-CMPND-MBR-CD      PIC X(01) VALUE SPACES.
DF3679             15 WL-807-R-CMPND-MBR-CD-EXC  PIC X(04) VALUE SPACES.
DF3679             15 WL-807-R-CMPND-MBR-CD-SC   PIC X(01) VALUE SPACE.
                                                                        MAPOPS3
CR6916             15 WL-807-R-CMPND-PREP-FEES   PIC S9(6)V99 COMP-3.   COR6916
CR6916             15 WL-807-R-CMPND-DISP-FEES   PIC S9(6)V99 COMP-3.   COR6916
DF3679****      12  FILLER                          PIC X(44).          CR4825
      *         12  FILLER                          PIC X(43).          CR4825
      *         12  FILLER                          PIC X(23).          CR14166
                12  FILLER                          PIC X(20).          CR14166

           05 WL-807-HISTORY-CHK-SW       PIC X(01) VALUE 'N'.          CR11363
              88 WL-807-HISTORY-CHK-YES      VALUE 'Y'.                 CR11363
              88 WL-807-HISTORY-CHK-NO       VALUE 'N'.                 CR11363
           05 WL-807-HIST-CHECK-PERIOD    PIC 9(3) VALUE ZEROES.        CR11363 
           05 WL-807-COVID-BOOSTER-CHK-SW PIC X(01) VALUE 'N'.          CR12125
              88 WL-807-COVID-BOOSTR-CHK-YES    VALUE 'Y'.              CR12125
              88 WL-807-COVID-BOOSTR-CHK-NO     VALUE 'N'.              CR12125
           05 WL-807-COVID-BOOSTR-PERIOD PIC 9(3) VALUE ZEROES.         CR12125 
           05 WL-807-COVID-BOOSTER-LIMIT   PIC 9(3) VALUE ZEROES.       CR12125 
           05 WL-807-SCC-07-SW PIC X(01) VALUE 'N'.                     CR12275
              88 WL-807-SCC-07-YES    VALUE 'Y'.                        CR12275
              88 WL-807-SCC-07-NO     VALUE 'N'.                        CR12275
           05 WL-807-SCC-10-SW PIC X(01) VALUE 'N'.                     CR12275
              88 WL-807-SCC-10-YES    VALUE 'Y'.                        CR12275
              88 WL-807-SCC-10-NO     VALUE 'N'.                        CR12275
           05 WL-807-3488-EXC-SW PIC X(01) VALUE 'N'.                   CR12275
              88 WL-807-3488-EXC-POST      VALUE 'Y'.                   CR12275
              88 WL-807-3488-EXC-NOT-POST  VALUE 'N'.                   CR12275
           05 WL-807-R-PAT-RESID-SUBM-IND     PIC X(01)  VALUE 'N'.     CR12914
              88 WL-807-R-PAT-RESID-SUBM-Y          VALUE 'Y'.          CR12914
              88 WL-807-R-PAT-RESID-SUBM-N          VALUE 'N'.          CR12914
           05 WL-807-MCARE-DRUG-EXC-IND       PIC X(01)  VALUE 'N'.     CR6791
              88 WL-807-MCARE-DRUG-EXC-YES          VALUE 'Y'.          CR6791
              88 WL-807-MCARE-DRUG-EXC-NO           VALUE 'N'.          CR6791
           05 WL-807-C-FILL-AFTER-DATE           PIC X(10).             CR14158
           05 WL-807-B-COPAY-RMN-AMT             PIC S9(7)V99 COMP-3.   CR14158
           05 WL-807-C-HDR-PD-DT                 PIC X(10).             CR14158 
           05 WL-807-3588-EXC-SW PIC X(01) VALUE 'N'.                   CR14676
              88 WL-807-3588-EXC-POST      VALUE 'Y'.                   CR14676
              88 WL-807-3588-EXC-NOT-POST  VALUE 'N'.                   CR14676
062700*
005840******************************************************************
005850*                 E N D  O F  C O P Y B O O K                    *
005860******************************************************************
