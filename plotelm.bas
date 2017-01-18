'$DYNAMIC
DECLARE SUB EMSOPENf (fSize&, ET%, HA%, EC%)
DECLARE SUB BORDER (XYscr!())
DECLARE SUB MakerEllipse (Xc!, Yc!, Theta!, Rad!, Ecc!, CLR!, GID%, Xmaker!())
DECLARE SUB EMSGETf (HA%, L&, V!)
DECLARE SUB EMSPUTf (HA%, L&, V!)
DECLARE SUB EMSOPEN (fDLIST&, ET%, HA%, EC%)
DECLARE SUB Qmouse (M1%, M2%, M3%, M4%)
DECLARE SUB HHOLLOW (NBD!, Xc!, Yc!, Rad!, RATIO!, RAVER!, HCFLAG!)
DECLARE SUB BHOLLOW (NBD!, Xc!, Yc!, Rad!, RATIO!, RAVER!, HCFLAG!)
DECLARE SUB BCIRCLE (NBD!, Xc!, Yc!, Rad!, RAVER!, HCFLAG!)
DECLARE SUB Retreve (PaddressF!, HistNoP!, Xar!(), Yar!(), Sc!, Cfile0$, XaddressF!, HistNoF!, SCX!)
DECLARE SUB NEWHIST (IWRITE!, PPOS!)
DECLARE SUB READSTR (PS1!, NS!, Z$)
DECLARE SUB SUMMARY (IPRINT!, PPOS!, Cfile$)
DECLARE SUB BRECT (NBD!, Xmin!, Xmax!, Ymin!, Ymax!, RAVER!)
DECLARE SUB PARSER (T$, Par$(), NSET!)
DECLARE SUB NBDISK (NBD!)
DECLARE SUB DISPL (XYlim!(), XYscr!(), XYplt!(), XYout!(), Contr!())
DECLARE FUNCTION AA! (I!)
DECLARE FUNCTION IA! (I!)
DECLARE FUNCTION CLIST! (I!, J!)
DECLARE FUNCTION DLIST! (I!, J!)
DECLARE FUNCTION ATAN2! (ARG1!, ARG2!)
DECLARE FUNCTION CONV! (Addr!, HistNo!)
DECLARE FUNCTION BCONT! (I!, J!)
DECLARE SUB ARROW (X1!, Y1!, X2!, Y2!, CLR!)
DECLARE SUB Cg (XY!(), Xg!, Yg!, Areax!)
DECLARE SUB ASCIIdump (CfileS$)
DECLARE SUB FORCES (XC1!, YC1!, RBAR1!, ECC1!, THETA1!, XC2!, YC2!, RBAR2!, ECC2!, THETA2!, CVX1!, CVY1!, CVX2!, CVY2!, CNX1!, CNY1!, CTX1!, CTY1!, DX1!, DY1!, DT1!, DX2!, DY2!, DT2!, FRN!, FRT!, DFN!, DFT!, FXD1!, FYD1!, FXD2!, FYD2!, DM1!, DM2!,  _
DN!, DT!, IFLAG!, STIF!, XLAMBDA!, BDT!, AMU!, XYCN!())
DECLARE SUB RTFOUR (A#, B#, C#, D#, E#, K!, RT#(), ERT#(), EPS#, EPSC#)
DECLARE SUB DLISTi (I!, J!, V!)
DECLARE SUB CLISTi (I!, J!, V!)
DECLARE SUB BCONTI (I!, J!, V!)
DECLARE SUB EMSALLOCATE (NDISK!, NDPART!, NBOX!, NBPART!, NCONT!, NCPART!)
DECLARE SUB EMSCLEAR ()
DECLARE SUB AVRGTHETA (Theta!)
DECLARE SUB SORT (N!, RA#(), AA#())
DECLARE SUB INTELLI (XC1!, YC1!, THETA1!, ITYP1!, XC2!, YC2!, THETA2!, ITYP2!, Xi1!, Yi1!, Xi2!, Yi2!, PN1!, PN2!, IFLAG!)
DECLARE ellipse (Xc!, Yc!, Theta!, Rad!, Ecc!, CLRL!)
DECLARE SUB AssignGroupH (G$, SItems$(), Saddr!(), Aaddr!(), Scale!(), SNitems!)
DECLARE SUB HIST (AR!(), AAr!(), Par$(), NA!, NS!, XYlim!(), XYscr!(), XYplt!(), Labl$(), Contr!())
DECLARE SUB RetreveH (Paddress!, AR!(), AAr!(), Sc!, Cfile$, ADI!)
DECLARE SUB Histograms (XYlimG!(), XYscrG!(), XYpltG!(), XYoutG!(), ContrG!(), LablG$(), Cfile$)
DECLARE SUB MOVETO (Xp!, YP!, RCOEFX!, RXSHIFT!, RCOEFY!, RYSHIFT!)
DECLARE SUB LINETO (Xp!, YP!, RCOEFX!, RXSHIFT!, RCOEFY!, RYSHIFT!)
DECLARE SUB MPNT (Xp!, YP!, RCOEFX!, RXSHIFT!, RCOEFY!, RYSHIFT!)
DECLARE SUB FillBoxes (XYlim!(), XYscr!(), RDIST!(), RDISTI!(), EDIST!(), TDIST!(), TDISTI!())
DECLARE SUB Fill (XY!(), Npoly!, XYlim!(), XYscr!(), XYplt!(), Contr!(), Separ!, Alpha!, RCOEFX!, RXSHIFT!, RCOEFY!, RYSHIFT!, Imaker%)
DECLARE SUB MSPECIAL (Xc!, Yc!, DIA!, I%, GID%)
DECLARE SUB ADDPOINT (XYP!(), XYC!(), IDP!(), IDC!(), NV!, XYins!(), IORIG!, IPO!())
DECLARE SUB MARK (XL!, YL!, XPc!(), IDC!())
DECLARE SUB CHECKTYPE (XYP!(), XYC!(), IDP!(), IDC!(), NV!, XYins!(), ISEQ!, IPO!())
DECLARE SUB Limits (XYP!(), XYlim!())
DECLARE SUB ReviewContacts ()
DECLARE SUB PlotLINKS (XYlim!(), XYscr!(), XYplt!(), XYout!(), Labl$(), Contr!(), CNT%())
DECLARE SUB MPLOT (XYlim!(), XYscr!(), XYplt!(), Labl$(), Contr!())
DECLARE SUB FindContact (IB1F!, IB2F!, DSN!, DST!, FRN!, FRT!)
DECLARE SUB PLOT (Xar!(), Yar!(), Par$(), NA!, NS!, XYlim!(), XYscr!(), XYplt!(), Labl$(), Contr!())
DECLARE SUB CHECKfile (Cfile$, ERFILE!)
DECLARE SUB Match (s$, G$)
DECLARE SUB AssignGroup (G$, SItems$(), Saddr!(), Scale!(), SNitems!)
DECLARE SUB Graphs (XYlimG!(), XYscrG!(), XYpltG!(), XYoutG!(), ContrG!(), LablG$(), Cfile$, Dfile$)
DECLARE SUB PlotFORCES (XYlim!(), XYscr!(), XYout!(), Contr!())
DECLARE SUB SLINE (X1!, Y1!, X2!, Y2!, CLR%, TH!, XYlim!(), XYscr!(), XYplt!(), Contr!())
DECLARE SUB FindTwoBalls (XYlim!(), XYscr!(), XYout!(), Contr!())
DECLARE SUB MOVEparticle (XYlim!(), XYscr!(), XYout!(), Contr!(), NFILE$)
DECLARE SUB AssignBOUNDARY (XYlim!(), XYscr!(), XYout!(), Contr!(), NFILE$)
DECLARE SUB DeleteBALLS (XYlim!(), XYscr!(), XYout!(), Contr!(), NFILE$)
DECLARE SUB MarkBALLS (XYlim!(), XYscr!(), XYout!(), Contr!(), Mlist!(), Mes$)
DECLARE SUB SearchBALL (Xo!, Yo!, Tc!, N!, Xc!, Yc!, ITYPE!, IBTYP!, FOUND!)
DECLARE SUB BallINFO (XYlim!(), XYscr!(), XYout!(), Contr!())
DECLARE SUB CheckContacts (N2!, X2!, Y2!, THETA2!, ITYP2!, RBAR2!, ECC2!, IFLAG!)
DECLARE SUB GETDISC (N!, Xc!, Yc!, Tc!, ITYPE!, IBTYP!)
DECLARE SUB MakeNext (Dfile$, NEWFILE!)
DECLARE SUB InsertBBall (N!, NBD!)
DECLARE SUB CleanBoundary ()
DECLARE SUB DeleteBall (N!)
DECLARE SUB GetContactList ()
DECLARE SUB GetDiskList ()
DECLARE SUB RewriteArray (NFILE$)
DECLARE SUB DISPLAY (Mes$)
DECLARE SUB CLEAN ()
DECLARE SUB FileINITIALIZE (BinfFile$)
DECLARE SUB PLOTELSt (Xc!, Yc!, Theta!, ITYP!, IBTYP!, CLR!)
DECLARE SUB PLOTpolyt (XY!(), CLR!)
DECLARE SUB ResizeWindow (XYlim!(), XYscr!(), XYout!(), Contr!(), RESIZED!)
DECLARE SUB AAI (PS1!, VL!)
DECLARE SUB CHECKinside (XY!(), Xo!, Yo!, INSIDE!)
DECLARE SUB MakeMenu (XYlim!(), XYscr!(), XYout!(), Contr!(), ITEMS$(), NITEMS!, Boxes!(), OPER!, OPTIONS!(), G$)
DECLARE SUB INITfile (Cfile$)
DECLARE SUB INITP (XYlim!())
DECLARE SUB PlotBalls (XYlim!(), XYscr!())
DECLARE SUB GETCONT (IB1!, IB2!, IAD!, X1!, Y1!, X2!, Y2!, FRN!, FRT!, DSN!, DST!, M1!, NDPART!)
DECLARE SUB PLOTELS (Xc!, Yc!, Theta!, ITYP!, IBTYP!, CLR!)
DECLARE SUB READNUM (PS1!, Z$)
DECLARE SUB MSETLINE (PN!)
DECLARE SUB MRECT (Xo!, Yo!, W!, H!, GID%)
DECLARE SUB MLINE (XI!, YI!, XF!, Yf!, GID%)
DECLARE SUB MPGON (X!(), Y!(), N%, FillP%, GID%)
DECLARE SUB MSETFONT (Font$, Font%)
DECLARE SUB MCLABEL (LB$, Xc!, Yc!, GID%)
DECLARE SUB MVLABEL (LB$, Xc!, Yc!, GID%)
DECLARE SUB MRLABEL (LB$, Xc!, Yc!, GID%)
DECLARE SUB PLGN (Xc!, Yc!, TH0!, DTH!, Rad!, N%)
DECLARE SUB Get.Data (Page$(), Dat$(), Dat%(), NRmin!, NRmax!, NcMin!, NcMax!, ERline!, Mes$)
DECLARE SUB BOX (X!, Y!, R!, E!, NBSAV!(), NBMAP!, SPLIT!())
DECLARE SUB Win (XYlim!(), XYscr!(), XYplt!(), Labl$(), Contr!())
DECLARE SUB DELAY (G$)
DECLARE SUB DRAG (XYlim!(), XYscr!(), XYout!(), G$, Contr!(), A0!(), RRD%)
DECLARE SUB LOCATOR (XYlim!(), XYscr!(), XYout!(), G$, Contr!())
DECLARE SUB INITPS (Contr!())
DECLARE SUB SCREDIT (XYlim!(), XYscr!(), XYplt!(), Labl$(), Par$(), Contr!(), XDAT!(), YDAT!(), NA!, NS!, ID%())
DECLARE SUB TRNPOLY (XY!(), XYT!(), Theta!, Xc!, Yc!)
DECLARE SUB PLOTpoly (XY!(), CLR!)
'
COMMON SHARED M1, NDISK, H, W, NBOX, TOL, MODE, NTYPE, STIF, XLAMBDA, AMU
COMMON SHARED Ecc, DENSITY, NN, DELBOX, NXB, NYB, NDPART, NCPART, NBPART, ICOUNT
COMMON SHARED IBPART, SHAPES(), R(), E(), NCONT, NBD, XMINA, YMINA, XMAXA, YMAXA, RAVER, RECMAX, RECHIS, RECPOL
COMMON SHARED HA%, iBCONT&, fBCONT&, iCLIST&, fCLIST&, iDLIST&, fDLIST&
COMMON SHARED BFLAG, DEBUG, NPOL, IADD, NADD, IPIPE, NPIPE, trackfl, BOX29, POLYSTYLE%, IGROUP, NGROUP, DPR
COMMON SHARED RECMAXZZ, RECHISZZ, RECPOLZZ, CURFILE%, NOSTOP, AUT, GTERM$, KSEQ
'
DEF SEG = 0
CLEAR , , 3600
DIM Page$(25), Dat$(25, 10), Dat%(25)
DIM Xar(1, 6000), Yar(1, 6000)
'
DIM Xa1(1, 6000), Ya1(1, 6000): DIM LBA1$(2)
DIM Xa2(1, 6000), Ya2(1, 6000): DIM LBA2$(2)
DIM Xa3(1, 6000), Ya3(1, 6000): DIM LBA3$(2)
'
CWD$ = UCASE$(COMMAND$)
BFLAG = 0
MOUSE% = 0: CALL Qmouse(MOUSE%, BT%, MX%, MY%)
ndim = 1: MAKER = 0: TWOP = 8 * ATN(1!): LASTBALL = 0
'
DIM XYlim(10), XYlimR(10), XYlimS(10), XYscr(10), XYplt(10), Contr(10), Labl$(30), Trans(4), Xmaker(10), XYout(10), Boxes(30), OPTIONS(30)
DIM XYlimG(10), XYscrG(10), XYpltG(10), XYoutG(10), ContrG(10), LablG$(30)
DIM XYlimF(10), XYscrF(10), XYpltF(10), XYoutF(10), ContrF(10), LablF$(30)
DIM XYlimT(10), XYscrT(10), XYpltT(10), XYoutT(10), ContrT(10), LablT$(30)
DIM XYscrZ(10)
DIM T(40), E(40), R(40), Theta(320), ITP(320), SHAPES(40, 2)
DIM ITEMS$(30)
DIM RT(20), ET(20), TT(36), RDIST(20), RDISTI(20), EDIST(36), TDIST(36), TDISTI(36), TBLOCK(20), THBLOCK(20)
DIM CNT%(1100)
DIM THREM(1), PATRN(16), COORDC(16), NDIST(16)
DIM Olist$(100), Hlist$(100)
DIM LPMODE AS LONG
REDIM BCT(0), SPLIT(0)
NOBFILE = 0: Olist$(0) = "0": Hlist$(0) = "0": SELITEMX$ = "": SELITEMY$ = ""
ND = 36
SMIN% = 0: CMIN% = 0: ASPECT = 0: ROIN% = 0
'
XSPMODE = 640: YSPMODE = 480
XSPMODE = 800: YSPMODE = 600
XSPMODE = 928: YSPMODE = 696
SPMODE = _NEWIMAGE(XSPMODE, YSPMODE, 256)
LPMODE = SPMODE
WDMODE = INT(YSPMODE / 16)
XDMODE = INT(XSPMODE / 8)
SCREEN SPMODE
_SCREENMOVE 0, 0
_TITLE "PLOTELS"
XYlim(8) = XSPMODE: XYlim(9) = YSPMODE: XYlim(10) = SPMODE
XYscrZ(1) = 0: XYscrZ(2) = 0: XYscrZ(3) = 0: XYscrZ(4) = 0
'
IBOX = 50
ICALL = 0
PATRN(16) = 0
PATRN(15) = 1
PATRN(14) = 10
PATRN(13) = 6
PATRN(12) = 7
PATRN(11) = 7
PATRN(10) = 7
PATRN(9) = 7
'
COORDC(0) = 8
COORDC(1) = 8
COORDC(2) = 15
COORDC(3) = 10
COORDC(4) = 13
COORDC(5) = 14
COORDC(6) = 12
COORDC(7) = 11
COORDC(8) = 9
COORDC(9) = 15
COORDC(10) = 15
COORDC(11) = 15
COORDC(12) = 15
COORDC(13) = 15
COORDC(14) = 15
COORDC(15) = 15
'
Cfile$ = "CI_0001.bin"
Dfile$ = "Plotels.csv"
Lfile$ = ""
REPRUN:
NRmin = 1: NRmax = 8: NcMin = 18: NcMax = 79: NCOL% = 2: EFL = 0: Mes$ = "": WINDOW: VIEW: CLS
AUT = 1
Page$(1) = "Work Directory ->"
IF CWD$ <> "" THEN
    Page$(1) = Page$(1) + CWD$
END IF
Page$(2) = "Starting  file ->"
IF Cfile$ <> "" THEN
    Page$(2) = Page$(2) + Cfile$
END IF
Page$(3) = "Trace file (T) ->EXTRACT.DA"
Page$(4) = "Auto str (6+2) ->"
Page$(5) = "Stop-wait  (Y) ->N"
Page$(6) = "Make movie (Y) ->N"
Page$(7) = "Plots (X - no) ->q/p-Et, Ev-Et, Fb-K, X"
IF VAL(LablF$(30)) <> 0 THEN
    Page$(8) = "Trace particle ->" + LablF$(30)
ELSE
    IF LASTBALL <> 0 THEN
        Page$(8) = "Trace particle ->" + MID$(STR$(LASTBALL), 2)
    ELSE
        Page$(8) = "Trace particle ->0"
    END IF
END IF
'
SCREEN 12: WINDOW: VIEW: CLS
FOR I% = 1 TO 25: Dat%(I%) = -1: Dat$(I%, 1) = "": Dat$(I%, 2) = "": NEXT I%
CALL GetData(Page$(), Dat$(), Dat%(), NRmin, NRmax, NcMin, NcMax, EFL, Mes$)
'
IF Dat%(1) <> 0 THEN
    CWD$ = Dat$(1, 1)
    Labl$(30) = CWD$
END IF
IF Dat%(3) <> 0 THEN
    Efile$ = Dat$(3, 1)
    IF INSTR(1, Efile$, "DAT") <> 0 THEN
        Efile$ = CWD$ + "\" + Efile$
        Ofile$ = CWD$ + "\" + "CDUMP.CSV"
        NOBFILE = 0
        CALL TRACE2P(Efile$, Ofile$)
        GOTO REPRUN
    END IF
END IF
IF Dat%(2) <> 0 THEN
    Cfile$ = Dat$(2, 1)
END IF
G$ = "": KSEQ = 0: KSQ$ = ""
AUT = 1: NOSTOP = 0
IF Dat%(4) = 1 THEN
    G$ = Dat$(4, 1)
    G$ = "!" + G$
    AUT = 0: KSEQ = -1
    IF Dat%(5) = 1 THEN
        IF UCASE$(Dat$(5, 1)) = "Y" THEN
            NOSTOP = 1
        END IF
    END IF
END IF
NOBMP = 0
IF Dat%(6) = 1 THEN
    IF UCASE$(Dat$(6, 1)) = "Y" THEN
        NOBMP = 1
        BMPlist$ = CWD$ + "\" + "FLIST.TXT"
        OPEN BMPlist$ FOR OUTPUT AS #5
    END IF
END IF

NEWF:
SCREEN 12: WINDOW: VIEW: CLS
'
'LOCATE 1, 6: PRINT "Initial file [" + CfileS$ + "] (X - initialize)";: INPUT " New name "; Cfile$
NEWFB:
'CALL CLEAN
IF Cfile$ = "X" OR Cfile$ = "x" THEN
    CALL FileINITIALIZE(Cfile$)
    WINDOW: VIEW: CLS
    GOTO NEWFB
ELSE
    Cfile$ = CWD$ + "\" + Cfile$
    Dfile$ = CWD$ + "\" + Dfile$
    LNO = LEN(CWD$) + 2
    NOBFILE = 0
    CALL CHECKfile(Cfile$, NOBFILE)
    IF NOBFILE <> 0 THEN
        LOCATE 1, 1: PRINT Cfile$; " Not found. Press any key ... ";
        CALL STRIPDIR(Cfile$)
        WHILE INKEY$ = "": WEND
        GOTO REPRUN:
    END IF
END IF

'
RESIZED = 0: Cfilei$ = Cfile$: NEWFILE = 1: GTERM$ = ""
'
NBOX = 6000: NBPART = 100: NFLP = 0
CALL EMSALLOCATE(NDISK, NDPART, NBOX, NBPART, NCONT, NCPART)
ITMP$ = "": GP$ = ""
NORSIDE = 1
IF Dat%(7) <> 0 THEN
    IF Dat%(7) = 4 AND UCASE$(Dat$(7, 4)) = "X" THEN
        LBA1$(1) = "": LBA2$(1) = "": LBA3$(1) = ""
    ELSE
        CALL GraphsInfo(Cfile$, "2", "9", "1", "A", 0, 0)
        CALL TRANSFER(1)
        CALL GraphsInfo(Cfile$, "2", "9", "2", "8", 0, 0)
        CALL TRANSFER(2)
        CALL GraphsInfo(Cfile$, "0", "0", "8", "1", 0, 0)
        CALL TRANSFER(3)
        S$ = "Last file: " + Lfile$
        L% = XDMODE - LEN(S$) - 5
        S$ = S$ + STRING$(L%, " ")
        LOCATE WDMODE, 6: PRINT S$;
        NORSIDE = 0
    END IF
END IF
IF Dat%(8) = 1 THEN
    Pno = VAL(Dat$(8, 1))
    LablF$(30) = MID$(STR$(Pno), 2)
    LASTBALL = Pno
    IF Pno <> 0 THEN
        CALL GraphsInfo(Cfile$, "2", "9", "9", "7", Pno, 0)
        CALL TRANSFER(0)
        NORSIDE = 0
    END IF
END IF
AGAINdisp:
GG$ = INKEY$
IF GG$ = CHR$(27) THEN
    G$ = ""
    AUT = 1
END IF
IF GTERM$ = "X" THEN
    G$ = ""
    GTERM$ = ""
    AUT = 1
END IF
CALL INITfile(Cfile$)
CALL INITP(XYlimR())
IF KSEQ = -1 THEN
    FOR I = 1 TO 4: XYlimS(I) = XYlimR(I): NEXT
    KSEQ = 1
END IF
IF RESIZED = 0 THEN
    FOR I = 1 TO 4: XYlim(I) = XYlimR(I): NEXT
END IF
IF AUT = 0 THEN
    FOR I = 1 TO 4: XYlim(I) = XYlimS(I): NEXT
END IF
'
IF NEWFILE = 0 THEN
    CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    CALL PlotBalls(XYlim(), XYscr())
ELSE
    CALL FillBoxes(XYlim(), XYscr(), RDIST(), RDISTI(), EDIST(), TDIST(), TDISTI())
    IF AUT = 1 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
        CALL PlotBalls(XYlim(), XYscr())
    ELSE
        CALL DECODEG(G$, VVL)
        IF VVL = 20 THEN
            IF LASTBALL <> 0 THEN
                LablF$(30) = MID$(STR$(LASTBALL), 2)
            ELSE
                AUT = 1
                CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
                CALL PlotBalls(XYlim(), XYscr())
                ITEMS$(0) = "0": G$ = ""
            END IF
        END IF
        IF VVL = 2 OR VVL = 16 THEN
            CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
            CALL PlotBalls(XYlim(), XYscr())
        END IF
    END IF
    NEWFILE = 0
END IF
'
NITEMS = 26: BOX29 = 30
ITEMS$(1) = "1 - Plot particles  "
ITEMS$(2) = "2 - Plot forces / A "
ITEMS$(3) = "3 - Assign Boundary "
ITEMS$(4) = "4 - Resize area     "
ITEMS$(5) = "5 - Redu current    "
ITEMS$(6) = "6 - Prev/Next file  "
ITEMS$(7) = "7 - Links-Coord / A "
ITEMS$(8) = "8 - Start again     "
ITEMS$(9) = "9 - Move particle   "
ITEMS$(10) = "A - Rotations      "
ITEMS$(11) = "B - Particle Info  "
ITEMS$(12) = "C - Delete particle"
ITEMS$(13) = "D - Polygon groups "
ITEMS$(14) = "E - Find two balls "
ITEMS$(15) = "F - ASCII Part/Full"
ITEMS$(16) = "G - Velocities / A "
ITEMS$(17) = "H - Graphs menu    "
ITEMS$(18) = "I - Histograms menu"
ITEMS$(19) = "J - Review contacts"
ITEMS$(20) = "K - FirstShell / A "
ITEMS$(21) = "L - First file     "
ITEMS$(22) = "M - Displacements  "
ITEMS$(23) = "N - Summary  / CLS "
ITEMS$(24) = "O - Poly ACT / ALL "
ITEMS$(25) = "P - Print ON       "
ITEMS$(26) = "Q - Exit           "
ITEMS$(27) = "R - First file     "
'
XYout(0) = 30
'
POLYSTYLE% = 0
'
MainPoll:
'
CALL MakeMenu(XYlim(), XYscr(), XYout(), Contr(), ITEMS$(), NITEMS, Boxes(), 0, OPTIONS(), G$)

CALL LOCATOR(XYlim(), XYscr(), XYout(), G$, Contr())
'
' Auto-resize from locator
'
IF XYout(1) <> XYout(2) AND XYout(3) <> XYout(4) THEN
    XYlim(1) = XYout(1): XYlim(2) = XYout(2)
    XYlim(3) = XYout(3): XYlim(4) = XYout(4)
    RESIZED = 1
    CLOSE #1
    WINDOW: VIEW: CLS
    NEWFILE = 1
    GOTO AGAINdisp
END IF
IF XYout(1) = XYout(2) AND XYout(3) = XYout(4) THEN
    Xs = XYout(1): Ys = XYout(3)
    IF Xs > XYlim(1) AND Xs < XYlim(2) THEN
        IF Ys > XYlim(3) AND Ys < XYlim(4) THEN
            IF VAL(LablF$(30)) = 0 THEN
                LablF$(30) = "-1"
                G$ = "K"
                ITEMS$(0) = "20"
            ELSE
                COLOR 2, 0
                LOCATE 1, 6: PRINT "PARTICLE " + LablF$(30) + " IS CURRENT - ";
                INPUT "REASSIGN TO INTERACTIVE ? (Y/N) "; IWHAT$
                IF UCASE$(IWHAT$) = "Y" THEN
                    LablF$(30) = "-1"
                    G$ = "K"
                    ITEMS$(0) = "20"
                END IF
                CALL CLEAN
            END IF
        END IF
    END IF
END IF
'
CALL MakeMenu(XYlim(), XYscr(), XYout(), Contr(), ITEMS$(), NITEMS, Boxes(), 1, OPTIONS(), G$)
'************
'
ScpMenu:
'
VVL = VAL(ITEMS$(0))
IF AUT = 1 THEN
    IF VVL = 2 OR VVL = 7 OR VVL = 16 OR VVL = 20 THEN
        G$ = "Q"
    END IF
END IF
NFL = 0
IF VVL < 0 THEN
    ITEMS$(0) = STR$(-VVL)
    NFL = 1
END IF
'
IF VAL(ITEMS$(0)) = NITEMS OR G$ = "Q" THEN 'Exit
    IF VAL(ITEMS$(0)) = NITEMS THEN NFL = 1
    IF NFL = 1 THEN
        ITMP$ = ITEMS$(0): GP$ = G$
        CLOSE
        END
    ELSE
        G$ = "!(6+" + MID$(ITEMS$(VVL), 1, 1) + ")"
        AUT = 0: KSEQ = 1
        NOSTOP = 0
        CLOSE #1
        WINDOW: VIEW: CLS
        NEWFILE = 1
        IF RESIZED = 1 THEN
            FOR I = 1 TO 4: XYlimS(I) = XYlim(I): NEXT
        ELSE
            FOR I = 1 TO 4: XYlimS(I) = XYlimR(I): NEXT
        END IF
        GOTO AGAINdisp
    END IF
END IF
'
IF VAL(ITEMS$(0)) = NITEMS - 1 OR G$ = "P" THEN 'Print ON
    ITMP$ = ITEMS$(0): GP$ = G$
    Contr(3) = 1
    OPTIONS(NITEMS - 1) = 1
    GOTO MainPoll
END IF
IF VAL(ITEMS$(0)) = 1 OR G$ = "1" THEN 'Plot BALLS
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    CALL PlotBalls(XYlim(), XYscr())
    IF Xmaker(0) = -1 THEN
        PRINT #3, "showpage"
        CLOSE #3
    END IF
END IF
'************
IF VAL(ITEMS$(0)) = 2 OR G$ = "2" THEN 'Plot FORCES
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    CALL PlotFORCES(XYlim(), XYscr(), XYout(), Contr())
    IF Contr(3) <> 0 THEN CLOSE #3
END IF

'************
IF VAL(ITEMS$(0)) = 3 OR G$ = "3" THEN 'Assign Boundary
    ITMP$ = ITEMS$(0): GP$ = G$
    CALL AssignBOUNDARY(XYlim(), XYscr(), XYout(), Contr(), NFILE$)
    IF NFILE$ <> "" THEN
        IF NFILE$ <> " " THEN
            Cfile$ = NFILE$
        END IF
        GOTO AGAINdisp
    END IF
END IF
'************
IF VAL(ITEMS$(0)) = 4 OR G$ = "4" THEN 'Resize
    ITMP$ = ITEMS$(0): GP$ = G$
    CALL ResizeWindow(XYlim(), XYscr(), XYout(), Contr(), ACTION)
    IF ACTION < 0 THEN
        GOTO MainPoll
    ELSE
        RESIZED = ACTION
        CLOSE #1
        WINDOW: VIEW: CLS
        NEWFILE = 1
        GOTO AGAINdisp
    END IF
END IF
'************
IF VAL(ITEMS$(0)) = 5 OR G$ = "5" THEN 'File again
    ITMP$ = ITEMS$(0): GP$ = G$
    CLOSE #1
    WINDOW: VIEW: CLS
    GOTO AGAINdisp
END IF
'************
IF ABS(VAL(ITEMS$(0))) = 6 OR G$ = "6" THEN 'Next File
    IF G$ = "6" THEN NFL = NFLP
    IF NOBMP = 1 THEN
        Bfile$ = Cfile$
        N = INSTR(1, Cfile$, "."): Bfile$ = MID$(Cfile$, 1, N - 1)
        CALL MakeFstring(KSEQ, KSQ$)
        Bfile$ = CWD$ + "\" + KSQ$
        CALL DECODEG(G$, VVX)
        IF NORIGHT = 1 THEN
            CALL SaveImage(LPMODE, Bfile$, XYscrS())
        ELSE
            CALL SaveImage(LPMODE, Bfile$, XYscrZ())
        END IF
        PRINT #5, Bfile$ + ".BMP"
    END IF
    Sfile$ = Cfile$
    NFLP = NFL
    IF NFL = 0 THEN
        CALL MakeNext(Cfile$, NEWFILE)
    ELSE
        CALL MakePrev(Cfile$, NEWFILE)
    END IF
    IF NEWFILE = 0 THEN
        NOBFILE = 1
        GOTO SCPCHK1
    END IF
    NOBFILE = 0
    CALL CHECKfile(Cfile$, NOBFILE)
    SCPCHK1:
    IF NOBFILE = 0 THEN
        CLOSE #1
        WINDOW: VIEW: CLS
        KSEQ = KSEQ + 1
        LOCATE 1, 6: PRINT Cfile$
        GOTO AGAINdisp
    ELSE
        LOCATE WDMODE, 1: PRINT STRING$(XDMODE, " ");
        LOCATE WDMODE, 6: PRINT "Last file "; Sfile$; "...";
        Cfile$ = Sfile$
        CLOSE #1
        CALL DECODEG(G$, VVX)
        IF NOBMP = 1 THEN
            CLOSE #5
            NOBMP = 0
            FLM$ = ""
            IF VVX = 2 THEN FLM$ = "M2.GIF"
            IF VVX = 16 THEN FLM$ = "MG.GIF"
            IF VVX = 7 THEN FLM$ = "M7.GIF"
            IF VVX = 20 THEN
                FLM$ = "B" + LablF$(30) + ".GIF"
            END IF
            AUT = 1
            IF FLM$ <> "" THEN
                BTFILE$ = CWD$ + "\MOVIE.BAT"
                OPEN BTFILE$ FOR OUTPUT AS #5
                S$ = "CONVERT " + CHR$(64) + BMPlist$ + " " + CWD$ + "\" + FLM$
                PRINT #5, S$
                CLOSE #5
                S$ = "CALL " + BTFILE$
                SHELL (S$)
            END IF
            Cfile$ = Cfilei$
            CALL STRIPDIR(Cfile$)
            GOTO REPRUN:
        END IF
        IF VVX = 2 OR VVX = 7 OR VVX = 20 OR VVX = 16 THEN
            WHILE INKEY$ = "": WEND
            ITEMS$(0) = "1": G$ = "1"
            NEWFILE = 0
            AUT = 1
            WINDOW: VIEW: CLS
            Cfile$ = Cfilei$
            'CALL STRIPDIR(Cfile$)
            GOTO AGAINdisp
        END IF
    END IF
END IF
'************
IF VAL(ITEMS$(0)) = 7 OR G$ = "7" THEN 'Links
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    CALL PlotLINKS(XYlim(), XYscr(), XYplt(), XYout(), Labl$(), Contr(), CNT%())
END IF
'************
IF VAL(ITEMS$(0)) = 8 OR G$ = "8" THEN 'Start again
    ND = LEN(CWD$) + 2
    Cfile$ = MID$(Cfile$, ND)
    CLOSE #1
    GOTO REPRUN:
END IF
'************
IF VAL(ITEMS$(0)) = 9 OR G$ = "9" THEN 'Move
    ITMP$ = ITEMS$(0): GP$ = G$
    CALL MOVEparticle(XYlim(), XYscr(), XYout(), Contr(), NFILE$)
    IF NFILE$ <> "" THEN
        IF NFILE$ <> " " THEN
            Cfile$ = NFILE$
        END IF
        GOTO AGAINdisp
    END IF
END IF
'************
IF VAL(ITEMS$(0)) = 10 OR G$ = "A" THEN 'Rotations
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    CALL ROTS(XYlim(), XYscr(), XYplt(), XYout(), Contr())
    IF Contr(3) <> 0 THEN CLOSE #3
END IF
'************
IF VAL(ITEMS$(0)) = 11 OR G$ = "B" THEN 'BallInfo
    ITMP$ = ITEMS$(0): GP$ = G$
    CALL BallINFO(XYlim(), XYscr(), XYout(), Contr())
END IF
'************
IF VAL(ITEMS$(0)) = 12 OR G$ = "C" THEN 'Make ASCII
    ITMP$ = ITEMS$(0): GP$ = G$
    CALL DeleteBALLS(XYlim(), XYscr(), XYout(), Contr(), NFILE$)
    IF NFILE$ <> "" THEN
        IF NFILE$ <> " " THEN
            Cfile$ = NFILE$
        END IF
        GOTO AGAINdisp
    END IF
END IF
'************
IF VAL(ITEMS$(0)) = 13 OR G$ = "D" THEN 'Polygon groups
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    CALL PolyGroups(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    IF Contr(3) <> 0 THEN CLOSE #3
END IF
'************
IF VAL(ITEMS$(0)) = 14 OR G$ = "E" THEN 'Two BALLS
    ITMP$ = ITEMS$(0): GP$ = G$
    CALL FindTwoBalls(XYlim(), XYscr(), XYout(), Contr())
END IF
'************
IF VAL(ITEMS$(0)) = 15 OR G$ = "F" THEN 'Save ASCII
    ITMP$ = ITEMS$(0): GP$ = G$
    IF NFL = 0 THEN
        CALL ASCIIdump(CfileS$)
    ELSE
        CALL SAVEascii(Cfile$)
    END IF
END IF
'************
IF VAL(ITEMS$(0)) = 16 OR G$ = "G" THEN 'Velocities
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    IFORD = 0
    '
    Sfile$ = Cfile$
    NEWFILE = 0
    CALL MakeNext(Sfile$, NEWFILE)
    IF NEWFILE = 1 THEN
        NOBFILE = 0
        CALL CHECKfile(Sfile$, NOBFILE)
    END IF
    IF NOBFILE = 0 THEN
        Ln$ = "Using Forward " + Sfile$
        IFORD = 1
    ELSE
        NEWFILE = 0
        Sfile$ = Cfile$
        CALL MakePrev(Sfile$, NEWFILE)
        IF NEWFILE = 1 THEN
            NOBFILE = 0
            CALL CHECKfile(Sfile$, NOBFILE)
            IF NOBFILE = 0 THEN
                Ln$ = "Using Backward " + Sfile$
                IFORD = 2
            END IF
        END IF
    END IF
    CALL INITfileZZ(Sfile$)
    'CALL VELS(XYlim(), XYscr(), XYplt(), XYout(), Contr())
    CALL PlotVELOCITY(XYlim(), XYscr(), XYplt(), XYout(), Contr(), IFORD, CLR, DEt)
    Ln$ = Ln$ + " (DEt= " + STR$(DEt) + ")"
    LOCATE 1, 6: PRINT Ln$;
    IF Contr(3) <> 0 THEN CLOSE #3
END IF

IF VAL(ITEMS$(0)) = 17 OR G$ = "H" THEN 'Graphs Menu
    Cfile$ = Cfilei$
    ITMP$ = ITEMS$(0): GP$ = G$
    FOR I = 1 TO 10: XYlimG(I) = XYlim(I): XYscrG(I) = XYscr(I): NEXT
    CALL Graphs(XYlimG(), XYscrG(), XYpltG(), XYoutG(), ContrG(), LablG$(), Cfile$, Dfile$)
    CLOSE #1
    WINDOW: VIEW: CLS
    NEWFILE = 1
    GOTO AGAINdisp
END IF
'************
IF VAL(ITEMS$(0)) = 18 OR G$ = "I" THEN 'Histogram menu
    ITMP$ = ITEMS$(0): GP$ = G$
    PPOS = RECHIS
    CALL NEWHIST(1, PPOS)
    CLOSE #1
    WINDOW: VIEW: CLS
    NEWFILE = 1
    GOTO AGAINdisp
END IF
'************
IF VAL(ITEMS$(0)) = 19 OR G$ = "J" THEN 'Initialize
    ITMP$ = ITEMS$(0): GP$ = G$
    WINDOW: VIEW (0, 0)-(XYscr(2), 479): CLS: VIEW
    CALL ReviewContacts
END IF
IF VAL(ITEMS$(0)) = 20 OR G$ = "K" THEN 'First shell
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    CALL FirstShell(XYlim(), XYscr(), XYplt(), XYout(), Labl$(), Contr())
    IF Contr(3) <> 0 THEN CLOSE #3
    IF AUT = 1 THEN
        ITEMS$(0) = "": G$ = ""
    END IF
END IF

IF VAL(ITEMS$(0)) = 21 OR G$ = "L" THEN 'New file
    CLOSE #1
    CALL MakeFirsFile(Cfile$)
    WINDOW: VIEW: CLS
    GOTO NEWF
    ITMP$ = ITEMS$(0): GP$ = G$
    CLOSE
    GOTO NEWF
END IF
IF VAL(ITEMS$(0)) = 22 OR G$ = "M" THEN 'Displacements
    ITMP$ = ITEMS$(0): GP$ = G$
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    CALL DISPL(XYlim(), XYscr(), XYplt(), XYout(), Contr())
    IF Contr(3) <> 0 THEN CLOSE #3
END IF
IF VAL(ITEMS$(0)) = 23 OR G$ = "N" THEN 'Summary
    IF NFL = 0 THEN
        WINDOW: VIEW: CLS
    ELSE
        ITMP$ = ITEMS$(0): GP$ = G$
        PPOS = RECMAX
        CALL SUMMARY(1, PPOS, Cfile$)
        CLOSE #1
        WINDOW: VIEW: CLS
        NEWFILE = 1
        GOTO AGAINdisp
    END IF
END IF
IF VAL(ITEMS$(0)) = 24 OR G$ = "O" THEN 'Poly ACT / ALL
    IF Contr(3) <> 0 THEN
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    END IF
    IF NFL = 0 THEN
        POLYSTYLE% = 0
        LOCATE 1, 6: PRINT "ALL POLYGONS"
    ELSE
        POLYSTYLE% = 1
        LOCATE 1, 6: PRINT "ACT POLYGONS"
    END IF
    CALL PlotPolygons(13)
    CALL PolyINFO(XYlim(), XYscr(), XYout(), Contr())
    IF Contr(3) <> 0 THEN CLOSE #3
END IF
'
GOTO MainPoll
'
'    File error handler
'
NOFILE:
IF NOBFILE = 22 THEN
    LOCATE 12, 1: PRINT "22", ERR
    WHILE INKEY$ = "": WEND
END IF
IF ERR = 53 OR ERR = 64 THEN
    CLOSE #9
    NOBFILE = 1

    RESUME NEXT
ELSE
    IF ERR = 71 THEN RESUME NEXT
    IF ERR = 55 THEN
        NOBFILE = 55
        RESUME NEXT
    END IF
    ON ERROR GOTO 0
    STOP
END IF

REM $STATIC
FUNCTION AA (I)
CALL READNUM(I, Z$)
CC# = CVD(Z$)
AA = CC#
END FUNCTION

REM $STATIC
FUNCTION ZZ (I)
CALL READNUMZZ(I, Z$)
CC# = CVD(Z$)
ZZ = CC#
END FUNCTION

SUB AAI (PS1, VL)
SHARED LNW%, FW%, RN, RNP, FLD$
'puts number in position ps
PS = PS1 + 1
BT = (PS - 1) * LNW% + 1
RN = INT((BT - 1) / FW%) + 1
PSR = BT - (RN - 1) * FW%
IF RN <> RNP THEN
    GET #1, RN
    RNP = RN
END IF
Z$ = MKS$(VL)
F$ = ""
FOR N% = 1 TO LNW%
    F$ = F$ + MID$(Z$, LNW% + 1 - N%, 1)
NEXT
MID$(FLD$, PSR, 4) = F$
PUT #1, RN
END SUB

SUB ADDPOINT (XYP(), XYC(), IDP(), IDC(), NV, XYins(), IORIG, IPO())
'
'   XL, YL - Last added point into XYins ; XYP(), IDP() array from which to add
'
np = XYP(0)
L = XYins(0)
XL = XYins(2 * L - 1)
YL = XYins(2 * L)
X0 = XYins(1)
Y0 = XYins(2)
'
'  Search where the last point is on the current polygon
'
FOR I = 1 TO np
    IF XYP(2 * I - 1) = XL AND XYP(2 * I) = YL THEN
        IDO = I + 1
        GOTO 100
    END IF
NEXT
100 '
OUTFLAG = 0
FOR I = IDO TO IDO + np - 1
    IP = I
    IF IP > np THEN
        IP = IP - np
    END IF
    IF IDP(IP) = NV THEN
        XL = XYP(2 * IP - 1)
        YL = XYP(2 * IP)
        IF XL <> X0 AND YL <> Y0 THEN
            L = L + 1
            XYins(2 * L - 1) = XL
            XYins(2 * L) = YL
            IDP(IP) = -NV
            IPO(L) = IORIG * NV
            OUTFLAG = 0
        ELSE
            GOTO ADDfin
        END IF
    END IF
    IF OUTFLAG = 1 GOTO ADDfin
    IF IDP(IP) = 2 THEN
        XL = XYP(2 * IP - 1)
        YL = XYP(2 * IP)
        IF XL <> X0 AND YL <> Y0 THEN
            L = L + 1
            XYins(2 * L - 1) = XYP(2 * IP - 1)
            XYins(2 * L) = XYP(2 * IP)
            IDP(IP) = -2
            IPO(L) = 2 * ABS(IORIG)
            CALL MARK(XL, YL, XYC(), IDC())
            OUTFLAG = 1
        END IF
    END IF
NEXT
ADDfin:
XYins(0) = L
END SUB

SUB ARROW (X1, Y1, X2, Y2, CLR)
Dx = X2 - X1: Dy = Y2 - Y1: D = SQR(Dx * Dx + Dy * Dy)
IF D = 0 THEN
    PSET (X1, Y1), CLR
    EXIT SUB
END IF
CSL = Dx / D
SNL = Dy / D
SNP = CSL
CSP = -SNL
'
XA = X1 + 5 / 6 * CSL * D
Ya = Y1 + 5 / 6 * SNL * D
'
XP1 = XA + 1 / 12 * D * CSP
YP1 = Ya + 1 / 12 * D * SNP
'
XP2 = XA - 1 / 12 * D * CSP
YP2 = Ya - 1 / 12 * D * SNP
'
LINE (X1, Y1)-(XA, Ya), CLR
LINE (XP1, YP1)-(XP2, YP2), CLR
LINE (XP1, YP1)-(X2, Y2), CLR
LINE (XP2, YP2)-(X2, Y2), CLR
XCA = (XP1 + XP2 + X2) / 3
YCA = (YP1 + YP2 + Y2) / 3
'     PAINT (XCA, YCA), CLR, CLR
END SUB

SUB ASCIIdump (CfileS$)
'
'     GOTO VVV
N = INSTR(CfileS$, ".")
IF N <> 0 THEN
    Afile$ = MID$(CfileS$, 1, N - 1) + ".TXT"
ELSE
    Afile$ = CfileS$ + ".TXT"
END IF
OPEN Afile$ FOR OUTPUT AS #5
PRINT #5, USING "Number of particles = #### "; NDISK
PRINT #5, "Particles list";
IAD = M1
'
FOR N = 1 TO NDISK
    '
    PRINT #5, USING "N = ###### B = # T = ##.######^^^^"; N; AA(IAD + 8); AA(IAD + 9)
    '
    '     Xo,Yo,To
    '
    PRINT #5, USING "R0 ##.######^^^^ ##.######^^^^ ##.######^^^^"; AA(IAD); AA(IAD + 1); AA(IAD + 10)
    '
    '     Dx,Dy,Dt
    '
    PRINT #5, USING "DR ##.######^^^^ ##.######^^^^ ##.######^^^^"; AA(IAD + 11); AA(IAD + 12); AA(IAD + 13)
    '
    '     Fx,Fy,Mt
    '
    PRINT #5, USING "FB ##.######^^^^ ##.######^^^^ ##.######^^^^"; AA(IAD + 5); AA(IAD + 6); AA(IAD + 7)
    '
    '     Vx,Vy,Vt
    '
    PRINT #5, USING "VB ##.######^^^^ ##.######^^^^ ##.######^^^^"; AA(IAD + 2); AA(IAD + 3); AA(IAD + 4)
    '
    IAD = IAD + NDPART
NEXT
PRINT #5, "End of disk list"
PRINT #5, USING "Number of contacts = ######"; NCONT
VVV:
IAD = M1 + NDISK * NDPART
'
ASCIIdmp:
'
IB1 = AA(IAD)
IF IB1 = 0 GOTO ASCIIfin
IB2 = AA(IAD + 1)
NC = NC + 1
NB1 = (IB1 - M1) / NDPART + 1
NB2 = (IB2 - M1) / NDPART + 1
DN = -AA(IAD + 2)
DT = -AA(IAD + 3)
FRN = -AA(IAD + 4)
FRT = -AA(IAD + 5)
PRINT USING "##### ##### ##### ##.#####^^^^ ##.#####^^^^ ##.#####^^^^ ##.#####^^^^"; NC; NB1; NB2; FRN; FRT; DN; DT
IAD = IAD + NCPART
GOTO ASCIIdmp:
ASCIIfin:
CALL CLEAN
END SUB

SUB AssignBOUNDARY (XYlim(), XYscr(), XYout(), Contr(), NFILE$)
DIM DBALLS(100)
'
LOCATE 1, 6: PRINT "Assign boundary 1 - Circular 2 - Rectangular";
'
DELB = 0
StartBOUND:
LOCATE 1, 6
PRINT USING "1 - Select ballls (##) | 2 - Assign | 3 - Exit"; DELB
CALL DELAY(G$)
IF VAL(G$) = 1 THEN
    IF DELB <> 0 THEN
        CALL PlotBalls(XYlim(), XYscr())
        DELB = 0
    END IF
    CALL MarkBALLS(XYlim(), XYscr(), XYout(), Contr(), DBALLS(), "boundary")
    DELB = DBALLS(0)
    GOTO StartBOUND
END IF
IF VAL(G$) = 2 THEN
    CALL GetContactList
    CALL GetDiskList
    CALL CleanBoundary
    CALL DISPLAY("Inserting disk ")
    FOR I = 1 TO DELB
        LOCATE 1, 22: PRINT USING "####"; I
        N = DBALLS(I)
        IAD = M1 + (N - 1) * NDPART
        CALL InsertBBall(N, NBD)
        FOR J = I + 1 TO DELB
            IF DBALLS(J) = NBD THEN
                DBALLS(J) = N
                EXIT FOR
            END IF
        NEXT
    NEXT
    CALL PlotBalls(XYlim(), XYscr())
    CALL RewriteArray(NFILE$)
    EXIT SUB
END IF
IF VAL(G$) = 3 THEN
    NFILE$ = " "
END IF
GOTO StartBOUND
END SUB

SUB AssignGroup (G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
K = INSTR(1, G$, "!")
NBL = 0
NHT = 1
NGP = 1
IF K = 0 THEN
    ' G$ = 5   - Use group 5
    ' G$=-567  - Ball 567 in group N=9
    N = VAL(G$)
    IF N < 0 THEN
        NBL = -N
        N = 9
    END IF
ELSE
    ' G$=!-5 means use histogram 5 with default group 1
    ' G$=!18 means use group 18 with default hisogram 1
    ' N=7 group selected
    IF LEN(SItems$(1)) > 12 THEN
        NHT = VAL(MID$(SItems$(1), 12))
    ELSE
        NHT = 1
    END IF
    IF LEN(SItems$(2)) > 12 THEN
        NGP = VAL(MID$(SItems$(2), 12))
    ELSE
        NGP = 1
    END IF
    G$ = MID$(G$, 1, K - 1)
    N = VAL(G$)
    IF N < 0 THEN
        NHT = -N
    ELSE
        NGP = N
    END IF
    N = 7
END IF
IF N = 1 THEN
    HistNo = 0
    Saddr(0) = HistNo
    SItems$(1) = "1 - S11      ": Sname$(1) = "S11": Saddr(1) = 0: Scale(1) = -1
    SItems$(2) = "2 - S12      ": Sname$(2) = "S12": Saddr(2) = 0: Scale(2) = -1
    SItems$(3) = "3 - S21      ": Sname$(3) = "S21": Saddr(3) = 0: Scale(3) = -1
    SItems$(4) = "4 - S22      ": Sname$(4) = "S22": Saddr(4) = 0: Scale(4) = -1
    SItems$(5) = "5 - Os       ": Sname$(5) = "Sp1": Saddr(5) = 0: Scale(5) = 1
    SItems$(6) = "6 - S1       ": Sname$(6) = "S1": Saddr(6) = 0: Scale(6) = -1
    SItems$(7) = "7 - S2       ": Sname$(7) = "S2": Saddr(7) = 0: Scale(7) = -1
    SItems$(8) = "8 - p        ": Sname$(8) = "P": Saddr(8) = 0: Scale(8) = -1
    SItems$(9) = "9 - q        ": Sname$(9) = "Q": Saddr(9) = 0: Scale(9) = 1
    SItems$(10) = "A - q/p     ": Sname$(10) = "SINfi": Saddr(10) = 0: Scale(10) = -1
    SItems$(11) = "B - Sr      ": Sname$(11) = "SRAT": Saddr(11) = 0: Scale(11) = 1
    SItems$(12) = "C - Log p   ": Sname$(12) = "  ": Saddr(12) = 0: Scale(12) = -1
    SItems$(13) = "D - Print ON": Sname$(13) = "  ": Saddr(13) = 0: Scale(13) = 1
    SItems$(14) = "E - Exit    ": Sname$(14) = "  ": Saddr(14) = 0: Scale(14) = 1
    SNitems = 14
END IF
IF N = 2 THEN
    HistNo = 0
    Saddr(0) = HistNo
    SItems$(1) = "1 - E11      ": Sname$(1) = "EB11": Saddr(1) = 0: Scale(1) = 1
    SItems$(2) = "2 - E12      ": Sname$(2) = "EB12": Saddr(2) = 0: Scale(2) = 1
    SItems$(3) = "3 - E21      ": Sname$(3) = "EB21": Saddr(3) = 0: Scale(3) = 1
    SItems$(4) = "4 - E22      ": Sname$(4) = "EB22": Saddr(4) = 0: Scale(4) = 1
    SItems$(5) = "5 - Os       ": Sname$(5) = "EPR1": Saddr(5) = 0: Scale(5) = 1
    SItems$(6) = "6 - E1       ": Sname$(6) = "E1": Saddr(6) = 0: Scale(6) = 1
    SItems$(7) = "7 - E2       ": Sname$(7) = "E2": Saddr(7) = 0: Scale(7) = 1
    SItems$(8) = "8 - Ev       ": Sname$(8) = "Ev": Saddr(8) = 0: Scale(8) = 1
    SItems$(9) = "9 - Et       ": Sname$(9) = "Et": Saddr(9) = 0: Scale(9) = 1
    SItems$(10) = "A - Ew      ": Sname$(10) = "Ew": Saddr(10) = 0: Scale(10) = 1
    SItems$(11) = "B - dEv/dEt ": Sname$(11) = "   ": Saddr(11) = 0: Scale(11) = 1
    SItems$(12) = "C - DARC    ": Sname$(12) = "DARC": Saddr(11) = 0: Scale(11) = 1
    SItems$(13) = "D - Print ON": Sname$(13) = "   ": Saddr(12) = 0: Scale(12) = 1
    SItems$(14) = "E - Exit    ": Sname$(14) = "   ": Saddr(13) = 0: Scale(13) = 1
    SNitems = 14
END IF
IF N = 3 THEN
    HistNo = 3
    Saddr(0) = HistNo
    SItems$(1) = "1 - a       ": Sname$(1) = "Contact normal": Saddr(1) = 1: Scale(1) = 1!
    SItems$(2) = "2 - b       ": Sname$(2) = "Contact normal": Saddr(2) = 2: Scale(2) = 1!
    SItems$(3) = "3 - Oa      ": Sname$(3) = "Contact normal": Saddr(3) = 3: Scale(3) = 1!
    SItems$(4) = "4 - Ob      ": Sname$(4) = "Contact normal": Saddr(4) = 4: Scale(4) = 1!
    SItems$(5) = "5 - Coord   ": Sname$(5) = "Gamma": Saddr(5) = 0: Scale(5) = 1!
    SItems$(6) = "6 - Density ": Sname$(6) = "PakRo": Saddr(6) = 0: Scale(6) = 1!
    SItems$(7) = "7 - Coord_Fl": Sname$(7) = "GammaFl": Saddr(7) = 0: Scale(7) = 1!
    SItems$(8) = "8 - Floaters": Sname$(8) = "NFLOAT": Saddr(8) = 0: Scale(8) = 1!
    SItems$(9) = "9 - Cre Cont": Sname$(9) = "XCRE": Saddr(9) = 0: Scale(9) = 1!
    SItems$(10) = "10 - Des Cont": Sname$(10) = "XDES": Saddr(10) = 0: Scale(10) = 1!
    SItems$(11) = "11- Print ON": Sname$(11) = "    ": Saddr(11) = 0: Scale(11) = 1!
    SItems$(12) = "12- Exit    ": Sname$(12) = "    ": Saddr(12) = 0: Scale(12) = 1!
    SNitems = 12
END IF
IF N = 4 THEN
    HistNo = 5
    Saddr(0) = HistNo
    SItems$(1) = "1 - af      ": Sname$(1) = "Averg normal": Saddr(1) = 1: Scale(1) = 1!
    SItems$(2) = "2 - bf      ": Sname$(2) = "Averg normal": Saddr(2) = 2: Scale(2) = 1!
    SItems$(3) = "3 - Oa      ": Sname$(3) = "Averg normal": Saddr(3) = 3: Scale(3) = 1!
    SItems$(4) = "4 - Ob      ": Sname$(4) = "Averg normal": Saddr(4) = 4: Scale(4) = 1!
    SItems$(5) = "5 - Fo      ": Sname$(5) = "Averg normal": Saddr(5) = 5: Scale(5) = 1!
    SItems$(6) = "6 - Print ON": Sname$(6) = "   ": Saddr(6) = 0: Scale(6) = 1!
    SItems$(7) = "7 - Exit    ": Sname$(7) = "   ": Saddr(7) = 0: Scale(7) = 1!
    SNitems = 7
END IF
IF N = 5 THEN
    HistNo = 7
    Saddr(0) = HistNo
    SItems$(1) = "1 - at      ": Sname$(1) = "Averg tangnt": Saddr(1) = 1: Scale(1) = 1!
    SItems$(2) = "2 - bt      ": Sname$(2) = "Averg tangnt": Saddr(2) = 2: Scale(2) = 1!
    SItems$(3) = "3 - Oa      ": Sname$(3) = "Averg tangnt": Saddr(3) = 3: Scale(3) = 1!
    SItems$(4) = "4 - Ob      ": Sname$(4) = "Averg tangnt": Saddr(4) = 4: Scale(4) = 1!
    SItems$(5) = "5 - Fa      ": Sname$(5) = "Averg tangnt": Saddr(5) = 5: Scale(5) = 1!
    SItems$(6) = "6 - Un      ": Sname$(6) = "Area": Saddr(6) = 0: Scale(6) = 1!
    SItems$(7) = "7 - Un      ": Sname$(7) = "Area": Saddr(7) = 0: Scale(7) = 1!
    SItems$(8) = "8 - Print ON": Sname$(8) = "    ": Saddr(8) = 0: Scale(8) = 1!
    SItems$(9) = "9 - Exit    ": Sname$(9) = "   ": Saddr(9) = 0: Scale(9) = 1!
    SNitems = 9
END IF
IF N = 6 THEN
    HistNo = 1
    Saddr(0) = HistNo
    SItems$(1) = "1 - ao      ": Sname$(1) = "Particle orientation": Saddr(1) = 1: Scale(1) = 1
    SItems$(2) = "2 - bo      ": Sname$(2) = "Particle orientation": Saddr(2) = 2: Scale(2) = 1
    SItems$(3) = "3 - Oa      ": Sname$(3) = "Particle orientation": Saddr(3) = 3: Scale(3) = 1
    SItems$(4) = "4 - Ob      ": Sname$(4) = "Particle orientation": Saddr(4) = 4: Scale(4) = 1
    SItems$(5) = "5 - Ecc     ": Sname$(5) = "ECC": Saddr(5) = 0: Scale(5) = 1
    SItems$(6) = "6 - Print ON": Sname$(6) = "Particle orientation": Saddr(6) = 0: Scale(6) = 1
    SItems$(7) = "7 - Exit    ": Sname$(7) = "Particle orientation": Saddr(7) = 0: Scale(7) = 1
    SNitems = 7
END IF
IF N = 7 THEN
    HistNo = 0
    NHT$ = MID$(STR$(NHT), 2): IF LEN(NHT$) < 2 THEN NHT$ = "0" + NHT$
    NGP$ = MID$(STR$(NGP), 2): IF LEN(NGP$) < 2 THEN NGP$ = "0" + NGP$
    SItems$(1) = "1 - Histo # " + NHT$: Saddr(1) = 0: Scale(1) = 1
    SItems$(2) = "2 - Group # " + NGP$: Saddr(2) = 0: Scale(2) = 1
    SItems$(3) = "3 - H gr - 01": Saddr(3) = 0: Scale(3) = 1
    SItems$(4) = "4 - V gr - 36": Saddr(4) = 0: Scale(4) = 1
    SItems$(5) = "5 - Run group": Saddr(5) = 0: Scale(5) = 1
    SItems$(6) = "6 -    ag    ": Saddr(6) = 1: Scale(6) = 1
    SItems$(7) = "7 -    bg    ": Saddr(7) = 2: Scale(7) = 1
    SItems$(8) = "8 -    Oa    ": Saddr(8) = 3: Scale(8) = 1
    SItems$(9) = "9 -    Ob    ": Saddr(9) = 4: Scale(9) = 1
    SItems$(10) = "A - List Hist": Saddr(10) = 0: Scale(10) = 1
    SItems$(11) = "B - Print ON": Saddr(11) = 0: Scale(11) = 1
    SItems$(12) = "C - Exit    ": Saddr(12) = 0: Scale(12) = 1
    SNitems = 12
END IF
IF N = 8 THEN
    HistNo = 0
    Saddr(0) = HistNo
    SItems$(1) = "1 - FbMAX   ": Sname$(1) = "FBmax": Saddr(1) = 0: Scale(1) = 1
    SItems$(2) = "2 - FbAVG   ": Sname$(2) = "FBavg": Saddr(2) = 0: Scale(2) = 1
    SItems$(3) = "3 - VbMAX   ": Sname$(3) = "VBmax": Saddr(3) = 0: Scale(3) = 1
    SItems$(4) = "4 - VbAVG   ": Sname$(4) = "VBavg": Saddr(4) = 0: Scale(4) = 1
    SItems$(5) = "5 - MbMAX   ": Sname$(5) = "ZOmax": Saddr(5) = 0: Scale(5) = 1
    SItems$(6) = "6 - MbAVG   ": Sname$(6) = "ZOavg": Saddr(6) = 0: Scale(6) = 1
    SItems$(7) = "7 - FnMAX   ": Sname$(7) = "FNmax": Saddr(7) = 0: Scale(7) = 1
    SItems$(8) = "8 - FsMAX   ": Sname$(8) = "FSmax": Saddr(8) = 0: Scale(8) = 1
    SItems$(9) = "9 - FnAVG   ": Sname$(9) = "FNavr": Saddr(9) = 0: Scale(9) = 1
    SItems$(10) = "C - Print ON": Sname$(10) = "   ": Saddr(10) = 0: Scale(10) = 1
    SItems$(11) = "D - Exit    ": Sname$(11) = "    : Saddr(11) = 0: Scale(11) = 1"
    SNitems = 11
END IF
IF N = 9 THEN
    NBLr = NBL
    SItems$(1) = "1 - Xdisp   ": Saddr(1) = 0: Scale(1) = NBL
    SItems$(2) = "2 - Ydisp   ": Saddr(2) = 1: Scale(2) = NBL
    SItems$(3) = "3 - XYdis   ": Saddr(3) = -1: Scale(3) = NBL
    SItems$(4) = "4 - Theta   ": Saddr(4) = 10: Scale(4) = NBL
    SItems$(5) = "5 - FXunb   ": Saddr(5) = 5: Scale(5) = NBL
    SItems$(6) = "6 - FYunb   ": Saddr(6) = 6: Scale(6) = NBL
    SItems$(7) = "7 - FFunb   ": Saddr(7) = -6: Scale(7) = NBL
    SItems$(8) = "8 - MZunb   ": Saddr(8) = 7: Scale(8) = NBL
    SItems$(9) = "9 - VXbal   ": Saddr(9) = 2: Scale(9) = NBL
    SItems$(10) = "A - VYbal  ": Saddr(10) = 3: Scale(10) = NBL
    SItems$(11) = "B- VVbal  ": Saddr(11) = -3: Scale(11) = NBL
    IF NBL = 0 THEN
        SItems$(12) = "C - Ball No ": Saddr(12) = 0: Scale(12) = 0
    ELSE
        SItems$(12) = "Ball " + STR$(NBLr): Saddr(12) = 0: Scale(12) = NBL
    END IF
    SItems$(13) = "C- ThetaAv ": Saddr(13) = -999: Scale(13) = 1!
    SItems$(14) = "D- Print ON": Saddr(14) = 0: Scale(14) = 1!
    SItems$(15) = "E- Exit    ": Saddr(15) = 0: Scale(15) = 1!
    SNitems = 15
END IF
'      A(122)=FX
'      A(123)=FY
'      A(124)=MO
'      A(125)=FBMAX
'      A(126)=MOMAX
'      A(127)=VBMAX
'      A(128)=FBAVG
'      A(129)=MOAVG
'      A(130)=VBAVG
'      A(131)=FCMAX
'      A(132)=FNMAX
'      A(133)=FSMAX
'      A(134)=FNAVG
'      A(135)=FSAVG
'      A(136)=FCAVG
'      A(137)=DLAVG
'      A(138)=DNAVG
'      A(139)=DSAVG
END SUB

FUNCTION ATAN2 (ARG1, ARG2)
pi = 4 * ATN(1!)
IF ABS(ARG2) <= 1E-11 THEN ARG2 = 1E-11
RRR = ARG1 / ARG2
ATN2 = ATN(RRR)
IF ARG2 < 0 THEN
    ATN2 = pi + ATN2
END IF
IF ATN2 < 0 THEN ATN2 = 2 * pi + ATN2
ATAN2 = ATN2
END FUNCTION

SUB AVRGTHETA (Theta)
SHARED ICALL, THREM()
SUM = 0
ICALL = ICALL + 1
FOR N = 1 TO NDISK
    LOCATE 1, 6: PRINT USING "####"; N
    IAD = M1 + (N - 1) * NDPART
    Theta = AA(IAD + 13)
    IF Theta > 100 THEN STOP
    IF ICALL = 1 THEN
        THREM(N) = Theta
        Theta = 0
    ELSE
        Theta = Theta - THREM(N)
    END IF
    SUM = SUM + Theta * Theta
NEXT
Theta = SQR(SUM / NDISK)
END SUB

SUB BallINFO (XYlim(), XYscr(), XYout(), Contr())
SHARED XSPMODE, YSPMODE, SPMODE, WDMODE, XDMODE, ND
SHARED Trans()
DIM NEIGH(20), IADCONT(20), APPLX(20), APPLY(20), FORX(20), FORY(20), XY(40)
DIM XYscrL(10), XYlimL(10), XYpltL(10), XYoutL(10), ContrL(10), LablL$(30), XYCN(4, 2)
DIM XYscrP(10), XYlimP(10), XYpltP(10), ContrP(10), LablP$(30)
DIM XINT1(20), YINT1(20), XINT2(20), YINT2(20), PNO(20)
REPINFO:
Mes$ = "Locate  particle (Esc - finish K - manual entry)   "
CALL LOCATOR(XYlim(), XYscr(), XYout(), G$, Contr())
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2)
Yoff = XYscr(3): Ylas = XYscr(4)
Trans(1) = (Xlas - Xoff) / (Xmax - Xmin)
Trans(3) = (Yoff - Ylas) / (Ymax - Ymin)
Trans(2) = Xoff - Trans(1) * Xmin
Trans(4) = Ylas - Trans(3) * Ymin
Trans(0) = 0

PCOPY 0, 1
IF UCASE$(G$) = "K" THEN
    CALL CLEAN
    LOCATE 1, 6: INPUT "Particle Number [0 - max force]"; FOUND
    IF FOUND = 0 THEN
        XYout(1) = 0.0: XYout(2) = 0.0
        XYout(3) = 0.0: XYout(4) = 0.0
        GOTO DOSEARCH
    END IF
    CALL GETDISC(FOUND, Xc, Yc, Tc, ITYPE, IBTYP)
    CALL CLEAN: LOCATE 1, 6: PRINT Mes$;
    CALL GETDISC(FOUND, XC1, YC1, THETA1, ITYPE1, IBTYP1)
    GOTO BPinfo
END IF
IF G$ = CHR$(27) THEN
    VIEW (SLMIN, 0)-(XSPMODE - 1, YSPMODE - 1): CLS
    CALL CLEAN
    EXIT SUB
END IF
DOSEARCH:
IF XYout(1) = XYout(2) AND XYout(3) = XYout(4) THEN
    Xo = XYout(1): Yo = XYout(3): IBTYP1 = 0
    CALL SearchBALL(Xo, Yo, THETA1, N, XC1, YC1, ITYPE1, IBTYP1, FOUND)
    IF FOUND = 0 GOTO REPINFO
    BPinfo:
    IABFOUND = M1 + (FOUND - 1) * NDPART
    IAD = IABFOUND
    CALL PLOTELS(XC1, YC1, THETA1, ITYPE1, IBTYP1, 15)
    IF ITYPE1 < 0 THEN
        RBAR1 = -ITYPE1
        ECC1 = 1
    ELSE
        RBAR1 = R(ITYPE1)
        ECC1 = E(ITYPE1)
    END IF
    SLMIN = XYscr(2) + 2: CMIN = INT(SLMIN / 8) + 2: CMAX = CMIN + 27
    IF CMAX < XDMODE THEN
        CMIN = CMIN + INT(0.5 * (XDMODE - CMAX))
    END IF
    VIEW (SLMIN, 0)-(XSPMODE - 1, YSPMODE - 1): CLS
    VIEW (XYscr(1), XYscr(3))-(XYscr(2), XYscr(4)) '
    '
    LOCATE 1, 6: PRINT USING "Particle ##### Addr ###### Type ###.### Bound ##"; FOUND; IABFOUND; AA(IAD + 9); AA(IAD + 8)
    LOCATE 2, CMIN: PRINT USING "Xc=####.###### Fx=#.#####^^^^"; (AA(IAD + 0) + AA(IAD + 11)); AA(IAD + 5)
    LOCATE 3, CMIN: PRINT USING "Yc=####.###### Fy=#.#####^^^^"; (AA(IAD + 1) + AA(IAD + 12)); AA(IAD + 6)
    LOCATE 4, CMIN: PRINT USING "To=###.####### Mt=#.#####^^^^"; (AA(IAD + 10) + AA(IAD + 13)); AA(IAD + 7)
    LOCATE 5, CMIN: PRINT USING "Vx=#.#####^^^^ Vy=#.#####^^^^"; AA(IAD + 2); AA(IAD + 3)
    LOCATE 6, CMIN: PRINT USING "Vv=#.#####^^^^ Ff=#.#####^^^^"; SQR(AA(IAD + 2) ^ 2 + AA(IAD + 3) ^ 2); SQR(AA(IAD + 5) ^ 2 + AA(IAD + 6) ^ 2)
    '
    NC = 0: KN = 0: WID = 3.5 * RBAR1
    WINDOW: VIEW
    FOR I% = 0 TO 10
        XYlimL(I%) = 0: XYscrL(I%) = 0: XYpltL(I%) = 0: ContrL(I%) = 0
        LablL$(I%) = ""
    NEXT
    XYlimL(1) = XC1 - WID: XYlimL(2) = XC1 + WID
    XYlimL(3) = YC1 - WID: XYlimL(4) = YC1 + WID
    XYlimL(8) = XSPMODE: XYlimL(9) = YSPMODE: XYlimL(10) = SPMODE
    XYscrL(3) = XYscr(2) + 35
    XYscrL(1) = XSPMODE - XYscrL(3) - 10: XYscrL(2) = XYscrL(1): XYscr(5) = 10
    XYscrL(4) = XYscr(4) - XYscrL(2) - 18

    XYscrP(1) = XYscrL(1): XYscrP(2) = XYscrL(2)
    XYscrP(3) = XYscrL(3):
    LablL$(1) = "X": LablL$(2) = "X"
    'ContrL(1) = -1
    CALL WIN(XYlimL(), XYscrL(), XYpltL(), LablL$(), ContrL())
    CALL PLOTELS(XC1, YC1, THETA1, ITYPE1, IBTYP1, 15)
    XYscrP(4) = XYscrL(3) - XYscrP(2) - 48
    IF XYscrP(4) < 14 THEN
        XYscrP(4) = 14
        XYscrL(2) = XYscrL(3) - 48 - XYscrP(4)
    END IF
    NC = 0: KN = 0
    FORX(0) = AA(IAD + 5)
    FORY(0) = AA(IAD + 6)
    IAD = M1 + NDISK * NDPART
    SMX = 0: SMY = 0
    XY(1) = 0: XY(2) = 0
    FAV = 0!
    ContInfo:
    IB1 = AA(IAD)
    IF IB1 = 0 GOTO ContFin
    IB2 = AA(IAD + 1)
    NC = NC + 1
    LOCATE WDMODE, 1: PRINT USING "####"; NC;
    G$ = INKEY$
    IF G$ = CHR$(27) GOTO ContFin
    KF = 0
    IF IB1 = IABFOUND THEN
        KF = 1
        KN = KN + 1
        NEIGH(KN) = IB2
        PNO(KN) = (IB2 - M1) / NDPART + 1
        IADCONT(KN) = IAD
        CALL GETDISC(-IB2, XC2, YC2, THETA2, ITYPE2, IBTYP2)
    END IF
    IF IB2 = IABFOUND THEN
        KF = 1
        KN = KN + 1
        NEIGH(KN) = IB1
        PNO(KN) = (IB1 - M1) / NDPART + 1
        IADCONT(KN) = IAD
        CALL GETDISC(-IB1, XC2, YC2, THETA2, ITYPE2, IBTYP2)
    END IF
    IF KF = 1 THEN
        IF ITYPE2 < 0 THEN
            RBAR2 = -ITYPE2
            ECC2 = 1
        ELSE
            RBAR2 = R(ITYPE2)
            ECC2 = E(ITYPE2)
        END IF
        CALL PLOTELS(XC2, YC2, THETA2, ITYPE2, IBTYP2, 10)
        FRN = -AA(IAD + 4)
        FRT = -AA(IAD + 5)
        CALL FORCES(XC1, YC1, RBAR1, ECC1, THETA1, XC2, YC2, RBAR2, ECC2, THETA2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, FRN, FRT, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA, BDT,  _
AMU, XYCN())
        IF FRN <> 0 AND IFLAG = 0 THEN
            KN = KN - 1
            GOTO NextCont
        END IF
        IF FRN = 0 AND IFLAG = 2 THEN
            BEEP
            KN = KN - 1
            GOTO NextCont
        END IF
        IF IFLAG = 0 THEN
            KN = KN - 1
            GOTO NextCont
        END IF
        APPLX(KN) = XYCN(1, 1)
        APPLY(KN) = XYCN(1, 2)
        FORX(KN) = FRN * CNX1 + FRT * CTX1
        FORY(KN) = FRN * CNY1 + FRT * CTY1
        XINT1(KN) = XYCN(2, 1): YINT1(KN) = XYCN(2, 2)
        XINT2(KN) = XYCN(3, 1): YINT2(KN) = XYCN(3, 2)
        SMX = SMX + FORX(KN)
        SMY = SMY + FORY(KN)
        FF = SQR(FORX(KN) ^ 2 + FORY(KN) ^ 2)
        FAV = FAV + FF
        KN1 = KN + 1
        XY(2 * KN1 - 1) = SMX
        XY(2 * KN1) = SMY
    END IF
    NextCont:
    IAD = IAD + NCPART
    GOTO ContInfo
    ContFin: IF KN = 0 GOTO InfoNuthing
    FAV = FAV / KN
    WINWX = .5 * (XYlimL(2) - XYlimL(1))
    WINWY = .5 * (XYlimL(4) - XYlimL(3))
    WID = WINWX
    IF WINWY < WID THEN WID = WINWY
    KN1 = KN + 1
    IF KN > 2 THEN
        XY(0) = KN1
        XY(1) = 0: XY(2) = 0
        CALL Cg(XY(), Xg, Yg, Area)
    ELSE
        Xg = APPLX(1)
        Yg = APPLY(1)
    END IF
    Rmax = 0
    FOR i = 1 TO KN1
        XY(2 * i - 1) = XY(2 * i - 1) - Xg
        XY(2 * i) = XY(2 * i) - Yg
        RR = SQR(XY(2 * i - 1) ^ 2 + XY(2 * i) ^ 2)
        IF RR > Rmax THEN Rmax = RR
    NEXT
    WID = .9 * WID
    FOR i = 1 TO KN1
        XY(2 * i - 1) = (XY(2 * i - 1)) * WID / Rmax + XC1
        XY(2 * i) = XY(2 * i) * WID / Rmax + YC1
    NEXT
    FOR i = 1 TO KN
        XF = APPLX(i): Yf = APPLY(i)
        XI = XF - 2 * RBAR1 * FORX(i) / FAV
        YI = Yf - 2 * RBAR1 * FORY(i) / FAV
        CALL ARROW(XI, YI, XF, Yf, 15 - i)
    NEXT
    LOCATE WDMODE, 1: PRINT "    ";
    XYlimP(8) = XSPMODE: XYlimP(9) = YSPMODE: XYlimP(10) = SPMODE
    CALL WIN(XYlimL(), XYscrP(), XYpltL(), LablL$(), ContrL())
    FOR i = 1 TO KN
        N1 = i: N2 = i + 1
        IF N2 > KN1 THEN N2 = 1
        XI = XY(2 * N1 - 1): YI = XY(2 * N1)
        XF = XY(2 * N2 - 1): Yf = XY(2 * N2)
        CALL ARROW(XI, YI, XF, Yf, 15 - i)
    NEXT
    LOCATE WDMODE, CMIN: PRINT USING "Equilibrium ##.#####   "; SQR(SMX ^ 2 + SMY ^ 2) / FAV;
    InfoNuthing:
    WHILE INKEY$ = "": WEND
    WINDOW: VIEW: CLS
    WID = RBAR1 * (1 + ECC1)
    XYlimL(1) = XC1 - WID: XYlimL(2) = XC1 + WID
    XYlimL(3) = YC1 - WID: XYlimL(4) = YC1 + WID
    REDU:
    XYlimL(8) = XSPMODE: XYlimL(9) = YSPMODE: XYlimL(10) = SPMODE
    XYscrL(1) = YSPMODE - 70: XYscrL(2) = YSPMODE - 40
    XYscrL(3) = 40: XYscrL(4) = 0
    ContrL(1) = -1
    CALL WIN(XYlimL(), XYscrL(), XYpltL(), LablL$(), ContrL())
    CALL PLOTELS(XC1, YC1, THETA1, ITYPE1, IBTYP1, 15)
    FOR i = 1 TO KN
        IB2 = NEIGH(i)
        CALL GETDISC(-IB2, XC2, YC2, THETA2, ITYPE2, IBTYP2)
        CALL PLOTELS(XC2, YC2, THETA2, ITYPE2, IBTYP2, 16)
        Xc = XINT1(i)
        Yc = YINT1(i)
        CALL DRAWCROSS(Xc, Yc, 5, 9)
        Xc = XINT2(i)
        Yc = YINT2(i)
        CALL DRAWCROSS(Xc, Yc, 5, 9)
        LOCATE WDMODE, 6: PRINT USING "# (#) IAD=####### PN=#### X1=#####.### Y1=#####.### X2=#####.### Y2=#####.###"; i; KN; IADCONT(i), PNO(i), XINT1(i), YINT1(i); XINT2(i), YINT2(i);
        LOCATE 1, 6: PRINT PNO(i)
        WHILE INKEY$ = "": WEND
    NEXT
    '
    LOCATE 1, 6: PRINT "Current ND"; ND; "Press R to reset"
    CALL LOCATOR(XYlimL(), XYscrL(), XYoutL(), G$, Contr())
    IF UCASE$(G$) = "R" THEN
        LOCATE 1, 6: PRINT "                                     ";
        LOCATE 1, 6: INPUT "ND"; ND
    END IF
    '
    IF XYoutL(1) <> XYoutL(2) AND XYoutL(3) <> XYoutL(4) THEN
        XYlimL(1) = XYoutL(1): XYlimL(2) = XYoutL(2)
        XYlimL(3) = XYoutL(3): XYlimL(4) = XYoutL(4)
        WINDOW: VIEW: CLS
        GOTO REDU
    END IF
    WINDOW (XYlim(1), XYlim(3))-(XYlim(2), XYlim(4))
    VIEW (XYscr(1), XYscr(3))-(XYscr(2), XYscr(4))
    WHILE INKEY$ = "": WEND
    PCOPY 1, 0
    LOCATE 1, 6: PRINT Mes$;
    COLOR 14
    GOTO REPINFO
END IF
FOUND = 0
GOTO REPINFO
END SUB

SUB PolyINFO (XYlim(), XYscr(), XYout(), Contr())
SHARED XSPMODE, YSPMODE, SPMODE, WDMODE, XDMODE, ND
SHARED Trans()
DIM NEIGH(20), IADCONT(20), APPLX(20), APPLY(20), FORX(20000), FORY(20000), XY(20000), IC(20000), IN(20000)
DIM XYscrL(10), XYlimL(10), XYpltL(10), XYoutL(10), ContrL(10), LablL$(30), XYCN(4, 2)
DIM XYscrP(10), XYlimP(10), XYpltP(10), ContrP(10), LablP$(30)
DIM XINT1(20), YINT1(20), XINT2(20), YINT2(20), PNO(20)
DIM XYN(20000)
INIT = 0
REPPINFO:
Mes$ = "Locate  polygon (Esc - finish K - manual entry)   "
CALL LOCATOR(XYlim(), XYscr(), XYout(), G$, Contr())
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2)
Yoff = XYscr(3): Ylas = XYscr(4)
SLMIN = Xlas + 2: SP% = INT((XSPMODE - SLMIN) / 8)
Trans(1) = (Xlas - Xoff) / (Xmax - Xmin)
Trans(3) = (Yoff - Ylas) / (Ymax - Ymin)
Trans(2) = Xoff - Trans(1) * Xmin
Trans(4) = Ylas - Trans(3) * Ymin
Trans(0) = 0

PCOPY 0, 1
IF UCASE$(G$) = "K" THEN
    CALL CLEAN
    LOCATE 1, 6: PRINT USING "Poligon NUMBER [of #####]"; NPOL;: INPUT FOUND
    IF FOUND = 0 THEN
        XYout(1) = 0.0: XYout(2) = 0.0
        XYout(3) = 0.0: XYout(4) = 0.0
        GOTO REPPINFO
    END IF
    IF FOUND <= NPOL THEN
        GOTO BPinfo
    ELSE
        GOTO REPPINFO
    END IF
END IF
IF G$ = CHR$(27) THEN
    VIEW (SLMIN, 0)-(XSPMODE - 1, YSPMODE - 1): CLS
    CALL CLEAN
    EXIT SUB
END IF
DOPSEARCH:
IF XYout(1) = XYout(2) AND XYout(3) = XYout(4) THEN
    Xo = XYout(1): Yo = XYout(3)
    CALL SearchPoly(Xo, Yo, FOUND)
    IF FOUND = 0 GOTO REPPINFO
    BPinfo:
    IF INIT <> 0 THEN
        CALL PLOTpoly(XY(), 13)
    END IF
    CALL GETPOLYall(FOUND, XY(), IC(), IN(), Xc, Yc, Area, Xmi, Xma, Ymi, Yma)
    CALL PLOTpoly(XY(), 14)
    INIT = FOUND
    SLMIN = XYscr(2) + 2: CMIN = INT(SLMIN / 8) + 2: CMAX = CMIN + 27
    '
    VIEW (SLMIN, 0)-(XSPMODE - 1, YSPMODE - 1): CLS
    VIEW (XYscr(1), XYscr(3))-(XYscr(2), XYscr(4))
    '
    KB = XY(0)
    S$ = ""
    KR% = 2
    LOCATE KR%, CMIN: PRINT "Particles of polygon" + STR$(FOUND);
    FOR K = 1 TO KB
        IF LEN(S$) + LEN(STR$(IC(K))) < SP% THEN
            S$ = S$ + STR$(IC(K))
        ELSE
            KR% = KR% + 1
            LOCATE KR%, CMIN: PRINT S$;
            S$ = ""
            S$ = S$ + STR$(IC(K))
        END IF
    NEXT
    KR% = KR% + 1
    LOCATE KR%, CMIN: PRINT S$;
    KR% = KR% + 1
    LOCATE KR%, CMIN: PRINT "List of neighbour poligons";
    KB = IN(0)
    S$ = ""
    FOR K = 1 TO KB
        IF LEN(S$) + LEN(STR$(IN(K))) < SP% THEN
            S$ = S$ + STR$(IN(K))
        ELSE
            KR% = KR% + 1
            LOCATE KR%, CMIN: PRINT S$;
            S$ = ""
            S$ = S$ + STR$(IN(K))
        END IF
    NEXT
    KR% = KR% + 1
    LOCATE KR%, CMIN: PRINT S$;
    '
    IF CMAX < XDMODE THEN
        CMIN = CMIN + INT(0.5 * (XDMODE - CMAX))
    END IF
    WINDOW: VIEW
    FOR I% = 0 TO 10
        XYlimL(I%) = 0: XYscrL(I%) = 0: XYpltL(I%) = 0: ContrL(I%) = 0
        LablL$(I%) = ""
    NEXT
    WID = Xma - Xmi: IF WID > (Yma - Ymi) THEN WID = Yma - Ymi
    XC1 = 0.5 * (Xma + Xmi): YC1 = 0.5 * (Yma + Ymi)
    XYlimL(1) = XC1 - 0.5 * WID: XYlimL(2) = XC1 + 0.5 * WID
    XYlimL(3) = YC1 - 0.5 * WID: XYlimL(4) = YC1 + 0.5 * WID
    XYlimL(8) = XSPMODE: XYlimL(9) = YSPMODE: XYlimL(10) = SPMODE
    XYscrL(3) = XYscr(2) + 35
    XYscrL(1) = XSPMODE - XYscrL(3) - 10: XYscrL(2) = XYscrL(1): XYscr(5) = 10
    XYscrL(4) = XYscr(4) - XYscrL(2) - 18

    XYscrP(1) = XYscrL(1): XYscrP(2) = XYscrL(2)
    XYscrP(3) = XYscrL(3):
    LablL$(1) = "X": LablL$(2) = "Y"
    ContrL(1) = -1
    CALL WIN(XYlimL(), XYscrL(), XYpltL(), LablL$(), ContrL())
    XYscrP(4) = XYscrL(3) - XYscrP(2) - 48
    FOR K = 1 TO KB
        CALL GETPOLY(IN(K), XYN())
        CALL Cg(XYN(), Xcn, Ycn, Arean)
        FORX(K) = Xcn
        FORY(K) = Ycn
        CALL PLOTpoly(XYN(), 10)
        St$ = MID$(STR$(IN(K)), 2)
        CALL DRAWSTR(Xcn, Ycn, St$, XYlimL(), XYscrL())
    NEXT
    CALL PLOTpoly(XY(), 14)
    CALL Cg(XY(), Xc, Yc, Area)
    FOR K = 1 TO KB
        LINE (Xc, Yc)-(FORX(K), FORY(K)), 1
    NEXT
    LOCATE WDMODE, 1: PRINT "    ";
    XYlimP(8) = XSPMODE: XYlimP(9) = YSPMODE: XYlimP(10) = SPMODE
    CALL WIN(XYlimL(), XYscrP(), XYpltL(), LablL$(), ContrL())
    InfoNuthing:
END IF
FOUND = 0
GOTO REPPINFO
END SUB

SUB BCIRCLE (NBD, Xc, Yc, Rad, RAVER, HCFLAG)
Dx = 0!: Dy = 0!: DT = 0!: Vx = 0!: Vy = 0!: Vt = 0!
Fx = 0!: Fy = 0!: Mt = 0!: IBTYP = 1!
ARC = 6.28 * Rad: NBD = INT(ARC / (4 * RAVER))
pi = 4 * ATN(1!)
DTH = 2 * pi / NBD
TH0 = -.5 * DTH
IAD = M1
FOR i = 1 TO NBD
    THI = TH0 + (i - 1) * DTH
    THF = THI + DTH
    XI = Xc + Rad * COS(THI)
    XF = Xc + Rad * COS(THF)
    YI = Yc + Rad * SIN(THI)
    Yf = Yc + Rad * SIN(THF)
    Xp = .5 * (XI + XF)
    YP = .5 * (YI + Yf)
    ARG1 = Yf - YI
    ARG2 = XF - XI
    ITYPE = -.25 * SQR(ARG1 ^ 2 + ARG2 ^ 2)
    Theta = ATAN2(ARG1, ARG2)
    CALL AAI(IAD, Xp)
    CALL AAI(IAD + 1, YP)
    CALL AAI(IAD + 2, Vx)
    CALL AAI(IAD + 3, Vy)
    CALL AAI(IAD + 4, Vt)
    CALL AAI(IAD + 5, Fx)
    CALL AAI(IAD + 6, Fy)
    CALL AAI(IAD + 7, Mt)
    CALL AAI(IAD + 8, IBTYP)
    CALL AAI(IAD + 9, ITYPE)
    CALL AAI(IAD + 10, Theta)
    CALL AAI(IAD + 11, Dx)
    CALL AAI(IAD + 12, Dy)
    CALL AAI(IAD + 13, DT)
    LINE (XI, YI)-(XF, Yf), 15
    IAD = IAD + NDPART
NEXT
END SUB

FUNCTION BCONT (I, J)
SHARED BCT()
L& = I * (NBPART + 1) + J
BCONT = BCT(L&)
END FUNCTION

SUB BCONTI (I, J, V)
SHARED BCT()
IF I > NBOX THEN
    LOCATE 1, 6: PRINT "Exceeded in BCONTY - 0 "; I
    WHILE INKEY$ = "": WEND
END IF
IF J > NBPART THEN
    LOCATE 1, 6: PRINT "Exceeded in BCONTY - 2 "; J
    WHILE INKEY$ = "": WEND
    STOP
END IF
L& = I * (NBPART + 1) + J
BCT(L&) = V
END SUB

SUB BHOLLOW (NBD, Xc, Yc, Rad, RATIO, RAVER, HCFLAG)
Dx = 0!: Dy = 0!: DT = 0!: Vx = 0!: Vy = 0!: Vt = 0!
Fx = 0!: Fy = 0!: Mt = 0!: IBTYP = 1!
RADE = Rad
RADi = Rad / RATIO
ARC1 = 6.28 * RADE: NBDE = INT(ARC1 / (4 * RAVER))
ARC2 = 6.28 * RADi: NBDI = INT(ARC2 / (4 * RAVER))
NBD = NBDE + NBDI
pi = 4 * ATN(1!)
DTHE = 2 * pi / NBDE
DTHI = 2 * pi / NBDI
TH0 = -.5 * DTHE
DTH = DTHE
Rad = RADE
IAD = M1
FOR i = 1 TO NBD
    IF i = NBDE + 1 THEN TH0 = -.5 * DTHI: DTH = DTHI: Rad = RADi: IBTYP = 2!
    THI = TH0 + (i - 1) * DTH
    THF = THI + DTH
    XI = Xc + Rad * COS(THI)
    XF = Xc + Rad * COS(THF)
    YI = Yc + Rad * SIN(THI)
    Yf = Yc + Rad * SIN(THF)
    Xp = .5 * (XI + XF)
    YP = .5 * (YI + Yf)
    ARG1 = Yf - YI
    ARG2 = XF - XI
    ITYPE = -.25 * SQR(ARG1 ^ 2 + ARG2 ^ 2)
    Theta = ATAN2(ARG1, ARG2)
    CALL AAI(IAD, Xp)
    CALL AAI(IAD + 1, YP)
    CALL AAI(IAD + 2, Vx)
    CALL AAI(IAD + 3, Vy)
    CALL AAI(IAD + 4, Vt)
    CALL AAI(IAD + 5, Fx)
    CALL AAI(IAD + 6, Fy)
    CALL AAI(IAD + 7, Mt)
    CALL AAI(IAD + 8, IBTYP)
    CALL AAI(IAD + 9, ITYPE)
    CALL AAI(IAD + 10, Theta)
    CALL AAI(IAD + 11, Dx)
    CALL AAI(IAD + 12, Dy)
    CALL AAI(IAD + 13, DT)
    LINE (XI, YI)-(XF, Yf), 15
    IAD = IAD + NDPART
NEXT
END SUB

SUB BORDER (XYlim(), XYscr())
WINDOW: VIEW
AXREV = 0
Xoff = XYscr(1)
Xlas = XYscr(2)
Yoff = XYscr(3)
Ylas = XYscr(4)
Xmin = XYlim(1)
Xmax = XYlim(2)
Ymin = XYlim(3)
Ymax = XYlim(4)
LINE (Xoff, Yoff)-(Xlas, Yoff)
LINE (Xoff, Yoff - 1)-(Xlas, Yoff - 1)
LINE (Xoff, Ylas)-(Xlas, Ylas)
LINE (Xoff, Ylas + 1)-(Xlas, Ylas + 1)
LINE (Xoff, Yoff)-(Xoff, Ylas)
LINE (Xoff + 1, Yoff)-(Xoff + 1, Ylas)
LINE (Xlas, Yoff)-(Xlas, Ylas)
LINE (Xlas - 1, Yoff)-(Xlas - 1, Ylas)
VIEW (Xoff, Yoff)-(Xlas, Ylas)
IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
END SUB

SUB BOX (X, Y, R, E, NBSAV(), NBMAP, SPLIT())
DIM XSP(100), YSP(100)
'
'*****DETERMINE BOXES THAT DISC IB1 MAPS INTO OR IS IN PROXIMITY TO
'
NBMAP = 0
DELX = DELBOX
DELY = DELBOX
RT = R * (1 + E) + TOL
XMA = X + RT
XMI = X - RT
YMA = Y + RT
YMI = Y - RT
IF XMA > XMAXA OR XMI < XMINA THEN EXIT SUB
IF YMA > YMAXA OR YMI < YMINA THEN EXIT SUB
'
NXL = INT((XMI - XMINA) / DELX)
NXU = INT((XMA - XMINA) / DELY)
IF XMA >= XMAXA THEN NXU = NXU - 1
DXL = XMI - DELX * NXL
DXU = (NXU + 1) * DELX - XMA
NBX = NXU - NXL + 1
IF NBX = 1 THEN
    XSP(1) = 2 * RT
ELSE
    XSP(1) = DELX - DXL
    XSP(NBX) = DELX - DXU
    FOR i = 2 TO NBX - 1
        XSP(i) = DELX
    NEXT
END IF
NYL = INT((YMI - YMINA) / DELY)
NYU = INT((YMA - YMINA) / DELY)
IF YMA >= YMAXA THEN NYU = NYU - 1
DYL = YMI - DELY * NYL
DYU = (NYU + 1) * DELY - YMA
NBY = NYU - NYL + 1
IF NBY = 1 THEN
    YSP(1) = 2 * RT
ELSE
    YSP(1) = DELY - DYL
    YSP(NBY) = DELY - DYU
    FOR i = 2 TO NBY - 1
        YSP(i) = DELY
    NEXT
END IF
'
FOR NYY = NYL TO NYU
    LNY = YSP(NYY - NYL + 1)
    NA = NYY * NXB
    FOR NXX = NXL TO NXU
        LNX = XSP(NXX - NXL + 1)
        NB = NA + NXX + 1
        IF NBMAP = 0 GOTO BOX1
        FOR N = 1 TO NBMAP
            IF NBSAV(N) = NB GOTO BOX2
        NEXT
        BOX1: NBMAP = NBMAP + 1
        SPLIT(NBMAP) = LNX * LNY
        NBSAV(NBMAP) = NB
    BOX2: NEXT
NEXT
'
ERASE XSP, YSP
END SUB

SUB BRECT (NBD, Xmin, Xmax, Ymin, Ymax, RAVER)
LINE (Xmin, Ymin)-(Xmax, Ymax), 11, B
Dx = 0!: Dy = 0!: DT = 0!: Vx = 0!: Vy = 0!: Vt = 0!
Fx = 0!: Fy = 0!: Mt = 0!: IBTYP = 1!
pi = 4 * ATN(1)
DDI = 4 * RAVER
IAD = M1: IBTYP = 1
NHY = INT((Ymax - Ymin) / DDI)
DDY = (Ymax - Ymin) / NHY
RBAR = -.25 * DDY
NHX = INT((Xmax - Xmin) / DDI)
DDX = (Xmax - Xmin) / NHX
NBD = 0
FOR K = 1 TO 4
    Theta = .5 * pi * K
    CS = COS(Theta)
    SN = SIN(Theta)
    IF K = 1 THEN
        NH = NHY
        DD = DDY
        XIN = Xmax
        YIN = Ymin
    END IF
    IF K = 2 THEN
        NH = NHX
        DD = DDX
        XIN = Xmax
        YIN = Ymax
    END IF
    IF K = 3 THEN
        NH = NHY
        DD = DDY
        XIN = Xmin
        YIN = Ymax
    END IF
    IF K = 4 THEN
        NH = NHX
        DD = DDX
        XIN = Xmin
        YIN = Ymin
    END IF
    FOR i = 1 TO NH
        IF K = 1 THEN
            XIN = Xmax
            YIN = Ymin
            DD = DDY
        END IF
        XI = XIN + (i - 1) * DD * CS
        YI = YIN + (i - 1) * DD * SN
        XF = XI + DD * CS
        Yf = YI + DD * SN
        LINE (XI, YI)-(XF, Yf), 12
        Yc = .5 * (YI + Yf)
        Xc = .5 * (XI + XF)
        ITYPE = -.25 * DD
        'WHILE INKEY$ = "": WEND
        CALL AAI(IAD, Xc)
        CALL AAI(IAD + 1, Yc)
        CALL AAI(IAD + 2, Vx)
        CALL AAI(IAD + 3, Vy)
        CALL AAI(IAD + 4, Vt)
        CALL AAI(IAD + 5, Fx)
        CALL AAI(IAD + 6, Fy)
        CALL AAI(IAD + 7, Mt)
        CALL AAI(IAD + 8, IBTYP)
        CALL AAI(IAD + 9, ITYPE)
        CALL AAI(IAD + 10, Theta)
        CALL AAI(IAD + 11, Dx)
        CALL AAI(IAD + 12, Dy)
        CALL AAI(IAD + 13, DT)
        IAD = IAD + NDPART
        NBD = NBD + 1
    NEXT
NEXT
END SUB

SUB Cg (XY(), Xg, Yg, Areax) STATIC
DIM Dx(20000), Dy(20000), DP(20000), Rxx(20000), Ryy(20000), Rx(20000), Ry(20000)
DTR = 3.1415926# / 180
K = ABS(XY(0))
IF K <= 2 THEN
    IF K = 1 THEN
        Xg = XY(1)
        Yg = XY(2): Area = 0
        EXIT SUB
    END IF
    IF K = 2 THEN
        Xg = 0.5 * (XY(1) + XY(3))
        Yg = 0.5 * (XY(2) + XY(4))
        Area = 0
        EXIT SUB
    END IF
    IF K = 3 THEN
        Xg = (XY(1) + XY(3) + XY(5)) / 3
        Yg = (XY(2) + XY(4) + XY(6)) / 3
        Area = 0
        EXIT SUB
    END IF
END IF
FOR I% = 1 TO K
    N1% = I%: N2% = I% + 1: IF N2% > K THEN N2% = 1
    Dx(I%) = XY(2 * N2% - 1) - XY(2 * N1% - 1)
    Dy(I%) = XY(2 * N2%) - XY(2 * N1%)
    Rxx(I%) = (XY(2 * N1% - 1) ^ 2 + XY(2 * N1% - 1) * XY(2 * N2% - 1) + XY(2 * N2% - 1) ^ 2) / 6
    Ryy(I%) = (XY(2 * N1%) ^ 2 + XY(2 * N1%) * XY(2 * N2%) + XY(2 * N2%) ^ 2) / 6
    Rx(I%) = .5 * (XY(2 * N1% - 1) + XY(2 * N2% - 1))
    Ry(I%) = .5 * (XY(2 * N1%) + XY(2 * N2%))
NEXT
Areax = 0: Areay = 0: Xg = 0: Yg = 0
FOR I% = 1 TO K
    Areax = Areax + Rx(I%) * Dy(I%): Xg = Xg + Rxx(I%) * Dy(I%)
    Areay = Areay + Ry(I%) * Dx(I%): Yg = Yg + Ryy(I%) * Dx(I%)
NEXT
Xg = Xg / Areax: Yg = Yg / Areay
END SUB

SUB CheckContacts (N2, X2, Y2, THETA2, ITYP2, RBAR2, ECC2, IFLAG)
DIM NBSAV(200), XYP(300), XYCN(4, 2)
'
CALL BOX(X2, Y2, RBAR2, ECC2, NBSAV(), NBMAP, SPLIT())
'
STIF = 1
IFLAG = 0
FOR i = 1 TO NBMAP
    NB = NBSAV(i)
    NP = BCONT(NB, 0)
    FOR J = 1 TO NP
        N1 = BCONT(NB, J)
        IF N1 <> N2 THEN
            CALL GETDISC(N1, X1, Y1, THETA1, ITYP1, IBTYP1)
            IF ITYP1 > 0 THEN
                RBAR1 = SHAPES(ITYP1, 1)
                ECC1 = SHAPES(ITYP1, 2)
                IF IBTYP <> 0 THEN ECC1 = 0
            ELSE
                Rad = -ITYP1
                Ecc = 1
            END IF
            DX1 = 0: DY1 = 0: DT1 = 0: xFNA = 0
            DX2 = 0: DY2 = 0: DT2 = 0: xFNA = 0
         CALL FORCES(X1, Y1, RBAR1, ECC1, THETA1, X2, Y2, RBAR2, ECC2, THETA2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, xFNA, FTA, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA _
, BDT, AMU, XYCN())
            IF IFLAG <> 0 THEN
                EXIT SUB
            END IF
        END IF
    NEXT
NEXT
END SUB

SUB CHECKfile (Cfile$, ERFILE)
SHARED NOBFILE
ON ERROR GOTO NOFILE
OPEN Cfile$ FOR INPUT AS #9
CLOSE #9
ON ERROR GOTO 0
ERFILE = NOBFILE
END SUB

SUB CHECKinside (XY(), Xo, Yo, INSIDE)
INSIDE = 0
N = XY(0)
KX = 1
KY = 2
CNT = 0
N2 = 2 * N
XI = XY(KX)
YI = XY(KY)
FOR i = 1 TO N
    KX = KX + 2
    IF KX > N2 THEN KX = 1
    KY = KY + 2
    IF KY > N2 THEN KY = 2
    XF = XY(KX)
    Yf = XY(KY)
    Dx = XF - XI
    Dy = Yf - YI
    DD = SQR(Dx * Dx + Dy * Dy)
    CS = Dy / DD
    SN = -Dx / DD
    PP = XI * CS + YI * SN
    DS1 = Xo * CS + Yo * SN - PP
    IF DS1 < 0 THEN CNT = CNT + 1
    XI = XF
    YI = Yf
NEXT
IF CNT = N THEN INSIDE = 1
END SUB

SUB CHECKTYPE (XYP(), XYC(), IDP(), IDC(), NV, XYins(), ISEQ, IPO())
NP = XYP(0)
'
'     Check if there is a requested type in polygon XYP
'
FOR i = 1 TO NP
    IF IDP(i) = 1 THEN
        XYins(1) = XYP(2 * i - 1)
        XYins(2) = XYP(2 * i)
        XYins(0) = NV
        IDP(i) = -NV
        ISEQ = 1
        IPO(1) = 1
        GOTO 150
    END IF
NEXT
'
'     Check if there is a requested time in polygon XYC
'
NC = XYC(0)
FOR i = 1 TO NC
    IF IDC(i) = NV THEN
        XYins(1) = XYC(2 * i - 1)
        XYins(2) = XYC(2 * i)
        XYins(0) = 1
        IDC(i) = -NV
        IPO(1) = -1
        ISEQ = -1
        GOTO 150
    END IF
NEXT
'
'     Start from common point if no polygon has a right type
'
FOR i = 1 TO NP
    IF IDP(i) = 2 THEN
        XL = XYP(2 * i - 1)
        YL = XYP(2 * i)
        XYins(1) = XL
        XYins(2) = YL
        XYins(0) = 1
        IDP(i) = -2
        IPO(1) = 2
        ISEQ = 0
        CALL MARK(XL, YL, XYC(), IDC())
        GOTO 150
    END IF
NEXT
150 '
END SUB

SUB CLEAN
LOCATE 1, 6: PRINT STRING$(74, " ");
END SUB

SUB CleanBoundary
FOR i = 1 TO NDISK
    CALL DLISTi(i, 8, 0)
NEXT
END SUB

FUNCTION CLIST (I, J)
L& = iCLIST& + (I - 1) * NCPART + J
CALL EMSGETf(HA%, L&, V)
CLIST = V
END FUNCTION

SUB CLISTi (I, J, V)
L& = iCLIST& + (I - 1) * NCPART + J
CALL EMSPUTf(HA%, L&, V)
END SUB

FUNCTION CONV (Addr, HistNo)
IF HistNo = 0 THEN
    TMP = RECMAX + (Addr - 1) * 3 + 2
ELSE
    IF Addr = 11 AND HistNo = 1 THEN
        TMP = 11
    ELSE
        TMP = RECHIS + (HistNo - 1) * 51 + 10 + 36 + Addr - 1
    END IF
END IF
CONV = TMP
END FUNCTION

SUB DELAY (G$)
DELAYREP:
G$ = INKEY$
IF G$ = "" GOTO DELAYREP
END SUB

SUB DeleteBall (N)
'
' Delete ball from A array
'
' (A) - do no touch balls with numbers less that N
' (B) - balls with number greater than N are decremented by one NDPART
'
IF N > 0 THEN
    COE = 1
ELSE
    COE = 0
    GOTO ACL
END IF
CALL CLEAN
LOCATE 1, 6: PRINT USING "Eliminating ball ####"; N
FOR i = N + 1 TO NDISK
    FOR J = 0 TO NDPART - 1
        V = DLIST(i, J)
        CALL DLISTi(i - 1, J, V)
    NEXT
NEXT
'
ACL: IBD = M1 + (ABS(N) - 1) * NDPART
'
CALL DISPLAY("Adjusting conatct list")
FOR i = 1 TO NCONT
    IB1 = CLIST(i, 0)
    IB2 = CLIST(i, 1)
    IF IB1 = IBD THEN CALL CLISTi(i, 0, -IB1)
    IF IB2 = IBD THEN CALL CLISTi(i, 1, -IB2)
    IF IB1 > IBD THEN CALL CLISTi(i, 0, IB1 - COE * NDPART)
    IF IB2 > IBD THEN CALL CLISTi(i, 1, IB2 - COE * NDPART)
NEXT
END SUB

SUB DeleteBALLS (XYlim(), XYscr(), XYout(), Contr(), NFILE$)
DIM DBALLS(100)
'
DELB = 0
StartASCII:
LOCATE 1, 6
PRINT USING "1 - Select ballls (##) | 2 - Delete | 3 - Exit"; DELB
CALL DELAY(G$)
IF VAL(G$) = 1 THEN
    IF DELB <> 0 THEN
        CALL PlotBalls(XYlim(), XYscr())
        DELB = 0
    END IF
    CALL MarkBALLS(XYlim(), XYscr(), XYout(), Contr(), DBALLS(), "delition")
    DELB = DBALLS(0)
    GOTO StartASCII
END IF
IF VAL(G$) = 2 THEN
    CALL GetContactList
    CALL GetDiskList
    FOR i = 1 TO DELB
        N = DBALLS(i)
        CALL DeleteBall(N)
        NDISK = NDISK - 1
        FOR J = i + 1 TO DELB
            IF DBALLS(J) > N THEN DBALLS(J) = DBALLS(J) - 1
        NEXT
    NEXT
    CALL RewriteArray(NFILE$)
    EXIT SUB
END IF
IF VAL(G$) = 3 THEN
    NFILE$ = " "
END IF
GOTO StartASCII
END SUB

SUB DISPL (XYlim(), XYscr(), XYplt(), XYout(), Contr())
SHARED Xmaker()
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
'
DMAX = 0
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IAD = M1 + (N - 1) * NDPART
    Dx = AA(IAD + 11)
    Dy = AA(IAD + 12)
    DD = SQR(Dx * Dx + Dy * Dy)
    IF DD > DMAX THEN DMAX = DD
    IF IBTYP = 0 THEN
        EXIT FOR
    END IF
NEXT
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IAD = M1 + (N - 1) * NDPART
    Dx = AA(IAD + 11) * 2 * RAVER / DMAX
    Dy = AA(IAD + 12) * 2 * RAVER / DMAX
    DT = AA(IAD + 13) * 2 * RAVER / DMAX
    XF = Xc + Dx
    Yf = Yc + Dy
    CALL ARROW(Xc, Yc, XF, Yf, 10)
NEXT
WHILE INKEY$ = "": WEND
END SUB

SUB VELS (XYlim(), XYscr(), XYplt(), XYout(), Contr())
SHARED Xmaker()
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
'
VMAX = 0
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IAD = M1 + (N - 1) * NDPART
    Vx = AA(IAD + 2)
    Vy = AA(IAD + 3)
    VV = SQR(Vx * Vx + Vy * Vy)
    IF VV > VMAX THEN VMAX = VV
    IF IBTYP = 0 THEN
        EXIT FOR
    END IF
NEXT
'
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IAD = M1 + (N - 1) * NDPART
    Vx = AA(IAD + 2) * 2 * RAVER / VMAX
    Vy = AA(IAD + 3) * 2 * RAVER / VMAX
    XF = Xc + Vx
    Yf = Yc + Vy
    CALL ARROW(Xc, Yc, XF, Yf, 10)
NEXT
WHILE INKEY$ = "": WEND
END SUB

SUB ROTS (XYlim(), XYscr(), XYplt(), XYout(), Contr())
SHARED Xmaker()
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
'
RMAX = 0
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IAD = M1 + (N - 1) * NDPART
    Ro = AA(IAD + 4)
    IF ABS(Ro) > RMAX THEN RMAX = ABS(Ro)
    IF IBTYP = 0 THEN
        EXIT FOR
    END IF
NEXT
'
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IAD = M1 + (N - 1) * NDPART
    Ro = AA(IAD + 4) * 2 * RAVER / RMAX
    IF Ro < 0 THEN
        CLR = 13
    ELSE
        CLR = 12
    END IF
    CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, CLR)
NEXT
WHILE INKEY$ = "": WEND
END SUB

SUB DISPLAY (Mes$)
CALL CLEAN
LOCATE 1, 6: PRINT Mes$;
END SUB

FUNCTION DLIST (I, J)
L& = iDLIST& + (I - 1) * NDPART + J
CALL EMSGETf(HA%, L&, V)
DLIST = V
END FUNCTION

SUB DLISTi (I, J, V)
L& = iDLIST& + (I - 1) * NDPART + J
CALL EMSPUTf(HA%, L&, V)
END SUB

REM $DYNAMIC
SUB EMSALLOCATE (NDISK, NDPART, NBOX, NBPART, NCONT, NCPART)
'  CALL EMSCLEAR
'
' BCONT
iBCONT& = 1
fBCONT& = iBCONT& - 1 + (NBOX + 1) * (NBPART + 1)
'
' CLIST
iCLIST& = fBCONT& + 1
fCLIST& = iCLIST& - 1 + (NCONT + 1) * (NCPART + 1)

' DLIST
iDLIST& = fCLIST& + 1
fDLIST& = iDLIST& - 1 + (NDISK + 1) * (NDPART + 1)
'
ET% = 2: HA% = 0: EC% = 0
'WHILE INKEY$ = "": WEND
EC% = -1: 'CALL EMSOPENf(fDLIST&, ET%, HA%, EC%)
IF EC% = -1 THEN
    CALL EMSOPENf(fID&, ET%, HA%, EC%)
    HA% = -1
ELSE
    OPEN "c:\dos\ems" FOR OUTPUT AS #18
    WRITE #18, HA%
    CLOSE #18
END IF
'LOCATE 1, 1: PRINT HA%
'WHILE INKEY$ = "": WEND
END SUB

REM $STATIC
'SUB EMSCLEAR
'OPEN "c:\dos\ems" FOR INPUT AS #18
'IF NOT EOF(18) THEN
'    INPUT #18, HA%
'CLOSE #18
'       IF HA% > 0 THEN CALL EMSCLOSEf(HA%)
'    END IF
'END SUB

REM $DYNAMIC
SUB EMSGETf (HA%, L&, V)
SHARED EMSF$
IF HA% > 0 THEN
    CALL EMSGETf(HA%, L&, V)
    EXIT SUB
ELSE
    GET #10, L&
    V = CVS(EMSF$)
END IF
END SUB

SUB EMSOPENf (fSize&, ET%, HA%, EC%)
SHARED EMSF$, EMSFILE%
IF EMSFILE% = 0 THEN
    OPEN "EMS" FOR RANDOM AS #10 LEN = 4
    FIELD #10, 4 AS EMSF$
    EMSFILE% = 1
    EC% = 0
END IF
END SUB

SUB EMSPUTf (HA%, L&, V)
SHARED EMSF$
IF HA% > 0 THEN
    CALL EMSPUTf(HA%, L&, V)
    EXIT SUB
ELSE
    LSET EMSF$ = MKS$(V)
    PUT #10, L&
END IF
END SUB

REM $STATIC
SUB FileINITIALIZE (BinFile$)
'DIM XX(1200), YY(1200), TT(1200), ID(1200)
DIM XYlim(10), XYscr(10), XYplt(10), Contr(10), Labl$(30)
DIM Page$(25), Dat$(25, 2), Dat%(25), Par$(10)
CLS
M1 = 1000: NCPART = 6: NDPART = 14
Page$(1) = "File with particle definitions and coordinates [balls.dat]->bc10000.dat"
Page$(2) = "1-CIRC  2- RECT 3 - HOLLOW [RarDia] 4-h-HOLLOW           -->4,10"
Page$(3) = "Binary file with A-array [NEW_1.BIN]----------------------->CIRC_1.BIN"
Page$(4) = "Generate Half-Circle (Y/N)  ------------------------------->Y"
RPPFileInit:
NRmin = 1: NRmax = 3: NcMax = 80
NcMin = INSTR(1, Page$(1), ">") + 1
CALL GetData(Page$(), Dat$(), Dat%(), NRmin, NRmax, NcMin, NcMax, ERline, Mes$)
HCFLAG = 0
IF UCASE$(Dat$(4, 1)) = "Y" THEN HCFLAG = 1
AsFile$ = "balls.dat"
IF Dat%(1) = 1 THEN
    AsFile$ = Dat$(1, 1)
END IF
BOUNDTYPE = 2
IF Dat%(2) <> 0 THEN
    BOUNDARY = VAL(Dat$(2, 1))
END IF
BinFile$ = "NEW_1.BIN"
IF Dat%(3) = 1 THEN
    BinFile$ = Dat$(3, 1)
END IF
OPEN AsFile$ FOR INPUT AS #3
LINE INPUT #3, T$
CALL PARSER(T$, Par$(), NSET)
IERR = 0
IF Par$(0) = "Number_of_particle_types" THEN
    NTYPE = VAL(Par$(1))
ELSE
    WINDOW: VIEW: CLS
    ERline = 1: Mes$ = "WRONG FILE; Press any key .. "
    GOTO RPPFileInit
    EXIT SUB
END IF
CLOSE #1
CALL INITfile(BinFile$)
Rmin = 1E+37
Rmax = 0
IAD = 50
FOR i = 1 TO NTYPE
    INPUT #3, NEDGE
    SHAPES(i, 0) = NEDGE: CALL AAI(IAD, NEDGE)
    IAD = IAD + 1
    FOR K = 1 TO NEDGE
        INPUT #3, XED, YED
        SHAPES(i, 2 * K - 1) = XED
        CALL AAI(IAD, XED)
        IAD = IAD + 1
        SHAPES(i, 2 * K) = YED
        CALL AAI(IAD, YED)
        IAD = IAD + 1
    NEXT
    IF NEDGE = 1 THEN
        RBAR = SHAPES(i, 1)
        Ecc = SHAPES(i, 2)
        R(i) = RBAR
        E(i) = Ecc
    END IF
    IF RBAR > Rmax THEN Rmax = RBAR
    IF RBAR < Rmin THEN Rmin = RBAR
NEXT
RAVER = .5 * (Rmin + Rmax)
LINE INPUT #3, T$
CALL PARSER(T$, Par$(), NSET)
Xmin = VAL(Par$(1)): Xmax = VAL(Par$(3))
LINE INPUT #3, T$
CALL PARSER(T$, Par$(), NSET)
Ymin = VAL(Par$(1)): Ymax = VAL(Par$(3))
LINE INPUT #3, T$
CALL PARSER(T$, Par$(), NSET)
IF Par$(0) <> "Number_of_particles" THEN STOP
NDISK = VAL(Par$(1))
IF BOUNDARY = 1 OR BOUNDARY = 3 THEN
    XCn = .5 * (Xmin + Xmax)
    YCn = .5 * (Ymin + Ymax)
    Dx = .5 * (Xmax - Xmin)
    Dy = .5 * (Ymax - Ymin)
    Rad = SQR(Dx * Dx + Dy * Dy)
    Rad = Dx
    IF Rad < Dy THEN Rad = Dy
    '  Xmin = XCn - RAD: Xmax = XCn + RAD
    '  Ymin = YCn - RAD: Ymax = YCn + RAD
END IF
WINDOW: VIEW: CLS
XYlim(1) = Xmin: XYlim(2) = Xmax
XYlim(3) = Ymin: XYlim(4) = Ymax
Labl$(1) = "X": Labl$(2) = "Y"
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
IF BOUNDARY = 2 THEN
    CALL BRECT(NBD, Xmin, Xmax, Ymin, Ymax, RAVER)
END IF
IF BOUNDARY = 1 THEN
    CALL BCIRCLE(NBD, XCn, YCn, Rad, RAVER, HCFLAG)
END IF
IF BOUNDARY = 3 THEN
    RATIO = VAL(Dat$(2, 2))
    RADi = Rad / RATIO
    CALL BHOLLOW(NBD, XCn, YCn, Rad, RATIO, RAVER, HCFLAG)
END IF
IF BOUNDARY = 4 THEN
    CALL HHOLLOW(NBD, XCn, YCn, Rad, RATIO, RAVER, HCFLAG)
END IF
'
FILEinitializeREP:
'
IAD = M1 + NBD * NDPART
NDISC = NBD
Dx = 0!: Dy = 0!: DT = 0!: Vx = 0!: Vy = 0!: Vt = 0!
Fx = 0!: Fy = 0!: Mt = 0!: IBTYP = 0!
FOR i = 1 TO NDISK
    LOCATE 1, 6: PRINT USING "Initializing particle ##### out of #####"; i + NBD; NDISK + NBD
    INPUT #3, Xc, Yc, Tc, ITYPE
    IF BOUNDARY = 3 THEN
        DDX = Xc - XCn
        DDY = Yc - YCn
        RADP = SQR(DDX * DDX + DDY * DDY)
        IF RADP < (RADi + Rmax) THEN GOTO SCPP
    END IF
    CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, 10)
    CALL AAI(IAD, Xc)
    CALL AAI(IAD + 1, Yc)
    CALL AAI(IAD + 2, Vx)
    CALL AAI(IAD + 3, Vy)
    CALL AAI(IAD + 4, Vt)
    CALL AAI(IAD + 5, Fx)
    CALL AAI(IAD + 6, Fy)
    CALL AAI(IAD + 7, Mt)
    CALL AAI(IAD + 8, IBTYP)
    CALL AAI(IAD + 9, ITYPE)
    CALL AAI(IAD + 10, Tc)
    CALL AAI(IAD + 11, Dx)
    CALL AAI(IAD + 12, Dy)
    CALL AAI(IAD + 13, DT)
    NDISC = NDISC + 1
    IAD = IAD + NDPART
    SCPP:
NEXT
VL = 0!
FOR J = 0 TO NCPART
    CALL AAI(IAD + J, VL)
NEXT
IAD = IAD + NCPART
CALL AAI(IAD, VL)
'
B = (Xmax - Xmin)
H = (Ymax - Ymin)
NBOX = B * H / (4 * Rmax * Rmax) + .5
NXB = CINT(SQR(NBOX * B / H) + .5)
NYB = CINT(SQR(NBOX * H / B) + .5)
DELBOX = B / NXB
NBOX = NXB * NYB
LOCATE 1, 1: PRINT B, H, NBOX, DELBOX
WHILE INKEY$ = "": WEND
MODE = 2
XLAMBDA = 0
AMU = 0
STIF = 3.5E+10
Ecc = 0
TOL = Rmax * 1.25
NN = 0
DENSITY = 2000
'
CALL AAI(1, M1)
CALL AAI(2, NDISC)
CALL AAI(3, H)
CALL AAI(4, NBOX)
CALL AAI(5, TOL)
CALL AAI(6, MODE)
CALL AAI(7, NTYPE)
CALL AAI(8, STIF)
CALL AAI(9, XLAMBDA)
CALL AAI(10, AMU)
CALL AAI(11, Ecc)
CALL AAI(12, DENSITY)
CALL AAI(13, NN)
CALL AAI(14, DELBOX)
CALL AAI(15, NXB)
CALL AAI(16, NYB)
CALL AAI(17, NDPART)
CALL AAI(18, NCPART)
CALL AAI(19, TOL)
CALL AAI(21, Xmin)
CALL AAI(22, Xmax)
CALL AAI(23, Ymin)
CALL AAI(24, Ymax)
CALL AAI(20, IAD)
CALL AAI(0, IAD)
CALL AAI(26, IAD)
CLOSE #1
LOCATE 1, 6: PRINT "                                              "
LOCATE 1, 6: PRINT "Initialization complete : Press any key ..."
WHILE INKEY$ = "": WEND
LOCATE 1, 6: PRINT "                                              "
END SUB

SUB Fill (XY(), Npoly, XYlim(), XYscr(), XYplt(), Contr(), Separ, Alpha, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT, Imaker%) STATIC
DIM X(100), Y(100), Xp(100), YP(100), XX(100), II(10)
Flag% = 0: N = Npoly: IF N < 0 THEN KEEPF% = 1: N = -N ELSE KEEPF% = 0
IF N = 0 THEN LOCATE 55, 1: PRINT "Fill: No data : WHILE INKEY$="; " : wend : goto 1490"
pi = 3.1415926#: DTR = pi / 180: Plx = 100 / ABS(XYlim(2) - XYlim(1)): Ply = 100 / ABS(XYlim(4) - XYlim(3))
IF N > 100 THEN LOCATE 55, 1: PRINT "Fill: too many points": WHILE INKEY$ = "": WEND: GOTO 1490
IF Separ = 0 THEN Separ = .03
Alpa = Alpha: AAA = DTR * Alpa
P% = Contr(3): CLR = XYscr(6): IF CLR = 0 THEN CLR = 15
Lscrx = 25.4: Lscry = Lscrx * 3 / 4
Plxscr = 640: Plyscr = 350: IF XYlim(10) = 1 THEN Plyscr = 480
Pxcmscr = Plxscr / Lscrx: Pycmscr = Plyscr / Lscry
IF P% = 0 GOTO Sc
Puxcm = (XYlim(2) - XYlim(1)) / XYplt(1)
Puycm = (XYlim(4) - XYlim(3)) / XYplt(2)
GOTO Par
Sc: Puxcm = (XYlim(2) - XYlim(1)) * Pxcmscr / (XYscr(2) - XYscr(1))
Puycm = (XYlim(4) - XYlim(3)) * Pycmscr / (XYscr(4) - XYscr(3))
Par: X1 = COS(AAA): X2 = SIN(AAA)
IF ABS(X1) < .0001 THEN X1 = .0001
TNT = X2 / X1
Talp = TNT * Puycm / Puxcm: Aalp = ATN(Talp) / DTR
DD = (Puxcm * COS(AAA)) ^ 2 + (Puycm * SIN(AAA)) ^ 2
TT = SQR(DD): DEL = Separ * Puxcm * Puycm / TT
FOR i = 1 TO N
    N1 = 2 * i - 1: N2 = N1 + 1
    X(i) = XY(N1): Y(i) = XY(N2)
NEXT i
Xc = 0: Yc = 0: IF KEEPF% = 1 GOTO 1103
IF P% <> 0 THEN
    IF Imaker% = 0 THEN
        PRINT #3, "newpath"
    ELSE
        PRINT #3, " <Polygon<GroupID    1>"
        PRINT #3, " <Fill 0>"
    END IF
END IF
FOR i = 1 TO N
    K1 = i: K2 = K1 + 1: IF K2 > N THEN K2 = 1
    Xc = Xc + X(i): Yc = Yc + Y(i)
    1090 LINE (X(K1), Y(K1))-(X(K2), Y(K2)), CLR
    1092 IF P% = 0 GOTO 1109
    IF Imaker% = 0 THEN
        IF i = 1 THEN CALL MOVETO(X(K1), Y(K1), RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        CALL LINETO(X(K2), Y(K2), RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
    ELSE
        IF i = 1 THEN CALL MPNT(X(K1), Y(K1), RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        IF i <> N THEN CALL MPNT(X(K2), Y(K2), RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
    END IF
1109 NEXT i
IF P% <> 0 THEN
    fc = 0!
    IF Contr(10) <> 0 THEN
        IF Imaker% = 0 THEN
            PRINT #3, USING "##.## setgray fill"; fc
            PRINT #3, "stroke"
        ELSE
            PRINT #3, ">"
        END IF
        PAINT (Xc / 4, Yc / 4), 15
        GOTO NXTFL
    ELSE
        IF Imaker% = 0 THEN
            PRINT #3, "stroke"
        ELSE
            PRINT #3, ">"
        END IF
    END IF
END IF
1103 Xc = Xc / N: Yc = Yc / N
1105 IF P% = 0 AND Separ <= .03 THEN PAINT (Xc, Yc), CLR, CLR: Flag% = 0: GOTO 1490
1111 AL = (90 - Aalp) * DTR: AL1 = -AL: CS = COS(AL1): SN = SIN(AL1)
1140 FOR i = 1 TO N
    1150 Xp(i) = X(i) * CS + Y(i) * SN
    YP(i) = -X(i) * SN + Y(i) * CS
1170 NEXT i
1245 FOR i = 1 TO N: XX(i) = Xp(i): NEXT i
1250 'SORT
FOR i = 1 TO N
    KMAX = N - i
    1280 FOR K = 1 TO KMAX
        IF XX(K) > XX(K + 1) THEN SWAP XX(K), XX(K + 1)
    NEXT K
NEXT i
1315 Xran = XX(N) - XX(1): XPL = XX(1): NSH = CINT(Xran / DEL): Ks% = 0
1316 IF NSH <> 0 THEN XDEL = Xran / NSH ELSE GOTO 1490
1317 IF KEEPF% = 1 THEN XDEL = DEL
1320 FOR i = 1 TO N
    1330 XI = XX(i): XF = XX(i + 1)
    1340 IF (XF - XI) < XDEL GOTO 1481
    1345 KK = 0
    1350 FOR J = 1 TO N
        1360 N1 = J: N2 = J + 1: IF J = N THEN N2 = 1
        1370 XTI = Xp(N1): XTF = Xp(N2)
        1380 IF XTI > XTF THEN SWAP XTI, XTF
        IF XI >= XTI AND XF <= XTF THEN KK = KK + 1: II(KK) = J
    NEXT J
    IF KK = 2 GOTO 1411 ELSE GOTO 1481
    1411 N1 = II(1): N2 = II(1) + 1: IF N2 > N THEN N2 = 1
    1412 XL1 = Xp(N1): YL1 = YP(N1): XL2 = Xp(N2): YL2 = YP(N2)
    1413 IF XL1 > XL2 THEN SWAP XL1, XL2: SWAP YL1, YL2
    1414 SLL = (YL2 - YL1) / (XL2 - XL1)
    1415 N1 = II(2): N2 = II(2) + 1: IF N2 > N THEN N2 = 1
    1416 XH1 = Xp(N1): YH1 = YP(N1): XH2 = Xp(N2): YH2 = YP(N2)
    1417 IF XH1 > XH2 THEN SWAP XH1, XH2: SWAP YH1, YH2
    1418 SLH = (YH2 - YH1) / (XH2 - XH1)
    1419 IF YL1 < YH1 AND YL2 < YH2 GOTO 1431
    SWAP XL1, XH1: SWAP XL2, XH2: SWAP YL1, YH1: SWAP YL2, YH2: SWAP SLL, SLH
    1431 XPL = XPL + XDEL
    1440 IF XPL < XI OR XPL > XF THEN XPL = XPL - XDEL: GOTO 1481
    X1 = XPL: Y1 = YL1 + SLL * (XPL - XL1): X2 = XPL: Y2 = YH1 + SLH * (XPL - XH1)
    XP1 = X1 * CS - Y1 * SN: YP1 = X1 * SN + Y1 * CS
    XP2 = X2 * CS - Y2 * SN: YP2 = X2 * SN + Y2 * CS
    IF P% <> 1 THEN LINE (XP1, YP1)-(XP2, YP2), CLR
    1461 IF P% = 0 GOTO 1470
    1462 'PLOTTER
    Ks% = Ks% + 1
    IF (Ks% MOD 2) = 0 THEN SWAP XP1, XP2: SWAP YP1, YP2
    IF Imaker% = 0 THEN
        CALL MOVETO(XP1, YP1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        CALL LINETO(XP2, YP2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        PRINT #3, "stroke"
    ELSE
        XIM = XP1 * RCOEFX + RXSHIFT
        YIM = YP1 * RCOEFY + RYSHIFT
        XFM = XP2 * RCOEFX + RXSHIFT
        YFM = YP2 * RCOEFY + RYSHIFT
        CALL MLINE(XIM, YIM, XFM, YFM, 1, 0)
    END IF
    1463 'END PLOTTER
    1470 GOTO 1431
1481 NEXT i
1490 IF Flag% = 1 THEN Alpha = 180 - Alpha: Flag% = 0: GOTO 1111
NXTFL: ERASE X, Y, Xp, YP, XX, II
END SUB

SUB FillBoxes (XYlim(), XYscr(), RDIST(), RDISTI(), EDIST(), TDIST(), TDISTI())
DIM NBSAV(400), SPLIT(400)
DTH = 2 * 3.1415926# / 36
CLR = 9: NBD = 0
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
FOR i = 1 TO NBOX
    CALL BCONTI(i, 0, 0)
NEXT
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IF ITYPE < 0 THEN
        RR = -ITYPE
        Ecc = 1
    ELSE
        Ecc = E(ITYPE)
        RR = R(ITYPE)
    END IF
    XI = Xc - 2 * RR * COS(Tc)
    YI = Yc - 2 * RR * SIN(Tc)
    XF = Xc + 2 * RR * COS(Tc)
    Yf = Yc + 2 * RR * SIN(Tc)
    IF Xc > Xmin AND Xc < Xmax AND Yc > Ymin AND Yc < Ymax THEN
        IF IBTYP <> 0 THEN
            NBD = NBD + 1
            '      CLR = 13
            '  ELSE
            '     CLR = 10
        END IF
        'CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, CLR)
    END IF
    CALL BOX(Xc, Yc, RR, Ecc, NBSAV(), NBMAP, SPLIT())
    FOR K = 1 TO NBMAP
        NB = NBSAV(K)
        NP = BCONT(NB, 0) + 1
        CALL BCONTI(NB, 0, NP)
        CALL BCONTI(NB, NP, N)
    NEXT
NEXT
'CALL BORDER(XYlim(), XYscr())
ERASE NBSAV, SPLIT
END SUB

SUB FindContact (IB1F, IB2F, DSN, DST, FRN, FRT)
CALL DISPLAY("Searchnig for contact...")
DSN = 0: DST = 0: FRN = 0: FRT = 0
IAD = M1 + NDISK * NDPART
NC = 0
FINDrep: NC = NC + 1
IB1 = AA(IAD)
IF IB1 = 0 GOTO FINfind
IB2 = AA(IAD + 1)
IF IB1 = IB1F AND IB2 = IB2F THEN
    LOCATE 6, 31: PRINT USING "Found contact No #####"; NC
    DSN = AA(IAD + 2)
    DST = AA(IAD + 3)
    FRN = AA(IAD + 4):
    FRT = AA(IAD + 5)
    EXIT SUB
END IF
IAD = IAD + NCPART
GOTO FINDrep:
FINfind:
CALL DISPLAY("----Contact not found----")
END SUB

SUB FINDINS (XYP(), XYC(), IDP(), IDC(), XYins(), IPO())
NV = 1
'
' Check if there is a requested time in polygon XYP
'
CALL CHECKTYPE(XYP(), XYC(), IDP(), IDC(), NV, XYins(), ISEQ, IPO())
'

L = 1
Lp = 0
FINDinsREP:
IF L <> Lp THEN
    Lp = L
    IF ISEQ > 0 THEN
        CALL ADDPOINT(XYP(), XYC(), IDP(), IDC(), NV, XYins(), 1, IPO())
    ELSE
        CALL ADDPOINT(XYC(), XYP(), IDC(), IDP(), NV, XYins(), -1, IPO())
    END IF
    L = XYins(0)
    KNF = 0
    IF L <> Lp THEN
        Lp = L
        IF ISEQ > 0 THEN
            CALL ADDPOINT(XYC(), XYP(), IDC(), IDP(), NV, XYins(), -1, IPO())
        ELSE
            CALL ADDPOINT(XYP(), XYC(), IDP(), IDC(), NV, XYins(), 1, IPO())
        END IF
        L = XYins(0)
        GOTO FINDinsREP
    END IF
END IF
END SUB

SUB FindTwoBalls (XYlim(), XYscr(), XYout(), Contr())
SHARED WDMODE, XDMODE
DIM XYP(30), XYCN(4, 2)
CALL CLEAN
LOCATE 1, 6: INPUT "Enter two particle numbers "; NB1, NB2
CALL GETDISC(NB1, XC1, YC1, Tc1, ITYP1, IBTYP1)
CALL PLOTELS(XC1, YC1, Tc1, ITYP1, IBTYP1, 14)
IF ITYP1 < 0 THEN
    RBAR1 = -ITYP1
    ECC1 = 1
ELSE
    RBAR1 = R(ITYP1)
    ECC1 = E(ITYP1)
END IF
IB1 = M1 + (NB1 - 1) * NDPART
CALL GETDISC(NB2, XC2, YC2, Tc2, ITYP2, IBTYP2)
IF ITYP2 < 0 THEN
    RBAR2 = -ITYP2
    ECC2 = 1
ELSE
    RBAR2 = R(ITYP2)
    ECC2 = E(ITYP2)
END IF
CALL PLOTELS(XC2, YC2, Tc2, ITYP2, IBTYP2, 13)
IB2 = M1 + (NB2 - 1) * NDPART
    CALL FORCES(XC1, YC1, RBAR1, ECC1, Tc1, XC2, YC2, RBAR2, ECC2, Tc2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, FRN, FRT, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA, BDT, AMU, XYCN( _
))
PN1 = XYCN(4, 1): PN2 = XYCN(4, 2)
LINE (Xi1, Yi1)-(Xi2, Yi2), 11
LOCATE WDMODE - 4, XDMODE - 20: PRINT USING "Penetr = #.##^^^^"; (PN1 + PN2);
CALL FindContact(IB1, IB2, DSN, DST, FRN, FRT)
LOCATE WDMODE - 3, XDMODE - 20: PRINT USING "Record = #.##^^^^"; DSN;
LOCATE WDMODE - 2, XDMODE - 20: PRINT USING "Distan = #.##^^^^"; SQR((XC1 - XC2) ^ 2 + (YC1 - YC2) ^ 2)
WHILE INKEY$ = "": WEND
CALL CLEAN
LOCATE WDMODE - 4, XDMODE - 20: PRINT STRING$(20, " ")
LOCATE WDMODE - 3, XDMODE - 20: PRINT STRING$(20, " ")
LOCATE WDMODE - 2, XDMODE - 20: PRINT STRING$(20, " ")
END SUB

SUB FORCES (XC1, YC1, RBAR1, ECC1, THETA1, XC2, YC2, RBAR2, ECC2, THETA2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, FRN, FRT, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA, BDT, AMU,  _
XYCN())
SHARED RAVER, DAVER, ISFLAG
'
'      XC1,YC1 - COORDINATE OF PARTICLE 1
'      RBAR1   - AVERAGE RADIUS OF PARTICLE 1
'      ECC1    - ECCENTRICITY OF PARTICLE 1
'      THETA1  - ORIENTATION OF MAJOR AXIS OF PARTICLE 1
'      CVX1,CVY1 - CONTACT VECTOR OF PARTICLE 1
'      CNX1,CNY1 - UNIT NORMAL VECTOR OF CONTACT (EXTERIOR NORMAL OF
'                  PARTICLE 1
'      CTX1,CTY1 - UNIT TANGENTIAL VECTOR OF CONTACT
'                  (COUNTER-CLOCK WISE FROM NORMAL VECTOR TO PARTICLE 1)
'      FRN       - CURRENT NORMAL FORCE (BEFORE INCREMENTAL UPDATE)
'      FRT       - CURRENT TANGENTIAL FORCE (BEFORE UPDATE)
'      DN        - INREMENT OF NORMAL COMPLIANCE
'      DT        - INCREMENT OF TANGENTIAL COMPLIANCE
'      XYCN(1,1),XYCN(1,2) - "POINT OF CONTACT"
'      XYCN(2,1),XYCN(2,2) - COORDINATES OF THE FIRST INTERSECTION POINT
'      XYCN(3,1),XYCN(3,2) - COORDINATES OF THE SEWCOND INTERSECTION POINT
'      XYCN(4,1),XYCN(4,2) - PENETRATIONS
'
'                      - CONTACT VECTORS POINT TO THE CONTACT POINT
'      FXD1,FYD1 - INCREMENT OF CARTESIAN COMPONENTS OF DUMPED CONTACT FORCE
'      DM1       - INCREMENT OF MOMENT
'      BDT       - DUMPING COEFFICIENT Fdumped=Fnot dumped*(1+BDT)
'      AMU       - FRICTION COEFFICIENT
'      XLAMBDA   - RATIO OF TANGENTIAL TO NORMAL STIFFNESS
'      STIF      - NORMAL CONTACT STIFFNESS (df=STIF/(R1+R2)*dn)
'
'
'      FOR INCREMENTS IN PARTICLE POSITIONS (DX1,DX2) AND (DY1,DY2)
'      AND ACCUMULATED NORMAL (FNA) AND TANGENTIAL (FRT) FORCES
'      THE SUBROUTINE FORCES CALCULATES INCREMENTS OF NORMAL AND
'      TANGENTIAL FORCES (DFN,DFT) AND DUMPED CARTESIAN COMPONENTS
'      OF TOTAL CONTACT FORCES (FXD1,FYD1) AND (FXD2,FYD2)
'      SLIP IS CHECKED ACCORDING TO FRICTION COEFFICIENT
'      AMU - DUMPING COEFFICIENT BDT (0 - NO DUMPING)
'
'     IFLAG = NUMBER OF ROOTS (1,2-NORMAL,3,4)
'     IFLAG = -1 (ONE ELLIPSE INSIDE THE OTHER)
'     IFLAG = 0  (NO ROOTS)
'
'     INSIDE = 1 TO CHECK IF ONE ELLIPSE INSIDE THE OTHER
'                IN THIS CASE NO FORCE CALCULATIONS
'
'     WHEN DX1,DY1,DT1,FNA,DX2,DY2,DT2,FN2 ASSIGNED 0
'     INCREMENTS ARE RECONSTRUCTED BASED ON OVERLAP ASSUMING THAT
'     MOTION OF A PARTICLE HAS NO ROTATION AND IS IN THE DIRECTION
'     OF THE CONTACT VECTOR
'
'     IMPLICIT REAL*8 (A-H,O-Z)
DIM RT#(4), RXY#(4, 2), RXYT#(4, 2), TMPD#(4), TMPY#(4)
'     REAL*4 XC1,YC1,RBAR1,ECC1,THETA1,XC2,YC2,RBAR2,ECC2,THETA2
'     REAL*4 CVX1,CVY1,CVX2,CVY2,CNX1,CNY1,CTX1,CTY1
'     REAL*4 DX1,DY1,DT1,DX2,DY2,DT2
'     REAL*4 FNA,FRT,DFN,DFT,FXD1,FYD1,FXD2,FYD2
'     REAL*4 DM1,DM2,DN,DT,XYCN(4,2),STIF,XLAMBDA,BDT,AMU,GAP
'     REAL*4 CMP1,CMP2,RAVER,DAVER
'
DIM XYCN#(4, 2), ERT#(4)
NOC% = 0
IF FRN <> 0 THEN
    NOC% = 1
    '   FRN = 0
END IF
XC1RR = XC1
YC1RR = YC1
RBAR1RR = RBAR1
ECC1RR = ECC1
THETA1RR = THETA1
XC2RR = XC2
YC2RR = YC2
RBAR2RR = RBAR2
ECC2RR = ECC2
THETA2RR = THETA2
CVX1RR = CVX1
CVY1RR = CVY1
CVX2RR = CVX2
CVY2RR = CVY2
CNX1RR = CNX1
CNY1RR = CNY1
CTX1RR = CTX1
CTY1RR = CTY1
DX1RR = DX1
DX2RR = DY1
DT1RR = DT1
DX2RR = DX2
DY2RR = DY2
DT2RR = DT2
FRMRR = FRN
DRTRR = FRT
DFNRR = DFN
DFTRR = DFT
FXD1RR = FXD1
FYD1RR = FYD1
FXD2RR = FXD2
FYD2RR = FYD2
DM1RR = DM1
DM2RR = DM2
DNRR = DN
DTRR = DT
IFLAGRR = IFLAG
SIFTRR = STIF
XLAMBDARR = XLAMBDA
BDTRR = BDT
AMURR = AMU

REPRUF:
KSW = 0
IF ECC2 = 1 THEN
    KSW = 1
    SWAP XC1, XC2
    SWAP YC1, YC2
    SWAP RBAR1, RBAR2
    SWAP ECC1, ECC2
    SWAP THETA1, THETA2
END IF

ISFLAG = 1: DAVER = 1
EPS# = 1E-14
EPSC# = .001
XC1# = XC1
YC1# = YC1
RBAR1# = RBAR1
ECC1# = ECC1
THETA1# = THETA1
XC2# = XC2
YC2# = YC2
RBAR2# = RBAR2
ECC2# = ECC2
THETA2# = THETA2
'
STIF# = STIF
XLAMBDA# = XLAMBDA
BDT# = BDT
AMU# = AMU
DX1# = DX1
DY1# = DY1
DT1# = DT1
DX2# = DX2
DY2# = DY2
DT2# = DT2
FRN# = FRN
FRT# = FRT
DFN# = DFN
DFT# = DFT
FXD1# = FXD1
FYD1# = FYD1
FXD2# = FXD2
FYD2# = FYD2
DM1# = DM1
DM2# = DM2
DN# = DN
DT# = DT
XC2# = XC2# - XC1#
YC2# = YC2# - YC1#
XC1R# = XC1#
YC1R# = YC1#
XC1# = 0#
YC1# = 0#
'
RAVER# = RAVER
DAVER# = DAVER
'
INSIDE = 0
MODE = 1
'
'     MODE = 0 - incremental DN ; MODE = 1 - penetration-based DN
'
pi# = 3.14159265358979#
ONE# = 1#
TWO# = 2#
THREE# = 3#
FOUR# = 4#
EIGHT# = 8#
ZERO# = 0#
HALF# = .5#
QUARTER# = .25#
COEF# = ONE#
'
'*****CHECK GAP USING MAXIMUM DIMENSIONS OF PARTICLES
'
IFLAG = 0
XDIF# = XC2# - XC1#
YDIF# = YC2# - YC1#
D# = SQR(XDIF# * XDIF# + YDIF# * YDIF#)
GA# = D# - RBAR1# * (1# + ECC1#) - RBAR2# * (1# + ECC2#)
IF GA# > ZERO# THEN
    IFLAG = 0
    GOTO 500
END IF
'
'*****SET-UP COEFFICIENTS OF ELLIPSE EQUATION
'
IF ECC1 = 1 THEN
    AL# = THETA1# - pi# / 2
    GOTO 431
END IF
ARG1# = YDIF#: ARG2# = XDIF#
IF ABS(ARG2#) <= 1E-11 THEN ARG2# = 1E-11
RRR# = ARG1# / ARG2#
ATN2# = ATN(RRR#)
IF ARG2# < 0 THEN
    ATN2# = pi# + ATN2#
ELSE
    ATN2# = ATN2#
END IF
IF ATN2# > pi# THEN ATN2# = ATN2# - 2# * pi#
AL# = ATN2#
431 CSA# = COS(AL#)
SNA# = SIN(AL#)
CSA2# = CSA# * CSA#
SNA2# = SNA# * SNA#
CSSN# = CSA# * SNA#
'
SCL1# = HALF# * (RBAR1# + RBAR2#)
SCL2# = SCL1# * SCL1#
SCL3# = SCL2# * SCL1#
SCL4# = SCL3# * SCL1#
IFLAG = 0
EB1# = TWO# * ECC1# / (ONE# + ECC1# * ECC1#)
EB2# = TWO# * ECC2# / (ONE# + ECC2# * ECC2#)
RB1# = RBAR1# * (ONE# - ECC1# * ECC1#) / SQR(ONE# + ECC1# * ECC1#)
RB2# = RBAR2# * (ONE# - ECC2# * ECC2#) / SQR(ONE# + ECC2# * ECC2#)
CS1# = COS(TWO# * THETA1#)
SN1# = SIN(TWO# * THETA1#)
CS2# = COS(TWO# * THETA2#)
SN2# = SIN(TWO# * THETA2#)
AO1# = ONE# - EB1# * CS1#
BO1# = -EB1# * SN1#
CO1# = ONE# + EB1# * CS1#
DO1# = -XC1# * AO1# - YC1# * BO1#
EO1# = -YC1# * CO1# - XC1# * BO1#
FO1# = XC1# * XC1# * AO1# + TWO# * XC1# * YC1# * BO1# + YC1# * YC1# * CO1# - RB1# * RB1#
A1# = AO1# * SNA2# - TWO# * BO1# * CSSN# + CO1# * CSA2#
B1# = (AO1# - CO1#) * CSSN# - BO1# * (CSA2# - SNA2#)
C1# = AO1# * CSA2# + TWO# * BO1# * CSSN# + CO1# * SNA2#
D1# = DO1# * SNA# - EO1# * CSA#
E1# = DO1# * CSA# + EO1# * SNA#
F1# = FO1#
C1I# = ONE# / C1#
A1# = A1# * C1I#
B1# = B1# * C1I#
D1# = D1# * C1I#
E1# = E1# * C1I#
F1# = F1# * C1I#
AO2# = ONE# - EB2# * CS2#
BO2# = -EB2# * SN2#
CO2# = ONE# + EB2# * CS2#
DO2# = -XC2# * AO2# - YC2# * BO2#
EO2# = -YC2# * CO2# - XC2# * BO2#
FO2# = XC2# * XC2# * AO2# + TWO# * XC2# * YC2# * BO2# + YC2# * YC2# * CO2# - RB2# * RB2#
A2# = AO2# * SNA2# - TWO# * BO2# * CSSN# + CO2# * CSA2#
B2# = (AO2# - CO2#) * CSSN# - BO2# * (CSA2# - SNA2#)
C2# = AO2# * CSA2# + TWO# * BO2# * CSSN# + CO2# * SNA2#
D2# = DO2# * SNA# - EO2# * CSA#
E2# = DO2# * CSA# + EO2# * SNA#
F2# = FO2#
C2I# = ONE# / C2#
A2# = A2# * C2I#
B2# = B2# * C2I#
D2# = D2# * C2I#
E2# = E2# * C2I#
F2# = F2# * C2I#
IF ECC1 = 1 THEN
    GG# = ZERO#
    BB# = ZERO#
    CC# = A2#
    DD# = TWO# * D2#
    EE# = F2#
    GOTO 432
END IF
A# = B2# * A1# - B1# * A2#
B# = E2# * A1# - E1# * A2# + TWO# * (B2# * D1# - B1# * D2#)
C# = B2# * F1# - B1# * F2# + TWO# * (E2# * D1# - E1# * D2#)
D# = E2# * F1# - E1# * F2#
DA# = -(A2# - A1#)
DD# = -(D2# - D1#)
DF# = -(F2# - F1#)
DB# = -FOUR# * (B2# - B1#)
DE# = -FOUR# * (E2# - E1#)
GG# = A# * DB# - DA# * DA#
BB# = A# * DE# + B# * DB# - FOUR# * DA# * DD#
CC# = B# * DE# + C# * DB# - FOUR# * DD# * DD# - TWO# * DF# * DA#
DD# = C# * DE# + D# * DB# - FOUR# * DD# * DF#
EE# = D# * DE# - DF# * DF#
432 EEI# = ONE# / EE#
AA1# = SCL4# * GG# * EEI#
BB1# = SCL3# * BB# * EEI#
CC1# = SCL2# * CC# * EEI#
DD1# = SCL1# * DD# * EEI#
EE1# = ONE#
'
'*****CALL FOUR TO SOLVE A FOURTH ORDER EQUATION FOR ELLIPSE INTERSECTIONS
'
CALL RTFOUR(AA1#, BB1#, CC1#, DD1#, EE1#, K, RT#(), ERT#(), EPS#, EPSC#)
'
'****** K           - NUMBER OF ROOTS
'****** RT(I),I=1,K - ROOTS (X - VALUES OF INTERSECTION POINTS)
'
IF K = 0 THEN
    '
    '****** IFLAG = 0  - NO INTERSECTION
    '
    IFLAG = 0
    GOTO 500
END IF
'
'*****ELIMINATE CLOSE ROOTS IF THERE ARE MORE TNAN 3
'
301 IF K > 2 THEN
    FOR i = 1 TO K
        RR# = RT#(i)
        FOR J = i + 1 TO K
            IF ABS(RR# - RT#(J)) < EPSC# THEN
                K = K - 1
                FOR M = J TO K
                    RT#(M) = RT#(M + 1)
                NEXT
                RT#(K + 1) = 9.999999999999999D+28
                GOTO 301
            END IF
        310 NEXT
    300 NEXT
END IF
IF K = 3 THEN K = 2
'
'     SOLVE FOR INTERSECTION POINTS EACH X FROM RT(I) GENERATES FOUR Ys
'     IF A PAIR OF Ys FOR DIFFERENT ELLIPSES ARE WITHIN EPS, TRUE INTERSECTI
'
FOR i = 1 TO K
    X# = SCL1# * RT#(i)
    P1# = E1# + B1# * X#
    Q1# = A1# * X# * X# + TWO# * D1# * X# + F1#
    P2# = E2# + B2# * X#
    Q2# = A2# * X# * X# + TWO# * D2# * X# + F2#
    DDS1# = (P1# * P1# - Q1#)
    DDS2# = (P2# * P2# - Q2#)
    IF DDS1# < ZERO# THEN DDS1# = 0#
    IF DDS2# < ZERO# THEN DDS2# = 0#
    RDS1# = SQR(DDS1#)
    RDS2# = SQR(DDS2#)
    Y1# = -P1# + RDS1#
    Y2# = -P2# + RDS2#
    TMPD#(1) = ABS(Y1# - Y2#)
    TMPY#(1) = HALF# * (Y1# + Y2#)
    Y1# = -P1# + RDS1#
    Y2# = -P2# - RDS2#
    TMPD#(2) = ABS(Y1# - Y2#)
    TMPY#(2) = HALF# * (Y1# + Y2#)
    Y1# = -P1# - RDS1#
    Y2# = -P2# - RDS2#
    TMPD#(3) = ABS(Y1# - Y2#)
    TMPY#(3) = HALF# * (Y1# + Y2#)
    Y1# = -P1# - RDS1#
    Y2# = -P2# + RDS2#
    TMPD#(4) = ABS(Y1# - Y2#)
    TMPY#(4) = HALF# * (Y1# + Y2#)
    DISMIN# = 9.999999999999999D+28
    FOR J = 1 TO 4
        IF TMPD#(J) < DISMIN# THEN
            DISMIN# = TMPD#(J)
            JMIN = J
        END IF
    NEXT
    RXY#(i, 1) = X#
    RXY#(i, 2) = TMPY#(JMIN)
10 NEXT
L = K
IF ECC1 = 1 THEN
    R1# = RXY#(1, 1)
    R2# = RXY#(2, 1)
    IF R1# > R2# THEN
        TMP# = R2#
        R2# = R1#
        R1# = TMP#
    END IF
    RBB# = 2 * RBAR1#
    RXY#(1, 1) = R1#
    RXY#(2, 1) = R2#
    RXY#(1, 2) = ZERO#
    RXY#(2, 2) = ZERO#
    IF R1# > RBB# THEN
        IFLAG = 0
        GOTO 500
    END IF
    IF R2# < -RBB# THEN
        IFLAG = 0
        GOTO 500
    END IF
    DST# = R2# - R1#
    IF R1# >= -RBB# AND R2# <= RBB# THEN
        IFLAG = 2
        GOTO 11
    END IF
    IF R1# < -RBB# AND R2# > -RBB# THEN
        IFLAG = 1
        ADS# = R2# + RBB#
    END IF
    IF R1# < RBB# AND R2# > RBB# THEN
        IFLAG = 1
        ADS# = RBB# - R1#
    END IF
    COEF# = ADS# / DST#
END IF
'
'***** L - NUMBER OF ROOTS
'
11 FOR II = 1 TO L
    X# = RXY#(II, 1) * SNA# + RXY#(II, 2) * CSA#
    Y# = -RXY#(II, 1) * CSA# + RXY#(II, 2) * SNA#
    RXY#(II, 1) = X#
    RXY#(II, 2) = Y#
129 NEXT
IF L = 4 THEN
    DMIN# = 1E+37
    FOR i = 1 TO 4
        FOR J = 1 TO 4
            IF i <> J THEN
                DXIJ = RXY#(i, 1) - RXY#(J, 1)
                DYIJ = RXY#(i, 2) - RXY#(J, 2)
                DST# = SQR(DXIJ * DXIJ + DYIJ * DYIJ)
                IF DST# < DMIN# THEN
                    DMIN# = DST#
                    Imin = i
                    JMIN = J
                END IF
            END IF
        NEXT
    NEXT
    KK = 0
    FOR i = 1 TO 4
        IF i <> Imin AND i <> JMIN THEN
            KK = KK + 1
            RXYT#(KK, 1) = RXY#(i, 1)
            RXYT#(KK, 2) = RXY#(i, 2)
        END IF
    NEXT
    L = 2
    FOR i = 1 TO L
        RXY#(i, 1) = RXYT#(i, 1)
        RXY#(i, 2) = RXYT#(i, 2)
    NEXT
END IF
'
A1# = AO1#
B1# = BO1#
C1# = CO1#
D1# = DO1#
E1# = EO1#
F1# = FO1#
'
A2# = AO2#
B2# = BO2#
C2# = CO2#
D2# = DO2#
E2# = EO2#
F2# = FO2#
IF IFLAG <> 1 THEN IFLAG = L
IF L = 2 THEN
    X1# = RXY#(1, 1)
    Y1# = RXY#(1, 2)
    X2# = RXY#(2, 1)
    Y2# = RXY#(2, 2)
    Dx# = (X2# - X1#)
    Dy# = (Y2# - Y1#)
    DS# = SQR(Dx# * Dx# + Dy# * Dy#)
    IF DS# = ZERO# THEN
        DS# = 1#
    END IF
    CNX1# = Dy# / DS#
    CNY1# = -Dx# / DS#
    Xc# = HALF# * (X1# + X2#)
    Yc# = HALF# * (Y1# + Y2#)
    XYCN#(1, 1) = XC1R# + Xc#
    XYCN#(1, 2) = YC1R# + Yc#
    XYCN#(2, 1) = XC1R# + X1#
    XYCN#(2, 2) = YC1R# + Y1#
    XYCN#(3, 1) = XC1R# + X2#
    XYCN#(3, 2) = YC1R# + Y2#
    Dx# = Xc# - XC1#
    Dy# = Yc# - YC1#
    CVX1# = Dx#
    CVY1# = Dy#
    DS# = SQR(Dx# * Dx# + Dy# * Dy#)
    CS# = Dx# / DS#
    SN# = Dy# / DS#
    IF (CS# * CNX1# + SN# * CNY1#) < ZERO# THEN
        CNX1# = -CNX1#
        CNY1# = -CNY1#
    END IF
    CTX1# = -CNY1#
    CTY1# = CNX1#
    CNX2# = -CNX1#
    CNY2# = -CNY1#
    PP# = X1# * CNX1# + Y1# * CNY1#
    '
    '*********COMPUTE TRUE PENETRATION OF THE FIRST ELLIPSE
    '
    GG# = CS# * CS# * A1# + TWO# * CS# * SN# * B1# + SN# * SN# * C1#
    IF ABS(GG#) < EPSC# THEN GG# = EPSC#
    BB# = CS# * Xc# * A1# + B1# * (Xc# * SN# + Yc# * CS#) + SN# * Yc# * C1# + D1# * CS# + E1# * SN#
    CC# = Xc# * Xc# * A1# + TWO# * B1# * Xc# * Yc# + Yc# * Yc# * C1# + TWO# * D1# * Xc# + TWO# * E1# * Yc# + F1#
    P# = BB# / GG#
    Q# = CC# / GG#
    DIS# = P# * P# - Q#
    IF DIS# < ZERO THEN DIS# = ZERO#
    DIS# = SQR(DIS#)
    GT1# = ABS(-P# - DIS#)
    GT2# = ABS(-P# + DIS#)
    IF GT2# < GT1# THEN GT1# = GT2#
    PN1# = GT1#
    X1# = PN1# * CS# + Xc#
    Y1# = PN1# * SN# + Yc#
    PN1# = ABS(X1# * CNX1# + Y1# * CNY1# - PP#)
    PN1# = PN1# * COEF#
    '
    '********* PN1 - PENETRATION, (X1,Y1) - IMAGE OF CONTACT ON ELLIPSE SURFACE
    '
    XT1# = X1# - XC1#
    YT1# = Y1# - YC1#
    '
    '********* (XT1,YT1) - VECTOR FROM PARTICLE CENTER TO IMAGE OF CONTACT
    '
    IF ISFLAG = 0 THEN
        AX# = RBAR1# * (ONE# + ECC1#)
        CSA# = (XT1# * COS(THETA1#) + YT1# * SIN(THETA1#)) / AX#
        BA# = (ONE# - ECC1#) / (ONE# + ECC1#)
        R1# = RBAR1# * (ONE# + ECC1#) / BA#
        BA# = BA# * BA#
        R1# = R1# * (ONE# - CSA# * CSA# * (ONE# - BA#)) ^ 1.5#
    END IF
    '         XCVR1#=X1#-R1#*CNX1#
    '         YCVR1#=Y1#-R1#*CNY1#
    '
    '********* R1 - RADIUS OF CURVATURE, (XCVR1,YCVR1) - CENTER OF CURVATURE
    '
    Dx# = Xc# - XC2#
    Dy# = Yc# - YC2#
    CVX2# = Dx#
    CVY2# = Dy#
    DS# = SQR(Dx# * Dx# + Dy# * Dy#)
    CS# = Dx# / DS#
    SN# = Dy# / DS#
    '
    '*********COMPUTE TRUE PENETRATION OF THE SECOND ELLIPSE
    '
    GG# = CS# * CS# * A2# + TWO# * CS# * SN# * B2# + SN# * SN# * C2#
    IF ABS(GG#) < EPSC# THEN GG# = EPSC#
    BB# = CS# * Xc# * A2# + B2# * (Xc# * SN# + Yc# * CS#) + SN# * Yc# * C2# + D2# * CS# + E2# * SN#
    CC# = Xc# * Xc# * A2# + TWO# * B2# * Xc# * Yc# + Yc# * Yc# * C2# + TWO# * D2# * Xc# + TWO# * E2# * Yc# + F2#
    P# = BB# / GG#
    Q# = CC# / GG#
    DIS# = P# * P# - Q#
    IF DIS# < ZERO# THEN DIS# = ZERO#
    DIS# = SQR(DIS#)
    GT1# = ABS(-P# - DIS#)
    GT2# = ABS(-P# + DIS#)
    IF GT2# < GT1# THEN GT1# = GT2#
    PN2# = GT1#
    X2# = PN2# * CS# + Xc#
    Y2# = PN2# * SN# + Yc#
    PN2# = ABS(X2# * CNX1# + Y2# * CNY1# - PP#)
    PN2# = PN2# * COEF#
    '
    '********* PN2 - PENETRATION, (X2,Y2) - IMAGE OF CONTACT ON ELLIPSE SURFACE
    '
    XT2# = X2# - XC2#
    YT2# = Y2# - YC2#
    '
    '********* (XT2,YT2) - VECTOR FROM PARTICLE CENTER TO IMAGE OF CONTACT
    '
    IF ISFLAG = 0 THEN
        AX# = RBAR2# * (ONE# + ECC2#)
        CSA# = (XT2# * COS(THETA2#) + YT2# * SIN(THETA2#)) / AX#
        BA# = (ONE# - ECC2#) / (ONE# + ECC2#)
        R2# = RBAR2# * (ONE# + ECC2#) / BA#
        BA# = BA# * BA#
        R2# = R2# * (ONE# - CSA# * CSA# * (ONE# - BA#)) ^ 1.5#
    END IF
    '         XCVR2#=X2#-R2#*CNX2#
    '         YCVR2#=Y2#-R2#*CNY2#
    '
    '********* R2 - RADIUS OF CURVATURE, (XCVR2,YCVR2) - CENTER OF CURVATURE
    '
    CMP1# = ABS(DX1#) + ABS(DY1#) + ABS(DT1#) + ABS(FRN#)
    CMP2# = ABS(DX2#) + ABS(DY2#) + ABS(DT2#) + ABS(FRN#)
    '
    '*****IF ALL INCREMENTS AND FORCES ARE ZERO, SET THEM BASED ON PENETRATION
    '
    IF CMP1# = ZERO# AND CMP2# = ZERO# THEN
        DX1# = PN1# * CNX1#
        DY1# = PN1# * CNY1#
        DX2# = PN2# * CNX2#
        DY2# = PN2# * CNY2#
    END IF
    IF INSIDE = 1 GOTO 20
    '
    '*****FORCE CALCULATIONS
    '
    '  (a) RELATIVE DISPLACEMENTS BETWEEN PARTICLES
    '
    Dx# = DX2# - DX1#
    Dy# = DY2# - DY1#
    '
    '  (b) VECTOR PRODUCTS TO RESOLVE CARTESIAN DISPLACEMENTS
    '
    PL1N1# = CVX1# * CNY1# - CVY1# * CNX1#
    PL2N1# = CVX2# * CNY1# - CVY2# * CNX1#
    PL1T1# = CVX1# * CTY1# - CVY1# * CTX1#
    PL2T1# = CVX2# * CTY1# - CVY2# * CTX1#
    '
    '  (d) NORMAL AND TANGENTIAL RELATIVE DISPLACEMENTS
    '
    IF MODE = 0 THEN
        DN# = DT2# * PL2N1# - DT1# * PL1N1#
    END IF
    '
    DT# = DT2# * PL2T1# - DT1# * PL1T1#
    IF CMP1# = ZERO# AND CMP2# = ZERO# THEN
        DN# = ZERO#
        DT# = ZERO#
    END IF
    IF MODE = 0 THEN
        DN# = DN# + Dx# * CNX1# + Dy# * CNY1#
    END IF
    '
    DT# = DT# + Dx# * CTX1# + Dy# * CTY1#
    '
    '  (d) JOINT CONTACT STIFFNESS
    '
    IF ISFLAG = 1 THEN
        DI# = ONE# / DAVER#
    ELSE
        DI# = ONE# / (R1# + R2#)
    END IF
    '
    '  (e) INCREMENTS OF NORMAL AND TANGENTIAL FORCES
    '

    IF MODE = 0 THEN
        DFN# = DN# * STIF# * DI#
    ELSE
        DFN# = -(PN1# + PN2#) * STIF# * DI# - FRN#
        DN# = DFN# / (STIF# * DI#)
    END IF
    '
    DFT# = DT# * STIF# * DI# * XLAMBDA#
    '
    '  (f) CHECK IF CURRENT INCREMENT DISRUPS THE CONTACT (IFLAG SET TO 0)2
    '
    FR# = FRN# + DFN#
    IF FR# > ZERO# THEN
        IFLAG = 0
        GOTO 500
    END IF
    '
    '  (g) MAXIMUM SHEARING RESISTANCE
    '
    FTMAX# = ABS(FR# * AMU#)
    '
    '  (h) CHECK FOR SLIDE WITH CURRENT TANGENTIAL FORCE
    '
    FT# = FRT# + DFT#
    ISL = 0
    IF AMU# > 9! THEN GOTO 499
    IF ABS(FT#) > FTMAX# THEN
        FT = SGN(FTMAX) * ABS(FT)
        DFT# = FT# - FRT#
        ISL = 1
    END IF
    '
    '   (i) DUMP NORMAL FORCE INCREMENT AND TANGENTIAL (IF NO SLIP)
    '

    499 DFND# = DFN# * (ONE# + BDT#)
    DFTD# = DFT#
    IF ISL = 0 THEN DFTD# = DFT# * (ONE# + BDT#)
    '
    '   (j) INCREMENT DUMPED NORMAL AND TANGENTIAL FORCES
    '
    FRD# = FRN# + DFND#
    FTD# = FRT# + DFTD#
    '
    '   (k) RESOLVE FORCES INTO CARTESIAN COMPOINENTS
    '
    FXD1# = FRD# * CNX1# + FTD# * CTX1#
    FYD1# = FRD# * CNY1# + FTD# * CTY1#
    FXD2# = -FXD1#
    FYD2# = -FYD1#
    '
    '   (l) CALCULATE MOMETS
    '
    DM1# = CVX1# * FYD1# - CVY1# * FXD1#
    DM2# = CVX2# * FYD2# - CVY2# * FXD2#
    '
    '     FORCE CALCULATION COMPLETE
    '*****NUMBER OF ROOTS EQUAL TO 2 PROCESSESD
    '
    20 '
END IF
'
IF L = 0 AND INSIDE = 1 THEN
    '
    '*****NO ROOTS; CHECK IF ONE ELLIPSE INSIDE THE OTHER
    '
    IFLAG = -1
    Dx# = XC2# - XC1#
    Dy# = YC2# - YC1#
    X0# = XC1#
    Y0# = YC1#
    DS# = SQR(Dx# * Dx# + Dy# * Dy#)
    IF ABS(DS#) < EPS THEN
        DS# = ONE#
        Dx# = ZERO#
        Dy# = ONE#
    END IF
    CS# = Dx# / DS#
    SN# = Dy# / DS#
    GG# = CS# * CS# * A1# + TWO# * CS# * SN# * B1# + SN# * SN# * C1#
    BB# = CS# * X0# * A1# + B1# * (X0# * SN# + Y0# * CS#) + SN# * Y0# * C1# + D1# * CS# + E1# * SN#
    CC# = X0# * X0# * A1# + TWO# * B1# * X0# * Y0# + Y0# * Y0# * C1# + TWO# * D1# * X0# + TWO# * E1# * Y0# + F1#
    P# = BB# / GG#
    Q# = CC# / GG#
    DIS# = (P# * P# - Q#)
    IF DIS# >= ZERO# THEN
        T1# = -P# - SQR(DIS#)
        T2# = -P# + SQR(DIS#)
        GG# = CS# * CS# * A2# + TWO# * CS# * SN# * B2# + SN# * SN# * C2#
        BB# = CS# * X0# * A2# + B2# * (X0# * SN# + Y0# * CS#) + SN# * Y0# * C2# + D2# * CS# + E2# * SN#
        CC# = X0# * X0# * A2# + TWO# * B2# * X0# * Y0# + Y0# * Y0# * C2# + TWO# * D2# * X0# + TWO# * E2# * Y0# + F2#
        P# = BB# / GG#
        Q# = CC# / GG#
        DIS# = (P# * P# - Q#)
        IF DIS# >= ZERO# THEN
            T3# = -P# - SQR(DIS#)
            T4# = -P# + SQR(DIS#)
            IF T1# > T3# AND T1# < T4# AND T2# > T3# AND T2# < T4# THEN
                '
                '*****FIRST ELLIPSE INSIDE SECOND
                '
                IFLAG = -1
            END IF
            IF T3# > T1# AND T3# < T2# AND T4# > T1# AND T4# < T2# THEN
                '
                '*****SECOND ELLIPSE INSIDE FIRST
                '
                IFLAG = -1
            END IF
        END IF
    END IF
END IF
500 IF IFLAG = 0 THEN
    DFN# = ZERO#
    DFT# = ZERO#
    FXD1# = ZERO#
    FYD1# = ZERO#
    FXD2# = ZERO#
    FYD2# = ZERO#
    DM1# = ZERO#
    DM2# = ZERO#
END IF
CVX1 = CVX1#
CVY1 = CVY1#
CVX2 = CVX2#
CVY2 = CVY2#
CNX1 = CNX1#
CNY1 = CNY1#
CTX1 = CTX1#
CTY1 = CTY1#
DX1 = DX1#
DY1 = DY1#
DT1 = DT1#
DX2 = DX2#
DY2 = DY2#
DT2 = DT2#
FRN = FRN#
FRT = FRT#
DFN = DFN#
DFT = DFT#
FXD1 = FXD1#
FYD1 = FYD1#
FXD2 = FXD2#
FYD2 = FYD2#
DM1 = DM1#
DM2 = DM2#
DN = DN#
DT = DT#
XYCN#(4, 1) = PN1#
XYCN#(4, 2) = PN2#
FOR i = 1 TO 4
    FOR J = 1 TO 2
        XYCN(i, J) = XYCN#(i, J)
    NEXT
NEXT
IF KSW = 1 THEN
    SWAP XC2, XC1
    SWAP YC2, YC1
    SWAP RBAR2, RBAR1
    SWAP ECC2, ECC1
    SWAP THETA2, THETA1
    SWAP CVX2, CVX1
    SWAP CVY2, CVY1
    CNX1 = -CNX1
    CNY1 = -CNY1
    CTX1 = -CTX1
    CTY1 = -CTY1
    SWAP DX2, DX1
    SWAP DY2, DY1
    SWAP DT2, DT1
    Xc = XYCN(1, 1)
    Yc = XYCN(1, 2)
    Dx = Xc - XC1
    Dy = Yc - YC1
    IF (Dx * CNX1 + Dy * CNY1) < 0 THEN
        CNX1 = -CNX1
        CNY1 = -CNY1
        CTX1 = -CTX1
        CTY1 = -CTY1
    END IF
END IF
IF NOC% <> 0 AND IFLAG = 0 THEN
    XC1 = XC1RR
    YC1 = YC1RR
    RBAR1 = RBAR1RR
    ECC1 = ECC1RR
    THETA1 = THETA1RR
    XC2 = XC2RR
    YC2 = YC2RR
    RBAR2 = RBAR2RR
    ECC2 = ECC2RR
    THETA2 = THETA2RR
    CVX1 = CVX1RR
    CVY1 = CVY1RR
    CVX2 = CVX2RR
    CVY2 = CVY2RR
    CNX1 = CNX1RR
    CNY1 = CNY1RR
    CTX1 = CTX1RR
    CTY1 = CTY1RR
    DX1 = DX1RR
    DX2 = DY1RR
    DT1 = DT1RR
    DX2 = DX2RR
    DY2 = DY2RR
    DT2 = DT2RR
    FRM = FRNRR
    DRT = FRTRR
    DFN = DFNRR
    DFT = DFTRR
    FXD1 = FXD1RR
    FYD1 = FYD1RR
    FXD2 = FXD2RR
    FYD2 = FYD2RR
    DM1 = DM1RR
    DM2 = DM2RR
    DN = DNRR
    DT = DTRR
    IFLAG = IFLAGRR
    SIFT = STIFRR
    XLAMBDA = XLAMBDARR
    BDT = BDTRR
    AMU = AMURR
    RBAR1 = RBAR1 * (1 + NOC% * 0.01)
    RBAR2 = RBAR2 * (1 + NOC% * 0.01)
    NOC% = NOC% + 1
    IF NOC% < 5 THEN
        GOTO REPRUF:
    END IF
END IF
EXIT SUB
END SUB

SUB GENPOLY (XY(), N, R, E, T)
DIM TH(50)
TDG = T * 3.1415926# / 180
'
'**** Generate a polygon within an ellipse
'
'     R - mean radius
'     E - eccentricity
'     T - orientation (deg)
'     N - number of sides
'
'**** Polygon coordinates are with respect to mass center
'
P2 = 2 * 3.141593
DT = 2 * P2 / (N + 1)
FOR i = 2 TO N + 1
    TH(i) = TH(i - 1) + DT * RND(1)
NEXT
CO = P2 / TH(N + 1)
FOR i = 2 TO N + 1
    TH(i) = CO * TH(i)
NEXT
FOR i = 1 TO N
    TT = TH(i) + TDG
    XY(2 * i - 1) = R * (1 + E) * COS(TT)
    XY(2 * i) = R * (1 - E) * SIN(TT)
NEXT
XY(0) = N
N2 = 2 * N
KX = 1
KY = 2
XI = XY(KX)
YI = XY(KY)
AX = 0!
AY = 0!
FOR i = 1 TO N
    KX = KX + 2
    IF KX > N2 THEN KX = 1
    KY = KX + 1
    XF = XY(KX)
    Yf = XY(KY)
    Dx = XF - XI
    Dy = Yf - YI
    Rx = (XI * XI + XI * XF + XF * XF) / 6
    Ry = (YI * YI + YI * Yf + Yf * Yf) / 6
    CX = .5 * (XI + XF)
    Cy = .5 * (YI + Yf)
    AX = AX + CX * Dy
    Xg = Xg + Rx * Dy
    AY = AY + Cy * Dx
    Yg = Yg + Ry * Dx
    XI = XF
    YI = Yf
NEXT
IF AX = 0! THEN AX = 1!
IF AY = 0! THEN AY = 1!
Xg = Xg / AX
Yg = Yg / AY
FOR i = 1 TO N
    XY(2 * i - 1) = XY(2 * i - 1) - Xg
    XY(2 * i) = XY(2 * i) - Yg
NEXT
XY(0) = N
ERASE TH
END SUB

SUB GETCONT (IB1, IB2, IAD, X1, Y1, X2, Y2, FRN, FRT, DSN, DST, M1, NDPART)
X1 = AA(IB1) + AA(IB1 + 11)
Y1 = AA(IB1 + 1) + AA(IB1 + 12)
X2 = AA(IB2) + AA(IB2 + 11)
Y2 = AA(IB2 + 1) + AA(IB2 + 12)
DSN = AA(IAD + 2)
DST = AA(IAD + 3)
FRN = AA(IAD + 4)
FRT = AA(IAD + 5)
END SUB

SUB GetContactList
NC = 0
CALL DISPLAY("Memorizing current contact list NC = ")
IAD = M1 + NDISK * NDPART
CLISTrep:
IB1 = AA(IAD)
IF IB1 = 0 GOTO FINlist
IB2 = AA(IAD + 1)
NC = NC + 1
LOCATE 1, 41: PRINT USING "####"; NC
CALL CLISTi(NC, 0, IB1)
CALL CLISTi(NC, 1, IB2)
FOR i = 2 TO NCPART - 1
    V = AA(IAD + i)
    CALL CLISTi(NC, i, V)
NEXT
IAD = IAD + NCPART
GOTO CLISTrep:
FINlist:
CALL CLEAN
NCONT = NC
END SUB

SUB GETDISC (N, Xc, Yc, Tc, ITYPE, IBTYP)
IF N <= NDISK THEN
    IF N < 0 THEN
        IAD = -N
    ELSE
        IAD = M1 + (N - 1) * NDPART
    END IF
    Xc = AA(IAD) + AA(IAD + 11)
    Yc = AA(IAD + 1) + AA(IAD + 12)
    Tc = AA(IAD + 10) + AA(IAD + 13)
    IBTYP = AA(IAD + 8)
    ITYPE = AA(IAD + 9)
ELSE
    IADD = AA(30)
    IF IADD <> 0 THEN
        ISEQ = N - NDISK
        IF ISEQ <= IADD THEN
            KD = NADD + (ISEQ - 1) * 7
            IBB = AA(KD + 5)
            CALL GETDISC(-IBB, Xc, Yc, Tc, ITYPE, IBTYP)
        END IF
    END IF
END IF
END SUB

SUB GETDISZ (N, Xc, Yc, Tc, ITYPE, IBTYP)
IF N <= NDISK THEN
    IF N < 0 THEN
        IAD = -N
    ELSE
        IAD = M1 + (N - 1) * NDPART
    END IF
    Xc = ZZ(IAD) + ZZ(IAD + 11)
    Yc = ZZ(IAD + 1) + ZZ(IAD + 12)
    Tc = ZZ(IAD + 10) + ZZ(IAD + 13)
    IBTYP = ZZ(IAD + 8)
    ITYPE = ZZ(IAD + 9)
ELSE
    IADD = ZZ(30)
    IF IADD <> 0 THEN
        ISEQ = N - NDISK
        IF ISEQ <= IADD THEN
            KD = NADD + (ISEQ - 1) * 7
            Xc = ZZ(KD + 1)
            Yc = ZZ(KD + 2)
            Dx = ZZ(KD + 3)
            Dy = ZZ(KD + 4)
            RBAR = 0.5 * (Dx * Dx + Dy * Dy)
            ITYPE = -RBAR
            Tc = ATAN2(Dy, Dx)
            IBTYP = -2
        END IF
    END IF
END IF
END SUB

SUB GetDiskList
CALL DISPLAY("Memorizing current particle list ND = ")
IAB = M1
FOR i = 1 TO NDISK
    LOCATE 1, 42: PRINT USING "####"; i
    FOR J = 0 TO NDPART - 1
        V = AA(IAB + J)
        CALL DLISTi(i, J, V)
    NEXT
    IAB = IAB + NDPART
NEXT
END SUB

SUB GetLinks (N)
END SUB

SUB Graphs (XYlimG(), XYscrG(), XYpltG(), XYoutG(), ContrG(), LablG$(), Cfile$, Dfile$)
SHARED Olist$(), Hlist$(), RECMAX, LASTBALL
SHARED XYlimF(), XYscrF(), XYpltF(), XYoutF(), ContrF(), LablF$()
DIM Boxes(30), OPTIONS(30), ITEMS$(30)
DIM SBoxes(30), SOPTIONS(30), SItems$(30)
DIM Saddr(30), Sname$(30), Par$(10), Scale(30)
SHARED Xar(), Yar()
DIM XYlimR(10), XYscrR(10), XYpltR(10), XYoutR(10), ContrR(10), LablR$(10)
SHARED NOBFILE, XSPMODE, YSPMODE, SPMODE, WDMODE
IF VAL(Olist$(0)) = 0 THEN
    PPOS = RECMAX
    CALL SUMMARY(0, PPOS, Cfile$)
    KV = VAL(Olist$(0))
END IF
IF VAL(Hlist$(0)) = 0 THEN
    PPOS = RECHIS
    CALL NEWHIST(0, PPOS)
    KH = VAL(Hlist$(0))
END IF
'
NewXLoc = 0
NITEMS = 11
ITEMS$(1) = "1 - Stress    "
ITEMS$(2) = "2 - Strain    "
ITEMS$(3) = "3 - Contacts  "
ITEMS$(4) = "4 - Normal forces"
ITEMS$(5) = "5 - Shear  forces"
ITEMS$(6) = "6 - Orientations"
ITEMS$(7) = "7 - V/H groups "
ITEMS$(8) = "8 - Equilibrium"
ITEMS$(9) = "9 - Particles"
ITEMS$(10) = "A - Set x-var"
ITEMS$(11) = "B - Exit "
'
XYoutG(0) = 30: Lx$ = "Output No": Xaddress = 13: SCX = 1
'
VIEW (XYscrG(2) + 8, 0)-(XSPMODE - 1, YSPMODE - 1): CLS: VIEW
MainPollG:
CALL MakeMenu(XYlimG(), XYscrG(), XYoutG(), ContrG(), ITEMS$(), NITEMS, Boxes(), 0, OPTIONS(), G$)
CALL LOCATOR(XYlimG(), XYscrG(), XYoutG(), G$, ContrG())
CALL MakeMenu(XYlimG(), XYscrG(), XYoutG(), ContrG(), ITEMS$(), NITEMS, Boxes(), 1, OPTIONS(), G$)
NFLx = 0: VVL = VAL(ITEMS$(0))
IF VVL < 0 THEN
    ITEMS$(0) = STR$(-VVL)
    NFLx = 1
END IF
'************
S$ = ITEMS$(0): CALL Match(S$, G$)
NSG = VAL(G$)
IF VAL(S$) = NITEMS - 1 THEN
    OPTIONS(NITEMS - 1) = 1
    GOTO MainPollG
END IF
'
IF VAL(S$) = NITEMS THEN
    CLOSE #1
    WINDOW: VIEW: CLS
    EXIT SUB
END IF
IF VAL(G$) = 9 THEN
    IF VAL(LablF$(30)) <> 0 THEN
        G$ = "-" + LablF$(30)
    ELSE
        IF LASTBALL <> 0 THEN
            G$ = STR$(-LASTBALL)
        END IF
    END IF
END IF
'
CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
'
SMainPollG:
CALL MakeMenu(XYlimG(), XYscrG(), XYoutG(), ContrG(), SItems$(), SNitems, SBoxes(), 0, SOPTIONS(), G$)
CALL LOCATOR(XYlimG(), XYscrG(), XYoutG(), G$, ContrG())
CALL MakeMenu(XYlimG(), XYscrG(), XYoutG(), ContrG(), SItems$(), SNitems, SBoxes(), 1, SOPTIONS(), G$)
NFLg = 0: VVL = VAL(SItems$(0))
IF VVL < 0 THEN
    SItems$(0) = STR$(-VVL)
    NFLg = 1
END IF
S$ = SItems$(0): CALL Match(S$, G$)
NSGG = VAL(G$)
IF VAL(S$) = SNitems - 1 THEN
    IF SOPTIONS(SNitems - 1) = 1 THEN
        SOPTIONS(SNitems - 1) = 0
        ContrG(3) = 0
    ELSE
        ContrG(3) = 0
        SOPTIONS(SNitems - 1) = 1
    END IF
    GOTO SMainPollG
END IF
IF VAL(S$) = SNitems THEN
    GOTO MainPollG
ELSE
    N = VAL(S$)
    IF N = 0 OR N > SNitems GOTO SMainPollG
    SN$ = Sname$(N): PaddressY = Saddr(N): Sc = Scale(N)
    NewYLoc = 0: HistNoY = 0
    IF NSG = 9 THEN
        IF NSGG = 12 THEN
            LOCATE 1, 6: INPUT "Ball No "; NBL
            LOCATE 1, 6: PRINT STRING$(74, " ");
            LablF$(30) = MID$(STR$(NBL), 2)
            G$ = STR$(-NBL)
            CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
            GOTO SMainPollG
        ELSE
            NewYLoc = Sc: Sc = 1
            HistNoY = NSG
        END IF
    END IF
    '
    IF NSG = 7 THEN
        IF NSGG < 5 THEN
            IF NSGG = 1 THEN
                LOCATE 1, 6: INPUT "Hist  No "; NBL
                LOCATE 1, 6: PRINT STRING$(74, " ");
                G$ = STR$(-NBL) + "!"
                CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
                GOTO SMainPollG
            END IF
            IF NSGG = 2 THEN
                LOCATE 1, 6: INPUT "Group No "; NBL
                LOCATE 1, 6: PRINT STRING$(74, " ");
                G$ = STR$(NBL) + "!"
                CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
                GOTO SMainPollG
            END IF
            IF NSGG = 3 THEN
                NBL = 1
                G$ = STR$(NBL) + "!"
                CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
                GOTO SMainPollG
            END IF
            IF NSGG = 4 THEN
                NBL = 36
                G$ = STR$(NBL) + "!"
                CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
                GOTO SMainPollG
            END IF
        ELSE
            HistNoY = VAL(MID$(SItems$(1), 12))
            IF NSGG = 5 THEN
                PaddressY = -VAL(MID$(SItems$(2), 12)) + 1
            END IF
            IF NSGG = 10 THEN
                VIEW (0, 0)-(XYscrG(2) + 8, YSPMODE - 1): CLS: VIEW
                KM = VAL(Hlist$(0))
                L = 1: CP = 1
                FOR K = 1 TO KM
                    L = L + 1
                    IF L < WDMODE THEN
                        LOCATE L, CP: PRINT Hlist$(K)
                    ELSE
                        L = 0: CP = CP + 40
                    END IF
                NEXT
                GOTO SMainPollG
            END IF
            NewYLoc = 0
            GOTO DORET:
        END IF
    END IF
    IF NewYLoc = 0 OR HistNoY = 0 THEN
        IF PaddressY = 0 THEN
            NewYLoc = MatchOList(SN$)
            HistNoY = 0
        ELSE
            HistNoY = MatchHList(SN$)
            NewYLoc = 0
        END IF
    END IF
    IF OPTIONS(NITEMS - 1) = 1 THEN
        NewXLoc = NewYLoc
        Xaddress = PaddressY
        HistNoX = HistNoY
        OPTIONS(NITEMS - 1) = 0
        Lx$ = Sname$(N)
        GOTO MainPollG
    END IF
    DORET:
    '
    NOBFILE = 0
    WINDOW: VIEW
    VIEW (0, 0)-(XYscrG(2) + 8, YSPMODE - 1): CLS: VIEW
    CALL Retreve(PaddressY, HistNoY, NewYLoc, Sc, Xaddress, HistNoX, NewXLoc, SCX, Xar(), Yar(), Cfile$)
    NS = 1
    Ly$ = SItems$(N)
    K% = INSTR(1, Ly$, "-")
    LablG$(2) = MID$(Ly$, K% + 2)
    LablG$(1) = Lx$
    IF SOPTIONS(SNitems - 1) = 1 THEN LablG$(0) = "C": ContrG(3) = 0
    FOR I = 1 TO 10
        XYlimR(I) = XYlimG(I)
        XYscrR(I) = XYscrG(I)
        XYpltR(I) = XYpltG(I)
        XYoutR(I) = XYoutG(I)
        ContrR(I) = ContrR(I)
        LablR$(I) = LablG$(I)
    NEXT
    XYscrG(1) = 0: XYscrG(2) = 0: KSEQR = KSEQ: KSEQ = -2
    CALL PLOT(Xar(), Yar(), Par$(), NA, NS, XYlimG(), XYscrG(), XYpltG(), LablG$(), ContrG())
    KSEQ = KSEQR
    LablF$(1) = LablG$(1): LablF$(2) = LablG$(2)
    XYlimF(1) = XYlimG(1): XYlimF(2) = XYlimG(2)
    XYlimF(3) = XYlimG(3): XYlimF(4) = XYlimG(4)
    C$ = LablG$(0): LablG$(0) = Cfile$
    CALL LastFile(Dfile$)
    CALL SaveData(Dfile$, Xar(), Yar(), Par$(), LablG$())
    CALL SaveDataExt(Dfile$, Xar(), Yar(), Par$(), LablG$())
    LablG$(0) = C$
    LablG$(0) = ""
    XYoutG(1) = .5 * (XYlimG(1) + XYlimG(2)): XYoutG(2) = XYoutG(1)
    XYoutG(3) = .5 * (XYlimG(3) + XYlimG(4)): XYoutG(4) = XYoutG(3)
    GOTO SMainPollG
END IF
'
GOTO MainPollG
'************
END SUB

SUB HHOLLOW (NBD, Xc, Yc, Rad, RATIO, RAVER, HCFLAG)
pi = 4 * ATN(1!)
Dx = 0!: Dy = 0!: DT = 0!: Vx = 0!: Vy = 0!: Vt = 0!
Fx = 0!: Fy = 0!: Mt = 0!: IBTYP = 1!
RADE = Rad
RADi = Rad / RATIO
ARC1 = pi * RADE: NBDE = INT(ARC1 / (4 * RAVER))
ARC2 = pi * RADi: NBDI = INT(ARC2 / (4 * RAVER))
NBD = NBDE + NBDI
DTHE = 2 * pi / NBDE
DTHI = 2 * pi / NBDI
TH0 = -.5 * DTHE
DTH = DTHE
Rad = RADE
IAD = M1
FOR I = 1 TO NBD
    IF I = NBDE + 1 THEN TH0 = -.5 * DTHI: DTH = DTHI: Rad = RADi: IBTYP = 2!
    THI = TH0 + (I - 1) * DTH
    THF = THI + DTH
    XI = Xc + Rad * COS(THI)
    XF = Xc + Rad * COS(THF)
    YI = Yc + Rad * SIN(THI)
    Yf = Yc + Rad * SIN(THF)
    Xp = .5 * (XI + XF)
    YP = .5 * (YI + Yf)
    ARG1 = Yf - YI
    ARG2 = XF - XI
    ITYPE = -.25 * SQR(ARG1 ^ 2 + ARG2 ^ 2)
    Theta = ATAN2(ARG1, ARG2)
    CALL AAI(IAD, Xp)
    CALL AAI(IAD + 1, YP)
    CALL AAI(IAD + 2, Vx)
    CALL AAI(IAD + 3, Vy)
    CALL AAI(IAD + 4, Vt)
    CALL AAI(IAD + 5, Fx)
    CALL AAI(IAD + 6, Fy)
    CALL AAI(IAD + 7, Mt)
    CALL AAI(IAD + 8, IBTYP)
    CALL AAI(IAD + 9, ITYPE)
    CALL AAI(IAD + 10, Theta)
    CALL AAI(IAD + 11, Dx)
    CALL AAI(IAD + 12, Dy)
    CALL AAI(IAD + 13, DT)
    LINE (XI, YI)-(XF, Yf), 15
    IAD = IAD + NDPART
NEXT

END SUB

SUB HIST (AR(), AAr(), Par$(), NA, NS, XYlim(), XYscr(), XYplt(), Labl$(), Contr())
'
' Ar        - Primary    Data   - Group   Classifier
' AAr       - Associated Data   - For Group Averages
' AAr(0) =0 - Plot Primary Histogram
' AAr(0)<>0 - Plot Group   Averages
'
' Operation modes:
'                  Aar(0) = 0 : Contr(7) = 0 : Contr(8) =-1 - Linear chart
'                  Aar(0) = 0 : Contr(7) = 1 : Contr(8) = 0 - Polar 180
'                  Aar(0) = 0 : Contr(7) = 1 : Contr(8) = 1 - Polar 360
'                  Aar(0) <>0 : Group averages in the same combination
'
'$DYNAMIC Xar,Yar
SHARED XSPMODE, YSPMODE, SPMODE
DIM Ah(72), AAh(72), XY(8), Xar(1, 1), Yar(1, 1), ID%(1, 1)
SCREEN SPMODE
Alpha = 45: Separ = .5
Xmin = 1E+37: Xmax = -1E+37: DTR = ATN(1!) / 45
NH = NA: NA = 0: NhR = NH
IF NH < 0 THEN
    NH = -NH
    FOR I = 1 TO NH
        Ah(I) = AR(I)
        AAh(I) = AAr(I)
    NEXT
    AR(0) = Xmax
    Xmin = 0: Xmax = 180
    dh = (Xmax - Xmin) / NH
    IF Contr(8) > 0 THEN
        GOTO ALLDONE
    ELSE
        Xmin = AR(39)
        Xmax = AR(40)
        IF Xmax < Xmin THEN
            SWAP Xmin, Xmax
        END IF
        Ymin = 0: Ymax = 0
        FOR K = 1 TO NH
            IF Ah(K) > Ymax THEN
                Ymax = Ah(K)
            END IF
        NEXT
        GOTO Pl
    END IF
END IF
IF NH = 0 THEN
    IF Contr(7) <> 0 THEN
        NH = 18
        NhR = NH
    ELSE
        NH = 10
        NhR = NH
    END IF
END IF
Rpt: Xmin = 1E+37: Xmax = -1E+37: DTR = ATN(1!) / 45
IF NhR <> NH THEN
    FOR I = 1 TO NH
        Ah(I) = AR(I)
        AAh(I) = AAr(I)
    NEXT
    AR(0) = Xmax
    Xmin = 0: Xmax = 180
    dh = (Xmax - Xmin) / NH
    GOTO ALLDONE
END IF
'
FOR I% = 1 TO NA
    IF AR(I%) > Xmax THEN Xmax = AR(I%)
    IF AR(I%) < Xmin THEN Xmin = AR(I%)
NEXT
'
dh = (Xmax - Xmin) / NH: dl = .25 * dh
FOR K% = 1 TO 50
    Ah(K%) = 0: AAh(K%) = 0!
NEXT
'
FOR I% = 1 TO NA
    K = INT((AR(I%) - Xmin) / dh) + 1
    IF K > NH THEN K = NH
    Ah(K) = Ah(K) + 1: AAh(K) = AAh(K) + AAr(I%)
NEXT
'
ALLDONE:
'
Ymin = 0: Ymax = 0
FOR K = 1 TO NH
    Z = Ah(K): IF Z <> 0 THEN AAh(K) = AAh(K) / Z
    IF Z > Ymax THEN Ymax = Z
NEXT
Npoly = 4
WINDOW: VIEW: CLS
IF AAr(0) > 0 THEN GOTO AA
Ymax = Ymax: Xran = Xmax - Xmin: Yran = Ymax - Ymin
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax + .1 * Yran
IF Contr(7) = 0 GOTO Pl
'
'Screen for polar histogram
'
Tmin = Xmin: Tmax = Xmax: CLR = XYscr(6): IF ABS(Tmin - AR(0)) < dh / 2 THEN Tmin = AR(0): Tmax = Tmin + 180: dh = (Tmax - Tmin) / NH
Ymax = Ymax: Ymin = -1.1 * Ymax: Ymax = 1.1 * Ymax: Xmin = Ymin: Xmax = Ymax
XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 40: XYscr(3) = 40: XYscr(4) = 0
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax: Labl$(1) = "": Labl$(2) = ""
XYlim(8) = XSPMODE: XYlim(9) = YSPMODE: XYlim(10) = SPMODE
Contr(1) = -1
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
Imaker% = 0: IF Contr(3) = 99 THEN Imaker% = 1: Contr(3) = 2
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
IF Contr(3) <> 0 THEN
    RHOFF = XYplt(7): RVOFF = XYplt(8): RHLAS = XYplt(9): RVLAS = XYplt(10)
    IF Imaker% = 0 THEN
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVLAS - RVOFF) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymin
    ELSE
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVOFF - RVLAS) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymax
    END IF
END IF
N = LEN(Labl$(0))
IF N <> 0 THEN
    IF N > 2 AND MID$(Labl$(0), N, 1) = "R" THEN GOTO FNTx
END IF
IF Contr(8) = 0 GOTO Plt
FOR K = 1 TO NH
    Ah(NH + K) = Ah(K)
NEXT K
XLmin = 1E+37: XLmax = -1E+37
YLmin = 1E+37: YLmax = -1E+37
Nh1 = 2 * NH: GOTO Lp
Plt: Nh1 = NH
Lp: IF Contr(3) <> 0 THEN
    IF Imaker% = 0 THEN
        PRINT #3, "1 setlinewidth"
        PRINT #3, "newpath"
    ELSE
        CALL MSETLINE(1!)
        IF Contr(9) <> 0 THEN
            PRINT #3, "<PolyLine<GroupID    1>"
            PRINT #3, "<Fill 15>"
        END IF
    END IF
END IF
FOR K% = 1 TO Nh1
    Ti = Tmin + (K% - 1) * dh: Tf = Ti + dh: Tc = .5 * (Ti + Tf)
    RF = Ah(K%)
    Tii = Ti * DTR: X1 = RF * COS(Tii): Y1 = RF * SIN(Tii)
    Tff = Tf * DTR: X2 = RF * COS(Tff): Y2 = RF * SIN(Tff)
    XY(1) = 0: XY(2) = 0: XY(3) = X1: XY(4) = Y1: XY(5) = X2: XY(6) = Y2
    IF (K% MOD 2) = 0 THEN Alpha = Tc + 45 ELSE Alpha = Tc - 45
    Npoly = 3
    IF XLmin > X1 THEN XLmin = X1
    IF XLmin > X2 THEN XLmin = X2
    IF YLmin > Y1 THEN YLmin = Y1
    IF YLmin > Y2 THEN YLmin = Y2
    IF XLmax < X1 THEN XLmax = X1
    IF XLmax < X2 THEN XLmax = X2
    IF YLmax < Y1 THEN YLmax = Y1
    IF YLmax < Y2 THEN YLmax = Y2
    IF Contr(9) = 0 GOTO Wire
    IF Contr(3) <> 0 AND Imaker% <> 0 THEN
        CALL MPNT(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        CALL MPNT(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
    END IF
    IF K% = 1 THEN
        X0 = X1: Y0 = Y1
        LINE (X1, Y1)-(X2, Y2)
        IF Contr(3) <> 0 AND Imaker% = 0 THEN
            CALL MOVETO(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
            CALL LINETO(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        END IF
        Xp = X2: YP = Y2
        GOTO NxtH
    END IF
    IF K% > 1 AND K% < Nh1 THEN
        LINE (Xp, YP)-(X1, Y1)
        LINE (X1, Y1)-(X2, Y2)
        IF Contr(3) <> 0 AND Imaker% = 0 THEN
            CALL LINETO(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
            CALL LINETO(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        END IF
        Xp = X2: YP = Y2
        GOTO NxtH
    END IF
    IF K% = Nh1 THEN
        LINE (Xp, YP)-(X1, Y1)
        LINE (X1, Y1)-(X2, Y2)
        LINE (X2, Y2)-(X0, Y0)
        IF Contr(3) <> 0 THEN
            IF Imaker% = 0 THEN
                CALL LINETO(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(X0, Y0, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                PRINT #3, "stroke"
            ELSE
                PRINT #3, ">"
            END IF
        END IF
        Xran = .05 * (XLmax - XLmin)
        Yran = .05 * (YLmax - YLmin)
        XLmin = XLmin - Xran
        YLmin = YLmin - Yran
        XLmax = XLmax + Xran
        YLmax = YLmax + Yran
        LINE (XLmin, 0)-(XLmax, 0)
        LINE (0, YLmin)-(0, YLmax)
        IF NhR <> NH THEN
            NV = AR(42)
            IF ABS(NV) = 1 OR ABS(NV) = 2 THEN
                LOCATE 30, 8: PRINT USING "A=##.### Ta=###.# B=##.### Tb=###.# Av=##.###^^^^"; AR(37); AR(39) / DTR; AR(38); AR(40) / DTR; AR(41)
            ELSE
                LOCATE 30, 8: PRINT USING "Aw = ##.### At = ###.# Tt = ##.### "; AR(38); AR(39); AR(40) / DTR;
            END IF
        END IF
        IF Contr(3) <> 0 THEN
            IF Imaker% = 0 THEN
                CALL MOVETO(XLmin, 0, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(XLmax, 0, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                PRINT #3, "stroke"
                CALL MOVETO(0, YLmin, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(0, YLmax, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                PRINT #3, "stroke"
            ELSE
                XI = RCOEFX * XLmin + RXSHIFT
                XF = RCOEFX * XLmax + RXSHIFT
                CALL MLINE(XI, RYSHIFT, XF, RYSHIFT, 1, 0)
                YI = RCOEFY * YLmin + RYSHIFT
                Yf = RCOEFY * YLmax + RYSHIFT
                CALL MLINE(RXSHIFT, YI, RXSHIFT, Yf, 1, 0)
            END IF
        END IF
        GOTO NxtH
    END IF
    Wire: CALL Fill(XY(), Npoly, XYlim(), XYscr(), XYplt(), Contr(), Separ, Alpha, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT, Imaker%)
NxtH: NEXT
GOTO Fin:
Pl: WINDOW: VIEW: CLS
XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 40: XYscr(3) = 40: XYscr(4) = 0
CLR = XYscr(6)
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax: Labl$(1) = "X": Labl$(2) = "N"
XYlim(8) = XSPMODE: XYlim(9) = YSPMODE: XYlim(10) = SPMODE
Contr(1) = 0
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
Imaker% = 0: IF Contr(3) = 99 THEN Imaker% = 1: Contr(3) = 2
dh = (AR(40) - AR(39)) / NH
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
IF Contr(3) <> 0 THEN
    RHOFF = XYplt(7): RVOFF = XYplt(8): RHLAS = XYplt(9): RVLAS = XYplt(10)
    IF Imaker% = 0 THEN
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVLAS - RVOFF) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymin
    ELSE
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVOFF - RVLAS) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymax
    END IF
END IF
N = LEN(Labl$(0))
IF N <> 0 THEN
    IF N > 2 AND MID$(Labl$(0), N, 1) = "R" THEN GOTO FNTx
END IF
FOR K% = 1 TO NH
    XI = AR(39) + (K% - 1) * dh: XF = XI + dh
    YI = 0: Yf = Ah(K%)
    XY(1) = XI + dl: XY(2) = YI: XY(3) = XY(1): XY(4) = Yf: XY(5) = XF - dl: XY(6) = Yf: XY(7) = XY(5): XY(8) = YI
    Npoly = 4
    CALL Fill(XY(), Npoly, XYlim(), XYscr(), XYplt(), Contr(), Separ, Alpha, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT, Imaker%)
NEXT
LINE (XYlim(1), XYlim(3))-(XYlim(2), XYlim(3))
LOCATE 1, 6: PRINT USING "Min=##.###^^^^ Max=##.###^^^^ Avr=##.###^^^^ Cv=##.### "; AR(39); AR(40); AR(41); AR(38)
GOTO Fin:
AA: Ymax = -1E+37: Ymin = 1E+37
FOR K = 1 TO NH
    IF AAh(K) > Ymax THEN Ymax = AAh(K)
    IF AAh(K) < Ymin THEN Ymin = AAh(K)
NEXT
Xran = Xmax - Xmin: Yran = Ymax - Ymin
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax + .1 * Yran
IF Contr(7) = 0 GOTO Pl1
'Screen for polar histogram
XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 40: XYscr(3) = 40: XYscr(4) = 0
Tmin = Xmin: Tmax = Xmax: CLR = XYscr(6): IF ABS(Tmin - AR(0)) < dh / 2 THEN Tmin = AR(0): Tmax = Tmin + 180: dh = (Tmax - Tmin) / NH
Ymin = -1.1 * Ymax: Ymax = 1.1 * Ymax: Xmin = Ymin: Xmax = Ymax
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax: Labl$(1) = "": Labl$(2) = ""
Contr(1) = -1
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
Imaker% = 0: IF Contr(3) = 99 THEN Imaker% = 1: Contr(3) = 2
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
IF Contr(3) <> 0 THEN
    RHOFF = XYplt(7): RVOFF = XYplt(8): RHLAS = XYplt(9): RVLAS = XYplt(10)
    IF Imaker% = 0 THEN
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVLAS - RVOFF) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymin
    ELSE
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVOFF - RVLAS) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymax
    END IF
END IF
N = LEN(Labl$(0))
IF N <> 0 THEN
    IF N > 2 AND MID$(Labl$(0), N, 1) = "R" THEN GOTO FNTx
END IF
IF Contr(8) = 0 GOTO Plt1
FOR K = 1 TO NH
    AAh(NH + K) = AAh(K)
NEXT K
Nh1 = 2 * NH: GOTO Lp1
Plt1: Nh1 = NH
Lp1:
IF Contr(3) <> 0 THEN
    IF Imaker% = 0 THEN
        PRINT #3, "1 setlinewidth"
        PRINT #3, "newpath"
    ELSE
        CALL MSETLINE(1!)
        IF Contr(9) <> 0 THEN
            PRINT #3, "<PolyLine<GroupID    1>"
            PRINT #3, "<Fill 15>"
        END IF
    END IF
END IF
FOR K% = 1 TO Nh1
    Ti = Tmin + (K% - 1) * dh: Tf = Ti + dh: Tc = .5 * (Ti + Tf)
    RF = AAh(K%)
    Tii = Ti * DTR: X1 = RF * COS(Tii): Y1 = RF * SIN(Tii)
    Tff = Tf * DTR: X2 = RF * COS(Tff): Y2 = RF * SIN(Tff)
    XY(1) = 0: XY(2) = 0: XY(3) = X1: XY(4) = Y1: XY(5) = X2: XY(6) = Y2
    IF (K% MOD 2) = 0 THEN Alpha = Tc + 45 ELSE Alpha = Tc - 45
    Npoly = 3
    IF XLmin > X1 THEN XLmin = X1
    IF XLmin > X2 THEN XLmin = X2
    IF YLmin > Y1 THEN YLmin = Y1
    IF YLmin > Y2 THEN YLmin = Y2
    IF XLmax < X1 THEN XLmax = X1
    IF XLmax < X2 THEN XLmax = X2
    IF YLmax < Y1 THEN YLmax = Y1
    IF YLmax < Y2 THEN YLmax = Y2
    IF Contr(9) = 0 GOTO Wire1
    IF Contr(3) <> 0 AND Imaker% <> 0 THEN
        CALL MPNT(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        CALL MPNT(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
    END IF
    IF K% = 1 THEN
        X0 = X1: Y0 = Y1
        LINE (X1, Y1)-(X2, Y2)
        IF Contr(3) <> 0 AND Imaker% = 0 THEN
            CALL MOVETO(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
            CALL LINETO(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        END IF
        Xp = X2: YP = Y2
        GOTO Nxt1
    END IF
    IF K% > 1 AND K% < Nh1 THEN
        LINE (Xp, YP)-(X1, Y1)
        LINE (X1, Y1)-(X2, Y2)
        IF Contr(3) <> 0 AND Imaker% = 0 THEN
            CALL LINETO(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
            CALL LINETO(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
        END IF
        Xp = X2: YP = Y2
        GOTO Nxt1
    END IF
    IF K% = Nh1 THEN
        LINE (Xp, YP)-(X1, Y1)
        LINE (X1, Y1)-(X2, Y2)
        LINE (X2, Y2)-(X0, Y0)
        IF Contr(3) <> 0 THEN
            IF Imaker% = 0 THEN
                CALL LINETO(X1, Y1, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(X2, Y2, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(X0, Y0, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                PRINT #3, "stroke"
            ELSE
                PRINT #3, ">"
            END IF
        END IF
        Xran = .05 * (XLmax - XLmin)
        Yran = .05 * (YLmax - YLmin)
        XLmin = XLmin - Xran
        YLmin = YLmin - Yran
        XLmax = XLmax + Xran
        YLmax = YLmax + Yran
        LINE (XLmin, 0)-(XLmax, 0)
        LINE (0, YLmin)-(0, YLmax)
        IF Contr(3) <> 0 THEN
            IF Imaker% = 0 THEN
                CALL MOVETO(XLmin, 0, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(XLmax, 0, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                PRINT #3, "stroke"
                CALL MOVETO(0, YLmin, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                CALL LINETO(0, YLmax, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
                PRINT #3, "stroke"
            ELSE
                XI = RCOEFX * XLmin + RXSHIFT
                XF = RCOEFX * XLmax + RXSHIFT
                CALL MLINE(XI, RYSHIFT, XF, RYSHIFT, 1, 0)
                YI = RCOEFY * YLmin + RYSHIFT
                Yf = RCOEFY * YLmax + RYSHIFT
                CALL MLINE(RXSHIFT, YI, RXSHIFT, Yf, 1, 0)
            END IF
        END IF
        GOTO Nxt1
    END IF
    Wire1: CALL Fill(XY(), Npoly, XYlim(), XYscr(), XYplt(), Contr(), Separ, Alpha, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT, Imaker%)
Nxt1: NEXT
GOTO Fin:
Pl1: WINDOW: VIEW: CLS
XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 40: XYscr(3) = 40: XYscr(4) = 0
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax: Labl$(1) = "": Labl$(2) = ""
XYlim(8) = XSPMODE: XYlim(9) = YSPMODE: XYlim(10) = SPMODE
Contr(1) = -1
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
Imaker% = 0: IF Contr(3) = 99 THEN Imaker% = 1: Contr(3) = 2
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
IF Contr(3) <> 0 THEN
    RHOFF = XYplt(7): RVOFF = XYplt(8): RHLAS = XYplt(9): RVLAS = XYplt(10)
    IF Imaker% = 0 THEN
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVLAS - RVOFF) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymin
    ELSE
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVOFF - RVLAS) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymax
    END IF
END IF
N = LEN(Labl$(0))
IF N <> 0 THEN
    IF N > 2 AND MID$(Labl$(0), N, 1) = "R" THEN GOTO FNTx
END IF
FOR K% = 1 TO NH
    XI = Xmin + (K% - 1) * dh: XF = XI + dh
    YI = 0: Yf = AAh(K%)
    XY(1) = XI + dl: XY(2) = YI: XY(3) = XY(1): XY(4) = Yf: XY(5) = XF - dl: XY(6) = Yf: XY(7) = XY(5): XY(8) = YI
    CALL Fill(XY(), Npoly, XYlim(), XYscr(), XYplt(), Contr(), Separ, Alpha, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT, Imaker%)
NEXT
LINE (XYlim(1), XYlim(3))-(XYlim(2), XYlim(3))
GOTO Fin
Fin: IF LEN(Labl$(0)) <= 2 THEN TCK$ = Labl$(0): RRR$ = "": GOTO ZZH
TCK$ = MID$(Labl$(0), 1, 2)
IF TCK$ = "CR" THEN RRR$ = MID$(Labl$(0), 3): Labl$(0) = "R" + RRR$ ELSE TCK$ = MID$(Labl$(0), 1, 1): RRR$ = MID$(Labl$(0), 2)
ZZH: IF Contr(0) = 0 AND TCK$ = "CR" THEN Labl$(0) = "R" + RRR$: LOCATE 1, 59: PRINT "Press any key ...    ";: WHILE INKEY$ = "": WEND:: GOTO Rpt
IF Contr(3) <> 0 THEN
    LOCATE 1, 59: PRINT STRING$(21, " ");
    LOCATE 1, 59: INPUT "Fin/Rep/Edit "; IWHAT$
    IF IWHAT$ = "R" OR IWHAT$ = "r" THEN LOCATE 1, 59: PRINT STRING$(19, " ");: GOTO Rpt
    IF IWHAT$ = "F" OR IWHAT$ = "f" THEN LOCATE 1, 59: PRINT STRING$(19, " ");: GOTO ffa
    FOR I = 1 TO 6: Par$(I) = "": NEXT I
    NA = 0: NS = 0
    CALL SCREDIT(XYlim(), XYscr(), XYplt(), Labl$(), Par$(), Contr(), Xar(), Yar(), NA, NS, ID%())
END IF
ffa: IF Contr(3) = 0 AND TCK$ = "C" THEN
    LOCATE 1, 59: INPUT "Hard Copy (Y/N) "; IWHAT$: LOCATE 1, 59: PRINT STRING$(20, " ");
    IF IWHAT$ = "Y" OR IWHAT$ = "y" THEN Contr(3) = 2: Labl$(0) = "R" + RRR$: LOCATE 1, 59: PRINT STRING$(19, " ");: GOTO Rpt
END IF
FNTx: IF Contr(3) <> 0 THEN
    IF Imaker% = 0 THEN
        PRINT #3, "showpage"
    END IF
    CLOSE #3
END IF
ERASE Ah, AAh, XY, Par$, Xar, Yar, ID%
END SUB

FUNCTION IA (I)
CALL READNUM(I, Z$): IA = CVI(Z$)
END FUNCTION

SUB INITfile (Cfile$)
SHARED RNP, FW%, LNW%, FLD$
SHARED Xar(), Yar()
RNP = 1234567
FW% = 80
LNW% = 8
OPEN Cfile$ FOR RANDOM AS #1 LEN = FW%
FIELD #1, 80 AS FLD$
M1 = AA(1)
NDPART = AA(17)
NCPART = AA(18)
RECMAX = AA(20)
RECHIS = AA(27)
RECPOL = AA(28)
RECET = RECMAX + 2 * (24 - 1) + 1
RECEV = RECMAX + 2 * (25 - 1) + 1
RECSF = RECMAX + 2 * (37 - 1) + 1
'
N1 = INSTR(1, Cfile$, "_")
IF N1 = 0 THEN
    N1 = INSTR(1, Cfile$, "-")
    IF N1 = 0 THEN
        N1 = INSTR(1, Cfile$, "+")
    END IF
END IF
N2 = INSTR(1, Cfile$, ".")
IF N2 - N1 - 1 > 2 THEN
    L = N2 - N1 - 1
    VS$ = MID$(Cfile$, N1 + 1, L)
    CURFILE% = VAL(VS$)
END IF
END SUB

SUB INITfileZZ (Zfile$)
SHARED RNZ, FW%, LNW%, FLZ$
RNP = 1234567
FW% = 80
LNW% = 8
OPEN Zfile$ FOR RANDOM AS #6 LEN = FW%
FIELD #6, 80 AS FLZ$
RECMAXZZ = ZZ(20)
RECHISZZ = ZZ(27)
RECPOLZZ = ZZ(28)
END SUB

SUB NEWBINFILE (Nfile$)
SHARED nRNP, nFW%, nLNW%, nFLD$
nRNP = 1234567
nFW% = 80
nLNW% = 8
OPEN Nfile$ FOR RANDOM AS #2 LEN = nFW%
FIELD #2, 80 AS nFLD$
VL# = 1000: PS = 1: CALL AAID(PS, VL#) 'M1
VL# = NDISK: PS = 2: CALL AAID(PS, VL#) 'NDISK
VL# = 0.01: PS = 3:: CALL AAID(PS, VL#) 'CLEN
VL# = NBOX: PS = 4:: CALL AAID(PS, VL#)
VL# = TOL: PS = 5: CALL AAID(PS, VL#)
VL# = MODE: PS = 6: CALL AAID(PS, VL#)
VL# = NTYPE: PS = 7: CALL AAID(PS, VL#)
VL# = STIF: PS = 8: CALL AAID(PS, VL#)
VL# = XLAMBDA: PS = 9: CALL AAID(PS, VL#)
VL# = XAMU: PS = 10: CALL AAID(PS, VL#)
VL# = ECC0: PS = 11: CALL AAID(PS, VL#)
VL# = DENSITY: PS = 12: CALL AAID(PS, VL#)
VL# = NN0: PS = 13: CALL AAID(PS, VL#)
VL# = DEL: PS = 14: CALL AAID(PS, VL#)
VL# = NX: PS = 15: CALL AAID(PS, VL#)
VL# = NY: PS = 16: CALL AAID(PS, VL#)
VL# = NDPART: PS = 17: CALL AAID(PS, VL#)
VL# = NCPART: PS = 18: CALL AAID(PS, VL#)
VL# = TOLNEXT: PS = 19: CALL AAID(PS, VL#)
VL# = RECMAX: PS = 20: CALL AAID(PS, VL#)
VL# = XMIN: PS = 21: CALL AAID(PS, VL#)
VL# = XMAX: PS = 22: CALL AAID(PS, VL#)
VL# = YMIN: PS = 23: CALL AAID(PS, VL#)
VL# = YMAX: PS = 24: CALL AAID(PS, VL#)
VL# = NNR: PS = 25: CALL AAID(PS, VL#)
VL# = IREC: PS = 26: CALL AAID(PS, VL#)
VL# = RECHIS: PS = 27: CALL AAID(PS, VL#)
VL# = RECPOL: PS = 28: CALL AAID(PS, VL#)
VL# = RECTFIC: PS = 29: CALL AAID(PS, VL#)
VL# = NUMFICT: PS = 30: CALL AAID(PS, VL#)
VL# = RECPIPE: PS = 31: CALL AAID(PS, VL#)
VL# = NUMPIPE: PS = 32: CALL AAID(PS, VL#)
VL# = RECDELP: PS = 33: CALL AAID(PS, VL#)
VL# = NPOL: PS = 34: CALL AAI(PS, VL#)
VL# = XCRE: PS = 35: CALL AAID(PS, VL#)
VL# = XDES: PS = 36: CALL AAID(PS, VL#)
VL# = ETPo: PS = 37: CALL AAID(PS, VL#)
VL# = SINFo: PS = 38: CALL AAID(PS, VL#)
VL# = CARC: PS = 39: CALL AAID(PS, VL#)
VL# = EIJB(1, 1): PS = 40: CALL AAID(PS, VL#)
VL# = EIJB(1, 2): PS = 41: CALL AAID(PS, VL#)
VL# = EIJB(2, 2): PS = 42: CALL AAID(PS, VL#)
VL# = EIJB(2, 2): PS = 43: CALL AAID(PS, VL#)
VL# = ET: PS = 44: CALL AAID(PS, VL#)
VL# = EN: PS = 45: CALL AAID(PS, VL#)
VL# = EI: PS = 46: CALL AAID(PS, VL#)
VL# = EIi: PS = 47: CALL AAID(PS, VL#)
VL# = EI: PS = 48: CALL AAID(PS, VL#)
VL# = EIi: PS = 49: CALL AAID(PS, VL#)


FOR I = 1 TO NTYPE
    PS = IAB: VL# = 1#: CALL AAID(PS, VL#)
    IAB = IAB + 1:
    VL# = R(I): PS = IAB: CALL AAID(PS, VL#)
    IAB = IAB + 1
    E(I) = ECC0
    IAB = IAB + 1
    VL# = E(I): PS = IAB: CALL AAID(PS, VL#)
NEXT
IAB = M1
END SUB

SUB AAID (PS1, VL#)
SHARED nLNW%, nFW%, nRN, nRNP, nFLD$
'puts number in position ps
PS = PS1 + 1
BT = (PS - 1) * nLNW% + 1
RN = INT((BT - 1) / nFW%) + 1
PSR = BT - (RN - 1) * nFW%
IF RN <> RNP THEN
    GET #2, RN
    RNP = RN
END IF
Z$ = MKD$(VL)
MID$(FLD$, PSR, 4) = Z$
PUT #2, RN
END SUB

SUB INITP (XYlim())
SHARED SHAPES(), Trans(), BCT(), SPLIT()
'
Trans(0) = 0
M1 = AA(1)
NDISK = AA(2)
H = AA(3)
NBOX = AA(4)
TOL = AA(5): TOL = 5
IBPART = AA(6)
NTYPE = AA(7)
STIF = AA(8)
XLAMBDA = AA(9)
AMU = AA(10)
Ecc = AA(11)
DENSITY = AA(12)
NN = AA(13)
DELBOX = AA(14)
NXB = AA(15)
NYB = AA(16)
NDPART = AA(17)
NCPART = AA(18)
TOLNEXT = AA(19)
RECMAX = AA(20)
RECHIS = AA(27)
NPOL = AA(34)
IADD = AA(30)
NADD = AA(29)
IPIPE = AA(32) 'Number of pipes
NPIPE = AA(31) 'Pointer
IGROUP = AA(44) 'Pointer to groups
NGROUP = AA(45) 'Number of polygon groups
CALL NBDISK(NBD)
NCONT = (RECMAX - M1 - NDISK * NDPART) / NCPART
XMINA = AA(21)
XMAXA = AA(22)
YMINA = AA(23)
YMAXA = AA(24)
NBPART = AA(47)
IF NBPART = 0 THEN NBPART = 100
IF XMINA = 0 AND XMAXA = 0 THEN
    XMINA = 0: XMAXA = H
    YMINA = 0: YMAXA = H
END IF
Rad = .25 * (XMAXA - XMINA) + .25 * (YMAXA - YMINA)
Xran = XMAXA - XMINA: Yran = YMAXA - YMINA
XYlen = Xran
IF XYlen < Yran THEN XYlen = Yran
Xc = .5 * (XMAXA + XMINA)
Yc = .5 * (YMAXA + YMINA)
XMINA = Xc - .5 * XYlen
XMAXA = Xc + .5 * XYlen
YMINA = Yc - .5 * XYlen
YMAXA = Yc + .5 * XYlen
XYlim(1) = XMINA
XYlim(2) = XMAXA
XYlim(3) = YMINA
XYlim(4) = YMAXA
IAD = 50
I = 0
REPinitp:
NEDGE = AA(IAD)
IF NEDGE = 0 GOTO FINinitp
I = I + 1
SHAPES(I, 0) = NEDGE
FOR J = 1 TO NEDGE
    IAD = IAD + 1
    SHAPES(I, 2 * J - 1) = AA(IAD)
    IAD = IAD + 1
    SHAPES(I, 2 * J) = AA(IAD)
NEXT
IAD = IAD + 1
IF I < NTYPE GOTO REPinitp
FINinitp:
RAVER = 0
RMAXG = 0
W = H
IAD = 50
K = 0
RMIN = 1.E+37
FOR I = 1 TO NTYPE
    NEDGE = AA(IAD)
    IAD = IAD + 1
    IF NEDGE <= 10 THEN
        SHAPES(I, 0) = NEDGE
        FOR K = 1 TO NEDGE
            XED = AA(IAD)
            IAD = IAD + 1
            SHAPES(I, 2 * K - 1) = XED
            YED = AA(IAD)
            IAD = IAD + 1
            SHAPES(I, 2 * K) = YED
        NEXT
        IF NEDGE = 1 THEN
            R(I) = SHAPES(I, 1)
            E(I) = SHAPES(I, 2)
        END IF
    ELSE
        SHAPES(I, 0) = 1
        R(I) = NEDGE
        E(I) = Ecc
        SHAPES(I, 1) = R(I)
        SHAPES(I, 2) = E(I)
    END IF
    RAVER = RAVER + R(I)
    RAVERS = RAVERS + R(I) ^ 2
    IF RMIN > R(I) THEN RMIN = R(I)
NEXT
RAVER = RAVER / NTYPE
RAVERS = RAVERS / NTYPE
' BCONT
'
XYlim(1) = XMINA
XYlim(2) = XMAXA
XYlim(3) = YMINA
XYlim(4) = YMAXA

B = (XMAXA - XMINA) * 1.1
H = (YMAXA - YMINA) * 1.1
NBOX = B * H / (4 * RAVER * RAVER) + .5
NXB = CINT(SQR(NBOX * B / H) + .5)
NYB = CINT(SQR(NBOX * H / B) + .5)
DELBOX = B / NXB
NBOX = NXB * NYB
ARBOX = DELBOX * DELBOX
NBPART = INT(ARBOX / RMIN) + 20
LNA& = (NBOX + 1) * (NBPART + 1)
REDIM BCT(LNA&)
REDIM SPLIT(NBOX)

'      A(1)  =M1
'      A(2)  =NDISK
'      A(3)  =H
'      A(4)  =NBOX
'      A(5)  =TOL
'      A(6)  =MODE
'      A(7)  =NTYPE
'      A(8)  =STIF
'      A(9)  =XLAMBDA
'      A(10) =AMU
'      A(11) =ECC
'      A(12) =DENSITY
'      A(13) =NN+NN0
'      A(14) =DELBOX
'      A(15) =NX
'      A(16) =NY
'      A(17) =NDPART
'      A(18) =NCPART
'      A(19) =TOLNEXT
'      A(20) = IADMAX
'C
'      A(100)=GAMMA
'      A(101)=tdensity
'      A(102)=EIJB(1,1)
'      A(103)=EIJB(1,2)
'      A(104)=EIJB(2,1)
'      A(105)=EIJB(2,2)
'      A(106)=EN
'      A(107)=ET
'      A(108)=EW
'      A(109)=PR
'C
'      A(110)=BSIG(1,1)
'      A(111)=BSIG(1,2)
'      A(112) = BSIG(2, 1)
'      A(113)=BSIG(2,2)
'      A(114)=SN
'      A(115)=ST
'      A(116)=ST/SN
'      A(117)=SI
'      A(118)=SII
'      A(119)=PDI
'      A(120)=PDII
'      A(121)=SR
'C
'      A(122)=FX
'      A(123)=FY
'      A(124)=MO
'      A(125)=FBMAX
'      A(126)=MOMAX
'      A(127)=VBMAX
'      A(128)=FBAVG
'      A(129)=MOAVG
'      A(130)=VBAVG
'      A(131)=FCMAX
'      A(132)=FNMAX
'      A(133)=FSMAX
'      A(134)=FNAVG
'      A(135)=FSAVG
'      A(136)=FCAVG
'      A(137)=DLAVG
'      A(138)=DNAVG
'      A(139)=DSAVG
'C
'      A(100)=GAMMA
'      A(101)=tdensity
'      A(141)=ETAA
'      A(142)=ETBB
'      A(143)=ETHTH2
'      A(144)=ETHTH4
'C
'      A(145)=FNAA
'      A(146)=FNBB
'      A(147)=FNTH2
'      A(148)=FNTH4
'C
'      A(149)=AA
'      A(150)=BB
'      A(151)=THETA2
'      A(152)=THETA4
'C
'      A(153)=FSAW
'      A(154)=FSAT
'      A(155)=FSTH2
'C
'      A(156)=DLAA
'      A(157)=DLBB
'      A(158)=DLTH2
'      A(159)=DLTH4
'C
'      A(160)=DNAA
'      A(161)=DNBB
'      A(162)=DNTH2
'      A(163)=DNTH4
'C
'      A(164)=DSAW
'      A(165)=DSAT
'      A(166)=DSTH2
'C
'      A(167)=FNAAV
'      A(168)=FNBBV
'      A(169)=FNTH2V
'      A(170)=FNTH4V
'C
'      A(171)=AAV
'      A(172)=BBV
'      A(173)=THETA2V
'      A(174)=THETA4V
'C
'      A(175)=FSAWV
'      A(176)=FSATV
'      A(177)=FSTH2V
'C
'      A(178)=DLAAV
'      A(179)=DLBBV
'      A(180)=DLTH2V
'      A(181)=DLTH4V
'C
'      A(182)=DNAAV
'      A(183)=DNBBV
'      A(184)=DNTH2V
'      A(185)=DNTH4V
'C
'      A(186)=DSAWV
'      A(187)=DSATV
'      A(188)=DSTH2V
'C
'      A(189)=AVRVBX
'      A(190)=AVRVBY
'      A(191)=VBXMAX
'      A(192)=VBYMAX
'      A(193)=cdensity
'      A(194)=ro
'      A(195)=cvar
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C     NFN(20)
'C
'      IADS=200
'      I=1
'      DO 100 IAD=IADS,IADS+19,1
'      A(IAD)=NFN(I)
'      I=I+1
'100   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19

'C     NFS(20)
'C
'      IADS=IAD
'      I=1
'      DO 101 IAD=IADS,IADS+19,1
'      A(IAD)=NFS(I)
'      I=I+1
'101   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C     NCI(36)
'C
'      IADS=IAD
'      I=1
'      DO 102 IAD=IADS,IADS+35,1
'      A(IAD)=NCI(I)
'      I=I+1
'102   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C     FNNA(36)
'C
'      IADS=IAD
'      I=1
'      DO 103 IAD=IADS,IADS+35,1
'      A(IAD)=FNNA(I)
'      I=I+1
'103   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C     FSA(36)
'C
'      IADS=IAD
'      I=1
'      DO 104 IAD=IADS,IADS+35,1
'      A(IAD)=FSA(I)
'      I=I+1
'104   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C     DLA(36)
'C
'      IADS=IAD
'      I=1
'      DO 105 IAD=IADS,IADS+35,1
'      A(IAD)=DLA(I)
'      I=I+1
'105   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C     DNA(36)
'C
'      IADS=IAD
'      I=1
'      DO 106 IAD=IADS,IADS+35,1
'      A(IAD)=DNA(I)
'      I=I+1
'106   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C     DSA(36)
'C
'      IADS=IAD
'      I=1
'      DO 107 IAD=IADS,IADS+35,1
'      A(IAD)=DSA(I)
'      I=I+1
'107   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF ELLIPSE ORIENTATIONS IADS+35
'C     NCT(36)
'C
'      IADS=IAD
'      I=1
'      DO 108 IAD=IADS,IADS+35,1
'      A(IAD)=NCT(I)
'      I=I+1
'108   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF ELLIPSE ORIENTATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT vectors IADS+35
'C     NCIV(36)
'C
'      IADS=IAD
'      I=1
'      DO 109 IAD=IADS,IADS+35,1
'      A(IAD)=NCIV(I)
'      I=I+1
'109   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF ELLIPSE ORIENTATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT vectors IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35
'C     FNAV(36)
'C
'      IADS=IAD
'      I=1
'      DO 110 IAD=IADS,IADS+35,1
'      A(IAD)=FNAV(I)
'      I=I+1
'110   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF ELLIPSE ORIENTATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT vectors IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C     FSAV(36)
'C
'      IADS=IAD
'      I=1
'      DO 111 IAD=IADS,IADS+35,1
'      A(IAD)=FSAV(I)
'      I=I+1
'111   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF ELLIPSE ORIENTATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT vectors IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35
'C     DLAV(36)
'C
'      IADS=IAD
'      I=1
'      DO 112 IAD=IADS,IADS+35,1
'      A(IAD)=DLAV(I)
'      I=I+1
'112   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF ELLIPSE ORIENTATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT vectors IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C     DNAV(36)
'C
'      IADS=IAD
'      I=1
'      DO 113 IAD=IADS,IADS+35,1
'      A(IAD)=DNAV(I)
'      I=I+1
'113   CONTINUE
'C
'C*****HISTOGRAM OF AVERAGE CONTACT NORMAL FORCES  IADS=200
'C*****HISTOGRAM OF AVERAGE CONTACT SHEAR FORCES  IADS+19
'C*****POLAR HISTOGRAM OF CONTACT NORMALS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35,
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF ELLIPSE ORIENTATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT vectors IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR FORCES IADS+35
'C*****POLAR HISTOGRAM OF CONTACT VECTOR LENGTHS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT NORMAL PENETRATIONS IADS+35
'C*****POLAR HISTOGRAM OF CONTACT SHEAR PENETRATIONS IADS+35
'C     DSAV(36)
'C
'      IADS=IAD
'      I=1
'      DO 114 IAD=IADS,IADS+35,1
'      A(IAD)=DSAV(I)
'      I=I+1
'114   CONTINUE
END SUB

REM $STATIC
SUB INITPS (Contr())
PRINT #3, "erasepage initgraphics save"
PRINT #3, "/hcenter {"
PRINT #3, "/text exch def"
PRINT #3, "/yc exch def"
PRINT #3, "/xc exch def"
PRINT #3, "text dup stringwidth pop neg 2 div"
PRINT #3, "xc add yc moveto show } def"
PRINT #3, "/hright {"
PRINT #3, "/text exch def"
PRINT #3, "/yc exch def"
PRINT #3, "/xc exch def"
PRINT #3, "text dup stringwidth pop neg"
PRINT #3, "xc add yc moveto show } def"
PRINT #3, "/Cio {/r exch def /y exch def /x exch def x y r 0 360 arc stroke} def"; ""
PRINT #3, "/Dia {/r exch def /y exch def /x exch def x y r 4   0 Sign} def"
PRINT #3, "/Rec {/r exch def /y exch def /x exch def x y r 4  45 Sign} def"
PRINT #3, "/Tri {/r exch def /y exch def /x exch def x y r 3  90 Sign} def"
PRINT #3, "/Pen {/r exch def /y exch def /x exch def x y r 5  90 Sign} def"
PRINT #3, "/Oct {/r exch def /y exch def /x exch def x y r 6  90 Sign} def"
PRINT #3, "/Itr {/r exch def /y exch def /x exch def x y r 3 270 Sign} def"
PRINT #3, "/Cir {/r exch def /y exch def /x exch def newpath x y r 0 360 arc fill} def"
PRINT #3, "/Str {/r exch def /y exch def /x exch def x y r 3  90 Sign x y r 3 270 Sign} def"
PRINT #3, "/Rtr {/r exch def /y exch def /x exch def x y r 3   0 Sign} def"
PRINT #3, "/Ltr {/r exch def /y exch def /x exch def x y r 3 180 Sign} def"
PRINT #3, "/Sign {"
PRINT #3, "/t exch def"
PRINT #3, "/n exch def"
PRINT #3, "/dt 360 n idiv def"
PRINT #3, "/rad exch def"
PRINT #3, "/yy exch def"
PRINT #3, "/xx exch def"
PRINT #3, "gsave"
PRINT #3, "xx yy translate"
PRINT #3, "rad rad scale"
PRINT #3, "newpath"
PRINT #3, "0 0 moveto"
PRINT #3, "t cos t sin moveto"
PRINT #3, "/t t dt add def"
PRINT #3, "/n n 1 sub def"
PRINT #3, "n {t cos t sin lineto"
PRINT #3, "/t t dt add def} repeat"
PRINT #3, "closepath"
PRINT #3, "fill"
PRINT #3, "grestore"
PRINT #3, "} def"
PRINT #3, "/ellipsedict 10 dict def"
PRINT #3, " ellipsedict /mtrx matrix put"
PRINT #3, "/ellipse"
PRINT #3, "{ellipsedict begin"
PRINT #3, "/gr exch def"
PRINT #3, "/ecc exch def"
PRINT #3, "/rbar exch def"
PRINT #3, "/t exch def"
PRINT #3, "/y exch def"
PRINT #3, "/x exch def"
PRINT #3, "/savematrix mtrx currentmatrix def"
PRINT #3, "/prx rbar rbar ecc mul add def"
PRINT #3, "/pry rbar rbar ecc mul sub def"
PRINT #3, "newpath"
PRINT #3, "x y translate"
PRINT #3, "t rotate"
PRINT #3, "prx pry scale"
PRINT #3, "0 0 1 0 360 arc"
PRINT #3, "gr setgray"
PRINT #3, "fill"
PRINT #3, "0 setgray"
PRINT #3, "0 0 1 0 360 arc"
PRINT #3, "savematrix setmatrix"
PRINT #3, "stroke"
PRINT #3, "} def"
END SUB

REM $DYNAMIC
SUB InsertBBall (N, NBD)

FOR I = 1 TO NDISK
    ITAG = DLIST(I, 8)
    IF ITAG <> 1 THEN
        NBD = I
        EXIT FOR
    END IF
NEXT
'
'  NBD - first non-boundary disk, EXCHANGE NBD AND N
'
IAB = M1 + (N - 1) * NDPART
IAD = M1 + (NBD - 1) * NDPART
FOR J = 0 TO NDPART - 1
    TMP1 = DLIST(NBD, J)
    TMP2 = DLIST(N, J)
    CALL DLISTi(NBD, J, TMP2)
    CALL DLISTi(N, J, TMP1)
NEXT
FOR I = 1 TO NCONT
    IB1 = CLIST(I, 0)
    IB2 = CLIST(I, 1)
    IF IB1 = IAB THEN CALL CLISTi(I, 0, IAD)
    IF IB2 = IAB THEN CALL CLISTi(I, 1, IAD)
    IF IB1 = IAD THEN CALL CLISTi(I, 0, IAB)
    IF IB2 = IAD THEN CALL CLISTi(I, 1, IAB)
NEXT
CALL DLISTi(NBD, 8, 1)
END SUB

REM $STATIC
SUB INTELLI (XC1, YC1, THETA1, ITYP1, XC2, YC2, THETA2, ITYP2, Xi1, Yi1, Xi2, Yi2, PN1, PN2, IFLAG)
END SUB

REM $DYNAMIC
SUB Limits (XYP(), XYlim())
N = XYP(0)
Xmin = 1E+37: Xmax = -1E+37
Ymin = 1E+37: Ymax = -1E+37
FOR I = 1 TO N
    X = XYP(2 * I - 1)
    Y = XYP(2 * I)
    IF Xmin > X THEN Xmin = X
    IF Xmax < X THEN Xmax = X
    IF Ymin > Y THEN Ymin = Y
    IF Ymax < Y THEN Ymax = Y
NEXT
XYlim(1) = Xmin
XYlim(2) = Xmax
XYlim(3) = Ymin
XYlim(4) = Ymax
END SUB

SUB LINETO (Xp, YP, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
XIR = Xp * RCOEFX + RXSHIFT
YIR = YP * RCOEFY + RYSHIFT
PRINT #3, USING "###.## ###.## lineto"; XIR; YIR
END SUB

SUB GetData (Page$(), Dat$(), Dat%(), NRMIN, NRMAX, NCMIN, NCMAX, Erline, MES$) STATIC
STATIC NR%, NC%
DIM Page%(25)
50001 'Subroutine SCREEN emulates full screen editing features
NRMIN% = NRMIN: NRMAX% = NRMAX: NCMIN% = NCMIN: NCMAX% = NCMAX: MF% = 3: Mouse% = 0: CALL Qmouse(Mouse%, BT%, MX%, MY%)
50100 EFL = Erline: NCOL% = 2: IF EFL <> 0 THEN NRE% = Erline: EFL = 1: NR% = NRE%: BEEP: GOTO 50275
50180 COLOR NCOL%, 0
50190 FOR NR% = 1 TO 25
    50195 NAD% = NCMAX% - LEN(Page$(NR%)): IF NAD% < 0 THEN NAD% = 0
    50210 Page$(NR%) = Page$(NR%) + STRING$(NAD%, " ")
50230 NEXT NR%
50240 FOR NR% = NRMIN% TO NRMAX%
    50245 LOCATE NR%, 1: PRINT MID$(Page$(NR%), 1, NCMIN% - 1);: COLOR 2, 0: PRINT MID$(Page$(NR%), NCMIN%);
    50250 Page%(NR%) = NCMIN%: IF Dat%(NR%) = 0 THEN Dat%(NR%) = -1
50260 NEXT NR%
50270 COLOR 2, 0
50271 NR% = NRMIN%
50275 NC% = INSTR(NCMIN%, Page$(NR%), " "): IF NC% = 0 THEN NC% = NCMIN%
50276 IF EFL = 1 THEN LOCATE 25, 1: PRINT STRING$(79, " ");: COLOR 4, 0: LOCATE 25, 1: PRINT MES$; NRE%;
50280 GOSUB 50330
50290 IF Mouse% THEN
    CALL Qmouse(MF%, BT%, MXN%, MYN%)
    IF BT% = 0 THEN BC% = 0 ELSE BC% = BC% + 1
    IF BC% = 1 THEN
        IF BT% = 1 THEN G$ = CHR$(13): GOTO TTTR
        IF BT% = 2 THEN G$ = CHR$(27): GOTO TTTR
    END IF
END IF
G$ = INKEY$: IF G$ = "" GOTO 50290
TTTR: IF G$ = CHR$(27) AND NR% = 25 AND NRMIN% = 25 THEN MES$ = G$: G$ = CHR$(13): GOTO 50297
50291 IF G$ = CHR$(13) AND NR% = 25 AND NRMIN% = 25 THEN MES$ = G$: GOTO 50297
50292 IF ASC(G$) = 18 THEN ZX$ = "R": MES$ = "File to rest ->": NDATR% = Dat%(25): PGR% = Page%(25): Dat%(25) = -5: GOTO 50296
50295 IF ASC(G$) = 19 THEN ZX$ = "S": MES$ = "File to save ->": NDATR% = Dat%(25): PGR% = Page%(25): Dat%(25) = -5: GOTO 50296 ELSE GOTO 50300
50296 LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: PAGER$ = Page$(25)
Page$(25) = STRING$(79, " "): LOCATE 25, 1: PRINT Page$(25);
LOCATE 25, 1: COLOR 4, 0: PRINT MES$;: NRMINR% = NRMIN%
NCMINR% = NCMIN%: NRMAXR% = NRMAX%: NRMIN% = 25
NRMAX% = 25: NCR% = NC%: NRR% = NR%: GOTO 50271
50297 GOSUB 50700: DDDDD$ = Dat$(25, 1): Page$(25) = PAGER$: Dat%(25) = NDATR%: Page%(25) = PGR%
50298 G$ = "": NCMIN% = NCMINR%: NRMIN% = NRMINR%: NRMAX% = NRMAXR%: NC% = NCR% + 1: NR% = NRR%: G$ = MID$(Page$(NR%), NC%, 1): IF ZX$ = "" GOTO 50300
50299 IF ZX$ = "S" THEN
    LOCATE 25, 1: PRINT Page$(25);
    IF DDDDD$ = "" GOTO 50271
    OPEN DDDDD$ FOR OUTPUT AS #2
    FOR I = 1 TO 25: PRINT #2, Page$(I): NEXT I
    CLOSE #2: DDDDD$ = "": Dat$(25, 1) = ""
    GOTO 50271
ELSE
    LOCATE 25, 1: PRINT Page$(25);
    IF DDDDD$ = "" THEN: GOTO 50271
    OPEN DDDDD$ FOR INPUT AS #2
    FOR I = 1 TO 25: LINE INPUT #2, Page$(I): NEXT I
    CLOSE #2: DDDDD$ = "": Dat$(25, 1) = ""
    GOTO 50001
END IF
50300 IF EFL = 1 THEN LOCATE 25, 1: PRINT Page$(25);: EFL = 0
50310 GOTO 50430
50320 GOTO 50290
50330 'PRINT BLINKING CURSOR
50340 'COLOR 18,0
50350 LOCATE NR%, NC%: PRINT "_";
50360 COLOR 2, 0
50370 RETURN
50380 'PRINT REVERCE VIDEO
50390 COLOR 12, 0
50400 LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);
50410 COLOR 2, 0
50420 RETURN
50422 'GET RID OF BLANKS
50424 IF LEFT$(D$, 1) = " " THEN D$ = MID$(D$, 2): GOTO 50424
50426 LLL = LEN(D$): IF LLL = 0 THEN RETURN
50428 IF RIGHT$(D$, 1) = " " THEN D$ = MID$(D$, 1, LLL - 1): GOTO 50426
50429 RETURN
50430 'INTERPRETE KEYS
50440 IF G$ = CHR$(27) THEN LOCATE NR%, NC%: GOTO 50700: PRINT MID$(Page$(NR%), NC%, 1);: GOTO 50700
50450 IF G$ = CHR$(13) THEN LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: NR% = NR% + 1: NC% = NCMIN%: GOTO 50600
50460 IF LEN(G$) <> 2 GOTO 50630
50470 K$ = RIGHT$(G$, 1)
50480 IF ASC(K$) = 75 AND NC% = NCMIN% THEN SOUND 200, 1: GOTO 50610
50490 IF ASC(K$) = 77 AND NC% = NCMAX% THEN SOUND 200, 1: GOTO 50610
50500 IF ASC(K$) = 72 AND NR% = NRMIN% THEN SOUND 200, 1: GOTO 50610
50510 IF ASC(K$) = 79 THEN LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: NC% = Page%(NR%) + 1: GOSUB 50330: GOTO 50610
50520 IF ASC(K$) = 80 AND NR% = NRMAX% THEN SOUND 200, 1: GOTO 50610
50530 IF ASC(K$) = 82 THEN LOCATE NR%, NC%: GOSUB 51020: LOCATE NR%, NC%: GOTO 50610
50540 IF ASC(K$) = 83 THEN LOCATE NR%, NC%: GOSUB 50950: LOCATE NR%, NC%: GOTO 50610
50550 IF ASC(K$) = 75 THEN LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: NC% = NC% - 1
50560 IF ASC(K$) = 71 THEN LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: NC% = NCMIN%: NR% = NRMIN%: LOCATE NR%, NC%: GOTO 50610
50570 IF ASC(K$) = 72 THEN LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: NR% = NR% - 1
50580 IF ASC(K$) = 77 THEN LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: NC% = NC% + 1
50590 IF ASC(K$) = 80 THEN LOCATE NR%, NC%: PRINT MID$(Page$(NR%), NC%, 1);: NR% = NR% + 1
50594 IF LEN(G$) = 2 AND RIGHT$(G$, 1) = CHR$(117) THEN CLS: CLOSE: END
50600 IF NR% > NRMAX% THEN NR% = NRMAX%
50610 IF ASC(MID$(Page$(NR%), NC%, 1)) = 32 THEN GOSUB 50330: GOTO 50320
50620 GOSUB 50380: GOTO 50320
50630 IF ASC(G$) = 8 AND NC% = NCMIN% THEN LOCATE NR%, NC%: PRINT " ";: MID$(Page$(NR%), NC%, 1) = " ": SOUND 200, 1: GOTO 50610
50640 IF ASC(G$) = 8 THEN LOCATE NR%, NC%: PRINT " ";: MID$(Page$(NR%), NC%, 1) = " ": NC% = NC% - 1: LOCATE NR%, NC%: PRINT " ";: MID$(Page$(NR%), NC%, 1) = " ": GOSUB 50930: GOTO 50610
50650 MID$(Page$(NR%), NC%) = G$: LOCATE NR%, NC%: PRINT G$;: GOSUB 50880: NC% = NC% + 1
50660 IF NC% > NCMAX% THEN NC% = NCMAX%: SOUND 200, 1
50670 IF ASC(MID$(Page$(NR%), NC%, 1)) = 32 THEN GOSUB 50330: GOTO 50320
50680 GOSUB 50380
50690 GOTO 50320
50700 IF ASC(MID$(Page$(NR%), NC%, 1)) = 32 THEN PRINT " "; ELSE PRINT MID$(Page$(NR%), NC%, 1); 'SUBROUTINE TO EXAMINE INPUT
50705 IF NRMIN% = 25 AND ASC(G$) = 27 GOTO 50280
50710 FOR NR% = NRMIN% TO NRMAX%
    50720 NDAT% = Dat%(NR%): I% = 0: IF NDAT% = -5 THEN NDAT% = -1: F5% = -5
    50730 IMIN% = NCMIN%
    50740 D$ = Page$(NR%)
    50750 n% = INSTR(IMIN%, D$, ",")
    50760 IF n% <> 0 THEN I% = I% + 1: Dat$(NR%, I%) = MID$(D$, IMIN%, n% - IMIN%): IMIN% = n% + 1: GOTO 50750
    50770 D$ = MID$(D$, IMIN%): IF D$ <> STRING$(LEN(D$), " ") THEN I% = I% + 1: GOSUB 50422: Dat$(NR%, I%) = D$
    50780 IF NDAT% < 0 THEN Dat%(NR%) = I%: GOTO 50850
    50790 IF I% = NDAT% GOTO 50850
    50800 LOCATE 25, 1: COLOR 4, 0: PRINT USING "ERROR ON LINE ## : "; NR%;: EFL = 1: NC% = Page%(NR%) + 1: IF Page%(NR%) = NCMIN% THEN NC% = NCMIN%
    50810 IF I% < NDAT% THEN PRINT "NOT ENOUGH DATA"; ELSE PRINT "TOO MUCH DATA";
    50820 COLOR 2, 0: LOCATE NR%, NC%
    50830 IF MID$(Page$(NR%), NC%, 1) <> " " THEN GOSUB 50380 ELSE GOSUB 50330
    50840 GOTO 50290
50850 NEXT NR%
50860 IF F5% = -5 THEN F5% = 0: RETURN ELSE GOTO 51170
50870 IF NC% > NCMAX% THEN NC% = NCMAX%: RETURN
50880 IF G$ = " " AND NC% >= Page%(NR%) THEN GOTO 51130
50890 IF Page%(NR%) < NC% THEN Page%(NR%) = NC%: LOCATE NR%, NC%
50900 RETURN
50910 IF MID$(Page$(NR%), NC%, 1) <> " " THEN NC% = NC% + 1
50920 RETURN
50930 IF Page%(NR%) = NC% THEN Page%(NR%) = NC% - 1
50940 RETURN
50950 'DELETE
50960 IF MID$(Page$(NR%), NC%, 1) = " " AND NC% = Page%(NR%) + 1 THEN RETURN
50970 Page$(NR%) = LEFT$(Page$(NR%), NC% - 1) + RIGHT$(Page$(NR%), LEN(Page$(NR%)) - NC%): Page$(NR%) = Page$(NR%) + STRING$(NCMAX% - LEN(Page$(NR%)), " ")
50980 LOCATE NR%, NCMIN%: PRINT MID$(Page$(NR%), NCMIN%);: LOCATE NR%, NC%
50990 IF Page%(NR%) <= NCMIN% THEN RETURN
Page%(NR%) = Page%(NR%) - 1
51010 RETURN
51020 'INSERT
51030 COLOR 12, 0: PRINT MID$(Page$(NR%), NC%, 1);: LOCATE NR%, NC%
51040 COLOR 2, 0: INFLAG = 1
51050 G$ = INKEY$: IF G$ = "" GOTO 51050
51060 IF LEN(G$) = 2 THEN COLOR 2, 0: PRINT MID$(Page$(NR%), NC%, 1);: GOTO 50295
51070 IF ASC(G$) = 8 THEN COLOR 2, 0: PRINT MID$(Page$(NR%), NC%, 1);: GOTO 50295
51075 IF ASC(G$) = 13 THEN COLOR 2, 0: PRINT MID$(Page$(NR%), NC%, 1);: GOTO 50295
51077 IF ASC(G$) = 27 THEN COLOR 2, 0: PRINT MID$(Page$(NR%), NC%, 1);: GOTO 50295
51080 IF Page%(NR%) + 1 > NCMAX% THEN SOUND 200, 1: COLOR 2, 0: PRINT MID$(Page$(NR%), NC%, 1);: RETURN
51090 Page$(NR%) = LEFT$(Page$(NR%), NC% - 1) + G$ + RIGHT$(Page$(NR%), LEN(Page$(NR%)) - NC% + 1)
51100 Page$(NR%) = MID$(Page$(NR%), 1, NCMAX%): LOCATE NR%, NCMIN%: PRINT MID$(Page$(NR%), NCMIN%);
51110 Page%(NR%) = Page%(NR%) + 1: GOSUB 50880
51120 NC% = NC% + 1: LOCATE NR%, NC%: GOTO 51030
51130 IF Page%(NR%) = 0 THEN Page%(NR%) = NCMIN%: RETURN
51140 IF MID$(Page$(NR%), Page%(NR%), 1) = " " THEN Page%(NR%) = Page%(NR%) - 1: GOTO 51130 ELSE GOTO 51160
51150 Page%(NR%) = Page%(NR%) - 1
51160 LOCATE NR%, NC%: RETURN
51170 ERASE Page%: END SUB

SUB LOCATOR (XYlim(), XYscr(), XYout(), G$, Contr())
REDIM A1%(500), A2%(500), A3%(500), A4%(500)
WINDOW: VIEW
IF LEFT$(G$, 1) = "!" THEN
    G$ = MID$(G$, 2)
    N = INSTR(1, G$, "+")
    K1$ = (MID$(G$, N - 1, 1)): K2$ = (MID$(G$, N + 1, 1))
    SWAP K1$, K2$
    G$ = "?(" + K1$ + "+" + K2$ + ")"
    AXREV = Contr(2):
    Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
    Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
    VIEW (Xoff, Yoff)-(Xlas, Ylas)
    IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
    EXIT SUB
END IF
G$ = ""
Mouse% = 0: CALL Qmouse(Mouse%, BT%, MX%, MY%)
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
IF XYout(1) = 0 AND XYout(2) = 0 AND XYout(3) = 0 AND XYout(4) = 0 THEN
    XYout(1) = .5 * (Xmin + Xmax): XYout(2) = .5 * (Ymin + Ymax)
    XYout(3) = XYout(1): XYout(4) = XYout(2)
END IF
Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
Xc = .5 * (XYout(1) + XYout(2)): Yc = .5 * (XYout(3) + XYout(4))
IF Xc < Xmin OR Xc > Xmax THEN
    Xc = .5 * (Xmin + Xmax)
END IF
IF Yc < Ymin OR Yc > Ymax THEN
    Yc = .5 * (Ymin + Ymax)
END IF
AXREV = Contr(2): PTR% = XYout(0)
IF XYlim(8) = 0 OR XYlim(9) = 0 THEN
    XYlim(8) = 639: XYlim(9) = 479
END IF
PTR% = XYlim(9) / 16
IF ABS(PTR%) >= 30 THEN SZ% = 16 ELSE SZ% = 8
NRMINS = INT(XYscr(4) / SZ%)
NCMINS = CINT(XYscr(2) / 8) - 25: IF NCMINS < 1 THEN NCMINS = 3
Coefx = (Xlas - Xoff) / (Xmax - Xmin)
Coefy = (Yoff - Ylas) / (Ymax - Ymin)
Xshift = Xoff - Coefx * Xmin
Yshift = Ylas - Coefy * Ymin
IF AXREV THEN Coefy = -Coefy: Yshift = Yoff - Coefy * Ymin
MXmin = 1: MXmax = XYlim(8): MYmin = 1: MYmax = XYlim(9)
IF MXmax = 0 OR MYmax = 0 THEN
    MXmax = 639: MYmax = 479
END IF
Xsmin = (MXmin - Xshift) / Coefx: Xsmax = (MXmax - Xshift) / Coefx
Ysmin = (MYmin - Yshift) / Coefy: Ysmax = (MYmax - Yshift) / Coefy
DPX = 1 / Coefx: DPY = 1 / Coefy: IF AXREV = 0 THEN SWAP Ysmin, Ysmax
DPXR = DPX: DPYR = DPY: CL% = XYout(7): IF CL% = 0 THEN CL% = 3
DLx = XYout(5): DLy = XYout(6)
IF DLx = 0 THEN DLx = 20 * DPX
IF DLy = 0 THEN DLy = 16 * DPY
XI = Xc: YI = Yc: FIRST = 0: MD = 0: FLR = 0
XLmin = Xsmin: XLmax = Xsmax: YLmin = Ysmin: YLmax = Ysmax
LXmin = MXmin: LXmax = MXmax: LYmin = MYmin: LYmax = MYmax
IF Mouse% = 0 THEN
    IF Ysmin < Ymin THEN Ysmin = Ymin: YLmin = Ymin
    IF Ysmax > Ymax THEN Ysmax = Ymax: YLmax = Ymax
    IF Xsmin < Xmin THEN Xsmin = Xmin: XLmin = Xmin
    IF Xsmax > Xmax THEN Xsmax = Xmax: XLmax = Xmax
END IF
Dx = DPX: Dy = DPY
MXN% = XI * Coefx + Xshift: MYN% = YI * Coefy + Yshift
1300 X1 = XI: Y1 = YI: X2 = X1: Y2 = Y1
1301 IF MD = 0 THEN X1 = XI: Y1 = YI: X2 = X1: Y2 = Y1
IF Mouse% <> 0 THEN
    MX% = XI * Coefx + Xshift: MY% = YI * Coefy + Yshift
    M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
    M% = 1: CALL Qmouse(M%, BT%, MX%, MY%)
END IF
1310 XG1 = X1: XG2 = X2: IF XG1 > XG2 THEN SWAP XG1, XG2
YG1 = Y1: YG2 = Y2: IF YG1 > YG2 THEN SWAP YG1, YG2
IF MD = 0 THEN
    XG1 = XG1 - DPXR: XG2 = XG2 + DPXR
    YG1 = YG1 - DPYR: YG2 = YG2 + DPYR
END IF
IF XG1 < XLmin OR XG1 > XLmax GOTO 1311
IF XG2 < XLmin OR XG2 > XLmax GOTO 1311
IF YG1 < YLmin OR YG1 > YLmax GOTO 1311
IF YG2 < YLmin OR YG2 > YLmax GOTO 1311
GOTO 1312
1311 X1 = X1R: Y1 = Y1R: X2 = X2R: Y2 = Y2R: GOTO 1310
1312 IF Mouse% = 0 OR MD = 1 THEN
    IF NOT AXREV AND MD = 0 THEN SWAP YG1, YG2
    IF AXREV THEN SWAP YG1, YG2
    XP1% = XG1 * Coefx + Xshift: XP2% = XG2 * Coefx + Xshift
    YP1% = YG1 * Coefy + Yshift: YP2% = YG2 * Coefy + Yshift
    GET (XP1%, YP1%)-(XP2%, YP1%), A1%(0)
    GET (XP2%, YP2%)-(XP2%, YP1%), A2%(0)
    GET (XP1%, YP2%)-(XP2%, YP2%), A3%(0)
    GET (XP1%, YP1%)-(XP1%, YP2%), A4%(0)
    MDP = MD
    LINE (XP1%, YP1%)-(XP2%, YP2%), CL%, B
    IF NOT AXREV AND MD = 0 THEN SWAP YG1, YG2
END IF
Xc = .5 * (XG1 + XG2): Yc = .5 * (YG1 + YG2)
IF PTR% < 0 THEN
    LOCATE NRMINS, NCMINS: PRINT USING "X:#.####^^^^"; Xc
    LOCATE NRMINS + 1, NCMINS: PRINT USING "Y:#.####^^^^"; Yc;
END IF
IF PTR% > 0 THEN
    LOCATE NRMINS, NCMINS: PRINT USING "X:#.####^^^^ Y:#.####^^^^"; Xc; Yc;
END IF
IF Mouse% <> 0 AND MFL% THEN
    IF MD = 0 THEN
        Xm = Xc: Ym = Yc
    ELSE
        IF FIRST = 0 THEN Xm = X2: Ym = Y2 ELSE Xm = X1: Ym = Y1
    END IF
    MX = Xm * Coefx + Xshift: MY = Ym * Coefy + Yshift
    MX% = MX: MY% = MY
    M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
END IF
IF MD = 0 GOTO 1850
1820 IF FIRST = 0 THEN
    X2D% = X2 * Coefx + Xshift: Y2D% = Y2 * Coefy + Yshift
    PSET (X2D%, Y2D%), 12
ELSE
    X1D% = X1 * Coefx + Xshift: Y1D% = Y1 * Coefy + Yshift
    PSET (X1D%, Y1D%), 12
END IF
1850 XN1 = X1: YN1 = Y1: XN2 = X2: YN2 = Y2
1900 MFL% = 0: G$ = INKEY$
IF G$ <> "" THEN
    GOTO 1902
END IF
IF Mouse% <> 0 THEN
    M% = 3: CALL Qmouse(M%, BT%, MXN%, MYN%): FL% = 0
    IF BT% = 0 THEN
        BC = 0
    ELSE
        lpbt: BC = BC + 1
        M% = 3: CALL Qmouse(M%, Bx%, MXN%, MYN%)
        IF Bx% <> 0 GOTO lpbt
    END IF
    IF BC <> 0 THEN
        IF BT% = 1 THEN
            IF MD = 1 THEN
                G$ = "S"
                GOTO 1902
            ELSE
                G$ = CHR$(13)
                GOTO 1902
            END IF
        END IF
        IF BT% = 2 THEN G$ = "-": GOTO 1902
        IF BT% = 3 THEN G$ = "+": GOTO 1902
    END IF
    IF MXN% < LXmin THEN MXN% = LXmin: FL% = 1
    IF MXN% > LXmax THEN MXN% = LXmax: FL% = 1
    IF MYN% < LYmin THEN MYN% = LYmin: FL% = 1
    IF MYN% > LYmax THEN MYN% = LYmax: FL% = 1
    IF FL% = 1 THEN M% = 4: CALL Qmouse(M%, BT%, MXN%, MYN%): MX% = MXN%: MY% = MYN%: GOTO CC
    IF MXN% = MX% AND MYN% = MY% THEN
        GOTO 1900
    ELSE
        MX% = MXN%: MY% = MYN%
    END IF
    CC: Xm = (MX% - Xshift) / Coefx
    Ym = (MY% - Yshift) / Coefy
    IF MD = 0 THEN
        XN1 = Xm: YN1 = Ym
        XN2 = XN1: YN2 = YN1
        GOTO 3500
    ELSE
        IF FIRST = 0 THEN XN2 = Xm: YN2 = Ym ELSE XN1 = Xm: YN1 = Ym
        GOTO 3500
    END IF
END IF
GOTO 1900
IF G$ = "" GOTO 1900
1902 IF LEN(G$) = 2 GOTO 2000
1903 G = ASC(G$): IF G > 90 THEN G = G - 32: G$ = CHR$(G)
1904 IF ASC(G$) <> 43 AND ASC(G$) <> 45 THEN FLR = 1: GOTO 3500
2000 G = ASC(RIGHT$(G$, 1))
IF ASC(RIGHT$(G$, 1)) = 117 THEN GOTO 4010
2200 IF ASC(G$) <> 43 THEN GOTO 2410
2300 IF FIRST = 1 THEN
    FIRST = 0
    IF Mouse% <> 0 THEN
        MX% = X2 * Coefx + Xshift: MY% = Y2 * Coefy + Yshift
        M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
    END IF
    GOTO 3500
END IF
2400 IF FIRST = 0 THEN
    FIRST = 1
    IF Mouse% <> 0 THEN
        MX% = X1 * Coefx + Xshift: MY% = Y1 * Coefy + Yshift
        M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
    END IF
    GOTO 3500
END IF
2410 IF G = 82 THEN SWAP DPX, DLx: SWAP DPY, DLy
2443 IF G <> 45 GOTO 2470
2445 IF MD = 0 THEN
    IF Mouse% <> 0 THEN
        IF MXN% < Xoff OR MXN% > Xlas GOTO 1900
        IF MYN% < Yoff OR MYN% > Ylas GOTO 1900
    END IF
    MD = 1
    XLmin = Xmin: XLmax = Xmax
    YLmin = Ymin: YLmax = Ymax
    LXmin = Xoff + 1: LXmax = Xlas - 1
    LYmin = Yoff + 1: LYmax = Ylas - 1
    IF Mouse% = 0 GOTO 3500 ELSE M% = 2: CALL Qmouse(M%, BT%, MX%, MY%): GOTO 3810
ELSE
    MD = 0
    XLmin = Xsmin: XLmax = Xsmax
    YLmin = Ysmin: YLmax = Ysmax
    LXmin = MXmin: LXmax = MXmax
    LYmin = MYmin: LYmax = MYmax
    IF Mouse% = 0 GOTO 3500 ELSE SO% = 1: GOTO 3501
END IF
2446 IF MD = 1 THEN MD = 0: GOTO 3500
2470 MFL% = 1: IF FIRST = 0 GOTO 3480
2500 IF G = 72 THEN YN1 = Y1 - DLy: GOTO 3500
2700 IF G = 73 THEN XN1 = X1 + DLx: YN1 = Y1 - DLy: GOTO 3500
2900 IF G = 77 THEN XN1 = X1 + DLx: GOTO 3500
3100 IF G = 81 THEN XN1 = X1 + DLx: YN1 = Y1 + DLy: GOTO 3500
3300 IF G = 80 THEN YN1 = Y1 + DLy: GOTO 3500
3410 IF G = 79 THEN XN1 = X1 - DLx: YN1 = Y1 + DLy: GOTO 3500
3430 IF G = 75 THEN XN1 = X1 - DLx: GOTO 3500
3450 IF G = 71 THEN XN1 = X1 - DLx: YN1 = Y1 - DLy: GOTO 3500
3470 GOTO 1900
3480 IF G = 72 THEN YN2 = Y2 - DLy: GOTO 3500
3482 IF G = 73 THEN XN2 = X2 + DLx: YN2 = Y2 - DLy: GOTO 3500
3484 IF G = 77 THEN XN2 = X2 + DLx: GOTO 3500
3486 IF G = 81 THEN XN2 = X2 + DLx: YN2 = Y2 + DLy: GOTO 3500
3488 IF G = 80 THEN YN2 = Y2 + DLy: GOTO 3500
3490 IF G = 79 THEN XN2 = X2 - DLx: YN2 = Y2 + DLy: GOTO 3500
3492 IF G = 75 THEN XN2 = X2 - DLx: GOTO 3500
3494 IF G = 71 THEN XN2 = X2 - DLx: YN2 = Y2 - DLy: GOTO 3500
3500 '
IF Mouse% = 0 OR MD = 1 THEN
    3501 LINE (XP1%, YP1%)-(XP2%, YP2%), 0, B
    IF NOT AXREV AND MDP = 0 THEN SWAP YG1, YG2
    XP1% = XG1 * Coefx + Xshift: XP2% = XG2 * Coefx + Xshift
    YP1% = YG1 * Coefy + Yshift: YP2% = YG2 * Coefy + Yshift
    3510 PUT (XP1%, YP1%), A1%(0), OR
    3600 PUT (XP2%, YP2%), A2%(0), OR
    3700 PUT (XP1%, YP2%), A3%(0), OR
    3800 PUT (XP1%, YP2%), A4%(0), OR
    IF NOT AXREV AND MDP = 0 THEN SWAP YG1, YG2
    IF SO% = 1 THEN M% = 1: CALL Qmouse(M%, BT%, MX%, MY%): SO% = 0
END IF
3805 IF FLR = 1 THEN
    IF ABS(XG2 - XG1) > ABS(2.1 * DPXR) OR ABS(YG2 - YG1) > ABS(2.1 * DPYR) THEN
        XYout(1) = XG1: XYout(2) = XG2
        XYout(3) = YG1: XYout(4) = YG2
    ELSE
        XYout(1) = .5 * (XG1 + XG2): XYout(2) = XYout(1)
        XYout(3) = .5 * (YG1 + YG2): XYout(4) = XYout(3)
    END IF
    GOTO 4010
END IF
3810 IF XN1 < XLmin OR XN1 > XLmax GOTO 4000
3820 IF XN2 < XLmin OR XN2 > XLmax GOTO 4000
3830 IF YN1 < Ysmin OR YN1 > YLmax GOTO 4000
3840 IF YN2 < Ysmin OR YN2 > YLmax GOTO 4000
3845 X1R = X1: Y1R = Y1: X2R = X2: Y2R = Y2
3900 X1 = XN1: Y1 = YN1: X2 = XN2: Y2 = YN2
1306 IF MD = 0 AND FIRST = 0 THEN X1 = X2: Y1 = Y2: XN1 = XN2: YN1 = YN2
1307 IF MD = 0 AND FIRST = 1 THEN X2 = X1: Y2 = Y1: XN2 = XN1: YN2 = YN1
4000 GOTO 1310
4010
IF PTR% < 0 THEN
    LOCATE NRMINS, NCMINS: PRINT STRING$(14, " ");
    LOCATE NRMINS + 1, NCMINS: PRINT STRING$(14, " ");
END IF
IF PTR% > 0 THEN
    LOCATE NRMINS, NCMINS: PRINT STRING$(25, " ");
END IF
COLOR 15
IF Mouse% <> 0 AND MD = 0 THEN M% = 2: CALL Qmouse(M%, BT%, MX%, MY%)
IF MD = 0 THEN XYout(1) = XYout(2): XYout(3) = XYout(4)
'IF G$ = "S" OR G$ = "+" THEN G$ = CHR$(13)
VIEW (Xoff, Yoff)-(Xlas, Ylas)
IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
IF XYout(1) > XYout(2) THEN
    SWAP XYout(1), XYout(2)
END IF
IF XYout(3) > XYout(4) THEN
    SWAP XYout(3), XYout(4)
END IF
ERASE A1%, A2%, A3%, A4%
END SUB

SUB DRAG (XYlim(), XYscr(), XYout(), G$, Contr(), A0(), DEL%)
REDIM A1%(500), A2%(500), A3%(500), A4%(500)
'WINDOW: VIEW
G$ = ""
Mouse% = 0: CALL Qmouse(Mouse%, BT%, MX%, MY%)
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
IF XYout(1) = 0 AND XYout(2) = 0 AND XYout(3) = 0 AND XYout(4) = 0 THEN
    XYout(1) = .5 * (Xmin + Xmax): XYout(2) = .5 * (Ymin + Ymax)
    XYout(3) = XYout(1): XYout(4) = XYout(2)
END IF
Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
Xc = .5 * (XYout(1) + XYout(2)): Yc = .5 * (XYout(3) + XYout(4))
IF Xc < Xmin OR Xc > Xmax THEN
    Xc = .5 * (Xmin + Xmax)
END IF
IF Yc < Ymin OR Yc > Ymax THEN
    Yc = .5 * (Ymin + Ymax)
END IF
AXREV = Contr(2): PTR% = XYout(0)
IF XYlim(8) = 0 OR XYlim(9) = 0 THEN
    XYlim(8) = 639: XYlim(9) = 479
END IF
PTR% = XYlim(9) / 16
IF ABS(PTR%) >= 30 THEN SZ% = 16 ELSE SZ% = 8
NRMINS = CINT(XYscr(4) / SZ%)
NCMINS = CINT(XYscr(2) / 8) - 25: IF NCMINS < 1 THEN NCMINS = 3
Coefx = (Xlas - Xoff) / (Xmax - Xmin)
Coefy = (Yoff - Ylas) / (Ymax - Ymin)
Xshift = Xoff - Coefx * Xmin
Yshift = Ylas - Coefy * Ymin
IF AXREV THEN Coefy = -Coefy: Yshift = Yoff - Coefy * Ymin
MXmin = 1 + DEL%: MXmax = XYlim(8) - DEL% - 1: MYmin = 1 + DEL%: MYmax = XYlim(9) - DEL% - 1
IF MXmax = 0 OR MYmax = 0 THEN
    MXmax = 639: MYmax = 479
END IF
Xsmin = (MXmin - Xshift) / Coefx: Xsmax = (MXmax - Xshift) / Coefx
Ysmin = (MYmin - Yshift) / Coefy: Ysmax = (MYmax - Yshift) / Coefy
DPX = 1 / Coefx: DPY = 1 / Coefy: IF AXREV = 0 THEN SWAP Ysmin, Ysmax
DPXR = DPX: DPYR = DPY: CL% = XYout(7): IF CL% = 0 THEN CL% = 3
DLx = XYout(5): DLy = XYout(6)
IF DLx = 0 THEN DLx = 20 * DPX
IF DLy = 0 THEN DLy = 16 * DPY
XI = Xc: YI = Yc: FIRST = 0: MD = 0: FLR = 0
XLmin = Xsmin: XLmax = Xsmax: YLmin = Ysmin: YLmax = Ysmax
LXmin = MXmin: LXmax = MXmax: LYmin = MYmin: LYmax = MYmax
IF Mouse% = 0 THEN
    IF Ysmin < Ymin THEN Ysmin = Ymin: YLmin = Ymin
    IF Ysmax > Ymax THEN Ysmax = Ymax: YLmax = Ymax
    IF Xsmin < Xmin THEN Xsmin = Xmin: XLmin = Xmin
    IF Xsmax > Xmax THEN Xsmax = Xmax: XLmax = Xmax
END IF
Dx = DPX: Dy = DPY
MXR% = XI * Coefx + Xshift: MYR% = YI * Coefy + Yshift
PUT (MXR% - DEL%, MYR% - DEL%), A0(), XOR
1300 X1 = XI: Y1 = YI: X2 = X1: Y2 = Y1
1301 IF MD = 0 THEN X1 = XI: Y1 = YI: X2 = X1: Y2 = Y1
IF Mouse% <> 0 THEN
    MX% = XI * Coefx + Xshift: MY% = YI * Coefy + Yshift
    M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
    M% = 1: CALL Qmouse(M%, BT%, MX%, MY%)
END IF
1310 XG1 = X1: XG2 = X2: IF XG1 > XG2 THEN SWAP XG1, XG2
YG1 = Y1: YG2 = Y2: IF YG1 > YG2 THEN SWAP YG1, YG2
IF MD = 0 THEN
    XG1 = XG1 - DPXR: XG2 = XG2 + DPXR
    YG1 = YG1 - DPYR: YG2 = YG2 + DPYR
END IF
IF XG1 < XLmin OR XG1 > XLmax GOTO 1311
IF XG2 < XLmin OR XG2 > XLmax GOTO 1311
IF YG1 < YLmin OR YG1 > YLmax GOTO 1311
IF YG2 < YLmin OR YG2 > YLmax GOTO 1311
GOTO 1312
1311 X1 = X1R: Y1 = Y1R: X2 = X2R: Y2 = Y2R: GOTO 1310
1312 IF Mouse% = 0 OR MD = 1 THEN
    IF NOT AXREV AND MD = 0 THEN SWAP YG1, YG2
    IF AXREV THEN SWAP YG1, YG2
    XP1% = XG1 * Coefx + Xshift: XP2% = XG2 * Coefx + Xshift
    YP1% = YG1 * Coefy + Yshift: YP2% = YG2 * Coefy + Yshift
    GET (XP1%, YP1%)-(XP2%, YP1%), A1%(0)
    GET (XP2%, YP2%)-(XP2%, YP1%), A2%(0)
    GET (XP1%, YP2%)-(XP2%, YP2%), A3%(0)
    GET (XP1%, YP1%)-(XP1%, YP2%), A4%(0)
    MDP = MD
    LINE (XP1%, YP1%)-(XP2%, YP2%), CL%, B
    IF NOT AXREV AND MD = 0 THEN SWAP YG1, YG2
END IF
Xc = .5 * (XG1 + XG2): Yc = .5 * (YG1 + YG2)
IF PTR% < 0 THEN
    LOCATE NRMINS, NCMINS: PRINT USING "X:#.####^^^^"; Xc
    LOCATE NRMINS + 1, NCMINS: PRINT USING "Y:#.####^^^^"; Yc;
END IF
IF PTR% > 0 THEN
    LOCATE NRMINS, NCMINS: PRINT USING "X:#.####^^^^ Y:#.####^^^^"; Xc; Yc;
END IF
'MXR% = Xc * Coefx + Xshift: MYR% = Yc * Coefy + Yshift
'IF MXR% < MXmin THEN MXR% = MXmin: IF MYR% < MYmin THEN MYR% = MYmin
'IF MXR% > MXmax THEN MXR% = MXmax: IF MYR% > MYmax THEN MYR% = MYmax
IF Mouse% <> 0 AND MFL% THEN
    IF MD = 0 THEN
        Xm = Xc: Ym = Yc
    ELSE
        IF FIRST = 0 THEN Xm = X2: Ym = Y2 ELSE Xm = X1: Ym = Y1
    END IF
    MX = Xm * Coefx + Xshift: MY = Ym * Coefy + Yshift
    MX% = MX: MY% = MY
    M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
END IF
IF MD = 0 GOTO 1850
1820 IF FIRST = 0 THEN
    X2D% = X2 * Coefx + Xshift: Y2D% = Y2 * Coefy + Yshift
    PSET (X2D%, Y2D%), 12
ELSE
    X1D% = X1 * Coefx + Xshift: Y1D% = Y1 * Coefy + Yshift
    PSET (X1D%, Y1D%), 12
END IF
1850 XN1 = X1: YN1 = Y1: XN2 = X2: YN2 = Y2
1900 MFL% = 0: G$ = INKEY$
IF G$ <> "" THEN
    GOTO 1902
END IF
IF Mouse% <> 0 THEN
    M% = 3: CALL Qmouse(M%, BT%, MXN%, MYN%): FL% = 0
    IF BT% = 0 THEN
        BC = 0
    ELSE
        lpbt: BC = BC + 1
        M% = 3: CALL Qmouse(M%, Bx%, MXN%, MYN%)
        IF Bx% <> 0 GOTO lpbt
    END IF
    IF BC <> 0 THEN
        IF BT% = 1 THEN
            IF MD = 1 THEN
                G$ = "S"
                GOTO 1902
            ELSE
                G$ = CHR$(13)
                GOTO 1902
            END IF
        END IF
        IF BT% = 2 THEN G$ = "-": GOTO 1902
        IF BT% = 3 THEN G$ = "+": GOTO 1902
    END IF
    IF MXN% < LXmin THEN MXN% = LXmin: FL% = 1
    IF MXN% > LXmax THEN MXN% = LXmax: FL% = 1
    IF MYN% < LYmin THEN MYN% = LYmin: FL% = 1
    IF MYN% > LYmax THEN MYN% = LYmax: FL% = 1
    IF FL% = 1 THEN M% = 4: CALL Qmouse(M%, BT%, MXN%, MYN%): MX% = MXN%: MY% = MYN%: GOTO CC
    IF MXN% = MX% AND MYN% = MY% THEN
        GOTO 1900
    ELSE
        MX% = MXN%: MY% = MYN%
    END IF
    CC: Xm = (MX% - Xshift) / Coefx
    Ym = (MY% - Yshift) / Coefy
    IF MD = 0 THEN
        XN1 = Xm: YN1 = Ym
        XN2 = XN1: YN2 = YN1
        GOTO 3500
    ELSE
        IF FIRST = 0 THEN XN2 = Xm: YN2 = Ym ELSE XN1 = Xm: YN1 = Ym
        GOTO 3500
    END IF
END IF
GOTO 1900
IF G$ = "" GOTO 1900
1902 IF LEN(G$) = 2 GOTO 2000
1903 G = ASC(G$): IF G > 90 THEN G = G - 32: G$ = CHR$(G)
1904 IF ASC(G$) <> 43 AND ASC(G$) <> 45 THEN FLR = 1: GOTO 3500
2000 G = ASC(RIGHT$(G$, 1))
IF ASC(RIGHT$(G$, 1)) = 117 THEN GOTO 4010
2200 IF ASC(G$) <> 43 THEN GOTO 2410
2300 IF FIRST = 1 THEN
    FIRST = 0
    IF Mouse% <> 0 THEN
        MX% = X2 * Coefx + Xshift: MY% = Y2 * Coefy + Yshift
        M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
    END IF
    GOTO 3500
END IF
2400 IF FIRST = 0 THEN
    FIRST = 1
    IF Mouse% <> 0 THEN
        MX% = X1 * Coefx + Xshift: MY% = Y1 * Coefy + Yshift
        M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
    END IF
    GOTO 3500
END IF
2410 IF G = 82 THEN SWAP DPX, DLx: SWAP DPY, DLy
2443 IF G <> 45 GOTO 2470
2445 IF MD = 0 THEN
    IF Mouse% <> 0 THEN
        IF MXN% < Xoff OR MXN% > Xlas GOTO 1900
        IF MYN% < Yoff OR MYN% > Ylas GOTO 1900
    END IF
    MD = 1
    XLmin = Xmin: XLmax = Xmax
    YLmin = Ymin: YLmax = Ymax
    LXmin = Xoff + 1: LXmax = Xlas - 1
    LYmin = Yoff + 1: LYmax = Ylas - 1
    IF Mouse% = 0 GOTO 3500 ELSE M% = 2: CALL Qmouse(M%, BT%, MX%, MY%): GOTO 3810
ELSE
    MD = 0
    XLmin = Xsmin: XLmax = Xsmax
    YLmin = Ysmin: YLmax = Ysmax
    LXmin = MXmin: LXmax = MXmax
    LYmin = MYmin: LYmax = MYmax
    IF Mouse% = 0 GOTO 3500 ELSE SO% = 1: GOTO 3501
END IF
2446 IF MD = 1 THEN MD = 0: GOTO 3500
2470 MFL% = 1: IF FIRST = 0 GOTO 3480
2500 IF G = 72 THEN YN1 = Y1 - DLy: GOTO 3500
2700 IF G = 73 THEN XN1 = X1 + DLx: YN1 = Y1 - DLy: GOTO 3500
2900 IF G = 77 THEN XN1 = X1 + DLx: GOTO 3500
3100 IF G = 81 THEN XN1 = X1 + DLx: YN1 = Y1 + DLy: GOTO 3500
3300 IF G = 80 THEN YN1 = Y1 + DLy: GOTO 3500
3410 IF G = 79 THEN XN1 = X1 - DLx: YN1 = Y1 + DLy: GOTO 3500
3430 IF G = 75 THEN XN1 = X1 - DLx: GOTO 3500
3450 IF G = 71 THEN XN1 = X1 - DLx: YN1 = Y1 - DLy: GOTO 3500
3470 GOTO 1900
3480 IF G = 72 THEN YN2 = Y2 - DLy: GOTO 3500
3482 IF G = 73 THEN XN2 = X2 + DLx: YN2 = Y2 - DLy: GOTO 3500
3484 IF G = 77 THEN XN2 = X2 + DLx: GOTO 3500
3486 IF G = 81 THEN XN2 = X2 + DLx: YN2 = Y2 + DLy: GOTO 3500
3488 IF G = 80 THEN YN2 = Y2 + DLy: GOTO 3500
3490 IF G = 79 THEN XN2 = X2 - DLx: YN2 = Y2 + DLy: GOTO 3500
3492 IF G = 75 THEN XN2 = X2 - DLx: GOTO 3500
3494 IF G = 71 THEN XN2 = X2 - DLx: YN2 = Y2 - DLy: GOTO 3500
3500 '
IF Mouse% = 0 OR MD = 1 THEN
    3501 LINE (XP1%, YP1%)-(XP2%, YP2%), 0, B
    IF NOT AXREV AND MDP = 0 THEN SWAP YG1, YG2
    XP1% = XG1 * Coefx + Xshift: XP2% = XG2 * Coefx + Xshift
    YP1% = YG1 * Coefy + Yshift: YP2% = YG2 * Coefy + Yshift
    3510 PUT (XP1%, YP1%), A1%(0), OR
    3600 PUT (XP2%, YP2%), A2%(0), OR
    3700 PUT (XP1%, YP2%), A3%(0), OR
    3800 PUT (XP1%, YP2%), A4%(0), OR
    IF NOT AXREV AND MDP = 0 THEN SWAP YG1, YG2
    IF SO% = 1 THEN M% = 1: CALL Qmouse(M%, BT%, MX%, MY%): SO% = 0
END IF
3805 IF FLR = 1 THEN
    IF ABS(XG2 - XG1) > ABS(2.1 * DPXR) OR ABS(YG2 - YG1) > ABS(2.1 * DPYR) THEN
        XYout(1) = XG1: XYout(2) = XG2
        XYout(3) = YG1: XYout(4) = YG2
    ELSE
        XYout(1) = Xc: XYout(2) = XYout(1)
        XYout(3) = Yc: XYout(4) = XYout(3)
        XYout(1) = (MXC% - Xshift) / Coefx: XYout(2) = XYout(1)
        XYout(3) = (MYC% - Yshift) / Coefy: XYout(4) = XYout(3)
        XYout(8) = NXC%
        XYout(9) = MYC%
    END IF
    GOTO 4010
END IF
3810 IF XN1 < XLmin OR XN1 > XLmax GOTO 4000
3820 IF XN2 < XLmin OR XN2 > XLmax GOTO 4000
3830 IF YN1 < Ysmin OR YN1 > YLmax GOTO 4000
3840 IF YN2 < Ysmin OR YN2 > YLmax GOTO 4000
3845 X1R = X1: Y1R = Y1: X2R = X2: Y2R = Y2
3900 X1 = XN1: Y1 = YN1: X2 = XN2: Y2 = YN2
1306 IF MD = 0 AND FIRST = 0 THEN X1 = X2: Y1 = Y2: XN1 = XN2: YN1 = YN2
1307 IF MD = 0 AND FIRST = 1 THEN X2 = X1: Y2 = Y1: XN2 = XN1: YN2 = YN1
MXC% = 0.5 * Coefx * (X1 + X2) + Xshift
MYC% = 0.5 * Coefy * (Y1 + Y2) + Yshift
IF MXC% < MXmin THEN MXC% = MXmin: IF MYC% < MYmin THEN MYC% = MYmin
IF MXC% > MXmax THEN MXC% = MXmax: IF MYC% > MYmax THEN MYC% = MYmax
IF MXR% <> MXC% OR MYR% <> MYC% THEN
    PUT (MXR% - DEL%, MYR% - DEL%), A0(), XOR
    PUT (MXC% - DEL%, MYC% - DEL%), A0(), XOR
    MXR% = MXC%: MYR% = MYC%
END IF
4000 GOTO 1310
4010
IF PTR% < 0 THEN
    LOCATE NRMINS, NCMINS: PRINT STRING$(14, " ");
    LOCATE NRMINS + 1, NCMINS: PRINT STRING$(14, " ");
END IF
IF PTR% > 0 THEN
    LOCATE NRMINS, NCMINS: PRINT STRING$(25, " ");
END IF
COLOR 15
IF Mouse% <> 0 AND MD = 0 THEN M% = 2: CALL Qmouse(M%, BT%, MX%, MY%)
IF MD = 0 THEN XYout(1) = XYout(2): XYout(3) = XYout(4)
'IF G$ = "S" OR G$ = "+" THEN G$ = CHR$(13)
VIEW (Xoff, Yoff)-(Xlas, Ylas)
IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
IF XYout(1) > XYout(2) THEN
    SWAP XYout(1), XYout(2)
END IF
IF XYout(3) > XYout(4) THEN
    SWAP XYout(3), XYout(4)
END IF
ERASE A1%, A2%, A3%, A4%
END SUB

SUB MakeMenu (XYlim(), XYscr(), XYout(), Contr(), ITEMS$(), NITEMS, Boxes(), OPER, OPTIONS(), G$) STATIC
SHARED XSPMODE, YSPMODE, SPMODE, WDMODE, XDMODE, CMIN%, ROIN%
COLFR% = 11: COLHIGH% = 12: COLLET% = 10
IF NITEMS = 0 THEN EXIT SUB
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
AXREV = Contr(2): CLEN = 16: IF Boxes(BOX29) <> 0 THEN CLEN = Boxes(BOX29)
Coefx = (Xlas - Xoff) / (Xmax - Xmin)
Coefy = (Yoff - Ylas) / (Ymax - Ymin)
Xshift = Xoff - Coefx * Xmin
Yshift = Ylas - Coefy * Ymin
IF AXREV THEN Coefy = -Coefy: Yshift = Yoff - Coefy * Ymin
IF OPER = 0 THEN
    IF AUT = 0 THEN EXIT SUB
    IF Boxes(BOX29) = 0 THEN WINDOW: VIEW
    VIEW (Xlas + 24, 0)-(XSPMODE - 1, Ylas - 16): CLS: VIEW
    Rmin% = INT(Yoff / CLEN) + 1: Rmax% = INT(Ylas / CLEN) + 1: RIN% = Rmin%
    CMAX% = 0
    FOR I% = 1 TO NITEMS
        IF LEN(ITEMS$(I%)) > CMAX% THEN CMAX% = LEN(ITEMS$(I%))
    NEXT I%
    COLOR COLLET%
    CMIN% = XDMODE - 1 - CMAX%
    CMAX% = XDMODE - 1
    CIN% = (CMIN% - 2) * 8 + 2: CFI% = CMAX% * 8 - 4: RINR% = RIN% - 1
    FOR I% = 1 TO NITEMS
        LIN% = RIN% * CLEN
        IF OPTIONS(I%) = 1 THEN COLOR COLHIGH%
        LOCATE RIN%, CMIN%: PRINT ITEMS$(I%)
        RIN% = RIN% + 1
        COLOR COLLET%
    NEXT I%
    RIN% = RINR%
    RIN% = RIN% + 1
    FOR I% = 1 TO NITEMS
        LIN% = RIN% * CLEN
        LINE (CIN%, LIN%)-(CFI%, LIN%), COLFR%: Boxes(I%) = LIN%
        RIN% = RIN% + 1
    NEXT I%
    ROIN% = RIN% + 1
    RIN% = RIN% - 1
    LINE (CIN%, RINR% * CLEN)-(CFI%, RIN% * CLEN), COLFR%, B
    Boxes(NITEMS + 1) = CIN%: Boxes(NITEMS + 2) = CFI%
    Boxes(0) = Boxes(1) - CLEN
    VIEW (Xoff, Yoff)-(Xlas, Ylas)
    IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
    EXIT SUB
ELSE
    N = INSTR(1, G$, "?")
    IF N <> 0 THEN
        G$ = MID$(G$, 2)
        N = INSTR(1, G$, "+")
        IF N <> 0 THEN
            K1$ = (MID$(G$, N - 1, 1)): K2$ = (MID$(G$, N + 1, 1))
            ITEMS$(0) = K1$
            G$ = "!(" + K1$ + "+" + K2$ + ")"
            IF VAL(ITEMS$(0)) = 0 THEN
                ITEMS$(0) = STR$(10 + ASC(K1$) - 65)
            END IF
            EXIT SUB
        END IF
    END IF
    Xc = .5 * (XYout(1) + XYout(2)): Yc = .5 * (XYout(3) + XYout(4))
    IF Boxes(BOX29) = 0 THEN Xc = Xc * Coefx + Xshift: Yc = Yc * Coefy + Yshift
    CIN% = Boxes(NITEMS + 1): CFI% = Boxes(NITEMS + 2)
    CEN% = .5 * (CIN% + CFI%)
    IF Boxes(BOX29) = 0 THEN
        WINDOW: VIEW
    END IF
    ITEMS$(0) = ""
    FOR I% = 1 TO NITEMS
        RI% = Boxes(I% - 1): RF% = Boxes(I%)
        IF Xc > CIN% AND Xc < CFI% AND Yc > RI% AND Yc < RF% THEN
            Xcc = 0.5 * (CIN% + CFI%)
            IF Xc < Xcc THEN
                ITEMS$(0) = STR$(-I%)
            ELSE
                ITEMS$(0) = STR$(I%)
            END IF
            CMIN% = CIN% / 8 + 2
            ROW% = RF% / CLEN
            LOCATE ROW%, CMIN%: COLOR 14: PRINT ITEMS$(I%);
            LINE (CIN%, RI%)-(CFI%, RF%), 15, B
        END IF
    NEXT I%
    IF Boxes(BOX29) = 0 THEN
        VIEW (Xoff, Yoff)-(Xlas, Ylas)
        IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
    END IF
END IF

END SUB

SUB MakeNext (Dfile$, NEWFILE)
N1 = INSTR(1, Dfile$, "_")
IF N1 = 0 THEN
    N1 = INSTR(1, Dfile$, "-")
    IF N1 = 0 THEN
        N1 = INSTR(1, Dfile$, "+")
    END IF
END IF
N2 = INSTR(1, Dfile$, ".")
IF N2 - N1 - 1 > 2 THEN
    L = N2 - N1 - 1
    VS$ = MID$(Dfile$, N1 + 1, L)
    VLU = VAL(VS$)
    IF VLU + 1 < 10 ^ L THEN
        VS$ = MID$(STR$(VLU + 1), 2)
        LV% = L - LEN(VS$)
        FOR K% = 1 TO LV%
            VS$ = "0" + VS$
        NEXT
        VS$ = MID$(Dfile$, 1, N1) + VS$ + MID$(Dfile$, N2)
        Dfile$ = VS$
        NEWFILE = 1
    ELSE
        LOCATE 1, 1: PRINT "ERROR MAKENEXT"
        WHILE INKEY$ = "": WEND
        STOP
    END IF
    EXIT SUB
END IF
PREV$ = MID$(Dfile$, 1, N1)
PAST$ = MID$(Dfile$, N2)
MDL$ = MID$(Dfile$, N1 + 1, N2 - N1 - 1)
U$ = MID$(MDL$, 1, 1): V$ = MID$(MDL$, 2, 1)
NN% = ASC(U$): KK% = ASC(V$) - 48
IF NN% >= 48 AND NN% <= 57 THEN
    NO% = VAL(MID$(Dfile$, N1 + 1, N2 - N1)) + 1
    IF NO% < 10 THEN
        MDL$ = "0" + MID$(STR$(NO%), 2)
    ELSE
        MDL$ = MID$(STR$(NO%), 2)
        IF MDL$ = "100" THEN
            MDL$ = "a0"
        END IF
    END IF
ELSE
    IF NN% >= 97 AND NN% <= 122 THEN
        III% = NN% - 96
        NO% = 10 * (9 + III%) + KK% + 1
        II% = NO% \ 10 - 10
        JJ% = NO% - (II% + 10) * 10
        UU$ = CHR$(96 + II% + 1)
        VV$ = CHR$(48 + JJ%)
        IF UU$ = "{" AND VV$ = "0" THEN
            N1 = INSTR(1, PREV$, "+")
            IF N1 <> 0 THEN
                NEWFILE = 0
                EXIT SUB
            END IF
            UU$ = "0": VV$ = "1"
            N1 = INSTR(1, PREV$, "_")
            IF N1 <> 0 THEN
                MID$(PREV$, N1, 1) = "-"
            ELSE
                N1 = INSTR(1, PREV$, "-")
                IF N1 <> 0 THEN
                    MID$(PREV$, N1, 1) = "+"
                END IF
            END IF
        END IF
        MDL$ = UU$ + VV$
    END IF
END IF
Dfile$ = PREV$ + MDL$ + PAST$
NEWFILE = 1
END SUB

SUB MakePrev (Dfile$, NEWFILE)
N1 = INSTR(1, Dfile$, "_")
IF N1 = 0 THEN
    N1 = INSTR(1, Dfile$, "-")
    IF N1 = 0 THEN
        N1 = INSTR(1, Dfile$, "+")
    END IF
END IF
N2 = INSTR(1, Dfile$, ".")
IF N2 - N1 - 1 > 2 THEN
    L = N2 - N1 - 1
    VS$ = MID$(Dfile$, N1 + 1, L)
    VLU = VAL(VS$)
    IF VLU + 1 < 10 ^ L THEN
        IF VLU - 1 > 0 THEN
            VS$ = MID$(STR$(VLU - 1), 2)
        ELSE
            LOCATE 1, 1: PRINT "FIRST FILE ALREADY"
            WHILE INKEY$ = "": WEND
            EXIT SUB
        END IF
        LV% = L - LEN(VS$)
        FOR K% = 1 TO LV%
            VS$ = "0" + VS$
        NEXT
        VS$ = MID$(Dfile$, 1, N1) + VS$ + MID$(Dfile$, N2)
        Dfile$ = VS$
        NEWFILE = 1
    ELSE
        LOCATE 1, 1: PRINT "ERROR MAKENEXT"
        WHILE INKEY$ = "": WEND
        STOP
    END IF
    EXIT SUB
END IF
PREV$ = MID$(Dfile$, 1, N1)
PAST$ = MID$(Dfile$, N2)
MDL$ = MID$(Dfile$, N1 + 1, N2 - N1 - 1)
U$ = MID$(MDL$, 1, 1): V$ = MID$(MDL$, 2, 1)
NN% = ASC(U$): KK% = ASC(V$) - 48
IF NN% >= 48 AND NN% <= 57 THEN
    NO% = VAL(MID$(Dfile$, N1 + 1, N2 - N1)) + 1
    IF NO% < 10 THEN
        MDL$ = "0" + MID$(STR$(NO%), 2)
    ELSE
        MDL$ = MID$(STR$(NO%), 2)
        IF MDL$ = "100" THEN
            MDL$ = "a0"
        END IF
    END IF
ELSE
    IF NN% >= 97 AND NN% <= 122 THEN
        III% = NN% - 96
        NO% = 10 * (9 + III%) + KK% + 1
        II% = NO% \ 10 - 10
        JJ% = NO% - (II% + 10) * 10
        UU$ = CHR$(96 + II% + 1)
        VV$ = CHR$(48 + JJ%)
        IF UU$ = "{" AND VV$ = "0" THEN
            N1 = INSTR(1, PREV$, "+")
            IF N1 <> 0 THEN
                NEWFILE = 0
                EXIT SUB
            END IF
            UU$ = "0": VV$ = "1"
            N1 = INSTR(1, PREV$, "_")
            IF N1 <> 0 THEN
                MID$(PREV$, N1, 1) = "-"
            ELSE
                N1 = INSTR(1, PREV$, "-")
                IF N1 <> 0 THEN
                    MID$(PREV$, N1, 1) = "+"
                END IF
            END IF
        END IF
        MDL$ = UU$ + VV$
    END IF
END IF
Dfile$ = PREV$ + MDL$ + PAST$
NEWFILE = 1
END SUB

REM $STATIC
SUB MakerEllipse (Xc, Yc, Theta, Rad, Ecc, CLR, GID%, Xmaker())
ThetaD = Theta * 180 / 3.1415926#
Dx = Rad * (1 + Ecc)
Dy = Rad * (1 - Ecc)
Xmin = Xc - Dx: Xmax = 2 * Dx
Ymin = Yc + Dy: Ymax = 2 * Dy
XminP = Xmin * Xmaker(1) + Xmaker(2)
XmaxP = Xmax * Xmaker(1)
YminP = Ymin * Xmaker(3) + Xmaker(4)
YmaxP = Ymax * Xmaker(1)
PRINT #3, USING "<Ellipse<GroupID ####>"; GID%
PRINT #3, USING "<ShapeRect ####.## ####.## ####.## ####.##>"; XminP; YminP; XmaxP; YmaxP
PRINT #3, USING "<Angle ####.##>"; ThetaD
PRINT #3, ">"
END SUB

REM $DYNAMIC
SUB MARK (XL, YL, XPc(), IDC())
NP = XPc(0)
FOR I = 1 TO NP
    IF XPc(2 * I - 1) = XL AND XPc(2 * I) = YL THEN
        IDC(I) = -2
        GOTO 130
    END IF
NEXT
130 '
END SUB

SUB MarkBALLS (XYlim(), XYscr(), XYout(), Contr(), Mlist(), Mes$)
DIM XY(50), XYT(20)
xmark = 0
REPmark:
LOCATE 1, 6: PRINT "Currently Marked for " + Mes$;
PRINT USING "## : Locate particle (Esc to exit) "; xmark
REPsearch:
CALL LOCATOR(XYlim(), XYscr(), XYout(), G$, Contr())
IF G$ = CHR$(27) THEN
    CALL CLEAN
    Mlist(0) = xmark
    EXIT SUB
END IF
IF XYout(1) = XYout(2) AND XYout(3) = XYout(4) THEN
    Xo = XYout(1): Yo = XYout(3): IBTYP = 0
    CALL SearchBALL(Xo, Yo, Tc, N, Xc, Yc, ITYPE, IBTYP, FOUND)
    IF FOUND <> 0 THEN
        xmark = xmark + 1
        CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, 14)
        Mlist(xmark) = FOUND
        GOTO REPmark
    ELSE
        GOTO REPsearch
    END IF
END IF
END SUB

SUB Match (S$, G$)
G$ = UCASE$(G$)
IF VAL(S$) = 0 THEN
    IF VAL(G$) <> 0 THEN
        S$ = G$
    ELSE
        N = 10 + ASC(G$) - 65
        S$ = MID$(STR$(N), 2)
    END IF
END IF
G$ = S$
END SUB

SUB MCLABEL (LB$, Xc, Yc, GID%)
PRINT #3, USING " <TextLine<GroupID ####>"; GID%
PRINT #3, USING " <TLOrigin ###.## ###.##>"; Xc; Yc
PRINT #3, " <TLAlignment Center>"
PRINT #3, " <String `" + LB$ + "'>"
PRINT #3, " >"
END SUB

SUB MLINE (XI, YI, XF, Yf, GID%, CLR%)
PRINT #3, USING " <PolyLine<GroupID ####>"; GID%
PRINT #3, USING " <Point ###.## ###.##>"; XI; YI
PRINT #3, USING " <Point ###.## ###.##>"; XF; Yf
PRINT #3, " >"
END SUB

SUB MOVEparticle (XYlim(), XYscr(), XYout(), Contr(), NFILE$)
SHARED Trans()
DIM XY(20), XYT(20), AP(2000), AR(2000)
DIM XYlimR(10), XYscrR(10), ContrR(10), XYplt(10), Labl$(30)
DIM bchange(20), xchange(20), ychange(20)

LRESIZE = 0: MSL$ = ""
RRPR:
LOCATE 1, 6: PRINT "Press Esc to finish " + MSL$;
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2)
Yoff = XYscr(3): Ylas = XYscr(4)
Trans(1) = (Xlas - Xoff) / (Xmax - Xmin)
Trans(3) = (Yoff - Ylas) / (Ymax - Ymin)
Trans(2) = Xoff - Trans(1) * Xmin
Trans(4) = Ylas - Trans(3) * Ymin
Trans(0) = 1
MOVEREP:
CALL LOCATOR(XYlim(), XYscr(), XYout(), G$, Contr())
SHARED XSPMODE, YSPMODE, SPMODE
FMOVE:
IF UCASE$(G$) = "R" AND LRESIZE <> 0 THEN
    FOR I = 1 TO 10
        XYlim(I) = XYlimR(I)
        XYscr(I) = XYscrR(I)
        Contr(I) = ContrR(I)
    NEXT
    VIEW (0, 0)-(Xlas + 2, YSPMODE - 1)
    CLS
    CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    CALL PlotBalls(XYlim(), XYscr())
    IF LRESIZE = 2 THEN GOTO exMOVE
    LRESIZE = 0
    MSL$ = ""
    GOTO RRPR
END IF
IF G$ = CHR$(27) THEN
    CALL CLEAN
    IF LRESIZE = 0 THEN
        GOTO exMOVE
    ELSE
        LRESIZE = 2
        G$ = "R"
        GOTO FMOVE
    END IF
END IF
IF XYout(1) = XYout(2) AND XYout(3) = XYout(4) THEN
    Xo = XYout(1): Yo = XYout(3): IBTYP = 0
    CALL SearchBALL(Xo, Yo, Tc, N, Xc, Yc, ITYPE, IBTYP, FOUND)
    IF FOUND <> 0 THEN
        IF ITYPE < 0 THEN
            Rad = -ITYPE
            Ecc = 1
        ELSE
            Rad = R(ITYPE)
            Ecc = E(ITYPE)
        END IF
        RRx = Rad * (1 + Ecc) * Trans(1)
        RRy = Rad * (1 + Ecc) * Trans(3)
        RR = RRx
        IF RRy > RR THEN RR = RRy
        IAD = M1 + (N - 1) * NDPART
        XPi = Xlas + 10: XPf = XSPMODE - 1
        YPf = YSPMODE - 1: YPi = YPf - (XPf - XPi)
        XPs = .5 * (XPi + XPf): YPs = .5 * (YPi + YPf)
        XPc = (XPs - Trans(2)) / Trans(1)
        YPc = (YPs - Trans(4)) / Trans(3)
        WINDOW: VIEW
        VIEW (XPi, YPi)-(XPf, YPf): CLS: VIEW
        '
        XCA = Xc * Trans(1) + Trans(2)
        YCA = Yc * Trans(3) + Trans(4)
        CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, 16)
        CALL PLOTELS(XPc, YPc, Tc, ITYPE, IBTYP, 13)
        GET (XPs - RR, YPs - RR)-(XPs + RR, YPs + RR), AP()
        VIEW (XPs - RR, YPs - RR)-(XPs + RR, YPs + RR)
        CLS
        WINDOW: VIEW
        RRD% = RR
        XYout(1) = (XCA - RR - Trans(2)) / Trans(1)
        XYout(2) = (XCA + RR - Trans(2)) / Trans(1)
        XYout(3) = (YCA - RR - Trans(4)) / Trans(3)
        XYout(4) = (YCA + RR - Trans(4)) / Trans(3)
        DRG:
        CALL DRAG(XYlim(), XYscr(), XYout(), G$, Contr(), AP(), RRD%)
        WINDOW: VIEW
        Xmp = XYout(1): Ymp = XYout(3)
        CALL PLOTELS(Xmp, Ymp, Tc, ITYPE, IBTYP, 11)
        IF G$ = CHR$(27) THEN
            CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, 12)
            NX1% = XYout(8)
            NY1% = XYout(9)
            PUT (NX1%, NY1%), AP(), XOR
            VIEW (Xoff, Yoff)-(Xlas, Ylas)
            WINDOW (Xmin, Ymin)-(Xmax, Ymax)
            LOCATE 2, 1: PRINT , NX1%, NY1%, "after"
            WHILE INKEY$ = "": WEND
            GOTO MOVEREP
        END IF
        CALL CheckContacts(N, Xnp, Ynp, Tc, ITYPE, Rad, Ecc, IFLAG)
        IF IFLAG <> 0 THEN
            BEEP
            XCA = Xnp * Trans(1) + Trans(2)
            YCA = Ynp * Trans(3) + Trans(4)
            XYout(1) = (XCA - RR - Trans(2)) / Trans(1)
            XYout(2) = (XCA + RR - Trans(2)) / Trans(1)
            XYout(3) = (YCA - RR - Trans(4)) / Trans(3)
            XYout(4) = (YCA + RR - Trans(4)) / Trans(3)
            GOTO DRG
        END IF
        CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, 1)
        nchange = nchange + 1
        bchange(nchange) = N
        xchange(nchange) = Xnp
        ychange(nchange) = Ynp
    END IF
ELSE
    '
    '   Local resize
    '
    FOR I = 1 TO 10
        XYlimR(I) = XYlim(I)
        XYscrR(I) = XYscr(I)
        ContrR(I) = Contr(I)
    NEXT
    VIEW (0, 0)-(Xlas + 2, YSPMODE - 1)
    CLS
    XYlim(1) = XYout(1): XYlim(2) = XYout(2)
    XYlim(3) = XYout(3): XYlim(4) = XYout(4)
    Labl$(1) = "X": Labl$(2) = "Y" '
    CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    CALL PlotBalls(XYlim(), XYscr())
    LRESIZE = 1
    MSL$ = "; R - Restore "
    GOTO RRPR
END IF
VIEW (Xoff, Yoff)-(Xlas, Ylas)
WINDOW (Xmin, Ymin)-(Xmax, Ymax)
GOTO MOVEREP
'
exMOVE:
'
IF nchange = 0 THEN EXIT SUB
CALL GetContactList
OPEN "TMP" FOR OUTPUT AS #4
FOR I = 1 TO NCONT
    VL = CLIST(I, 4)
    IF VL <> 0 THEN
        PRINT #4, VL
        PRINT VL
    END IF
NEXT
CALL GetDiskList
FOR I = 1 TO nchange
    N = bchange(I)
    CALL DeleteBall(-N)
    CALL DLISTi(N, 0, xchange(I))
    CALL DLISTi(N, 1, ychange(I))
    CALL DLISTi(N, 11, 0!)
    CALL DLISTi(N, 12, 0!)
NEXT
'CALL RewriteArray(NFILE$)
'
END SUB

REM $STATIC
SUB MOVETO (Xp, YP, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
XIR = Xp * RCOEFX + RXSHIFT
YIR = YP * RCOEFY + RYSHIFT
PRINT #3, USING "###.## ###.## moveto"; XIR; YIR
END SUB

REM $DYNAMIC
SUB MPGON (X(), Y(), N%, FillP%, GID%)
PRINT #3, USING " <Polygon<GroupID ####>"; GID%
PRINT #3, USING "<Fill ##>"; FillP%
FOR I = 1 TO N%
    PRINT #3, USING " <Point ###.## ###.##>"; X(I); Y(I)
NEXT
PRINT #3, " >"
END SUB

SUB MPLINE (X(), Y(), N%, FillP%, GID%)
PRINT #3, USING " <PolyLine<GroupID ####>"; GID%
PRINT #3, USING "<Fill #>"; FillP%
FOR I = 1 TO N%
    PRINT #3, USING " <Point ###.## ###.##>"; X(I); Y(I)
NEXT
PRINT #3, " >"
END SUB

SUB MPLOT (XYlim(), XYscr(), XYplt(), Labl$(), Contr())
SHARED Xmaker(), XSPMODE, YSPMODE, SPMODE, SMIN%, ASPECT
IACTION = Contr(3): Contr(3) = 0
IF IACTION <> 0 THEN Labl$(0) = "C"
XYlim(8) = XSPMODE: XYlim(9) = YSPMODE: XYlim(10) = SPMODE
Labl$(1) = "X": Labl$(2) = "Y"
Contr(1) = -1 ': Contr(2) = -1
XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 40: XYscr(3) = 40: XYscr(4) = 0
'
SCREEN SPMODE: WINDOW
'
'VIEW (0, 0)-(XYscr(2) + 40, YSPMODE - 1): CLS: WINDOW: VIEW
'
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
SMIN% = XYscr(2) + 2
ASPECT = (XYscr(2) - XYscr(1)) / (XYscr(4) - XYscr(3))
CALL GETDPR(XYlim(), XYscr(), Contr())
'
IF IACTION = 0 THEN EXIT SUB
Labl$(0) = "R": Contr(3) = 1
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
CALL GETDPR(XYlim(), XYscr(), Contr())
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
RHOFF = XYplt(7): RVOFF = XYplt(8): RHLAS = XYplt(9): RVLAS = XYplt(10)
IF Contr(3) <> 0 THEN
    IF Contr(3) <> 99 THEN
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVLAS - RVOFF) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymin
    ELSE
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVOFF - RVLAS) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymax
    END IF
END IF
Xmaker(1) = RCOEFX: Xmaker(2) = RXSHIFT
Xmaker(3) = RCOEFY: Xmaker(4) = RYSHIFT
IF Contr(3) = 99 THEN
    Xmaker(0) = 1
ELSE
    Xmaker(0) = -1
END IF
END SUB

REM $STATIC
SUB MPNT (Xp, YP, RCOEFX, RXSHIFT, RCOEFY, RYSHIFT)
XIR = Xp * RCOEFX + RXSHIFT
YIR = YP * RCOEFY + RYSHIFT
PRINT #3, USING " <Point ###.## ###.##>"; XIR; YIR
END SUB

REM $DYNAMIC
SUB MRECT (Xo, Yo, W, H, GID%, CLR)
PRINT #3, USING " <Rectangle<GroupID ####>"; GID%
PRINT #3, USING " <BRect ###.## ###.## ###.## ###.##>"; Xo; Yo; W; H
PRINT #3, " >"
END SUB

SUB MRLABEL (LB$, Xc, Yc, GID%)
PRINT #3, USING " <TextLine<GroupID ####>"; GID%
PRINT #3, USING " <TLOrigin ###.## ###.##>"; Xc; Yc
PRINT #3, " <TLAlignment Right>"
PRINT #3, " <String `" + LB$ + "'>"
PRINT #3, " >"
END SUB

SUB MSETFONT (Font$, Font%)
FON$ = Font$
N = INSTR(1, FON$, "-")
IF N <> 0 THEN
    FON$ = MID$(FON$, 1, N - 1)
END IF
IF MID$(Font$, N + 1, 1) = "B" THEN BL$ = "Yes" ELSE BL$ = "No"
s$ = " <Font<FFamily `" + FON$ + "'><FSize" + STR$(Font%) + "><FPlain Yes><FBold " + BL$ + ">"
PRINT #3, s$
s$ = " <FDX 0><FDY 0><FDAX 0><FNoAdvance No>"
PRINT #3, s$
s$ = " >"
PRINT #3, s$
END SUB

SUB MSETLINE (PN)
PRINT #3, USING "<PenWidth #.##>"; PN
END SUB

SUB MSPECIAL (Xc, Yc, DIA, I%, GID%)
IF I% = 0 THEN
    PRINT #3, USING " <Ellipse<GroupID ####>"; GID%
    PRINT #3, "<Fill 7>"
    PRINT #3, USING "<BRect ###.## ###.## ###.## ###.##>"; Xc - .5 * DIA; Yc - .5 * DIA; DIA; DIA
    PRINT #3, ">"
    EXIT SUB
END IF
IF I% = 1 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 0
    DTH = 90
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 4)
    EXIT SUB
END IF
IF I% = 2 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 45
    DTH = 90
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 4)
    EXIT SUB
END IF
IF I% = 3 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = -30
    DTH = 120
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 3)
    EXIT SUB
END IF
IF I% = 4 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 90
    DTH = 72
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 5)
    EXIT SUB
END IF
IF I% = 5 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 90
    DTH = 60
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 6)
    EXIT SUB
END IF
IF I% = 6 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 30
    DTH = 120
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 3)
    EXIT SUB
END IF
IF I% = 7 THEN
    PRINT #3, USING " <Ellipse<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    PRINT #3, USING "<BRect ###.## ###.## ###.## ###.##>"; Xc - .5 * DIA; Yc - .5 * DIA; DIA; DIA
    PRINT #3, ">"
    EXIT SUB
END IF
IF I% = 8 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 90
    DTH = 270
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 3)
    EXIT SUB
END IF
IF I% = 9 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 0
    DTH = 120
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 3)
    EXIT SUB
END IF
IF I% = 10 THEN
    PRINT #3, USING " <Polygon<GroupID ####>"; GID%
    PRINT #3, "<Fill 0>"
    TH0 = 180
    DTH = 120
    CALL PLGN(Xc, Yc, TH0, DTH, .5 * DIA, 3)
    EXIT SUB
END IF
END SUB

SUB MVLABEL (LB$, Xc, Yc, GID%)
PRINT #3, USING " <TextLine<GroupID ####>"; GID%
PRINT #3, USING " <TLOrigin ###.## ###.##>"; Xc; Yc
PRINT #3, " <TLAlignment Center>"
PRINT #3, " <Angle 90>"
PRINT #3, " <String `" + LB$ + "'>"
PRINT #3, " >"
END SUB

SUB MVMS (X, Y)
SHARED Trans()
MX% = X * Trans(1) + Trans(2): MY% = Y * Trans(3) + Trans(4)
M% = 4: CALL Qmouse(M%, BT%, MX%, MY%)
M% = 1: CALL Qmouse(M%, BT%, MX%, MY%)
END SUB

REM $STATIC
SUB NBDISK (NBD)
IAD = M1
FOR I = 1 TO NDISK
    IF AA(IAD + 8) = 0 THEN
        NBD = I - 1
        EXIT FOR
    END IF
    IAD = IAD + NDPART
NEXT
END SUB

SUB NEWHIST (IWRITE, PPOS)
DIM AR(42), AAr(42), Par$(10), XYlimG(10), XYscrG(10), XYpltG(10), LablG$(30), ContrG(30)
SHARED Hlist$()
DIM ADRS(100)
KG = 0
PPOSI = PPOS
IF IWRITE <> 0 THEN
    WINDOW: VIEW: CLS
END IF
ROWMAX = 25
CLP = 1: K = 0: KT = 0
NEWSummary:
CALL READSTR(PPOS, 40, Z$)
PPOS = PPOS + 5
IF K > ROWMAX THEN
    K = 0
    CLP = CLP + 40
END IF
K = K + 1
IF IWRITE = 0 THEN
    KG = KG + 1
    Hlist$(KG) = Z$
END IF
ADRS(K) = PPOS
IF IWRITE <> 0 THEN
    LOCATE K, CLP
END IF
IF MID$(Z$, 1, 3) <> "END" THEN
    IF IWRITE <> 0 THEN PRINT Z$;
    PPOS = PPOS + 42
    KT = KT + 1
    GOTO NEWSummary
ELSE
    IF IWRITE <> 0 THEN
        REPENTRY:
        LOCATE 29, 1: INPUT "Enter chart No "; ICH
        IF ICH = 0 THEN
            CLS
            EXIT SUB
        END IF
        IF ICH > KT THEN
            LOCATE 29, 1: PRINT "Maximum No of charts is"; KT;: PRINT " Press any key to continue ..."
            GOTO NOHPLOT
        END IF
        PPOS = PPOSI + (ICH - 1) * 47
        CALL READSTR(PPOS, 40, Z$)
        PPOS = PPOS + 5
        KZ% = 0
        FOR I = 1 TO 42
            AR(I) = AA(PPOS + I - 1)
            IF AR(I) = 0.0 THEN
                KZ% = KZ% + 1
            END IF
        NEXT
        IF KZ% >= 36 THEN
            LOCATE 29, 1: PRINT "ZERO DATA - CANNOT PLOT - press any key to continue..."
            GOTO NOHPLOT
        END IF
        NS = 1
        LablG$(1) = "X"
        LablG$(2) = "Y"
        WINDOW: VIEW: CLS
        LablG$(3) = MID$(Z$, 4)
        AR(0) = 0: NA = -36: ContrG(7) = 1: ContrG(8) = 1: ContrG(9) = 1
        IF AR(42) = -1 THEN
            ContrG(8) = -1 'Linear histogram
        END IF
        CALL HIST(AR(), AAr(), Par$(), NA, NS, XYlimG(), XYscrG(), XYpltG(), LablG$(), ContrG())
        LablG$(0) = ""
        NOHPLOT:
        WHILE INKEY$ = "": WEND
        CLP = 1: K = 0: KT = 0
        PPOS = PPOSI
        WINDOW: VIEW: CLS
        GOTO NEWSummary
    ELSE
        Hlist$(0) = MID$(STR$(KG), 2)
    END IF
END IF
END SUB

SUB PARSER (T$, Par$(), NSET) STATIC
K% = 0: NSET = -1
N% = LEN(T$)
FOR I% = 1 TO N%
    C$ = MID$(T$, I%, 1)
    IF C$ = "=" THEN C$ = " "
    IF C$ = " " AND K% <> 0 THEN K% = 0
    IF C$ = " " AND K% = 0 GOTO TF1
    IF K% = 0 THEN NSET = NSET + 1: Par$(NSET) = ""
    K% = K% + 1: Par$(NSET) = Par$(NSET) + C$
TF1: NEXT
END SUB

REM $DYNAMIC
SUB PLGN (Xc, Yc, TH0, DTH, Rad, N%)
TH = TH0
FOR I = 1 TO N%
    X = Xc + Rad * COS(TH * .01745329#)
    Y = Yc + Rad * SIN(TH * .01745329#)
    TH = TH + DTH
    PRINT #3, USING " <Point ###.## ###.##>"; X; Y
NEXT
PRINT #3, ">"
END SUB

SUB PLOT (Xar(), Yar(), Par$(), NA, NS, XYlim(), XYscr(), XYplt(), Labl$(), Contr()) STATIC
'XAr(Ns,Na) - Plot data
'YAr(Ns,Na) - Plot data
'Ns        - Number of curves
'Na        - Number of point per curve
DIM PAT$(16), AS$(16), AC$(16), Colr(16), A$(16), XYLIML(16)
XSPMODE = XYlim(8): YSPMODE = XYlim(9): SPMODE = XYlim(10)
Colr(1) = 9: Colr(2) = 10: Colr(3) = 11: Colr(4) = 12: Colr(5) = 13: Colr(6) = 1: Colr(7) = 2: Colr(8) = 3: Colr(9) = 4: Colr(10) = 5
Colr(11) = 9: Colr(12) = 10: Colr(13) = 11: Colr(14) = 12: Colr(15) = 13: Colr(16) = 1
Colr(0) = 15
REDIM ID%(0, 0)
PLRPT: Xmin = 1E+37: Xmax = -1E+37: Ymin = Xmin: Ymax = Xmax: ID%(0, 0) = 0
AS$(0) = "Cio"
AS$(1) = "Dia"
AS$(2) = "Rec"
AS$(3) = "Tri"
AS$(4) = "Pen"
AS$(5) = "Oct"
AS$(6) = "Itr"
AS$(7) = "Cir"
AS$(8) = "Str"
AS$(9) = "Rtr"
AS$(10) = "Ltr"
AC$(0) = "BU2G2R4H2BD2U1"
AC$(1) = "BD1BL1R3H1L1E1F2"
AC$(2) = "BU1BL1R3G1L1F1E2"
AC$(3) = "L1U1R3D1L3D1R3"
AC$(4) = "U2D4U4G2F2E2H2D2R2L4"
AC$(5) = "U1G1D1R3U1HD1R1"
AC$(6) = "U1L1G1D1F1R1E1U1H1D2L2R1U1"
AC$(7) = AC$(1): AC$(8) = AC$(2): AC$(9) = AC$(3): AC$(10) = AC$(4)
PAT$(1) = "111111111100000111111111100000"
PAT$(2) = "111111111100100"
PAT$(3) = "1111100000"
PAT$(4) = "1111100100"
PAT$(5) = "1111111111000001111100000"
PAT$(6) = "1111111111001001111100100"
PAT$(7) = "11100"
PAT$(8) = "11100100"
PAT$(9) = "1111100100100"
PAT$(10) = "1010": TCKK$ = ""
IF NS < 10 THEN FOR I% = NS + 1 TO 6: Par$(I%) = "": NEXT I%
Gflag = 0: NAX = NA
IF NAX = 0 THEN 'Xdat - groups assumed different size
    IF Yar(0, 0) <> 0 THEN Imin% = 0 ELSE Imin% = 1
    FOR I% = Imin% TO NS
        Nps = Yar(I%, 0)
        IF Nps > NAX THEN NAX = Nps
    NEXT I%
    Gflag = 1
END IF
'If Gflag=0 [Na<>0 all groups have same length, and same independent argument
'            contained in YAr(0,j) , j=1, to Na]
'If Gflag=1 [Na=0 groups may be different size, different independent agument
'            contained in XAr(i,j) , i=1,Ns, j=1,YAr(i,0)]
IF Gflag = 1 THEN
    FOR I% = Imin% TO NS
        Nps = Yar(I%, 0)
        FOR J% = 1 TO Nps
            IF Xar(I%, J%) > Xmax THEN Xmax = Xar(I%, J%)
            IF Xar(I%, J%) < Xmin THEN Xmin = Xar(I%, J%)
            IF Yar(I%, J%) > Ymax THEN Ymax = Yar(I%, J%)
            IF Yar(I%, J%) < Ymin THEN Ymin = Yar(I%, J%)
        NEXT J%
    NEXT I%
ELSE
    FOR I% = Imin% TO NS
        Yar(I%, 0) = NAX
        FOR J% = 1 TO NAX
            IF Yar(I%, J%) > Ymax THEN Ymax = Yar(I%, J%)
            IF Yar(I%, J%) < Ymin THEN Ymin = Yar(I%, J%)
            IF I% = 1 THEN
                IF Yar(0, J%) > Xmax THEN Xmax = Yar(0, J%)
                IF Yar(0, J%) < Xmin THEN Xmin = Yar(0, J%)
            END IF
        NEXT J%
    NEXT I%
END IF
XcO = 1
IF INSTR(1, Labl$(1), "#") <> 0 THEN
    XcO = 0
    XMIN1 = INT(Xmin)
    IF XMIN1 > Xmin THEN XMIN1 = XMIN1 - 1
    Xmin = XMIN1
    XMAX1 = INT(Xmax)
    IF XMAX1 < Xmax THEN XMAX1 = XMAX1 + 1
    Xmax = XMAX1
END IF
YcO = 1
IF INSTR(1, Labl$(2), "#") <> 0 THEN
    YcO = 0
    YMIN1 = INT(Ymin)
    IF YMIN1 > Ymin THEN YMIN1 = YMIN1 + 1
    Ymin = YMIN1
    YMAX1 = INT(Ymax)
    IF YMAX1 < Ymax THEN YMAX1 = YMAX1 + 1
    Ymax = YMAX1
END IF
XYLIML(1) = Xmin: XYLIML(2) = Xmax
XYLIML(3) = Ymin: XYLIML(4) = Ymax
dXran = .1 * (Xmax - Xmin): dYran = .1 * (Ymax - Ymin)
XminR = Xmin - XcO * dXran: XmaxR = Xmax + XcO * dXran
YminR = Ymin - YcO * dYran: YmaxR = Ymax + YcO * dYran
IF XminR * Xmin < 0 THEN XminR = Xmin
IF XmaxR * Xmax < 0 THEN XmaxR = Xmax
IF YminR * Ymin < 0 THEN YminR = Ymin
IF YmaxR * Ymax < 0 THEN YmaxR = Ymax
Xmin = XminR: Xmax = XmaxR: Ymin = YminR: Ymax = YmaxR
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax
'FOR I% = 1 TO NAX: ID%(I%, 1) = I%: ID%(I%, 2) = I%: NEXT I%
REDOP:
IF XYscr(1) = 0 AND XYscr(2) = 0 THEN
    XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 70: XYscr(3) = 40: XYscr(4) = 0: XYscr(5) = 0
END IF
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
Imaker% = 0
IF Contr(3) = 99 THEN Imaker% = 1: Contr(3) = 1
N = LEN(Labl$(0))
IF N <> 0 THEN
    IF N > 2 AND MID$(Labl$(0), N, 1) = "R" THEN GOTO FNTO
END IF
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
XLEN = XYplt(1): Ylen = XYplt(2)
PERC = .05
XYLEN = SQR((Xmax - Xmin) ^ 2 + (Ymax - Ymin) ^ 2)
PIXS = PERC * SQR((Xoff - Xlas) ^ 2 + (Yoff - Ylas) ^ 2): DPIXS = PIXS / 16
PIXP = PERC * SQR(XLEN * XLEN + Ylen * Ylen): DPIXP = PIXP / 16
AXFS = (Xoff - Xlas) / (Xmax - Xmin): AXFP = XLEN / (Xmax - Xmin)
AYFS = (Ylas - Yoff) / (Ymin - Ymax): AYFP = Ylen / (Ymax - Ymin)
CPEN = 0: Cpause = 0
DASH = 0
IF Contr(3) <> 0 AND NS > 1 THEN
    DASH = XYplt(0)
END IF
LEGT$ = Par$(0): FMT$ = ""
IF LEN(LEGT$) <> 0 THEN
    FMT1$ = "#.#####^^^^"
    N = INSTR(1, LEGT$, ";")
    IF N <> 0 THEN FMT$ = MID$(LEGT$, N + 1): Par$(0) = MID$(LEGT$, 1, N - 1) ELSE FMT$ = ""
    IF N <> 0 AND FMT$ = "" THEN FMT$ = FMT1$
END IF
IF FMT$ = "" GOTO PLXFM
OPEN "TMP" FOR OUTPUT AS #2
FOR I% = 1 TO NS
    LEGT$ = Par$(I%)
    N1 = INSTR(1, LEGT$, "["): N2 = INSTR(1, LEGT$, "]")
    IF N1 = 0 AND N2 = 0 THEN P1$ = LEGT$: P2$ = ""
    IF N1 <> 0 THEN P1$ = MID$(LEGT$, 1, N1 - 1)
    IF N2 <> 0 THEN P2$ = MID$(LEGT$, N2 + 1)
    s = 1: PP$ = P1$: IF P2$ <> "" THEN PP$ = P2$: s = 2
    PRINT #2, USING FMT$; VAL(PP$)
NEXT I%
CLOSE #2: OPEN "TMP" FOR INPUT AS #2
FOR I% = 1 TO NS
    LEGT$ = Par$(I%)
    N1 = INSTR(1, LEGT$, "["): N2 = INSTR(1, LEGT$, "]")
    IF N1 = 0 AND N2 = 0 THEN P1$ = LEGT$: P2$ = "":
    IF N1 <> 0 THEN P1$ = MID$(LEGT$, 1, N1 - 1)
    IF N2 <> 0 THEN P2$ = MID$(LEGT$, N2 + 1)
    s = 1: PP$ = P1$: IF P2$ <> "" THEN PP$ = P2$: s = 2
    LINE INPUT #2, PP$
    IF s = 1 THEN P1$ = PP$ ELSE P2$ = PP$
    IF N1 <> 0 AND N2 <> 0 THEN WD$ = MID$(LEGT$, N1, N2 - N1 + 1) ELSE WD$ = ""
    Par$(I%) = P1$ + WD$ + P2$
NEXT I%
CLOSE #2
PLXFM: '
Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
LEGT$ = Par$(0)
AXREV = Contr(2)
POSI% = INT(Xlas / 8) + 1
YCEN = INT(.5 * (Ylas + Yoff) / 16) + Yoff / 16
LEGLEN = NS
IF LEN(LEGT$) <> 0 THEN LEGLEN = LEGLEN + 3
ROIN% = INT((YCEN - LEGLEN / 2)) + 1
IF ROIN% < 2 THEN ROIN% = 2
IF LEN(LEGT$) <> 0 THEN
    COLI% = .5 * (POSI% + 127 - LEN(LEGT$)): IF COLI% < POSI% THEN COLI% = POSI%: LEGT$ = MID$(LEGT$, 1, 127 - POSI%)
    LOCATE ROIN%, COLI%: PRINT LEGT$
    ROIN% = ROIN% + 1
    LOCATE ROIN%, COLI%: PRINT STRING$(LEN(LEGT$), "-");
    ROIN% = ROIN% + 2
END IF
IF Contr(3) <> 0 THEN
    RHOFF = XYplt(7): RVOFF = XYplt(8): RHLAS = XYplt(9): RVLAS = XYplt(10)
    IF Imaker% = 0 THEN
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVLAS - RVOFF) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymin
        IF AXREV THEN RCOEFY = -RCOEFY: RYSHIFT = RVLAS - RCOEFY * Ymin
    ELSE
        RCOEFX = (RHLAS - RHOFF) / (Xmax - Xmin)
        RCOEFY = (RVOFF - RVLAS) / (Ymax - Ymin)
        RXSHIFT = RHOFF - RCOEFX * Xmin
        RYSHIFT = RVOFF - RCOEFY * Ymax
        IF AXREV THEN RCOEFY = -RCOEFY: RYSHIFT = RVLAS - RCOEFY * Ymax
    END IF
END IF
IF Contr(3) <> 0 THEN
    IF Imaker% = 0 THEN
        PRINT #3, "1 setlinewidth"
    ELSE
        CALL MSETLINE(PN)
    END IF
END IF
FOR I% = Imin% TO NS
    Nps = Yar(I%, 0) - 1
    IF Contr(10) <> 2 THEN
        IF Contr(3) <> 0 THEN
            IF Imaker% = 0 THEN
                PRINT #3, "newpath"
            ELSE
                IF I% <> 0 THEN
                    PRINT #3, "<PolyLine<GroupID    1>"
                    PRINT #3, "<Pen 0>"
                    PRINT #3, "<Fill 15>"
                END IF
            END IF
        END IF
        IF Contr(3) <> 0 AND I% <> 0 AND DASH <> 0 THEN
            LENPAT% = LEN(PAT$(I%))
            C% = VAL(MID$(PAT$(I%), 1, 1))
            K% = 0: L% = 0
            A$ = "["
            FOR PS% = 1 TO LENPAT%
                IF VAL(MID$(PAT$(I%), PS%, 1)) = C% THEN
                    K% = K% + 1
                ELSE
                    L% = L% + 1
                    C% = VAL(MID$(PAT$(I%), PS%, 1))
                    IF L% = 1 THEN A$ = A$ + MID$(STR$(K%), 2) ELSE A$ = A$ + STR$(K%)
                    K% = 1
                END IF
            NEXT
            A$ = A$ + STR$(K%) + "]"
            Dist = 0
            FOR II = 1 TO Nps
                K1% = II: K2% = II + 1
                IF Gflag = 1 THEN
                    XI = Xar(I%, K1%): XF = Xar(I%, K2%)
                ELSE
                    XI = Yar(0, K1%): XF = Yar(0, K2%)
                END IF
                YI = Yar(I%, K1%): Yf = Yar(I%, K2%)
                Dx = XF - XI: Dy = Yf - YI
                Dist = Dist + SQR(Dx * Dx + Dy * Dy)
            NEXT
            LENGAR% = Dist
            NTIMES% = LENGAR% \ LENPAT%
            NRESID% = LENGAR% - NTIMES% * LENPAT%
            DIFMIN% = 1000
            FOR O% = 0 TO LENPAT% - 1
                NTIMES% = (LENGAR% + O%) \ LENPAT%
                NRESID% = LENGAR% + O% - NTIMES% * LENPAT%
                IF NRESID% = 0 THEN NRESID% = LENPAT%
                CL% = VAL(MID$(A$(1), NRESID%, 1))
                FOR L% = 0 TO NRESID%
                    PS% = NRESID% - L%
                    IF PS% = 0 THEN PS% = LENPAT%
                    IF VAL(MID$(A$(I%), PS%, 1)) <> CL% THEN EXIT FOR
                NEXT
                CR% = VAL(MID$(A$(I%), O% + 1, 1))
                FOR R% = 0 TO LENPAT% - O% - 1
                    PS% = R% + 1 + O%
                    IF VAL(MID$(A$(I%), PS%, 1)) <> CR% THEN EXIT FOR
                NEXT
                IF CL% = 1 AND CR% = 1 THEN
                    DF% = ABS(L% - R%)
                    IF DF% < DIFMIN% THEN DIFMIN% = DF%: OFFSET% = O%
                END IF
            NEXT
            IF DIFMIN% = 1000 THEN OFFSET% = 0
            PRINT #3, A$;: PRINT #3, USING " ## setdash"; OFFSET%
        END IF
    END IF
    CLR = Colr(I%) ': IF XYscr(7) <> 0 THEN CLR = 15
    St$ = "C" + MID$(STR$(CLR), 2)
    Nps = Yar(I%, 0) - 1
    IF Nps = 0 THEN
        XI = Xar(I%, 1): Yf = Yar(I%, 1)
        PSET (XI, Yf), CLR
        LOCATE 1, 7: PRINT "T="; XI;: PRINT "Y="; Yf
    END IF
    FOR J% = 1 TO Nps
        K1% = J%: K2% = J% + 1
        IF Gflag = 1 THEN
            XI = Xar(I%, K1%): XF = Xar(I%, K2%)
        ELSE
            XI = Yar(0, K1%): XF = Yar(0, K2%)
        END IF
        YI = Yar(I%, K1%): Yf = Yar(I%, K2%)
        IF XI < XYlim(1) OR XF > XYlim(2) OR YI < XYlim(3) OR Yf > XYlim(4) GOTO DND
        IF I% <> 0 AND Contr(10) <> 2 THEN
            IF J% < KSEQ THEN CLR = 12 ELSE CLR = 1
            LINE (XI, YI)-(XF, Yf), CLR
            IF Contr(3) <> 0 THEN
                IF J% = 1 THEN
                    XIR = XI * RCOEFX + RXSHIFT
                    YIR = YI * RCOEFY + RYSHIFT
                    IF Imaker% = 0 THEN
                        PRINT #3, USING "###.## ###.## moveto"; XIR; YIR
                    ELSE
                        PRINT #3, USING " <Point ###.## ###.##>"; XIR; YIR
                    END IF
                END IF
                XIR = XF * RCOEFX + RXSHIFT
                YIR = Yf * RCOEFY + RYSHIFT
                IF Imaker% = 0 THEN
                    PRINT #3, USING "###.## ###.## lineto"; XIR; YIR
                ELSE
                    PRINT #3, USING " <Point ###.## ###.##>"; XIR; YIR
                END IF
            END IF
        ELSE
            PSET (XI, YI): DRAW St$ + AC$(I%)
            IF Contr(3) <> 0 THEN
                XIR = XI * RCOEFX + RXSHIFT
                YIR = YI * RCOEFY + RYSHIFT
                IF Imaker% = 0 THEN
                    PRINT #3, USING "###.## ###.## 8 "; XIR; YIR;: PRINT #3, AS$(I%)
                ELSE
                    CALL MSPECIAL(XIR, YIR, 4!, I%, 1)
                END IF
            END IF
            IF J% = Nps THEN
                PSET (XF, Yf): DRAW St$ + AC$(I%)
                IF Contr(3) <> 0 THEN
                    XIR = XF * RCOEFX + RXSHIFT
                    YIR = Yf * RCOEFY + RYSHIFT
                    IF Imaker% = 0 THEN
                        PRINT #3, USING "###.## ###.## 8 "; XIR; YIR;: PRINT #3, AS$(I%)
                    ELSE
                        CALL MSPECIAL(XIR, YIR, 4!, I%, 1)
                    END IF
                END IF
            END IF
        END IF
        DND:
    NEXT J%
    IF Contr(3) <> 0 THEN
        IF Imaker% = 0 THEN
            PRINT #3, "stroke"
        ELSE
            PRINT #3, " >"
        END IF
    END IF
    LEGEND: IF I% = 0 THEN GOTO XX3
    LEGT$ = Par$(I%)
    N1 = INSTR(1, LEGT$, "["): N2 = INSTR(1, LEGT$, "]")
    IF N1 = 0 AND N2 = 0 THEN P1$ = LEGT$: P2$ = ""
    IF N1 <> 0 THEN P1$ = MID$(LEGT$, 1, N1 - 1)
    IF N2 <> 0 THEN P2$ = MID$(LEGT$, N2 + 1)
    WD% = 0
    IF N1 <> 0 AND N2 <> 0 AND N1 < N2 THEN WD% = N2 - N1 - 1
    IF NS = 1 AND P1$ = "" AND WD% = 0 AND P2$ = "" GOTO XX3:
    IF P1$ = "" THEN P1$ = MID$(STR$(I%), 2)
    IF WD% = 0 THEN WD% = 4
    LEGT$ = P1$ + STRING$(WD% + 2, " ") + P2$
    COLI% = .5 * (POSI% + 127 - LEN(LEGT$)): IF COLI% < POSI% THEN COLI% = POSI%: LEGT$ = MID$(LEGT$, 1, 127 - POSI%)
    LOCATE ROIN%, COLI%: PRINT LEGT$;
    ROIN% = ROIN% + 1
    WINDOW: VIEW
    X1 = (COLI% + LEN(P1$)) * 8: X2 = X1 + WD% * 8: Y1 = (ROIN% - 1) * 16 - 9
    LINE (X1, Y1)-(X2, Y1), CLR: GOTO PLEL
    Xc = .5 * (X1 + X2)
    X1 = Xc - PIXS / 2 - 4: X2 = Xc + PIXS / 2 - 4
    PLEL: VIEW (Xoff, Yoff)-(Xlas, Ylas)
    IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
    IF I% = NS GOTO XX3
    IF Contr(3) <> 0 AND NS > 1 THEN
        IF Cpause = 0 THEN GOTO XX3
        LOCATE 1, 59: PRINT "Press any key ...   "
        WHILE INKEY$ = "": WEND
        LOCATE 1, 59: PRINT "End key twice - quit"
    END IF
XX3: NEXT I%
GOTO PLFin
PLKB: G$ = INKEY$: IF G$ = "" THEN FLGR = 0: RETURN
IF ASC(RIGHT$(G$, 1)) = 79 THEN FLGR = 1: RETURN ELSE FLGR = 0: RETURN
PLFin:
IF LEN(Labl$(0)) <= 2 AND TCKK$ = "" THEN TCK$ = Labl$(0): RRR$ = "": GOTO PLZX
TCK$ = MID$(Labl$(0), 1, 2)
IF TCK$ = "CR" THEN RRR$ = MID$(Labl$(0), 3): Labl$(0) = "R" + RRR$ ELSE TCK$ = MID$(Labl$(0), 1, 1): RRR$ = MID$(Labl$(0), 2)
PLZX: IF Contr(9) = 1 THEN Labl$(0) = TCKK$: GOTO EEEE:
IF Contr(0) = 0 AND TCK$ = "CR" THEN
    Labl$(0) = "R" + RRR$: LOCATE 1, 64: PRINT "E - Edit";
    PLLPX: IWHAT$ = INKEY$: IF IWHAT$ = "" GOTO PLLPX:
    IF IWHAT$ = "E" OR IWHAT$ = "e" THEN
        EEEE: CALL SCREDIT(XYlim(), XYscr(), XYplt(), Labl$(), Par$(), Contr(), Xar(), Yar(), NA, NS, ID%())
        IF Contr(9) = 1 THEN
            TCKK$ = Labl$(0): Labl$(0) = "C"
            IF XYlim(1) = 0 AND XYlim(2) = 0 AND XYlim(3) = 0 AND XYlim(4) = 0 THEN
                FOR I = 1 TO 4: XYlim(I) = XYLIML(I): NEXT
            END IF
            GOTO REDOP
        END IF
        WHILE INKEY$ = "": WEND
    END IF
    GOTO PLRPT
END IF
IF Contr(3) <> 0 THEN
    LOCATE 1, 59: PRINT STRING$(21, " ");
    LOCATE 1, 59: INPUT "Fin/Rep/Edit "; IWHAT$
    IF IWHAT$ = "R" OR IWHAT$ = "r" THEN
        LOCATE 1, 59: PRINT STRING$(19, " ");
        IF Contr(3) <> 0 THEN CLOSE #3
        GOTO PLRPT
    END IF
    IF IWHAT$ = "F" OR IWHAT$ = "f" THEN
        LOCATE 1, 59: PRINT STRING$(19, " ");
        IF Imaker% = 0 THEN PRINT #3, "showpage"
        CLOSE #3
        GOTO FFX
    END IF
    IF CPEN = 1 THEN Par$(0) = Par$(0) + "@"
    Contr(3) = -1
    CALL SCREDIT(XYlim(), XYscr(), XYplt(), Labl$(), Par$(), Contr(), Xar(), Yar(), NA, NS, ID%())
END IF
FFX: IF Contr(3) = 0 AND TCK$ = "C" THEN
    LOCATE 1, 59
    PRINT "Hard Copy (Y/N) ";
    WTS: G$ = INKEY$: IF G$ = "" GOTO WTS
    IWHAT$ = G$: LOCATE 1, 59: PRINT STRING$(20, " ");
    IF IWHAT$ = "Y" OR IWHAT$ = "y" THEN Contr(3) = 2: Labl$(0) = "R" + RRR$: LOCATE 1, 59: PRINT STRING$(19, " ");: GOTO PLRPT
END IF
FNTO: CLOSE #3
ERASE PAT$, AS$, AC$, Colr, ID%, A$, XYLIML
END SUB

SUB PlotBBalls (XYlim(), XYscr(), CLR)

FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IF IBTYP = 0 THEN
        EXIT FOR
    ELSE
        CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, CLR)
    END IF
NEXT

END SUB
REM $STATIC
SUB PlotBalls (XYlim(), XYscr())
SHARED Xmaker()
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
'
FOR N = NDISK TO 1 STEP -1
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IF Xc > Xmin AND Xc < Xmax THEN
        IF Yc > Ymin AND Yc < Ymax THEN
            IF IBTYP = 0 THEN
                CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, 10)
            ELSE
                CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, 13)
            END IF
        END IF
    END IF
NEXT
IF Xmaker(0) = 1 THEN
    PRINT #3, "<Group <ID 5>>"
END IF
CALL BORDER(XYlim(), XYscr())
END SUB

SUB PLOTbox (NB)
SHARED NX, NY, DELX, DELY
SHARED XYlim(), XYscr(), XYplt(), Contr(), Labl$(), Trans()
NXX = (NB - 1) MOD NX + 1
NYY = (NB - NXX) / NX + 1
XB = (NXX - 1) * DELX
YB = (NYY - 1) * DELY
X1 = Trans(1) * XB + Trans(2)
Y1 = Trans(3) * YB + Trans(4)
X2 = (XB + DELX) * Trans(1) + Trans(2)
Y2 = (YB + DELY) * Trans(3) + Trans(4)
LINE (X1, Y1)-(X2, Y2), 15, BF
END SUB

SUB PLOTellipse (Xc, Yc, Theta, Rad, Ecc, CLRL)
SHARED Trans(), Xmaker(), TWOP, PATRN(), ND
T% = Trans(0)
W% = Xmaker(0)
IF W% <> 0 THEN T% = 0
CLR = ABS(CLRL)
IF CLR > 16 THEN CLR = 16
IF CLR = 9 THEN CLR = 16
I = 0: DT = TWOP / ND
IF W% <> 0 THEN DIM XX(ND), YY(ND)
A = Rad * (1 + Ecc)
B = Rad * (1 - Ecc)
CS = COS(-Theta)
SN = SIN(-Theta)
IF Ecc = 1 THEN
    RR = Rad * 2
    XI = Xc - RR * CS
    YI = Yc + RR * SN
    XF = Xc + RR * CS
    Yf = Yc - RR * SN
    IF T% <> 0 THEN
        XI = XI * Trans(1) + Trans(2)
        YI = YI * Trans(3) + Trans(4)
        XF = XF * Trans(1) + Trans(2)
        Yf = Yf * Trans(3) + Trans(4)
    END IF
    '
    LINE (XI, YI)-(XF, Yf), CLR - 1
    EXIT SUB
END IF
FOR I = 1 TO ND + 1
    Tnot = (I - 1) * DT
    XE = A * COS(Tnot)
    YE = B * SIN(Tnot)
    X = Xc + XE * CS + YE * SN
    Y = Yc - XE * SN + YE * CS
    IF T% <> 0 THEN
        X = X * Trans(1) + Trans(2)
        Y = Y * Trans(3) + Trans(4)
    END IF
    IF Tnot > 0 THEN
        LINE (X1, Y1)-(X, Y), CLR - 1
        IF BFLAG <> 0 THEN
            LOCATE 3, 1: PRINT X1, Y1
            WHILE INKEY$ = "": WEND
        END IF
    END IF
    X1 = X: Y1 = Y
NEXT
IF W% = 1 THEN
    CALL MakerEllipse(Xc, Yc, Theta, Rad, Ecc, CLR, 5, Xmaker())
END IF
IF W% = -1 THEN
    TT = Theta * 180 / 3.1415926#
    IF TT < 0 THEN TT = 360 + TT
    XXL = Xc * Xmaker(1) + Xmaker(2)
    YYL = Yc * Xmaker(3) + Xmaker(4)
    RRB = Rad * Xmaker(1)
    PRINT #3, USING "####.#### ####.#### ####.#### ###.#### ##.### 1 ellipse"; XXL; YYL; TT; RRB; Ecc
    ERASE XX, YY
END IF
END SUB

SUB PLOTELS (Xc, Yc, Theta, ITYP, IBTYP, CLR)
SHARED XYlim(), XYscr(), XYplt(), Contr(), Labl$(), Trans(), SHAPES()
DIM XY(30), XYT(30)
'
IF ITYP > 0 THEN
    FOR K = 0 TO 2 * SHAPES(ITYP, K): XY(K) = SHAPES(ITYP, K): NEXT
ELSE
    XY(0) = 1
END IF
'
IF XY(0) <> 1 THEN
    CALL TRNPOLY(XY(), XYT(), Theta, Xc, Yc)
    CALL PLOTpoly(XYT(), CLR)
ELSE
    IF ITYP > 0 THEN
        Rad = SHAPES(ITYP, 1)
        Ecc = SHAPES(ITYP, 2)
        IF IBTYP <> 0 THEN Ecc = 0
    ELSE
        Rad = -ITYP
        Ecc = 1
    END IF
    CALL PLOTellipse(Xc, Yc, Theta, Rad, Ecc, CLR)
END IF
CLRP = CLR - 9
IF CLRP = 0 THEN CLRP = 7
IF CLR = 9 THEN
    EXIT SUB
END IF
IF ITYP > 0 THEN
    XCP = Xc
    YCP = Yc
    IF Trans(0) = 1 THEN
        XCP = Xc * Trans(1) + Trans(2)
        YCP = Yc * Trans(3) + Trans(4)
    END IF
    PAINT (XCP, YCP), CLRP, CLR - 1
END IF
END SUB

SUB PLOTELSsc (Xc, Yc, Theta, ITYP, IBTYP, CLR, SCL)
SHARED XYlim(), XYscr(), XYplt(), Contr(), Labl$(), Trans(), SHAPES()
DIM XY(30), XYT(30)
'
IF ITYP > 0 THEN
    FOR K = 0 TO 2 * SHAPES(ITYP, K): XY(K) = SHAPES(ITYP, K): NEXT
ELSE
    XY(0) = 1
END IF
'
IF XY(0) <> 1 THEN
    CALL TRNPOLY(XY(), XYT(), Theta, Xc, Yc)
    CALL PLOTpoly(XYT(), CLR)
ELSE
    IF ITYP > 0 THEN
        Rad = SHAPES(ITYP, 1) * SCL
        Ecc = SHAPES(ITYP, 2)
        IF IBTYP <> 0 THEN Ecc = 0
    ELSE
        Rad = -ITYP * SCL
        Ecc = 1
    END IF
    CALL PLOTellipse(Xc, Yc, Theta, Rad, Ecc, CLR)
END IF
CLRP = CLR - 9
IF CLRP = 0 THEN CLRP = 7
IF CLR = 9 THEN
    EXIT SUB
END IF
IF ITYP > 0 THEN
    XCP = Xc
    YCP = Yc
    IF Trans(0) = 1 THEN
        XCP = Xc * Trans(1) + Trans(2)
        YCP = Yc * Trans(3) + Trans(4)
    END IF
    PAINT (XCP, YCP), CLRP, CLR - 1
END IF
END SUB


SUB PLOTELSt (Xc, Yc, Theta, ITYP, IBTYP, CLR)
SHARED XYlim(), XYscr(), XYplt(), Contr(), Labl$(), Trans(), SHAPES()
DIM XY(30), XYT(30)
'
FOR K = 0 TO 2 * SHAPES(ITYP, K): XY(K) = SHAPES(ITYP, K): NEXT
'
CALL TRNPOLY(XY(), XYT(), Theta, Xc, Yc)
CALL PLOTpolyt(XYT(), CLR)
IF IBTYP <> 0 THEN PAINT (Xc, Yc), 4, CLR
END SUB
'
SUB CLOOP (UPT)
STIFF = 5264123.0: DAVER = 39.897: CO = 0.5 / STIFF
IAD = M1 + NDPART * NDISK
UPT = 0
CLPcont:
IB1 = AA(IAD)
IF IB1 = 0 GOTO FINloop
IB2 = AA(IAD + 1)
CALL GETCONT(IB1, IB2, IAD, X1, Y1, X2, Y2, FRN, FRT, DSN, DST, M1, NDPART)
IF FRN <> 0 THEN
    UPT = UPT + (FRN * FRN + 2 * FRT * FRT) * CO
END IF
IAD = IAD + NCPART
GOTO CLPcont
FINloop:
'
END SUB

SUB PlotFORCES (XYlim(), XYscr(), XYout(), Contr())
SHARED WDMODE, ROIN%, CMIN%
NDPART = 14: NCPART = 6
DIM XYCN(4, 2)
NCc = 0: NFC1 = 0: NFC2 = 0
SHARED Xmaker(), SHAPES()
DIM XYplt(10)
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
IAD = M1 + NDPART * NDISK
NA = AA(20) + 2 * (47 - 1) + 1
CLR = 12: IBMAX = M1 + NBD * NDPART: FCAVG = AA(NA): THMAX = .017
IF FCAVG = 0 THEN FCAVG = 3000.00
PNTMAX = 72 * THMAX / 2.54: COE = 1!
NFC = 0
NCN = 0
LAV = 0
CAV = 0
FAV = 0
FAVS = 0
S11 = 0
S22 = 0
CRL = 0
KIN = 0: IBK = M1 + (667 - 1) * NDPART
NEXTforce:
IB1 = AA(IAD)
IF IB1 = 0 GOTO FINforces
IB2 = AA(IAD + 1)
IF IB1 >= IBMAX THEN NFC = NFC + 1
IF IB2 >= IBMAX THEN NFC = NFC + 1
'NOPLT = 1
CALL GETCONT(IB1, IB2, IAD, X1, Y1, X2, Y2, FRN, FRT, DSN, DST, M1, NDPART)
'IF IB1 = IBK OR IB2 = IBK THEN
'    NB1 = (IB1 - M1) / NDPART + 1: NB2 = (IB2 - M1) / NDPART + 1
'    NOPLT = 1
'END IF
IF FRN <> 0 THEN
    NCN = NCN + 1
    FRC = SQR(FRN * FRN + FRT * FRT)
    TN = FRC / (3 * FCAVG) * THMAX: IF TN > THMAX THEN TN = THMAX
    CALL GETDISC(-IB1, XC1, YC1, THETA1, ITYP1, IBTYP1)
    CALL GETDISC(-IB2, XC2, YC2, THETA2, ITYP2, IBTYP2)
    IF IBTYP1 = 0 AND IBTYP2 = 0 THEN NFC1 = NFC1 + 2
    IF IBTYP1 = 0 AND IBTYP2 = 1 THEN NFC2 = NFC2 + 1
    IF IBTYP2 = 0 AND IBTYP1 = 1 THEN NFC2 = NFC2 + 1
    IF IBTYP1 = 1 AND IBTYP2 = 1 THEN STOP
    NFC = NFC + (1 - IBTYP1) + (1 - IBTYP2)
    IF ITYP1 < 0 THEN
        RBAR1 = -ITYP1
        ECC1 = 1
    ELSE
        RBAR1 = SHAPES(ITYP1, 1)
        ECC1 = SHAPES(ITYP1, 2)
        IF IBTYP1 <> 0 THEN ECC1 = 0
    END IF
    IF ITYP2 < 0 THEN
        RBAR2 = -ITYP2
        ECC2 = 1
    ELSE
        RBAR2 = SHAPES(ITYP2, 1)
        ECC2 = SHAPES(ITYP2, 2)
        IF IBTYP2 <> 0 THEN ECC2 = 0
    END IF
    DX1 = 0: DX2 = 0: DT1 = 0: DY1 = 0: DY2 = 0: DT2 = 0
    xFNA = FRN: FTA = 0: DN = 0: DT = 0
    '        IF ECC1 = 1 THEN GOTO SSSX
     CALL FORCES(XC1, YC1, RBAR1, ECC1, THETA1, XC2, YC2, RBAR2, ECC2, THETA2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, xFNA, FTA, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA, BDT _
, AMU, XYCN())
    IF IFLAG = 0 THEN
        IF FRN <> 0 THEN
            LOCATE 1, 1: PRINT IB1
            WHILE INKEY$ = "": WEND
            GOTO SSSX
        END IF
    END IF
    CVL1 = SQR(CVX1 ^ 2 + CVY1 ^ 2)
    CVL2 = SQR(CVX2 ^ 2 + CVY2 ^ 2)
    CSLN1 = (CNX1 * CVX1 + CNY1 * CVY1) / CVL1
    CSLN2 = -(CNX1 * CVX2 + CNY1 * CVY2) / CVL2
    IF IBTYP1 = 0 THEN
        LAV = LAV + CVL1
        CAV = CAV + CSLN1
        FAV = FAV + FRN
        S11 = S11 + FRN * CVX1 * CNX1
        S22 = S22 + FRN * CVY1 * CNY1
        CRL = CRL + FRN * CVL1
        FAVS = FAVS + FRN * FRN
        KIN = KIN + 1
    END IF
    IF IBTYP2 = 0 THEN
        LAV = LAV + CVL2
        CAV = CAV + CSLN2
        FAV = FAV + FRN
        FAVS = FAVS + FRN * FRN
        S11 = S11 - FRN * CVX2 * CNX1
        S22 = S22 - FRN * CVY2 * CNY1
        CRL = CRL + FRN * CVL2
        KIN = KIN + 1
    END IF
    'LOCATE 2, 1: PRINT FRN, IFLAG
    IF IFLAG <> 2 THEN
        '         STOP
        GOTO SSSX
    END IF
    Xcen = XYCN(1, 1): Ycen = XYCN(1, 2)
    CVL1 = COE * CVL1 * CSLN1
    CVL2 = COE * CVL2 * CSLN2
    CFX = FRN * CNX1 + FRT * CTX1
    CFY = FRN * CNY1 + FRT * CTY1
    CFL = SQR(CFX * CFX + CFY * CFY)
    CFX = CFX / CFL
    CFY = CFY / CFL
    TN = (FRC / FCAVG) * PNTMAX
    IF TN > PNTMAX THEN TN = PNTMAX
    XI = Xcen - CFX * CVL1: YI = Ycen - CFY * CVL1
    XF = Xcen + CFX * CVL2: Yf = Ycen + CFY * CVL2
    IIN% = 0: IFI% = 0
    IF XI > Xmin AND XI < Xmax AND YI > Ymin AND YI < Ymax THEN IIN% = 1
    IF XF > Xmin AND XF < Xmax AND Yf > Ymin AND Yf < Ymax THEN IFI% = 1
    IF IFI% + IIN% > 0 THEN
        '        IF NOPLT = 1 THEN
        CALL SLINE(XI, YI, XF, Yf, 12, TN, XYlim(), XYscr(), XYplt(), Contr())
        'LOCATE 1, 1: PRINT USING "#### #### ##.###^^^^ ##.###^^^^ ##.###^^^^ ##.###^^^^"; IB1, IB2, FRN, FRC, TN, FCAVG
        'WHILE INKEY$ = "": WEND
        '        END IF
        IF Contr(3) <> 0 THEN
            IF Xmaker(0) = 1 THEN
                CALL MSETLINE(PN)
                XI = XI * Xmaker(1) + Xmaker(2)
                YI = YI * Xmaker(3) + Xmaker(4)
                XF = XF * Xmaker(1) + Xmaker(2)
                Yf = Yf * Xmaker(3) + Xmaker(4)
                CALL MLINE(XI, YI, XF, Yf, 2, 0)
            ELSE
                PRINT #3, "newpath"
                PRINT #3, USING "##.## setlinewidth"; PN
                PRINT #3, USING "###.## ###.## moveto"; XI; YI
                PRINT #3, USING "###.## ###.## lineto"; XF; Yf
                PRINT #3, "stroke"
            END IF
        END IF
    END IF
END IF
SSSX:
IAD = IAD + NCPART
G$ = ""
IF G$ <> CHR$(27) GOTO NEXTforce
FINforces:
GAMMA = NFC / (NDISK - NBD)
IF KIN <> 0 THEN
    LAV = LAV / KIN
    CAV = CAV / KIN
    FAV = FAV / KIN
    FAVS = FAVS / KIN
    CRL = CRL / KIN
ELSE
    LAV = 0
    CAV = 0
    FAV = 0
    FAVS = 0
    CRL = 0
END IF
IF LAV <> 0 AND FAV <> 0 THEN
    ZLN = CRL / (LAV * FAV)
ELSE
    ZLN = 0
END IF
IF FACS <> 0 THEN
    ZFR = FAV * FAV / FAVS
ELSE
    ZFR = 0
END IF
'
LOCATE WDMODE, CMIN% - 8: PRINT USING "ZETlf = #.###"; ZLN;
LOCATE WDMODE, CMIN% + 8: PRINT USING "ZETff = #.###"; ZFR;
'WHILE INKEY$ = "": WEND
IF Contr(3) <> 0 THEN
    IF Xmaker(0) = 1 THEN
        PRINT #3, "<Group <ID 2>>"
    ELSE
        PRINT #3, "showpage"
    END IF
END IF
CALL BORDER(XYlim(), XYscr())
CALL SMALLPLOT(XYlim(), XYscr(), XYout(), Contr(), 3)
CALL CHECKAUTO
END SUB

SUB PlotLINKS (XYlim(), XYscr(), XYplt(), XYout(), Labl$(), Contr(), CNT%())
SHARED COORDC(), NDIST(), Trans()
SHARED ROIN%, CMIN%, WDMODE
DIM CLRS%(16)
IAD = M1 + NDPART * NDISK: DEBUG = 0
CLR = 12: IBMAX = M1 + NBD * NDPART
NFC = 0!
FOR I% = 1 TO NDISK: CNT%(I%) = 0: NEXT
NextLINK:
IB1 = AA(IAD)
IF IB1 = 0 GOTO FINcontacts
IB2 = AA(IAD + 1)
CALL GETCONT(IB1, IB2, IAD, X1, Y1, X2, Y2, FRN, FRT, DSN, DST, M1, NDPART)
MAXC% = 0
IF FRN <> 0 THEN
    LINE (X1, Y1)-(X2, Y2), CLR
    IF IB1 >= IBMAX THEN NFC = NFC + 1
    IF IB2 >= IBMAX THEN NFC = NFC + 1
    N1 = (IB1 - M1) / NDPART + 1
    N2 = (IB2 - M1) / NDPART + 1
    CNT%(N1) = CNT%(N1) + 1
    CNT%(N2) = CNT%(N2) + 1
    IF CNT%(N1) > MAXC% THEN MAXC% = CNT%(N1)
    IF CNT%(N2) > MAXC% THEN MAXC% = CNT%(N1)
END IF
IF DEBUG = 1 THEN
    WHILE INKEY$ = "": WEND
    LINE (X1, Y1)-(X2, Y2), 15
    WHILE INKEY$ = "": WEND
END IF
IAD = IAD + NCPART
GOTO NextLINK
'
FINcontacts:
'
WINDOW: VIEW: CLS
CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2)
Yoff = XYscr(3): Ylas = XYscr(4)
Trans(1) = (Xlas - Xoff) / (Xmax - Xmin)
Trans(3) = (Yoff - Ylas) / (Ymax - Ymin)
Trans(2) = Xoff - Trans(1) * Xmin
Trans(4) = Ylas - Trans(3) * Ymin
MAXN% = MAXC%
MAXC% = 12 - MAXC%
FOR N = 0 TO 16: NDIST(N) = 0: NEXT
NBD = 0
FOR N = 1 TO NDISK
    CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
    IF ITYPE < 0 THEN
        RR = -ITYPE
        NBD = NBD + 1
    ELSE
        RR = R(ITYPE)
    END IF
    IF IBTYP = 0 THEN
        NV% = CNT%(N)
        NDIST(NV%) = NDIST(NV%) + 1
        CLR = COORDC(NV%) + 1
        '        IF NV% = 0 THEN
        'LOCATE 1, 1: PRINT CLR
        'WHILE INKEY$ = "": WEND
        '        END IF
        CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, CLR)
    END IF
NEXT
GAMMA = NFC / (NDISK - NBD)
LOCATE ROIN%, CMIN%: PRINT USING "Gamma=#.###"; GAMMA
Trans(0) = 1
K% = ROIN% + 1
SC = 16 / (2 * RAVER * ABS(Trans(1)))
FOR M% = 0 TO 16
    IF NDIST(M%) <> 0 THEN
        K% = K% + 1
        YPi = K% * 16: YPf = YPi + 16
        XPi = CMIN% * 8: XPf = XPi + 16
        XPs = .5 * (XPi + XPf): YPs = .5 * (YPi + YPf)
        XPc = (XPs - Trans(2)) / Trans(1)
        YPc = (YPs - Trans(4)) / Trans(3)
        WINDOW: VIEW
        VIEW (XPi, YPi)-(XPf, YPf): CLS: VIEW
        CLR = COORDC(M%) + 1
        CALL PLOTELSsc(XPc, YPc, Tc, ITYPE, IBTYP, CLR, SC)
        LOCATE K% + 1, CMIN% + 4
        PRINT USING "#"; M%;
        PRINT " (";
        PRINT NDIST(M%);
        PRINT ")"
    END IF
NEXT
WINDOW: VIEW
Trans(0) = 0
FOR N = 1 TO NDISK
    CNT%(N) = 0
NEXT
CALL BORDER(XYlim(), XYscr())
CALL SMALLPLOT(XYlim(), XYscr(), XYout(), Contr(), 2)
CALL CHECKAUTO
END SUB

SUB PLOTpoly (XY(), CLR)
SHARED Trans(), Xmaker()
W% = Xmaker(0)
IF W% <> 0 THEN DIM XX(100), YY(100)
N = XY(0)
ING% = 1
IF N < 0 THEN
    ING% = -1
    N = -N
END IF
IF POLYSTYLE% = 1 AND ING% = -1 THEN
    EXIT SUB
END IF
N2 = 2 * N
KX = 1
KY = 2
XI = XY(KX)
YI = XY(KY)
FOR I = 1 TO N
    KX = KX + 2
    IF KX > N2 THEN KX = 1
    KY = KX + 1
    XF = XY(KX)
    Yf = XY(KY)
    IF DEBUG = 0 THEN
        IF ING% > 0 THEN
            LINE (XI, YI)-(XF, Yf), CLR
        ELSE
            LINE (XI, YI)-(XF, Yf), 7, , 3000
        END IF
    ELSE
        LINE (XI, YI)-(XF, Yf), 15
        LOCATE 3, 1: PRINT USING "####.#### ####.#### ####.#### ####.####"; XI, YI, XF, Yf
        DELAY ("")
    END IF
    IF W% <> 0 THEN
        XX(I) = XY(KX) * Xmaker(1) + Xmaker(2)
        YY(I) = XY(KY) * Xmaker(3) + Xmaker(4)
    END IF
    XI = XF
    YI = Yf
NEXT
IF W% <> 0 THEN
    N% = N
    CALL MPGON(XX(), YY(), N%, 15, 1)
    ERASE XX, YY
END IF
END SUB

SUB PLOTpolyP (XY(), CLRB, CLRP)
SHARED Trans(), Xmaker()
W% = Xmaker(0)
IF W% <> 0 THEN DIM XX(100), YY(100)
N = XY(0)
ING% = 1
IF N < 0 THEN
    ING% = -1
    N = -N
END IF
IF POLYSTYLE% = 1 AND ING% = -1 THEN
    EXIT SUB
END IF
againp:
N2 = 2 * N
KX = 1
KY = 2
XI = XY(KX)
YI = XY(KY)
FOR I = 1 TO N
    KX = KX + 2
    IF KX > N2 THEN KX = 1
    KY = KX + 1
    XF = XY(KX)
    Yf = XY(KY)
    IF DEBUG = 0 THEN
        LINE (XI, YI)-(XF, Yf), 0
    END IF
    XI = XF
    YI = Yf
NEXT
IF CLRP < 0 THEN
    EXIT SUB
END IF
Xg = 0.0
CALL InsidePoint(XY(), Xg, Yg)
PAINT (Xg, Yg), 0, 0
N2 = 2 * N
KX = 1
KY = 2
XI = XY(KX)
YI = XY(KY)
FOR I = 1 TO N
    KX = KX + 2
    IF KX > N2 THEN KX = 1
    KY = KX + 1
    XF = XY(KX)
    Yf = XY(KY)
    IF DEBUG = 0 THEN
        LINE (XI, YI)-(XF, Yf), CLRB
    END IF
    XI = XF
    YI = Yf
NEXT
'
NBACKM = 10
NBACK = 0
Xg = 0.0
ANOTHERP:
CALL InsidePoint(XY(), Xg, Yg)
M = POINT(Xg, Yg)
IF M = CLRP OR M = CLRB THEN
    NBACK = NBACK + 1
    IF NBACK < NBACKM THEN
        Xg = NBACK / NBACKM
        GOTO ANOTHERP
    ELSE
        'LOCATE 2, 1: PRINT "AGAIN PAINT"; M, Xg
        'WHILE INKEY$ = "": WEND
        CLRP = -1
        GOTO againp
    END IF
END IF
PAINT (Xg, Yg), CLRP, CLRB
IF W% <> 0 THEN
    N% = N
    CALL MPGON(XX(), YY(), N%, 15, 1)
    ERASE XX, YY
END IF
END SUB

SUB PLOTpolyt (XY(), CLR)
SHARED Trans()
N = XY(0)
ING% = 1
IF N < 0 THEN
    ING% = -1
    N = -N
END IF
N2 = 2 * N
KX = 1
KY = 2
XI = XY(KX)
YI = XY(KY)
FOR I = 1 TO N
    KX = KX + 2
    IF KX > N2 THEN KX = 1
    KY = KX + 1
    XF = XY(KX)
    Yf = XY(KY)
    XiS = XI * Trans(1) + Trans(2)
    YiS = YI * Trans(3) + Trans(4)
    XfS = XF * Trans(1) + Trans(2)
    YfS = Yf * Trans(3) + Trans(4)
    LINE (XiS, YiS)-(XfS, YfS), CLR
    XI = XF
    YI = Yf
NEXT
END SUB

SUB Qmouse (m0%, m1%, m2%, m3%) STATIC
IF m0% = 0 THEN
    _MOUSESHOW: _DELAY 0.05: _DELAY 0.05
    m0% = -1
    RepMs:
    ml% = _MOUSEINPUT
    IF ml% = -1 THEN
        m1% = _MOUSEBUTTON(1)
        m2% = _MOUSEX
        m3% = _MOUSEY
        IF m1% = -1 THEN
            GOTO RepMs
        END IF
    END IF
END IF
IF m0% = 1 THEN
    _MOUSESHOW: _DELAY 0.05
END IF
IF m0% = 2 THEN
    _MOUSEHIDE: _DELAY 0.05
END IF
IF m0% = 3 THEN
    ml% = _MOUSEINPUT
    IF ml% = -1 THEN
        m2% = _MOUSEX
        m3% = _MOUSEY
        m1% = _MOUSEBUTTON(1)
        IF m1% = -1 THEN
            m1% = 1
            EXIT SUB
        END IF
        m1% = _MOUSEBUTTON(2)
        IF m1% = -1 THEN
            m1% = 2
            EXIT SUB
        END IF
        m1% = _MOUSEBUTTON(3)
        IF m1% = -1 THEN
            m1% = 3
            EXIT SUB
        END IF
    END IF
END IF
IF m0% = 4 THEN
    _MOUSEMOVE m2%, m3%: _DELAY 0.05
END IF
END SUB

SUB READNUMZZ (PS1, Z$)
SHARED LNW%, FW%, RN, RNZ, FLZ$
'gives number in position ps
PS = PS1 + 1
BT = (PS - 1) * LNW% + 1
RN = INT((BT - 1) / FW%) + 1
PSR = BT - (RN - 1) * FW%
IF RN <> RNZ THEN
    GET #6, RN
    RNZ = RN
END IF
Z$ = MID$(FLZ$, PSR, LNW%)
'F$ = ""
'FOR N% = 1 TO 4
'F$ = F$ + MID$(Z$, 5 - N%, 1)
'NEXT
'Z$ = F$
END SUB

SUB READNUM (PS1, Z$)
SHARED LNW%, FW%, RN, RNP, FLD$
'gives number in position ps
PS = PS1 + 1
BT = (PS - 1) * LNW% + 1
RN = INT((BT - 1) / FW%) + 1
PSR = BT - (RN - 1) * FW%
IF RN <> RNP THEN
    GET #1, RN
    RNP = RN
END IF
Z$ = MID$(FLD$, PSR, LNW%)
'F$ = ""
'FOR N% = 1 TO 4
'F$ = F$ + MID$(Z$, 5 - N%, 1)
'NEXT
'Z$ = F$
END SUB

SUB READSTR (PS1, NS, Z$)
SHARED LNW%, FW%, RN, RNP, FLD$
Z$ = ""
'gives string in the position ps
FOR K% = 1 TO NS / 8
    PS = PS1 + K%
    BT = (PS - 1) * LNW% + 1
    RN = INT((BT - 1) / FW%) + 1
    PSR = BT - (RN - 1) * FW%
    IF RN <> RNP THEN
        GET #1, RN
        RNP = RN
    END IF
    Z$ = Z$ + MID$(FLD$, PSR, LNW%)
NEXT
END SUB

SUB ResizeWindow (XYlim(), XYscr(), XYout(), Contr(), RESIZED)
LOCATE 1, 6: PRINT "Esc - Quit ; R - Restore"
'
ResizeLOC:
'
CALL LOCATOR(XYlim(), XYscr(), XYout(), G$, Contr())
IF G$ = CHR$(27) THEN
    LOCATE 1, 6: PRINT "                        "
    RESIZED = -1 'Do nothing
    EXIT SUB
END IF
IF G$ = "R" OR G$ = "r" THEN
    RESIZED = 0 'Restore
    EXIT SUB
END IF
IF XYout(1) = XYout(2) AND XYout(3) = XYout(4) THEN
    GOTO ResizeLOC
END IF
XYlim(1) = XYout(1): XYlim(2) = XYout(2)
XYlim(3) = XYout(3): XYlim(4) = XYout(4)
RESIZED = 1 'Resize
END SUB

SUB REST (FL0$)
END SUB

SUB Retreve (PaddressF, HistNoY, NewYLoc, Sc, XaddressF, HistNoF, NewXLoc, SCX, Xar(), Yar(), Cfile0$)
SHARED NOBFILE, WDMODE, XDMODE, SPMODE, Lfile$
SCREEN SPMODE
Cfile$ = Cfile0$
'NewYloc <> 0 - Sequence No of item in the "NEW AREA"
'NewYloc  = 0 - Retrieved from HistNoY at PaddressF
'LOCATE 1, 1: PRINT XaddressF, HistNoF, NewXLoc, Sc
'LOCATE 2, 1: PRINT PaddressF, HistNoY, NewYLoc, SCX
'WHILE INKEY$ = "": WEND
IAD = 0
ILOGY = 0
ILOGX = 0
Deriv = 0
YMX = 0
FMC$ = ""
CLOSE #1
'
IF PaddressF = -999 THEN
    GOTO STRTRet
END IF
IF NewYLoc <> 0 AND HistNoY <> 0 THEN
    NB = NewYLoc
    Yaddress = M1 + (NB - 1) * NDPART
    IF ABS(PaddressF) = 0 THEN
        INC = 0
        IINC = 11
    END IF
    IF ABS(PaddressF) = 1 THEN
        INC = 1
        IINC = 12
    END IF

    IF ABS(PaddressF) = 2 THEN
        INC = 2
        IINC = 0
    END IF

    IF ABS(PaddressF) = 3 THEN
        INC = 3
        IINC = 0
    END IF

    IF ABS(PaddressF) = 10 THEN
        INC = 10
        IINC = 13
    END IF
    IF ABS(PaddressF) = 5 THEN
        INC = 5
        IINC = 0
    END IF
    IF ABS(PaddressF) = 6 THEN
        INC = 6
        IINC = 0
    END IF
    IF ABS(PaddressF) = 7 THEN
        INC = 7
        IINC = 0
    END IF
END IF
Deriv = 0

STRTRet: K = 0
NEWFILE = 1
REPRet: NOBFILE = 0
IF NEWFILE = 0 THEN
    NOBFILE = 1
    GOTO SCKCHK2
END IF
CALL CHECKfile(Cfile$, NOBFILE)
SCKCHK2:
IF NOBFILE = 1 THEN
    IF Deriv <> 0 THEN
        FOR I = 1 TO K - 1
            Dx = Xar(1, I + 1) - Xar(1, I)
            Dy = Yar(1, I + 1) - Yar(1, I)
            Yar(1, I) = Dy / Dx
        NEXT
        K = K - 1
    END IF
    Xar(1, 0) = K
    Yar(1, 0) = K
    LOCATE WDMODE, 6 ': PRINT STRING$(XDMODE - 7, " ");
    EXIT SUB
ELSE
    CALL INITfile(Cfile$)
    LOCATE WDMODE, 6: PRINT Cfile$; ": ";
    K = K + 1
    Xar(0, K) = CURFILE%
    Yar(0, K) = CURFILE%
    IF NewYLoc <> 0 AND HistNoY <> 0 THEN
        IF PaddressF = -3 OR PaddressF = -6 THEN
            Yar(1, K) = SQR(AA(Yaddress + INC) ^ 2 + AA(Yaddress + INC - 1) ^ 2) * Sc
            IF NewXLoc <> 0 THEN
                Xaddress = RECMAX + 2 * (NewXLoc - 1) + 1
                Xar(1, K) = AA(Xaddress) * SCX
            ELSE
                Xar(1, K) = K
            END IF
        ELSE
            IF IINC <> 0 THEN
                Yar(1, K) = (AA(Yaddress + INC) + AA(Yaddress + IINC)) * Sc
                IF PaddressF = -1 THEN
                    Xar(1, K) = (AA(Yaddress + INC - 1) + AA(Yaddress + IINC - 1)) * Sc
                ELSE
                    IF HistNoF <> 0 THEN
                        Xaddress = RECHIS + (HistNoF - 1) * 47 + 40 + XaddressF
                        Xar(1, K) = AA(Xaddress) * SCX
                    ELSE
                        Xar(1, K) = K
                    END IF
                END IF
            ELSE
                Yar(1, K) = AA(Yaddress + INC) * Sc
                IF Paddress < 0 THEN
                    Xar(1, K) = AA(Yaddress + INC - 1) * Sc
                ELSE
                    IF HistNoF <> 0 THEN
                        Xaddress = RECHIS + (HistNoF - 1) * 47 + 40 + XaddressF
                        Xar(1, K) = AA(Xaddress) * SCX
                    ELSE
                        Xar(1, K) = K
                    END IF
                END IF
            END IF
        END IF
    ELSE
        IF NewYLoc <> 0 THEN
            Yaddress = RECMAX + 2 * (NewYLoc - 1) + 1
            IF NewYLoc = 10 THEN
                UPT = 0
                CALL CLOOP(UPT)
                AREA = AA(Yaddress) * Sc
                Yar(1, K) = UPT / AREA
            ELSE
                Yar(1, K) = AA(Yaddress) * Sc
            END IF
            IF NewXLoc <> 0 THEN
                Xaddress = RECMAX + 2 * (NewXLoc - 1) + 1
                Xar(1, K) = AA(Xaddress) * SCX
            ELSE
                IF HistNoF <> 0 THEN
                    Xaddress = RECHIS + (HistNoF - 1) * 47 + 40 + XaddressF
                    Xar(1, K) = AA(Xaddress) * SCX
                ELSE
                    Xar(1, K) = K
                END IF
            END IF
        ELSE
            Yaddress = RECHIS + (HistNoY - 1) * 47 + 40 + PaddressF
            Yar(1, K) = AA(Yaddress) * Sc
            IF NewXLoc <> 0 THEN
                Xaddress = RECMAX + 2 * (NewXLoc - 1) + 1
                Xar(1, K) = AA(Xaddress) * SCX
            ELSE
                IF HistNoF <> 0 THEN
                    Xaddress = RECHIS + (HistNoF - 1) * 47 + 40 + XaddressF
                    Xar(1, K) = AA(Xaddress) * SCX
                ELSE
                    Xar(1, K) = K
                END IF
            END IF
        END IF
    END IF
    IF ABS(Yar(1, K)) > YMX THEN
        YMX = Yar(1, K)
        FMC$ = Cfile$
    END IF
    PRINT USING " #.####^^^^ #.####^^^^ (#.####^^^^ "; Xar(1, K), Yar(1, K), YMX;
    PRINT ")";
    'WHILE INKEY$ = "": WEND
    '
    IF ILOGX <> 0 THEN
        IF Xar(1, K) <> 0 THEN
            Xar(1, K) = LOG(ABS(Xar(1, K))) / LOG(10!)
        ELSE
            K = K - 1
        END IF
    END IF
    IF ILOGY <> 0 THEN
        IF Yar(1, K) <> 0 THEN
            Yar(1, K) = LOG(ABS(Yar(1, K))) / LOG(10!)
        ELSE
            K = K - 1
        END IF
    END IF
    NXTRET:
    CLOSE #1
    Lfile$ = Cfile$
    CALL MakeNext(Cfile$, NEWFILE)
    GOTO REPRet
END IF
END SUB

SUB RetreveH (Paddress, AR(), AAr(), Sc, Cfile$, ADI)
K = 0
INCR = 35
IF Paddress < 0 THEN
    INCR = 20
    Paddress = -Paddress
END IF
FOR I = Paddress TO Paddress + INCR
    K = K + 1
    AR(K) = AA(I) * Sc
NEXT
NV = 4
IF ADI < 0 THEN
    NV = 3
    ADI = -ADI
END IF
AR(37) = NV
FOR I = 1 TO NV
    AR(37 + I) = AA(ADI + I - 1)
NEXT
END SUB

SUB ReviewContacts
SHARED SMIN%, Trans(), ASPECT, XSPMODE, YSPMODE, SPMODE, WDMODE, CMIN%, SHAPES(), ND
DIM XYlim(10), XYscr(10), XYplt(10), Labl$(310), Contr(10), XYCN(4, 2)
DIM XYP(30)
NDR = ND
ND = 3600
AVG = AA(136)
WINDOW: VIEW: CLS
AUTO = 0
'LOCATE 1, 6: INPUT "Autoamatic mode (Y/N) "; IWHAT$
IWHAT$ = "Y"
IF UCASE$(IWHAT$) = "Y" THEN AUTO = 1
CALL CLEAN
WDL = XSPMODE - (SMIN% + 1): HDL = WDL / ASPECT
WDO = SMIN% - 1: HDO = YSPMODE - HDL - 40
XYlim(0) = 0
XYlim(1) = XMINA: XYlim(2) = XMAXA: XYlim(3) = YMINA: XYlim(4) = YMAXA: XYlim(10) = SPMODE
XYlim(8) = XSPMODE: XYlim(9) = YSPMODE
XYscr(1) = WDL: XYscr(2) = HDL: XYscr(3) = WDO: XYscr(4) = HDO: XYscr(5) = 2
Contr(1) = -1
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
HDLL = XYscr(4) - XYscr(3)
Trans(1) = (XYscr(2) - XYscr(1)) / (XYlim(2) - XYlim(1))
Trans(3) = (XYscr(3) - XYscr(4)) / (XYlim(4) - XYlim(3))
Trans(2) = XYscr(1) - Trans(1) * XYlim(1)
Trans(4) = XYscr(4) - Trans(3) * XYlim(3)
CALL PlotBalls(XYlim(), XYscr())
PCOPY 0, 1
XYscr(1) = 0: XYscr(2) = 0
XYscr(3) = 0: XYscr(4) = 0
NC = 0
IAD = M1 + NDISK * NDPART
REVIEWrep:
IB1 = AA(IAD)
IF IB1 = 0 GOTO FINreview
IB2 = AA(IAD + 1)
NC = NC + 1
CALL GETDISC(-IB1, XC1, YC1, THETA1, ITYP1, IBTYP1)
CALL GETDISC(-IB2, XC2, YC2, THETA2, ITYP2, IBTYP2)
FB1X = AA(IB1 + 5): FB1Y = AA(IB1 + 6)
FB1 = SQR(FB1X * FB1X + FB1Y * FB1Y) / AVG
FB2X = AA(IB2 + 5): FB2Y = AA(IB2 + 6)
FB2 = SQR(FB2X * FB2X + FB2Y * FB2Y) / AVG
N1 = (IB1 - M1) / NDPART + 1
N2 = (IB2 - M1) / NDPART + 1
Xmin = XC1: IF Xmin > XC2 THEN Xmin = XC2
Xmax = XC1: IF Xmax < XC2 THEN Xmax = XC2
Ymin = YC1: IF Ymin > YC2 THEN Ymin = YC2
Ymax = YC1: IF Ymax < YC2 THEN Ymax = YC2
IF ITYP1 < 0 THEN
    RBAR1 = -ITYP1
    ECC1 = 1
ELSE
    RBAR1 = SHAPES(ITYP1, 1)
    ECC1 = SHAPES(ITYP1, 2)
END IF
IF ITYP2 < 0 THEN
    RBAR2 = -ITYP2
    ECC2 = 1
ELSE
    RBAR2 = SHAPES(ITYP2, 1)
    ECC2 = SHAPES(ITYP2, 2)
END IF
Rmax = RBAR1: IF Rmax < RBAR2 THEN Rmax = RBAR2
XYlim(1) = Xmin - Rmax
XYlim(2) = Xmax + Rmax
XYlim(3) = Ymin - Rmax
XYlim(4) = Ymax + Rmax
ASPECT = WDL / HDL
DX = XYlim(2) - XYlim(1): DY = XYlim(4) - XYlim(3)
DD = DX: IF DD < DY THEN DD = DY
XXc = 0.5 * (XYlim(1) + XYlim(2))
YYc = 0.5 * (XYlim(3) + XYlim(4))
'XYlim(1) = XXc - 0.5 * DD * ASPECT: XYlim(2) = XXc + 0.5 * DD * ASPECT
'XYlim(3) = YYc - 0.5 * DD: XYlim(4) = YYc + 0.5 * DD
DX1 = 0: DX2 = 0: DT1 = 0: DY1 = 0: DY2 = 0: DT2 = 0
FRN = 0: FRT = 0: DN = 0: DT = 0
CALL FORCES(XC1, YC1, RBAR1, ECC1, THETA1, XC2, YC2, RBAR2, ECC2, THETA2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, FRN, FRT, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA, BDT,  _
AMU, XYCN())
IF IFLAG = 0 AND AA(IAD + 4) <> 0 THEN
    BEEP
    CALL DISPLAY("Warning: Binary inconsistent")
END IF
IF IFLAG = 2 THEN
    PN1 = XYCN(4, 1)
    PN2 = XYCN(4, 2)
    PNC = PN1 + PN2
    PNA = AA(IAD + 2)
    IF PNA <> 0 THEN
        DEL = 100 * ABS(PNC - PNA) / PNA
    ELSE
        DEL = 999
    END IF
    'IF DEL > .01 AND AUTO = 1 THEN BEEP: BEEP: BEEP
    LOCATE 1, 6: PRINT "NC="; NC;
    PRINT USING " DEL ###.#### (%) [PN1 = ##.#### PN2 = ##.#### PNC ##.#### PNA ##.#### ] ECC1 = ##.## ECC2 = ##.##"; DEL; PN1; PN2; PNC; PNA; ECC1; ECC2
ELSE
    PN1 = 0.0
    PN2 = 0.0
    PNC = PN1 + PN2
    PNA = AA(IAD + 2)
    DEL = 999
    LOCATE 1, 6: PRINT "NC="; NC;
    PRINT USING " DEL ###.#### (%) [PN1 = ##.#### PN2 = ##.#### PNC ##.#### PNA ##.#### ] ECC1 = ##.## ECC2 = ##.##"; DEL; PN1; PN2; PNC; PNA; ECC1; ECC2
END IF
IF IFLAG = 2 THEN L$ = "Contact " ELSE L$ = "Link "
XYlim(8) = XSPMODE: XYlim(9) = YSPMODE: XYlim(10) = SPMODE
Contr(1) = -1 ': Contr(2) = -1
XYscr(1) = WDL: XYscr(2) = HDL: XYscr(3) = WDO: XYscr(4) = HDO - HDLL: XYscr(5) = 0
Contr(1) = -1
CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
CALL PLOTELS(XC1, YC1, THETA1, ITYP1, IBTYP1, 9)
CALL PLOTELS(XC2, YC2, THETA2, ITYP2, IBTYP2, 9)
CALL BORDER(XYlim(), XYscr())
WINDOW: VIEW: Trans(0) = 1
CALL PLOTELS(XC1, YC1, THETA1, ITYP1, IBTYP1, 15)
CALL PLOTELS(XC2, YC2, THETA2, ITYP2, IBTYP2, 15)
Trans(0) = 0
'
DN = AA(IAD + 2)
DS = AA(IAD + 3)
FDN = AA(IAD + 4)
FDS = AA(IAD + 5)
IAD = IAD + NCPART
IF IFLAG = 2 THEN
    Labl$(3) = L$ + " between particles" + STR$(N1) + " and" + STR$(N2)
    Labl$(1) = "X": Labl$(2) = "Y"
    Contr(1) = -1 ': Contr(2) = -1
    XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 40: XYscr(3) = 40: XYscr(4) = 0
    Contr(1) = -1
    Xc = XYCN(1, 1)
    Yc = XYCN(1, 2)
    X1 = XYCN(2, 1)
    Y1 = XYCN(2, 2)
    X2 = XYCN(3, 1)
    Y2 = XYCN(3, 2)
    DX = ABS(X2 - X1)
    DY = ABS(Y2 - Y1)
    DD = DX
    IF DD < DY THEN DD = DY
    XYlim(1) = Xc - DD: XYlim(2) = Xc + DD
    XYlim(3) = Yc - DD: XYlim(4) = Yc + DD
    CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    CALL PLOTELS(XC1, YC1, THETA1, ITYP1, IBTYP1, 9)
    CALL PLOTELS(XC2, YC2, THETA2, ITYP2, IBTYP2, 9)
    LINE (X1, Y1)-(X2, Y2), 15
    PSET (Xc, Yc), 12
    Labl$(1) = "": Labl$(2) = "": Labl$(3) = ""
    WINDOW: VIEW
END IF
'IF AUTO = 0 THEN
'    CALL DELAY(G$)
'END IF
WHILE INKEY$ = "": WEND
PCOPY 1, 0
GOTO REVIEWrep:
FINreview:
CALL CLEAN
LOCATE 1, 1: PRINT "FINISHED"
WHILE INKEY$ = "": WEND
ND = NDR
END SUB

SUB RewriteArray (NFILE$)
CALL CLEAN
LOCATE 1, 6: INPUT "ENTER FILE NAME [d:new.bin]"; NFILE$
IF NFILE$ = "" THEN NFILE$ = "d:new.bin"
CLOSE #1
CALL INITfile(NFILE$)
'
CALL DISPLAY("Writing basic information")
CALL AAI(1, M1)
CALL AAI(2, NDISK)
CALL AAI(3, H)
CALL AAI(4, NBOX)
CALL AAI(5, TOL)
CALL AAI(6, MODE)
CALL AAI(7, NTYPE)
CALL AAI(8, STIF)
CALL AAI(9, XLAMBDA)
CALL AAI(10, AMU)
CALL AAI(11, Ecc)
CALL AAI(12, DENSITY)
CALL AAI(13, NN)
CALL AAI(14, DELBOX)
CALL AAI(15, NXB)
CALL AAI(16, NYB)
CALL AAI(17, NDPART)
CALL AAI(18, NCPART)
CALL AAI(19, TOLNEXT)
CALL AAI(20, RECMAX)
CALL AAI(21, XMINA)
CALL AAI(22, XMAXA)
CALL AAI(23, YMINA)
CALL AAI(24, YMAXA)
'
IAD = 50
K = 0
FOR I = 1 TO NTYPE
    NEDGE = SHAPES(I, 0)
    CALL AAI(IAD, NEDGE)
    IAD = IAD + 1
    FOR K = 1 TO NEDGE
        XED = SHAPES(I, 2 * K - 1)
        CALL AAI(IAD, XED)
        IAD = IAD + 1
        YED = SHAPES(I, 2 * K)
        CALL AAI(IAD, YED)
        IAD = IAD + 1
    NEXT
NEXT
'
CALL DISPLAY("Writing disk partition I =")
FOR I = 1 TO NDISK
    LOCATE 1, 32: PRINT USING "####"; I
    FOR J = 0 TO NDPART - 1
        VL = DLIST(I, J)
        IAD = M1 + (I - 1) * NDPART + J
        CALL AAI(IAD, VL)
    NEXT
NEXT
CALL DISPLAY("Writing contact partition I =")
NC = 0
IAD = M1 + NDPART * NDISK
NCONT = NCONT + 1
FOR J = 0 TO NCPART - 1
    CALL CLISTi(NCONT, J, 0)
NEXT
FOR I = 1 TO NCONT
    LOCATE 1, 36: PRINT USING "####"; I
    IF CLIST(I, 0) >= 0 AND CLIST(I, 1) >= 0 THEN
        FOR J = 0 TO NCPART - 1
            VL = CLIST(I, J)
            CALL AAI(IAD + J, VL)
        NEXT
        IAD = IAD + NCPART
    END IF
NEXT
NB = M1 + NDISK * NDPART + NCONT * NCPART
CALL AAI(0, NB)
CALL AAI(20, NB)
CLOSE #1
CALL CLEAN
'
END SUB

SUB RTFOUR (A#, B#, C#, D#, E#, K, RT#(), ERT#(), EPS#, EPSC#)
ONE# = 1#
TWO# = 2#
THREE# = 3#
FOUR# = 4#
EIGHT# = 8#
ZERO# = 0#
HALF# = .5#
QUARTER# = .25#
pi# = 3.14159265358979#
K = 0
IF ABS(A#) < EPS# AND ABS(B#) < EPS# THEN
    P# = HALF# * D# / C#
    Q# = E# / C#
    DIS# = P# * P# - Q#
    IF DIS# < ZERO# THEN
        K = 0
    ELSE
        RT#(1) = -P# + SQR(DIS#)
        RT#(2) = -P# - SQR(DIS#)
        K = 2
    END IF
    EXIT SUB
END IF
AR# = B# / A#
BR# = C# / A#
CR# = D# / A#
DR# = E# / A#
AI# = EIGHT#
BI# = -FOUR# * BR#
CI# = TWO# * (AR# * CR# - FOUR# * DR#)
DI# = -(CR# * CR# + DR# * (AR# * AR# - FOUR# * BR#))
BP# = BI# / THREE#
CP# = CI# / THREE#
Q# = AI# * CP# - BP# * BP#
R# = HALF# * (THREE# * AI# * BP# * CP# - AI# * AI# * DI#) - BP# * BP# * BP#
DS# = Q# * Q# * Q# + R# * R#
IF Q# <= ZERO# THEN
    IF DS# <= ZERO# THEN
        AR0# = -Q# * Q# * Q#
        ARG# = R# / SQR(AR0#)
        TNX# = SQR(ONE# - ARG# * ARG#) / ARG#
        IF ARG# > ZERO# THEN
            ACS# = ATN(TNX#)
        ELSE
            ACS# = pi# - ATN(ABS(TNX#))
        END IF
        ANG# = ACS# / THREE#
        COE# = TWO# * SQR(-Q#)
        Y1# = COE# * COS(ANG#)
        R1# = (Y1# - BP#) / AI#
    ELSE
        AR0# = -Q# * Q# * Q#
        ARG# = R# / SQR(AR0#)
        IF ARG# >= ZERO# THEN
            ACS# = LOG(ARG# + SQR(ARG# * ARG# - ONE#))
            ANG# = ACS# / THREE#
            COE# = TWO# * SQR(-Q#)
            Y1# = COE# * HALF# * (EXP(ANG#) + EXP(-ANG#))
            R1# = (Y1# - BP#) / AI#
        ELSE
            XX# = ABS(ARG#)
            RS# = LOG(XX# + SQR(XX# * XX# + ONE#))
            RS# = -RS#
            ASN# = RS#
            ANG# = ASN# / THREE#
            COE# = TWO# * SQR(-Q#)
            Y1# = COE# * HALF# * (EXP(ANG#) - EXP(-ANG#))
            R1# = (Y1# - BP#) / AI#
        END IF
    END IF
ELSE
    AR0# = Q# * Q# * Q#
    IF ABS(R#) < EPS# AND ABS(AR0#) < EPS# THEN
        ARG# = ZERO#
    ELSE
        ARG# = R# / SQR(AR0#)
    END IF
    XX# = ABS(ARG#)
    RS# = LOG(XX# + SQR(XX# * XX# + ONE#))
    IF ARG# < ZERO# THEN RS# = -RS#
    ASN# = RS#
    ANG# = ASN# / THREE#
    COE# = TWO# * SQR(Q#)
    Y1# = COE# * HALF# * (EXP(ANG#) - EXP(-ANG#))
    R1# = (Y1# - BP#) / AI#
END IF
DS1# = QUARTER# * AR# * AR# + TWO# * R1# - BR#
DS2# = R1# * R1# - DR#
IF DS1# < ZERO# OR DS2# < ZERO# THEN
    K = 0
    EXIT SUB
END IF
T1# = HALF# * AR#
T2# = SQR(DS1#)
P1# = HALF# * (T1# - T2#)
P3# = P1#
P2# = HALF# * (T1# + T2#)
P4# = P2#
T3# = SQR(DS2#)
Q1# = R1# + T3#
Q3# = R1# - T3#
Q2# = R1# - T3#
Q4# = R1# + T3#
DIS1# = P1# * P1# - Q1#
DIS2# = P2# * P2# - Q2#
DIS3# = P3# * P3# - Q3#
DIS4# = P4# * P4# - Q4#
IF ABS(DIS1#) <= EPS# THEN DIS1# = ZERO#
IF ABS(DIS2#) <= EPS# THEN DIS2# = ZERO#
IF ABS(DIS3#) <= EPS# THEN DIS3# = ZERO#
IF ABS(DIS4#) <= EPS# THEN DIS4# = ZERO#
K = 0
IF DIS1# >= ZERO# THEN
    T4# = SQR(DIS1#)
    R1# = -P1# + T4#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
    R1# = -P1# - T4#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
END IF
IF DIS2# >= ZERO# THEN
    T5# = SQR(DIS2#)
    R1# = -P2# + T5#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
    R1# = -P2# - T5#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
END IF
IF DIS3# >= ZERO# THEN
    T4# = SQR(DIS3#)
    R1# = -P3# + T4#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
    R1# = -P3# - T4#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
END IF
IF DIS4# >= ZERO# THEN
    T5# = SQR(DIS4#)
    R1# = -P4# + T5#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
    R1# = -P4# - T5#
    R2# = R1# * R1#
    R3# = R2# * R1#
    R4# = R3# * R1#
    CHK# = A# * R4# + B# * R3# + C# * R2# + D# * R1# + E#
    IF ABS(CHK#) < EPSC# THEN
        IF K < 4 THEN
            K = K + 1
            RT#(K) = R1#
            ERT#(K) = ABS(CHK#)
        END IF
    END IF
END IF
IF K > 2 THEN
    CALL SORT(K, ERT#(), RT#())
END IF
'     ERASE ERT#
EXIT SUB
END SUB

SUB SCREDIT (XYlim(), XYscr(), XYplt(), Labl$(), Par$(), Contr(), XDAT(), YDAT(), NA, NS, ID%())
END SUB

SUB SearchBALL (Xo, Yo, Tc, N, Xc, Yc, ITYPE, IBTYP, FOUND)
DIM NBSAV(20), SPLIT(20), XYT(50), XY(50)
NOBD = 0
IF IBTYP < 0 THEN
    NOBD = 1
END IF
IF ABS(Xo) < 1.E-3 AND ABS(Yo) < 1.0E-3 THEN
    FFmax = 0
    FOR N = 1 TO NDISK
        IAD = M1 + (N - 1) * NDPART
        IBTYP = AA(IAD + 8)
        ITYPE = AA(IAD + 9)
        IF IBTYP = 0 THEN
            Fx = AA(IAD + 5): Fy = AA(IAD + 6): FF = SQR(Fx * Fx + Fy * Fy)
            IF FF > FFmax THEN
                FFmax = FF
                FOUND = N
                IADF = IAD
            END IF
        END IF
    NEXT
    Xc = AA(IADF) + AA(IADF + 11)
    Yc = AA(IADF + 1) + AA(IADF + 12)
    Tc = AA(IADF + 10) + AA(IADF + 13)
    IBTYP = AA(IADF + 8)
    ITYPE = AA(IADF + 9)
    EXIT SUB
END IF
'
RBAR = R(1): Ecc = 0: FOUND = 0
CALL BOX(Xo, Yo, RBAR, Ecc, NBSAV(), NBMAP, SPLIT())
FOR I = 1 TO NBMAP
    NB = NBSAV(I)
    NP = BCONT(NB, 0)
    FOR J = 1 TO NP
        N = BCONT(NB, J)
        CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
        D = SQR((Xc - Xo) ^ 2 + (Yc - Yo) ^ 2)
        IF ITYPE < 0 THEN
            RB = -2 * ITYPE
            IF NOBD = 1 THEN RB = 0
        ELSE
            RB = R(ITYPE)
        END IF
        IF D < RB THEN
            IF ITYPE < 0 THEN
                FOUND = N
                EXIT SUB
            END IF
            NN = SHAPES(ITYPE, 0)
            IF NN > 1 THEN
                FOR K = 0 TO 2 * NN
                    XY(K) = SHAPES(ITYPE, K)
                NEXT
                CALL TRNPOLY(XY(), XYT(), Tc, 0!, 0!)
                CALL CHECKinside(XYT(), Xc - Xo, Yc - Yo, INSIDE)
            ELSE
                INSIDE = 1
            END IF
            IF INSIDE = 1 THEN
                FOUND = N
                EXIT SUB
            END IF
        END IF
    NEXT
NEXT
END SUB

SUB SLINE (X1, Y1, X2, Y2, CLR%, TH, XYlim(), XYscr(), XYplt(), Contr())
pi = 3.1415926#: DTR = pi / 180
Dx = X2 - X1: Dy = Y2 - Y1: D = SQR(Dx * Dx + Dy * Dy)
NSEP = 50
SN = Dy / D: CS = Dx / D
Separ = TH
P% = Contr(3): CLR = XYscr(6)
Lscrx = 25.4: Lscry = Lscrx * .75
Plxscr = 640: Plyscr = 480
Pxcmscr = Plxscr / Lscrx: Pycmscr = Plyscr / Lscry
Puxcm = (XYlim(2) - XYlim(1)) * Pxcmscr / (XYscr(2) - XYscr(1))
Puycm = (XYlim(4) - XYlim(3)) * Pycmscr / (XYscr(4) - XYscr(3))
DEL = Separ * Puxcm * Puycm / SQR((Puxcm * SN) ^ 2 + (Puycm * CS) ^ 2)
SN = Dx / D
CS = -Dy / D
DEL = DEL / (2 * NSEP + 1)
Xc = .5 * (X1 + X2): Yc = .5 * (Y1 + Y2)
FOR K = -NSEP TO NSEP
    XP1I = X1 - K * CS * DEL
    YP1I = Y1 - K * SN * DEL
    XP2I = X2 - K * CS * DEL
    YP2I = Y2 - K * SN * DEL
    LINE (XP1I, YP1I)-(XP2I, YP2I), CLR%
NEXT
END SUB

SUB SORT (N, RA#(), BB#())
L = N / 2 + 1
IR = N
S10:
IF L > 1 THEN
    L = L - 1
    RRA# = RA#(L)
    AAA# = BB#(L)
ELSE
    RRA# = RA#(IR)
    AAA# = BB#(IR)
    RA#(IR) = RA#(1)
    BB#(IR) = BB#(1)
    IR = IR - 1
    IF IR = 1 THEN
        RA#(1) = RRA#
        BB#(1) = AAA#
        EXIT SUB
    END IF
END IF
I = L
J = L + L
S20: IF J <= IR THEN
    IF J < IR THEN
        IF RA#(J) < RA#(J + 1) THEN J = J + 1
    END IF
    IF RRA < RA#(J) THEN
        RA#(I) = RA#(J)
        BB#(I) = BB#(J)
        I = J
        J = J + J
    ELSE
        J = IR + 1
    END IF
    GOTO S20
END IF
RA#(I) = RRA#
BB#(I) = AAA#
GOTO S10
END SUB

SUB SUMMARY (IPRINT, PPOS, Cfile$)
SHARED XDMODE, Olist$()
RREPsummary:
IF IPRINT <> 0 THEN
    WIINDOW: VIEW: CLS
END IF
CLP = 1: K = 0: KG = 0: ROWMAX = 25
REPsummary:
CALL READSTR(PPOS, 8, Z$): VL = AA(PPOS + 1)
IF K > ROWMAX THEN
    K = 0
    CLP = CLP + 30
END IF
K = K + 1: KG = KG + 1
IF CLP > XDMODE GOTO XXX
IF IPRINT <> 0 THEN
    LOCATE K, CLP
END IF
IF MID$(Z$, 1, 3) <> "END" THEN
    IF IPRINT <> 0 THEN
        PRINT Z$;
        PRINT USING "#.######^^^^"; VL
    ELSE
        Olist$(KG) = RTRIM$(Z$)
    END IF
    PPOS = PPOS + 2
    GOTO REPsummary
ELSE
    XXX:
    IF IPRINT <> 0 THEN
        Sfile$ = Cfile$
        LOCATE 30, 1: PRINT "File: " + Cfile$ + " N - next file ...";
        LPsummary:
        G$ = INKEY$
        IF G$ = "" THEN GOTO LPsummary:
        IF UCASE$(G$) = "N" THEN
            CLOSE #1
            Sfile$ = Cfile$
            CALL MakeNext(Cfile$, NEWFILE)
            CALL CHECKfile(Cfile$, NOBFILE)
            IF NOBFILE = 0 THEN
                CALL INITfile(Cfile$)
                LOCATE 23, 65: PRINT Cfile$;
                PPOS = RECMAX
                GOTO RREPsummary
            ELSE
                LOCATE 30, 1: PRINT "File: " + Cfile$ + " does not exist ...";
                PRINT "ERROR "; NOBFILE
                WHILE INKEY$ = "": WEND
                Cfile$ = Sfile$
                GOTO XXX
            END IF
        ELSE
            IF G$ = CHR$(27) THEN
                CLS
                Cfile$ = Sfile$
                EXIT SUB
            END IF
            GOTO LPsummary
        END IF
        CLS
    ELSE
        Olist$(0) = MID$(STR$(KG), 2)
    END IF
END IF
END SUB

SUB TRNPOLY (XY(), XYT(), Theta, Xc, Yc)
END SUB

REM $STATIC
SUB WIN (XYlim(), XYscr(), XYplt(), Labl$(), Contr()) STATIC
REDIM ABC$(255), A(3), PAR$(6), Xar(1, 1), Yar(1, 1), ID%(1, 1), XT(3), YT(3)
REDIM Page$(1), Dat$(1, 1), Dat%(30)
REDIM Titl$(3), Items$(0), Tran(0, 0)
STATIC XREMMIN, XREMMAX, YREMMIN, YREMMAX, NOFRAME, NOBOUND
SHARED HFRAME, FIXFLAG, XAXADJ, YAXADJ
XYP5 = XYplt(5): XYP6 = XYplt(6): LSHIFT = 0: NItems = 0
XREMMIN = XYlim(1): XREMMAX = XYlim(2): YREMMIN = XYlim(3): YREMMAX = XYlim(4)
FontL% = XYplt(5): FontS% = XYplt(6)
'MODE = 1  - VGA : MODE = 0 - EGA
'MACH - 1  - Toshiba (plasma display)
'XYlim(10) = 1: MACH = 1
GID% = ABS(Contr(8))
IF GID% = 0 THEN GID% = 99
dfl$ = ""
Dat%(11) = Contr(1)
Xlef = 0
Xoff% = -1 'Special
KILLANO = 0
IF XYscr(1) = 0 AND XYscr(2) = 0 AND XYscr(3) = 0 AND XYscr(4) = 0 THEN
    Xoff% = 3
    KILLANO = 1
END IF
MODE = XYlim(10)
MACH = 0
IF MODE = 1 THEN
    NSCXDEF = 460
    NSCYDEF = 460
    Yoff% = 1 'Special
    IF XYscr(1) = 0 AND XYscr(2) = 0 AND XYscr(3) = 0 AND XYscr(4) = 0 THEN
        Yoff% = 18
    END IF
END IF
IF MODE = 0 THEN
    NSCXDEF = 435
    NSCYDEF = 340
    Yoff% = 14
END IF
IF MODE < 0 THEN
    COLOR 10
    NSCXDEF = XYlim(8)
    NSCYDEF = XYlim(9)
    Yoff% = 18
END IF
XYscr(10) = MODE

Plx = 1: Ply = 1
N = INSTR(1, Labl$(0), "!")
IF N <> 0 THEN
    MID$(Labl$(0), N) = "@": STRT$ = "!"
ELSE
    STRT$ = ""
END IF
OFL$ = ""
N = INSTR(1, Labl$(0), "@")
IF N <> 0 THEN
    OFL$ = MID$(Labl$(0), N + 1)
    Labl$(0) = MID$(Labl$(0), 1, N - 1)
END IF
TCK$ = ""
NOBOUND = 0
IF Labl$(1) = "@" AND Labl$(1) = "@" THEN
    NOBOUND = 1 ': Labl$(1) = "": Labl$(2) = ""
END IF
RFLAG = 0: TFLAG = 0: PFLAG% = 1: NOFRAME = 0: Pi = 3.1415926#: UPFLAG = 0: DOFLG% = 0
TCK$ = MID$(Labl$(0), 1, 1)
IF TCK$ = "" THEN PFLAG% = 0: HFRAME = 0: FIXFLAG = 0: RFLAG = 0: GOTO 60000
IF TCK$ = "C" THEN PFLAG% = 0: HFRAME = 0: FIXFLAG = 0: RFLAG = 0: GOTO 60000
IF TCK$ = "R" THEN PFLAG% = 0: NOBOUND = 0: HFRAME = 0: FIXFLAG = 0: RFLAG = 0
PFLAG% = 0
IF Labl$(1) = "@" AND Labl$(1) = "@" THEN
    NOBOUND = 1 ': Labl$(1) = "": Labl$(2) = ""
    'MID$(RRR$, 3) = "N"
END IF
      
STRT: WINDOW: VIEW: CLS
Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
REDIM Page$(30), Dat$(30, 30), Dat%(30)
Page$(1) = "Plot   title ->"
Page$(2) = "Plot   title ->"
Page$(3) = "Plot   title ->"
Page$(4) = "Hor.   label ->"
Page$(5) = "Hor.   annot ->"
Page$(6) = "Vert   label ->"
Page$(7) = "Vert   annot ->"
Page$(8) = "Axes   sizes ->"
Page$(9) = "Plot offsets ->"
Page$(10) = "Show data/fil->"
Page$(11) = "Equal scales ->"
Page$(12) = "No annotation->"
Page$(13) = "No plot frame->"
Page$(14) = "Ticks outward->"
Page$(15) = "Axes & arrows->"
Page$(16) = "Fix  boundary->"
Page$(17) = "Title Up/Down->"
Page$(18) = "Simbols only ->"
Page$(19) = "Plot/Scrn/Ret->"
Page$(20) = "Plot file    ->"
Page$(21) = "Fn size (T,L)->"
Page$(22) = "Fn name      ->Helvetica-Bold"
Page$(23) = "Dashlines    ->"
Page$(24) = "Lansdcape    ->"
Page$(25) = STRING$(79, " ")
Kp = 1: KK = 0: T$ = Labl$(3): Titl$(1) = "": Titl$(2) = "": Titl$(3) = ""
FOR I% = 1 TO 3
    N = INSTR(Kp, T$, ";"): IF N <> 0 THEN KK = KK + 1: Titl$(KK) = MID$(T$, Kp, N - Kp): Kp = N + 1 ELSE KK = KK + 1: Titl$(KK) = MID$(T$, Kp): GOTO Sc1
NEXT I%
Sc1: Page$(1) = Page$(1) + Titl$(1)
Page$(2) = Page$(2) + Titl$(2)
Page$(3) = Page$(3) + Titl$(3)
Page$(4) = Page$(4) + Labl$(1)
Page$(6) = Page$(6) + Labl$(2)
IF OFL$ = "" THEN Page$(20) = Page$(20) + "MAKER.MIF" ELSE Page$(20) = Page$(20) + OFL$
IF XYplt(6) <> 0 AND XYplt(5) <> 0 THEN
    Page$(21) = Page$(21) + MID$(STR$(XYplt(5)), 2) + "," + MID$(STR$(XYplt(5)), 2)
    XYplt(5) = 0: XYplt(6) = 0
ELSE
    Page$(21) = Page$(21) + "12,12"
END IF
IF RRR$ = "" THEN G$ = " ": GOTO SS
IF MID$(RRR$, 1, 1) <> "Z" THEN Contr(1) = -1 ELSE Contr(1) = 0
IF MID$(RRR$, 2, 1) <> "Z" THEN NOFRAME = 1 ELSE NOFRAME = 0
IF MID$(RRR$, 3, 1) <> "Z" THEN NOBOUND = 1 ELSE NOBOUND = 0
IF MID$(RRR$, 4, 1) <> "Z" THEN Contr(6) = 1 ELSE Contr(6) = 0
IF MID$(RRR$, 5, 1) <> "Z" THEN HFRAME = 1 ELSE HFRAME = 0
IF MID$(RRR$, 6, 1) <> "Z" THEN FIXFLAG = 1 ELSE FIXFLAG = 0
G$ = MID$(RRR$, 7, 1)
SS: IF FIXFLAG = 1 THEN Page$(16) = Page$(16) + "F"
IF XYplt(1) = 0 THEN XYplt(1) = 13.33
IF XYplt(2) = 0 THEN XYplt(2) = 10
IF Contr(1) = -1 THEN Page$(11) = Page$(11) + "E": XYplt(2) = 0!
IF XYplt(3) = 0 THEN XYplt(3) = 4
IF XYplt(4) = 0 THEN XYplt(4) = 3
IF XYplt(5) = 0 THEN XYplt(5) = .15
IF XYplt(6) = 0 THEN XYplt(6) = .225
IF UPFLAG = 1 THEN Page$(17) = Page$(17) + "U" ELSE Page$(17) = Page$(17) + "D"
Page$(8) = Page$(8) + MID$(STR$(XYplt(1)), 2) + "," + MID$(STR$(XYplt(2)), 2)
IF XYplt(0) <> 0 THEN Page$(23) = Page$(23) + "D"
IF XYplt(3) < 0 THEN S$ = "-" ELSE S$ = ""
Page$(9) = Page$(9) + S$ + MID$(STR$(XYplt(3)), 2) + "," + MID$(STR$(XYplt(4)), 2)
IF NOBOUND = 1 THEN Page$(13) = Page$(13) + "N"
IF HFRAME = 1 THEN Page$(15) = Page$(15) + "A"
NNX = VAL(Labl$(4)): NNY = VAL(Labl$(6 + NNX))
IF NOFRAME = 1 THEN Page$(12) = Page$(12) + "N"
IF Contr(6) = 1 THEN Page$(14) = Page$(14) + "T"
Pager5$ = Page$(5): Pager7$ = Page$(7):
FOR I = 0 TO NNX
    Page$(5) = Page$(5) + Labl$(5 + I): IF LEN(Page$(5)) > 79 THEN Page$(5) = Pager5$ + Labl$(5) + "," + Labl$(5 + NNX): Page$(7) = Pager7$ + Labl$(7 + NNX) + "," + Labl$(7 + NNX + NNY): GOTO Cnt2
    IF I <> NNX THEN Page$(5) = Page$(5) + ","
NEXT I
IF Contr(10) = 2 THEN Page$(18) = Page$(18) + "S"
IF Contr(10) = 1 THEN Page$(10) = Page$(10) + "S"
Cnt1: FOR I = 0 TO NNY
    Page$(7) = Page$(7) + Labl$(7 + NNX + I): IF LEN(Page$(7)) > 79 THEN Page$(5) = Pager5$ + Labl$(5) + "," + Labl$(5 + NNX): Page$(7) = Pager7$ + Labl$(7 + NNX) + "," + Labl$(7 + NNX + NNY): GOTO Cnt2
    IF I <> NNY THEN Page$(7) = Page$(7) + ","
NEXT I
Cnt2: IF G$ <> " " THEN Page$(19) = Page$(19) + G$: GOTO TTT
IF Contr(3) = 0 THEN Page$(19) = Page$(19) + "S" ELSE Page$(19) = Page$(19) + "P"
TTT: NcMin = 16: NcMax = 79: NRmin = 1: NRmax = 24: CLS
FOR I = 1 TO NRmax: Dat%(I) = -1: FOR J = 1 TO 11: Dat$(I, J) = " ": NEXT J: NEXT I
ERline = 0: Mes$ = ""
IF OFL$ <> "" THEN Mes$ = "SCIP"
CALL GetData(Page$(), Dat$(), Dat%(), NRmin, NRmax, NcMin, NcMax, ERline, Mes$)
dfl$ = Dat$(20, 1): IF Labl$(30) <> "" THEN dfl$ = Labl$(30) + "\" + dfl$
Lands% = Dat%(24)
FontL% = VAL(Dat$(21, 1))
dat11% = Dat%(11)
WINDOW: VIEW: CLS: COLOR 15: RRR$ = "" '
Font$ = Dat$(22, 1)
GAR$ = Dat$(17, 1): GOSUB 6000: IF GAR$ = "U" OR GAR$ = "u" THEN UPFLAG = 1 ELSE UPFLAG = 0
KK = 0
GAR$ = Dat$(1, 1): GOSUB 6000: IF GAR$ <> "" THEN Labl$(3) = GAR$ ELSE Labl$(3) = " ": KK = 1
GAR$ = Dat$(2, 1): GOSUB 6000: IF GAR$ <> "" THEN Labl$(3) = Labl$(3) + ";" + GAR$: KK = 2
GAR$ = Dat$(3, 1): GOSUB 6000: IF GAR$ <> "" THEN Labl$(3) = Labl$(3) + ";" + GAR$: KK = 3
GAR$ = Dat$(4, 1): GOSUB 6000: IF GAR$ <> "" THEN Labl$(1) = GAR$ ELSE Labl$(1) = ""
GAR$ = Dat$(6, 1): GOSUB 6000: IF GAR$ <> "" THEN Labl$(2) = GAR$ ELSE Labl$(2) = ""
N = INSTR(1, Labl$(2), "^")
IF N <> 0 THEN
    Labl$(2) = MID$(Labl$(2), 1, N - 1)
    IF Contr(2) = 0 THEN Contr(2) = 1
END IF
XYplt(0) = 0
IF Dat%(23) <> 0 THEN XYplt(0) = 1
IF Labl$(1) = "-" THEN NOXAXIS = 1 ELSE NOXAXIS = 0
IF Labl$(1) = "-" THEN NOYAXIS = 1 ELSE NOYAXIS = 0
FOR I% = 11 TO 16
    IF Dat%(I%) <> 0 THEN RRR$ = RRR$ + "N" ELSE RRR$ = RRR$ + "Z"
NEXT I%
NNX = Dat%(5) - 1: NNY = Dat%(7) - 1
IF NNX = 1 AND NNY = 1 THEN
    XYlim(1) = VAL(Dat$(5, 1)): XYlim(2) = VAL(Dat$(5, 2))
    XYlim(3) = VAL(Dat$(7, 1)): XYlim(4) = VAL(Dat$(7, 2))
    NNX = 0: NNY = 0
    PFLAG% = 0: RFLAG = 0
    GOTO Ct
END IF
IF NNX < 1 OR NNY < 1 THEN RFLAG = 0: PFLAG% = 0: XYlim(1) = XREMMIN: XYlim(2) = XREMMAX: XYlim(3) = YREMMIN: XYlim(4) = YREMMAX: G$ = "S": GOTO TR ELSE RFLAG = 1: PFLAG% = 0
PFLAG% = 0
Ct: Labl$(4) = MID$(STR$(NNX), 2): Labl$(6 + NNX) = MID$(STR$(NNY), 2)
XYplt(6) = 2 / 3 * VAL(Dat$(21, 2)) * 2.54 / 72
XYplt(5) = .5 * XYplt(6)
XYplt(3) = VAL(Dat$(9, 1)): XYplt(4) = VAL(Dat$(9, 2))
XYplt(1) = VAL(Dat$(8, 1)): XYplt(2) = VAL(Dat$(8, 2))
XYplt(10) = 0
IF Dat%(23) <> 0 THEN XYplt(10) = 1
TMP = 0
IF UPFLAG = 1 THEN
    TMP = (((KK - 1) * VAL(Dat$(21, 1)) + 1.5 * VAL(Dat$(21, 2)))) * .03528
    IF KK = 2 OR KK = 1 THEN TMP = TMP + .5 * VAL(Dat$(21, 1)) * .03528
    XYplt(4) = XYplt(4) + TMP
END IF
IF Dat%(14) <> 0 THEN Contr(6) = 1 ELSE Contr(6) = 0
IF PFLAG% = 0 AND RFLAG = 0 GOTO XX
LBXMAX = -1
FOR I = 0 TO NNX
    GAR$ = Dat$(5, I + 1): GOSUB 6000
    IF LEN(GAR$) > LBXMAX THEN LBXMAX = LEN(GAR$)
    Labl$(5 + I) = GAR$
NEXT I
DDx$ = GAR$
XYlim(1) = VAL(Labl$(5)): XYlim(2) = VAL(Labl$(5 + NNX))
LBYMAX = -1
FOR I = 0 TO NNY
    GAR$ = Dat$(7, I + 1): GOSUB 6000
    IF LEN(GAR$) > LBYMAX THEN LBYMAX = LEN(GAR$)
    Labl$(7 + I + NNX) = GAR$
NEXT I
XYlim(3) = VAL(Labl$(7 + NNX)): XYlim(4) = VAL(Labl$(7 + NNX + NNY))
XX: IF Dat%(12) <> 0 THEN NOFRAME = 1 ELSE NOFRAME = 0
IF Dat%(13) <> 0 THEN NOBOUND = 1 ELSE NOBOUND = 0
IF NOBOUND = 1 THEN NOFRAME = 1
IF Dat%(15) <> 0 THEN HFRAME = 1 ELSE HFRAME = 0
IF Dat%(16) <> 0 THEN FIXFLAG = 1 ELSE FIXFLAG = 0: GOTO TT
'Size adjustment
XOFADJ = (.5 * LBYMAX + 2.04) * VAL(Dat$(21, 2)) * .03528
YOFADJ = 2.8 * VAL(Dat$(21, 2)) * .03528 + TMP
IF KK <> 0 THEN YOFADJ = YOFADJ - VAL(Dat$(21, 2)) * .333 * .03528
IF HFRAME = 1 THEN
    XOFADJ = XOFADJ + (XYplt(1) - XOFADJ) / (1 + 2 * NNX)
    YOFADJ = YOFADJ + (XYplt(2) - YOFADJ) / (1 + 2 * NNY)
END IF
TT: GAR$ = Dat$(19, 1): GOSUB 6000: IF GAR$ = "" THEN GAR$ = "S"
N = ASC(GAR$): IF N > 96 THEN N = N - 32
G$ = CHR$(N): RRR$ = RRR$ + G$
Contr(10) = 0
IF Dat%(18) <> 0 THEN Contr(10) = 2
IF Dat%(10) <> 0 THEN Contr(10) = 1
IF G$ = "R" THEN Labl$(0) = TCK$ + RRR$: GOTO finwin
TR: XYscr(1) = Xlas - Xoff: XYscr(2) = Ylas - Yoff: XYscr(3) = Xoff: XYscr(4) = 1: Ylas = 0
IF G$ = "S" THEN Contr(3) = 0: TCK$ = "CR"
IF G$ = "P" THEN Contr(3) = 2
Labl$(0) = TCK$ + RRR$
ZZX: Dx = XYlim(2) - XYlim(1): Dy = XYlim(4) - XYlim(3)
IF DOFLG% = 1 THEN XYplt(1) = AXXV: XYplt(2) = 0
IF DOFLG% = 2 THEN XYplt(1) = 0: XYplt(2) = AYYV
LHOR = XYplt(1): LVER = XYplt(2): OFFH = XYplt(3): OFFV = XYplt(4)
IF ABS(Dx) < .00001 OR ABS(Dy) < .00001 GOTO STRT
IF dat11% <> 0 THEN
    Contr(1) = -1
    IF LVER = 0 AND LHOR = 0 THEN LVER = 22!: OFFV = 3: OFFH = 4
    IF LHOR = 0 AND LVER <> 0 THEN LHOR = LVER * Dx / Dy
    IF LVER = 0 AND LHOR <> 0 THEN LVER = LHOR * Dy / Dx
    IF Lands% = 0 THEN
        IF (LVER + OFFV) > 27! THEN LVER = 27 - OFFV: LHOR = LVER * Dx / Dy
        IF (LHOR + OFFH) > 21! THEN LHOR = 21 - OFFH: LVER = LHOR * Dy / Dx
    END IF
    XYplt(1) = LHOR: XYplt(2) = LVER: XYplt(3) = OFFH: XYplt(4) = OFFV
END IF
IF dat11% = -2 THEN dat11% = 0: RETURN
IF dat11% = 0 THEN Contr(1) = 0
GOTO 60000
6000 'GET RID OF BLANKS
6010 IF LEFT$(GAR$, 1) = " " THEN GAR$ = MID$(GAR$, 2): GOTO 6010
6015 LLL = LEN(GAR$)
6020 IF RIGHT$(GAR$, 1) = " " THEN GAR$ = MID$(GAR$, 1, LLL - 1): LLL = LLL - 1: GOTO 6020
6060 RETURN
7001 RETURN
7011 IF ASC(RIGHT$(G$, 1)) = 79 THEN FLRG = 1: RETURN ELSE FLRG = 0: RETURN
60000 'Screen setup
60040 'XLABEL$   - LABEL X  | ?LABEL$="" - NO FRAME, NO LABEL
60050 'YLABEL$   - LABEL Y  | ?LABEL$=" " - FRAME, BLANK LABEL
60060 'NSCX      - HORIZONTAL LENGTH OF WINDOW
60070 'NSCY      - VERTICAL   LENGTH OF WINDOW
60080 'NSHX      - HORIZONTAL SHIFT  OF WINDOW
60090 'NSHY      - VERTICAL   SHIFT  OF WINDOW
60091 'Xoff%      - INITIAL X-SCREEN OFFSET IN PIXELS
60092 'Yoff%      - INITIAL Y-SCREEN OFFSET IN PIXELS
60093 'Xlas%      - LAST    X-SCREEN OF THE FRAME
60094 'Ylas%      - LAST    Y-SCREEN OF THE FRAME
60095 'LHOR      - LENGTH OF HORIZONTAL AXIS IN CM
60096 'LVER      - LENGTH OF VERTICAL   AXIS IN CM
60097 'OFFH      - HORIZONTAL OFFSET ON THE PAGE
60098 'OFFV      - VERTICAL OFFSET   ON THE PAGE
60100 'IEQUAL=-1 - EQUAL SCALES ALONG  X AND Y
60110 'IACTION    - 0 SCREEN ONLY 1 - PLOTTER ONLY 2 - BOTH
60111 Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
60112 NscX = XYscr(1): Nscy = XYscr(2): NSHX = XYscr(3): NSHY = XYscr(4): BACKGR = XYscr(0): Colr = XYscr(6)
GFLAG = 0
IF Nscy + Yoff% >= NSCYDEF THEN Nscy = NSCYDEF - Yoff% - 4
IF Colr = 0 THEN Colr = 15
Dx = XYlim(2) - XYlim(1): Dy = XYlim(4) - XYlim(3)
IF DOFLG% = 1 THEN XYplt(1) = AXXV: XYplt(2) = 0
IF DOFLG% = 2 THEN XYplt(1) = 0: XYplt(2) = AYYV
LHOR = XYplt(1): LVER = XYplt(2): OFFH = XYplt(3): OFFV = XYplt(4)
'IF ABS(Dx) < .00001 OR ABS(Dy) < .00001 GOTO STRT
IF Dat%(11) <> 0 THEN
    Contr(1) = -1
    IF LVER = 0 AND LHOR = 0 THEN LVER = 22!: OFFV = 3: OFFH = 4
    IF LHOR = 0 AND LVER <> 0 THEN LHOR = LVER * Dx / Dy
    IF LVER = 0 AND LHOR <> 0 THEN LVER = LHOR * Dy / Dx
    IF Dat%(24) = 0 THEN
        IF (LVER + OFFV) > 27! THEN LVER = 27 - OFFV: LHOR = LVER * Dx / Dy
        IF (LHOR + OFFH) > 21! THEN LHOR = 21 - OFFH: LVER = LHOR * Dy / Dx
    END IF
    XYplt(1) = LHOR: XYplt(2) = LVER: XYplt(3) = OFFH: XYplt(4) = OFFV
END IF
IF Dat%(11) = -2 THEN Dat%(11) = 0: RETURN
IF Dat%(11) = 0 THEN Contr(1) = 0
60113 IF XYplt(1) = 0 AND XYplt(2) = 0 THEN XYplt(1) = 13.33: XYplt(2) = 10!: XYplt(3) = 5: XYplt(4) = 3: XYplt(5) = .15: XYplt(6) = .225: GOTO 60114
IF XYplt(2) = 0 THEN DOFLG% = 1: AXXV = XYplt(1): XYplt(1) = 0: GOTO 60113
IF XYplt(1) = 0 THEN DOFLG% = 2: AYYV = XYplt(2): XYplt(2) = 0: GOTO 60113
60114 LHOR = XYplt(1): LVER = XYplt(2): Rlim = 1E-08
IF FIXFLAG = 1 THEN LHOR = LHOR - XOFADJ: LVER = LVER - YOFADJ
OFFH = XYplt(3): OFFV = 27.94 - XYplt(4) - LVER: CHRW = XYplt(5): CHRH = XYplt(6)
IF CHRW = 0 THEN CHRW = .15: CHRH = .225
IF FIXFLAG = 1 THEN OFFH = OFFH + XOFADJ
IF FIXFLAG = 1 AND HFRAME = 1 THEN OFFV = OFFV - 4 * XYplt(5)
60115 N = INSTR(1, Labl$(1), "#")
LOGX = 0: LOGY = 0
IF N <> 0 THEN LOGX = 1: XLB$ = MID$(Labl$(1), 1, N - 1): Labl$(1) = ""
N = INSTR(1, Labl$(2), "#")
IF N <> 0 THEN LOGY = 1: YLB$ = MID$(Labl$(2), 1, N - 1): Labl$(2) = ""
XLABEL$ = Labl$(1): YLABEL$ = Labl$(2): Title$ = Labl$(3): HFLAG = 0
60116 IEQUAL = Contr(1)
AXREV = Contr(2): IF AXREV <> 0 THEN AXREV = -1
IACTION = Contr(3)
XFLAG% = Contr(4)
YFLAG% = Contr(5)
HFLAG = Contr(6)
Eflag = Contr(7)
IF Eflag = 1 AND Labl$(1) = "" AND Labl$(2) = "" THEN
    NOBOUND = 1
    Yoff% = 0
    Xoff% = -1
    Nscy = 470
    NscX = 470
END IF
IF RFLAG = 0 GOTO 60117
NNX = VAL(Labl$(4)): NNY = VAL(Labl$(6 + NNX)): NTX = NNX + 1: NTY = NNY + 1
Nx = NSCXDEF / NNX
Ny = NSCYDEF / NNY
IXRMIN = 0: IXRMAX = NNX
IYRMIN = 0: IYRMAX = NNY
GOSUB 65462
60117 IF ABS(IACTION) <> 1 THEN
    IF MODE = 1 THEN
        SCREEN 12
        COLOR Colr
    END IF
    IF MODE = 0 THEN
        SCREEN 9
        COLOR Colr, BACKGR
    END IF
END IF
60118
IF ABS(IACTION) > 0 THEN
    IMAKER% = 0
    IF OFL$ <> "" THEN dfl$ = OFL$
    IF dfl$ = "" THEN dfl$ = "MAKER.MIF"
    N = INSTR(1, dfl$, ".")
    IF N <> 0 THEN
        Wt$ = MID$(dfl$, N + 1, 3)
        IF Wt$ = "MIF" OR Wt$ = "mif" THEN IMAKER% = 1: Contr(3) = 99
    END IF
    IF OFL$ <> "" AND STRT$ = "" GOTO NOPEN
    IF dfl$ = "+" THEN GOTO NOPEN
    CLOSE #3
    OPEN dfl$ FOR OUTPUT AS #3
    NOPEN:
END IF
60119 IF XLABEL$ <> "" THEN IXLAB = -1: IXANO = -1 ELSE IXLAB = 0: IXANO = 0
60120 IF YLABEL$ <> "" THEN IYLAB = -1: IYANO = -1 ELSE IYLAB = 0: IYANO = 0
60150 IF ABS(IACTION) <> 1 THEN VIEW: WINDOW
IF NOFRAME = 1 THEN IXANO = 0: IXLAB = 0: IYANO = 0: IXANO = 0: XLABEL$ = "": YLABEL$ = ""
60160 GAR$ = XLABEL$: GOSUB 64000: XLABEL$ = GAR$
60161 GAR$ = YLABEL$: GOSUB 64000: YLABEL$ = GAR$
60290 FAX = 1: FAY = 1
60291 IF NscX = 0 AND Nscy = 0 THEN NscX = NSCXDEF: Nscy = NSCYDEF: NSHX = 0: NSHY = 0
60292 IF Nscy / NscX > 3 THEN NscX = 0: Nscy = 0: GOTO 60291
60300 'CONTINUE
60307 XMINR0 = Xmin: YMINR0 = Ymin: XMAXR0 = Xmax: YMAXR0 = Ymax
60308 TRUE = -1: FALSE = 0: Ni = 5
60309 IF NscX > 320 THEN Ni = 10
60310 IF XYscr(5) <> 0 THEN Ni = XYscr(5)
60312 Xlef = 0: IF Xoff% <> -1 THEN Xoff% = 3
60320 XXOF = Xoff%
60330 YYOF = Yoff% - KILLANO * IXANO * 8 - IXLAB * 8 + 30 * LOGX
60340 RANX = Xmax - Xmin: RANY = Ymax - Ymin: XOFFR = Xoff%
IF ABS(RANX) < Rlim THEN
    Xc = .5 * (Xmin + Xmax)
    IF ABS(Xc) < Rlim THEN Xmin = Xc - .5 * Rlim: Xmax = Xc + .5 * Rlim ELSE Xmin = Xc * .9: Xmax = Xc * 1.1
    GFLAG = 1
END IF
IF ABS(RANY) < Rlim THEN
    Yc = .5 * (Ymin + Ymax)
    IF ABS(Yc) < Rlim THEN Ymin = Yc - .5 * Rlim: Ymax = Yc + .5 * Rlim ELSE Ymin = Yc * .9: Ymax = Yc * 1.1
END IF
60350 REM TO SETUP THE SCREEN
60360 Rmax = RANX: IF RANY > Rmax THEN Rmax = RANY
60370 IF IEQUAL = -1 THEN Dr = Rmax / Ni: NNNX = INT(RANX / Dr): NNNY = INT(RANY / Dr) ELSE NNNX = Ni: NNNY = Ni
60375 IF NNNX < 3 OR NNNY < 3 AND FSCR% <> 1 THEN IEQUAL = 0: NNNX = Ni: NNNY = Ni
60380 IF MODE = 1 THEN
    Fact = 1
END IF
IF MODE = 0 THEN
    Fact = 1.3333
END IF
IF MODE < 0 THEN
    Fact = 1.0
END IF
IF MACH = 1 THEN Fact = 1!
FLAG = 0
60390 N = NNNX: XYMIN = Xmin: XYMAX = Xmax: NNXY = NNX: GOSUB 62670
60410 XMIN1 = XYMIN: XMAX1 = XYMAX: IXMIN1 = Imin: IXMAX1 = Imax: NTX1 = NTIK: MSDX1 = MSD
60420 IMX = -Im: IF Im > 0 THEN IMX = 0
60430 N = NNNY: XYMIN = Ymin: XYMAX = Ymax: NNXY = NNY: GOSUB 62670
60440 Ymin1 = XYMIN: Ymax1 = XYMAX: IYMIN1 = Imin: IYMAX1 = Imax: NTY1 = NTIK: MSDY1 = MSD
60450 IMY = -Im: IF Im > 0 THEN IMY = 0
60455 IF FLAG = 0 THEN IMMX = IMX: IMMY = IMY
60460 GOSUB 62990
60480 IF FLAG > 0 GOTO 60390
60505 IF NTX1 <= 2 THEN FAX = 10 ^ IMMX: Xmin = XMINR0 * FAX: Xmax = XMAXR0 * FAX: XLABEL$ = XLABEL$ + " x 10^" + STR$(-IMMX): FLAG = 1
60506 IF NTY1 <= 2 THEN FAY = 10 ^ IMMY: Ymin = YMINR0 * FAY: Ymax = YMAXR0 * FAY: YLABEL$ = YLABEL$ + " x 10^" + STR$(-IMMY): FLAG = 1
60507 IF FLAG = 1 GOTO 60300
60508 Xmin = XMIN1: Ymin = Ymin1: Xmax = XMAX1: Ymax = Ymax1: NTX = NTX1: NTY = NTY1
60510 IYMIN = IYMIN1: IYMAX = IYMAX1
60520 IXMIN = IXMIN1: IXMAX = IXMAX1
IF RFLAG = 1 THEN IYMIN = IYRMIN: IYMAX = IYRMAX: IXMIN = IXRMIN: IXMAX = IXRMAX
60530 MSDX = MSDX1: MSDY = MSDY1
60540 TML$ = "L3": TMR$ = "R3": TMU$ = "U2": TMD$ = "D2": IF HFLAG <> 0 THEN TML$ = "  ": TMR$ = "L3": TMU$ = "D2": TMD$ = "  "
60550 NNX = NTX - 1: NNY = NTY - 1: Xoff% = Xoff% + NSHX: Yoff% = Yoff% + NSHY
60560 Xlas% = Xoff% + Nx * NNX: Ylas% = Yoff% + Ny * NNY: Ymax = Ymax / FAY: Ymin = Ymin / FAY: Xmax = Xmax / FAX: Xmin = Xmin / FAX
XminO = Xmin: YminO = Ymin: XmaxO = Xmax: YmaxO = Ymax
IF Eflag = 1 THEN
    IF IEQUAL <> 0 THEN
        Asp = (YREMMAX - YREMMIN) / (XREMMAX - XREMMIN)
        SlnXo = Xlas% - Xoff%
        SlnYo = Ylas% - Yoff%
        SlnX1 = SlnXo
        SlnY1 = SlnXo * Asp
        FLAG1 = FALSE
        IF SlnX1 <= NscXR AND SlnY1 <= NscYR THEN FLAG1 = TRUE
        SlnY2 = SlnYo
        SlnX2 = SlnYo / Asp
        FLAG2 = FALSE
        IF SlnX2 <= NscXR AND SlnY2 <= NscYR THEN FLAG2 = TRUE
        IF FLAG1 = TRUE AND FLAG2 = TRUE THEN
            PRD1 = SlnX1 * SlnY1
            PRD2 = SlnX2 * SlnY2
            IF PRD1 > PRD2 THEN
                FLAG2 = FALSE
            ELSE
                FLAG1 = FALSE
            END IF
        END IF
        IF FLAG1 = FALSE AND FLAG2 = TRUE THEN
            SlnX = SlnX2
            SlnY = SlnY2
        END IF
        IF FLAG1 = TRUE AND FLAG2 = FALSE THEN
            SlnX = SlnX1
            SlnY = SlnY1
        END IF
        Xlas% = Xoff% + SlnX
        Ylas% = Yoff% + SlnY
        Xmin = XREMMIN: Xmax = XREMMAX
        Ymin = YREMMIN: Ymax = YREMMAX
        Nx = (Xlas% - Xoff%) / (Xmax - Xmin) * MSDX
        Ny = (Ylas% - Yoff%) / (Ymax - Ymin) * MSDY
    ELSE
        IF AXREV = 0 THEN
            Xmin = XREMMIN: Xmax = XREMMAX
        ELSE
            Ymin = YREMMIN: Ymax = YREMMAX
        END IF
    END IF
END IF
Coefx = (Xlas% - Xoff%) / (Xmax - Xmin)
Coefy = (Yoff% - Ylas%) / (Ymax - Ymin)
Xshift = Xoff% - Coefx * Xmin
Yshift = Ylas% - Coefy * Ymin
IF AXREV THEN Coefy = -Coefy: Yshift = Yoff% - Coefy * Ymin
DXsh = 0: DYsh = 0
IF Eflag = 1 THEN
    DXsh = XminO * Coefx + Xshift - Xoff%
    DYsh = YminO * Coefy + Yshift - Ylas%
END IF
IF dfl$ = "+" THEN IACTION = 0: GOTO finwin
IF ABS(IACTION) < 1 GOTO 60597
St = .0352777
MVT$ = "###.## ###.## moveto"
LTO$ = "###.## ###.## lineto"
IF IMAKER% = 0 THEN
    RHOFF = CINT(OFFH / St): RVOFF = CINT(OFFV / St)
    RHLAS = CINT((LHOR + OFFH) / St): RVLAS = CINT((OFFV + LVER) / St)
    XYplt(7) = RHOFF: XYplt(8) = RVOFF: XYplt(9) = RHLAS: XYplt(10) = RVLAS
    LOCATE 1, 60: PRINT "End key to quit  "
    Rcoefx = (RHLAS - RHOFF) / (Xmax - Xmin)
    RCoefy = (RVLAS - RVOFF) / (Ymax - Ymin)
    RXshift = RHOFF - Rcoefx * Xmin
    RYshift = RVOFF - RCoefy * Ymin
    IF AXREV THEN RCoefy = -RCoefy: RYshift = RVLAS - RCoefy * Ymin
ELSE
    RHOFF = CINT(OFFH / St): RVOFF = ((27.94 - LVER - OFFV) / St)
    RHLAS = CINT((LHOR + OFFH) / St): RVLAS = CINT((27.94 - OFFV) / St)
    XYplt(7) = RHOFF: XYplt(8) = RVOFF: XYplt(9) = RHLAS: XYplt(10) = RVLAS
    Rcoefx = (RHLAS - RHOFF) / (Xmax - Xmin)
    RCoefy = (RVOFF - RVLAS) / (Ymax - Ymin)
    RXshift = RHOFF - Rcoefx * Xmin
    RYshift = RVOFF - RCoefy * Ymax
    IF AXREV THEN RCoefy = -RCoefy: RYshift = RVLAS - RCoefy * Ymax
END IF
TLNX = .012 * (RHLAS - RHOFF)
TLNY = .012 * (RVLAS - RVOFF)
TLN = TLNY: IF TLNY < TLNX THEN TLN = TLNX
IF HFLAG <> 0 THEN TLN = -TLN
60567 IF Contr(3) <> 0 THEN
    IF OFL$ <> "" AND STRT$ = "" GOTO NOWRN
    IF IMAKER% = 0 THEN
        CALL INITPS(Contr())
    ELSE
        PRINT #3, "<MIFFile 2.00>"
        PRINT #3, "<Units Upt>"
        'CALL MakerNewPage
    END IF
    NOWRN:
END IF
60571 IF NOBOUND = 0 THEN
    IF Dat%(24) <> 0 THEN
        IF IMAKER% = 0 THEN
            PRINT #3, USING "###.## 0 translate"; RVLAS + XYplt(4) / St
            PRINT #3, "90 rotate"
        ELSE
        END IF
    END IF
    IF IMAKER% = 0 THEN
        PRINT #3, "newpath"
        PRINT #3, "2 setlinewidth"
        PRINT #3, "1 setlinejoin"
        PRINT #3, USING MVT$; RHOFF; RVOFF
    ELSE
        CALL MSETLINE(2!)
    END IF
    IF HFRAME = 0 THEN
        IF IMAKER% = 0 THEN
            PRINT #3, USING LTO$; RHOFF; RVLAS
            PRINT #3, USING LTO$; RHLAS; RVLAS
            PRINT #3, USING LTO$; RHLAS; RVOFF
        ELSE
            IF LOGX = 0 AND LOGY = 0 THEN
                IF ABS(IACTION) >= 1 THEN
                    IF IMAKER% = 0 THEN
                    ELSE
                        CALL MSETLINE(2!)
                        CALL MRECT(RHOFF, RVOFF, RHLAS - RHOFF, RVLAS - RVOFF, GID%, 15)
                    END IF
                END IF
            END IF
        END IF
    ELSE
        IF IMAKER% = 0 THEN
            PRINT #3, USING LTO$; RHLAS; RVOFF
            PRINT #3, USING LTO$; RHOFF; RVOFF
            PRINT #3, USING LTO$; RHOFF; RVLAS
            PRINT #3, USING LTO$; RHOFF; RVOFF
        END IF
    END IF
    IF IMAKER% = 0 THEN
        PRINT #3, "closepath"
        PRINT #3, "stroke"
        PRINT #3, "1 setlinewidth"
    ELSE
        CALL MSETLINE(1!)
    END IF
    60572 '
END IF
GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
60597
XYscr(1) = Xoff%: XYscr(2) = Xlas%: XYscr(3) = Yoff%: XYscr(4) = Ylas%: XYscr(8) = NNX: XYscr(9) = NNY
XYlim(1) = Xmin: XYlim(2) = Xmax: XYlim(3) = Ymin: XYlim(4) = Ymax
IF ABS(IACTION) = 1 GOTO 60655
IF NOBOUND = 1 GOTO 60655
IF HFRAME = 0 THEN
    LINE (Xoff%, Yoff%)-(Xlas%, Yoff%)
    IF MACH = 1 OR MODE = 1 THEN LINE (Xoff%, Yoff% - 1)-(Xlas%, Yoff% - 1)
END IF
60630 LINE (Xoff%, Ylas%)-(Xlas%, Ylas%)
IF MACH = 1 OR MODE = 1 THEN LINE (Xoff%, Ylas% + 1)-(Xlas%, Ylas% + 1)
IF DOFLG% >= 0 AND IEQUAL = -1 THEN Dat%(11) = -2: GOSUB ZZX
60640 LINE (Xoff%, Yoff%)-(Xoff%, Ylas%)
LINE (Xoff% + 1, Yoff%)-(Xoff% + 1, Ylas%)
60650 IF HFRAME = 0 THEN
    LINE (Xlas%, Yoff%)-(Xlas%, Ylas%)
    LINE (Xlas% - 1, Yoff%)-(Xlas% - 1, Ylas%)
END IF
60655 IF NOT IXANO THEN GOTO 60711
60660 FOR I = 1 TO NNX + 1
    60670 XcO = DXsh + Xoff% + (I - 1) * Nx: YcO = Yoff%
    IF Eflag = 1 THEN
        IF XcO < Xoff% OR XcO > Xlas% THEN GOTO 60710
    END IF
    IF HFRAME = 1 GOTO 60690
    IF LEN(Title$) > 2 AND UPFLAG = 0 GOTO 60690
    60680 IF ABS(IACTION) <> 1 THEN PSET (XcO, YcO): DRAW TMD$
    60681 IF ABS(IACTION) < 1 GOTO 60690
    Rx = XminO + (I - 1) * MSDX: Ry = Ymax
    IF Eflag = 1 THEN
        IF Rx < Xmin OR Rx > Xmax THEN GOTO 60710
    END IF
    IF AXREV THEN Ry = Ymin
    Rx = Rx * Rcoefx + RXshift
    Ry = Ry * RCoefy + RYshift
    IF IMAKER% = 0 THEN
        PRINT #3, "newpath"
        PRINT #3, USING MVT$; Rx; Ry
        PRINT #3, USING LTO$; Rx; Ry - TLN
        PRINT #3, "stroke"
    ELSE
        CALL MLINE(Rx, Ry, Rx, Ry + TLN, GID%, 0)
    END IF
    GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
    60690 YcO = Ylas%
    60700 IF ABS(IACTION) <> 1 THEN PSET (XcO, YcO): DRAW TMU$
60710 NEXT I
60711 IF NOT IYANO THEN GOTO 60771
60720 FOR J = 1 TO NNY + 1
    60730 XcO = Xoff%: YcO = DYsh + Ylas% - (J - 1) * Ny
    IF Eflag = 1 THEN
        IF YcO < Yoff% OR YcO > Ylas% THEN GOTO 60770
    END IF
    60740 IF ABS(IACTION) <> 1 THEN
        IF LOGX = 0 THEN
            PSET (XcO, YcO): DRAW TMR$
        ELSE
            LINE (Xoff%, YcO)-(Xlas%, YcO), 7
        END IF
    END IF
    60742 IF ABS(IACTION) < 1 GOTO 60750
    IF HFRAME = 1 GOTO 60750
    Ry = YminO + (J - 1) * MSDY: Rx = Xmax
    IF Eflag = 1 THEN
        IF Ry < Ymin OR Ry > Ymax THEN GOTO 60775
    END IF
    Rx = Rx * Rcoefx + RXshift
    Ry = Ry * RCoefy + RYshift
    IF IMAKER% = 0 THEN
        PRINT #3, "newpath"
        PRINT #3, USING MVT$; Rx; Ry
        PRINT #3, USING LTO$; Rx - TLN; Ry
        PRINT #3, "stroke"
    ELSE
        IF LOGX = 1 THEN
            CALL MLINE(RHOFF, Ry, RHLAS, Ry, GID%, 10)
        ELSE
            CALL MLINE(Rx, Ry, Rx - TLN, Ry, GID%, 0)
        END IF
    END IF
    GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
    60750 XcO = Xlas%
    IF HFRAME = 1 GOTO 60770
    60760 IF ABS(IACTION) <> 1 THEN PSET (XcO, YcO): DRAW TML$
60770 NEXT J
60771 IF NOT IXANO THEN GOTO 60776
IF ABS(IACTION) < 1 GOTO 60780
60772 FOR I = NNX + 1 TO 1 STEP -1
    60773 Rx = XminO + (I - 1) * MSDX
    IF Eflag = 1 THEN
        IF Rx < Xmin OR Rx > Xmax THEN GOTO 60775
    END IF
    IF AXREV THEN Ry = Ymax ELSE Ry = Ymin
    Rx = Rx * Rcoefx + RXshift
    Ry = Ry * RCoefy + RYshift
    IF IMAKER% = 0 THEN
        PRINT #3, "newpath"
        PRINT #3, USING MVT$; Rx; Ry
        PRINT #3, USING LTO$; Rx; Ry + TLN
        PRINT #3, "stroke"
    ELSE
        CALL MLINE(Rx, Ry, Rx, Ry - TLN, GID%, 0)
    END IF
    GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
60775 NEXT I
60776 IF NOT IYANO THEN GOTO 60779
IF ABS(IACTION) < 1 GOTO 60779
FOR J = NNY + 1 TO 1 STEP -1
    Ry = YminO + (J - 1) * MSDY: Rx = Xmin
    IF Eflag = 1 THEN
        IF Ry < Ymin OR Ry > Ymax THEN GOTO 60778
    END IF
    Rx = Rx * Rcoefx + RXshift
    Ry = Ry * RCoefy + RYshift
    IF IMAKER% = 0 THEN
        PRINT #3, "newpath"
        PRINT #3, USING MVT$; Rx; Ry
        PRINT #3, USING LTO$; Rx + TLN; Ry
        PRINT #3, "stroke"
    ELSE
        IF LOGY = 0 THEN
            CALL MLINE(Rx, Ry, Rx + TLN, Ry, GID%, 0)
        END IF
    END IF
    60778 GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
NEXT J
60779 IF IACTION = 0 GOTO 60780
IF HFRAME = 0 GOTO 60780
Dx = .5 * MSDX * Rcoefx
Dy = .5 * MSDY * ABS(RCoefy)
IF TCK$ = "CR" GOTO 60780
IF NOBOUND = 1 GOTO 60780
IF IACTION = 0 GOTO 60780
ART = ABS(TLN)
ARL = 3.5 * ART
IF IMAKER% = 0 THEN
    PRINT #3, "2 setlinewidth"
    PRINT #3, "2 setlinejoin"
    PRINT #3, USING MVT$; RHLAS; RVOFF
    PRINT #3, USING LTO$; RHLAS + Dx / 2; RVOFF
    PRINT #3, "stroke"
    PRINT #3, "newpath"
    PRINT #3, "0 setlinewidth"
    PRINT #3, USING MVT$; RHLAS + Dx / 2 + ARL; RVOFF
    PRINT #3, USING LTO$; RHLAS + Dx / 2; RVOFF + ART
    PRINT #3, USING LTO$; RHLAS + Dx / 2; RVOFF - ART
    PRINT #3, "closepath"
    PRINT #3, "fill"
    PRINT #3, "2 setlinewidth"
    PRINT #3, "2 setlinejoin"
    PRINT #3, USING MVT$; RHOFF; RVLAS
    PRINT #3, USING LTO$; RHOFF; RVLAS + Dy / 2
    PRINT #3, "stroke"
    PRINT #3, "newpath"
    PRINT #3, "0 setlinewidth"
    PRINT #3, USING MVT$; RHOFF; RVLAS + Dy / 2 + ARL
    PRINT #3, USING LTO$; RHOFF - ART; RVLAS + Dy / 2
    PRINT #3, USING LTO$; RHOFF + ART; RVLAS + Dy / 2
    PRINT #3, "closepath"
    PRINT #3, "fill"
ELSE
    CALL MSETLINE(2)
    CALL MLINE(RHOFF, RVLAS, RHLAS + Dx / 2, RVLAS, GID%, 15)
    CALL MSETLINE(.5)
    XT(1) = RHLAS + Dx / 2 + ARL: YT(1) = RVLAS
    XT(2) = RHLAS + Dx / 2: YT(2) = RVLAS - ART
    XT(3) = RHLAS + Dx / 2: YT(3) = RVLAS + ART
    CALL MPGON(XT(), YT(), 3, 0, GID%)
    CALL MSETLINE(2)
    CALL MLINE(RHOFF, RVLAS, RHOFF, RVOFF - Dy / 2, GID%, 15)
    CALL MSETLINE(.5)
    YT(1) = RVOFF - Dy / 2 - ARL: XT(1) = RHOFF
    YT(2) = RVOFF - Dx / 2: XT(2) = RHOFF - ART
    YT(3) = RVOFF - Dx / 2: XT(3) = RHOFF + ART
    CALL MPGON(XT(), YT(), 3, 0, GID%)
END IF
60780 REM END OF SETUP
60790 REM  X-LABEL
60800 IF NOT IXLAB GOTO 61210
60820 ABC$(32) = "BR6"
60821 ABC$(37) = "br1e6br1bd2d1bl8d1br9bd2br1"
60822 ABC$(61) = "BU2R4BU2L4R4BD4BR2"
60830 ABC$(65) = "U5E1R2F1D2L4R4D3BR2"
60840 ABC$(66) = "U6R3F1D1G1L3R3F1D1G1L3BR6"
60850 ABC$(67) = "BU1U4E1R2F1BD4G1L2H1BR4BD1BR2"
60860 ABC$(68) = "U6R3F1D4G1L3BR6"
60870 ABC$(69) = "U6R4BD3BL2L2D3R4BR2"
60880 ABC$(70) = "U6R4BD3BL1L3D3BR6"
60890 ABC$(71) = "BU1U4E1R2F1BD3L2R2D1G1L2H1BR6BD1"
60900 ABC$(72) = "U6BR4D6BU3L4R4D3BR2"
60910 ABC$(73) = "BU6R2L1D6L1R2BR2"
60920 ABC$(74) = "BU1F1E1U5L1R2BD6BR2"
60930 ABC$(75) = "U6BR4G3L1R1F3BR2"
60940 ABC$(76) = "U6D6R4BR2"
60950 ABC$(77) = "U6D1F2E2U1D6BR2"
60960 ABC$(78) = "U6D1F4U5D6BR2"
60970 ABC$(79) = "BU1U4E1R2F1D4G1L2H1BR6BD1"
60980 ABC$(80) = "U6R3F1D1G1L3BR6BD3"
60990 ABC$(81) = "BU1U4E1R2F1D4G1L2H1BR3BF1F1BR2BU1"
61000 ABC$(82) = "U6R3F1D1G1L3BR1F3BR2"
61010 ABC$(83) = "BU1F1R2E1H4E1R2F1BD5BR2"
61015 ABC$(120) = "BR1BU1E4BL4F4BD1BR2"
61016 ABC$(94) = "BR5BU4H2G2BD4BR5"
61020 ABC$(84) = "BU6R2D6U6R2BD6BR2"
61030 ABC$(85) = "BU6D5F1R2E1U5BD6BR2"
61040 ABC$(86) = "BU6D4F2E2U4BR2BD6"
61050 ABC$(87) = "BU6D5F1E1U1D1F1E1U5BR2BD6"
61060 ABC$(88) = "U1E4U1BL4D1F4D1BR2"
61070 ABC$(89) = "BU6D2F1R2E1U2D5G1L2H1BR6BD1"
61080 ABC$(90) = "BU5U1R4D1G4D1R4U1BR2BD1"
61090 ABC$(48) = "BU1U4E1R1F1D4G1L1H1BR5BD1"
61100 ABC$(49) = "R1U6G2E2D6R1BR2"
61110 ABC$(50) = "BU4U1E1R1F1D1G3D1R3U1D1BR2"
61120 ABC$(51) = "BU5E1R1F1D1G1F1D1G1L1H1BR5BD1"
61121 ABC$(40) = "BR2H2U2E2BD6BR4"
61122 ABC$(41) = "BR2E2U2H2BR4BD6"
61130 ABC$(52) = "BU2U1E3D4L3R3D2BR2"
61140 ABC$(53) = "BU2D1F1R1E1U2H1L2U2R3D1BD5BR2"
61150 ABC$(54) = "BU2E1R1F1D1G1L1H1U4E1R1F1BD5BR2"
61160 ABC$(55) = "BU5U1R3D2G3D1BR5"
61170 ABC$(56) = "BU1U1E1H1U1E1R1F1D1G1L1R1F1D1G1L1H1BR5BD1"
61180 ABC$(57) = "BU4U1E1R1F1D1G1L1H1BR3D3G1L1H1BR5BD1"
61190 ABC$(46) = "BR2"
61195 ABC$(47) = "BR5BU5G4BBD1BR7"
61200 ABC$(45) = "BU3BR1R3BD3BR3": IF TFLAG = 1 THEN TFLAG = 0: RETURN
61210 IF PFLAG% = 0 THEN Labl$(4) = STR$(NNX)
61220 LXL = LEN(XLABEL$): X0 = .5 * (Xoff% + Xlas%) - LXL * 3: Y0 = Ylas% + 20: LXLT = 1000
61225 IF X0 < Xoff% THEN X0 = Xoff%
61226 IF (X0 + LXL * 6) > Xlas% THEN X0 = Xoff%: LXLT = CINT((Xlas% - Xoff%) / 6)
61227 IF LXL > LXLT THEN LXL = LXLT
61230 IF ABS(IACTION) <> 1 THEN DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
YmaxS = Y0 + 3
61240 FOR IX = 1 TO LXL
    61250 C$ = MID$(XLABEL$, IX, 1)
    61260 N = ASC(C$): IF N > 90 AND N <> 94 THEN N = N - 32
    61270 IF N = 46 AND IXLAB THEN PSET STEP(0, 0)
    61275 IF ABS(IACTION) = 1 GOTO 61290
    61280 IF IXLAB THEN DRAW "C14" + ABC$(N)
61290 NEXT IX
61291 IF FontL% = 0 THEN FontL% = 12
IF Font$ = "" THEN Font$ = "Helvetica-Bold"
IF ABS(IACTION) < 1 GOTO 61300
61292 IF IMAKER% = 0 THEN
    PRINT #3, "/" + Font$ + " findfont"
    PRINT #3, USING "[## 0 0 ## 0 0] makefont setfont"; FontL%; FontL%
ELSE
    CALL MSETFONT(Font$, FontL%)
END IF
IF NOXAXIS = 1 OR XLABEL$ = "" GOTO 61300
YLB = Ymin
IF AXREV THEN YLB = Ymax
Rx = .5 * (Xmin + Xmax) * Rcoefx + RXshift
Ry = YLB * RCoefy + RYshift
RYL = Ry - 2.8 * FontL%
IF IMAKER% = 0 THEN
    PRINT #3, USING "###.## ###.## "; Rx; RYL
    PRINT #3, "("; XLABEL$; ") hcenter"
ELSE
    Rx = .5 * (Xmin + Xmax) * Rcoefx + RXshift
    RYL = RVLAS + 2.8 * FontL%
    CALL MCLABEL(XLABEL$, Rx, RYL, GID%)
END IF
61293 GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
61300 Ii = -1:
61305 Ic = 0
61310 FOR I = IXMIN TO IXMAX
    61315 IF SYFLAG = 1 AND Ic = 1 THEN GOTO 61715
    61317 Ii = Ii + 1: IF RFLAG = 1 THEN dddd$ = Labl$(5 + Ii): LLLK = LEN(dddd$): GOSUB 65450: GOTO 61640
    61320 dddd$ = "": LDD = 0
    61330 GAR = I * MSDX * (10 ^ IMX): GOSUB 64041: NANOT = GAR
    61340 IF NANOT < 0 THEN FLAGN = 1: NANOT = -NANOT
    61350 IF ABS(NANOT) < 9.999999E-06 THEN dddd$ = "0": LDD = 5: GOTO 61610
    61360 NANOT$ = STR$(NANOT)
    61370 NANOT$ = MID$(NANOT$, 2)
    61380 LNN = LEN(NANOT$)
    61390 LNAN = LNN - IMX
    61400 IF LNAN < 0 THEN NANOT$ = STRING$(ABS(LNAN), "0") + NANOT$: LNAN = 0
    61405 IF ABS(IACTION) = 1 GOTO 61420
    61410 IF IXANO THEN DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
    61420 FOR J = 1 TO LNAN
        61430 C$ = MID$(NANOT$, J, 1)
        61440 NN = ASC(C$)
        61450 dddd$ = dddd$ + CHR$(NN)
        61460 LDD = LDD + 5
        61470 IF NN = 49 THEN LDD = LDD - 2
    61480 NEXT J
    61490 IF ABS(IMX) < 9.999999E-06 GOTO 61520
    61500 dddd$ = dddd$ + CHR$(46)
    61510 LDD = LDD + 1
    61520 IF FLAGN = 1 THEN dddd$ = "-" + dddd$: NANOT = -NANOT: FLAGN = 0: LDD = LDD + 3
    61530 FOR J = 1 TO IMX
        61540 C$ = MID$(NANOT$, LNAN + J, 1)
        61550 NN = ASC(C$): IF NN = 32 THEN NN = 48
        61560 dddd$ = dddd$ + CHR$(NN)
        61570 LDD = LDD + 5
        61580 IF NN = 49 THEN LDD = LDD - 2
    61590 NEXT J
    61600 IF LEFT$(dddd$, 1) = "." THEN dddd$ = "0" + dddd$: LDD = LDD + 2
    61610 IF LEFT$(dddd$, 2) = "-." THEN dddd$ = "-0" + MID$(dddd$, 2): LDD = LDD + 7
    61620 LLLK = LEN(dddd$): IF PFLAG% = 0 THEN Labl$(5 + Ii) = dddd$
    61630 IF NANOT < 0 THEN LDD = LDD + 8 ELSE LDD = LDD - 1
    61640 X0 = DXsh + (I - IXMIN) * Nx + Xoff%: Y0 = Ylas% + 10
    IF Eflag = 1 THEN
        IF X0 < Xoff% OR X0 > Xlas% THEN GOTO 61715
    END IF
    X0 = X0 - LDD / 2
    IF GFLAG = 1 THEN
        IF X0 < Xoff% OR X0 + LDD > Xlas% THEN GOTO 61715
    END IF
    61645 IF XFLAG% = 1 AND VAL(dddd$) < 0 GOTO 61720
    IF ABS(IACTION) = 1 GOTO 61650
    61650 IF IXANO THEN DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
    61660 FOR LLL = 1 TO LLLK
        61670 C$ = MID$(dddd$, LLL, 1)
        61680 NN = ASC(C$)
        61685 IF ABS(IACTION) = 1 GOTO 61710
        61690 IF NN = 46 AND IXANO THEN PSET STEP(0, 0): DRAW ABC$(46): GOTO 61710
        61700 IF IXANO THEN DRAW "C14" + ABC$(NN)
    61710 NEXT LLL
    61715 IF Ic = 0 THEN Ic = 1 ELSE Ic = 0
    61716 IF ABS(IACTION) < 1 GOTO 61720
    IF NOXAXIS = 1 GOTO 61720
    61717 XLB = XminO + (I - IXMIN) * MSDX
    61718 IF IXANO THEN
        YLB = Ymin
        IF AXREV THEN YLB = Ymax
        Rx = XLB * Rcoefx + RXshift
        Ry = YLB * RCoefy + RYshift
        IF Eflag = 1 THEN
            IF Rx < RHOFF OR Rx > RHLAS THEN GOTO 61719
        END IF
        IF IMAKER% = 0 THEN
            PRINT #3, USING "###.## ###.## "; Rx; Ry - 1.5 * FontL%;
            PRINT #3, "("; dddd$; ") hcenter"
        ELSE
            RYL = Ry + 1.5 * FontL%
            CALL MCLABEL(dddd$, Rx, RYL, GID%)
        END IF
    END IF
    61719 GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
61720 NEXT I
XmaxS = Xlas% + 3 * LEN(dddd$)
61725 DDx$ = dddd$
61730 REM END OF XLABEL
61740 REM  Y-LABEL
61750 IF NOT IYLAB GOTO 62140
61760 ABC$(32) = "BU6"
61765 ABC$(37) = "H4BR4D1BL4BD2D1BU2BR5BU4"
61766 ABC$(43) = "BU3R4L2U2D4U2R2BD2BR2"
61767 ABC$(45) = "BU3R4BD2BR2"
'ABC$(45) = "BL3BU1U4BU1BR3BU2"
61770 ABC$(65) = "L5H1U2E1R2D4U4R3BU2"
61780 ABC$(66) = "L6U3E1R1F1D3U3E1R1F1D3BU6"
61790 ABC$(67) = "BL1L4H1U2E1BR4F1D2G1BU4BR1BU2"
61800 ABC$(68) = "L6U3E1R4F1D3BU6"
61810 ABC$(69) = "L6U4BR3BD2D2R3U4BU2"
61820 ABC$(70) = "L6U4BR3BD1D3R3BU6"
61830 ABC$(71) = "BL1L4H1U2E1BR3D2U2R1F1D2G1BU6BR1"
61840 ABC$(72) = "L6BU4R6BL3D4U4R3BU2"
61850 ABC$(73) = "BL6U2D1R6D1U2BU2"
61860 ABC$(74) = "BL1E1H1L5D1U2BR6BU2"
61870 ABC$(75) = "L6BU4F3D1U1E3BU2"
61880 ABC$(76) = "L6R6U4BU2"
61890 ABC$(77) = "L6R1E2H2L1R6BU2"
61900 ABC$(78) = "L6R1E4L5R6BU2"
61910 ABC$(79) = "BL1L4H1U2E1R4F1D2G1BU6BR1"
61920 ABC$(80) = "L6U3E1R1F1D3BU6BR3"
61930 ABC$(81) = "BL1L4H1U2E1R4F1D2G1BU3BE1E1BU2BL1"
61940 ABC$(82) = "L6U3E1R1F1D3BU1E3BU2"
61950 ABC$(83) = "BL1E1U2H1G4H1U2E1BR5BU2"
61960 ABC$(84) = "BL6U2R6L6U2BR6BU2"
61970 ABC$(85) = "BL6R5E1U2H1L5BR6BU2"
61980 ABC$(86) = "BL6R4E2H2L4BR6BU2"
61985 ABC$(87) = "BL6R5E1H1L1R1E1H1L5BU2BR6"
61990 ABC$(88) = "L1H4L1BD4R1E4R1BU2"
62000 ABC$(89) = "BL6R2E1U2H1L2R5F1D2G1BU6BR1"
62010 ABC$(90) = "BL5L1U4R1F4R1U4L1BU2BR1"
62020 ABC$(48) = "BU1U4E1R1F1D4G1L1H1BR5BD1"
62030 ABC$(49) = "R1U6G2E2D6R1BR2"
62040 ABC$(50) = "BU4U1E1R1F1D1G3D1R3U1D1BR2"
62050 ABC$(51) = "BU5E1R1F1D1G1F1D1G1L1H1BR5BD1"
62060 ABC$(52) = "BU2U1E3D4L3R3D2BR2"
62070 ABC$(53) = "BU2D1F1R1E1U2H1L2U2R3D1BD5BR2"
62080 ABC$(54) = "BU2E1R1F1D1G1L1H1U4E1R1F1BD5BR2"
62090 ABC$(55) = "BU5U1R3D2G3D1BR5"
62100 ABC$(56) = "BU1U1E1H1U1E1R1F1D1G1L1R1F1D1G1L1H1BR5BD1"
62110 ABC$(57) = "BU4U1E1R1F1D1G1L1H1BR3D3G1L1H1BR5BD1"
62120 ABC$(46) = "BR2"
62125 ABC$(47) = "BU1H4BR4BU3"
62131 ABC$(40) = "BU2G2L2H2BU4BR6"
62132 ABC$(41) = "BU2H2L2G2BU2BR6"
YminS = Yoff%
IF TFLAG = 1 THEN TFLAG = 0: RETURN
62140 GOTO 62230
62145 IF PFLAG% = 0 THEN Labl$(6 + NNX) = STR$(NNY) '?????
62150 LYL = LEN(YLABEL$): X0 = Xoff% - Ly - 6: Y0 = .5 * (Yoff% + Ylas%) + LYL * 3: LYLT = 1000 '?????
62155 IF Y0 > Ylas% THEN Y0 = Ylas%
62157 IF (Y0 - LYL * 6) < Yoff% THEN Y0 = Ylas%: LYLT = CINT((Ylas% - Yoff%) / 6)
62158 IF LYL > LYLT THEN LYL = LYLT
XminS = X0 - 6
62159 IF ABS(IACTION) = 1 GOTO 62170
62160 IF IYLAB THEN DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
62170 FOR IY = 1 TO LYL
    62180 C$ = MID$(YLABEL$, IY, 1)
    62190 N = ASC(C$): IF N > 90 AND N <> 94 THEN N = N - 32
    62195 IF ABS(IACTION) = 1 GOTO 62220
    62200 IF N = 46 AND IYLAB THEN PSET STEP(0, 0)
    62210 IF IYLAB THEN DRAW "C14" + ABC$(N)
62220 NEXT IY
62221 IF ABS(IACTION) < 1 GOTO 62225
LDMAX = LDMAX + 1
IF NOYAXIS = 1 OR YLABEL$ = "" GOTO 62225
IF IMAKER% = 0 THEN
    PRINT #3, "("; dddd$; ")"; " dup stringwidth pop neg"
    PRINT #3, USING "###.## add ###.## pop"; Rx; Ry - .375 * FontL%
    PRINT #3, USING "###.## sub"; .625 * FontL%
    Ry = .5 * (Ymin + Ymax) * RCoefy + RYshift
    PRINT #3, USING "###.##"; Ry
    PRINT #3, "/" + Font$ + " findfont"
    PRINT #3, USING "[0 ## -## 0 0 0] makefont setfont"; FontL%; FontL%
    PRINT #3, "("; YLABEL$; ")"; " stringwidth exch pop neg 2 div add"
    PRINT #3, " moveto ("; YLABEL$; ") show"
ELSE
    Rx = RHOFF - .5 * LEN(dddd$) * FontL% - .75 * FontL% - .375 * FontL%
    Ry = .5 * (Ymin + Ymax) * RCoefy + RYshift
    CALL MVLABEL(YLABEL$, Rx, Ry, GID%)
END IF
GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
62225 IF ABS(IACTION) = 1 GOTO 62229
62227 VIEW (Xoff%, Yoff%)-(Xlas%, Ylas%)
62228 IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
XYscr(7) = XminS
XYscr(8) = XmaxS
XYscr(9) = Yoff% - 1
XYscr(10) = YmaxS
62229 GOTO 64050
62230 Ii = -1
62235 Ic = 0: LDMAX = 0
Lymax = 0
KNEG = 0: KPOS = 0
62240 FOR I = IYMIN TO IYMAX
    62250 dddd$ = "": LDD = 0: Ii = Ii + 1
    62252 IF RFLAG = 1 THEN dddd$ = Labl$(7 + NNX + Ii): LLLK = LEN(dddd$): GOSUB 65450: GOTO 62570
    62255 IF SYFLAG = 1 AND Ic = 1 THEN GOTO 62636
    62260 GAR = I * MSDY * (10 ^ IMY): GOSUB 64041: NANOT = GAR: IF NANOT < 0 THEN FLAGN = 1: NANOT = -NANOT
    62270 IF ABS(NANOT) < 9.999999E-06 THEN dddd$ = "0": LDD = 5: GOTO 62540
    62280 NANOT$ = STR$(NANOT)
    62290 IF NANOT >= 0 THEN NANOT$ = MID$(NANOT$, 2)
    62300 LNN = LEN(NANOT$)
    62310 LNAN = LNN - IMY
    62320 IF LNAN < 0 THEN NANOT$ = STRING$(ABS(LNAN), "0") + NANOT$: LNAN = 0
    IF ABS(IACTION) = 1 GOTO 62340
    62330 IF IYANO THEN DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
    62340 FOR J = 1 TO LNAN
        62350 C$ = MID$(NANOT$, J, 1)
        62360 NN = ASC(C$)
        62370 dddd$ = dddd$ + CHR$(NN)
        62380 LDD = LDD + 5
        62390 IF NN = 49 THEN LDD = LDD - 1
    62400 NEXT J
    62410 IF ABS(IMY) < 9.999999E-06 GOTO 62440
    62420 dddd$ = dddd$ + CHR$(46)
    62430 LDD = LDD + 3
    62440 IF FLAGN = 1 THEN dddd$ = "-" + dddd$: NANOT = -NANOT: FLAGN = 0: LDD = LDD + 3
    62450 FOR J = 1 TO IMY
        62460 C$ = MID$(NANOT$, LNAN + J, 1)
        62470 NN = ASC(C$): IF NN = 32 THEN NN = 48
        62480 dddd$ = dddd$ + CHR$(NN)
        62490 LDD = LDD + 5
        62500 IF NN = 49 THEN LDD = LDD - 1
    62510 NEXT J
    62520 IF LEFT$(dddd$, 2) = "-." THEN dddd$ = "-0" + MID$(dddd$, 2): LDD = LDD + 7
    62530 IF LEFT$(dddd$, 1) = "." THEN dddd$ = "0" + dddd$: LDD = LDD + 5
    62540 LLLK = LEN(dddd$): IF PFLAG% = 0 THEN Labl$(7 + NNX + Ii) = dddd$
    62550 IF NANOT >= 0 THEN LDD = LDD + 3
    62560 IF NANOT < 0 THEN LDD = LDD + 7
    62565 IF SF = 1 THEN SF = 0
    62570 X0 = Xoff% - LDD: Y0 = DYsh + Ylas% - (I - IYMIN) * Ny: IF AXREV THEN Y0 = Yoff% + (I - IYMIN) * Ny
    IF Eflag = 1 THEN
        IF Y0 < Yoff% OR Y0 > Ylas% THEN GOTO 62636
    END IF
    IF Lymax < LLLK THEN Lymax = LLLK
    Y0 = Y0 + 3
    62571 IF LLLK > LDMAX THEN LDMAX = LLLK: DDY$ = dddd$
    62572 IF YFLAG% = 1 AND VAL(dddd$) < 0 GOTO 62647
    62573 IF I <> IYMIN AND I <> IYMAX GOTO 62579
    62574 IF Labl$(3) = "" OR HFRAME = 1 GOTO 62579
    62575 IF UPFLAG = 1 GOTO 62579
    62576 IF AXREV <> 0 AND I = IYMIN GOTO 62647
    62577 IF AXREV = 0 AND I = IYMAX GOTO 62647
    62579 IF ABS(IACTION) = 1 GOTO 62590
    62580 IF IYANO THEN DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
    62590 FOR LLL = 1 TO LLLK
        62600 C$ = MID$(dddd$, LLL, 1)
        62610 NN = ASC(C$)
        62615 IF ABS(IACTION) = 1 GOTO 62631
        62620 IF NN = 46 AND IYANO THEN PSET STEP(0, 0): DRAW ABC$(46): GOTO 62631
        62630 IF IYANO THEN DRAW "C14" + ABC$(NN)
    62631 NEXT LLL
    62632 IF ABS(IACTION) < 1 GOTO 62636
    IF NOYAXIS = 1 GOTO 62636
    62633 YLB = YminO + (I - IYMIN) * MSDY
    62634 IF IYANO THEN
        Rx = Xmin * Rcoefx + RXshift - .75 * FontL%
        Ry = YLB * RCoefy + RYshift
        IF Eflag = 1 THEN
            IF Ry < RVOFF OR Ry > RVLAS THEN GOTO 62636
        END IF
        IF IMAKER% = 0 THEN
            PRINT #3, USING "###.## ###.## "; Rx; Ry - .375 * FontL%;
            PRINT #3, "("; dddd$; ") hright"
        ELSE
            RYL = Ry + .375 * FontL%
            CALL MRLABEL(dddd$, Rx, RYL, GID%)
        END IF
    END IF
    GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
    62636 IF Ic = 1 THEN Ic = 0 ELSE Ic = 1
    IF VAL(dddd$) < 0 THEN KNEG = KNEG + 1
    IF VAL(dddd$) > 0 THEN KPOS = KPOS + 1
62647 NEXT I
IF KNEG <= KPOS THEN Lymax = Lymax - 1
62648 ABC$(48) = "BL1L4H1U1E1R4F1D1BU5"
62649 ABC$(49) = "BU3L6F1BR5U2BU2"
62650 ABC$(45) = "BL3U4BR3BU2"
62651 ABC$(94) = "BL4H2E2BR4BU2"
62652 ABC$(120) = "BD1BL1H4BR4G4BR4BU2BR1"
62653 ABC$(50) = "BU3D3L1U0H3L2G1D1F1BR5BU5"
62654 ABC$(51) = "BL1E1U1H1L1G1H1L1G1D1F1BR5BU5"
62655 ABC$(52) = "BU3L6F3R1U3BR2BU2"
62656 ABC$(53) = "BL1R0E1U1H1L2G1D2L2U3R0BR5BU2"
Ly = 5 * Lymax + 3
62660 GOTO 62145
62670 'REM AUTOSCALING SUBROUTINE
60675 IF RFLAG = 1 THEN MSD = (XYMAX - XYMIN) / NNXY: NTIK = NNXY + 1: RETURN
62680 A(1) = 1!: A(2) = 2!: A(3) = 5!
62690 MSD = (XYMAX - XYMIN) / N
62700 FOR I = -11 TO 12
    62710 FOR K = 1 TO 3
        62720 IF MSD <= A(K) * 10 ^ I THEN GOTO 62750
    62730 NEXT K
62740 NEXT I
IF K > 3 THEN K = 3
62750 MSD = A(K) * 10 ^ I
62755 GAR = XYMIN / MSD: GOSUB 64041: Imin = GAR
62760 IF (XYMIN - Imin * MSD) < -9.999999E-05 THEN Imin = Imin - 1
62770 GAR = XYMAX / MSD: Imax = FIX(GAR): IF GAR < 0 THEN Imax = Imax - 1
62780 IF (XYMAX - Imax * MSD) > 9.999999E-05 THEN Imax = Imax + 1
62790 XYMIN = MSD * Imin: XYMAX = MSD * Imax
62800 NTIK = Imax - Imin + 1
62810 Im = I
62830 RETURN
62990 REM SUBR. TO COMPUTE NX,NY
63000 FLAG1 = FALSE: FLAG2 = FALSE
63003 XXXX = Ymin1: XX = MSDY1: MMM = IMY: GOSUB 63490: LENYI = YYYY
63004 XXXX = Ymax1: GOSUB 63490: LENYF = YYYY
63005 LENY = LENYI: IF LENY < LENYF THEN LENY = LENYF
63006 XXXX = XMIN1: XX = MSDX1: MMM = IMX: GOSUB 63490: LENXI = YYYY
63007 XXXX = XMAX1: GOSUB 63490: LENXF = YYYY
63008 LENX = LENXI: IF LENX < LENXF THEN LENX = LENXF
63009 Lx = LENX * 5: Ly = LENY * 6: Xoff = XXOF - KILLANO * IYANO * Ly - KILLANO * IYLAB * 8 + 1: Xlef = LENX * 3 * (-IXANO) * KILLANO
NscXR = INT(NscX - Xoff - Xlef) + 1
NscYR = INT(Nscy - KILLANO * YYOF + 2 * (KILLANO * IXANO + KILLANO * IXLAB)) + 1
63010 NX1 = NscXR / (NTX1 - 1)
63015 NY2 = NscYR / (NTY1 - 1)
63020 NY1 = NX1 / Fact * (MSDY1 / MSDX1)
63030 LLN1 = NY1 * (NTY1 - 1) + YYOF: LYDIF = NscYR - LLN1
63040 IF LLN1 <= NscYR THEN FLAG1 = TRUE
63060 NX2 = NY2 * Fact * (MSDX1 / MSDY1)
63070 LLN2 = NX2 * (NTX1 - 1) + Xoff: LXDIF = NscXR - LLN2
63080 IF LXDIF >= 0 THEN FLAG2 = TRUE
63082 IF LYDIF >= 0 THEN FLAG1 = TRUE
63090 IF IEQUAL = FALSE THEN Nx = NX1: Ny = NY2: GOTO 63280
63100 IF FLAG2 THEN Nx = NX2: Ny = NY2: GOTO 63280
63110 IF FLAG1 THEN Nx = NX1: Ny = NY1: GOTO 63280
63280 IF Lx < Nx AND Ny > 8 THEN SYFLAG = 0: FLAG = 0: RETURN
63281 IF Eflag = 1 AND NNNX <= 5 THEN Nx = NX2: Ny = NY2: RETURN
63295 NNNX = NNNX - 1
FLAG = 1 + FLAG
IF IEQUAL = -1 THEN NNNY = NNNX
IF NNNX = 1 THEN FLAG = 0
63299 RETURN
63490 REM SUBR TO CALCULATE LENGTH
63500 GAR = XXXX * (10 ^ MMM): GOSUB 64041: NNNNN = GAR
63510 NNNNN$ = STR$(NNNNN)
63520 IF NNNNN >= 0 THEN NNNNN$ = MID$(NNNNN$, 2)
63530 LNN = LEN(NNNNN$)
63540 LNAN = LNN - MMM
63550 IF LNAN < 0 THEN NNNNN$ = STRING$(ABS(LNAN), "0") + NNNNN$
63560 YYYY = LEN(NNNNN$): IF RFLAG = 1 THEN YYYY = LLB
63570 IF ABS(XXXX) < 1 AND NNNNN$ <> "0" THEN YYYY = YYYY + 1
63575 IF ABS(XX) < 1 THEN YYYY = YYYY + .6
63580 RETURN
64000 'GET RID OF BLANKS
64010 IF LEFT$(GAR$, 1) = " " THEN GAR$ = MID$(GAR$, 2): GOTO 64010
64020 LLL = LEN(GAR$): IF LLL = 1 THEN RETURN
64030 IF RIGHT$(GAR$, 1) = " " THEN GAR$ = MID$(GAR$, 1, LLL - 1): GOTO 64020
64040 RETURN
64041 'SIMULATE CINT(X)
64042 GARX = GAR: GAR = FIX(GARX): DGAR = ABS(GAR - GARX)
64043 IF DGAR > .5 AND GAR > 0 THEN GAR = GAR + 1: RETURN
64044 IF DGAR > .5 AND GAR < 0 THEN GAR = GAR - 1
64045 RETURN

64050 'ARROWS
'WHILE INKEY$ = "": WEND
64054 'TITLE
'BRING BACK ABC
TFLAG = 1
64057 Kp = 1: KK = 0: GOSUB 60820
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
IF IACTION >= 1 THEN
    IF IMAKER% = 0 THEN
    ELSE
        CALL MSETFONT(Font$, FontL%)
    END IF
END IF
IF IACTION >= 1 THEN
    IF IMAKER% = 0 THEN
    ELSE
        CALL MSETLINE(.5)
    END IF
END IF
IF LOGX = 1 THEN
    NXcyc = Xmax - Xmin
    EYSHIFT = 18
    EXSHIFT = 0
    FOR I = 1 TO NXcyc + 1
        Xc = Xmin + (I - 1)
        Xo = Xc
        Yo = Ymin
        Z$ = STR$(Xc)
        IF Xc > 0 THEN Z$ = MID$(Z$, 2)
        SEXP$ = "10^" + Z$
        GOSUB DrawExp
        IF IACTION >= 1 THEN
            IF IMAKER% = 0 THEN
            ELSE
                XA = Xc * Rcoefx + RXshift
                YA = Ymin * RCoefy + RYshift + 1.5 * FontL%
                CALL MCLABEL(SEXP$, XA, YA, GID%)
            END IF
        END IF
        IF I <= NXcyc THEN
            FOR J = 1 TO 10
                V = J * (10 ^ Xc)
                X = LOG(V) / LOG(10)
                LINE (X, Ymin)-(X, Ymax), 7
                IF IACTION < 1 GOTO W23
                Xp = X * Rcoefx + RXshift
                YpI = Ymin * RCoefy + RYshift
                Ypf = Ymax * RCoefy + RYshift
                IF IMAKER% = 0 THEN
                ELSE
                    CALL MLINE(Xp, YpI, Xp, Ypf, GID%, 11)
                END IF
                W23:
            NEXT
        END IF
    NEXT
    SEXP$ = XLB$
    Xo = .5 * (Xmin + Xmax)
    Yo = Ymin: EYSHIFT = 30
    GOSUB DrawExp
    IF IACTION >= 1 THEN
        IF IMAKER% = 0 THEN
        ELSE
            Rx = .5 * (Xmin + Xmax) * Rcoefx + RXshift
            RYL = RVLAS + 2.8 * FontL%
            CALL MCLABEL(XLB$, Rx, RYL, GID%)
        END IF
    END IF
END IF
IF LOGY = 1 THEN
    NYcyc = Ymax - Ymin
    Z$ = STR$(Ymax)
    IF Ymax > 0 THEN Z$ = MID$(Z$, 2)
    SEXP$ = "10^" + Z$
    EXSHIFT = 3 * LEN(SEXP$)
    EYSHIFT = 4
    FOR J = 1 TO NYcyc + 1
        Yc = Ymin + (J - 1)
        Z$ = STR$(Yc)
        IF Yc > 0 THEN Z$ = MID$(Z$, 2)
        SEXP$ = "10^" + Z$
        Xo = Xmin
        Yo = Yc
        GOSUB DrawExp
        IF IACTION >= 1 THEN
            IF IMAKER% = 0 THEN
            ELSE
                Rx = Xmin * Rcoefx + RXshift - .75 * FontL%
                Ry = Yc * RCoefy + RYshift + .375 * FontL%
                CALL MRLABEL(SEXP$, Rx, Ry, GID%)
            END IF
        END IF
        IF J <= NYcyc THEN
            FOR I = 1 TO 10
                V = I * (10 ^ Yc)
                Y = LOG(V) / LOG(10!)
                LINE (Xmin, Y)-(Xmax, Y), 7
                IF IACTION < 1 GOTO W24
                Yp = Y * RCoefy + RYshift
                XpI = Xmin * Rcoefx + RXshift
                XpF = Xmax * Rcoefx + RXshift
                IF IMAKER% = 0 THEN
                ELSE
                    CALL MLINE(XpI, Yp, XpF, Yp, GID%, 10)
                END IF
                W24:
            NEXT
        END IF
    NEXT
    TFLAG = 1: GOSUB 61760
    SEXP$ = YLB$ + "^"
    Xo = Xmin
    Yo = .5 * (Ymax + Ymin): EXSHIFT = EXSHIFT + 12
    EYSHIFT = 3 * (LEN(YLB$) - 1)
    GOSUB DrawExp
    IF IMAKER% = 0 THEN
    ELSE
        Rx = RHOFF - .5 * 3 * FontL% - .75 * FontL% - .375 * FontL%
        Ry = .5 * (Ymin + Ymax) * RCoefy + RYshift
        CALL MVLABEL(YLB$, Rx, Ry, GID%)
    END IF
END IF
IF LOGX <> 0 OR LOGY <> 0 THEN
    IF IACTION >= 0 THEN
        IF IMAKER% = 0 THEN
        ELSE
            CALL MSETLINE(2!)
            CALL MRECT(RHOFF, RVOFF, RHLAS - RHOFF, RVLAS - RVOFF, GID%, 15)
        END IF
    END IF
END IF
FOR I% = 1 TO 3
    N = INSTR(Kp, Title$, ";")
    IF N <> 0 THEN
        KK = KK + 1
        Titl$(KK) = MID$(Title$, Kp, N - Kp)
        Kp = N + 1
    ELSE
        KK = KK + 1
        Titl$(KK) = MID$(Title$, Kp)
        GOTO 64100
    END IF
NEXT I%
64100 KKL = KK * UPFLAG: IF UPFLAG = 1 THEN KKL = KKL + .5
IF UPFLAG = 0 THEN
    ACCU = 0
ELSE
    ACCU = -((KK - 1) * VAL(Dat$(21, 1)) + 1.5 * VAL(Dat$(21, 2)))
    IF KK = 2 OR KK = 1 THEN ACCU = ACCU - .5 * VAL(Dat$(21, 2))
END IF
64105 FOR K% = 1 TO KK
    SCP = KKL - K%
    64110 Title$ = Titl$(K%): LNT = LEN(Title$)
    NXA = 6: NYA = 6: SBX = CHRW: SBY = CHRH: Fl = 0: XSCR = Xlas% - Xoff%: YSCR = Ylas% - Yoff%
    NCRX = CINT(2 / 3 * LHOR / SBX): NCRY = CINT(.5 * LVER / SBY)
    IF NCRX = 0 THEN NCRX = 1
    IF NCRY = 0 THEN NCRY = 1
    NXS = CINT(XSCR / NCRX): IF NXS < NXA THEN Fl = 1: NXS = NXA: NCRX = INT(XSCR / NXS): SBX = 2 / 3 * LHOR / NCRX
    NYS = CINT(YSCR / NCRY): IF NYS < NYA THEN Fl = 1: NYS = NYA: NCRY = INT(YSCR / NYS): SBY = .5 * LVER / NCRY
    IF IACTION > 0 THEN
        64111 IF K% <> 3 THEN FontL% = VAL(Dat$(21, 1)) ELSE FontL% = VAL(Dat$(21, 2))
        IF IMAKER% = 0 THEN
            PRINT #3, "/" + Font$ + " findfont"
            PRINT #3, USING "[## 0 0 ## 0 0] makefont setfont"; FontL%; FontL%
        ELSE
            CALL MSETFONT(Font$, FontL%)
        END IF
        ACCU = ACCU + FontL%
    END IF
    64117 SCX = ABS(NXS / Coefx): SCY = ABS(NYS / Coefy)
    64118 X0 = (Xmax + Xmin) / 2 - .5 * LNT * SCX
    64119 Y0 = SCP * SCY: IF HFRAME = 1 THEN Y0 = Y0 + Yf + .5 * SCY ELSE Y0 = Y0 + Ymax
    64120 IF AXREV THEN
        64130 Y0 = SCP * SCY: IF HFRAME = 1 THEN Y0 = Yf - Ymax + Ymin + .5 * SCY ELSE Y0 = -Y0 + Ymin
    64131 END IF
    IF UPFLAG = 1 GOTO 64150
    64132 FOR l = 1 TO LNT
        64133 C$ = MID$(Title$, l, 1)
        64134 N = ASC(C$): IF N > 90 AND N <> 94 THEN N = N - 32
        64135 IF IACTION = 1 GOTO 64149
        64136 IF N = 46 THEN PSET (X0 + SCX / 4, Y0) ELSE PRESET (X0, Y0)
        64137 DRAW "C15" + ABC$(N)
        64138 X0 = X0 + SCX: IF N = 47 THEN X0 = X0 + SCX / 6
    64149 NEXT l
    64150 IF IACTION < 1 GOTO 64185
    IF Title$ = "" OR Title$ = " " GOTO 64185
    Rx = .5 * (Xmin + Xmax) * Rcoefx + RXshift
    YLB = Ymax: MSS = MSDY: IF AXREV THEN YLB = Ymin: MSS = -MSDY
    Ry = (YLB + .5 * HFRAME * MSS) * RCoefy + RYshift
    IF IMAKER% = 0 THEN
        PRINT #3, USING "###.## ###.## "; Rx; Ry - ACCU;
        PRINT #3, "("; Title$; ") hcenter"
    ELSE
        CALL MCLABEL(Title$, Rx, Ry + ACCU, GID%)
    END IF
    YLEV = Ymax ': IF HFRAME = 1 THEN YLEV = YF: SCP = SCP + .5
    GOSUB 7001: IF FLRG = 1 THEN GOTO STRT
    64185 'WHILE INKEY$ = "": WEND
NEXT
IF IACTION < 1 GOTO LBFX

64187 GOTO LBFX
65450 'HANDLE LABELS
65451 LL = LEN(dddd$): LDD = 0
65455 FOR HH = 1 TO LL
    65457 C$ = MID$(dddd$, HH, 1): NN = ASC(C$)
    65458 LDD = LDD + 5
    65459 IF NN = 49 THEN LDD = LDD - 1
    65460 IF NN = 46 THEN LDD = LDD - 2
    IF C$ = "-" THEN LDD = LDD - 2
NEXT HH
IF VAL(dddd$) >= 0 THEN LDD = LDD + 3
IF VAL(dddd$) < 0 THEN LDD = LDD + 7
65461 RETURN
65462 LBB = -10: FOR I = 0 TO NNY
    65463 GAR$ = Labl$(5 + NNY + I): LL = LEN(GAR$)
    IF INSTR(1, GAR$, ".") THEN LL = LL - .4
    IF INSTR(1, GAR$, "-") THEN LL = LL - .4
    IF LL > LLB THEN LLB = LL
NEXT I
RETURN
LBFX: IF IACTION = -1 GOTO finwin
IF IACTION <> 0 AND TCK$ = "" THEN
    'LOCATE 1, 60: PRINT "Press any key ..."
    'WHILE INKEY$ = "": WEND
    'LOCATE 1, 60: PRINT STRING$(20, " ")
    GOTO finwin
END IF
IF IACTION <> 0 AND TCK$ = "C" THEN LOCATE 1, 60: PRINT "Press any key ...": WHILE INKEY$ = "": WEND: LOCATE 1, 60: PRINT STRING$(20, " "): GOTO finwin
IF IACTION = 0 AND TCK$ = "R" THEN LOCATE 1, 60: PRINT "Press any key ...": WHILE INKEY$ = "": WEND: LOCATE 1, 60: PRINT STRING$(20, " "): GOTO finwin
IF TCK$ = "R" THEN
    LOCATE 1, 60: INPUT "Fin/Rep/Edit "; IWHAT$
    IF IWHAT$ = "F" OR IWHAT$ = "f" THEN LOCATE 1, 60: PRINT STRING$(20, " "): GOTO finwin
    IF IWHAT$ = "R" OR IWHAT$ = "r" GOTO LBFY
    Na = 0: Ns = 0: ID%(0, 0) = 0
    CALL SCREDIT(XYlim(), XYscr(), XYplt(), Labl$(), PAR$(), Contr(), Xar(), Yar(), Na, Ns, ID%())
    GOTO finwin
    LBFY: IF TCK$ = "R" THEN
        LOCATE 7, 1: PRINT "GOING TO STRT"
        WHILE INKEY$ = "": WEND
        GOTO STRT:
    END IF
END IF

finwin: IF ABS(IACTION) > 0 AND IMAKER% <> 0 THEN
    IF Contr(8) >= 0 THEN
        'CALL MGroup(GID%)
    END IF
END IF
ERASE PAR$, A, Titl$, Page$, Dat$, Dat%, Xar, Yar, ID%, XT, YT
EXIT SUB
DrawExp:
WINDOW: VIEW
N = INSTR(1, SEXP$, "^")
IF N = 0 THEN K = 0 ELSE K = 1
X0 = Coefx * Xo + Xshift - 3 * (LEN(SEXP$) - 1) + (4 - EXSHIFT) * K
IF X0 < 0 THEN X0 = 7
Y0 = Coefy * Yo + Yshift + EYSHIFT
IF IACTION <> 1 THEN DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
FOR BB = 1 TO LEN(SEXP$)
    N$ = MID$(SEXP$, BB, 1)
    IF N$ = "^" THEN
        DRAW "BU5"
    ELSE
        N = ASC(N$)
        DRAW "C15" + ABC$(N)
    END IF
NEXT
VIEW (Xoff%, Yoff%)-(Xlas%, Ylas%)
IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)
RETURN
END SUB

SUB PlotVELOCITY (XYlim(), XYscr(), XYplt(), XYout(), Contr(), IFORD, CLR, DEt)
DIM XYC(1000), XYP(1000), DCX(1000), DCY(1000)
DIM THT(1000), THP(1000), IC(1000), IT(1000), DRT(1000)
'IFORD = 1: FILE ZZ IS NEXT
'IFORD = 2: FILE ZZ IS PREVIOUS
SHARED WDMODE, ROIN%, CMIN%
KSQ = 25
NA = AA(20) + 2 * (KSQ - 1) + 1: ETC = AA(NA)
NA = ZZ(20) + 2 * (KSQ - 1) + 1: ETP = ZZ(NA)
DEt = ETP - ETC
KSQ = 54
NA = AA(20) + 2 * (KSQ - 1) + 1: CLAVER = AA(NA)

DDM = 0: OPT = 0: KA = 0: DSUM = 0
'
FOR OPT = O TO 1
    FOR N = 1 TO NPOL
        CALL GETPOLYDISP(N, XYC(), THT(), XYP(), THP(), IC(), IT(), IBP)
        IF IBP = 0 THEN
            KS = XYC(0)
            FOR K = 1 TO KS
                XC = XYC(2 * K - 1)
                YC = XYC(2 * K)
                TC = THT(K)
                '
                XP = XYP(2 * K - 1)
                YP = XYP(2 * K)
                TP = THP(K)
                '
                DCX(K) = XP - XC
                DCY(K) = YP - YC
                DRT(K) = TP - TC
                '
            NEXT
            FOR K = 1 TO KS
                K1 = K
                K2 = K + 1
                IF K2 > KS THEN
                    K2 = 1
                END IF
                IB1 = IC(K1)
                IB2 = IC(K2)
                ITYPE1 = IT(K1)
                ITYPE2 = IT(K2)
                '
                XC1 = XYC(2 * K1 - 1)
                YC1 = XYC(2 * K1)
                TH1 = THT(K1)
                '
                XC2 = XYC(2 * K2 - 1)
                YC2 = XYC(2 * K2)
                TH2 = THT(K2)
                '
                DX1 = DCX(K1)
                DY1 = DCY(K1)
                DX2 = DCX(K2)
                DY2 = DCY(K2)
                DT1 = DRT(K1)
                DT2 = DRT(K2)
                '
                'IF N = 71 AND IB1 = 404 THEN
                '    LOCATE 3, 1: PRINT IB1, XC1, YC1, TH1
                '    LOCATE 4, 1: PRINT IB1, DX1, DY1, DT1
                '    WHILE INKEY$ = "": WEND
                'END IF
                'IF N = 71 AND IB2 = 481 THEN
                '    LOCATE 5, 1: PRINT IB2, XC2, YC2, TH2
                '    LOCATE 6, 1: PRINT IB2, DX2, DY2, DT2
                '    WHILE INKEY$ = "": WEND
                'END IF
                '
                CALL PLOTRELDISP(XC1, YC1, TH1, XC2, YC2, TH2, ITYPE1, ITYPE2, DX1, DY1, DT1, DX2, DY2, DT2, DDM, DD, OPT, CLAVER)
                IF OPT = 0 THEN
                    DSUM = DSUM + DD
                    KA = KA + 1
                END IF
            NEXT
        END IF
    NEXT
NEXT
KSQ = 25
NA = AA(20) + 2 * (KSQ - 1) + 1: ETC = AA(NA)
NA = ZZ(20) + 2 * (KSQ - 1) + 1: ETP = ZZ(NA)
DE = ETP - ETC
IF KA <> 0 THEN
    DSUM = DSUM / KA
END IF
IF DE <> 0 THEN
    DDM = DDM / DE
    DSUM = DSUM / DE
END IF
LOCATE WDMODE, CMIN% - 8: PRINT USING "RDmax=##.###"; DDM / (2 * CLAVER);
LOCATE WDMODE, CMIN% + 8: PRINT USING "RRavg=##.###"; DSUM / (2 * CLAVER);
CLOSE #6
CALL BORDER(XYlim(), XYscr())
CALL SMALLPLOT(XYlim(), XYscr(), XYout(), Contr(), 3)
CALL CHECKAUTO
END SUB

SUB PLOTRELDISP (XC1, YC1, TH1, XC2, YC2, TH2, ITYP1, ITYP2, DX1, DY1, DT1, DX2, DY2, DT2, DDM, DD, OPT, CLAVER)
NDPART = 14: NCPART = 6
DIM XYCN(4, 2)

IF ITYP1 < 0 THEN
    RBAR1 = -ITYP1
    ECC1 = 1
ELSE
    RBAR1 = SHAPES(ITYP1, 1)
    ECC1 = SHAPES(ITYP1, 2)
END IF
IF ITYP2 < 0 THEN
    RBAR2 = -ITYP2
    ECC2 = 1
ELSE
    RBAR2 = SHAPES(ITYP2, 1)
    ECC2 = SHAPES(ITYP2, 2)
END IF
STIF = 1: XLAMBDA = 0: AMU = 0
'DX1 = 0: DX2 = 0: DY1 = 0: DY2 = 0: DT1 = 0: DT2 = 0
FRN = 0: FTA = 0: DN = 0: DT = 0
CALL FORCES(XC1, YC1, RBAR1, ECC1, TH1, XC2, YC2, RBAR2, ECC2, TH2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, FNA, FTA, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA, BDT, AMU, XYCN())
'
DX = (DN * CNX1 + DT * CTX1)
DY = (DN * CNY1 + DT * CTY1)
IF OPT = 0 THEN
    DD = SQR(DX * DX + DY * DY)
    IF DD > DDM THEN DDM = DD
ELSE
    DX = DX / DDM
    DY = DY / DDM
    Xcen = XYCN(1, 1): Ycen = XYCN(1, 2)
    PSET (Xcen, Ycen), 15
    Xi = Xcen: Yi = Ycen
    Xf = Xcen + 0.5 * CLAVER * DX
    Yf = Ycen + 0.5 * CLAVER * DY
    LINE (Xi, Yi)-(Xf, Yf), 15
END IF
END SUB

SUB PlotPolygons (CLR)
DIM XY1(20000)
FOR N = 1 TO NPOL
    CALL GETPOLY(N, XY1())
    CALL PLOTpoly(XY1(), CLR)
NEXT
END SUB

SUB Polygons (XYlim(), XYscr(), XYplt(), Labl$(), Cntrl())
DIM XY1(20000), XY2(20000)
FOR N = 1 TO NPOL
    CALL GETPOLY(N, XY1())
    CALL PLOTpoly(XY1(), 13)
NEXT
IF IPIPE <> 0 THEN
    KB = NPIPE + NPOL
    FOR N = 1 TO NPOL
        CALL GETPOLY(N, XY1())
        CALL Cg(XY1(), Xc1, Yc1, Area1)
        KM = AA(NPIPE + N)
        NN = AA(KB + KM)
        FOR KN = 1 TO NN
            M = AA(KB + KM + KN)
            CALL GETPOLY(M, XY2())
            CALL Cg(XY2(), Xc2, Yc2, Area2)
            LINE (Xc1, Yc1)-(Xc2, Yc2), 14
        NEXT
    NEXT
END IF
IPIPE = AA(32) 'Number of pipes
NPIPE = AA(31) 'Pointer
END SUB

SUB PolyGroups (XYlim(), XYscr(), XYplt(), Labl$(), Cntrl())
DIM XY1(20000)
CLS
CALL BORDER(XYlim(), XYscr())
CLR = 16
CALL PlotBBalls(XYlim(), XYscr(), CLR)
'
'IGROUP = AA(44) - Pointer to groups
'NGROUP = AA(45) - Number of polygon groups
CLRG = 9
FOR N = 1 TO NGROUP
    LC = IGROUP + N - 1
    IP = AA(LC)
    NP = AA(IP)
    IF NP = 1 THEN
        CLRB = 15
        CLRP = 7
    ELSE
        CLRG = CLRG + 1
        IF CLRG = 15 THEN
            CLRG = 10
        END IF
        CLRB = 15
        CLRP = CLRG
    END IF
    FOR M = 1 TO NP
        I = AA(IP + M)
        CALL GETPOLY(I, XY1())
        ICOUNT = I
        '       LOCATE 1, 6: PRINT "NG="; N; " NP="; NP; " M="; M; " IP="; I; " PS="; XY1(0)
        '       WHILE INKEY$ = "": WEND
        IF XY1(0) > 0 THEN
            CALL PLOTPOLYS(XY1(), CLRB, CLRP)
        END IF
    NEXT
NEXT
END SUB

SUB GETPOLYDISP (IP, XYC(), THT(), XYP(), THP(), IC(), IT(), IBP)
KP = RECPOL + IP
KI = RECPOL + NPOL
KP = AA(KP)
IBP = 0
ING% = 1
IF KP < 0 THEN
    ING% = -1
    KP = -KP
END IF
KS = AA(KI + KP)
FOR KK = 1 TO KS
    IB = AA(KI + KP + KK)
    IC(KK) = IB
    CALL GETDISC(IB, CXc, CYc, Tc, ITYPE, IBTYP)
    THT(KK) = Tc
    IT(KK) = ITYPE
    CALL GETDISZ(IB, PXc, PYc, PTc, ITYPE, IBTYP)
    THP(KK) = PTc
    K1 = 2 * KK - 1: K2 = K1 + 1
    XYC(K1) = CXc: XYC(K2) = CYc
    XYP(K1) = PXc: XYP(K2) = PYc
    IF IBTYP = -2 THEN
        IBP = -2
    END IF
NEXT
XYC(0) = KS * ING%
XYC(0) = KS * ING%
END SUB

SUB GETPOLYall (IP, XYf(), IC(), IN(), Xc, Yc, Area, Xmin, Xmax, Ymin, Ymax)
DIM XY(20000)
KP = RECPOL + IP
KI = RECPOL + NPOL
KP = AA(KP)
ING% = 1
IF KP < 0 THEN
    ING% = -1
    KP = -KP
END IF
KS = AA(KI + KP)
FOR KK = 1 TO KS
    IB = AA(KI + KP + KK)
    IC(KK) = IB
    CALL GETDISC(IB, Xc, Yc, Tc, ITYPE, IBTYP)
    XYf(2 * KK - 1) = Xc: XYf(2 * KK) = Yc
NEXT
XYf(0) = KS * ING%
Xmin = 1.E+37: Xmax = -1.E+37
Ymin = Xmin: Ymax = Xmax
CALL Cg(XYf(), Xc, Yc, Area)
IF IPIPE <> 0 THEN
    KB = NPIPE + NPOL
    N = IP
    KM = AA(NPIPE + N)
    NN = AA(KB + KM)
    FOR KN = 1 TO NN
        M = AA(KB + KM + KN)
        IN(KN) = M
        IF M = 0 THEN
            LOCATE 1, 6: PRINT USING "NO CONNECTION THROUGH SIDE ## OF ######"; KN; IP
            WHILE INKEY$ = "": WEND
            LOCATE 1, 6: PRINT STRING$(73, " ");
        END IF
        CALL GETPOLY(M, XY())
        LS = ABS(XY(0))
        FOR L = 1 TO LS
            Xv = XY(2 * L - 1): Yv = XY(2 * L)
            IF Xv < Xmin THEN Xmin = Xv
            IF Xv > Xmax THEN Xmax = Xv
            IF Yv < Ymin THEN Ymin = Yv
            IF Yv > Ymax THEN Ymax = Yv
        NEXT
    NEXT
    IN(0) = NN
END IF
END SUB

SUB GETPOLY (IP, XY())
ING% = 1
KP = RECPOL + IP
KI = RECPOL + NPOL
KP = AA(KP)
IF KP < 0 THEN
    ING% = -1
    KP = -KP
END IF
KS = AA(KI + KP)
IF UBOUND(XY) < 2 * KS THEN
    'LOCATE 1, 6: PRINT "INSUFFICIENT SIZE FOR POLIGOM "; IP; "WITH KS = "; KS
    'WHILE INKEY$ = "": WEND
    'LOCATE 1, 6: PRINT STRING$(73, " ");
    EXIT SUB
END IF
FOR KK = 1 TO KS
    IB = AA(KI + KP + KK)
    CALL GETDISC(IB, Xc, Yc, Tc, ITYPE, IBTYP)
    XY(2 * KK - 1) = Xc: XY(2 * KK) = Yc
NEXT
XY(0) = KS * ING%
END SUB
'
SUB DRAWCROSS (XC, YC, SZ, CLN)
SHARED Trans()
T% = Trans(0)
IF CLR > 15 THEN CLR = 15
XI = XC - SZ: XF = XI + 2 * SZ
YI = YC - SZ: YF = YI + 2 * SZ
IF T% <> 0 THEN
    XI = XC * Trans(1) + Trans(2) - SZ
    XF = XI + 2 * SZ
    YI = YC * Trans(3) + Trans(4) - SZ
    YF = YI + 2 * SZ
END IF
'
LINE (XI, YC)-(XF, YC), CLN
LINE (XC, YI)-(XC, YF), CLN
'
END SUB

SUB TRACE2P (Efile$, Ofile$)
SHARED XSPMODE, YSPMODE, SPMODE
SHARED SHAPES()
DIM XYlim(10), XYscr(10), XYplt(10), Contr(10), Labl$(30)
CALL CHECKfile(Efile$, ERFILE)
IF ERFILE = 1 THEN
    EXIT SUB
END IF
OPEN Efile$ FOR INPUT AS #8
OPEN Ofile$ FOR OUTPUT AS #7
INPUT #8, RAVER, STIF, AMU, XLAMBDA
INPUT #8, IIB1, IB1, ITAG1, ITYPE1, RBAR1, ECC1
INPUT #8, IIB2, IB2, ITAG2, ITYPE2, RBAR2, ECC2
IF ITYPE1 = 0 THEN
    ITYPE1 = -RBAR1
ELSE
    SHAPES(ITYPE1, 0) = 1: SHAPES(ITYPE1, 1) = RBAR1: SHAPES(ITYPE1, 2) = ECC1
END IF
IF ITYPE2 = 0 THEN
    ITYPE2 = -RBAR2
ELSE
    SHAPES(ITYPE2, 0) = 1: SHAPES(ITYPE2, 1) = RBAR2: SHAPES(ITYPE2, 2) = ECC2
END IF
K = 0: XMIN = 1.E+28: XMAX = -1.E+28: YMIN = 1.E+28: YMAX = -1.E+28
REPREAD:
IF NOT EOF(8) THEN
    INPUT #8, NN, IFLAG
    INPUT #8, Xc, Yc, Xi1, Yi1, Xi2, Yi2, PN, CLN
    INPUT #8, XC1, YC1, THETA1, CVX1, CVY1
    INPUT #8, XC2, YC2, THETA2, CVX2, CVY2
    INPUT #8, CNX1, CNY1, CNX2, CNY2, DFN, DFT
    INPUT #8, DX1, DY1, DT1, DX2, DY2, DT2, DN, DT
    INPUT #8, FNA, FTA, DFX1, DFY1, DFX2, DFY2
    INPUT #8, FNB, FTB, FX1, FY1, FX2, FY2
    WRITE #7, FNA, FNB, DX2, DY2, DT2
    IF K = 0 THEN
        IF XC1 < XMIN THEN XMIN = XC1
        IF XC2 < XMIN THEN XMIN = XC2
        IF XC1 > XMAX THEN XMAX = XC1
        IF XC2 > XMAX THEN XMAX = XC2
        IF YC1 < YMIN THEN YMIN = YC1
        IF YC2 < YMIN THEN YMIN = YC2
        IF YC1 > YMAX THEN YMAX = YC1
        IF YC2 > YMAX THEN YMAX = YC2
        RBR = RBAR1: IF RBR < RBAR2 THEN RBR = RBAR2
        XYlim(8) = XSPMODE: XYlim(9) = YSPMODE: XYlim(10) = SPMODE
        Labl$(1) = "X": Labl$(2) = "Y"
        Contr(1) = -1 ': Contr(2) = -1
        XYscr(1) = YSPMODE - 70: XYscr(2) = YSPMODE - 50: XYscr(3) = 40: XYscr(4) = 0
        XYlim(1) = XMIN - RBR: XYlim(2) = XMAX + RBR: XYlim(3) = YMIN - RBR: XYlim(4) = YMAX + RBR
        SCREEN SPMODE: WINDOW
        CALL WIN(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
    ELSE
        CLS
    END IF
    K = K + 1
    CALL PLOTELS(XC1, YC1, THETA1, ITYPE1, ITAG1, 16)
    CALL PLOTELS(XC2, YC2, THETA2, ITYPE2, ITAG2, 16)
    LOCATE 1, 1: PRINT USING "####### ##.#### ##.#### ##.####^^^^ ##.####^^^^ ##.####^^^^ ##.####^^^^ ##.####^^^^ ##.###^^^^"; NN; PN, CLN, FNA; FTA; DX1; DY1; DX2; DY2;
    LWAIT:
    G$ = INKEY$
    IF G$ = "" THEN
        'GOTO LWAIT:
        GOTO REPREAD
    ELSE
        IF UCASE$(G$) = "X" THEN
            EXIT SUB
        ELSE
            GOTO REPREAD
        END IF
    END IF
END IF
WHILE INKEY$ = "": WEND
CLOSE
END SUB

FUNCTION MatchOList (SN$)
SHARED Olist$()
KG = VAL(Olist$(0))
KF = 0
FOR K = 1 TO KG
    IF Olist$(K) = SN$ THEN
        KF = K
        EXIT FOR
    END IF
NEXT
MatchOList = KF
END FUNCTION

FUNCTION MatchHList (SN$)
SHARED Hlist$()
KG = VAL(Hlist$(0))
KF = 0
FOR K = 1 TO KG
    IF INSTR(1, Hlist$(K), SN$) <> 0 THEN
        KF = K
        EXIT FOR
    END IF
NEXT
MatchHList = KF
END FUNCTION

SUB SaveImage (image AS LONG, filename AS STRING, Xyscr())
WINDOW: VIEW
bytesperpixel& = _PIXELSIZE(image&)
IF bytesperpixel& = 0 THEN PRINT "Text modes unsupported!": END
IF bytesperpixel& = 1 THEN bpp& = 8 ELSE bpp& = 24
x& = _WIDTH(image&)
y& = _HEIGHT(image&)
IF Xyscr(3) <> 0 AND Xyscr(4) <> 0 THEN
    IF x& > Xyscr(2) + 20 THEN x& = Xyscr(2) + 20
    IF y& > Xyscr(4) + 40 THEN y& = Xyscr(4) + 40
END IF
b$ = "BM????QB64????" + MKL$(40) + MKL$(x&) + MKL$(y&) + MKI$(1) + MKI$(bpp&) + MKL$(0) + "????" + STRING$(16, 0) 'partial BMP header info(???? to be filled later)
IF bytesperpixel& = 1 THEN
    FOR c& = 0 TO 255 ' read BGR color settings from JPG image + 1 byte spacer(CHR$(0))
        cv& = _PALETTECOLOR(c&, image&) ' color attribute to read.
        b$ = b$ + CHR$(_BLUE32(cv&)) + CHR$(_GREEN32(cv&)) + CHR$(_RED32(cv&)) + CHR$(0) 'spacer byte
    NEXT
END IF
MID$(b$, 11, 4) = MKL$(LEN(b$)) ' image pixel data offset(BMP header)
lastsource& = _SOURCE
_SOURCE image&
IF ((x& * 3) MOD 4) THEN padder$ = STRING$(4 - ((x& * 3) MOD 4), 0)
FOR py& = y& - 1 TO 0 STEP -1 ' read JPG image pixel color data
    r$ = ""
    FOR px& = 0 TO x& - 1
        c& = POINT(px&, py&) 'POINT 32 bit values are large LONG values
        IF bytesperpixel& = 1 THEN r$ = r$ + CHR$(c&) ELSE r$ = r$ + LEFT$(MKL$(c&), 3)
    NEXT px&
    d$ = d$ + r$ + padder$
NEXT py&
_SOURCE lastsource&
MID$(b$, 35, 4) = MKL$(LEN(d$)) ' image size(BMP header)
b$ = b$ + d$ ' total file data bytes to create file
MID$(b$, 3, 4) = MKL$(LEN(b$)) ' size of data file(BMP header)
IF LCASE$(RIGHT$(filename$, 4)) <> ".bmp" THEN ext$ = ".bmp"
f& = FREEFILE
OPEN filename$ + ext$ FOR OUTPUT AS #f&: CLOSE #f& ' erases an existing file
OPEN filename$ + ext$ FOR BINARY AS #f&
PUT #f&, , b$
CLOSE #f&
END SUB


SUB STRIPDIR (Cfile$)
N = LEN(Cfile$)
L = 0
FOR K = N TO 1 STEP -1
    IF MID$(Cfile$, K, 1) = "\" THEN
        L = K
        EXIT FOR
    END IF
NEXT
IF L <> 0 THEN
    Cfile$ = MID$(Cfile$, L + 1)
END IF
END SUB

SUB SaveData (DMPFILE$, Xar(), Yar(), PAR$(), Labl$())
NSD% = 1: NS = 1
OPEN DMPFILE$ FOR OUTPUT AS #2
PRINT #2, "HEADER=" + "FILE " + Labl$(0)
PRINT #2, USING "NO. OF SCREENS=#"; NSD%
PRINT #2, USING "NO. OF SETS PER SCREEN=#"; NS
PRINT #2, "LEGEND TITLE=" + PAR$(0)
PRINT #2, "TITLE="; Labl$(3)
PRINT #2, "XLABEL=" + Labl$(1)
PRINT #2, "YLABEL=" + Labl$(2)
FOR I = 1 TO NS
    ND = Yar(I, 0)
    PRINT #2, USING "SET # PARAM="; I;: PRINT #2, PAR$(I)
    FOR J = 1 TO ND
        PRINT #2, USING "#.#######^^^^ , #.#######^^^^ "; Xar(I, J); Yar(I, J)
    NEXT J
    PRINT #2, "XEND,YEND"
NEXT I
CLOSE #2
END SUB

SUB SaveDataExt (DMPFILE1$, Xar(), Yar(), PAR$(), Labl$())
NSD% = 1: NS = 1
N = INSTR(1, DMPFILE1$, ".")
DMPFILE$ = MID$(DMPFILE1$, 1, N) + "TXT"
OPEN DMPFILE$ FOR OUTPUT AS #2
PRINT #2, "HEADER=" + "FILE " + Labl$(0)
PRINT #2, USING "NO. OF SCREENS=#"; NSD%
PRINT #2, USING "NO. OF SETS PER SCREEN=#"; NS
PRINT #2, "LEGEND TITLE=" + PAR$(0)
PRINT #2, "TITLE="; Labl$(3)
PRINT #2, "XLABEL=" + Labl$(1)
PRINT #2, "YLABEL=" + Labl$(2)
FOR I = 1 TO NS
    ND = Yar(I, 0)
    PRINT #2, USING "SET # PARAM="; I;: PRINT #2, PAR$(I)
    FOR J = 1 TO ND
        PRINT #2, USING "#### , #.#######^^^^ , #.#######^^^^ "; Xar(0, J); Xar(I, J); Yar(I, J)
    NEXT J
    PRINT #2, "XEND,YEND"
NEXT I
CLOSE #2
END SUB

SUB LastFile (Dfile$)
OPEN "C:\TEMP\LastFile.txt" FOR OUTPUT AS #2
PRINT #2, Dfile$
CLOSE #2
END SUB

SUB MakeFstring (KSEX, KSQ$)
KSQ$ = MID$(STR$(KSEX), 2)
KSQ$ = STRING$(4 - LEN(KSQ$), "0") + KSQ$
END SUB

SUB MakeFirsFile (Cfile$)
N% = LEN(Cfile$)
FOR I% = N% TO 1 STEP -1
    IF MID$(Cfile$, I%, 1) = "\" THEN
        Cfile$ = MID$(Cfile$, I% + 1)
        EXIT FOR
    END IF
NEXT
N1 = INSTR(1, Cfile$, "_")
IF N1 = 0 THEN
    N1 = INSTR(1, Cfile$, "-")
    IF N1 = 0 THEN
        N1 = INSTR(1, Cfile$, "+")
    END IF
END IF
N2 = INSTR(1, Cfile$, ".")
FOR L% = N1 + 1 TO N2 - 1
    MID$(Cfile$, L%, 1) = "0"
NEXT
MID$(Cfile$, N2 - 1, 1) = "1"
END SUB

FUNCTION IfInsidePoly (X, Y, Verts())
DIM Ymin AS DOUBLE: DIM I AS LONG: DIM K AS LONG: DIM icnt AS INTEGER
IF POLYSTYLE% = 1 AND Verts(0) < 0 THEN
    IfInsidePoly = 0
    EXIT SUB
END IF
icnt = 0: N = ABS(Verts(0))
X1 = Verts(1): Y1 = Verts(2)
FOR I = 2 TO N + 1
    K = I
    IF K > N THEN K = 1
    'LOCATE 2, 1: PRINT K
    'WHILE INKEY$ = "": WEND
    X2 = Verts(2 * K - 1)
    Y2 = Verts(2 * K)
    Ymin = Y1
    IF Ymin > Y2 THEN
        Ymin = Y2
    END IF
    IF Y > Ymin THEN
        Ymax = Y1
        IF Ymax < Y2 THEN
            Ymax = Y2
        END IF
        IF Y <= Ymax THEN
            Xmax = X1
            IF Xmax < X2 THEN
                Xmax = X2
            END IF
            IF X <= Xmax THEN
                IF Y1 <> Y2 THEN
                    Xin = X1 + (Y - Y1) * (X2 - X1) / (Y2 - Y1)
                    IF X1 = X2 OR X <= Xin THEN
                        icnt = icnt + 1
                    END IF
                END IF
            END IF
        END IF
    END IF
    X1 = X2
    Y1 = Y2
NEXT
IF icnt = (icnt \ 2) * 2 THEN
    IfInsidePoly = 0
ELSE
    IfInsidePoly = -1
END IF
END FUNCTION

FUNCTION POLYINT (XI(), Y, Verts())
DIM Ymin AS DOUBLE: DIM I AS LONG: DIM K AS LONG: DIM icnt AS INTEGER
icnt = 0: N = ABS(Verts(0))
X1 = Verts(1): Y1 = Verts(2)
FOR I = 2 TO N + 1
    K = I
    IF K > N THEN K = 1
    X2 = Verts(2 * K - 1)
    Y2 = Verts(2 * K)
    Ymin = Y1
    IF Ymin > Y2 THEN
        Ymin = Y2
    END IF
    IF Y > Ymin THEN
        Ymax = Y1
        IF Ymax < Y2 THEN
            Ymax = Y2
        END IF
        IF Y <= Ymax THEN
            Xmax = X1
            IF Xmax < X2 THEN
                Xmax = X2
            END IF
            IF X <= Xmax THEN
                IF Y1 <> Y2 THEN
                    Xin = X1 + (Y - Y1) * (X2 - X1) / (Y2 - Y1)
                    IF X1 = X2 OR X <= Xin THEN
                        FOR L = 1 TO icnt
                            IF Xin < XI(L) THEN
                                FOR K = icnt TO L STEP -1
                                    XI(K + 1) = X(K)
                                NEXT
                                EXIT FOR
                            END IF
                        NEXT
                        XI(L) = Xin
                        icnt = icnt + 1
                    END IF
                END IF
            END IF
        END IF
    END IF
    X1 = X2
    Y1 = Y2
NEXT

END FUNCTION

SUB POLYINT (XI(), Y, Verts())
DIM Ymin AS DOUBLE
DIM I AS INTEGER
DIM K AS INTEGER
DIM icnt AS INTEGER
DIM X, X1, X2, Y1, Y2, Xmax, Ymax, Xin AS DOUBLE
DIM L, N AS INTEGER
icnt = 0: N = Verts(0)
IF N < 0 THEN N = -N
X1 = Verts(1): Y1 = Verts(2)
FOR I = 2 TO N + 1
    K = I
    IF K > N THEN K = 1
    X2 = Verts(2 * K - 1)
    Y2 = Verts(2 * K)
    Ymin = Y1
    IF Ymin > Y2 THEN
        Ymin = Y2
    END IF
    IF Y > Ymin THEN
        Ymax = Y1
        IF Ymax < Y2 THEN
            Ymax = Y2
        END IF
        IF Y <= Ymax THEN
            Xmax = X1
            IF Xmax < X2 THEN
                Xmax = X2
            END IF
            IF X <= Xmax THEN
                IF Y1 <> Y2 THEN
                    Xin = X1 + (Y - Y1) * (X2 - X1) / (Y2 - Y1)
                    IF X1 = X2 OR X <= Xin THEN
                        FOR L = 1 TO icnt
                            IF Xin < XI(L) THEN
                                FOR K = icnt TO L STEP -1
                                    XI(K + 1) = XI(K)
                                NEXT
                                EXIT FOR
                            END IF
                        NEXT
                        XI(L) = Xin
                        icnt = icnt + 1
                    END IF
                END IF
            END IF
        END IF
    END IF
    X1 = X2
    Y1 = Y2
NEXT
XI(0) = icnt
END SUB

SUB ExtPoly (Verts(), Xmin, Xmax, Ymin, Ymax)
DIM N AS INTEGER
DIM X, Y AS DOUBLE
N = Verts(0)
IF N < 0 THEN N = -N
Xmin = 1.0E+37: Ymin = 1.0E+37
Xmax = -1.0E+37: Ymax = -1.0E+37
FOR I = 1 TO N
    X = Verts(2 * I - 1): Y = Verts(2 * I)
    IF X < Xmin THEN Xmin = X
    IF Y < Ymin THEN Ymin = Y
    IF X > Xmax THEN Xmax = X
    IF Y > Ymax THEN Ymax = Y
NEXT
END SUB

SUB InsidePoint (Verts(), Xc, Yc)
DIM Xmin, Xmax, Ymin, Ymax
DIM icnt
DIM XI(20000)
IF Xc = 0.0 THEN Xc = 0.5
CALL ExtPoly(Verts(), Xmin, Xmax, Ymin, Ymax)
Yc = Ymin + Xc * (Ymax - Ymin)
CALL POLYINT(XI(), Yc, Verts())
icnt = XI(0)
IF icnt > 1 THEN
    Xc = 0.5 * (XI(icnt) + XI(icnt - 1))
ELSE
    LOCATE 3, 1: PRINT "SOMETHING BAD "; Xc, icnt
    WHILE INKEY$ = "": WEND
END IF
END SUB

SUB SearchPoly (Xc, Yc, FOUND)
FOUND = 0
DIM XY(20000)
FOR N = 1 TO NPOL
    CALL GETPOLY(N, XY())
    IF IfInsidePoly(Xc, Yc, XY()) = -1 THEN
        FOUND = N
        EXIT FOR
    END IF
NEXT
END SUB

SUB DRAWSTR (Xc, Yc, St$, XYlim(), XYscr())
DIM ABC$(60)
ABC$(45) = "BU3BR1R3BD3BR3"
ABC$(46) = "BR2"
ABC$(47) = "BR5BU5G4BBD1BR7"
ABC$(48) = "BU1U4E1R1F1D4G1L1H1BR5BD1"
ABC$(49) = "R1U6G2E2D6R1BR2"
ABC$(50) = "BU4U1E1R1F1D1G3D1R3U1D1BR2"
ABC$(51) = "BU5E1R1F1D1G1F1D1G1L1H1BR5BD1"
ABC$(40) = "BR2H2U2E2BD6BR4"
ABC$(41) = "BR2E2U2H2BR4BD6"
ABC$(52) = "BU2U1E3D4L3R3D2BR2"
ABC$(53) = "BU2D1F1R1E1U2H1L2U2R3D1BD5BR2"
ABC$(54) = "BU2E1R1F1D1G1L1H1U4E1R1F1BD5BR2"
ABC$(55) = "BU5U1R3D2G3D1BR5"
ABC$(56) = "BU1U1E1H1U1E1R1F1D1G1L1R1F1D1G1L1H1BR5BD1"
ABC$(57) = "BU4U1E1R1F1D1G1L1H1BR3D3G1L1H1BR5BD1"
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
Xoff% = XYscr(1): Xlas% = XYscr(2): Yoff% = XYscr(3): Ylas% = XYscr(4)
AXREV = 0
Coefx = (Xlas% - Xoff%) / (Xmax - Xmin)
Coefy = (Yoff% - Ylas%) / (Ymax - Ymin)
Xshift = Xoff% - Coefx * Xmin
Yshift = Ylas% - Coefy * Ymin
IF AXREV THEN Coefy = -Coefy: Yshift = Yoff% - Coefy * Ymin
WINDOW: VIEW
X0 = Coefx * Xc + Xshift: Y0 = Coefy * Yc + Yshift
DRAW "BM0,0": DRAW "BR=" + VARPTR$(X0): DRAW "BD=" + VARPTR$(Y0)
YmaxS = Y0 + 3
LXL = LEN(St$)
FOR IX = 1 TO LXL
    C$ = MID$(St$, IX, 1)
    N = ASC(C$): IF N > 90 AND N <> 94 THEN N = N - 32
    IF N = 46 THEN PSET STEP(0, 0)
    DRAW "C14" + ABC$(N)
NEXT
VIEW (Xoff%, Yoff%)-(Xlas%, Ylas%)
IF AXREV = 0 THEN WINDOW (Xmin, Ymin)-(Xmax, Ymax) ELSE WINDOW SCREEN(Xmin, Ymin)-(Xmax, Ymax)

END SUB

SUB SAVEascii (Cfile$)
IF Cfile$ = "" THEN
    ASfile$ = "BALLS.DAT"
ELSE
    N = INSTR(1, UCASE$(Cfile$), "BIN")
    ASfile$ = Cfile$
    IF N <> 0 THEN
        MID$(ASfile$, N) = "DAT"
    END IF
END IF
OPEN ASfile$ FOR OUTPUT AS #3
Xmin = AA(21)
Xmax = AA(22)
Ymin = AA(23)
Ymax = AA(24)
BLOC = Xmax - Xmin
HLOC = Ymax - Ymin
S$ = "Number" + CHR$(95) + "of" + CHR$(95) + "particle" + CHR$(95) + "types="
PRINT #3, S$;: PRINT #3, USING "##"; NTYPE
FOR I = 1 TO NTYPE
    NEDGE = SHAPES(I, 0)
    PRINT #3, USING "##"; NEDGE
    FOR K = 1 TO NEDGE
        PRINT #3, USING "#.######^^^^ #.######^^^^"; SHAPES(I, 2 * K - 1); SHAPES(I, 2 * K)
    NEXT
NEXT
PRINT #3, USING "Xmin = ####.#### Xmax = ####.####"; Xmin; Xmax
PRINT #3, USING "Ymin = ####.#### Ymax = ####.####"; Ymin; Ymax
NDISCO = 0
FOR I = 1 TO NDISK
    CALL GETDISC(I, Xc, Yc, Tc, ITYPE, IBTYP)
    IF ITYPE > 0 THEN
        NDISCO = NDISCO + 1
    END IF
NEXT
S$ = "Number" + CHR$(95) + "of" + CHR$(95) + "particles="
PRINT #3, S$;: PRINT #3, USING "######"; NDISCO
KO = 0
FOR I = 1 TO NDISK
    CALL GETDISC(I, Xc, Yc, Tc, ITYPE, IBTYP)
    IF ITYPE > 0 THEN
        KO = KO + 1
        PRINT #3, USING "#.######^^^^ #.######^^^^ #.######^^^^ ##"; Xc; Yc; Tc; ITYPE
    END IF
NEXT
PRINT #3, USING "B=#.######^^^^ H=#.######^^^^ NX=## NY=## NB=####"; BLOC; HLOC; NXB; NYB; NXB * NYB
CLOSE #3
LOCATE 1, 6: PRINT KO; " PARTICLES WRITTEN ";
PRINT ASfile$; " IS GENERATED. Press any key ..."
WHILE INKEY$ = "": WEND
LOCATE 1, 6: PRINT STRING$(80, " ");
END SUB

SUB ExpandPoly (Vxy() AS DOUBLE, Dp AS DOUBLE)
Kpoly = Vxy(0)
ING% = 0
IF Kpoly < 0 THEN
    Kpoly = -Kpoly
    ING% = 1
END IF
DIM Sn(Kpoly) AS DOUBLE: DIM CS(Kpoly) AS DOUBLE: DIM Pp(Kpoly) AS DOUBLE
DIM CosA1 AS DOUBLE: DIM SinA1 AS DOUBLE: DIM Pp1 AS DOUBLE
DIM CosA2 AS DOUBLE: DIM SinA2 AS DOUBLE: DIM Pp2 AS DOUBLE
DIM Xi AS DOUBLE: DIM Yi AS DOUBLE
DIM Ko AS INTEGER
DIM I AS INTEGER
DIM N1 AS INTEGER: DIM N2 AS INTEGER
DIM X1 AS DOUBLE: DIM Y1 AS DOUBLE
DIM X2 AS DOUBLE: DIM Y2 AS DOUBLE
DIM Dx AS DOUBLE: DIM Dy AS DOUBLE
DIM Ds AS DOUBLE
'
FOR K = 1 TO Kpoly
    N1 = K
    N2 = N1 + 1
    IF N2 > Kpoly THEN
        N2 = 1
    END IF
    X1 = Vxy(2 * N1 - 1)
    Y1 = Vxy(2 * N1)
    X2 = Vxy(2 * N2 - 1)
    Y2 = Vxy(2 * N2)
    Dx = X2 - X1
    Dy = Y2 - Y1
    Ds = SQR(Dx * Dx + Dy * Dy)
    IF Ds < 0.000001 THEN
        Ds = 1
    END IF
    Sn(K) = Dx / Ds: CS(K) = -Dy / Ds
    Pp(K) = CS(K) * X1 + Sn(K) * Y1 + Dp
NEXT
'
Ko = 1
FOR K = 1 TO Kpoly
    N1 = K
    N2 = N1 + 1
    IF N2 > Kpoly THEN
        N2 = 1
    END IF
    CosA1 = CS(N1): SinA1 = Sn(N1): Pp1 = Pp(N1)
    CosA2 = CS(N2): SinA2 = Sn(N2): Pp2 = Pp(N2)
    Ko = Ko + 1
    IF Ko > Kpoly THEN
        Ko = 1
    END IF
    IF IntNormal(CosA1, SinA1, Pp1, CosA2, SinA2, Pp2, Xi, Yi) = 0 THEN
        Vxy(2 * Ko - 1) = Xi
        Vxy(2 * Ko) = Yi
    ELSE
        Xi = Vxy(2 * Ko - 1)
        Yi = Vxy(2 * Ko)
        Dx = 0.5D0 * (CosA1 + CosA2) * Dp
        Dy = 0.5D0 * (SinA1 + SinA2) * Dp
        Dd = SQR(Dx * Dx + Dy * Dy)
        Xi = Xi + Dx
        Yi = Yi + Dy
        Vxy(2 * Ko - 1) = Xi
        Vxy(2 * Ko) = Yi
    END IF
NEXT
END SUB

FUNCTION IntNormal (CosA1 AS DOUBLE, SinA1 AS DOUBLE, Pp1 AS DOUBLE, CosA2 AS DOUBLE, SinA2 AS DOUBLE, Pp2 AS DOUBLE, Xi AS DOUBLE, Yi AS DOUBLE)
DIM Dt AS DOUBLE
'Xcosa1 + Ysina1 = p1
'Xcosa2 + Ysina2 = p2
IntNormal = 0
Dt = CosA1 * SinA2 - SinA1 * CosA2
IF ABS(Dt) > 0.1 THEN
    Xi = (Pp1 * SinA2 - Pp2 * SinA1) / Dt
    Yi = (CosA1 * Pp2 - CosA2 * Pp1) / Dt
ELSE
    IntNormal = 1
END IF
END FUNCTION

SUB PLOTPOLYS (XYs(), CLRB, CLRP)
DIM XY(4000) AS DOUBLE
DIM Dp AS DOUBLE
N = XYs(0)
CO = 0.5
ING% = 1
IF N < 0 THEN
    ING% = -1
    N = -N
END IF
XYs(0) = N
IF POLYSTYLE% = 1 AND ING% = -1 THEN
    EXIT SUB
END IF
Xmin = 1.0E+37: Xmax = -1.0E+37
Ymin = 1.0E+37: Ymax = -1.0E+37

FOR I = 1 TO N
    X = XYs(2 * I - 1)
    Y = XYs(2 * I)
    XY(2 * I - 1) = CDBL(X)
    XY(2 * I) = CDBL(Y)
    IF X < Xmin THEN Xmin = X
    IF X > Xmax THEN Xmax = X
    IF Y < Ymin THEN Ymin = Y
    IF Y > Ymax THEN Ymax = Y
NEXT
DX = (Xmax - Xmin) / DPR
DY = (Ymax - Ymin) / DPR
DW = DX
IF DW > DY THEN
    DW = DY
END IF
IRUNMAX = 8
IRUN = 0
againL:
N2 = 2 * N
KX = 1
KY = 2
XY(0) = N
XI = XY(KX)
YI = XY(KY)
IRUNMAX = 2
CLR = CLRB
FOR I = 1 TO N
    KX = KX + 2
    IF KX > N2 THEN KX = 1
    KY = KX + 1
    XF = XY(KX)
    Yf = XY(KY)
    LINE (XI, YI)-(XF, Yf), CLR
    XI = XF
    YI = Yf
NEXT
Dp = CDBL(CO * DPR): IF Dp > 0.20 * DW THEN Dp = 0.20 * DW
CALL ExpandPoly(XY(), -Dp)
IRUN = IRUN + 1
IF IRUN <= IRUNMAX THEN
    GOTO againL
END IF
IF CLRP < 0 THEN
    EXIT SUB
END IF
M = -1
CALL Cg(XYs(), Xg, Yg, Areax)
IF DW > 3 THEN
    M = POINT(Xg, Yg)
    IF M <> 0 THEN
        FOR I = 1 TO 4
            Xg = 0.20 * I
            CALL InsidePoint(XYs(), Xg, Yg)
            M = POINT(Xg, Yg)
            IF M <> CLRB THEN
                EXIT FOR
            END IF
        NEXT
    END IF
END IF
IF DW <= 3 THEN
    PSET (Xg, Yg), CLRP
ELSE
    IF M >= 0 THEN
        PAINT (Xg, Yg), CLRP, CLRB
    END IF
END IF
END SUB

SUB GETDPR (XYlim(), XYscr(), Contr())
AXREV = Contr(2):
Xmin = XYlim(1): Xmax = XYlim(2): Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2): Yoff = XYscr(3): Ylas = XYscr(4)
Coefx = (Xlas - Xoff) / (Xmax - Xmin)
Coefy = (Yoff - Ylas) / (Ymax - Ymin)
Xshift = Xoff - Coefx * Xmin
Yshift = Ylas - Coefy * Ymin
IF AXREV THEN Coefy = -Coefy: Yshift = Yoff - Coefy * Ymin
DPR = 0.5 * (ABS(Coefx) + ABS(Coefy))
DPR = 1 / DPR
END SUB

SUB CHECKAUTO
IF AUT = 1 THEN
    CALL DELAY(GG$)
    EXIT SUB
END IF
IF NOSTOP = 1 THEN
    CALL DELAY(GG$)
    IF UCASE$(GG$) = "A" THEN
        NOSTOP = 0
    END IF
    IF UCASE$(GG$) = "X" THEN
        GTERM$ = "X"
        AUT = 1
    END IF
ELSE
    SLEEP 2
    GG$ = INKEY$
    IF UCASE$(GG$) = "S" THEN
        NOSTOP = 1
    END IF
    IF UCASE$(GG$) = "X" THEN
        GTERM$ = "X"
        AUT = 1
    END IF
END IF
END SUB

SUB SMALLPLOT (XYlim(), XYscr(), XYout(), Contr(), NPLOT)
SHARED XSPMODE, YSPMODE, SPMODE, WDMODE, XDMODE, ND
SHARED Xa1(), Ya1(), Xa2(), Ya2(), Xa3(), Ya3()
SHARED LBA1$(), LBA2$(), LBA3$()
SHARED Trans()
IF NPLOT = 0 THEN EXIT SUB

IF LBA1$(1) = "" AND LBA2$(1) = "" AND LBA3$(1) = "" THEN EXIT SUB

DIM XYlimL(10), XYscrL(10), XYpltL(10), ContrL(10), LablL$(30)
DIM XYlimP(10), XYscrP(10), XYpltP(10), ContrP(10), LablP$(30)
DIM XYlimQ(10), XYscrQ(10), XYpltQ(10), ContrQ(10), LablQ$(30)
DIM PAR$(10)
SLMIN = XYscr(2) + 2: CMIN = INT(SLMIN / 8) + 2: CMAX = CMIN + 27
IF CMAX < XDMODE THEN
    CMIN = CMIN + INT(0.5 * (XDMODE - CMAX))
END IF
VIEW (XYscr(1), XYscr(3))-(XYscr(2), XYscr(4))
FOR I% = 0 TO 10
    XYlimL(I%) = 0: XYscrL(I%) = 0: XYpltL(I%) = 0: ContrL(I%) = 0: LablL$(I%) = ""
    XYlimP(I%) = 0: XYscrP(I%) = 0: XYpltP(I%) = 0: ContrP(I%) = 0: LablP$(I%) = ""
    XYlimQ(I%) = 0: XYscrQ(I%) = 0: XYpltQ(I%) = 0: ContrQ(I%) = 0: LablQ$(I%) = ""
NEXT

XSmin = XYscr(2) + 40: YSmin = XYscr(3) ' X AND Y offsets of the plot area
XSwid = XSPMODE - XSmin - 10 ' Width (X) of the plot area
YPhei = XYscr(4) - XYscr(3) ' Height (Y) of the plot area
YShei = YPhei / 3 - 24 ' Height of plot area for one screen

XYlimL(1) = 0: XYlimL(2) = 1
XYlimL(3) = 0: XYlimL(4) = 0.5

XYlimL(8) = XSPMODE: XYlimL(9) = YSPMODE: XYlimL(10) = SPMODE

XYscrL(1) = XSwid
XYscrL(2) = YShei
XYscrL(3) = XSmin: XYscr(5) = 10
XYscrL(4) = YSmin - 18

LablL$(1) = LBA1$(1): LablL$(2) = LBA1$(2): NS = 1: NA = 0
PAR$(0) = ""
CALL PLOT(Xa1(), Ya1(), PAR$(), NA, NS, XYlimL(), XYscrL(), XYpltL(), LablL$(), ContrL())

IF NPLOT <= 1 THEN EXIT SUB

XYscrP(1) = XSwid
XYscrP(2) = YShei
XYscrP(3) = XSmin: XYscr(5) = 10
XYscrP(4) = XYscrL(4) + 18

ContrP(1) = 0

XYlimP(8) = XSPMODE: XYlimP(9) = YSPMODE: XYlimP(10) = SPMODE
LablP$(1) = LBA2$(1): LablP$(2) = LBA2$(2): NS = 1: NA = 0
PAR$(0) = ""
CALL PLOT(Xa2(), Ya2(), PAR$(), NA, NS, XYlimP(), XYscrP(), XYpltP(), LablP$(), ContrP())

IF NPLOT <= 2 THEN EXIT SUB

XYlimQ(1) = 0: XYlimQ(2) = 1
XYlimQ(3) = 0: XYlimQ(4) = 0.5

XYscrQ(1) = XSwid
XYscrQ(2) = YShei
XYscrQ(3) = XSmin: XYscr(5) = 10
XYscrQ(4) = XYscrP(4) + 18

ContrQ(1) = 0

XYlimQ(8) = XSPMODE: XYlimQ(9) = YSPMODE: XYlimQ(10) = SPMODE
LablQ$(1) = LBA3$(1): LablQ$(2) = LBA3$(2): NS = 1: NA = 0
PAR$(0) = ""
CALL PLOT(Xa3(), Ya3(), PAR$(), NA, NS, XYlimQ(), XYscrQ(), XYpltQ(), LablQ$(), ContrQ())

END SUB

SUB GraphsInfo (Cfile$, NNgrX$, NgrX$, NNgrY$, NgrY$, NBL, NGR)
SHARED Olist$(), Hlist$(), RECMAX
DIM Boxes(30), OPTIONS(30), ITEMS$(30)
DIM SBoxes(30), SOPTIONS(30), SItems$(30)
DIM Saddr(30), Sname$(30), Par$(10), Scale(30)
SHARED Xar(), Yar()
SHARED XSPMODE, YSPMODE, SPMODE, WDMODE
SHARED SELITEMX$, SELITEMY$
'IF NOBFILE = 1 THEN
'    LOCATE 5, 1: PRINT "Closing"
'    WHILE INKEY$ = "": WEND
'EXIT SUB
'END IF
CALL INITfile(Cfile$)
IF VAL(Olist$(0)) = 0 THEN
    PPOS = RECMAX
    CALL SUMMARY(0, PPOS, Cfile$)
    KV = VAL(Olist$(0))
END IF
IF VAL(Hlist$(0)) = 0 THEN
    PPOS = RECHIS
    CALL NEWHIST(0, PPOS)
    KH = VAL(Hlist$(0))
END IF
'
NewXLoc = 0
NITEMS = 11
ITEMS$(1) = "1 - Stress    "
ITEMS$(2) = "2 - Strain    "
ITEMS$(3) = "3 - Contacts  "
ITEMS$(4) = "4 - Normal forces"
ITEMS$(5) = "5 - Shear  forces"
ITEMS$(6) = "6 - Orientations"
ITEMS$(7) = "7 - V/H groups "
ITEMS$(8) = "8 - Equilibrium"
ITEMS$(9) = "9 - Particles"
ITEMS$(10) = "A - Set x-var"
ITEMS$(11) = "B - Exit "
'
IF NNgrX$ = "0" AND NgrX$ = "0" THEN
    HistNoX = 0
    Lx$ = "Output No": Xaddress = 13: SCX = 1
    GOTO SKIPX:
END IF

'
G$ = NNgrX$
NSGG = VAL(G$)
CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
S$ = "0"
CALL Match(S$, NgrX$)
NSG = VAL(S$)
SN$ = SItems$(NSG): Xaddress = Saddr(NSG): SCX = Scale(NSG): Lx$ = SN$
NewXLoc = 0: HistNoX = 0
SS$ = Sname$(NSG)
IF Xaddress = 0 THEN
    NewXLoc = MatchOList(SS$)
    HistNoX = 0
ELSE
    HistNoX = MatchHList(SS$)
END IF
IF NSGG = 9 THEN
    IF NBL <> 0 THEN
        NewXLoc = NBL
        HistNoX = NSGG
        Sc = 1
    END IF
END IF
'
IF NSGG = 7 THEN
    IF NSG <= 5 THEN
        HistNoX = NBL
        NewXLoc = 0
        Xaddress = -NGR + 1
    ELSE
        HistNoX = NBL
        NewXLoc = 0
    END IF
END IF
'
SKIPX:
'
G$ = NNgrY$
NSGG = VAL(G$)
CALL AssignGroup(G$, SItems$(), Sname$(), Saddr(), Scale(), SNitems)
'
S$ = "0"
CALL Match(S$, NgrY$)
NSG = VAL(S$)
SN$ = SItems$(NSG): PaddressY = Saddr(NSG): Sc = Scale(NSG): Ly$ = SN$
SS$ = Sname$(NSG)
NewYLoc = 0: HistNoY = 0
IF PaddressY = 0 THEN
    NewYLoc = MatchOList(SS$)
    HistNoY = 0
ELSE
    HistNoY = MatchHList(SS$)
END IF
IF NSGG = 9 THEN
    IF NBL <> 0 THEN
        NewYLoc = NBL
        HistNoY = NSGG
        Sc = 1
    END IF
END IF
'
IF NSGG = 7 THEN
    IF NSG <= 5 THEN
        HistNoY = NBL
        NewYLoc = 0
        PaddressY = -NGR + 1
    ELSE
        HistNoY = NBL
        NewYLoc = 0
    END IF
END IF
'
CALL Retreve(PaddressY, HistNoY, NewYLoc, Sc, Xaddress, HistNoX, NewXLoc, SCX, Xar(), Yar(), Cfile$)
K% = INSTR(1, Ly$, "-")
IF K% <> 0 THEN
    Ly$ = MID$(Ly$, K% + 2)
END IF
K% = INSTR(1, Lx$, "-")
IF K% <> 0 THEN
    Lx$ = MID$(Lx$, K% + 2)
END IF
SELITEMX$ = Lx$
SELITEMY$ = Ly$
END SUB

SUB TRANSFER (N)
SHARED Xar(), Yar()
SHARED Xa1(), Ya1(), Xa2(), Ya2(), Xa3(), Ya3()
SHARED LBA1$(), LBA2$(), LBA3$(), LablF$()
SHARED SELITEMX$, SELITEMY$
IF N = 0 THEN
    LablF$(1) = SELITEMX$
    LablF$(2) = SELITEMY$
    EXIT SUB
END IF
IF N = 1 THEN
    KD% = Xar(1, 0)
    FOR I% = 0 TO KD%
        Xa1(1, I%) = Xar(1, I%)
        Ya1(1, I%) = Yar(1, I%)
    NEXT
    LBA1$(1) = SELITEMX$
    LBA1$(2) = SELITEMY$
    EXIT SUB
END IF
IF N = 2 THEN
    KD% = Xar(1, 0)
    FOR I% = 0 TO KD%
        Xa2(1, I%) = Xar(1, I%)
        Ya2(1, I%) = Yar(1, I%)
    NEXT
    LBA2$(1) = SELITEMX$
    LBA2$(2) = SELITEMY$
    EXIT SUB
END IF
IF N = 3 THEN
    KD% = Xar(1, 0)
    FOR I% = 0 TO KD%
        Xa3(1, I%) = Xar(1, I%)
        Ya3(1, I%) = Yar(1, I%)
    NEXT
    LBA3$(1) = SELITEMX$
    LBA3$(2) = SELITEMY$
    EXIT SUB
END IF
END SUB

SUB NEILIST (IBL, PLS(), BLS(), Xmin, Xmax, Ymin, Ymax, XG, YG)
DIM IC(1000), XY(2000)
Xmin = 1.E+37: Xmax = -1.E+37
Ymin = 1.E+37: Ymax = -1.E+37
NN = 0
PLS(0) = NN
BLS(1) = IBL
BLS(0) = 1
FOR IP = 1 TO NPOL
    KP = RECPOL + IP
    KI = RECPOL + NPOL
    KP = AA(KP)
    ING% = 1
    IF KP < 0 THEN
        ING% = -1
        KP = -KP
    END IF
    KS = AA(KI + KP)
    FND = 0
    FOR KK = 1 TO KS
        IB = AA(KI + KP + KK)
        IC(KK) = IB
        IF IB = IBL THEN
            NN = NN + 1
            PLS(NN) = IP
            PLS(0) = NN
            FND = KK
        END IF
    NEXT
    IF FND <> 0 THEN
        CALL ADDTOLIST(IC(), KS, BLS(), Xmin, Xmax, Ymin, Ymax)
    END IF
NEXT
CALL GETDISC(IBL, Xc, Yc, Tc, ITYPE, IBTYP)
NN = PLS(0)
XG = Xc: YG = Yc
IF NN = 0 THEN
    FOR IP = 1 TO NPOL
        CALL GETPOLY(IP, XY())
        IF IfInsidePoly(Xc, Yc, XY()) = -1 THEN
            PLS(1) = IP
            PLS(0) = 1
            KP = RECPOL + IP
            KI = RECPOL + NPOL
            KP = AA(KP)
            ING% = 1
            IF KP < 0 THEN
                ING% = -1
                KP = -KP
            END IF
            KS = AA(KI + KP)
            FOR KK = 1 TO KS
                IB = AA(KI + KP + KK)
                IC(KK) = IB
            NEXT
            IC(0) = KS
            CALL ADDTOLIST(IC(), KS, BLS(), Xmin, Xmax, Ymin, Ymax)
            EXIT FOR
        END IF
    NEXT
    ' THERE MAY BE OTHER BALLS IN THE POLYGON - CHECK

    FOR N = 1 TO NDISK
        IF N <> IBL THEN
            CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
            IF Xc > Xmin AND Xc < Xmax THEN
                IF Yc > Ymin AND Yc < Ymax THEN
                    IF IBTYP = 0 THEN
                        IF IfInsidePoly(Xc, Yc, XY()) = -1 THEN
                            KS = BLS(0) + 1
                            BLS(KS) = N
                            BLS(0) = KS
                        END IF
                    END IF
                END IF
            END IF
        END IF
    NEXT
ELSE
    FOR KN = 1 TO NN
        IP = PLS(KN)
        CALL GETPOLY(IP, XY())
        FOR N = 1 TO NDISK
            IF N <> IBL THEN
                CALL GETDISC(N, Xc, Yc, Tc, ITYPE, IBTYP)
                IF Xc > Xmin AND Xc < Xmax THEN
                    IF Yc > Ymin AND Yc < Ymax THEN
                        IF IBTYP = 0 THEN
                            IF IfInsidePoly(Xc, Yc, XY()) = -1 THEN
                                KS = BLS(0) + 1
                                BLS(KS) = N
                                BLS(0) = KS
                            END IF
                        END IF
                    END IF
                END IF
            END IF
        NEXT
    NEXT
END IF
END SUB

SUB ADDTOLIST (IC(), KS, BLS(), Xmin, Xmax, Ymin, Ymax)
SHARED SHAPES()
NB = BLS(0)
FOR K = 1 TO KS
    IB = IC(K)
    FND = 0
    FOR N = 1 TO NB
        IF BLS(N) = IB THEN
            FND = N
        END IF
    NEXT
    IF FND = 0 THEN
        NB = NB + 1
        BLS(NB) = IB
        BLS(0) = NB
        CALL GETDISC(IB, Xc, Yc, Tc, ITYPE, IBTYP)
        IF ITYPE < 0 THEN
            RBAR = -ITYPE
            Ecc = 1
        ELSE
            RBAR = SHAPES(ITYPE, 1)
            Ecc = SHAPES(ITYPE, 2)
            IF IBTYP <> 0 THEN Ecc = 0
        END IF
        RBAR = RBAR * (1 + Ecc)
        IF Xc + RBAR > Xmax THEN
            Xmax = Xc + RBAR
        END IF
        IF Xc - RBAR < Xmin THEN
            Xmin = Xc - RBAR
        END IF
        IF Yc + RBAR > Ymax THEN
            Ymax = Yc + RBAR
        END IF
        IF Yc - RBAR < Ymin THEN
            Ymin = Yc - RBAR
        END IF
    END IF
NEXT
END SUB

SUB FirstShell (XYlim(), XYscr(), XYplt(), XYout(), Labl$(), Contr())
SHARED XSPMODE, YSPMODE, SPMODE, WDMODE, XDMODE, ND, LASTBALL
SHARED XYlimF(), XYscrF(), XYpltF(), XYoutF(), ContrF(), LablF$()
SHARED XYlimT(), XYscrT(), XYpltT(), XYoutT(), ContrT(), LablT$()
SHARED Xar(), Yar()
SHARED Trans()
SHARED Cfile$, LNO, NOBMP
DIM NEIGH(1000), IADCONT(1000), APPLX(1000), APPLY(1000), FORX(1000), FORY(1000), XY(40)
DIM IB1R(1000), IB2R(1000), FRNR(1000), FTNR(1000)
DIM XYscrL(10), XYlimL(10), XYpltL(10), ContrL(10), LablL$(30), XYoutL(10), XYCN(4, 2)
DIM XYscrP(10), XYlimP(10), XYpltP(10), ContrP(10), LablP$(30)
DIM XYlimQ(10), XYscrQ(10), XYpltQ(10), ContrQ(10), LablQ$(30)
DIM XYlimR(10), XYscrR(10), XYpltR(10), ContrR(10), LablR$(30)
DIM XINT1(1000), YINT1(1000), XINT2(1000), YINT2(1000), PNO(1000)
DIM PLS(20), BLS(1000), Par$(10)
' LablF$(30) = 0  - locator will be called locally
' LablF$(30) =-1  - selection using XYout set outside
' LablF$(30) = N  - use ball N without search
'              In AUT = 0 set XYlim() = XYlimL() on first call
'              In AUT = 0 set XYlimL()= XYlim() after
FOUND = 0
IF VAL(LablF$(30)) <> 0 THEN
    FOUND = VAL(LablF$(30))
    IF FOUND = -1 THEN
        FOUND = 0
        LablF$(30) = "0"
        G$ = ""
        GOTO NOLOC
    END IF
END IF
Mes$ = "Locate  particle (Esc - finish K - manual entry)   "
REPINFO:
IF FOUND <> 0 THEN
    G$ = "K"
    GOTO NOLOC
END IF
LOCATE 1, 6: PRINT Mes$
CALL LOCATOR(XYlim(), XYscr(), XYout(), G$, Contr())
NOLOC:
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
Xoff = XYscr(1): Xlas = XYscr(2)
Yoff = XYscr(3): Ylas = XYscr(4)
Trans(1) = (Xlas - Xoff) / (Xmax - Xmin)
Trans(3) = (Yoff - Ylas) / (Ymax - Ymin)
Trans(2) = Xoff - Trans(1) * Xmin
Trans(4) = Ylas - Trans(3) * Ymin
Trans(0) = 0
SLMIN = XYscr(2) + 2: CMIN = INT(SLMIN / 8) + 2: CMAX = CMIN + 27
IF CMAX < XDMODE THEN
    CMIN = CMIN + INT(0.5 * (XDMODE - CMAX))
END IF
PCOPY 0, 1
VIEW (SLMIN, 0)-(XSPMODE - 1, YSPMODE - 1): CLS
IF UCASE$(G$) = "K" THEN
    IF FOUND <> 0 GOTO DOWORK
    CALL CLEAN
    LOCATE 1, 6: INPUT "Particle Number [0 - max force]"; FOUND
    IF FOUND = 0 THEN
        XYout(1) = 0.0: XYout(2) = 0.0
        XYout(3) = 0.0: XYout(4) = 0.0
        GOTO DOSEARCH
    END IF
    DOWORK:
    CALL GETDISC(FOUND, Xc, Yc, Tc, ITYPE, IBTYP)
    'CALL CLEAN: LOCATE 1, 6: PRINT Mes$;
    CALL GETDISC(FOUND, XC1, YC1, THETA1, ITYPE1, IBTYP1)
    GOTO BPinfo
END IF
IF G$ = CHR$(27) THEN
    VIEW (SLMIN, 0)-(XSPMODE - 1, YSPMODE - 1): CLS
    CALL CLEAN
    EXIT SUB
END IF
DOSEARCH:
IF XYout(1) = XYout(2) AND XYout(3) = XYout(4) THEN
    Xo = XYout(1): Yo = XYout(3): IBTYP1 = -1
    CALL SearchBALL(Xo, Yo, THETA1, N, XC1, YC1, ITYPE1, IBTYP1, FOUND)
    IF FOUND = 0 GOTO REPINFO
    BPinfo:
    IABFOUND = M1 + (FOUND - 1) * NDPART
    IAD = IABFOUND
    CALL PLOTELS(XC1, YC1, THETA1, ITYPE1, IBTYP1, 15)
    IF ITYPE1 < 0 THEN
        RBAR1 = -ITYPE1
        ECC1 = 1
    ELSE
        RBAR1 = R(ITYPE1)
        ECC1 = E(ITYPE1)
    END IF
    SLMIN = XYscr(2) + 2: CMIN = INT(SLMIN / 8) + 2: CMAX = CMIN + 27
    IF CMAX < XDMODE THEN
        CMIN = CMIN + INT(0.5 * (XDMODE - CMAX))
    END IF
    VIEW (SLMIN, 0)-(XSPMODE - 1, YSPMODE - 1): CLS
    VIEW (XYscr(1), XYscr(3))-(XYscr(2), XYscr(4))
    CALL NEILIST(FOUND, PLS(), BLS(), XminL, XmaxL, YminL, YmaxL, XG, YG)
    FOR I% = 0 TO 10
        XYlimP(I%) = 0: XYscrP(I%) = 0: XYpltP(I%) = 0: ContrP(I%) = 0: LablP$(I%) = ""
        XYlimQ(I%) = 0: XYscrQ(I%) = 0: XYpltQ(I%) = 0: ContrQ(I%) = 0: LablQ$(I%) = ""
        XYlimR(I%) = 0: XYscrR(I%) = 0: XYpltR(I%) = 0: ContrR(I%) = 0: LablR$(I%) = ""
    NEXT
    '
    FOR I% = 0 TO 10
        XYlimL(I%) = 0: XYscrL(I%) = 0: XYpltL(I%) = 0: ContrL(I%) = 0: LablL$(I%) = ""
    NEXT
    WID = XmaxL - XminL: IF WID < (YmaxL - YminL) THEN WID = YmaxL - YminL
    WID = 0.5 * WID * 1.05
    XC1 = 0.5 * (XminL + XmaxL) - XG: YC1 = 0.5 * (YmaxL + YminL) - YG
    XYlimL(1) = XC1 - WID: XYlimL(2) = XC1 + WID
    XYlimL(3) = YC1 - WID: XYlimL(4) = YC1 + WID
    ContrL(7) = 1
    IF NOBMP = 1 THEN
        IF KSEQ = 1 THEN
            FOR I = 1 TO 10
                XYlimT(I) = XYlimL(I)
                XYscrT(I) = XYscrL(I)
            NEXT
        ELSE
            FOR I = 1 TO 10
                XYlimL(I) = XYlimT(I)
                XYscrL(I) = XYscrT(I)
            NEXT
        END IF
    END IF
    '
    WINDOW: VIEW: CLS
    CALL MPLOT(XYlimL(), XYscrL(), XYpltL(), LablL$(), ContrL())
    PLS(0) = -PLS(0) 'DO NOT PLOT CLUSTER HERE - NO POLYGONS SHOWN
    CALL PlotCluster(XYlimL(), XYscrL(), PLS(), BLS(), XG, YG)
    PLS(0) = -PLS(0) 'RESTORE
    '
    LOCATE 1, 6
    PRINT USING "Part ##### Addr ###### Type ###"; FOUND; IABFOUND; AA(IAD + 9);
    PRINT USING " Xc=####.###### Yc=####.###### Tc=###.###### "; (AA(IAD + 0) + AA(IAD + 11)); (AA(IAD + 1) + AA(IAD + 12)); (AA(IAD + 10) + AA(IAD + 13))
    LOCATE WDMODE, 6
    PRINT USING "Fx=#.###^^^^ Fy=#.###^^^^"; AA(IAD + 5); AA(IAD + 6);
    PRINT USING " Vx=#.###^^^^ Vy=#.###^^^^"; AA(IAD + 2); AA(IAD + 3);
    PRINT USING " Vv=#.###^^^^ Ff=#.###^^^^"; SQR(AA(IAD + 2) ^ 2 + AA(IAD + 3) ^ 2); SQR(AA(IAD + 5) ^ 2 + AA(IAD + 6) ^ 2);
    NC = 0: KN = 0
    NA = AA(20) + 2 * (47 - 1) + 1: FCAVG = AA(NA)
    FORX(0) = AA(IAD + 5)
    FORY(0) = AA(IAD + 6)
    IAD = M1 + NDISK * NDPART
    SMX = 0: SMY = 0
    XY(1) = 0: XY(2) = 0
    FAV = 0!
    '
    ContInfo:
    '
    IB1 = AA(IAD)
    IF IB1 = 0 GOTO ContFin
    IB2 = AA(IAD + 1)
    FRN = -AA(IAD + 4)
    IF FRN = 0 THEN GOTO NextCont:
    FRT = -AA(IAD + 5)
    IB = (IB1 - M1) / NDPART + 1
    CALL IFINLIST(IB, BLS(), FND1)
    IB = (IB2 - M1) / NDPART + 1
    CALL IFINLIST(IB, BLS(), FND2)
    IF FND1 <> 0 AND FND2 <> 0 THEN
        FND = FND1
        IF FND > FND2 THEN FND = FND2
        IF FND = FND1 THEN
            FND2 = 0
        END IF
        IF FND = FND2 THEN
            FND1 = 0
        END IF
    END IF
    IF FND1 <> 0 THEN
        KN = KN + 1
        IADCONT(KN) = IAD
        NEIGH(KN) = FND1
    END IF
    IF FND2 <> 0 THEN
        KN = KN + 1
        IADCONT(KN) = IAD
        NEIGH(KN) = FND2
    END IF
    NextCont:
    IAD = IAD + NCPART
    GOTO ContInfo
    ContFin:
    '
    NA = AA(20) + 2 * (47 - 1) + 1
    CLR = 12: FCAVG = AA(NA): THMAX = .017
    IF FCAVG = 0 THEN FCAVG = 3000.00
    PNTMAX = 72 * THMAX / 2.54: COE = 1!
    '
    KC = 0
    FAV = 0
    FOR K = 1 TO KN
        IAD = IADCONT(K)
        IB1 = AA(IAD)
        IB2 = AA(IAD + 1)
        IF IB2 = IABFOUND THEN
            SWAP IB1, IB2
        END IF
        FRN = -AA(IAD + 4)
        IF FRN = 0 THEN GOTO NextCont:
        FRT = -AA(IAD + 5)
        FRC = SQR(FRN * FRN + FRT * FRT)
        TN = FRC / (3 * FCAVG) * THMAX: IF TN > THMAX THEN TN = THMAX
        CALL GETDISC(-IB1, XC1, YC1, THETA1, ITYP1, IBTYP1)
        CALL GETDISC(-IB2, XC2, YC2, THETA2, ITYP2, IBTYP2)
        IF ITYP1 < 0 THEN
            RBAR1 = -ITYP1
            ECC1 = 1
        ELSE
            RBAR1 = SHAPES(ITYP1, 1)
            ECC1 = SHAPES(ITYP1, 2)
            IF IBTYP1 <> 0 THEN ECC1 = 0
        END IF
        IF ITYP2 < 0 THEN
            RBAR2 = -ITYP2
            ECC2 = 1
        ELSE
            RBAR2 = SHAPES(ITYP2, 1)
            ECC2 = SHAPES(ITYP2, 2)
            IF IBTYP2 <> 0 THEN ECC2 = 0
        END IF
        DX1 = 0: DX2 = 0: DT1 = 0: DY1 = 0: DY2 = 0: DT2 = 0
        xFNA = FRN: FTA = 0: DN = 0: DT = 0
        CALL FORCES(XC1, YC1, RBAR1, ECC1, THETA1, XC2, YC2, RBAR2, ECC2, THETA2, CVX1, CVY1, CVX2, CVY2, CNX1, CNY1, CTX1, CTY1, DX1, DY1, DT1, DX2, DY2, DT2, xFNA, FTA, DFN, DFT, FXD1, FYD1, FXD2, FYD2, DM1, DM2, DN, DT, IFLAG, STIF, XLAMBDA, BDT, AMU, XYCN())
        IF IFLAG = 0 THEN
            IF FRN <> 0 THEN
                LOCATE 1, 1: PRINT IB1
                WHILE INKEY$ = "": WEND
                GOTO SSSX
            END IF
        END IF
        '
        IF NEIGH(K) = 1 THEN
            KC = KC + 1
            APPLX(KC) = XYCN(1, 1) - XG
            APPLY(KC) = XYCN(1, 2) - YG
            FORX(KC) = FRN * CNX1 + FRT * CTX1
            FORY(KC) = FRN * CNY1 + FRT * CTY1
            XINT1(KC) = XYCN(2, 1): YINT1(KN) = XYCN(2, 2)
            XINT2(KC) = XYCN(3, 1): YINT2(KN) = XYCN(3, 2)
            SMX = SMX + FORX(KC)
            SMY = SMY + FORY(KC)
            FF = SQR(FORX(KC) ^ 2 + FORY(KC) ^ 2)
            FAV = FAV + FF
            KC1 = KC + 1
            XY(2 * KC1 - 1) = SMX
            XY(2 * KC1) = SMY
        END IF
        '
        CVL1 = SQR(CVX1 ^ 2 + CVY1 ^ 2)
        CVL2 = SQR(CVX2 ^ 2 + CVY2 ^ 2)
        CSLN1 = (CNX1 * CVX1 + CNY1 * CVY1) / CVL1
        CSLN2 = -(CNX1 * CVX2 + CNY1 * CVY2) / CVL2
        IF IFLAG <> 2 THEN
            GOTO SSSX
        END IF
        Xcen = XYCN(1, 1): Ycen = XYCN(1, 2)
        CVL1 = COE * CVL1 * CSLN1
        CVL2 = COE * CVL2 * CSLN2
        CFX = FRN * CNX1 + FRT * CTX1
        CFY = FRN * CNY1 + FRT * CTY1
        CFL = SQR(CFX * CFX + CFY * CFY)
        CFX = CFX / CFL
        CFY = CFY / CFL
        TN = (FRC / FCAVG) * PNTMAX
        IF TN > PNTMAX THEN TN = PNTMAX
        XI = Xcen - CFX * CVL2: YI = Ycen - CFY * CVL2
        XF = Xcen + CFX * CVL1: Yf = Ycen + CFY * CVL1
        XI = XI - XG: YI = YI - YG
        XF = XF - XG: Yf = Yf - YG
        '        IIN% = 0: IFI% = 0
        '        IF XI > Xmin AND XI < Xmax AND YI > Ymin AND YI < Ymax THEN IIN% = 1
        '        IF XF > Xmin AND XF < Xmax AND Yf > Ymin AND Yf < Ymax THEN IFI% = 1
        '        IF IFI% + IIN% > 0 THEN
        CALL SLINE(XI, YI, XF, Yf, 12, TN, XYlimL(), XYscrL(), XYpltL(), ContrL())
        '        END IF
        SSSX:
    NEXT
END IF

XSmin = XYscrL(2) + 40: YSmin = XYscrL(3) ' X AND Y offsets of the plot area
XSwid = XSPMODE - XSmin - 10 ' Width (X) of the plot area
YPhei = XYscrL(4) - XYscrL(3) ' Height (Y) of the plot area
YShei = YPhei / 3 - 20 ' Height of plot area for one screen
IF YShei < XSwid THEN
    Dfr = XSwid - YShei
    XSmin = XSmin + 0.5 * Dfr
    XSwid = YShei
END IF
'
' Start if small cluster window
'
XYlimP(8) = XSPMODE: XYlimP(9) = YSPMODE: XYlimP(10) = SPMODE
XYscrP(1) = XSwid
XYscrP(2) = XSwid
XYscrP(3) = XSmin: XYscr(5) = 10
XYscrP(4) = YSmin - 18
LablP$(1) = "X": LablP$(2) = "Y"
XYlimP(1) = XYlimL(1): XYlimP(2) = XYlimL(2): XYlimP(3) = XYlimL(3): XYlimP(4) = XYlimL(4)
ContrP(7) = 1: ContrP(1) = -1:
CALL WIN(XYlimP(), XYscrP(), XYpltP(), LablP$(), ContrP())
CALL PlotCluster(XYlimP(), XYscrP(), PLS(), BLS(), XG, YG)
'
'Plot foces on small window particle
'
KN = KC
FAV = FAV / KN
WINWX = .5 * (XYlimP(2) - XYlimP(1))
WINWY = .5 * (XYlimP(4) - XYlimP(3))
WID = WINWX
IF WINWY < WID THEN WID = WINWY
KN1 = KN + 1
XY(0) = KN1
XY(1) = 0: XY(2) = 0
IF KN = 0 THEN
    XY(3) = FORX(0)
    XY(4) = FORY(0)
    KN = 1
    KN1 = 2
    FAV = SQR(XY(3) ^ 2 + XY(4) ^ 2)
    IF FAV = 0 THEN
        FAV = FCAVG
    END IF
END IF
FXmin = 1.E+37: FXmax = -1.E+37
FYmin = 1.E+37: FYmax = -1.E+37
FOR I = 1 TO KN1
    Fx = XY(2 * I - 1)
    Fy = XY(2 * I)
    IF Fx < FXmin THEN FXmin = Fx
    IF Fx > FXmax THEN FXmax = Fx
    IF Fy < FYmin THEN FYmin = Fy
    IF Fy > FYmax THEN FYmax = Fy
NEXT
WIDX = FXmax - FXmin
WIDY = FYmax - FYmin
WID = WIDX
WID = 0.5 * WID * 1.05
IF WID < WIDY THEN WID = WIDY
IF WID = 0 THEN
    WID = 0.1 * FCAVG
    FXmin = FCAVG
    FXmax = FCAVG
    FYmin = FCAVG
    FYmax = FCAVG
END IF
FXC = 0.5 * (FXmin + FXmax): FYC = 0.5 * (FYmin + FYmax)
FXmin = FXC - WID: FXmax = FXC + WID
FYmin = FXY - WID: FYmax = FYC + WID
FOR I = 1 TO KN
    XF = APPLX(I): Yf = APPLY(I)
    XI = XF - 2 * RBAR1 * FORX(I) / FAV
    YI = Yf - 2 * RBAR1 * FORY(I) / FAV
    CALL ARROW(XI, YI, XF, Yf, 15 - I)
NEXT
LOCATE WDMODE, 1: PRINT "    ";
'
' Force polygon
'
XYlimQ(8) = XSPMODE: XYlimQ(9) = YSPMODE: XYlimQ(10) = SPMODE
XYscrQ(1) = XSwid
XYscrQ(2) = XSwid
XYscrQ(3) = XSmin: XYscr(5) = 10
XYscrQ(4) = XYscrP(4) + 13
LablQ$(1) = "FX": LablQ$(2) = "FY"
XYlimQ(1) = FXmin: XYlimQ(2) = FXmax: XYlimQ(3) = FYmin: XYlimQ(4) = FYmax
ContrQ(7) = 1: '  ContrQ(1) = -1
CALL WIN(XYlimQ(), XYscrQ(), XYpltQ(), LablQ$(), ContrQ())
FOR I = 1 TO KN
    N1 = I: N2 = I + 1
    IF N2 > KN1 THEN N2 = 1
    XI = XY(2 * N1 - 1): YI = XY(2 * N1)
    XF = XY(2 * N2 - 1): Yf = XY(2 * N2)
    CALL ARROW(XI, YI, XF, Yf, 15 - I)
NEXT
IF KN = 0 THEN
    SMX = 0: SMY = 0: FAV = 1.0
END IF
CMIN = INT(XSmin / 8) + 1
LOCATE WDMODE, CMIN + 2: PRINT USING "Equilibrium ##.#####   "; SQR(SMX ^ 2 + SMY ^ 2) / FAV;
IF VAL(LablF$(30)) = 0 THEN
    GOTO Infonothing
END IF
XYlimR(8) = XSPMODE: XYlimR(9) = YSPMODE: XYlimR(10) = SPMODE
XYscrR(1) = XSwid
XYscrR(2) = XSwid
XYscrR(3) = XSmin:
XYscr(5) = 10
XYscrR(4) = XYscrQ(4) + 13
LablR$(1) = LablF$(1): LablR$(2) = LablF$(2): NS = 1: NA = 0
XYlimR(1) = XYlimF(1): XYlimR(2) = XYlimF(2): XYlimR(3) = XYlimF(3): XYlimR(4) = XYlimF(4)
'CALL WIN(XYlimR(), XYscrR(), XYpltR(), LablR$(), ContrR())
Par$(0) = ""
IF Xar(1, 0) <> 0 THEN
    CALL PLOT(Xar(), Yar(), Par$(), NA, NS, XYlimR(), XYscrR(), XYpltR(), LablR$(), ContrR())
END IF
Infonothing:
NRMINS = INT(XYscrL(4) / 16)
NCMINS = CINT(XYscrL(2) / 8) - LEN(Cfile$) + LNO - 1
LOCATE NRMINS, NCMINS: PRINT MID$(Cfile$, LNO)
IF AUT = 1 THEN
    WHILE INKEY$ = "": WEND
    LASTBALL = FOUND
    PCOPY 1, 0
    CALL CLEAN
    VIEW (Xoff, Yoff)-(Xlas, Ylas)
    WINDOW (XYlim(1), XYlim(3))-(XYlim(2), XYlim(4))
ELSE
    CALL CHECKAUTO
    IF AUT = 1 THEN
        LablF$(30) = "0"
        VIEW (XYscrL(2), 0)-(XSPMODE - 1, YSPMODE - 1): CLS
        VIEW
        LOCATE WDMODE, CMIN + 2: PRINT "Press any key ...";
        WHILE INKEY$ = "": WEND
        WINDOW: VIEW: CLS
        GTERM$ = ""
        CALL MPLOT(XYlim(), XYscr(), XYplt(), Labl$(), Contr())
        CALL PlotBalls(XYlim(), XYscr())
    END IF
END IF
END SUB

SUB PlotCluster (XYlim(), XYscr(), PLS(), BLS(), XG, YG)
DIM XY(2000)
Xmin = XYlim(1): Xmax = XYlim(2)
Ymin = XYlim(3): Ymax = XYlim(4)
NOBD = 0
IF PLS(0) < 0 THEN NOBD = 1
NCL = BLS(0)
'
FOR N = 1 TO NCL
    IB = BLS(N)
    CALL GETDISC(IB, Xc, Yc, Tc, ITYPE, IBTYP)
    IF IB > NDISK THEN
        IF NOBD = 0 THEN GOTO SCPP
        IADD = AA(30)
        IF IADD <> 0 THEN
            ISEQ = IB - NDISK
            IF ISEQ <= IADD THEN
                KD = NADD + (ISEQ - 1) * 7
                IBB = AA(KD + 5)
                CALL GETDISC(-IBB, Xc, Yc, Tc, ITYPE, IBTYP)
            END IF
        END IF
    END IF
    IF N = 1 THEN
        CLR0 = 15
        CLR1 = CLR0 + 1
    ELSE
        CLR0 = 10
        CLR1 = CLR0 + 2
    END IF
    Xc = Xc - XG: Yc = Yc - YG
    IF IBTYP = 0 THEN
        CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, CLR0)
    ELSE
        CALL PLOTELS(Xc, Yc, Tc, ITYPE, IBTYP, CLR1)
    END IF
    SCPP:
NEXT
NPL = PLS(0)
CLR0 = 7
FOR N = 1 TO NPL
    IP = PLS(N)
    CALL GETPOLY(IP, XY())
    CALL SHIFTPOLY(XY(), XG, YG)
    CALL PLOTpoly(XY(), CLR0)
NEXT
CALL BORDER(XYlim(), XYscr())
END SUB

SUB IFINLIST (IB, BLS(), FND)
NB = BLS(0)
FND = 0
FOR N = 1 TO NB
    IF BLS(N) = IB THEN
        FND = N
        EXIT FOR
    END IF
NEXT
END SUB

SUB DECODEG (GG$, VVL)
N1 = INSTR(1, GG$, "?")
N2 = INSTR(1, GG$, "!")
IF N1 = 0 AND N2 = 0 THEN
    VVL = 0
    EXIT SUB
END IF
G$ = MID$(GG$, 2)
N = INSTR(1, G$, "+")
IF N <> 0 THEN
    K1$ = (MID$(G$, N - 1, 1))
    K2$ = (MID$(G$, N + 1, 1))
    IF VAL(K1$) = 6 THEN
        VVL = VAL(K2$)
        IF VVL = 0 THEN
            VVL = ASC(K2$) - 55
        END IF
        EXIT SUB
    END IF
    IF VAL(K2$) = 6 THEN
        VVL = VAL(K1$)
        IF VVL = 0 THEN
            VVL = ASC(K2$) - 55
        END IF
        EXIT SUB
    END IF
END IF
END SUB

SUB SHIFTPOLY (XY(), XG, YG)
KS = XY(0)
FOR K = 1 TO KS
    XY(2 * K - 1) = XY(2 * K - 1) - XG
    XY(2 * K) = XY(2 * K) - YG
NEXT
END SUB
