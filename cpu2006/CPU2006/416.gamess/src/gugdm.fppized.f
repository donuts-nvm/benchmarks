C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 22 MAY 01 - MWS - DELETE GUGNO TO USE ONLY GUGNOS, FIXING IBLOCK
C 19 SEP 01 - MWS - CONVERT MXAOCI PARAMETER TO MXAO
C  6 SEP 01 - MWS - ADD DUMMY ARGUMENTS TO NAMEIO CALL
C 28 JUL 00 - MWS - GUGADM,GUGNO: ALLOW STATE AVERAGING OF CI DENSITY
C 26 NOV 96 - VAG - INCREASE CI AO'S TO 768
C 17 AUG 96 - SPW - GUGNO: SAVE SCF DENSITY ON DAF 316 IF CI GRADIENT
C 29 SEP 96 - KRG - ADDED SOME ERROR MESSAGES BEFORE ABRT CALLS
C 13 JUN 96 - MWS - REMOVE SOME FTNCHEK WARNINGS
C 11 NOV 94 - MWS - REMOVE SOME FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  1 JUN 94 - MWS - GUGADM,GUGDM: ALLOW FOR SUCCESSFUL PARALLEL RUNS
C 16 JUL 93 - MWS - INCREASE MAXIMUM CI ROOTS TO 100
C  2 APR 92 - MWS,TLW - COMMON ENRGYS MADE PURE FLOATING POINT
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C 11 JAN 92 - TLW - DRTDM,GUGDM: MAKE READS PARALLEL
C 10 JAN 92 - TLW - CHANGE REWINDS TO CALL SEQREW
C  6 JAN 92 - TLW - MAKE WRITES PARALLEL;ADD COMMON PAR
C  2 JUN 90 - MWS - SAVE ENERGY OF ROOT -IROOT- IN /FUNCT/
C  7 JAN 90 - MWS - SUPPRESS PRINTOUT OF DRT FILE TITLE
C 26 SEP 89 - MWS - ADD NFT13,NFT14 TO /CIFILS/
C 25 SEP 89 - MWS - WRITE NATURAL ORBITALS AND OCCUPATIONS TO DAF 19,21
C 20 FEB 89 - STE - GUGADM: FIX MEM BUG, BETTER 9028 DIAGNOSTIC
C  7 OCT 88 - MWS - INCREASE AO NUMBER FROM 128 TO 256
C  1 JUL 88 - JAM - CHANGE PUSQL CALL IN LOOPY1
C  9 MAY 88 - MWS - USE FORMAT 9120 TO PUNCH VECWRD IN GUGNO
C 15 NOV 87 - STE - USE EXETYP
C  4 MAY 87 - STE - GUGADM,GUGNO: IA USED ONLY BY GLDIAG; NO INDEXING
C 31 OCT 86 - STE - INITDM: SAVE ICOUNT,IOFF IN /GDMSAV/;USE PARAMETER
C 30 JUL 86 - MWS - PAD COMMON ENRGYS
C 21 JUL 86 - MWS - SAVE REPLACES COMMON /INDMSV/
C  7 JUL 86 - JAB - SANITIZE FLOATING POINT CONSTANTS
C 29 OCT 85 - STE - FREE DYNAMIC MEMORY BASE SO LOADFM.NE.0 OK
C 16 OCT 85 - STE - USE GENERIC MAX,MIN,SQRT; /CIFILS/ IN GUGADM
C                   /INDMSV/ IN INITDM; DSWAP IN GUGNO
C  6 JUL 84 - STE - CHANGE /ENRGCI/ TO ENRGYS IN GUGADM,GUGNO
C                   REPLACE NOOUT1 WITH PREVNL, NOOUT2 WITH PREV
C 24 FEB 84 - STE - ALLOCATE ONLY MEMORY NEEDED IN GUGADM
C 11 JAN 84 - STE - SAVE VARIABLES IN INITDM
C 28 DEC 83 - STE - ADD IBLOCK VARIABLE, DELETE DMDIAG,RDCIVC
C  1 DEC 83 - STE - FIX IX INDEXING IN GUGADM FOR NWDVAR=1
C  3 NOV 83 - STE - FIX CALL TO PUSQL IN LOOPY1
C MAR 23 83 - MWS - MINOR FORMAT ERROR IN GUGADM KILLED
C MAR  4 83 - MWS - FIX ERROR IN PUNCHING NO-S, ADD IROOT INPUT
C JAN 11 82 - MWS - DELETE WRITING NO-S TO DAF, SO ORIGINAL MO-S
C                   ARE THERE FOR ALL OTHER STATES
C NOV 22 82 - MWS - KQNAM=101 TO READ IN $GUGDM PROPERLY
C NOV 15 82 - MWS - PUNCH NO-S, IMPROVE PRINTOUT
C OCT 21 82 - MWS - ADD DMY ARGUMENTS TO NAMEIO CALL
C 29 SEP 82 - MWS - CONVERT TO IBM
C
C*MODULE GUGDM   *DECK DRTDM
      SUBROUTINE DRTDM(INDX,NABCB,IARC,NLWKS,NUWKS,PUWK,IWGHT,
     +                 MAP,ISYM,LEVNR,LEVPT,NROWS,NROWS4,NWKS,NFT11)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER PUWK
C
      PARAMETER (MXAO=2047)
C
      DIMENSION NABCB(NROWS),IARC(NROWS4),NLWKS(NROWS),NUWKS(NROWS)
      DIMENSION PUWK(NROWS),IWGHT(NROWS4),INDX(NWKS)
      DIMENSION MAP(MXAO),ISYM(MXAO),LEVNR(MXAO),LEVPT(MXAO)
C
      READ (NFT11) MAP
      READ (NFT11) ISYM
      READ (NFT11) LEVNR
      READ (NFT11) LEVPT
      READ (NFT11)
      READ (NFT11) NABCB
      READ (NFT11)
      READ (NFT11) NLWKS
      READ (NFT11) NUWKS
      READ (NFT11) PUWK
      READ (NFT11) INDX
      READ (NFT11) IARC
      READ (NFT11) IWGHT
      RETURN
      END
C*MODULE GUGDM   *DECK GUGADM
      SUBROUTINE GUGADM(NPRINT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL SOME,ERROR,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXRT=100, MXAO=2047)
C
      DIMENSION IH(1)
      DIMENSION TITLE(10),TITLE1(10)
      DIMENSION MAP(MXAO),ISYM(MXAO),LEVNR(MXAO),LEVPT(MXAO)
      DIMENSION NFLGDM(MXRT),WSTATE(MXRT),ISTAVG(MXRT)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / H(1)
      COMMON /FUNCT / E,EG(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /LOOPS1/ ACOF,NWKS,NORBS,IB,JB,IUWK,JUWK,NUWK,NLWK,
     *                IHAI,LDGUGA
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      EQUIVALENCE (H(1),IH(1))
C
      PARAMETER (NNAM=5)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
      CHARACTER*8 :: GUGDM_STR
      EQUIVALENCE (GUGDM, GUGDM_STR)
      DATA GUGDM_STR/"GUGDM   "/
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"NFLGDM  ","WSTATE  ","NWORD   ","IROOT   ",
     * "IBLOCK  "/
      DATA KQNAM/-1,-3,1,1,1/
C
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA CHECK_STR/"CHECK   "/, DEBUG_STR/"DEBUG   "/, 
     * DBUGME_STR/"GUGADM  "/
C
      SOME = NPRINT .NE.-5 .AND. MASWRK
      IF (EXETYP .EQ. DEBUG  .OR.  EXETYP .EQ. DBUGME) THEN
         SOME = .TRUE. .AND. MASWRK
      END IF
      IF (SOME) WRITE (IW,9000)
C
C     ----- READ NAMELIST -$GUGDM- -----
C
      KQNAM(1) = 10*MXRT + 1
      KQNAM(2) = 10*MXRT + 3
      DO 10 I=1,MXRT
         NFLGDM(I) = 0
         WSTATE(I) = 0.0D+00
   10 CONTINUE
      NFLGDM(1) = 1
      NWORD = 0
      IROOT = 1
      IBLOCK = 0
C
      CALL NAMEIO(IR,JRET,GUGDM,NNAM,QNAM,KQNAM,
     *            NFLGDM,WSTATE,NWORD,IROOT,IBLOCK,
     *            0,0,0,0,  0,0,0,0,0,
     *     0,0,0,0,0,  0,0,0,0,0,
     *     0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *     0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
      IF (JRET .GT. 1) THEN
         IF (MASWRK) WRITE (IW,9010)
         CALL ABRT
         STOP
      END IF
C
      NSTATE = INT(STATN)
C
      SUM = 0.0D+00
      NSTAVG = 0
      DO I=1,NSTATE
         SUM = SUM + WSTATE(I)
         IF(WSTATE(I).GT.0.0D+00) THEN
            NSTAVG=NSTAVG+1
            ISTAVG(NSTAVG) = I
         END IF
      ENDDO
      IF(NSTAVG.GT.0) THEN
         IROOT=0
         DO I=1,NSTATE
            WSTATE(I) = WSTATE(I)/SUM
            NFLGDM(I) = 0
         ENDDO
      END IF
C
      IF(NSTAVG.EQ.0) THEN
         E = ESTATE(IROOT)
         IF(NFLGDM(IROOT).EQ.0) NFLGDM(IROOT)=1
         IF(SOME) THEN
            WRITE(IW,9020) IROOT,E
            WRITE(IW,9030) (NFLGDM(I),I=1,NSTATE)
         END IF
      ELSE
         E = 0.0D+00
         DO I=1,NSTAVG
            E = E + WSTATE(ISTAVG(I)) * ESTATE(I)
            NFLGDM(ISTAVG(I)) = -1
         ENDDO
         IF(SOME) THEN
            WRITE(IW,9040) NSTAVG,E
            WRITE(IW,9050) (ISTAVG(I),WSTATE(ISTAVG(I)),I=1,NSTAVG)
         END IF
      END IF
      IF(SOME) WRITE(IW,9060) NWORD,IBLOCK
C
C     ----- GET FAST MEMORY -----
C
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOT)
      IF (NWORD.GT.0) NGOT = MIN(NGOT, NWORD)
C
C     ----- READ HEADER RECORD OF CI VECTOR TAPE -----
C
      CALL SEQREW(NFT12)
      READ (NFT12) NSTATE,NWKS,TITLE,TITLE1
      IF (SOME) WRITE (IW,9070) NSTATE,NWKS
C
C.... READ VARIOUS PARAMETERS FROM DRT TAPE
C
      CALL SEQREW(NFT11)
      CALL RDDRT(NORBMX,NORBS,NSYM,NROWS,NWKS0,LEVFRM,NEMEMX,
     *           NREFS,IEXCT,NFOCI,INTACT,NCORBS)
      READ (NFT11) TITLE
      ERROR = .FALSE.
      DO 140 I = 1,10
         ERROR = ERROR .OR. (TITLE(I) .NE. TITLE1(I))
  140 CONTINUE
      IF(ERROR  .OR.  NWKS.NE.NWKS0) GO TO 240
C
      L0 = NQMT
      L1 = NUM
      L2 = (NUM*NUM+NUM)/2
      L3 = NUM*NUM
      NORBS2 = (NORBS*NORBS+NORBS)/2
      NOCC   = NORBS+NCORBS
      NOCC2  = (NOCC*NOCC+NOCC)/2
C
C     IX(IHA)     INDX
C      X(IHB)     CI VECTOR
C     IX(IH1)     SPIN OFFSET ARRAY
C     IX(IH2)     IARC
C     IX(IH3)     NLWKS
C     IX(IH4)     NUWKS
C     IX(IH5)     PUWKS
C     IX(IH6)     IWGHT
C
      IHA    = LOADFM + 1
      IHB    = IHA    + NWKS
      IH1    = IHB    + NWKS
      IH2    = IH1    + NROWS
      IH3    = IH2    + NROWS*4
      IH4    = IH3    + NROWS
      IH5    = IH4    + NROWS
      IH6    = IH5    + NROWS
      LDAO   = IH6    + NROWS*4
      LDMO   = LDAO   + L2
      LDAVG  = LDMO   + NOCC2
      LDGUGA = LDAVG  + NORBS2
      LSCR   = LDGUGA + NORBS2
      LS     = LSCR   + 8*L1
      LQ     = LS     + L2
      LIWRK  = LQ     + L3
      LVEC   = LIWRK  + L1
      LOCC   = LVEC   + L3
      LIA    = LOCC   + L1
      LWRK   = LIA    + L1
      LAST   = LWRK   + L3
C
      NEED = LAST-LOADFM-1
      IF(NEED.GT.NGOT) GO TO 260
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 230
C
C     ----- READ -DRT- DATA -----
C
      NROWS4 = NROWS*4
      CALL DRTDM(H(IHA),H(IH1),H(IH2),H(IH3),H(IH4),H(IH5),H(IH6),MAP,
     *           ISYM,LEVNR,LEVPT,NROWS,NROWS4,NWKS,NFT11)
      CALL SEQREW(NFT11)
C
      IHAI = IHA*NWDVAR - NWDVAR
      DO 160 I = 1,NWKS
  160 IH(IHAI+I) = IH(IHAI+I)+NWKS+LOADFM
C
      NCORBS = 0
      DO 200 I = 1,NORBMX
         IF (MAP(I).LT.0) NCORBS = NCORBS+1
  200 CONTINUE
C
C     ----- LOOP OVER FORMING DENSITY MATRIX OF EACH CI STATE -----
C     ONE MAY FORM STATE-SECIFIC NO-S, OR ELSE JUST STATE-AVERAGE
C     AND THEN COMPUTE THE STATE-AVERAGED NO-S BELOW THIS LOOP.
C
      CALL VCLR(H(LDAVG),1,NORBS2)
C
      DO 220 KSTAT = 1,NSTATE
         CALL SQREAD(NFT12,H(IHB),NWKS)
         IF(NFLGDM(KSTAT).EQ.0) GO TO 220
         IF(SOME) WRITE(IW,9080) KSTAT,ESTATE(KSTAT)
C
C     INITDM INITIALIZES THE DENSITY MATRIX, AT -LDGUGA-
C     NOTE THAT THIS ADDRESS IS PASSED IN COMMON, UGH.
C     LOOPY1 GENERATES THE LOOPS AND THEIR COEFFICIENTS,WHICH ARE
C     THEN PASSED TO MAKE THE DENSITY MATRIX.
C     CLOSDM FINISHES THE DENSITY MATRIX.
C
         CALL INITDM
         CALL LOOPY1(H(IH1),H(IH2),H(IH3),H(IH4),H(IH5),H(IH6),
     *               NROWS,ISYM,LEVPT,LEVNR,NROWS4)
         CALL CLOSDM
C
C     GUGNOS COMPUTES AND PRINTS NATURAL ORBITALS.
C
         IDMPRT=NFLGDM(KSTAT)
         WEIGHT=WSTATE(KSTAT)
         CALL GUGNOS(KSTAT,NSTAVG,WEIGHT,IDMPRT,IBLOCK,IROOT,MAP,
     *               H(LDGUGA),H(LDAVG),H(LDMO),H(LDAO),H(LVEC),H(LS),
     *               H(LOCC),H(LQ),H(LIA),H(LSCR),H(LIWRK),H(LWRK),
     *               NORBS,NORBS2,NOCC2,NCORBS,L0,L1,L2,L3,SOME)
  220 CONTINUE
C
C         FOR AVERAGED DENSITIES, THE LOOP ABOVE JUST PREPARED THE
C         WEIGHTED SUM OF 1-PDM.  NOW WE HAVE TO MAKE NATURAL ORBITALS,
C         SAVE THE DENSITY TO DAF, ETC.
C
      IF(NSTAVG.GT.0) THEN
         KSTAT =0
         WEIGHT=0.0D+00
         IDMPRT=1
         IF(SOME) WRITE(IW,9085) E
         CALL GUGNOS(KSTAT,NSTAVG,WEIGHT,IDMPRT,IBLOCK,IROOT,MAP,
     *               H(LDGUGA),H(LDAVG),H(LDMO),H(LDAO),H(LVEC),H(LS),
     *               H(LOCC),H(LQ),H(LIA),H(LSCR),H(LIWRK),H(LWRK),
     *               NORBS,NORBS2,NOCC2,NCORBS,L0,L1,L2,L3,SOME)
      END IF
C
C     ----- RESET FAST MEMORY -----
C
  230 CONTINUE
      CALL RETFM(NEED)
      IF (MASWRK) WRITE (IW,9090)
      CALL TIMIT(1)
      RETURN
C
  240 CONTINUE
      IF (MASWRK) WRITE (IW,9100) NWKS,NWKS0,TITLE
      CALL ABRT
      STOP
C
  260 CONTINUE
      MORE = LAST-NGOT
      IF (MASWRK) WRITE (IW,9110)
     *   MORE,NWKS,L1,L2,L3,NORBS,NROWS,LAST,NEED,NGOT
      CALL ABRT
      STOP
C
 9000 FORMAT(/10X,38("-")/
     *        10X,'CI DENSITY MATRIX AND NATURAL ORBITALS'/
     *        10X,38(1H-))
 9010 FORMAT(1X,'*** ERROR READING $GUGDM INPUT.')
 9020 FORMAT(1X,'PROPERTIES WILL BE COMPUTED FOR STATE -IROOT-',I4,
     *          ' WITH E=',F20.10)
 9030 FORMAT(1X,'NFLGDM=',10I4)
 9040 FORMAT(1X,'PROPERTIES WILL BE COMPUTED FOR STATE AVERAGED CI',
     *          ' DENSITY.'/
     *       1X,'AVERAGE ENERGY OVER',I4,' STATES IS',F20.10)
 9050 FORMAT(1X,'STATE,WSTATE:',5(I4,'=',F6.4,:,', '))
 9060 FORMAT(1X,'NWORD=',I10,'  IBLOCK=',I4)
 9070 FORMAT(1X,'NUMBER OF STATES         =',I7/
     +       1X,'NUMBER OF CONFIGURATIONS =',I7)
 9080 FORMAT(/10X,'CI EIGENSTATE',I5,' TOTAL ENERGY =',F20.10)
 9085 FORMAT(/10X,'FORMING NATURAL ORBITALS FOR STATE AVERAGED DENSITY'/
     *        10X,'THE AVERAGE OF THE STATE ENERGIES IS',F20.10)
 9090 FORMAT(" ...... END OF DENSITY MATRIX CALCULATION ...... ")
 9100 FORMAT(" ERROR IN CI VECTOR TAPE AND DRT TAPE.",/,
     *     22H NWKS, NWKS0, TITLE = ,2I7,3X,10A8)
 9110 FORMAT(1X,'*** NOT ENOUGH CORE. INCREASE BY',I10,' WORDS ***'/
     *      1X,'NWKS=',I10,' L1,L2,L3=',3I8,' NORBS,NROWS=',2I8/
     *      1X,'LAST,NEED,NGOT=',3I15)
      END
C*MODULE GUGDM   *DECK GUGNOS
      SUBROUTINE GUGNOS(KSTAT,NSTAVG,WEIGHT,IDMPRT,IBLOCK,IROOT,MAP,
     *                  DGUGA,DAVG,DMO,DAO,VEC,S,OCC,Q,IA,SCR,IWRK,WRK,
     *                  NORBS,NORBS2,NOCC2,NCORBS,L0,L1,L2,L3,SOME)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DGUGA(NORBS2),DAVG(NORBS2),DMO(NOCC2),DAO(L2),
     *          VEC(L1,L1),S(L2),OCC(L1),Q(L3),IA(L1),SCR(L1,8),
     *          IWRK(L1),WRK(L3),MAP(*)
      DIMENSION TIMSTR(3)
      LOGICAL SOME
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      CHARACTER*8 :: ENDWRD_STR
      EQUIVALENCE (ENDWRD, ENDWRD_STR)
      CHARACTER*8 :: OCCWRD_STR
      EQUIVALENCE (OCCWRD, OCCWRD_STR)
      CHARACTER*8 :: VECWRD_STR
      EQUIVALENCE (VECWRD, VECWRD_STR)
      DATA OCCWRD_STR,VECWRD_STR,ENDWRD_STR/" $OCC   "," $VEC   ",
     * " $END   "/
C
C     KSTAT.GT.0 MEANS WE ARE GIVEN THE DENSITY OF THAT SPECIFIC
C     STATE IN -DGUGA-.  WHEN KSTAT.GT.0 AND NSTAVG.GT.0, THEN WE
C     ARE DOING STATE AVERAGING, SO -DGUGA- IS TO BE SUMMED INTO
C     THE AVERAGE -DAVG- WITH THE GIVEN STATE -WEIGHT-.
C
C     KSTAT.EQ.0 MEANS THE AVERAGE -DAVG- HAS BEEN COMPLETED, AND
C     SHOULD BE USED TO PRINT NO'S.
C
C        IF WE ARE IN THE STATE AVERAGING LOOP, THAT'S ALL WE DO.
C        IF WE HAVE FINISHED AVERAGING, WE COPY THE AVERAGE DENSITY AND
C        PROCESS IT LIKE A SINGLE STATE DENSITY, EXCEPT FOR MESSAGES.
C
      IF(KSTAT.GT.0  .AND.  NSTAVG.GT.0) THEN
         IF(SOME  .AND.  IDMPRT.EQ.2) THEN
            WRITE(IW,9000) KSTAT
            CALL PRTRI(DGUGA,NORBS)
         END IF
         CALL DAXPY(NORBS2,WEIGHT,DGUGA,1,DAVG,1)
         RETURN
      END IF
      IF(KSTAT.EQ.0) CALL DCOPY(NORBS2,DAVG,1,DGUGA,1)
C
      DO I=1,L1
         IA(I) = (I*I-I)/2
      ENDDO
C
C        INSERT CORE ORBITAL DENSITY ELEMENTS
C
      CALL VCLR(DMO,1,NOCC2)
      DO I=1,NCORBS
         DMO(IA(I)+I) = 2.0D+00
      ENDDO
C
C        GUGA'S DENSITY IS OVER THE SORTED MO'S, SO WE MUST FIRST
C        PERMUTE DENSITY OF OCCUPIED ORBITALS TO ORIGINAL ORDER
C
      DO I=NCORBS+1,NCORBS+NORBS
         IMAP = MAP(I)
         DO J=NCORBS+1,I
            JMAP   = MAP(J)
            IJMO   = IA(I)+J
            IJGUGA = IA(MAX(IMAP,JMAP))+MIN(IMAP,JMAP)
            DMO(IJMO) = DGUGA(IJGUGA)
         ENDDO
      ENDDO
C
      IF(SOME  .AND.  IDMPRT.EQ.2) THEN
         IF(KSTAT.GT.0) THEN
            WRITE(IW,9010)
         ELSE
            WRITE(IW,9020)
         END IF
         CALL PRTRI(DMO,NCORBS+NORBS)
      END IF
C
C        BACK-TRANSFORM DENSITY TO THE AO BASIS
C
      CALL DAREAD(IDAF,IODA,VEC,L3,15,0)
      CALL TRPOSQ(VEC,L1)
      CALL TFTRI(DAO,DMO,VEC,SCR,L1,NCORBS+NORBS,L1)
C
C        SAVE THIS DENSITY FOR PROPERTIES?
C        CI GRADIENT RUNS REQUIRE HF DENSITY BE SAVED ON THE SIDE...
C
      IF(KSTAT.EQ.IROOT) THEN
         IF(SOME) WRITE(IW,9030) KSTAT
         CALL DERCHK(NDER)
         IF(NDER.NE.0) THEN
            CALL DAREAD(IDAF,IODA,S,L2, 16,0)
            CALL DAWRIT(IDAF,IODA,S,L2,316,0)
         END IF
         CALL DAWRIT(IDAF,IODA,DAO,L2,16,0)
      END IF
      IF(KSTAT.EQ.0) THEN
         IF(SOME) WRITE(IW,9040)
         CALL DAWRIT(IDAF,IODA,DAO,L2,16,0)
      END IF
C
C     ----- OPTION TO PREVENT INTERNAL/EXTERNAL ORBITAL MIXING -----
C     THIS IS DONE AFTER SAVING FULL DENSITY MATARIX IN THE AO BASIS,
C     AS WE WANT TO BE SURE TO COMPUTE THE CORRECT PROPERTIES, AND
C     SO WE REPEAT THE BACK-TRANSFORMATION ON THE BLOCKED DENSITY.
C
      IF(IBLOCK.GT.0) THEN
         IF(SOME) WRITE(IW,9050) IBLOCK
         DO I = IBLOCK+1,NORBS+NCORBS
            LOC = IA(I)
            DO J = 1,IBLOCK
               DMO(LOC+J) = 0.0D+00
            ENDDO
         ENDDO
         CALL TFTRI(DAO,DMO,VEC,SCR,L1,NCORBS+NORBS,L1)
      END IF
C
C        EXTRACT NATURAL ORBITALS, INSERT ORIGINAL CORE ORBITALS,
C        SAVE THE NATURAL ORBITALS WITH THEIR SYMMETRY LABELS.
C
      CALL UHFNOS(OCC,IWRK,SCR,DAO,S,VEC,WRK,Q,L0,L1,L2,0)
      IF(NCORBS.GT.0) THEN
         CALL DAREAD(IDAF,IODA,WRK,L3,15,0)
         CALL DCOPY(L1*NCORBS,WRK,1,VEC,1)
      END IF
C
      CALL DAREAD(IDAF,IODA,   S,L2, 12,0)
      CALL DAREAD(IDAF,IODA,   Q,L3, 45,0)
      CALL SYMMOS(IWRK,Q,S,VEC,SCR,L0,L1,NCORBS+NORBS,L1)
C
C        SAVE NO'S ON DAF (IN BETA ORBITAL RECORDS, ACTUALLY)
C
      IF(KSTAT.EQ.IROOT  .OR.  KSTAT.EQ.0) THEN
         CALL DAWRIT(IDAF,IODA, VEC,L3, 19,0)
         CALL DAWRIT(IDAF,IODA,IWRK,L1,256,1)
      END IF
C
C        PRINT AND PUNCH NO'S IN AO BASIS
C
      IF(SOME) THEN
         WRITE(IW,9060)
         CALL PREVS(VEC,OCC,IWRK,NCORBS+NORBS,L1,L1)
C
         CALL TMDATE(TIMSTR)
         IF(KSTAT.GT.0) WRITE(IP,9100) KSTAT,TIMSTR
         IF(KSTAT.EQ.0) WRITE(IP,9105) TIMSTR
         WRITE(IP,9120) OCCWRD
         WRITE(IP,9110) (OCC(I),I=1,NCORBS+NORBS)
         WRITE(IP,9120) ENDWRD
         WRITE(IP,9120) VECWRD
         CALL PUSQL(VEC,NCORBS+NORBS,L1,L1)
         WRITE(IP,9120) ENDWRD
      END IF
C
      RETURN
C
 9000 FORMAT(/1X,'STATE-SPECIFIC DENSITY MATRIX FOR ROOT',I4/
     *        1X,'NOTE THAT DENSITY OUTPUT DOES NOT CORRESPOND TO THE'/
     *        1X,'INITIAL ORBITAL ORDER.  HERE THE ORBITALS ARE SORTED'/
     *        1X,'BY IRREDUCIBLE REPRESENTATIONS, AND OCCUPANCY CLASS.')
 9010 FORMAT(/10X,'STATE-SPECIFIC 1E- DENSITY MATRIX FOR CI STATE',I4)
 9020 FORMAT(/10X,'STATE-AVERAGED 1E- DENSITY MATRIX')
 9030 FORMAT(10X,'DENSITY MATRIX OF STATE',I4,' SAVED FOR PROPERTY',
     *           ' ANALYSIS')
 9040 FORMAT(10X,'STATE-AVERAGED DENSITY MATRIX SAVED FOR PROPERTY',
     *           ' ANALYSIS')
 9050 FORMAT(10X,'OFF-DIAGONAL DENSITY MATRIX BLOCK ABOVE MO NO.',
     *            I5,' IS BEING ZERO''D.')
 9060 FORMAT(/10X,'NATURAL ORBITALS IN ATOMIC ORBITAL BASIS'/
     *        10X,40(1H-))
C
 9100 FORMAT('GUGA-CI NO-S FOR STATE',I4,' GENERATED ',3A8)
 9105 FORMAT('STATE AVERAGED GUGA-CI NO-S GENERATED ',3A8)
 9110 FORMAT(5F16.10)
 9120 FORMAT(A8)
      END
C*MODULE GUGDM   *DECK INITDM
      SUBROUTINE INITDM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (LENOFF= 2047, ZERO=0.0D+00)
C
      LOGICAL OUT
C
      DIMENSION INDX(1),VEC(1)
C
      COMMON /FMCOM / DM(1)
      COMMON /GDMSAV/ ICOUNT,IOFF(LENOFF)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /LOOPS1/ ACOF,NWKS,NORBS,IB,JB,IUWK,JUWK,NUWK,NLWK,
     *                IHAI,LDGUGA
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      EQUIVALENCE (DM(1),INDX(1)),(DM(1),VEC(1))
C          REMOVE THE ASSIGNMENT STATEMENT IF YOU WANT TO SEE OUTPUT.
      DATA OUT/.TRUE./
      SAVE OUT
      OUT=.FALSE.
C
C....  ZERO THE DENSITY MATRIX
C
      CALL VCLR(DM(LDGUGA),1,(NORBS*NORBS+NORBS)/2)
C
C....  CREATE OFFSET ARRAY TO ACCOUNT FOR POSITION OF DENSITY MATRIX
C
      II = -1
      DO 120 I = 1,LENOFF
         IOFF(I) = II + LDGUGA
         II = II + I
  120 CONTINUE
      ICOUNT = 0
      RETURN
C
C     ------------  ENTRY MAKEDM IN INITDM -----
      ENTRY MAKEDM
C     ------------
C
C....  THIS SECTION CREATES THE DENSITY MATRIX FROM DATA PASSED
C....  IN BY LOOPY1
C
      IF (IB .GT. LENOFF) THEN
         IF(MASWRK) WRITE(IW,*)
     *        'IB.GT.LENOFF IN -MAKEDM-, IB,LENOFF=',IB,LENOFF
         CALL ABRT
      END IF
      ICOUNT = ICOUNT+1
C
C....  BREAKDOWN THE LOOP INTO A SUM OF C(I) * C(J)
C
      IND = IUWK + IHAI
      JND = JUWK + IHAI
      DC = ZERO
      DO 160 I = 1,NLWK
      II = INDX(IND)
      JJ = INDX(JND)
      DO 140 J = 1,NUWK
      DC = DC + VEC(II) * VEC(JJ)
      II = II + 1
  140 JJ = JJ + 1
      IND = IND + 1
  160 JND = JND + 1
      LAD = IOFF(IB) + JB
C
C....  ADD THIS CONTRIBUTION TO THE DENSITY MATRIX
C
      DM(LAD) = DM(LAD) + ACOF * DC
      RETURN
C
C     ------------  ENTRY CLOSDM IN INITDM -----
      ENTRY CLOSDM
C     ------------
C
C....  FINISH UP BY PRINTING THE NUMBER OF LOOPS GENERATED
C
      IF(OUT  .AND.  MASWRK) WRITE (IW,9008) ICOUNT
      OUT=.FALSE.
      RETURN
C
 9008 FORMAT(1X,'NUMBER OF 1E-LOOPS       =',I7)
      END
C*MODULE GUGDM   *DECK LOOPY1
      SUBROUTINE LOOPY1(NABC,IARC,NLWKS,NUWKS,PUWK,IWGHT,NROWS,
     +                  ISYM,LEVPT,LEVNR,NROWS4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER PUWK
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXAO=2047)
C
C....  THIS ROUTINE IS A STRIPPED DOWN VERSION OF LOOPY THAT ONLY
C....  HANDLES THE ONE-ELECTRON LOOPS
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /LOOPS1/ ACOF,NWKS,NORBS,IB,JB,IUWK,JUWK,NUWK,NLWK,
     *                IHAI,LDGUGA
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DIMENSION ISEGM(MXAO),JSEGM(MXAO),IMAIN(MXAO),ISUB(MXAO)
      DIMENSION ACOEF(MXAO),IUWKMN(MXAO),IUWKSB(MXAO),ISHIFT(4)
      DIMENSION ISYM(MXAO),LEVPT(MXAO),LEVNR(MXAO)
      DIMENSION NABC(NROWS),IARC(NROWS4),NLWKS(NROWS)
      DIMENSION NUWKS(NROWS),PUWK(NROWS),IWGHT(NROWS4)
      DIMENSION JSEGNR(3),JSEGPT(3),IARCMN(21),IARCSB(21)
      DIMENSION NXTSEG(21),JXT(21),JMN(21),JSB(21)
      DIMENSION COEFFS(20,5),CFS(100)
C
      EQUIVALENCE (COEFFS(1,1),CFS(1))
      EQUIVALENCE (JXT(1),NXTSEG(1))
      EQUIVALENCE (JMN(1),IARCMN(1))
      EQUIVALENCE (JSB(1),IARCSB(1))
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
C
      DATA JSEGNR/7,14,21/
      DATA JXT/0,0,0,2,2,3,3,0,0,2,2,2,2,3,0,0,3,3,3,3,2/
      DATA JMN/2,3,4,3,4,2,4,1,2,1,2,3,4,2,1,3,1,2,3,4,3/
      DATA JSB/2,3,4,1,2,1,3,3,4,1,2,3,4,3,2,4,1,2,3,4,2/
C
 9008 FORMAT(" PROBLEMS WITH PARTIAL SPACE")
C
      JSEGPT(1) = 0
      JSEGPT(2) = JSEGNR(1)
      JSEGPT(3) = JSEGNR(2)
      DO 120 J = 1,5
         COEFFS(1,J) = ZERO
         COEFFS(2,J) = ZERO
  120 CONTINUE
C
C....  THESE ARE THE POSSIBLE SEGMENT COEFFICIENTS
C
      DO 140 I = 3,20
         A = I - 2
         COEFFS(I,1) = ONE/A
         COEFFS(I,2) = -ONE/A
         COEFFS(I,3) = SQRT((A+ONE)/A)
         COEFFS(I,4) = SQRT(A/(A+ONE))
         COEFFS(I,5) = SQRT(A*(A+TWO))/(A+ONE)
  140 CONTINUE
      DO 160 K = 1,4
         ISHIFT(K) = (K-1)*NROWS
  160 CONTINUE
      NLEVS = NORBS+1
C
C....  START THE MAIN LOOP
C
      DO 640 LEVI = 2,NLEVS
         LEV = LEVI
         LEVM = LEV-1
         JSM = ISYM(LEVM)
         IAD = LEVM
         NR = LEVNR(LEV)
         NPT = LEVPT(LEV)
         DO 620 IROW = 1,NR
         NPT = NPT+1
         ISEGM(LEV) = 1
         ISEG = 1
         IMN = NPT
         ISB = NPT
         KSEG = 0
         KSEGMX = JSEGNR(ISEG)
         IUWKMN(LEV) = PUWK(NPT)
         IUWKSB(LEV) = PUWK(NPT)
         IMAIN(LEV) = NPT
         ISUB(LEV) = NPT
         NUWK = NUWKS(NPT)
         ACOEF(LEV) = ONE
C
C....     SEARCH FOR NEXT LOOP SEGMENT
C
  180    KSEG = KSEG + 1
         IF (KSEG .GT. KSEGMX) GO TO 600
         KMN = IARCMN(KSEG)
         IARPT = IMN + ISHIFT(KMN)
         KMN = IARC(IARPT)
         IF (KMN .EQ. 0) GO TO 180
         KSB = IARCSB(KSEG)
         JARPT = ISB + ISHIFT(KSB)
         KSB = IARC(JARPT)
         IF (KSB .EQ. 0) GO TO 180
         JSEGM(LEV) = KSEG
         IUWKMN(LEVM) = IUWKMN(LEV) + IWGHT(IARPT)
         IUWKSB(LEVM) = IUWKSB(LEV) + IWGHT(JARPT)
C
C....     HAVING FOUND A VALID SEGMENT,
C         UPDATE THE VALUE OF THE COEFFICIENT
C
         GO TO (200,200,260,200,340,200,320,200,300,200,380,220,220,240,
     +        200,360,200,220,400,220,280),KSEG
C
  200    ACOEF(LEVM) = ACOEF(LEV)
         GO TO 420
C
  220    ACOEF(LEVM) = -ACOEF(LEV)
         GO TO 420
C
  240    IA = NABC(IMN)+2
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  260    ACOEF(LEVM) = ACOEF(LEV)+ACOEF(LEV)
         GO TO 420
C
  280    IA = NABC(IMN)+24
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  300    IA = NABC(IMN)+42
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  320    IA = NABC(IMN)+43
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  340    IA = NABC(IMN)+62
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  360    IA = NABC(IMN)+63
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  380    IA = NABC(IMN)+81
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  400    IA = NABC(IMN)+83
         ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
         GO TO 420
C
  420    CONTINUE
         IF (NXTSEG(KSEG) .GT. 0) GO TO 580
         IF (ISYM(LEVM) .NE. JSM) GO TO 180
         IF (KMN-KSB) 440,560,440
C
C....     UPDATE ALL VARIABLES FOR THE NEW SEGMENT
C
  440    LEVL = LEVM
         KSEGMX = 4
  460    LEV = LEVM
         LEVM = LEV-1
         IF (LEVM .GT. 0) GO TO 480
         IF (MASWRK) WRITE (IW,9008)
         CALL ABRT
C
  480    CONTINUE
         KSEG = 0
         IMN = KMN
         IMAIN(LEV) = KMN
         ISB = KSB
         ISUB(LEV) = KSB
  500    KSEG = KSEG + 1
         IF (KSEG .GT. KSEGMX) GO TO 540
         IARPT = IMN + ISHIFT(KSEG)
         KMN = IARC(IARPT)
         IF (KMN .LE. 0) GO TO 500
         JARPT = ISB + ISHIFT(KSEG)
         KSB = IARC(JARPT)
         IF (KSB .LE. 0) GO TO 500
         JSEGM(LEV) = KSEG
         IMAIN(LEVM) = KMN
         IUWKMN(LEVM) = IUWKMN(LEV)+IWGHT(IARPT)
         ISUB(LEVM) = KSB
         IUWKSB(LEVM) = IUWKSB(LEV)+IWGHT(JARPT)
         IF (KMN-KSB) 460,520,460
C
C....     A LOOP HAS BEEN CONSTRUCTED; FIND ITS CONTRIBUTION AND ADD
C....     TO THE DENSTIY MATRIX
C
  520    IB = IAD
         JB = LEVL
         NLWK = NLWKS(KMN)
         IUWK = IUWKMN(LEVM)
         JUWK = IUWKSB(LEVM)
         ACOF = ACOEF(LEVL)
         CALL MAKEDM
         GO TO 500
C
  540    IF (LEV .EQ. LEVL) GO TO 600
         LEVM = LEV
         LEV = LEVM+1
         IMN = IMAIN(LEV)
         ISB = ISUB(LEV)
         KSEG = JSEGM(LEV)
         GO TO 500
C
C....     A LOOP HAS BEEN CONSTRUCTED;
C         ADD ITS CONTRIBUTION TO THE DENSITY MATRIX
C
  560    IB = IAD
         JB = LEVM
         NLWK = NLWKS(KMN)
         IUWK = IUWKMN(LEVM)
         JUWK = IUWKSB(LEVM)
         ACOF = ACOEF(LEVM)
         CALL MAKEDM
         GO TO 180
C
  580    CONTINUE
         LEV = LEVM
         LEVM = LEV - 1
         ISEG = NXTSEG(KSEG)
         ISEGM(LEV) = ISEG
         KSEG = JSEGPT(ISEG)
         IMN = KMN
         IMAIN(LEV) = KMN
         ISB = KSB
         ISUB(LEV) = KSB
         KSEGMX = JSEGNR(ISEG)
         GO TO 180
C
  600    CONTINUE
         IF (LEV .EQ. LEVI) GO TO 620
         LEVM = LEV
         LEV = LEVM + 1
         ISEG = ISEGM(LEV)
         IMN = IMAIN(LEV)
         ISB = ISUB(LEV)
         KSEG = JSEGM(LEV)
         KSEGMX = JSEGNR(ISEG)
         GO TO 180
C
  620    CONTINUE
  640 CONTINUE
      RETURN
      END