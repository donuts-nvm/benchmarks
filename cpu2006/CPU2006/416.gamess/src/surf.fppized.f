C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 26 MAR 02 - MWS - ADD CCTYP,DFTYPE INPUTS, ALLOW NEVALS TO INCREMENT
C 25 OCT 01 - DGF - INTRODUCE GRIDS AS AN ALTERNATIVE TO EQUAL SPACING
C  6 SEP 01 - MWS - ADD DUMMY ARGUMENTS TO NAMEIO CALL
C  1 AUG 01 - DGF - SURFX: RETURN FAST MEMORY AT END
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C 13 JUN 01 - DGF - GENERALISE TO ARBITRARY RUNTYP
C 13 JUN 96 - VAG - ADD VARIABLE FOR CI TYPE TO SCFOPT COMMON
C 26 JUL 95 - MWS - USE CONTINUE AT BOTTOM OF DO LOOPS
C  1 FEB 95 - RPM - NEW MODULE FOR PES SCANNING
C
C*MODULE SURF   *DECK SURFX
      SUBROUTINE SURFX
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     RICHARD P. MULLER, 1995
C     THIS ROUTINE WILL COMPUTE A POTENTIAL SURFACE OVER 1 OR 2 COORDS.
C
      PARAMETER (MXATM=500)
C
      CHARACTER*4 ATMLAB(106)
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /CSURF / DISP(2),ORIG(2),NDISP(2),IVEC1(2),IVEC2(2),
     *                NG1,NG2,SCFTYPE(2),RUNTYPE(2),CITYPE(2),
     *                CCTYPE(2),MPLEVEL(2),DFTTYPE(2),
     *                SFGRID1(100),SFGRID2(100),NUMDIM
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      NATM = NAT
      CALL VALFM(LOADFM)
      LC0   = LOADFM+1
      LIGRP1 = LC0   + NATM*3
      LIGRP2 = LIGRP1 + NATM
      LAST  = LIGRP2 + NATM
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
C     GET INPUT DATA
C
      CALL SRFINP(X(LIGRP1),X(LIGRP2),NATM)
C
C     DO EITHER ONE- OR TWO-DIMENSIONAL MAPPING
C
      IF (NAT.EQ.2 .OR. NUMDIM.EQ.1) THEN
         CALL SRFONE(X(LC0),X(LIGRP1),          NATM,ATMLAB)
      ELSE
         CALL SRFTWO(X(LC0),X(LIGRP1),X(LIGRP2),NATM,ATMLAB)
      END IF
C
      CALL RETFM(NEED)
      IF(MASWRK) WRITE(IW,*) '... DONE WITH POTENTIAL SURFACE SCAN ...'
      CALL TIMIT(1)
      RETURN
      END
C*MODULE SURF   *DECK SRFONE
      SUBROUTINE SRFONE(C0,IGRP1,NATM,ATMLAB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      CHARACTER*4 ATMLAB(*)
C
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      DIMENSION C0(3,NATM),IGRP1(NATM)
      DIMENSION VEC1(3)
C
      COMMON /CSURF / DISP(2),ORIG(2),NDISP(2),IVEC1(2),IVEC2(2),
     *                NG1,NG2,SCFTYPE(2),RUNTYPE(2),CITYPE(2),
     *                CCTYPE(2),MPLEVEL(2),DFTTYPE(2),
     *                SFGRID1(100),SFGRID2(100),NUMDIM
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      DATA RNONE_STR/"NONE    "/
C
C     THIS ROUTINE COMPUTES A POTENTIAL SURFACE OVER ONE COORDINATE.
C     RICHARD P. MULLER, 1995
C
      CALL SETLAB(3,ATMLAB)
C
C     SAVE THE INITIAL COORDINATES:
C
      DO 110 IAT = 1, NAT
         DO 100 I = 1,3
            C0(I,IAT) = C(I,IAT)
  100    CONTINUE
  110 CONTINUE
C
C     VEC1 IS THE UNIT VECTOR IN THE DISPLACEMENT DIRECTIONS
C     DEFINE THE DISPLACEMENT VECTORS. THE ORDERING REQUIRES THAT THESE
C     VECTORS BE SPECIFIED FROM CENTER ATOM TO OUTER ATOM.
C
      R1 = 0.0D+00
      DO 200 I = 1, 3
         VEC1(I) = C(I,IVEC1(2)) - C(I,IVEC1(1))
         R1 = R1 + VEC1(I)*VEC1(I)
  200 CONTINUE
      R1 = SQRT(R1)
      DO 210 I = 1, 3
         VEC1(I) = VEC1(I)/R1
  210 CONTINUE
C
C     MINIMIZE OUTPUT
C
C     NPRINT=-5
C     NPUNCH=0
C
C     LOOP OVER GEOMETRIES
C
      NPTS = NDISP(1)
      DO 390 I1 = 1,NPTS
         I1M = I1-1
         X1M = I1M
         D1 = X1M*DISP(1) + ORIG(1)
         IF(ABS(DISP(1)).LT.0.001D+00) D1=SFGRID1(I1)
C
C     DISPLACE THE ATOMS IN GROUP 1
C
         DO 310 IAT = 1, NG1
            DO 300 I = 1, 3
               C(I,IGRP1(IAT)) = C0(I,IGRP1(IAT)) + D1*VEC1(I)
  300       CONTINUE
  310    CONTINUE
         CALL COORDOUT
C
         SCFTYPS=SCFTYP
         CITYPS =CITYP
         CCTYPS =CCTYP
         MPLEVLS=MPLEVL
         DFTYPES=DFTYPE
C
         RUNTYP=RUNTYPE(1)
         SCFTYP=SCFTYPE(1)
         CITYP = CITYPE(1)
         CCTYP = CCTYPE(1)
         MPLEVL=MPLEVEL(1)
         DFTYP =DFTTYPE(1)
         IF(CCTYP.NE.RNONE) CALL CCINP
         IF(DFTYP.NE.RNONE) CALL DFTINP(DFTYP)
C
         CALL BRNCHX(RUNTYPE(1))
C
C           POSSIBLE SECOND CALCULATION IS AT THIS SAME GEOMETRY,
C           AND SHOULD BE ABLE TO REUSE INTEGRALS.  IT WILL NOT
C           BE COUNTED AS A SECOND ENERGY EVALUATION, HOWEVER.
C
         IF(RUNTYPE(2).NE.RNONE) THEN
            IF(MASWRK) WRITE(IW,9020)
            IREST=2
            NEVALS=NEVALS-1
C
            RUNTYP=RUNTYPE(2)
            SCFTYP=SCFTYPE(2)
            CITYP = CITYPE(2)
            CCTYP = CCTYPE(2)
            MPLEVL=MPLEVEL(2)
            DFTYP =DFTTYPE(2)
            IF(CCTYP.NE.RNONE) CALL CCINP
            IF(DFTYP.NE.RNONE) CALL DFTINP(DFTYP)
C
            CALL BRNCHX(RUNTYPE(2))
         END IF
C
         IF(IREST.NE.0) IREST=0
C
         SCFTYP=SCFTYPS
         CITYP =CITYPS
         CCTYP =CCTYPS
         MPLEVL=MPLEVLS
         DFTYP =DFTYPES
C
         CALL SRFOUT(D1,0.0D+00,ATMLAB)
  390 CONTINUE
      RETURN
 9020 FORMAT(//1X,'START OF SECOND JOB TYPE AT THIS GEOMETRY...'/)
      END
C*MODULE SURF   *DECK SRFTWO
      SUBROUTINE SRFTWO(C0,IGRP1,IGRP2,NATM,ATMLAB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      CHARACTER*4 ATMLAB(*)
C
      DIMENSION C0(3,NATM),IGRP1(NATM),IGRP2(NATM)
      DIMENSION VEC1(3), VEC2(3)
C
      COMMON /CSURF / DISP(2),ORIG(2),NDISP(2),IVEC1(2),IVEC2(2),
     *                NG1,NG2,SCFTYPE(2),RUNTYPE(2),CITYPE(2),
     *                CCTYPE(2),MPLEVEL(2),DFTTYPE(2),
     *                SFGRID1(100),SFGRID2(100),NUMDIM
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      DATA RNONE_STR/"NONE    "/
C
C     THIS ROUTINE COMPUTES A POTENTIAL SURFACE OVER TWO COORDINATES.
C     RICHARD P. MULLER, 1995
C
      CALL SETLAB(3,ATMLAB)
C
C     SAVE THE INITIAL COORDINATES:
C
      DO 110 IAT = 1, NAT
         DO 100 I = 1,3
            C0(I,IAT) = C(I,IAT)
  100    CONTINUE
  110 CONTINUE
C
C     DEFINE THE DISPLACEMENT VECTORS. THE ORDERING REQUIRES THAT THESE
C     VECTORS BE SPECIFIED FROM CENTER ATOM TO OUTER ATOM.
C     VEC1 AND VEC2 ARE UNIT VECTORS IN THE DISPLACEMENT DIRECTIONS
C
      R1 = 0.0D+00
      R2 = 0.0D+00
      DO 200 I = 1, 3
         VEC1(I) = C(I,IVEC1(2)) - C(I,IVEC1(1))
         VEC2(I) = C(I,IVEC2(2)) - C(I,IVEC2(1))
         R1 = R1 + VEC1(I)*VEC1(I)
         R2 = R2 + VEC2(I)*VEC2(I)
  200 CONTINUE
      R1 = SQRT(R1)
      R2 = SQRT(R2)
      DO 210 I = 1, 3
         VEC1(I) = VEC1(I)/R1
         VEC2(I) = VEC2(I)/R2
  210 CONTINUE
C
C     MINIMIZE OUTPUT
C
C     NPRINT=-5
C     NPUNCH=0
C
C     LOOP OVER GEOMETRIES
C
      NPT1 = NDISP(1)
      NPT2 = NDISP(2)
      DO 390 I1 = 1,NPT1
         I1M = I1-1
         X1M = I1M
         D1 = X1M*DISP(1) + ORIG(1)
         IF(ABS(DISP(1)).LT.0.001D+00) D1=SFGRID1(I1)
C
C     DISPLACE THE ATOMS IN GROUP 1
C
         DO 310 IAT = 1, NG1
            DO 300 I = 1, 3
               C(I,IGRP1(IAT)) =
     $              C0(I,IGRP1(IAT)) + D1*VEC1(I)
  300       CONTINUE
  310    CONTINUE
C
         DO 340 I2 = 1,NPT2
            I2M = I2-1
            X2M = I2M
            D2 = X2M*DISP(2) + ORIG(2)
            IF(ABS(DISP(2)).LT.0.001D+00) D2=SFGRID2(I2)
C
C     DISPLACE THE ATOMS IN GROUP 2
C
            DO 330 IAT = 1, NG2
               DO 320 I = 1, 3
                  C(I,IGRP2(IAT)) =
     $                 C0(I,IGRP2(IAT)) + D2*VEC2(I)
  320          CONTINUE
  330       CONTINUE
            CALL COORDOUT
C
C           CALL ENERGX
C
C     IT SEEMS BEST TO FOOL THE REST OF GAMESS INTO THINKING OF HAVING
C     GENUINE RUNTYP=RUNTYP(IN $SURF) RUN, RATHER THAN RUNTYP=SURFACE
C
         SCFTYPS=SCFTYP
         CITYPS =CITYP
         CCTYPS =CCTYP
         MPLEVLS=MPLEVL
         DFTYPES=DFTYPE
C
         RUNTYP=RUNTYPE(1)
         SCFTYP=SCFTYPE(1)
         CITYP = CITYPE(1)
         CCTYP = CCTYPE(1)
         MPLEVL=MPLEVEL(1)
         DFTYP =DFTTYPE(1)
         IF(CCTYP.NE.RNONE) CALL CCINP
         IF(DFTYP.NE.RNONE) CALL DFTINP(DFTYP)
C
         CALL BRNCHX(RUNTYPE(1))
C
C           POSSIBLE SECOND CALCULATION IS AT THIS SAME GEOMETRY,
C           AND SHOULD BE ABLE TO REUSE INTEGRALS.  IT WILL NOT
C           BE COUNTED AS A SECOND ENERGY EVALUATION, HOWEVER.
C
         IF(RUNTYPE(2).NE.RNONE) THEN
            IF(MASWRK) WRITE(IW,9020)
            IREST=2
            NEVALS=NEVALS-1
C
            RUNTYP=RUNTYPE(2)
            SCFTYP=SCFTYPE(2)
            CITYP = CITYPE(2)
            CCTYP = CCTYPE(2)
            MPLEVL=MPLEVEL(2)
            DFTYP =DFTTYPE(2)
            IF(CCTYP.NE.RNONE) CALL CCINP
            IF(DFTYP.NE.RNONE) CALL DFTINP(DFTYP)
C
            CALL BRNCHX(RUNTYPE(2))
         END IF
C
         IF(IREST.NE.0) IREST=0
C
         SCFTYP=SCFTYPS
         CITYP =CITYPS
         CCTYP =CCTYPS
         MPLEVL=MPLEVLS
         DFTYP =DFTYPES
C
         CALL SRFOUT(D1,D2,ATMLAB)
C
  340    CONTINUE
  390 CONTINUE
      RETURN
 9020 FORMAT(//1X,'START OF SECOND JOB TYPE AT THIS GEOMETRY...'/)
      END
C*MODULE SURF   *DECK SRFINP
      SUBROUTINE SRFINP(IGRP1,IGRP2,NATM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MAXGRD=100)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION IGRP1(NATM),IGRP2(NATM)
C
      COMMON /CSURF / DISP(2),ORIG(2),NDISP(2),IVEC1(2),IVEC2(2),
     *                NG1,NG2,SCFTYPE(2),RUNTYPE(2),CITYPE(2),
     *                CCTYPE(2),MPLEVEL(2),DFTTYPE(2),
     *                SFGRID1(100),SFGRID2(100),NUMDIM
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (TOANGS=0.52917724924D+00)
C
      PARAMETER (NNAM=24)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
      CHARACTER*8 :: SURF_STR
      EQUIVALENCE (SURF, SURF_STR)
      DATA SURF_STR/"SURF    "/
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"ORIG1   ","ORIG2   ","DISP1   ","DISP2   ",
     *           "NDISP1  ","NDISP2  ","IVEC1   ","IVEC2   ",
     *           "IGRP1   ","IGRP2   ","RUNTP1  ","RUNTP2  ",
     *           "SCFTP1  ","SCFTP2  ","CITYP1  ","CITYP2  ",
     *           "MPLEV1  ","MPLEV2  ","CCTYP1  ","CCTYP2  ",
     *           "DFTYP1  ","DFTYP2  ","GRID1   ","GRID2   "/
      DATA KQNAM /3,3,3,3,  1,1,21,21,  -1,-1,5,5,  5,5,5,5,
     *            1,1,5,5,  5,5,-3,-3/
C
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      CHARACTER*8 :: ENERGY_STR
      EQUIVALENCE (ENERGY, ENERGY_STR)
      DATA RNONE_STR/"NONE    "/,ENERGY_STR/"ENERGY  "/
C
      KQNAM( 9) = 10*NATM+1
      KQNAM(10) = 10*NATM+1
      KQNAM(23) = 10*MAXGRD+3
      KQNAM(24) = 10*MAXGRD+3
C
C     INITIALIZE STUFF
C
      ORIG(1) = 0.0D+00
      ORIG(2) = 0.0D+00
      DISP(1) = 0.0D+00
      DISP(2) = 0.0D+00
      NDISP(1) = 0
      NDISP(2) = 0
      IVEC1(1) = 0
      IVEC1(2) = 0
      IVEC2(1) = 0
      IVEC2(2) = 0
      RUNTYPE(1)=ENERGY
      RUNTYPE(2)=RNONE
      SCFTYPE(1)=SCFTYP
      SCFTYPE(2)=SCFTYP
      CITYPE(1) =CITYP
      CITYPE(2) =CITYP
      CCTYPE(1) =CCTYP
      CCTYPE(2) =CCTYP
      MPLEVEL(1)=MPLEVL
      MPLEVEL(2)=MPLEVL
      DFTTYPE(1)=DFTYPE
      DFTTYPE(2)=DFTYPE
C
      DO 100 I = 1, NATM
         IGRP1(I) = 0
         IGRP2(I) = 0
  100 CONTINUE
      CALL VCLR(SFGRID1,1,MAXGRD)
      CALL VCLR(SFGRID2,1,MAXGRD)
C
      JRET=0
      CALL NAMEIO(IR, JRET, SURF, NNAM, QNAM, KQNAM,
     *            ORIG(1),ORIG(2),DISP(1),DISP(2),NDISP(1),NDISP(2),
     *            IVEC1,IVEC2,IGRP1,IGRP2,RUNTYPE(1),RUNTYPE(2),
     *            SCFTYPE(1),SCFTYPE(2),CITYPE(1),CITYPE(2),MPLEVEL(1),
     *            MPLEVEL(2),CCTYPE(1),CCTYPE(2),DFTTYPE(1),DFTTYPE(2),
     *            SFGRID1,SFGRID2,
     *            0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,
     *            0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0)
C
C     DO SOME ERROR CHECKING OF THE INPUT JUST READ...
C
C     IF (ABS(DISP(1)).LE.0.001D+00) THEN
C        IF(MASWRK) WRITE(IW,*) 'STEP SIZE TOO SMALL OR NONEXISTANT'
C        CALL ABRT
C        STOP
C     END IF
C
      IF (JRET.NE.0) THEN
         IF(MASWRK) WRITE(IW,*) '$SURF INPUT ERROR'
         CALL ABRT
      END IF
      IF (NDISP(1) .EQ. 0) THEN
         IF(MASWRK) WRITE(IW,*) 'NO STEPS REQUESTED! '
         CALL ABRT
         STOP
      END IF
C
C     COUNT THE ATOMS IN GROUP1 AND GROUP2
C
      NG1 = 0
      DO 200 I = 1, NATM
         IF (IGRP1(I) .GT. 0) NG1 = NG1+1
  200 CONTINUE
      NG2 = 0
      DO 210 I = 1, NATM
         IF (IGRP2(I) .GT. 0) NG2 = NG2+1
  210 CONTINUE
C
      IF (NG1 .EQ. 0) THEN
         IF(MASWRK) WRITE(IW,*) 'NO ATOMS TO MOVE IN SURFACE MAPPING'
         CALL ABRT
         STOP
      END IF
C
      IF (IVEC1(1) .LE. 0 .OR. IVEC1(2) .LE. 0
     $     .OR. IVEC1(1) .EQ. IVEC1(2) ) THEN
         IF(MASWRK) WRITE(IW,*) 'VECTOR 1 INCORRECTLY DEFINED'
         CALL ABRT
         STOP
      END IF
C
      IF (IVEC2(1).EQ.0 .AND. IVEC2(2).EQ.0) THEN
         NUMDIM = 1
      ELSE
         NUMDIM = 2
      END IF
C
      IF(MASWRK) THEN
         WRITE(IW,9000)
         WRITE(IW,9005) 1,RUNTYPE(1),SCFTYPE(1),CITYPE(1),
     *                    MPLEVEL(1),CCTYPE(1),DFTTYPE(1)
         IF(RUNTYPE(2).NE.RNONE) 
     *   WRITE(IW,9005) 2,RUNTYPE(2),SCFTYPE(2),CITYPE(2),
     *                    MPLEVEL(2),CCTYPE(2),DFTTYPE(2)
         WRITE(IW,9010) 1,IVEC1(1),IVEC1(2),ORIG(1),DISP(1),NDISP(1)
         WRITE(IW,9020) 1,NG1,(IGRP1(I),I=1,NG1)
         IF(NUMDIM.EQ.2) THEN
         WRITE(IW,9010) 2,IVEC2(1),IVEC2(2),ORIG(2),DISP(2),NDISP(2)
         WRITE(IW,9020) 2,NG2,(IGRP2(I),I=1,NG2)
         END IF
      END IF
C
      NCORR1=0
      IF(CITYPE(1) .NE.RNONE) NCORR1=NCORR1+1
      IF(MPLEVEL(1).NE.0)     NCORR1=NCORR1+1
      IF(CCTYPE(1) .NE.RNONE) NCORR1=NCORR1+1
      IF(DFTTYPE(1).NE.RNONE) NCORR1=NCORR1+1
C
      NCORR2=0
      IF(CITYPE(2) .NE.RNONE) NCORR2=NCORR2+1
      IF(MPLEVEL(2).NE.0)     NCORR2=NCORR2+1
      IF(CCTYPE(2) .NE.RNONE) NCORR2=NCORR2+1
      IF(DFTTYPE(2).NE.RNONE) NCORR2=NCORR2+1
C
      IF(NCORR1.GT.1  .OR.  NCORR2.GT.1) THEN
         IF(MASWRK) WRITE(IW,*)
     *      'USE ONLY ONE CORRELATION METHOD IN EACH CALCULATION'
         CALL ABRT
      END IF
C
C     CONVERT TO BOHR
C
      ORIG(1) = ORIG(1)/TOANGS
      ORIG(2) = ORIG(2)/TOANGS
      DISP(1) = DISP(1)/TOANGS
      DISP(2) = DISP(2)/TOANGS
C
      DO I=1,NDISP(1)
         SFGRID1(I)=SFGRID1(I)/TOANGS
      ENDDO
      DO I=1,NDISP(2)
         SFGRID2(I)=SFGRID2(I)/TOANGS
      ENDDO
      RETURN
C
 9000 FORMAT(/5X,27("-")/5X,'POTENTIAL SURFACE MAP INPUT'/5X,27("-"))
 9005 FORMAT(1X,'JOB',I2,' IS RUNTYP=',A8,' SCFTYP=',A8,'  CITYP=',A8/
     *       10X,'MPLEVL=',I8,'  CCTYP=',A8,' DFTTYP=',A8)
 9010 FORMAT(1X,'COORD',I2,' LYING ALONG ATOM PAIR',2I5/
     *       1X,'HAS ORIGIN=',F6.3,', DISPLACEMENT=',F6.3,
     *       1X,' AND',I3,' STEPS.')
 9020 FORMAT(1X,'GROUP',I2,' CONTAINS',I5,' ATOMS:'/(1X,10I5/))
      END
C*MODULE SURF   *DECK SRFOUT
      SUBROUTINE SRFOUT(D1,D2,ATMLAB)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      CHARACTER*4 ATMLAB(*)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFIRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      COMMON /FUNCT / E,EG(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ONE=1.0D+00, TOANGS=0.52917724924D+00)
C
C        OUTPUT FINAL RESULTS FOR THIS SURFACE POINT
C
      IF(.NOT.MASWRK) RETURN
C
      CALL DSCAL(3*NAT,TOANGS,C,1)
C
      WRITE(IW,9000) TOANGS*D1,TOANGS*D2,E
      DO 100 IAT = 1,NAT
         NUCZ = INT(ZAN(IAT)) + IZCORE(IAT)
         WRITE(IW,9010) ATMLAB(NUCZ),(C(IXYZ,IAT),IXYZ=1,3)
  100 CONTINUE
      WRITE(IW,9020)
C
      CALL DSCAL(3*NAT,ONE/TOANGS,C,1)
      RETURN
C
 9000 FORMAT(1X,'---- SURFACE MAPPING GEOMETRY ----'/
     *       1X,'COORD 1=',F6.3,' COORD 2=',F6.3/
     *       1X,'HAS ENERGY VALUE',F15.6)
 9010 FORMAT(1X,A2,2X,3F10.5)
 9020 FORMAT(1X,'----------------------------------')
      END
C*MODULE SURF   *DECK COORDOUT
      SUBROUTINE COORDOUT
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      PARAMETER (MXATM=500,MXAO=2047)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /RUNLAB/ TITLE(10),A(MXATM),B(MXATM),BFLAB(MXAO)
C
      IF(.NOT.MASWRK) RETURN
      WRITE (IW,9050)
      DO 1100 IAT = 1,NAT
         WRITE (IW,9060) A(IAT),B(IAT),ZAN(IAT),
     *                   C(1,IAT),C(2,IAT),C(3,IAT)
 1100 CONTINUE
C
C     ----- PRINT INTERNUCLEAR DISTANCE TABLE -----
C
      CALL INTR

 9050 FORMAT(/1X,'ATOM',6X,'ATOMIC',22X,'COORDINATES (BOHR)'/
     *         11X,'CHARGE',9X,'X',19X,'Y',19X,'Z')
 9060 FORMAT(1X,A8,A2,F5.1,F17.10,2F20.10)
      END