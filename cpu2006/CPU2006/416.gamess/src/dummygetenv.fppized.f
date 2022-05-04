C     A function to return the environment variable values that the program expects,
C     without calling the real GETENV() and without setting the environment vars
C     using the C function envvar_()

C These are the array indices of the 
C variables


      SUBROUTINE DUMMYSETENVVARS

      COMMON /ENVVARS/ ENVVARVALUE 
      CHARACTER*32 ENVVARVALUE
      DIMENSION ENVVARVALUE(35)

      CHARACTER*32 ENVVAR, VALUE
      INTEGER INDEX

C  Read the env parameters from the command prompt   

   20 READ (*, *, END=50) ENVVAR, VALUE
C         WRITE (*,*) ENVVAR, VALUE
         CALL GETENVINDEX(ENVVAR, INDEX)
         ENVVARVALUE(INDEX) = VALUE
         GO TO 20
   50 ENVVARVALUE(35) = ' '
      END
      

      SUBROUTINE DUMMYGETENV (ENVVAR, VALUE)

      COMMON /ENVVARS/ ENVVARVALUE 
      CHARACTER*32 ENVVARVALUE
      DIMENSION ENVVARVALUE(35)

      CHARACTER*(*) ENVVAR, VALUE
      INTEGER INDEX

C      WRITE (*,*) ENVVAR
      CALL GETENVINDEX(ENVVAR, INDEX)

      VALUE = ENVVARVALUE(INDEX)

      END


C     Subroutine to return the index of a ENVVAR in 
C     in the array ENVVARVALUE

      SUBROUTINE GETENVINDEX(ENVVAR, INDEX)

      CHARACTER*(*) ENVVAR
      INTEGER INDEX


      IF (ENVVAR.EQ.'EXTBAS') THEN
         INDEX = 1
      ELSE IF (ENVVAR.EQ.'IRCDATA') THEN
         INDEX = 2
      ELSE IF (ENVVAR.EQ.'INPUT') THEN
         INDEX = 3
      ELSE IF (ENVVAR.EQ.'OUTPUT') THEN
         INDEX = 4
      ELSE IF (ENVVAR.EQ.'PUNCH') THEN
         INDEX = 5
      ELSE IF (ENVVAR.EQ.'AOINTS') THEN
         INDEX = 6
      ELSE IF (ENVVAR.EQ.'MOINTS') THEN
         INDEX = 7
      ELSE IF (ENVVAR.EQ.'DICTNRY') THEN
         INDEX = 8
      ELSE IF (ENVVAR.EQ.'WORK15') THEN
         INDEX = 9
      ELSE IF (ENVVAR.EQ.'WORK16') THEN
         INDEX = 10
      ELSE IF (ENVVAR.EQ.'FOCKDER') THEN
         INDEX = 11
      ELSE IF (ENVVAR.EQ.'PCMDATA') THEN
         INDEX = 12
      ELSE IF (ENVVAR.EQ.'DASORT') THEN
         INDEX = 13
      ELSE IF (ENVVAR.EQ.'CCREST') THEN
         INDEX = 15
      ELSE IF (ENVVAR.EQ.'CCDIIS') THEN
         INDEX = 16
      ELSE IF (ENVVAR.EQ.'CCINTS') THEN
         INDEX = 17
      ELSE IF (ENVVAR.EQ.'CCT1AMP') THEN
         INDEX = 18
      ELSE IF (ENVVAR.EQ.'CCT2AMP') THEN
         INDEX = 19
      ELSE IF (ENVVAR.EQ.'CCT3AMP') THEN
         INDEX = 20
      ELSE IF (ENVVAR.EQ.'CCVM') THEN
         INDEX = 21
      ELSE IF (ENVVAR.EQ.'CCVE') THEN
         INDEX = 22
      ELSE IF (ENVVAR.EQ.'ORDINT') THEN
         INDEX = 23
      ELSE IF (ENVVAR.EQ.'EIGENVFILE') THEN
         INDEX = 14
      ELSE IF (ENVVAR.EQ.'DAFL30') THEN
         INDEX = 24
      ELSE IF (ENVVAR.EQ.'JKFILE') THEN
         INDEX = 25
      ELSE IF (ENVVAR.EQ.'CASINTS') THEN
         INDEX = 26
      ELSE IF (ENVVAR.EQ.'CIVECTR') THEN
         INDEX = 27
      ELSE IF (ENVVAR.EQ.'AABB41') THEN
         INDEX = 28
      ELSE IF (ENVVAR.EQ.'BBAA42') THEN
         INDEX = 29
      ELSE IF (ENVVAR.EQ.'BBBB43') THEN
         INDEX = 30
      ELSE
         INDEX = 35
      END IF

      END
