C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: PREPOBS_PREVENTS
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2001-10-10
C
C ABSTRACT: PREPARES OBSERVATIONAL PREPBUFR FILE FOR SUBSEQUENT
C   QUALITY CONTROL AND ANALYSIS PROGRAMS.  THIS IS DONE THROUGH THE
C   FOLLOWING: INTERPOLATION OF GLOBAL SPECTRAL SIMGA FIRST GUESS TO
C   PREPBUFR OBSERVATION LOCATIONS WITH ENCODING OF FIRST GUESS VALUES
C   INTO PREPBUFR REPORTS; ENCODING OF "PREVENT" AND/OR "VIRTMP"
C   EVENTS INTO PREPBUFR REPORTS; AND ENCODING OF OBSERVATION ERRORS
C   FROM THE ERROR SPECIFICATION FILE INTO PREPBUFR REPORTS.  FOR
C   MORE INFORMATION ON THE DETAILS OF THE "PREVENT" AND "VIRTMP"
C   EVENTS, SEE THE DOCBLOCK FOR W3LIB ROUTINE "GBLEVENTS".  THIS
C   PROGRAM CALLS GBLEVENTS, WHICH RUNS HERE IN THE "PREVENTS" MODE.
C   W3LIB ROUTINE GBLEVENTS DOES THE BULK OF THE WORK HERE.  AFTER
C   EACH REPORT IS UPDATED BY GBLEVENTS, IT IS WRITTEN OUT TO A
C   "PREPROCESSED" VERSION OF THE PREPBUFR FILE.  
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN  ORIGINAL VERSION FOR REANALYSIS
C 1994-09-06  J. WOOLLEN  VERSION FOR IMPLEMENTATION IN GBL SYSTEM
C 1997-10-07  D.A. KEYSER -- ADDED NAMELIST SWITCH TO BYPASS VIRT.
C             TEMPERATURE EVENT FOR NON-RADIOSONDE/SATSND DATA TYPES
C             (INVOKED IN RUC VERSION - TOB NOT CHANGED FROM INPUT)
C 1997-11-24  D.A. KEYSER -- ADDED NAMELIST SWITCH "REDUCE" TO BYPASS
C             ALL PREVENTS PROCESSING (IF TRUE) FOR MESSAGE TYPES NOT
C             EQUAL TO "ADPUPA", "AIRCFT" AND "PROFLR"
C 1998-02-03  D.A. KEYSER -- CORRECTED ERROR FROM PREVIOUS CHANGE THAT
C             RESULTED IN BYPASSING THE VIRT. TEMPERATURE EVENT FOR
C             "ADPUPA" AND "SFCSHP" TYPES WHEN N-LIST SWITCH "REDUCE"
C             IS TRUE - REDUCE=TRUE WILL NOW CONTINUE TO DO ALL
C             PREVENTS PROCESSING FOR MESSAGE TYPES "ADPUPA", "AIRCFT",
C             "PROFLR" AS WELL AS NOW "ADPSFC" AND "SFCSHP"
C 1998-08-25  D.A. KEYSER -- ADDED SWITCHES 'DOBERR' AND 'DOFCST' IN
C             NAMELIST READ FROM DATA CARDS; SUBROUTINE NOW Y2K AND
C             FORTRAN 90 COMPLIANT
C 1998-09-14  J.WOOLLEN - ADDED SWITCH FOR INSTALLING ANALYSED VALUES
C 1998-09-17  D.A. KEYSER -- PROGRAM NOW CALLS EXIT PRIOR TO STOP FOR
C             NON-ZERO EXIT STATES (TRANSFERS EXIT STATE TO UNIX
C             FOREGROUND STATUS CODE)
C 1998-09-21  D. A. KEYSER -- SUBROUTINE NOW Y2K AND FORTRAN 90
C             COMPLIANT
C 1998-07-06  D. A. KEYSER -- MODIFIED TO COMPILE AND RUN ON IBM;
C             NOW CALLS NEW W3LIB ROUTINE "GBLEVENTS" TO PERFORM
C             MOST OF THE FUNCTIONS THAT THIS PROGRAM USED TO DO
C             (THIS W3LIB ROUTINE IS ALSO CALLED BY PREPDATA,
C             SYNDATA AND POSTEVENTS), ONLY THE READING IN OF
C             REPORTS IS DONE BY THIS MAIN PROGRAM NOW
C 1999-09-26  D. A. KEYSER -- CHANGES TO MAKE CODE MORE PORTABLE
C 2001-02-02  D. A. KEYSER -- MINOR HOUSKEEPING CHANGES; PICKS UP AN
C             UPDATED W3LIB ROUTINE GBLEVENTS
C 2001-10-10  D. A. KEYSER -- MODIFIED TO NOW PASS TWO SPANNING GLOBAL
C             SIGMA GUESS FILES INTO W3LIB ROUTINE GBLEVENTS IN
C             SITUATIONS WHERE THE CENTER DATE FOR THE PREPBUFR FILE
C             HAS AN HOUR THAT IS NOT A MULTIPLE OF 3 (SEE 2001-10-10
C             CHANGES TO GBLEVENTS)
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT (DATA CARDS - SEE NAMELIST
C                DOCUMENTATION IN W3LIB ROUTINE GBLEVENTS DOCBLOCK)
C     UNIT 11  - PREPBUFR FILE
C     UNIT 12  - FIRST INPUT SPECTRAL (GLOBAL) SIGMA FIRST GUESS FILE;
C              - IF HOUR IN CENTER DATE FOR PREPBUFR FILE IS A MULTIPLE
C              - OF 3 THEN THIS FILE IS VALID AT THE CENTER DATE OF THE
C              - PREPBUFR FILE, IF THE HOUR IN CENTER DATE FOR PREPBUFR
C              - FILE IS NOT A MULTIPLE OF 3 THEN THIS FILE IS VALID AT
C              - THE CLOSEST TIME PRIOR TO THE CENTER DATE OF THE
C              - PREPBUFR FILE THAT IS A MULTIPLE OF 3
C     UNIT 13  - SECOND INPUT SPECTRAL (GLOBAL) SIGMA FIRST GUESS FILE;
C              - IF HOUR IN CENTER DATE FOR PREPBUFR FILE IS A MULTIPLE
C              - OF 3 THEN THIS FILE IS EMPTY, IF THE HOUR IN CENTER
C              - DATE FOR PREPBUFR FILE IS NOT A MULTIPLE OF 3 THEN
C              - THIS FILE IS VALID AT THE CLOSEST TIME AFTER THE
C              - CENTER DATE OF THE PREPBUFR FILE THAT IS A MULTIPLE OF
C              - 3
C     UNIT 14  - OBSERVATION ERROR FILE
C     UNIT 15  - EXPECTED CENTER DATE IN PREPBUFR FILE IN FORM
C                YYYYMMDDHH
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - PREPBUFR FILE (NOW CONTAINING FIRST GUESS VALUES,
C              - "PREVENT" AND "VIRTMP" EVENTS, AND OBERVATIONAL ERROR
C              - VALUES)
C     UNIT 52  - "PREVENT" EVENTS DATA FILTERING SUMMARY PRINT FILE
C
C   SUBPROGRAMS CALLED:
C       W3LIB    - W3TAGB    W3TAGE    GBLEVENTS ERREXIT
C       BUFRLIB  - DATELEN   OPENBF    READMG    OPENMB
C                - WRITSB    CLOSBF
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C     COND =  21 - DATE DISAGREEMENT BETWEEN ACTUAL CENTER DATE IN
C                  PREPBUFR FILE AND EXPECTED CENTER DATE READ IN
C                  FROM UNIT 15
C     COND =  22 - BAD OR MISSING DATE READ IN FROM UNIT 15
C     COND =  60-79 - RESERVED FOR W3LIB ROUTINE GBLEVENTS (SEE
C                      GBLEVENTS DOCBLOCK)
C
C
C REMARKS: NONE.
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$

      PROGRAM PREPOBS_PREVENTS

      use gblevn_module, only : bmiss

      character(80) filo
      CHARACTER(8)  SUBSET,LAST

      integer IUNITG

      DATA  LAST/'XXXXXXXX'/

      include "mpif.h"

!--------------------------------------------------------------------------
      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,nprc,ierr)
!--------------------------------------------------------------------------
      bmiss=10e10; call setbmiss(bmiss)
!--------------------------------------------------------------------------

      if(myid==0)then
      CALL W3TAGB('PREPOBS_PREVENTS',2001,0283,0061,'NP22')
      PRINT 700
  700 FORMAT(/'  =====> WELCOME TO PREVENTS PROGRAM -- LAST UPDATED ',
     $ '2001-10-10'/)
      print*,'num threads=',ncpus()
      call prttime('start')
      endif

      IUNITI    = 11
      IUNITG    = 12
      IUNITG    = 13
      IUNITE    = 14
      IUNITD    = 15
      IUNITP    = 50             
      IUNITO    = 51             
      IUNITS    = 52

C  OPEN INPUT PREPBUFR FILE JUST TO GET MESSAGE DATE (WHICH IS THE
C   ACTUAL CENTER DATE), LATER CLOSE FILE
C  ---------------------------------------------------------------

      CALL DATELEN(10)

      CALL OPENBF(IUNITI,'IN',IUNITI)
      CALL READMG(IUNITI,SUBSET,IDATEP,IRET)

      if(myid==0)PRINT 53, IDATEP
   53 FORMAT(/' --> ACTUAL   CENTER DATE OF PREPBUFR FILE READ FROM ',
     $ ' SEC. 1 MESSAGE DATE IS:',I11/)

      IF(IDATEP.LT.1000000000) call bort('idatep not 10 digits')

C  READ IN EXPECTED CENTER DATE OF PREPBUFR FILE
C  ---------------------------------------------

      REWIND IUNITD
      READ(IUNITD,'(6X,I10)',END=904,ERR=904)  IDATED
      if(myid==0)PRINT 3, IUNITD, IDATED
    3 FORMAT(/' --> EXPECTED CENTER DATE OF PREPBUFR FILE READ FROM ',
     $ 'UNIT',I3,' IS:',13X,I11/)

C  CHECK ACTUAL CENTER DATE OF PREPBUFR FILE VS. EXPECTED CENTER DATE
C  ------------------------------------------------------------------

      IF(IDATEP.NE.IDATED)  GO TO 901

      CALL CLOSBF(IUNITI)

C  CALL W3LIB ROUTINE GBLEVENTS TO store first guess interpolation arrays
C  ----------------------------------------------------------------------

      SUBSET='NONE'
!     CALL GBLEVENTS(IDATED,IUNITG,IUNITE,IUNITP,IUNITS,SUBSET,newtyp)

C  OPEN INPUT AND OUTPUT PREPBUFR FILES FOR DATA PROCESSING
C  --------------------------------------------------------

      CALL OPENBF(IUNITI,'IN ',IUNITI)
      WRITE(FILO,'("mpi.fort.",I2.2)')MYID+1       
      OPEN(IUNITP,FILE=FILO,FORM='UNFORMATTED')
      CALL OPENBF(IUNITP,'OUT',IUNITI)
      CALL MAXOUT(20000)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      NEWTYP=0; nmsg=-1

C  LOOP THROUGH THE INPUT MESSAGES adding prevents and writing back out
C  --------------------------------------------------------------------

      DO WHILE(IREADMG(IUNITI,SUBSET,JDATEP).EQ.0)
      nmsg=nmsg+1;if(mod(nmsg,nprc)/=myid)cycle
      CALL OPENMB(IUNITP,SUBSET,JDATEP)
      IF(SUBSET.NE.LAST)THEN
         NEWTYP = 1
         IF(MYID==0) print *, 'New input message type read in: ',SUBSET
      END IF

      DO WHILE(IREADSB(IUNITI).EQ.0)
      CALL UFBCPY(IUNITI,IUNITP)
      CALL GBLEVENTS(IDATED,IUNITG,IUNITE,IUNITP,IUNITS,SUBSET,newtyp)
      CALL WRITSB(IUNITP)
      NEWTYP = 0
      ENDDO

      LAST = SUBSET
      ENDDO

C  CLOSE THE BUFR FILES
C  --------------------

      CALL CLOSBF(IUNITI)
      CALL CLOSBF(IUNITP)

C  ALL DONE - now sort it out
C  --------------------------

      call mpi_barrier(MPI_COMM_WORLD,ierr) ! need all processors here 

      IF(MYID==0) THEN
         call prttime('prevents')
         CALL SORTBUFR(IUNITO)
         call prttime('sortbufr')
         CALL W3TAGE('PREPOBS_PREVENTS')
         call prttime('end')
      ENDIF

      CALL MPI_FINALIZE(MRET)

      STOP

c  some error exits
c  ----------------

  901 CONTINUE
      PRINT 9901, IDATEP,IDATED
 9901 FORMAT(/' ##> ACTUAL CENTER DATE OF INPUT PREPBUFR FILE (',I10,
     $ ') DOES NOT MATCH EXPECTED CENTER DATE (',I10,') - STOP 21'/)
      CALL W3TAGE('PREPOBS_PREVENTS')
      CALL ERREXIT(21)

  904 CONTINUE
      PRINT 9902, IUNITD
 9902 FORMAT(/' ##> BAD OR MISSING EXPECTED PREPBUFR CENTER DATE ',
     $ 'READ FROM UNIT',I3,' - STOP 22'/)
      CALL W3TAGE('PREPOBS_PREVENTS')
      CALL ERREXIT(22)

      END
