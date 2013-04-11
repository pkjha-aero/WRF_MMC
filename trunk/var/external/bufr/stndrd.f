      SUBROUTINE STNDRD(LUNIT,MSGIN,LMSGOT,MSGOT)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STNDRD
C   PRGMMR: ATOR             ORG: NP12       DATE: 2004-08-18
C
C ABSTRACT: THIS SUBROUTINE READS AN INPUT NCEP BUFR MESSAGE CONTAINED
C   WITHIN ARRAY MSGIN AND, USING THE BUFR TABLES INFORMATION ASSOCIATED
C   WITH LOGICAL UNIT LUNIT, OUTPUTS A "STANDARDIZED" VERSION OF THIS
C   SAME MESSAGE WITHIN ARRAY MSGOT.  THIS "STANDARDIZATION" INVOLVES
C   REMOVING ALL OCCURRENCES OF NCEP BUFRLIB-SPECIFIC BYTE COUNTERS AND
C   BIT PADS IN SECTION 4 AS WELL AS REPLACING THE TOP-LEVEL TABLE A FXY
C   NUMBER IN SECTION 3 WITH AN EQUIVALENT SEQUENCE OF LOWER-LEVEL
C   TABLE B, TABLE C, TABLE D AND/OR REPLICATION FXY NUMBERS WHICH
C   DIRECTLY CONSTITUTE THAT TABLE A FXY NUMBER AND WHICH THEMSELVES ARE
C   ALL WMO-STANDARD.  THE RESULT IS THAT THE OUTPUT MESSAGE IN MSGOT IS
C   NOW ENTIRELY COMPLIANT WITH WMO FM-94 BUFR REGULATIONS (I.E. IT IS
C   NOW "STANDARD"). IT IS IMPORTANT TO NOTE THAT THE SEQUENCE EXPANSION
C   WITHIN SECTION 3 MAY CAUSE THE FINAL "STANDARDIZED" BUFR MESSAGE TO
C   BE LONGER THAN THE ORIGINAL INPUT NCEP BUFR MESSAGE BY AS MANY AS
C   1000 BYTES, SO THE USER MUST ALLOW FOR ENOUGH SPACE TO ACCOMODATE
C   SUCH AN EXPANSION WITHIN THE MSGOT ARRAY.
C
C PROGRAM HISTORY LOG:
C 2004-08-18  J. ATOR    -- ORIGINAL AUTHOR
C                           THIS SUBROUTINE IS MODELED AFTER SUBROUTINE
C                           STANDARD; HOWEVER, IT USES SUBROUTINE RESTD
C                           TO EXPAND SECTION 3 AS MANY LEVELS AS
C                           NECESSARY IN ORDER TO ATTAIN TRUE WMO
C                           STANDARDIZATION (WHEREAS STANDARD ONLY
C                           EXPANDED THE TOP-LEVEL TABLE A FXY NUMBER
C                           ONE LEVEL DEEP), AND IT ALSO CONTAINS AN
C                           EXTRA INPUT ARGUMENT LMSGOT WHICH PREVENTS
C                           OVERFLOW OF THE MSGOT ARRAY
C 2005-11-29  J. ATOR    -- USE GETLENS AND IUPBS01; ENSURE THAT BYTE 4
C                           OF SECTION 4 IS ZEROED OUT IN MSGOT; CHECK
C                           EDITION NUMBER OF BUFR MESSAGE BEFORE 
C                           PADDING TO AN EVEN BYTE COUNT
C
C USAGE:    CALL STNDRD (LUNIT, MSGIN, LMSGOT, MSGOT)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     MSGIN    - INTEGER: *-WORD ARRAY CONTAINING BUFR MESSAGE IN NCEP
C                BUFR
C     LMSGOT   - INTEGER: DIMENSIONED SIZE (IN INTEGER WORDS) OF MSGOT;
C                USED BY THE SUBROUTINE TO ENSURE THAT IT DOES NOT
C                OVERFLOW THE MSGOT ARRAY
C
C   OUTPUT ARGUMENT LIST:
C     MSGOT    - INTEGER: *-WORD ARRAY CONTAINING INPUT BUFR MESSAGE
C                NOW IN STANDARDIZED BUFR
C
C REMARKS:
C    MSGIN AND MSGOT MUST BE SEPARATE ARRAYS.
C
C    THIS ROUTINE CALLS:        BORT     GETLENS  ISTDESC  IUPB
C                               IUPBS01  MVB      NUMTAB   PKB
C                               PKC      RESTD    STATUS   UPB
C                               UPC
C    THIS ROUTINE IS CALLED BY: MSGWRT
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      DIMENSION ICD(MAXNC)

      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      DIMENSION MSGIN(*),MSGOT(*)

      CHARACTER*128 BORT_STR
      CHARACTER*8   SUBSET
      CHARACTER*4   SEVN
      CHARACTER*1   TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  LUNIT MUST POINT TO AN OPEN BUFR FILE
C  -------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

C  IDENTIFY THE SECTION LENGTHS AND ADDRESSES IN MSGIN
C  ---------------------------------------------------

      CALL GETLENS(MSGIN,5,LEN0,LEN1,LEN2,LEN3,LEN4,LEN5)

      IAD3 = LEN0+LEN1+LEN2
      IAD4 = IAD3+LEN3

      LENN = LEN0+LEN1+LEN2+LEN3+LEN4+LEN5

      LENM = IUPBS01(MSGIN,'LENM')

      IF(LENN.NE.LENM) GOTO 901

      MBIT = (LENN-4)*8
      CALL UPC(SEVN,4,MSGIN,MBIT)
      IF(SEVN.NE.'7777') GOTO 902

C  COPY SECTIONS 0 THROUGH PART OF SECTION 3 INTO MSGOT
C  ----------------------------------------------------

      MXBYTO = (LMSGOT*NBYTW) - 8

      LBYTO = IAD3+7
      IF(LBYTO.GT.MXBYTO) GOTO 905
      CALL MVB(MSGIN,1,MSGOT,1,LBYTO)

C  REWRITE NEW SECTION 3 IN A "STANDARD" FORM
C  ------------------------------------------

      NSUB = IUPB(MSGIN,IAD3+ 5,16)
      ISUB = IUPB(MSGIN,IAD3+10,16)
      IBIT = (IAD3+7)*8

      CALL NUMTAB(LUN,ISUB,SUBSET,TAB,ITAB)
      IF(ITAB.EQ.0) GOTO 903

      IF (ISTDESC(ISUB).EQ.0) THEN

C         ISUB IS A NON-STANDARD TABLE A DESCRIPTOR AND NEEDS
C         TO BE EXPANDED INTO AN EQUIVALENT STANDARD SEQUENCE  

          CALL RESTD(LUN,ISUB,NCD,ICD)
      ELSE

C         ISUB IS ALREADY A STANDARD DESCRIPTOR, SO JUST COPY
C         IT "AS IS" INTO THE NEW SECTION 3 (I.E. NO EXPANSION
C         IS NECESSARY!)

          NCD = 1
          ICD(NCD) = ISUB
      ENDIF

C     USE THE EDITION NUMBER TO DETERMINE THE LENGTH OF THE
C     NEW SECTION 3

      LEN3 = 7+(NCD*2)
      IBEN = IUPBS01(MSGIN,'BEN')
      IF(IBEN.LT.4) THEN
          LEN3 = LEN3+1
      ENDIF
      LBYTO = LBYTO + LEN3 - 7
      IF(LBYTO.GT.MXBYTO) GOTO 905

C     STORE THE DESCRIPTORS INTO THE NEW SECTION 3

      DO N=1,NCD
          CALL PKB(ICD(N),16,MSGOT,IBIT)
      ENDDO

C     DEPENDING ON THE EDITION NUMBER, PAD OUT THE NEW SECTION 3 WITH AN
C     ADDITIONAL ZEROED-OUT BYTE IN ORDER TO ENSURE AN EVEN BYTE COUNT

      IF(IBEN.LT.4) THEN
          CALL PKB(0,8,MSGOT,IBIT)
      ENDIF

C     STORE THE LENGTH OF THE NEW SECTION 3

      IBIT = IAD3*8
      CALL PKB(LEN3,24,MSGOT,IBIT)

C  NOW THE TRICKY PART - NEW SECTION 4
C  -----------------------------------

      NAD4 = IAD3+LEN3

      IBIT = (IAD4+4)*8
      JBIT = (NAD4+4)*8

      LBYTO = LBYTO + 4

C     COPY THE SUBSETS, MINUS THE BYTE COUNTERS AND BIT PADS,
C     INTO THE NEW SECTION 4

      DO 10 I=1,NSUB
      CALL UPB(LSUB,16,MSGIN,IBIT)

      DO L=1,LSUB-2
      CALL UPB(NVAL,8,MSGIN,IBIT)
      LBYTO = LBYTO + 1
      IF(LBYTO.GT.MXBYTO) GOTO 905
      CALL PKB(NVAL,8,MSGOT,JBIT)
      ENDDO

      DO K=1,8
      KBIT = IBIT-K-8
      CALL UPB(KVAL,8,MSGIN,KBIT)
      IF(KVAL.EQ.K) THEN
         JBIT = JBIT-K-8
         GOTO 10
      ENDIF
      ENDDO
      GOTO 904

10    ENDDO

C     FROM THIS POINT ON, WE WILL NEED (AT MOST) 6 MORE BYTES OF SPACE
C     WITHIN MSGOT IN ORDER TO BE ABLE TO STORE THE ENTIRE STANDARDIZED
C     MESSAGE (I.E. WE WILL NEED (AT MOST) 2 MORE ZEROED-OUT BYTES IN
C     SECTION 4 PLUS THE 4 BYTES '7777' IN SECTION 5), SO DO A FINAL
C     MSGOT OVERFLOW CHECK NOW.

      IF(LBYTO+6.GT.MXBYTO) GOTO 905

C     PAD THE NEW SECTION 4 WITH ZEROES UP TO THE NEXT WHOLE BYTE
C     BOUNDARY.

      DO WHILE(.NOT.(MOD(JBIT,8).EQ.0))
         CALL PKB(0,1,MSGOT,JBIT)
      ENDDO

C     DEPENDING ON THE EDITION NUMBER, WE MAY NEED TO FURTHER PAD THE
C     NEW SECTION 4 WITH AN ADDITIONAL ZEROED-OUT BYTE IN ORDER TO
C     ENSURE THAT THE PADDING IS UP TO AN EVEN BYTE BOUNDARY.

      IF( (IBEN.LT.4) .AND. (MOD(JBIT/8,2).NE.0) ) THEN
         CALL PKB(0,8,MSGOT,JBIT)
      ENDIF

      IBIT = NAD4*8
      LEN4 = JBIT/8 - NAD4
      CALL PKB(LEN4,24,MSGOT,IBIT)
      CALL PKB(0,8,MSGOT,IBIT)

C  FINISH THE NEW MESSAGE WITH AN UPDATED SECTION 0 BYTE COUNT
C  -----------------------------------------------------------

      IBIT = 32
      LENN = LEN0+LEN1+LEN2+LEN3+LEN4+LEN5
      CALL PKB(LENN,24,MSGOT,IBIT)

      CALL PKC('7777',4,MSGOT,JBIT)

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: STNDRD - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   WRITE(BORT_STR,'("BUFRLIB: STNDRD - INPUT MESSAGE LENGTH FROM'//
     . ' SECTION 0",I6," DOES NOT EQUAL SUM OF ALL INDIVIDUAL SECTION'//
     . ' LENGTHS (",I6,")")') LENM,LENN
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: STNDRD - INPUT MESSAGE DOES NOT '//
     . 'END WITH ""7777"" (ENDS WITH ",A)') SEVN
      CALL BORT(BORT_STR)
903   CALL BORT('BUFRLIB: STNDRD - INPUT MESSAGE TABLE D SUBSET '//
     . 'DESCRIPTOR NOT FOUND IN INTERNAL TABLE D')
904   CALL BORT('BUFRLIB: STNDRD - BIT MISMATCH COPYING SECTION 4 '//
     . 'FROM INPUT TO OUTPUT (STANDARD) MESSAGE')
905   CALL BORT('BUFRLIB: STNDRD - OVERFLOW OF OUTPUT (STANDARD) '//
     . 'MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')
      END