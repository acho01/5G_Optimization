      IMPLICIT NONE
      INTEGER(4), PARAMETER :: MAX=8, MEE=2*MAX+1
      INTEGER(4), PARAMETER :: NMAX=2*MEE

      CHARACTER(100) :: radiusChar1
      CHARACTER(100) :: radiusChar2
      CHARACTER(100) :: radiusChar3
      CHARACTER(100) :: radiusChar4
      CHARACTER(100) :: epsChar1
      CHARACTER(100) :: epsChar2
      CHARACTER(100) :: epsChar3
      CHARACTER(100) :: epsChar4
      CHARACTER(100) :: distChar1
      CHARACTER(100) :: distChar2
      CHARACTER(100) :: distChar3
      CHARACTER(100) :: fileName
      CHARACTER(100) :: minFreqP
      CHARACTER(100) :: maxFreqP
      CHARACTER(100) :: freqDeltaP

      REAL :: radius1
      REAL :: radius2
      REAL :: radius3
      REAL :: radius4

      REAL :: eps1
      REAL :: eps2
      REAL :: eps3
      REAL :: eps4

      REAL :: dist1
      REAL :: dist2
      REAL :: dist3

      REAL :: minFreq
      REAL :: maxFreq
      REAL :: freqDelta

      CHARACTER(100) :: minBetaP
      CHARACTER(100) :: maxBetaP
      CHARACTER(100) :: betaStepP

      CHARACTER(100) :: minAlphaP
      CHARACTER(100) :: maxAlphaP
      CHARACTER(100) :: alphaStepP

      CHARACTER(100) :: deltaP

      REAL(8) :: ERBC,MUBC,ERM1,MUM1
      REAL(8) :: ERM2
      REAL(8) :: ERM3
      REAL(8) :: ERM4
      REAL(8) :: K0A,PI
      REAL(8) :: K0A2
      REAL(8) :: K0A3
      REAL(8) :: K0A4
      CHARACTER :: CTME*2,CMECO*6
      INTEGER(4) :: I,L,M,N,J,Q
      INTEGER(4) :: CONTR
      REAL(8) :: KXO,KYO

      COMPLEX(8) :: R12_up(-MAX:MAX,-MAX:MAX),T12_up(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: R21_up(-MAX:MAX,-MAX:MAX),T21_up(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) ::R12_up2(-MAX:MAX,-MAX:MAX),T12_up2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) ::R21_up2(-MAX:MAX,-MAX:MAX),T21_up2(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) ::R12_up3(-MAX:MAX,-MAX:MAX),T12_up3(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) ::R21_up3(-MAX:MAX,-MAX:MAX),T21_up3(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) ::R12_up4(-MAX:MAX,-MAX:MAX),T12_up4(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) ::R21_up4(-MAX:MAX,-MAX:MAX),T21_up4(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) ::R12(-MAX:MAX,-MAX:MAX),T12(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) ::R21(-MAX:MAX,-MAX:MAX),T21(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) :: A1(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: A2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: A3(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: A4(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: A5(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: A6(-MAX:MAX,-MAX:MAX)

      REAL(8) :: K0H,PHI,FK0D,BK0D

      CHARACTER :: CR_PLANE*5

      REAL(8) :: B1,C1
      COMPLEX(8) :: CI
      REAL(8) :: KH
      REAL(8) :: K0D1
      REAL(8) :: K0D2
      REAL(8) :: K0D3
      COMPLEX(8) :: AY,AY2,AY3,AY4,AY5

      COMPLEX(8) :: FF_UP(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: FF_LO(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: WD(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: WD_inv(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: FF2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: TT2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: FF2_trans(-MAX:MAX,-MAX:MAX)

      REAL(8) :: width
      REAL(8):: start_point, last_point, kizami
  	COMPLEX(8) :: FF(-MAX:MAX,-MAX:MAX)
  	INTEGER(4) :: IPVT(MEE)
  	COMPLEX(8) :: DET1
  	REAL(8) :: DET2,P
  	COMPLEX(8) ::  Z1, Z2, determ1, determ, Z5, Z6

      REAL(8) :: AA,cpsec     !h/lambda_0
      common /incident/ PHI
      common /K0H/ K0H

      COMPLEX(8) :: RR_gen(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_1(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_3(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_4(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_5(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_6(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_7(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR_gen_8(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) :: UU(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) :: CA(-MAX:MAX)
      COMPLEX(8) :: SA(-MAX:MAX)
      COMPLEX(8) :: BETA,BETA1

      COMPLEX(8) :: LATTICS(-MAX:MAX,-MAX:MAX)

  	COMPLEX(8) ::  QQQ
      REAL(8) :: realbeta, imagbeta
      REAL(8) :: delta
      REAL(8) :: minBeta
      REAL(8) :: maxAlpha
      REAL(8) :: minAlpha
      REAL(8) :: maxBeta
      REAL(8) :: betaStep
      REAL(8) :: alphaStep
      REAL(8) :: t_stop
      REAL(8) :: t_start

      CALL GET_COMMAND_ARGUMENT(1,radiusChar1)   !read parameters
      CALL GET_COMMAND_ARGUMENT(2,radiusChar2)
      CALL GET_COMMAND_ARGUMENT(3,radiusChar3)
      CALL GET_COMMAND_ARGUMENT(4,radiusChar4)
      CALL GET_COMMAND_ARGUMENT(5,distChar1)
      CALL GET_COMMAND_ARGUMENT(6,distChar2)
      CALL GET_COMMAND_ARGUMENT(7,distChar3)
      CALL GET_COMMAND_ARGUMENT(8,epsChar1)
      CALL GET_COMMAND_ARGUMENT(9,epsChar2)
      CALL GET_COMMAND_ARGUMENT(10,epsChar3)
      CALL GET_COMMAND_ARGUMENT(11,epsChar4)

      CALL GET_COMMAND_ARGUMENT(12,minBetaP)
      CALL GET_COMMAND_ARGUMENT(13,maxBetaP)
      CALL GET_COMMAND_ARGUMENT(14,betaStepP)
      CALL GET_COMMAND_ARGUMENT(15,minAlphaP)
      CALL GET_COMMAND_ARGUMENT(16,maxAlphaP)
      CALL GET_COMMAND_ARGUMENT(17,alphaStepP)
      CALL GET_COMMAND_ARGUMENT(18,deltaP)
      CALL GET_COMMAND_ARGUMENT(19,minFreqP)
      CALL GET_COMMAND_ARGUMENT(20,maxFreqP)
      CALL GET_COMMAND_ARGUMENT(21,freqDeltaP)
      CALL GET_COMMAND_ARGUMENT(22,fileName)


**************************************************************
      OPEN(55,FILE=fileName)
**************************************************************
      PI=DACOS(-1.D0) ! pi=3.14
      CI=(0.D0,1.D0)  ! complex i

      CONTR=0 !DO NOT TOUCH

      KXO=0.0D0   !!!! DO NOT TOUCH
      KYO=0.0D0   !!!! DO NOT TOUCH
      CR_PLANE='shift'   ! DO NOT TOUCH

      CTME='TM'   ! IF TM, ELECTRIC FIELD PARALLE TO THE AXIS
                  ! IF TE, MAGNETIC FIELD PARALLE TO THE AXIS

      CMECO='medium' ! RODS ARE DIELECTRIC. IF 'CONDUC' METALLIC RODS

      ERBC=1.0D0    !DIELECTRIC PERMITTIVITY OF background
      MUBC=1.0D0    !MAGNETIC PERMEABILTY background. DO NOT TOUCH

 !     ERM1=11.7D0   !DIELECTRIC PERMITTIVITY OF THE RODS
 !     ERM2=11.7D0
 !     ERM3=11.7D0
 !     ERM4=11.7D0
      MUM1=1.0D0    !DO NOT TOUCH
***************************************************************
***************************************************************
      IF(COMMAND_ARGUMENT_COUNT().NE.22)THEN
        WRITE(*,*)'ERROR, 22 COMMAND-LINE ARGUMENTS REQUIRED, STOPPING'
        STOP
      ENDIF



      READ(radiusChar1,*)radius1                    !then, convert them to REALs
      READ(radiusChar2,*)radius2
      READ(radiusChar3,*)radius3
      READ(radiusChar4,*)radius4
      READ(distChar1,*)dist1                    !then, convert them to REALs
      READ(distChar2,*)dist2
      READ(distChar3,*)dist3
      READ(epsChar1,*)ERM1
      READ(epsChar2,*)ERM2
      READ(epsChar3,*)ERM3
      READ(epsChar4,*)ERM4

      READ(minBetaP,*)minBeta                    !then, convert them to REALs
      READ(maxBetaP,*)maxBeta
      READ(betaStepP,*)betaStep
      READ(minAlphaP,*)minAlpha
      READ(maxAlphaP,*)maxAlpha
      READ(alphaStepP,*)alphaStep
      READ(deltaP,*)delta

      READ(minFreqP,*)minFreq
      READ(maxFreqP,*)maxFreq
      READ(freqDeltaP,*)freqDelta



      WRITE(*,*) radius1, radius2
       WRITE(*,*) minBeta, maxBeta,  betaStep, minAlpha
       WRITE(*,*) maxAlpha, alphaStep,delta
       WRITE(*,*) minFreq,maxFreq,freqDelta

#       minBeta = 0.00001d0;
#       maxBeta = 0.3d0;
#       minAlpha = 0.00001d0;
#       maxAlpha = 0.3d0;
#       delta = 0.008d0;

      call cpu_time(t_start)
      do 44 AA=minFreq,maxFreq,freqDelta
      do 22 realbeta=minBeta,maxBeta,betaStep      ! beta*h/(2*pi)
       do 33 imagbeta=minAlpha, maxAlpha,alphaStep   ! alpha*h/(2*pi)

      BETA=realbeta+CI*imagbeta   ! beta*h/(2*pi)   ! DO NOT TOUCH
***************************************************************
      BETA1=BETA*(AA*dsqrt(ERBC))**(-1.d0)   !DO NOT TOUCH

      K0H=2.D0*PI*AA    !k_0*h    DO NOT TOUCH
      KH=DSQRT(ERBC*MUBC)*K0H   ! DO NOT TOUCH

      K0A=radius1*K0H     !RADIUS OF THE ELEMENTS PER LAYER
      K0A2=radius2*K0H     !RADIUS OF THE ELEMENTS PER LAYER
      K0A3=radius3*K0H     !RADIUS OF THE ELEMENTS PER LAYER
      K0A4=radius4*K0H     !RADIUS OF THE ELEMENTS PER LAYER
*************************************************
      B1=2.D0*PI/KH    ! DO NOT TOUCH
****************************************************
c      In lattice Sums enters the parameters beta*h

      CALL LATT(MAX,K0H,BETA*2.d0*PI,LATTICS)   ! DO NOT TOUCH
****************************************************
      CALL RRTT(ERBC,MUBC,ERM1,MUM1,CONTR,KXO,KYO,K0A,K0H,  ! REF. TRA. 1-LAYER
     & BETA1,MAX,R12_up,T12_up,R21_up,T21_up,CTME,CMECO,
     & LATTICS,CR_PLANE)
      CALL RRTT(ERBC,MUBC,ERM2,MUM1,CONTR,KXO,KYO,K0A2,K0H,  ! REF. TRA. 2-LAYER
     & BETA1,MAX,R12_up2,T12_up2,R21_up2,T21_up2,CTME,CMECO,
     & LATTICS,CR_PLANE)
      CALL RRTT(ERBC,MUBC,ERM3,MUM1,CONTR,KXO,KYO,K0A3,K0H,  ! REF. TRA. 3-LAYER
     & BETA1,MAX,R12_up3,T12_up3,R21_up3,T21_up3,CTME,CMECO,
     & LATTICS,CR_PLANE)
      CALL RRTT(ERBC,MUBC,ERM4,MUM1,CONTR,KXO,KYO,K0A4,K0H,  ! REF. TRA. 4-LAYER
     & BETA1,MAX,R12_up4,T12_up4,R21_up4,T21_up4,CTME,CMECO,
     & LATTICS,CR_PLANE)

***************************************************************
***************************************************************
***************************************************************
      K0D1=dist1*K0H  ! Distance between the layers
      K0D2=dist2*K0H
      K0D3=dist3*K0H

      call Gen_Ref(RR_gen_1,
     & R12_up,R12_up2,R12_up2,T12_up2,T12_up2,
     &MAX,ERBC,MUBC,K0H,K0D1,BETA1)

      call Gen_Ref(RR_gen_2,
     & RR_gen_1,R12_up3,R12_up3,T12_up3,T12_up3,
     &MAX,ERBC,MUBC,K0H,K0D2,BETA1)

      call Gen_Ref(RR_gen,
     & RR_gen_2,R12_up4,R12_up4,T12_up4,T12_up4,
     &MAX,ERBC,MUBC,K0H,K0D3,BETA1)

 !     call Gen_Ref(RR_gen_4,
 !    & RR_gen,R12_up4,R12_up4,T12_up4,T12_up4,MAX,ERBC,MUBC,K0H,K0D,BETA1)

 !     call Gen_Ref(RR_gen_5,
 !    & RR_gen_4,R12_up,R12_up,T12_up,T12_up,MAX,ERBC,MUBC,K0H,K0D,BETA1)

 !     call Gen_Ref(RR_gen_6,
  !   & RR_gen_5,R12_up,R12_up,T12_up,T12_up,MAX,ERBC,MUBC,K0H,K0D,BETA1)

   !   call Gen_Ref(RR_gen,
   !  & RR_gen_6,R12_up,R12_up,T12_up,T12_up,MAX,ERBC,MUBC,K0H,K0D,BETA1)
**********************************************************************
**********************************************************************
      width=K0H*1.0d0
      CALL DD(MAX,ERBC,MUBC,K0H,width,BETA1,WD)

      DO I=-MAX,MAX
         DO J=-MAX,MAX
            Z1=(0.D0,0.D0)
            Z2=(0.D0,0.D0)
            DO M=-MAX,MAX
              Z1=Z1+WD(I,M)*RR_gen(M,J)      ! Up
              Z2=Z2+WD(I,M)*RR_gen(M,J)      ! Down
            END DO
            FF_UP(I,J)=Z1
            FF_LO(I,J)=Z2
         END DO
      END DO

      DO I=-MAX,MAX
         DO J=-MAX,MAX
            Z1=(0.D0,0.D0)
            DO M=-MAX,MAX
               Z1=Z1+FF_UP(I,M)*FF_LO(M,J)   ! DO NOT TOUCH
            END DO
            FF2(I,J)=-Z1
         END DO
         FF2(I,I)=FF2(I,I)+1.d0
      END DO
******************************************************
      CALL DLFTCG(MEE,FF2,MEE,FF,MEE,IPVT)
      CALL DLFDCG(MEE,FF,MEE,IPVT,DET1,DET2)   ! DO NOT TOUCH
      determ=DET1*10.D0**DET2
******************************************************
 !     write(55,*) realbeta, imagbeta
    !   write(55,*) BETA, abs(determ)
   !    write(55,*) BETA, abs(determ)
       if (abs(determ).lt.0.03) then
        write(*,*) 'Maybe Found the Root'
   !     write(55,*) 'frequency: ', AA
   !     write(55,*)'beta: ', minBeta, '-', maxBeta
   !     write(55,*)'alpha: ', minAlpha, '-', maxAlpha
   !     write(55,*) BETA, abs(determ)
         write(55,*) AA, realbeta, imagbeta
        minBeta = realbeta;
        maxBeta = minBeta + delta;
        maxAlpha = imagbeta;
        minAlpha = maxAlpha - delta;
       if(minAlpha.lt.0) then
            minAlpha = 0d0;
        end if
        go to 44;
      end if
33    continue
22    continue
44    continue
       call cpu_time(t_stop)
        write ( *, * ) 'Elapsed CPU time = ', t_stop - t_start

      END



************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************

      SUBROUTINE Gen_Ref(RR_gen,
     & Ref,RRRR1,RRRR2,TTTT1,TTTT2,MAX,ERBC,MUBC,K0H,K0D,BETA1)

      IMPLICIT NONE

      INTEGER(4):: MAX,MEE

      COMPLEX(8) :: CI

      COMPLEX(8) :: RR_gen(-MAX:MAX,-MAX:MAX)

      CHARACTER :: CTME*2,CMECO*6
      REAL(8) :: PI

      COMPLEX(8) :: II(-MAX:MAX,-MAX:MAX)        !UNIT MATRIX

      INTEGER(4) :: N,M,I,J,P,Q
      COMPLEX(8) :: Z1,Z2,Z3,Z4

      COMPLEX(8) :: Ref(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) :: RR1(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR3(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR4(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR5(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR6(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR7(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR8(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR9(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RR10(-MAX:MAX,-MAX:MAX)

      REAL(8) ::KH,K0H
      REAL(8) :: ERBC,MUBC,KD,K0D
      COMPLEX(8) :: WD(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RRRR1(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: TTTT1(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: RRRR2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: TTTT2(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: MM_inv(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: MM(-MAX:MAX,-MAX:MAX)

      COMPLEX(8) :: BETA1
*************************************************************************************
*************************************************************************************
      PI=DACOS(-1.D0)
      CI=(0.D0,1.D0)

      call DD(MAX,ERBC,MUBC,K0H,K0D,BETA1,WD)

       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z1=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z1=Z1+WD(M,J)*Ref(J,N)
            END DO
           RR1(M,N)=Z1
       END DO
       END DO


       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z2=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z2=Z2+RRRR2(M,J)*RR1(J,N)
            END DO
           RR2(M,N)=Z2
       END DO
       END DO


       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z2=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z2=Z2+WD(M,J)*RR2(J,N)
            END DO
           RR3(M,N)=-Z2
       END DO
       END DO
***************************************************
      DO M=-MAX,MAX
         DO N=-MAX,MAX
          II(M,N)=(0.D0,0.d0)
         END DO
	    II(M,M)=(1.d0,0.d0)
      END DO
***************************************************
      MM=II+RR3
***************************************************
      MEE=2*MAX+1

      CALL DLINCG(MEE,MM,MEE,MM_inv,MEE)
************************************************
       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z2=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z2=Z2+WD(M,J)*TTTT1(J,N)
            END DO
            RR4(M,N)=Z2
       END DO
       END DO


       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z2=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z2=Z2+MM_inv(M,J)*RR4(J,N)
            END DO
            RR5(M,N)=Z2
       END DO
       END DO


       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z2=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z2=Z2+Ref(M,J)*RR5(J,N)
            END DO
           RR6(M,N)=Z2
       END DO
       END DO


       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z2=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z2=Z2+WD(M,J)*RR6(J,N)
            END DO
            RR7(M,N)=Z2
       END DO
       END DO


       DO M=-MAX,MAX
       DO N=-MAX,MAX
            Z2=(0.D0,0.d0)
            DO J=-MAX,MAX
                 Z2=Z2+TTTT2(M,J)*RR7(J,N)
            END DO
            RR8(M,N)=Z2
       END DO
       END DO

        RR_gen=RRRR1+RR8


        RETURN
        END SUBROUTINE Gen_Ref

************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************
************************************************************************

      SUBROUTINE RRTT(ERBC,MUBC,ERM1,MUM1,CONTR,KXO,KYO,K0A,K0H,
     & BETA1,MMAXA1,R12,T12,R21,T21,CTME,CMECO,LATTICS,CR_PLANE)

      IMPLICIT NONE

      INTEGER(4) :: MMAXA1
      COMPLEX(8) :: UU(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: VV(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: PIN(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: QIN(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: BUF(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: BUF1(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)

      REAL(8) :: A1,B1,C1
      COMPLEX(8) :: Z1,Z2,Z3,Z4,Z5,Z6,Z7
      COMPLEX(8) :: Z_Z1,Z_Z2,Z_Z3,Z_Z4
      COMPLEX(8) :: Z_Z5,Z_Z6,Z_Z7
      REAL(8) :: ERBC,MUBC,K0H,KH,PI
      REAL(8) :: PHI
      COMPLEX(8) :: CI

      COMPLEX(8) :: TMATRIX(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: YMATRIX(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)

      COMPLEX(8) :: LATTICS(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: PS(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: QS(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: RR(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: FF(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: FFQ(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: R12(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: T12(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: R21(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      COMPLEX(8) :: T21(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      INTEGER(4) :: MA
      INTEGER(4) :: CONTR,CONTR1
      REAL(8) :: KXO,KYO,ERM1,MUM1
      REAL(8) :: K0A
      INTEGER(4) :: L,M,N,J,I

      CHARACTER  :: CTME*2,CMECO*6

      COMPLEX(8) :: BETA1
      COMPLEX(8) :: CA(-MMAXA1:MMAXA1)
      COMPLEX(8) :: SA(-MMAXA1:MMAXA1)
      COMPLEX(8) :: AY,AY2
      CHARACTER :: CR_PLANE*5
      REAL(8) :: KXB

  	COMPLEX(8) ::  QQQ

      PI=DACOS(-1.D0)
      CI=(0.D0,1.D0)

      KH=DSQRT(ERBC*MUBC)*K0H

      A1=2.D0/KH
      B1=2.D0*PI/KH

      DO L=-MMAXA1,MMAXA1
         CA(L)=BETA1+DBLE(L)*B1  ! X houkou no denpan jyousuu.
         AY=CA(L)*CA(L)-1.D0-1.D-20*CI
         SA(L)=CI*CDSQRT(AY)     ! Y houkou no denpan jyousuu.
      END DO
      QQQ=-SA(0)
      SA(0)=QQQ

      DO L=-MMAXA1,MMAXA1
         Z1=CA(L)+CI*SA(L)
         Z2=CA(L)-CI*SA(L)
         DO M=-MMAXA1,MMAXA1
            Z5=(-CI)**M*A1/SA(L)
            Z6=Z1**M

            Z7=Z2**M
            UU(L,M)=Z5*Z6
            VV(L,M)=Z5*Z7
            PIN(M,L)=CI**M*Z6
            QIN(M,L)=CI**M*Z7
         END DO
      END DO
**********
      CONTR1=0

      CALL  TMAX_Y(TMATRIX,YMATRIX,MMAXA1,ERBC,MUBC,ERM1,MUM1,
     & K0A,CTME,CMECO,CONTR1,KXO,KYO)
****************************************************
****************************************************
      DO M=-MMAXA1,MMAXA1
         DO N=-MMAXA1,MMAXA1
            Z1=(0.D0,0.D0)
            Z2=(0.D0,0.D0)
            Z3=(0.D0,0.D0)
            DO J=-MMAXA1,MMAXA1
               Z1=Z1+TMATRIX(M,J)*LATTICS(J,N)
               Z2=Z2+TMATRIX(M,J)*PIN(J,N)
               Z3=Z3+TMATRIX(M,J)*QIN(J,N)
            END DO
            RR(M,N)=-Z1
            FF(M,N)=Z2  !TP^{in}
            FFQ(M,N)=Z3 !TQ^{in}
         END DO
         RR(M,M)=RR(M,M)+1.D0
      END DO

*  compute the inverse of a complex general matrix
      MA=2*MMAXA1+1

c      WRITE(6,*) 'INVERSE'
      CALL DLINCG(MA,RR,MA,BUF,MA)

      DO M=-MMAXA1,MMAXA1
         DO N=-MMAXA1,MMAXA1
            Z1=(0.D0,0.D0)
            Z2=(0.D0,0.D0)
            DO J=-MMAXA1,MMAXA1
               Z1=Z1+BUF(M,J)*FF(J,N)  !
               Z2=Z2+BUF(M,J)*FFQ(J,N)  !
            END DO
            PS(M,N)=Z1
            QS(M,N)=Z2
         END DO
      END DO

      DO M=-MMAXA1,MMAXA1
         DO N=-MMAXA1,MMAXA1
            Z1=(0.D0,0.D0)
            Z2=(0.D0,0.D0)
            Z3=(0.D0,0.D0)
            Z4=(0.D0,0.D0)
            DO J=-MMAXA1,MMAXA1
               Z1=Z1+UU(M,J)*PS(J,N)
               Z2=Z2+VV(M,J)*PS(J,N)
               Z3=Z3+VV(M,J)*QS(J,N)
               Z4=Z4+UU(M,J)*QS(J,N)
            END DO
            R12(M,N)=Z1
            T12(M,N)=Z2
            R21(M,N)=Z3
            T21(M,N)=Z4
         END DO
         T12(M,M)=T12(M,M)+1.D0
         T21(M,M)=T21(M,M)+1.D0
      END DO



      END SUBROUTINE RRTT

********************************************************************

*******************************************************************
*******************************************************************
*******************************************************************

      SUBROUTINE LATT(MAX,K0H,BETA,LATTICS_SUMS)

      USE BINOM_INT
      USE GAMMA_INT
      USE GAMIC_INT
      USE ERFC_INT
      USE CERFE_INT
      USE ENE_INT
      USE FAC_INT
      USE EI_INT

      IMPLICIT NONE

      INTEGER(4) :: MAX
      REAL(8) :: K0A,PI

      INTEGER(4) :: I,L,M,N,J,Q

      REAL(8) :: K0H,PHI,FK0D,BK0D

      REAL(8) :: B1,C1
      COMPLEX(8) :: CA(-MAX:MAX)
      COMPLEX(8) :: SA(-MAX:MAX),GA(-MAX:MAX),PA(-MAX:MAX)
      COMPLEX(8) :: TT(-MAX:MAX),a,aaa,UU
      COMPLEX(8) :: CI
      REAL(8) :: K0D,KH,UU1,UU2
      COMPLEX(8) :: AY,AY2,AY3,AY4,AY5,tau0,sum,sum1

  	COMPLEX(8) ::  Z1, Z2, Z3, Z4, Z5

      REAL(8) :: AA     !h/lambda_0
      INTEGER(4) :: SS,NN,TUTU
      REAL(8) :: b,KK,ETA,c
      REAL(8) :: JJ(0:2*MAX)

      REAL(8) :: summm
      COMPLEX(8) :: LATTICS(0:2*MAX),tau2(0:2*MAX),tau1(0:2*MAX)
      COMPLEX(8) :: BETA

      COMPLEX(8) :: LATTICS_SUMS(-MAX:MAX,-MAX:MAX)
  	COMPLEX(8) :: QQQ


      PI=DACOS(-1.D0)
      CI=(0.D0,1.D0)

      ETA=1.77d0   ! should be properly chosen. Very importrant in Ewald method
****************************************************
      do TUTU=1,MAX  ! It defines the element of Lattice Sums matrix, N=m-n
                       ! MAX should be even
****************************************************
****************************************************
      N=2d0*TUTU
****************************************************
****************************************************
****************************************************
      sum=(0.d0,0.d0)
      do SS=1,MAX   !MAX   ! summation with s in (5.10)
        call INTEG(JJ,K0H,SS,ETA,MAX)

        sum=sum+(cdexp(CI*SS*BETA)+(-1.d0)**N*cdexp(-CI*SS*BETA))*
     &          (SS/K0H)**N*JJ(N)
      end do

      tau2(N)=2.d0**(N+1)/(CI*PI)*sum
*************************************************
*************************************************
*************************************************
*************************************************
*************************************************
      B1=2.D0*PI/K0H

	DO L=-MAX,MAX
         CA(L)=(BETA/K0H)+DBLE(L)*B1
         AY=CA(L)*CA(L)-1.D0-1.D-20*CI
         SA(L)=CI*CDSQRT(AY)
         GA(L)=CDSQRT(AY)     ! GA is eqaul to gamma_s in reference, eq.(5.3)
         PA(L)=GA(L)*K0H/(2*ETA)
      END DO
      QQQ=-SA(0)
      SA(0)=QQQ
      GA(0)=SA(0)/CI
      PA(0)=GA(0)*K0H/(2*ETA)

      sum1=(0.d0,0.d0)
	DO L=-MAX,MAX
       sum=(0.d0,0.d0)
       do J=0,(N)/2     !!!!!
        a=PA(L)
        call SUMM(UU,a,J)
        UU1=BINOM(N,2*J)
        sum=sum+(-1.d0)**J*UU1*(SA(L)/CA(L))**(2.d0*J)*UU
       end do
       TT(L)=sum
       sum1=sum1+(CA(L)**N/SA(L))*TT(L)
      end do

      tau1(N)=2.d0*CI**N/K0H*sum1
*************************************************
      LATTICS(N)=tau1(N)+tau2(N)
*************************************************
*************************************************
      end do






****************************************************
      do TUTU=1,MAX    ! It defines the element of Lattice Sums matrix, N=m-n
                       ! MAX should be even
****************************************************
****************************************************
      N=2d0*TUTU-1d0
****************************************************
****************************************************
****************************************************
      sum=(0.d0,0.d0)
      do SS=1,MAX   !MAX   ! summation with s in (5.10)
        call INTEG(JJ,K0H,SS,ETA,MAX)

        sum=sum+(cdexp(CI*SS*BETA)+(-1.d0)**N*cdexp(-CI*SS*BETA))*
     &          (SS/K0H)**N*JJ(N)
      end do

      tau2(N)=2.d0**(N+1)/(CI*PI)*sum
*************************************************
*************************************************
*************************************************
*************************************************
*************************************************
      B1=2.D0*PI/K0H

	DO L=-MAX,MAX
         CA(L)=(BETA/K0H)+DBLE(L)*B1
         AY=CA(L)*CA(L)-1.D0-1.D-20*CI
         SA(L)=CI*CDSQRT(AY)
         GA(L)=CDSQRT(AY)     ! GA is eqaul to gamma_s in reference, eq.(5.3)
         PA(L)=GA(L)*K0H/(2*ETA)
      END DO
      QQQ=-SA(0)
      SA(0)=QQQ
      GA(0)=SA(0)/CI
      PA(0)=GA(0)*K0H/(2*ETA)

      sum1=(0.d0,0.d0)
	DO L=-MAX,MAX
       sum=(0.d0,0.d0)
       do J=0,(N-1)/2     !!!!!
        a=PA(L)
        call SUMM(UU,a,J)
        UU1=BINOM(N,2*J)
        sum=sum+(-1.d0)**J*UU1*(SA(L)/CA(L))**(2.d0*J)*UU
       end do
       TT(L)=sum
       sum1=sum1+(CA(L)**N/SA(L))*TT(L)
      end do

      tau1(N)=2.d0*CI**N/K0H*sum1
*************************************************
      LATTICS(N)=tau1(N)+tau2(N)
*************************************************
*************************************************
      end do



****************************************************
      N=0d0
****************************************************
      sum=(0.d0,0.d0)
      do SS=1,MAX   !MAX   ! summation with s in (5.10)
        call INTEG(JJ,K0H,SS,ETA,MAX)

        sum=sum+(cdexp(CI*SS*BETA)+(-1.d0)**N*cdexp(-CI*SS*BETA))*
     &          (SS/K0H)**N*JJ(N)
      end do

      tau2(N)=2.d0**(N+1)/(CI*PI)*sum
*************************************************
*************************************************
*************************************************
*************************************************
*************************************************
      B1=2.D0*PI/K0H

	DO L=-MAX,MAX
         CA(L)=(BETA/K0H)+DBLE(L)*B1
         AY=CA(L)*CA(L)-1.D0-1.D-20*CI
         SA(L)=CI*CDSQRT(AY)
         GA(L)=CDSQRT(AY)     ! GA is eqaul to gamma_s in reference, eq.(5.3)
         PA(L)=GA(L)*K0H/(2*ETA)
      END DO
      QQQ=-SA(0)
      SA(0)=QQQ
      GA(0)=SA(0)/CI
      PA(0)=GA(0)*K0H/(2*ETA)

      sum1=(0.d0,0.d0)
	DO L=-MAX,MAX
       sum=(0.d0,0.d0)
       do J=0,(N)/2     !!!!!
        a=PA(L)
        call SUMM(UU,a,J)
        UU1=BINOM(N,2*J)
        sum=sum+(-1.d0)**J*UU1*(SA(L)/CA(L))**(2.d0*J)*UU
       end do
       TT(L)=sum
       sum1=sum1+(CA(L)**N/SA(L))*TT(L)
      end do

      tau1(N)=2.d0*CI**N/K0H*sum1
*************************************************
*************************************************
      c=K0H**2/(4.d0*ETA**2)
      tau0=-1.d0-CI/PI*EI(c)
*************************************************
      LATTICS(N)=tau1(N)+tau2(N)+tau0
*************************************************
*************************************************

      DO I=-MAX,MAX
         DO J=-MAX,MAX
            M=I-J
            IF(M>=0) THEN
               LATTICS_SUMS(I,J)=LATTICS(M)
            ELSE
               LATTICS_SUMS(I,J)=(-1)**M*LATTICS(-M)
            END IF
         END DO
      END DO


        END SUBROUTINE LATT


************************************************************************
************************************************************************
************************************************************************
************************************************************************
**********************  START OF SUBROUTINE  ***************************************

      SUBROUTINE INTEG(JJ,KK,SS,ETA,MAX)

      USE BINOM_INT
      USE GAMMA_INT
      USE GAMIC_INT
      USE ERFC_INT
      USE CERFE_INT
      USE ENE_INT
      USE FAC_INT
      USE EI_INT

      IMPLICIT NONE

      INTEGER(4) :: MAX

      INTEGER(4) :: M,N,I,L

      INTEGER(4) :: NN,SS
      COMPLEX(8):: CI

      REAL(8) :: a,b,h,PI,KK
      REAL(8) :: XX,sum
      REAL(8) :: JJ(0:2*MAX)

      REAL(8) :: ETA

      INTEGER(4), PARAMETER :: YYY=11
      REAL(8) :: YY1,YY2,YY3(YYY),YY4(YYY),YY5
      REAL(8) :: summm,summm1,summm2
**************************************************************
      PI=DACOS(-1.D0)
      CI=(0.D0,1.D0)
*****************************************************
*****************************************************
*****************************************************
       YY1=SS**2*ETA**2
       call ENE(YY1,YYY,YY3)

       do 10 L=1,YYY
         YY4(L)=YY3(L)*exp(-YY1)
10     continue

       summm=0.d0
       do I=0,10
         YY5=FAC(I)
         summm=summm+0.5d0*(KK/(2*ETA))**(2*I)*
     &         (1.d0/YY5)*YY4(I+1)
       end do

       JJ(0)=summm
*****************************************************
*****************************************************
*****************************************************
       YY1=SS**2*ETA**2
       call ENE(YY1,YYY,YY3)

       summm1=(ETA**2/2.d0)/(ETA**2*SS**2)*exp(-ETA**2*SS**2)

       do 11 L=1,YYY
         YY4(L)=YY3(L)*exp(-YY1)
11     continue

       summm2=0.d0
       do I=1,10
         YY5=FAC(I)
         summm2=summm2+ETA**2/2.d0*(KK/2)**(2*I)*
     &         (1.d0/YY5)*YY4(I)*(1.d0/ETA**(2*I))
       end do

       JJ(1)=summm1+summm2
*****************************************************
*****************************************************
*****************************************************
       do N=2,2*MAX
        JJ(N)=(0.5d0/SS**2)*(  2*(N-1)*JJ(N-1)-
     &     (KK**2/2d0)*JJ(N-2)+
     &     ETA**(2.d0*(N-1))*exp(-ETA**2*SS**2+KK**2/(4.d0*ETA**2))  )
        end do
*****************************************************
*****************************************************
*****************************************************


        END SUBROUTINE INTEG
************************************************************************
************************************************************************




************************************************************************
************************************************************************
************************************************************************
**********************  START OF SUBROUTINE  ***************************

      SUBROUTINE SUMM(TT,a,J)
      USE CERFE_INT
      USE GAMMA_INT
      USE ERFC_INT

      IMPLICIT NONE

      INTEGER(4) :: MAX

      INTEGER(4) :: M,N,P,L,J

      COMPLEX(8):: CI,a,aaa,sum

      REAL(8) :: PI,YYYY

      COMPLEX(8) :: PA
      COMPLEX(8) :: TT,KK

**************************************************************
      PI=DACOS(-1.D0)
      CI=(0.D0,1.D0)


       if ( (real(a).ge.0.d0).AND.(imag(a).ge.0.d0) ) then
         aaa=CI*real(a)-imag(a)
       else if ( (real(a).ge.0.d0).AND.(imag(a).lt.0.d0) ) then
         aaa=CI*real(a)-imag(a)
       else if ( (real(a).le.0.d0).AND.(imag(a).le.0.d0) ) then
         aaa=CI*real(a)-imag(a)
       else if ( (real(a).le.0.d0).AND.(imag(a).ge.0.d0) ) then
         aaa=CI*real(a)-imag(a)
       end if

       KK=cdexp(aaa**2)*CERFE(aaa)

       if (J==0) then
        sum=(0.d0,0.d0)
       else
        sum=(0.d0,0.d0)
       do P=1,J
        sum=sum+a**(1-2*P)/GAMMA(1.5d0-P)
       end do
       end if

       TT=KK-cdexp(-a**2)*sum


       END SUBROUTINE SUMM
************************************************************************











      SUBROUTINE TMAX_Y(T_MATRIX,Y_MATRIX,MAX,ERBC,MUBC,ERM1,MUM1,
     & K0A,CTME,CMECO,CONTR,KXO,KYO)

************************************************************************
*    Subroutine Name TMATR                                             *
*     T-Matrix of the isolated single cylinder which radius is a       *
*                                                                      *
*  Copyright :  Hongting Jia, May. 23  2002  Ver.1 new version         *
*                                                                      *
*   ===  input  ===                                                    *
*   1. MAXA  ... INTEGER(4)                                            *
*      The argument MAXA is integer number                             *
*   2. ERBC,MUBC,ERM1,MUM1 ... REAL(8)                                 *
*      ERBC,MUBC are the relative permittivity and                     *
*      relative permeability of the background medium, respectively.   *
*      ERM1,MUM1 are the relative permittivity and                     *
*      relative permeability of the cylindrical objects, respectively. *
*   3. K0A  ...  REAL(8)                                               *
*      K0A is k0*a wich a is the radius of the cylinder and k0 is wave *
*      number in free space.                                           *
*   4. CONTR ... INTEGER(4)                                            *
*      CONTR is the contral parameter. when the reference              *
*      cylinder is located at  the origin, CONTR=0; otherwise CONTR=1  *
*   5. KXO,KYO ... REAL(8)                                             *
*      KXO,KYO are k0*Xo and k0*Yo, respectively. (Xo,Yo) is the       *
*      position of the reference cylinder.                             *
*   6. CTME,CMECO  ... CHARACTER   *2                                  *
*      CTME*2  if CTME='TE' TE wave, if CTME='TM' TM wave              *
*      CMECO*6  if CMECO='medimu' , if CMECO='conduc' TM wave          *
*                                                                      *
*   === output ===                                                     *
*      T_MATRIX(-MAX:MAX,-MXA:MXA)  ... COMPLEX(8)                     *
*      T_MATRIX  is the T-matrix which is expressed at the origin      *
*      Y_MATRIX(-MAX:MAX,-MXA:MXA)  ... COMPLEX(8)                     *
*      Y_MATRIX  is the Y-matrix which is expressed at the origin      *
*                                                                      *
*                                                                      *
***********************************************************************
      IMPLICIT NONE
      INTEGER(4) :: MAX

      COMPLEX(8) :: T_MATRIX(-MAX:MAX,-MAX:MAX)
      COMPLEX(8) :: Y_MATRIX(-MAX:MAX,-MAX:MAX)


      INTEGER(4), PARAMETER :: BJMAX=100
      REAL(8) :: ADJ(BJMAX)
      INTEGER(4) :: NMAX
      COMPLEX(8) :: AT_MATRIX(-BJMAX:BJMAX,-BJMAX:BJMAX)
      REAL(8) :: BSBAC(-BJMAX:BJMAX)
      REAL(8) :: YBC(-BJMAX:BJMAX)
      COMPLEX(8) :: HBC(-BJMAX:BJMAX)
      REAL(8) :: BM1(-BJMAX:BJMAX)
      REAL(8) :: YM1(-BJMAX:BJMAX)
      COMPLEX(8) :: HM1(-BJMAX:BJMAX)

      COMPLEX(8) :: TMA(-BJMAX:BJMAX),YMB(-BJMAX:BJMAX)
      COMPLEX(8) :: A1,Z1,A2,Z2

      INTEGER(4) :: J_MAX,J_MAX1,J_MAX2
      REAL(8) :: KBA,K1A,K0A
      CHARACTER :: CTME*2,CMECO*6
      REAL(8) :: ERBC,MUBC,ERM1,MUM1
      INTEGER(4) :: I,J
      REAL(8) :: ETAR
      COMPLEX(8) :: CI
      INTEGER(4) :: I_MAX
      INTEGER(4) :: CONTR
      REAL(8) :: KXO,KYO


      REAL(8) :: X0,Y0,X1,Y1,R
      REAL(8) :: THETA_0,THETA_1
      COMPLEX(8) :: RT_L(-BJMAX:BJMAX),RT_R(-BJMAX:BJMAX)
      REAL(8) :: BD(-BJMAX:BJMAX)
      INTEGER(4) :: MA,MAV2
      INTEGER(4) :: H_MAX
      COMPLEX(8) :: BETA_LEFT(-BJMAX:BJMAX,-BJMAX:BJMAX)
      COMPLEX(8) :: BETA_RIGHT(-BJMAX:BJMAX,-BJMAX:BJMAX)
      COMPLEX(8) :: ZA(-BJMAX:BJMAX,-BJMAX:BJMAX)
      INTEGER(4) :: N,M,J1



      CI=(0.D0,1.D0)



      IF(((CMECO.NE.'conduc').AND.(CMECO.NE.'medium'))
     & .or.((CTME.NE.'TE').AND.(CTME.NE.'TM'))) THEN
         WRITE(6,*) 'input error, please check your input parameter'
         RETURN
      END IF

      KBA=K0A*DSQRT(ERBC*MUBC)
* Bessel function of real argument
      CALL DJJ(KBA,BJMAX,ADJ,NMAX)

      J_MAX=NMAX
      DO I=0,J_MAX-1
         BSBAC(I)=ADJ(I+1)
         BSBAC(-I)=(-1)**I*ADJ(I+1)
      END DO
* Neumann function of real argument
      CALL DYY(KBA,BJMAX,ADJ,NMAX)
      J_MAX=MIN0(NMAX,J_MAX)
       DO I=0,J_MAX-1
         YBC(I)=ADJ(I+1)
         YBC(-I)=(-1)**I*ADJ(I+1)
      END DO
** Hanker function
      J_MAX1=J_MAX-1
      DO I=-J_MAX1,J_MAX1
         HBC(I)=BSBAC(I)+CI*YBC(I)
      END DO
***********

      IF(CMECO=='conduc') THEN
         J_MAX2=J_MAX-2
         IF(CTME=='TE') THEN
******  TE Conductor
            DO I=-J_MAX2,J_MAX2
               TMA(I)=-(BSBAC(I-1)-BSBAC(I+1))/
     &           (HBC(I-1)-HBC(I+1))
            END DO
         ELSE
******  TM Conductor
            DO I=-J_MAX2,J_MAX2
               TMA(I)=-BSBAC(I)/HBC(I)
            END DO
         END IF
****************
         DO I=-J_MAX2,J_MAX2
            YMB(I)=0.D0
         END DO
      END IF

      IF(CMECO=='medium') THEN
         ETAR=DSQRT(ERM1*MUBC/(ERBC*MUM1))
         K1A=DSQRT(ERM1*MUM1)*K0A

* Bessel function of real argument
         CALL DJJ(K1A,BJMAX,ADJ,NMAX)

         J_MAX=MIN0(NMAX,J_MAX)
         DO I=0,J_MAX-1
            BM1(I)=ADJ(I+1)
            BM1(-I)=(-1)**I*ADJ(I+1)
         END DO
* Neumann function of real argument
         CALL DYY(K1A,BJMAX,ADJ,NMAX)
         J_MAX=MIN0(NMAX,J_MAX)

         DO I=0,J_MAX-1
            YM1(I)=ADJ(I+1)
            YM1(-I)=(-1)**I*ADJ(I+1)
         END DO
** Hanker function
         J_MAX1=J_MAX-1
         DO I=-J_MAX1,J_MAX1
            HM1(I)=BM1(I)+CI*YM1(I)
         END DO
***********

         IF(CTME=='TM') THEN
         J_MAX2=J_MAX1-1
******   TM medium

            DO I=-J_MAX2,J_MAX2
               A1=BM1(I)*(BSBAC(I-1)-BSBAC(I+1))*0.5D0
     &            -ETAR*BSBAC(I)*(BM1(I-1)-BM1(I+1))*0.5D0
               Z1=ETAR*HBC(I)*(BM1(I-1)-BM1(I+1))*0.5D0
     &            -BM1(I)*(HBC(I-1)-HBC(I+1))*0.5D0

               TMA(I)=A1/Z1


               A2=BSBAC(I)*(HBC(I-1)-HBC(I+1))*0.5D0
     &            -HBC(I)*(BSBAC(I-1)-BSBAC(I+1))*0.5D0
               Z2=BM1(I)*(HBC(I-1)-HBC(I+1))*0.5D0
     &             -ETAR*HBC(I)*(BM1(I-1)-BM1(I+1))*0.5D0

               YMB(I)=A2/Z2
            END DO
         ELSE
******  TE medium
            J_MAX2=J_MAX1-1
            DO I=-J_MAX2,J_MAX2
               A1=BSBAC(I)*(BM1(I-1)-BM1(I+1))*0.5D0
     &              -ETAR*BM1(I)*(BSBAC(I-1)-BSBAC(I+1))*0.5D0
               Z1=ETAR*BM1(I)*(HBC(I-1)-HBC(I+1))*0.5D0
     &              -HBC(I)*(BM1(I-1)-BM1(I+1))*0.5D0

               TMA(I)=A1/Z1

               A2=BSBAC(I)*(HBC(I-1)-HBC(I+1))*0.5D0
     &              -HBC(I)*(BSBAC(I-1)-BSBAC(I+1))*0.5D0
               Z2=BM1(I)*(HBC(I-1)-HBC(I+1))*0.5D0
     &              -HBC(I)*(BM1(I-1)-BM1(I+1))*0.5D0/ETAR

               YMB(I)=A2/Z2
            END DO
         END IF


      END IF

      IF(CONTR==0) THEN
         I_MAX=MIN(J_MAX2,MAX)

         DO I=-MAX,MAX
            DO J=-MAX,MAX
               T_MATRIX(I,J)=(0.D0,0.D0)
               Y_MATRIX(I,J)=(0.D0,0.D0)
            END DO
         END DO

         DO I=-I_MAX,I_MAX
            T_MATRIX(I,I)=TMA(I)
            Y_MATRIX(I,I)=YMB(I)
         END DO
      ELSE
**********
         DO I=-J_MAX2,J_MAX2
            DO J=-J_MAX2,J_MAX2
               AT_MATRIX(I,J)=(0.D0,0.D0)
            END DO
         END DO

         DO I=-J_MAX2,J_MAX2
            AT_MATRIX(I,I)=TMA(I)
         END DO
**********
         X0=DSQRT(ERBC*MUBC)*KXO
         Y0=DSQRT(ERBC*MUBC)*KYO

         R=DSQRT(X0*X0+Y0*Y0)
         X1=-X0
         Y1=-Y0
         CALL DJJ(R,BJMAX,ADJ,NMAX)
         DO I=0,NMAX-1
            BD(I)=ADJ(I+1)
            BD(-I)=(-1)**I*ADJ(I+1)
         END DO
         CALL ANGLE(THETA_0,X0,Y0)
         CALL ANGLE(THETA_1,X1,Y1)

         MA=NMAX-1
         DO I=-MA,MA
            RT_L(I)=BD(I)*CDEXP(CI*DBLE(I)*THETA_1)
            RT_R(I)=BD(I)*CDEXP(CI*DBLE(I)*THETA_0)
         END DO

         MAV2=MA/2
         H_MAX=MIN0(J_MAX2,MAV2)

         DO N=-H_MAX,H_MAX
            DO M=-H_MAX,H_MAX
               J=M-N
               BETA_LEFT(N,M)=RT_L(J)
               BETA_RIGHT(N,M)=RT_R(J)
            END DO
         END DO


         DO I=-H_MAX,H_MAX
            DO J=-H_MAX,H_MAX
               Z1=(0.D0,0.D0)
               DO J1=-H_MAX,H_MAX
                  Z1=Z1+BETA_LEFT(I,J1)*AT_MATRIX(J1,J)
               END DO
               ZA(I,J)=Z1
            END DO
         END DO

         DO I=-H_MAX,H_MAX
            DO J=-H_MAX,H_MAX
               Z1=(0.D0,0.D0)
               DO J1=-H_MAX,H_MAX
                  Z1=Z1+ZA(I,J1)*BETA_RIGHT(J1,J)
               END DO
               AT_MATRIX(I,J)=Z1
            END DO
         END DO


         I_MAX=MIN0(H_MAX,MAX)

         DO I=-MAX,MAX
            DO J=-MAX,MAX
               T_MATRIX(I,J)=(0.D0,0.D0)
               Y_MATRIX(I,J)=(0.D0,0.D0)
            END DO
         END DO

         DO I=-I_MAX,I_MAX
            Y_MATRIX(I,I)=YMB(I)
            DO J=-I_MAX,I_MAX
               T_MATRIX(I,J)=AT_MATRIX(I,J)
            END DO
         END DO
      END IF
      RETURN
      END








      SUBROUTINE DD(MMAXA1,ERBC,MUBC,K0H,K0D,BETA1,WD)
      IMPLICIT NONE
      INTEGER(4) :: MMAXA1
      COMPLEX(8) :: WD(-MMAXA1:MMAXA1,-MMAXA1:MMAXA1)
      REAL(8) :: ERBC,MUBC,KD,K0D
      COMPLEX(8) :: SA(-MMAXA1:MMAXA1)
      REAL(8) ::KH,K0H,B1
      COMPLEX(8) :: Z1,CI,AY
      REAL(8) :: PHI,PI
      INTEGER(4) :: L,J

      COMPLEX(8) :: BETA1,A2,CCA
      COMPLEX(8) ::  QQQ

      PI=DACOS(-1.D0)
      CI=(0.D0,1.D0)

      KD=DSQRT(ERBC*MUBC)*K0D
      KH=DSQRT(ERBC*MUBC)*K0H

      B1=2.D0*PI/KH
      A2=BETA1
      DO L=-MMAXA1,MMAXA1
         CCA=A2+DBLE(L)*B1
         AY=CCA*CCA-1.D0-1.D-20*CI
         SA(L)=CI*CDSQRT(AY)
      END DO
      QQQ=-SA(0)
      SA(0)=QQQ

      DO L=-MMAXA1,MMAXA1
         DO J=-MMAXA1,MMAXA1
            WD(L,J)=0.D0
         END DO
         Z1=CI*SA(L)*KD
         WD(L,L)=CDEXP(Z1)
      END DO


      RETURN
      END











      SUBROUTINE ANGLE(ANG,X,Y)
      REAL(8) :: ANG,X,Y,R,PI,X1,Y1
      R=DSQRT(X*X+Y*Y)
      IF(R<1.D-14) THEN
         WRITE(6,*) 'THE POINT IS THE ORIGIN'
         RETURN
      END IF
      X1=X/R
      Y1=Y/R
      PI=DACOS(-1.D0)
      IF(DABS(Y1)>0.5D0) THEN
         ANG=DACOS(X1)
         IF(Y1<0.D0) ANG=2.D0*PI-ANG
      ELSE
         IF(X1>0.D0) THEN
            ANG=DASIN(Y1)
            IF(ANG<0.D0) ANG=ANG+2.D0*PI
         ELSE
            ANG=PI-DASIN(Y1)
         END IF
      END IF
      RETURN
      END


      SUBROUTINE DYY(X, MM, ADY, NMAX)
**********************************************************************
*     Subroutine Name  DYY                                           *
*        Bessel Functions of the second kind   (Array Type)          *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*       X...REAL*8                                                   *
*                                                                    *
*  ===  output ===                                                   *
*       NMAX...maximum N for given X which satisfy  |Yn(X)|<1.0E+75  *
*       ADY(N) (N=1, NMAX)...Bessel functions of the second kind     *
*          ADY(1) = Y0(X)                                            *
*          ADY(2) = Y1(X)                                            *
*          ADY(3) = Y2(X)                                            *
*          ...........                                               *
*          ...........                                               *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 ( A-H, O-Z )
       REAL*8 ADY(MM)
       REAL*4 XS
       REAL*8 HG2(19),HGW(19)
*
       DATA HG2/0.25485979166099077D+02,  0.18046505467728980D+02,
     $          0.12771825354869194D+02,  0.87697567302686021D+01,
     $          0.56944233429577552D+01,  0.33691762702432690D+01,
     $          0.16923950797931789D+01,  0.60323635708174870D+00,
     $          0.66702230958194404D-01,  0.15129959781108085D+02,
     $          0.91242480375311789D+01,  0.51961525300544656D+01,
     $          0.25525898026681713D+01,  0.89830283456961770D+00,
     $          0.98747014068481182D-01,  0.85886356890120343D+01,
     $          0.39269635013582872D+01,  0.13390972881263614D+01,
     $          0.14530352150331709D+00/
       DATA HGW/0.78281997721158910D-11,  0.10467205795792082D-07,
     $          0.18106544810934304D-05,  0.91811268679294035D-04,
     $          0.18885226302684179D-02,  0.18640042387544652D-01,
     $          0.97301747641315429D-01,  0.28480728566997958D+00,
     $          0.48349569472545555D+00,  0.26585516843563016D-06,
     $          0.85736870435878587D-04,  0.39053905846290619D-02,
     $          0.51607985615883930D-01,  0.26049231026416113D+00,
     $          0.57013523626247958D+00,  0.19960407221136762D-03,
     $          0.17077983007413475D-01,  0.20780232581489188D+00,
     $          0.66114701255824129D+00/
       DATA P2 /0.63661977236758134D+00/
       DATA GAM/0.57721566490153286D+00/
*
      IF(X) 70, 70, 90
   90  XS = X
       Z = 2.0D0 / X
       DL = DLOG(0.5D0 * X)
      IF(XS .LE. 0.1) THEN
       NMAX = -176.50 / ALOG(0.0425 * XS)
       NNN = 11
       MMM = NNN + 5
        IF(XS .LE. 0.001) THEN
         T2 = 0.25D0 * X * X
         AJ0 = T2 * (0.25D0 * T2 - 1.0D0) + 1.0D0
         AJ1 = 0.5D0 * X * (T2 * (T2/12.0D0 - 0.5D0) + 1.0D0)
         ADY(1) = P2 * ((DL + GAM) * AJ0
     $          + T2 * (1.0D0 - 3.0D0 * T2 / 8.0D0))
         ADY(2) = P2 * ((DL + GAM - 1.D0) * AJ1
     $          - (1.0D0 - T2 * (1.0D0 + 0.25D0 * T2)) / X)
         GO TO 555
        ELSE
         GO TO 333
        END IF
      END IF
*
      IF( XS .LE. 1.0) THEN
       NMAX = XS * (-13.25 * XS + 31.50) + 28.0
       NNN = 6.0 * XS + 11.0
       MMM = NNN + 7
       GO TO 333
      END IF
      IF( XS .LE. 10.0) THEN
       NMAX = XS * (-0.28 * XS + 7.80) + 39.5
       NNN = 2.2 * XS + 17.0
       MMM = NNN + 11
       GO TO 333
      END IF
      IF( XS .LE. 20.0) THEN
       NMAX = 2.71 * XS + 62.0
       K = 1
       L = 9
       GO TO 444
      END IF
      IF( XS .LE. 30.0) THEN
       NMAX = 2.21 * XS + 73.5
       K = 10
       L = 15
       GO TO 444
      END IF
      IF( XS .LE. 50.0) THEN
       NMAX = 1.88 * XS + 84.0
       K = 10
       L = 15
       GO TO 444
      END IF
      IF( XS .LE. 100.0) THEN
       NMAX = 1.59 * XS + 98.5
       K = 10
       L = 15
       GO TO 444
      END IF
      IF( XS .LE. 173.0) THEN
       NMAX = 1.40 * XS + 118.0
       K = 16
       L = 19
       GO TO 444
      ELSE
       GO TO 70
      END IF
*
* recurrence relation to get J0(x) and J1(x)
*
  333 CONTINUE
      IF( (NNN/2)*2 .NE. NNN) NNN = NNN - 1
      NB = NNN
      IF( (NNN/4)*4 .EQ. NNN) NB = NNN + 2
      T3 = 0.0D0
      T2 = 1.0D-75
      NM = NB + MMM + 1
      DO 100 I = NB, MMM
       N = NM - I
       T1 = Z * DFLOAT(N) * T2 - T3
       T3 = T2
       T2 = T1
  100 CONTINUE
      S = 0.0D0
      T2 = T2 / T3 * 1.0D-78
      T3 = 1.0D-78
      S1 = 0.0D0
      S2 = 0.0D0
      N2 = NB - 2
      DO 200 I = 1, N2, 4
       N = NB - I + 1
       T1 = Z * DFLOAT(N) * T2 - T3
       WK = 4.0D0 * DFLOAT(N-1) / DFLOAT(N * (N-2))
       S2 = S2 + WK * T1
       N = N - 1
       T3 = T2
       T2 = T1
       T1 = Z * DFLOAT(N) * T2 - T3
       S1 = S1 + 2.0D0 * T1 / DFLOAT(N-1)
       S = S + T1
       T3 = T2
       T2 = T1
       N = N - 1
       T1 = Z * DFLOAT(N) * T2 - T3
       WK = 4.0D0 * DFLOAT(N-1) / DFLOAT(N * (N-2))
       S2 = S2 - WK * T1
       T3 = T2
       T2 = T1
       N = N - 1
       T1 = Z * DFLOAT(N) * T2 - T3
       S1 = S1 - 2.0D0 * T1 / DFLOAT(N-1)
       S = S + T1
       T3 = T2
       T2 = T1
  200 CONTINUE
      AJ1 = 2.0D0 * T2 * Z - T3
      AJ0 = AJ1 * Z -T2
      S = S + S + AJ0
      S = 1.0D0 / S
      AJ0 = S * AJ0
      AJ1 = S * AJ1
      S1 = S * S1
      S2 = S * S2
      ADY(1) = P2 * ((DL + GAM) * AJ0 - 2.0D0 * S1)
      ADY(2) = -P2 * (AJ0 / X - (DL + GAM - 1.0D0) * AJ1 + S2)
      GO TO 555
*
*  Numenical Integration for   Y0(x) and Y1(x)
*
  444 CONTINUE
      T1 = 0.0D0
      T2 = 0.0D0
      S1 = 0.0D0
      S2 = 0.0D0
      T3 = 4.0D0 * X * X
      DO 300 I = K, L
       R2 = DSQRT(HG2(I) * HG2(I) + T3)
       RX = DSQRT(0.5D0 * (R2 + HG2(I)))
       RXI = 1.0D0 / RX
       R2I = 1.0D0 / R2
       T1 = T1 + RXI * R2I * HGW(I)
       S1 = S1 + RX * R2I * HGW(I)
       T2 = T2 + RXI * HG2(I) * HGW(I)
       S2 = S2 + RX * HG2(I) * HGW(I)
  300 CONTINUE
      C = DCOS(X)
      S = DSIN(X)
      ADY(1) =  2.0D0 * P2 * (T1 * S * X - S1 * C)
      ADY(2) = -2.0D0 * P2 * (T2 * S + S2 * C / X)
*
*  recurrence relation   Yn+1(x) = 2*n/x*Yn(x) - Yn-1(x)
*
  555 CONTINUE
      DO 400 N = 3, NMAX
       ADY(N) = (Z * DFLOAT(N - 2)) * ADY(N-1) - ADY(N-2)
  400 CONTINUE
      GO TO 5000
   70 WRITE(6,109) X
  109 FORMAT(1H ,'THE VALUE OF X IS INVALID',5X,'X=',D15.7)
      NMAX = 1
      ADY(1) = 0.D0
 5000 NMAX = NMAX - 1
      RETURN
      END


      SUBROUTINE DJJ(X, MM, ADJ, NMAX)
**********************************************************************
*     Subroutine Name  DJJ                                           *
*         Bessel Functions of the first kind   (Array Type)          *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*       X...REAL*8                                                   *
*                                                                    *
*  ===  output ===                                                   *
*       NMAX...maximum N for given X  satisfying  |Jn(X)|>1.0E-78    *
*       ADJ(N) (N=1, NMAX)...Bessel functions of the first kind      *
*         ADJ(1) = J0(X)                                             *
*         ADJ(2) = J1(X)                                             *
*         ADJ(3) = J2(X)                                             *
*         ..........                                                 *
*         ..........                                                 *
*                                                                    *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 ( A-H, O-Z )
       REAL*4 XS
       REAL*8 ADJ(MM)
      IF( X ) 90, 80, 90
   90 XD = DABS(X)
      XS = XD
*
      IF(XS .LE. 0.1) THEN
       NMAX = -176.50 / ALOG(0.0425 * XS)
       M = NMAX + 5
        IF(XS .LE. 0.001) THEN
         T1 = 1.0D0
         T2 = 0.25D0 * X * X
         T3 = 0.50D0 * T2 * T2
         W1 = 0.0D0
         W2 = 1.0D0
         DO 500 N = 1, NMAX
          W1 = W1 + 1.0D0
          W2 = W2 + 1.0D0
          ADJ(N) = T1 * (1.0D0 - (T2 - T3 / W2) / W1)
          T1 = 0.50D0 * X * T1 / W1
  500    CONTINUE
         GO TO 5000
        ELSE
         GO TO 333
        END IF
      END IF
*
      IF( XS .LE. 1.0) THEN
       NMAX = XS * (-9.84 * XS + 28.40) + 29.2
       M = NMAX + 6
       GO TO 333
      END IF
      IF( XS .LE. 10.0) THEN
       NMAX = XS * (-0.26 * XS + 7.54) + 42.0
       M = NMAX + 10
       GO TO 333
      END IF
      IF( XS .LE. 20.0) THEN
       NMAX = 2.63 * XS + 66.0
       M = NMAX + 10
       GO TO 333
      END IF
      IF( XS .LE. 30.0) THEN
       NMAX = 2.17 * XS + 75.5
       M = NMAX + 12
       GO TO 333
      END IF
      IF( XS .LE. 50.0) THEN
       NMAX = 1.91 * XS + 83.2
       M = NMAX + 13
       GO TO 333
      END IF
      IF( XS .LE. 100.0) THEN
       NMAX = 1.58 * XS + 98.3
       M = NMAX + 15
       GO TO 333
      END IF
      IF( XS .LE. 173.0) THEN
       NMAX = 1.40 * XS + 117.5
       M = NMAX + 17
       GO TO 333
      ELSE
       GO TO 70
      END IF
*
*   recurrence relation  Jn-1(x) = 2*n/x*Jn(x) - Jn+1(x)
*
  333 IF( (NMAX / 2) * 2 .NE. NMAX) NMAX = NMAX - 1
      Z = 2.0D0 / XD
      T3 = 0.0D0
      T2 = 1.0D-75
      DO 100 N = M + 1, NMAX + 1, -1
       T1 = (Z * DFLOAT(N)) * T2 - T3
       T3 = T2
       T2 = T1
  100 CONTINUE
      S = 0.0D0
      ADJ(NMAX + 2) = 1.0D-78
      ADJ(NMAX + 1) = T2 / T3 * 1.D-78
      DO 200 N = NMAX,  2, -2
       ADJ(N) = (Z * DFLOAT(N)) * ADJ(N + 1) - ADJ(N + 2)
       ADJ(N-1) = (Z * DFLOAT(N-1)) * ADJ(N) - ADJ(N + 1)
       S = S + ADJ(N - 1)
  200 CONTINUE
      S = 2.D0 * S - ADJ(1)
      S = 1.0D0 / S
      DO 300 N = 1, NMAX
       ADJ(N) = S * ADJ(N)
  300 CONTINUE
*
      IF(X .GT. 0.0D0) GO TO 5000
      DO 400 N = 2, NMAX, 2
       ADJ(N) = -ADJ(N)
  400 CONTINUE
      GO TO 5000
   80 ADJ(1) = 1.0D0
      NMAX = 1
      GO TO 5000
   70 WRITE(6,109) X
  109 FORMAT(1H ,'(SUBR.DJJ) THE VALUE OF X IS INVALID',5X,'X=',D15.7)
      NMAX = 1
      ADJ(1) = 0.D0
 5000 NMAX = NMAX - 1
      RETURN
      END












      SUBROUTINE CDYY(Z,DRIY,NMAX)
**********************************************************************
*     Subroutine Name  CDYY                                          *
*        Bessel Functions of the second kind with Complex Variable   *
*           (Array Type)                                             *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*         Z...COMPLEX*16                                             *
*                                                                    *
*  ===  output ===                                                   *
*         NMAX...maximum N for given Z  satisfying  |Yn(Z)|<1.0E+75  *
*         CDY(N) (N=1, NMAX)...Bessel functions of the second kind   *
*                                                                    *
*           DRIY(1) = Re(Y0(Z))    DRIY(2) = Im(Y0(Z))               *
*           DRIY(3) = Re(Y1(Z))    DRIY(4) = Im(Y1(Z))               *
*           DRIY(5) = Re(Y2(Z))    DRIY(6) = Im(Y2(Z))               *
*           .............          ............                      *
*           .............          ............                      *
*                                                                    *
*                                                                    *
*  ===  notes on argument  ===                                       *
*       Although the dummy argument DRIY is a real array , one can   *
*      call this SUBROUTINE subprogram  which has an actual argument *
*      being declared as a complex array. (See the test program)     *
*                                                                    *
*  ===  note on external routines ===                                *
*      Subroutines DJJ,DII,DYY,DKK,CDJJ,CDII and CDKK are referred   *
*      in this subroutine subprogram CDYY.                           *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 ( A-H, O-Z )
       COMPLEX*16 Z,Z1,Z2
       REAL*4 XS,YS,ABSZ
       DIMENSION DRIY(1),DRIJ(800),ADI(400),ADK(400),ADJ(400),ADY(400)
       EQUIVALENCE (DRIJ(1),ADI(1)), (DRIJ(401),ADK(1))
       EQUIVALENCE (DRIJ(1),ADJ(1)), (DRIJ(401),ADY(1))
       REAL*8 R0(10), S0(10), R1(10), S1(10)
*
       DATA R0/ 0.75940584281266233D-11,  0.61511873267825649D-09,
     $          0.39367598891408415D-07,  0.19290123456790123D-05,
     $          0.69444444444444444D-04,  0.17361111111111111D-02,
     $          0.27777777777777778D-01,  0.25000000000000000D+00,
     $          0.10000000000000000D+01,  0.10000000000000000D+01/
       DATA S0/ 0.22242756054762939D-12,  0.21483350211950277D-10,
     $          0.16718048413148328D-08,  0.10207455998272325D-06,
     $          0.47260802469135802D-05,  0.15856481481481481D-03,
     $          0.36168981481481481D-02,  0.50925925925925926D-01,
     $          0.37500000000000000D+00,  0.10000000000000000D+01/
       DATA R1/ 0.75940584281266233D-12,  0.68346525853139610D-10,
     $          0.49209498614260519D-08,  0.27557319223985891D-06,
     $          0.11574074074074074D-04,  0.34722222222222222D-03,
     $          0.69444444444444444D-02,  0.83333333333333333D-01,
     $          0.50000000000000000D+00,  0.10000000000000000D+01/
       DATA S1/ 0.28537989410459969D-11,  0.24241319368069914D-09,
     $          0.16291859005506965D-07,  0.83852985638699924D-06,
     $          0.31635802469135802D-04,  0.82175925925925926D-03,
     $          0.13310185185185185D-01,  0.11111111111111111D+00,
     $          0.25000000000000000D+00, -0.10000000000000000D+01/
       DATA PI/ 0.31415926535897932D+01/
       DATA P2/ 0.63661977236758134D+00/
       DATA GAM/0.57721566490153286D+00/
*

      X = DREAL(Z)
      Y = DIMAG(Z)
      XD = DABS(X)
      YD = DABS(Y)
      XS = XD
      YS = YD
      ABSZ = SQRT(XS*XS + YS*YS)
      IF(ABSZ .EQ. 0.0 .OR. ABSZ .GT. 170.0) GO TO 456
      IF(Y .EQ. 0.0D0) GO TO 1000
      IF(X .EQ. 0.0D0) GO TO 2000
      ABS2 = X*X + Y*Y
      S = X / ABS2
      T = -Y / ABS2
      IF(ABSZ .GT. 1.0) GO TO 1111
      IF(ABSZ .GT. 0.1) GO TO 20
      NMAX = -174.0 / ALOG(0.040 * ABSZ)
      GO TO 30
   20 NMAX = ABSZ * (-10.98*ABSZ+29.90)+30.78
   30 TH = DATAN(Y / X)
      IF(X .GT. 0.0D0) GO TO 10
      IF(TH .GT. 0.0D0) TH = TH - PI
      IF(TH .LT. 0.0D0) TH = TH + PI
   10 DL = 0.5D0 * DLOG(0.25D0 * ABS2) + GAM
      XR = 0.25D0 * (Y*Y - X*X)
      XI = -0.5D0 * X * Y
*
*        power series expansion    ---  |Z| < 1
*
      A0 = R0(1)
      B0 = 0.0D0
      C0 = S0(1)
      D0 = 0.0D0
      E0 = R1(1)
      F0 = 0.0D0
      G0 = S1(1)
      H0 = 0.0D0
      DO 50 I = 2, 10
       A1 = XR*A0 - XI*B0 + R0(I)
       B1 = XR*B0 + XI*A0
       A0 = A1
       B0 = B1
       C1 = XR*C0 - XI*D0 + S0(I)
       D1 = XR*D0 + XI*C0
       C0 = C1
       D0 = D1
       E1 = XR*E0 - XI*F0 + R1(I)
       F1 = XR*F0 + XI*E0
       E0 = E1
       F0 = F1
       G1 = XR*G0 - XI*H0 + S1(I)
       H1 = XR*H0 + XI*G0
       G0 = G1
       H0 = H1
   50 CONTINUE
      C1 = XR * C0 - XI * D0
      D1 = XR * D0 + XI * C0
      E1 = 0.5D0 * (X * E0 - Y * F0)
      F1 = 0.5D0 * (X * F0 + Y * E0)
      G1 = 0.25D0 * (X * G0 - Y * H0)
      H1 = 0.25D0 * (X * H0 + Y * G0)
      DRIY(1) = P2 * (DL * A0 - TH * B0 - C1)
      DRIY(2) = P2 * (DL * B0 + TH * A0 - D1)
      DL = DL - 1.0D0
      DRIY(3) = P2 * (-S + DL * E1 - TH * F1 - G1)
      DRIY(4) = P2 * (-T + DL * F1 + TH * E1 - H1)
      DO 500 N = 3, NMAX
      FN = 2 * N - 4
       DRIY(2*N-1) = FN * (S * DRIY(2*N-3) - T * DRIY(2*N-2))
     $              -DRIY(2*N-5)
       DRIY(2*N)   = FN * (S * DRIY(2*N-2) + T * DRIY(2*N-3))
     $              -DRIY(2*N-4)
  500 CONTINUE
      GO TO 5000
 1000 IF(X .LT. 0.0D0) GO TO 1500
*
*       positive real axis
*
      CALL DYY(XD, ADY, NMAX)
      DO 300 N = 1, NMAX
       DRIY(2*N-1) = ADY(N)
       DRIY(2*N)   = 0.0D0
  300 CONTINUE
      GO TO 5000
*
*       negative real axis
*
 1500 CONTINUE
      CALL DJJ(XD, ADJ, N1)
      CALL DYY(XD, ADY, N2)
      NMAX = MIN0(N1, N2)
      DO 350 N = 1, NMAX, 2
       DRIY(2*N-1) =  ADY(N)
       DRIY(2*N)   =  2.0D0 * ADJ(N)
       DRIY(2*N+1) = -ADY(N+1)
       DRIY(2*N+2) = -2.0D0 * ADJ(N+1)
  350 CONTINUE
      GO TO 5000
*
*       imaginay axis
*
 2000 CONTINUE
      CALL DII(YD, ADI, N1)
      CALL DKK(YD, ADK, N2)
      NMAX = MIN0(N1, N2)
      DO 400 N = 1, NMAX, 4
       DRIY(2*N-1) = -P2*ADK(N)
       DRIY(2*N)   =  ADI(N)
       DRIY(2*N+1) = -ADI(N+1)
       DRIY(2*N+2) =  P2*ADK(N+1)
       DRIY(2*N+3) =  P2*ADK(N+2)
       DRIY(2*N+4) = -ADI(N+2)
       DRIY(2*N+5) =  ADI(N+3)
       DRIY(2*N+6) = -P2*ADK(N+3)
  400 CONTINUE
      IF(Y .GT. 0.0D0) GO TO 5000
  444 CONTINUE
      DO 450 N = 1, NMAX
       DRIY(2*N) = -DRIY(2*N)
  450 CONTINUE
      GO TO 5000
 1111 Z1 = DCMPLX(X, YD)
      Z2 = DCMPLX(YD, -X)
      CALL CDJJ(Z1, DRIJ, N1)
      CALL CDKK(Z2, DRIY, N2)
      NMAX = MIN0(N1, N2)
      DO 200 N = 1, NMAX, 4
       DRIY(2*N-1) = -DRIJ(2*N)   - P2 * DRIY(2*N-1)
       DRIY(2*N)   =  DRIJ(2*N-1) - P2 * DRIY(2*N)
       DD = DRIY(2*N+1)
       DRIY(2*N+1) = -DRIJ(2*N+2) - P2 * DRIY(2*N+2)
       DRIY(2*N+2) =  DRIJ(2*N+1) + P2 * DD
       DRIY(2*N+3) = -DRIJ(2*N+4) + P2 * DRIY(2*N+3)
       DRIY(2*N+4) =  DRIJ(2*N+3) + P2 * DRIY(2*N+4)
       DD = DRIY(2*N+5)
       DRIY(2*N+5) = -DRIJ(2*N+6) + P2 * DRIY(2*N+6)
       DRIY(2*N+6) =  DRIJ(2*N+5) - P2 * DD
  200 CONTINUE
      IF(Y .GT. 0.0D0) GO TO 5000
      GO TO 444
  456 WRITE(6,101) Z,ABSZ
  101 FORMAT(1H0,'(SUBR.CDYY) Z=',2D15.7,5X,'ABSZ=',E15.7/1H ,
     $ 'ABSZ GREATER THAN 170.0     RETURN WITHOUT CALCULATION')
      NMAX = 1
      DRIY(1)=0.D0
      DRIY(2)=0.D0
 5000 NMAX = NMAX - 1

      RETURN
      END



***********
      SUBROUTINE CDII(Z, DRII, NMAX)
**********************************************************************
*     Subroutine Name  CDII                                          *
*       Modified Bessel Functions of the First Kind                  *
*            with Complex Variable   (Array Type)                    *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*         Z...COMPLEX*16                                             *
*                                                                    *
*  ===  output ===                                                   *
*         NMAX...maximum N for given Z  satisfying  |Jn(Z)|>1.0E-78  *
*         CDI(N) (N=1, NMAX)...Modified Bessel functions             *
*                                                                    *
*           DRII(1) = Re(I0(Z))    DRII(2) = Im(I0(Z))               *
*           DRII(3) = Re(I1(Z))    DRII(4) = Im(I1(Z))               *
*           DRII(5) = Re(I2(Z))    DRII(6) = Im(I2(Z))               *
*           .............          ............                      *
*           .............          ............                      *
*                                                                    *
*  ===  notes on argument  ===                                       *
*       Although the dummy argument DRII is a real array , one can   *
*      call this SUBROUTINE subprogram  which has an actual argument *
*      being declared as a complex array. (See the test program)     *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 ( A-H, O-Z)
       COMPLEX*16 Z
       REAL*8 DRII(1)
       REAL*4 XS,YS,ABSZ
      X = DREAL(Z)
      Y = DIMAG(Z)
      JQ = 1
      IF(X .LT. 0.0D0 .AND. Y .GE. 0.0D0) JQ = 2
      IF(X .LT. 0.0D0 .AND. Y .LT. 0.0D0) JQ = 3
      IF(X .GE. 0.0D0 .AND. Y .LT. 0.0D0) JQ = 4
      XD = DABS(X)
      YD = DABS(Y)
      XS = XD
      YS = YD
      ABSZ = SQRT(XS**2 + YS**2)
*
      IF(ABSZ .GT. 170.0) GO TO 456
      IF(ABSZ .EQ. 0.0) GO TO 345
      IF(ABSZ .LE. 0.1) THEN
       NMAX = -174.0 / ALOG(0.040 * ABSZ)
       M = NMAX + 6
       GO TO 123
      END IF
      IF(ABSZ .LE. 1.0) THEN
       NMAX = ABSZ * (-10.98 * ABSZ + 29.90) + 30.78
       M = NMAX + 8
       GO TO 123
      END IF
      IF(ABSZ .LE. 10.0) THEN
       NMAX = ABSZ * (-0.27 * ABSZ + 7.54) + 43.0
       M = NMAX + 10
       GO TO 123
      END IF
      IF(ABSZ .LE. 20.0) THEN
       NMAX = ABSZ*(-0.035 * ABSZ+3.68)+59.0
       M = NMAX + 11
       GO TO 123
      END IF
      IF(ABSZ .LE. 30.0) THEN
       NMAX = ABSZ * (-0.0338 * ABSZ + 3.86) + 55.3
       M = NMAX + 13
       GO TO 123
      END IF
      IF(ABSZ .LE. 50.0) THEN
       NMAX = ABSZ * 1.91 + 84.0
       M = NMAX + 14
       GO TO 123
      END IF
      IF(ABSZ .LE. 100.0) THEN
       NMAX = ABSZ * (-0.00281 * ABSZ + 2.09) + 80.0
       M = NMAX + 16
       GO TO 123
      END IF
      NMAX = ABSZ * (-0.000992 * ABSZ + 1.67) + 100.0
      M = NMAX + 18
*
*    recurrence relation  In-1(z) = 2*n/z*In(z) + In+1(z)
*
  123 CONTINUE
      SD =  2.0D0 * XD / (XD**2 + YD**2)
      TD = -2.0D0 * YD / (XD**2 + YD**2)
      IF( (NMAX/2)*2 .NE. NMAX) NMAX = NMAX - 1
      T3 = 0.0D0
      T2 = 1.0D-70
      S3 = 0.0D0
      S2 = 1.0D-70
      DO 100 K = M, NMAX, -1
       T1 = (DFLOAT(K + 1) * (SD * T2 - TD * S2)) + T3
       S1 = (DFLOAT(K + 1) * (SD * S2 + TD * T2)) + S3
       T3 = T2
       S3 = S2
       T2 = T1
       S2 = S1
  100 CONTINUE
      REN1 = DMIN1(DABS(T2), DABS(T3), DABS(S2), DABS(S3))
      REN2 = DMAX1(DABS(T2), DABS(T3), DABS(S2), DABS(S3))
      REN = 0.5D0*(REN1 + REN2)
      DRII(2 * NMAX + 1) = T2 / REN * 1.0D-70
      DRII(2 * NMAX + 2) = S2 / REN * 1.0D-70
      DRII(2 * NMAX + 3) = T3 / REN * 1.0D-70
      DRII(2 * NMAX + 4) = S3 / REN * 1.0D-70
      W1 = 0.0D0
      W2 = 0.0D0
      DO 200 K = NMAX - 1, 0, -1
       DRII(2*K+1) = (DFLOAT(K+1) * (SD * DRII(2*K+3)
     $                             - TD * DRII(2*K+4))) + DRII(2*K+5)
       DRII(2*K+2) = (DFLOAT(K+1) * (SD * DRII(2*K+4)
     $                             + TD * DRII(2*K+3))) + DRII(2*K+6)
       W1 = W1 + DRII(2 * K + 1)
       W2 = W2 + DRII(2 * K + 2)
  200 CONTINUE
      W1 = W1 + W1 - DRII(1)
      W2 = W2 + W2 - DRII(2)
      S1 = ( W1 * DCOS(YD) + W2 * DSIN(YD))
      S2 = (-W1 * DSIN(YD) + W2 * DCOS(YD))
      IF(DABS(W1) .GT. DABS(W2)) THEN
       T3 = W1 * (1.0D0 + (W2 / W1)**2)
       T3 = 1.0D0 / T3
       T1 = (S1 / W1) * T3
       T2 =-(S2 / W1) * T3
      ELSE
       T3 = W2 * (1.0D0 + (W1/W2)**2)
       T3 = 1.0D0 / T3
       T1 =  (S1 / W2) * T3
       T2 = -(S2 / W2) * T3
      END IF
      EX = DEXP(XD)
      T1 = T1 * EX
      T2 = T2 * EX
      DO 300 I = 1, NMAX
       S1 = T1 * DRII(2 * I - 1) - T2 * DRII(2 * I)
       S2 = T1 * DRII(2 * I) + T2 * DRII(2 * I - 1)
       DRII(2 * I - 1) = S1
       DRII(2 * I) = S2
  300 CONTINUE
      IF(X) 310,330,310
  330 N2 = NMAX * 2
      DO 340 I = 2, N2, 4
       DRII(I) = 0.0D0
       DRII(I + 1) = 0.0D0
  340 CONTINUE
  310 GO TO (900, 500, 500, 600), JQ
  500 DO 510 N = 2, NMAX, 2
       DRII(2 * N - 1) = -DRII(2 * N - 1)
       DRII(2 * N) = -DRII(2 * N)
  510 CONTINUE
      GO TO (900, 600, 900, 600), JQ
  600 DO 610 N = 1, NMAX
       DRII(2 * N)= -DRII(2 * N)
  610 CONTINUE
  900 NMAX = NMAX - 1
      RETURN
  345 NMAX=0
      DRII(1) = 1.0D0
      DRII(2) = 0.0D0
      RETURN
  456 WRITE(6,202) Z,ABSZ
  202 FORMAT(1H0,'(SUBR.CDJJ)  Z=',2D15.7,5X,'ABSZ=',E15.7
     $   /1H ,'ABSZ GREATER THAN 170.0    RETURN WITHOUT CALCULATION')
      DRII(1) = 0.D0
      DRII(2) = 0.D0
      NMAX=0
      RETURN
      END

*********
      SUBROUTINE CDJJ(Z, DRIJ, NMAX)
**********************************************************************
*     Subroutine Name  CDJJ                                          *
*        Bessel Functions of the first kind with Complex Variable    *
*           (Array Type)                                             *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*         Z...COMPLEX*16                                             *
*                                                                    *
*  ===  output ===                                                   *
*         NMAX...maximum N for given Z  satisfying  |Jn(Z)|>1.0E-78  *
*         CDJ(N) (N=1, NMAX)...Bessel functions                      *
*                                                                    *
*           DRIJ(1) = Re(J0(Z))    DRIJ(2) = Im(J0(Z))               *
*           DRIJ(3) = Re(J1(Z))    DRIJ(4) = Im(J1(Z))               *
*           DRIJ(5) = Re(J2(Z))    DRIJ(6) = Im(J2(Z))               *
*           .............          ............                      *
*           .............          ............                      *
*                                                                    *
*  ===  notes on argument  ===                                       *
*       Although the dummy argument DRIJ is a real array , one can   *
*      call this SUBROUTINE subprogram  which has an actual argument *
*      being declared as a complex array. (See the test program)     *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 (A-H , O-Z)
       COMPLEX*16 Z
       DIMENSION DRIJ(1)
       REAL*4 XS,YS,ABSZ
      X=DREAL(Z)
      Y=DIMAG(Z)
      JQ = 1
      IF(X .LT. 0.0D0 .AND. Y .GE. 0.0D0) JQ = 2
      IF(X .LT. 0.0D0 .AND. Y .LT. 0.0D0) JQ = 3
      IF(X .GE. 0.0D0 .AND. Y .LT. 0.0D0) JQ = 4
      XD = DABS(X)
      YD = DABS(Y)
      XS = XD
      YS = YD
      ABSZ = SQRT(XS**2 + YS**2)
*
      IF(ABSZ .GT. 170.0) GO TO 456
      IF(ABSZ .EQ. 0.0) GO TO 345
      IF(ABSZ .LE. 0.1) THEN
       NMAX = -174.0 / ALOG(0.0400 * ABSZ)
       M = NMAX + 6
       GO TO 123
      END IF
      IF(ABSZ .LE. 1.0) THEN
       NMAX = ABSZ * (-10.98 * ABSZ + 29.90) + 30.78
       M = NMAX + 8
       GO TO 123
      END IF
      IF(ABSZ .LE. 10.0) THEN
       NMAX = ABSZ * (-0.27 * ABSZ + 7.54) + 43.0
       M = NMAX + 10
       GO TO 123
      END IF
      IF(ABSZ .LE. 20.0) THEN
       NMAX = ABSZ * (-0.035 * ABSZ + 3.68) + 59.0
       M = NMAX + 11
       GO TO 123
      END IF
      IF(ABSZ .LE. 30.0) THEN
       NMAX = ABSZ * (-0.034 * ABSZ + 3.86) + 55.3
       M = NMAX + 13
       GO TO 123
      END IF
      IF(ABSZ .LE. 50.0) THEN
       NMAX = ABSZ * 1.91 + 84.0
       M = NMAX + 14
       GO TO 123
      END IF
      IF(ABSZ .LE. 100.0) THEN
       NMAX = ABSZ * (-0.00281 * ABSZ + 2.09) + 85.0
       M = NMAX + 16
       GO TO 123
      END IF
      NMAX = ABSZ * (-0.000992 * ABSZ + 1.67) + 101.0
      M = NMAX + 18
*
*     recurrence relation   Jn-1(z) = 2*n/z*Jn(z) -Jn+1(z)
*
  123 CONTINUE
      SD =  2.0D0 * XD / (XD**2 + YD**2)
      TD = -2.0D0 * YD / (XD**2 + YD**2)
      IF((NMAX/2)*2 .NE. NMAX) NMAX = NMAX - 1
      T3 = 0.0D0
      S3 = 0.0D0
      T2 = 1.0D-70
      S2 = 1.0D-70
      DO 100 K = M, NMAX, -1
       T1 = (DFLOAT(K + 1) * (SD * T2 - TD * S2)) - T3
       S1 = (DFLOAT(K + 1) * (SD * S2 + TD * T2)) - S3
       T3 = T2
       S3 = S2
       T2 = T1
       S2 = S1
  100 CONTINUE
      REN1 = DMIN1(DABS(T2), DABS(T3), DABS(S2), DABS(S3))
      REN2 = DMAX1(DABS(T2), DABS(T3), DABS(S2), DABS(S3))
      REN = 0.5D0 * (REN1 + REN2)
      DRIJ(2 * NMAX + 1) = T2 / REN * 1.0D-70
      DRIJ(2 * NMAX + 2) = S2 / REN * 1.0D-70
      DRIJ(2 * NMAX + 3) = T3 / REN * 1.0D-70
      DRIJ(2 * NMAX + 4) = S3 / REN * 1.0D-70
      W1 = 0.0D0
      W2 = 0.0D0
      II = NMAX / 2
      FUGO = -1.0D0
      IF((II/2)*2 .NE. II) FUGO = 1.0D0
      DO 200 K = NMAX-1, 1, -2
       DRIJ(2*K+1) = (DFLOAT(K+1) * (SD * DRIJ(2*K+3)
     $                             - TD * DRIJ(2*K+4))) - DRIJ(2*K+5)
       DRIJ(2*K+2) = (DFLOAT(K+1) * (SD * DRIJ(2*K+4)
     $                             + TD * DRIJ(2*K+3))) - DRIJ(2*K+6)
       W1 = W1 + FUGO * DRIJ(2*K+2)
       W2 = W2 - FUGO * DRIJ(2*K+1)
       DRIJ(2*K-1) = (DFLOAT(K) * (SD * DRIJ(2*K+1)
     $                           - TD * DRIJ(2*K+2))) - DRIJ(2*K+3)
       DRIJ(2*K)   = (DFLOAT(K) * (SD * DRIJ(2*K+2)
     $                           + TD * DRIJ(2*K+1))) - DRIJ(2*K+4)
       W1 = W1 + FUGO * DRIJ(2*K-1)
       W2 = W2 + FUGO * DRIJ(2*K)
       FUGO = -FUGO
  200 CONTINUE
      W1 = W1 + W1 - DRIJ(1)
      W2 = W2 + W2 - DRIJ(2)
      S1 = (W1 * DCOS(XD) - W2 * DSIN(XD))
      S2 = (W1 * DSIN(XD) + W2 * DCOS(XD))
      IF(DABS(W1) .GT. DABS(W2)) THEN
       T3 = W1 * (1.0D0 + (W2/W1)**2)
       T3 = 1.0D0 / T3
       T1 =  (S1 / W1) * T3
       T2 = -(S2 / W1) * T3
      ELSE
       T3 = W2 * (1.0D0 + (W1/W2)**2)
       T3 = 1.0D0 / T3
       T1 =  (S1 / W2) * T3
       T2 = -(S2 / W2) * T3
      END IF
      EX = DEXP(YD)
      T1 = T1 * EX
      T2 = T2 * EX
      DO 300 I = 1, NMAX
       S1 = T1 * DRIJ(2 * I - 1) -T2 * DRIJ(2 * I)
       S2 = T1 * DRIJ(2 * I) + T2 * DRIJ(2 * I - 1)
       DRIJ(2 * I - 1) = S1
       DRIJ(2 * I) = S2
  300 CONTINUE
      IF(Y) 310, 330, 310
  330 DO 340 I = 1, NMAX
       DRIJ(2 * I) = 0.0D0
  340 CONTINUE
  310 GO TO (900, 500, 500, 600), JQ
  500 DO 510 N = 2, NMAX, 2
       DRIJ(2 * N - 1) = -DRIJ(2 * N - 1)
       DRIJ(2 * N) = -DRIJ(2 * N)
  510 CONTINUE
      GO TO (900, 600, 900, 600), JQ
  600 DO 610 N = 1, NMAX
       DRIJ(2 * N) = -DRIJ(2 * N)
  610 CONTINUE
  900 NMAX = NMAX - 1
      RETURN
  345 NMAX = 0
      DRIJ(1) = 1.0D0
      DRIJ(2) = 0.0D0
      RETURN
  456 WRITE(6,202) Z,ABSZ
  202 FORMAT(1H0,'(SUBR.CDJJ)  Z=',2D15.7,5X,'ABSZ=',E15.7
     $   /1H ,'ABSZ GREATER THAN 170.0   RETURN WITHOUT CALCULATION')
      DRIJ(1) = 0.D0
      DRIJ(2) = 0.D0
      NMAX=0
      RETURN
      END

*********
      SUBROUTINE CDKK(Z, DRIK, NMAX)
**********************************************************************
*     Subroutine Name  CDKK                                          *
*         Modified Bessel Functions of the Second Kind               *
*            with Complex Variable  (Array Type)                     *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*         Z...COMPLEX*16                                             *
*                                                                    *
*  ===  output ===                                                   *
*         NMAX...maximum N for given Z  satisfying   |Kn(Z)|<1.0E+75 *
*         CDJ(N) (N=1, NMAX)...Bessel functions                      *
*                                                                    *
*           DRIK(1) = Re(K0(Z))    DRIK(2) = Im(K0(Z))               *
*           DRIK(3) = Re(K1(Z))    DRIK(4) = Im(K1(Z))               *
*           DRIK(5) = Re(K2(Z))    DRIK(6) = Im(K2(Z))               *
*           .............          ............                      *
*           .............          ............                      *
*                                                                    *
*                                                                    *
*  ===  notes on argument  ===                                       *
*       Although the dummy argument DRIK is a real array , one can   *
*      call this SUBROUTINE subprogram  which has an actual argument *
*      being declared as a complex array. (See the test program)     *
*                                                                    *
*  ===  note on external routines ===                                *
*      Subroutines DJJ,DII,DYY,DKK and CDII are referred in this     *
*      subroutine subprogram CDKK.                                   *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 ( A-H, O-Z )
       COMPLEX*16 Z, ZZ
       REAL*4 XS, YS, ABSZ
       REAL*8 DRIK(1),DRII(800),ADI(400),ADK(400),ADJ(400),ADY(400)
       EQUIVALENCE (DRII(1),ADI(1)), (DRII(401),ADK(1))
       EQUIVALENCE (DRII(1),ADJ(1)), (DRII(401),ADY(1))
       REAL*8 TT2(19), W0(19), W1(19), HG2(19), HGW(19)
       REAL*8 R0(10),  S0(10), R1(10), S1(10)
       DATA TT2/ 0.50816303094955846D+02,  0.40181837283931421D+02,
     $           0.31708614169687260D+02,  0.24958034076222436D+02,
     $           0.19580666544561472D+02,  0.15298162754506354D+02,
     $           0.11888847347168759D+02,  0.91762438374046260D+01,
     $           0.70199396567662448D+01,  0.53083179739444593D+01,
     $           0.39527800697270042D+01,  0.28831591991125255D+01,
     $           0.20440885482015186D+01,  0.13921352803190245D+01,
     $           0.89355232092143692D+00,  0.52253150686907060D+00,
     $           0.25986767778329635D+00,  0.91964558813223533D-01,
     $           0.10130976294839900D-01/
       DATA W0/  0.14218868556399025D-21,  0.52928400912154829D-17,
     $           0.22714546957181119D-13,  0.17429758067817779D-10,
     $           0.33925627129335466D-08,  0.22133366350879712D-06,
     $           0.60436959864401697D-05,  0.82442796769693937D-04,
     $           0.64689844467092127D-03,  0.32668591886986535D-02,
     $           0.11610849355475341D-01,  0.31182218709688200D-01,
     $           0.66948999642992045D-01,  0.12014580707255842D+00,
     $           0.18661317032220902D+00,  0.25770930676459221D+00,
     $           0.32288745265914786D+00,  0.37237295940447920D+00,
     $           0.39898061810456932D+00/
       DATA W1/  0.72255033422931014D-20,  0.21267603931508927D-15,
     $           0.72024680550449987D-12,  0.43501249579690905D-09,
     $           0.66428639213464601D-07,  0.33859984074087223D-05,
     $           0.71852578995483689D-04,  0.75651520579630600D-03,
     $           0.45411880456458046D-02,  0.17341527349714676D-01,
     $           0.45895133924925560D-01,  0.89903300721576239D-01,
     $           0.13684968348378760D+00,  0.16725921680811156D+00,
     $           0.16674863145591728D+00,  0.13466123239788594D+00,
     $           0.83908012507896790D-01,  0.34245114925607327D-01,
     $           0.40420631841179627D-02/
       DATA HG2/ 0.25485979166099077D+02,  0.18046505467728980D+02,
     $           0.12771825354869194D+02,  0.87697567302686021D+01,
     $           0.56944233429577552D+01,  0.33691762702432690D+01,
     $           0.16923950797931789D+01,  0.60323635708174870D+00,
     $           0.66702230958194404D-01,  0.15129959781108085D+02,
     $           0.91242480375311789D+01,  0.51961525300544656D+01,
     $           0.25525898026681713D+01,  0.89830283456961770D+00,
     $           0.98747014068481182D-01,  0.85886356890120343D+01,
     $           0.39269635013582872D+01,  0.13390972881263614D+01,
     $           0.14530352150331709D+00/
       DATA HGW/ 0.78281997721158910D-11,  0.10467205795792082D-07,
     $           0.18106544810934304D-05,  0.91811268679294035D-04,
     $           0.18885226302684179D-02,  0.18640042387544652D-01,
     $           0.97301747641315429D-01,  0.28480728566997958D+00,
     $           0.48349569472545555D+00,  0.26585516843563016D-06,
     $           0.85736870435878587D-04,  0.39053905846290619D-02,
     $           0.51607985615883930D-01,  0.26049231026416113D+00,
     $           0.57013523626247958D+00,  0.19960407221136762D-03,
     $           0.17077983007413475D-01,  0.20780232581489188D+00,
     $           0.66114701255824129D+00/
       DATA R0/  0.75940584281266233D-11,  0.61511873267825649D-09,
     $           0.39367598891408415D-07,  0.19290123456790123D-05,
     $           0.69444444444444444D-04,  0.17361111111111111D-02,
     $           0.27777777777777778D-01,  0.25000000000000000D+00,
     $           0.10000000000000000D+01,  0.10000000000000000D+01/
       DATA S0/  0.22242756054762939D-12,  0.21483350211950277D-10,
     $           0.16718048413148328D-08,  0.10207455998272325D-06,
     $           0.47260802469135802D-05,  0.15856481481481481D-03,
     $           0.36168981481481481D-02,  0.50925925925925926D-01,
     $           0.37500000000000000D+00,  0.10000000000000000D+01/
       DATA R1/  0.75940584281266233D-12,  0.68346525853139610D-10,
     $           0.49209498614260519D-08,  0.27557319223985891D-06,
     $           0.11574074074074074D-04,  0.34722222222222222D-03,
     $           0.69444444444444444D-02,  0.83333333333333333D-01,
     $           0.50000000000000000D+00,  0.10000000000000000D+01/
       DATA S1/  0.28537989410459969D-11,  0.24241319368069914D-09,
     $           0.16291859005506965D-07,  0.83852985638699924D-06,
     $           0.31635802469135802D-04,  0.82175925925925926D-03,
     $           0.13310185185185185D-01,  0.11111111111111111D+00,
     $           0.25000000000000000D+00, -0.10000000000000000D+01/
       DATA PI / 0.31415926535897932D+01/
       DATA PI2/ 0.15707963267948966D+01/
       DATA GAM/ 0.57721566490153286D+00/
*
      X = DREAL(Z)
      Y = DIMAG(Z)
      XD = DABS(X)
      YD = DABS(Y)
      XS = XD
      YS = YD
      ABSZ = SQRT(XS*XS + YS*YS)
      IF(ABSZ .EQ. 0.0. OR. ABSZ .GT. 170.0) GO TO 456
      IF(Y .EQ. 0.0D0) GO TO 1000
      IF(X .EQ. 0.0D0) GO TO 2000
      ABS2 = X*X + Y*Y
      S = X / ABS2
      T = -Y / ABS2
      SS = DABS(S)
      TT = -DABS(T)
      IF(ABSZ .GT. 1.0) GO TO 11
      IF(ABSZ .GT. 0.1) GO TO 20
      NMAX = -174.0 / ALOG(0.040 * ABSZ)
      GO TO 30
   20 NMAX = ABSZ * (-10.98 * ABSZ + 29.90) + 30.78
   30 TH = DATAN(Y / X)
      IF(X .GT. 0.0D0) GO TO 10
      IF(TH .GT. 0.0D0) TH = TH - PI
      IF(TH .LT. 0.0D0) TH = TH + PI
   10 DL = 0.5D0 * DLOG(0.25D0 * ABS2) + GAM
      XR = 0.25D0 *(X*X - Y*Y)
      XI = 0.5D0 * X * Y
*
*          power series expansion   ---   |Z| < 1
*
      A0 = R0(1)
      B0 = 0.0D0
      C0 = S0(1)
      D0 = 0.0D0
      E0 = R1(1)
      F0 = 0.0D0
      G0 = S1(1)
      H0 = 0.0D0
      DO 50 I = 2, 10
       A1 = XR * A0 - XI * B0 + R0(I)
       B1 = XR * B0 + XI * A0
       A0 = A1
       B0 = B1
       C1 = XR * C0 - XI * D0 + S0(I)
       D1 = XR * D0 + XI * C0
       C0 = C1
       D0 =D1
       E1 = XR * E0 - XI * F0 + R1(I)
       F1 = XR * F0 + XI * E0
       E0 = E1
       F0 = F1
       G1 = XR * G0 - XI * H0 + S1(I)
       H1 = XR * H0 + XI * G0
       G0 = G1
       H0 = H1
   50 CONTINUE
      C1 = XR * C0 - XI * D0
      D1 = XR * D0 + XI * C0
      E1 = 0.5D0 * (X * E0 - Y * F0)
      F1 = 0.5D0 * (X * F0 + Y * E0)
      G1 = 0.25D0 * (X * G0 - Y * H0)
      H1 = 0.25D0 * (X * H0 + Y * G0)
      DRIK(1) = -DL * A0 + TH * B0 + C1
      DRIK(2) = -DL * B0 - TH * A0 + D1
      DL = DL - 1.0D0
      DRIK(3) = S + DL * E1 - TH * F1 - G1
      DRIK(4) = T + DL * F1 + TH * E1 - H1
      DO 500 N = 3, NMAX
       FN = 2 * N - 4
       DRIK(2 * N - 1) = FN * (S * DRIK(2 * N - 3)
     $    - T * DRIK(2 * N - 2)) + DRIK(2 * N - 5)
       DRIK(2 * N) =     FN * (S * DRIK(2 * N - 2)
     $    + T * DRIK(2 * N - 3)) + DRIK(2 * N - 4)
  500 CONTINUE
      GO TO 5000
   11 IF((ABSZ-1.0) * (ABSZ-10.0) .GT. 0.0) GO TO 12
       NMAX = ABSZ * (-0.269 * ABSZ + 7.54) + 42.0
       GO TO 333
   12 IF((ABSZ-10.0) * (ABSZ-20.0) .GT. 0.0) GO TO 13
       NMAX = ABSZ * (-0.0350 * ABSZ + 3.68) + 55.0
       K = 1
       L = 9
       GO TO 444
   13 IF((ABSZ-20.0) * (ABSZ-30.0) .GT. 0.0) GO TO 14
       NMAX = ABSZ * (-0.0338 * ABSZ + 3.86) + 53.0
       K = 10
       L = 15
       GO TO 444
   14 IF((ABSZ- 30.0) * (ABSZ-50.0) .GT. 0.0) GO TO 15
       NMAX = ABSZ * 1.91 + 82.0
       K = 10
       L = 15
       GO TO 444
   15 IF((ABSZ-50.0) * (ABSZ-100.0) .GT. 0.0) GO TO 16
       NMAX = ABSZ * (-0.00281 * ABSZ + 2.09) + 50.0
       K=10
       L=15
       GO TO 444
   16 NMAX = ABSZ * (-0.000992 * ABSZ + 1.67) + 50.0
       K = 16
       L = 19
       GO TO 444
*
*       numerical integration for  K0(z) AND K1(z)
*               double exponential formula
*
  333 X2 = 2.0D0 * XD
      Y4 = 4.0D0 * Y * Y
      A0 = 0.0D0
      B0 = 0.0D0
      A1 = 0.0D0
      B1 = 0.0D0
      DO 100 I = 1, 19
       R = X2 + TT2(I)
       RR = DSQRT(R*R + Y4)
       A = DSQRT(0.5D0 * (R + RR))
       B = YD / A
       AB = (R + RR) / (R*R + R*RR + Y4)
       A1 = A1 + W1(I) * A
       B1 = B1 + W1(I) * B
       A0 = A0 + W0(I) * A * AB
       B0 = B0 - W0(I) * B * AB
  100 CONTINUE
      GO TO 666
*
*       numerical integration for K0(z) and K1(z)
*                Hermite-Gauss formula
*
  444 X2 = 2.0D0 * XD
      Y4 = 4.0D0 * Y * Y
      A0 = 0.0D0
      B0 = 0.0D0
      A1 = 0.0D0
      B1 = 0.0D0
      DO 200 I = K, L
       R = X2 + HG2(I)
       RR = DSQRT(R*R + Y4)
       A = DSQRT(0.5D0 * (R + RR))
       B = YD / A
       AB = (R + RR)/(R*R + R*RR + Y4)
       A1 = A1 + HGW(I) * HG2(I) * A
       B1 = B1 + HGW(I) * HG2(I) * B
       A0 = A0 + HGW(I) * A * AB
       B0 = B0 - HGW(I) * B * AB
  200 CONTINUE
      A0 = A0 + A0
      B0 = B0 + B0
      A1 = A1 + A1
      B1 = B1 + B1
  666 EE = DEXP(-XD)
      EC =  EE * DCOS(YD)
      ES = -EE * DSIN(YD)
      DRIK(1) = A0 * EC - B0 * ES
      DRIK(2) = A0 * ES + B0 * EC
      C1 = A1 * EC - B1 * ES
      C2 = A1 * ES + B1 * EC
      DRIK(3) = C1 * SS - C2 * TT
      DRIK(4) = C1 * TT + C2 * SS
      DO 700 N = 3, NMAX
       FN = 2 * N - 4
       DRIK(2*N-1) = FN * (SS * DRIK(2*N-3) - TT * DRIK(2*N-2))
     $               + DRIK(2*N-5)
       DRIK(2*N)   = FN * (SS * DRIK(2*N-2) + TT * DRIK(2*N-3))
     $               + DRIK(2*N-4)
  700 CONTINUE
      IF(X .GT. 0.0D0) GO TO 777
      ZZ = DCMPLX(XD,YD)
      CALL CDII(ZZ, DRII, NM)
      NMAX = MIN0(NMAX, NM)
      DO 600 N = 1, NMAX, 2
       DRIK(2*N-1) =  DRIK(2*N-1) -  PI * DRII(2*N)
       DRIK(2*N)   = -DRIK(2*N)   -  PI * DRII(2*N-1)
       DRIK(2*N+1) = -DRIK(2*N+1) -  PI * DRII(2*N+2)
       DRIK(2*N+2) =  DRIK(2*N+2) -  PI * DRII(2*N+1)
  600 CONTINUE
      GO TO 777
 1000 IF(X .LT. 0.0D0) GO TO 1500
*
*       positive real axis
*
      CALL DKK(XD, ADK, NMAX)
      DO 300 N = 1, NMAX
       DRIK(2*N-1) = ADK(N)
       DRIK(2*N)   = 0.0D0
  300 CONTINUE
      GO TO 5000
*
*       negative real axis
*
 1500 CONTINUE
      CALL DII(XD, ADI, N1)
      CALL DKK(XD, ADK, N2)
      NMAX = MIN0(N1, N2)
      DO 350 N = 1, NMAX, 2
       DRIK(2*N-1) =  ADK(N)
       DRIK(2*N)   = -PI * ADI(N)
       DRIK(2*N+1) = -ADK(N+1)
       DRIK(2*N+2) = -PI * ADI(N+1)
  350 CONTINUE
      GO TO 5000
*
*       imaginary axis
*
 2000 CONTINUE
      CALL DJJ(YD, ADJ, N1)
      CALL DYY(YD, ADY, N2)
      NMAX = MIN0(N1, N2)
      DO 400 N = 1, NMAX, 4
       DRIK(2*N-1) = -PI2 * ADY(N)
       DRIK(2*N)   = -PI2 * ADJ(N)
       DRIK(2*N+1) = -PI2 * ADJ(N+1)
       DRIK(2*N+2) =  PI2 * ADY(N+1)
       DRIK(2*N+3) =  PI2 * ADY(N+2)
       DRIK(2*N+4) =  PI2 * ADJ(N+2)
       DRIK(2*N+5) =  PI2 * ADJ(N+3)
       DRIK(2*N+6) = -PI2 * ADY(N+3)
  400 CONTINUE
  777 IF(Y .GT. 0.0D0) GO TO 5000
      DO 450 N = 1, NMAX
       DRIK(2*N) = -DRIK(2 * N)
  450 CONTINUE
      GO TO 5000
  456 WRITE(6,101) Z,ABSZ
  101 FORMAT(1H0,'(SUBR.CDKK) Z=',2D15.7,5X,'ABSZ=',E15.7/1H ,
     $' ABSZ GREATER THAN 170.0     RETURN WITHOUT CALCULATION')
      NMAX=1
      DRIK(1)=0.D0
      DRIK(2)=0.D0
 5000 NMAX=NMAX-1
      RETURN
      END

***************
      SUBROUTINE DII(X, ADI, NMAX)
**********************************************************************
*     Subroutine Name  DII                                           *
*        Modified Bessel Functions of the first kind   (Array Type)  *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*       X...REAL*8                                                   *
*                                                                    *
*  ===  output ===                                                   *
*       NMAX...maximum N for given X  satisfying  |In(X)|>1.0E-78    *
*       ADI(N) (N=1, NMAX)...modified Bessel functions               *
*                            of the  first kind                      *
*         ADI(1) = I0(X)                                             *
*         ADI(2) = I1(X)                                             *
*         ADI(3) = I2(X)                                             *
*         ...........                                                *
*         ...........                                                *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 ( A-H, O-Z )
       REAL*8 ADI(1)
       REAL*4 XS
      IF(X) 90, 80, 90
   90 XD = DABS(X)
      XS = XD
*
      IF(XS .LE. 0.1) THEN
       NMAX = -176.95 / ALOG(0.0425 * XS)
       M = NMAX + 6
       IF(XS .LE. 0.001) THEN
        T1 = 1.0D0
        T2 = 0.25D0 * X * X
        W1 = 0.0D0
        W2 = 1.0D0
        DO 500 N = 1, NMAX
         W1 = W1 + 1.0D0
         W2 = W2 + 1.0D0
         ADI(N) = T1 * (1.0D0 + T2 * (1.0D0 + 0.5D0 * T2 / W2) / W1)
         T1 = 0.5D0 * X * T1 / W1
  500   CONTINUE
        GO TO 5000
       ELSE
        GO TO 333
       END IF
      END IF
*
      IF( XS .LE. 1.0) THEN
       NMAX = XS * (-10.98 * XS + 29.90) + 29.9
       M = NMAX + 7
       GO TO 333
      END IF
      IF( XS .LE. 10.0) THEN
       NMAX = XS * (-0.27 * XS + 7.62) + 41.6
       M = NMAX + 10
       GO TO 333
      END IF
      IF( XS .LE. 20.0) THEN
       NMAX = 2.71 * XS + 65.0
       M = NMAX + 11
       GO TO 333
      END IF
      IF( XS .LE. 30.0) THEN
       NMAX = 2.30 * XS + 73.0
       M = NMAX + 12
       GO TO 333
      END IF
      IF( XS .LE. 50.0) THEN
       NMAX = 2.0 * XS + 82.3
       M = NMAX + 12
       GO TO 333
      END IF
      IF( XS .LE. 100.0) THEN
       NMAX = 1.75 * XS + 94.0
       M = NMAX + 14
       GO TO 333
      END IF
      IF( XS .LE. 170.0) THEN
       NMAX = 1.61 * XS + 107.5
       M = NMAX + 16
      ELSE
       GO TO 70
      END IF
*
*   recurrence relation   In-1(x) = 2*n/x*In(x) + In+1(x)
*
  333 Z = 2.0D0 / XD
      T3 = 0.0D0
      T2 = 1.0D-75
      DO 100 N = M + 1, NMAX + 1, -1
       T1 = (DFLOAT(N) * T2) * Z + T3
       T3 = T2
       T2 = T1
  100 CONTINUE
      ADI(NMAX + 2) = 1.0D-78
      ADI(NMAX + 1) = T2 / T3 * 1.0D-78
      S = 0.0D0
      DO 200 N = NMAX, 1, -1
       ADI(N) = (Z * DFLOAT(N)) * ADI(N + 1) + ADI(N + 2)
       S = S + ADI(N)
  200 CONTINUE
      S = 2.D0 * S - ADI(1)
      S = DEXP(XD) / S
      DO 300 N = 1, NMAX
       ADI(N) = S * ADI(N)
  300 CONTINUE
*
      IF(X. GT. 0.0D0) GO TO 5000
      DO 400 N = 2, NMAX, 2
       ADI(N) = -ADI(N)
  400 CONTINUE
      GO TO 5000
   80 ADI(1) = 1.0D0
      NMAX = 1
      GO TO 5000
   70 WRITE(6,109) X
  109 FORMAT(1H ,'(SUBR.DII) THE VALUE OF X IS INVALID',5X,'X=', D15.7)
      NMAX = 1
      ADI(1) = 0.D0
 5000 NMAX = NMAX - 1
      RETURN
      END

*******

      SUBROUTINE DKK(X, ADK, NMAX)
**********************************************************************
*     Subroutine Name  DKK                                           *
*         Modified Bessel Functions of the second kind  (Array Type) *
*                                                                    *
*     Copyright  :  I.Tonozuka,  June 30  1989  Ver.1                *
*                                                                    *
*                                                                    *
*  ===  input  ===                                                   *
*         X...REAL*8                                                 *
*                                                                    *
*  ===  output ===                                                   *
*         NMAX...maximum N for given X  satisfying  |Kn(X)|<1.0E+75  *
*         ADK(N) (N=1, NMAX)...modified Bessel functions             *
*                              of the second kind                    *
*           ADK(1) = K0(X)                                           *
*           ADK(2) = K1(X)                                           *
*           ADK(3) = K2(X)                                           *
*           ..........                                               *
*           ..........                                               *
*                                                                    *
*                                                                    *
**********************************************************************
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*4 XS
       REAL*8 TT2(19),W0(19),W1(19),HG2(19),HGW(19)
       REAL*8 ADK(1)
*
       DATA TT2/ 0.50816303094955846D+02,  0.40181837283931421D+02,
     $           0.31708614169687260D+02,  0.24958034076222436D+02,
     $           0.19580666544561472D+02,  0.15298162754506354D+02,
     $           0.11888847347168759D+02,  0.91762438374046260D+01,
     $           0.70199396567662448D+01,  0.53083179739444593D+01,
     $           0.39527800697270042D+01,  0.28831591991125255D+01,
     $           0.20440885482015186D+01,  0.13921352803190245D+01,
     $           0.89355232092143692D+00,  0.52253150686907060D+00,
     $           0.25986767778329635D+00,  0.91964558813223533D-01,
     $           0.10130976294839900D-01/
       DATA W0/  0.14218868556399025D-21,  0.52928400912154829D-17,
     $           0.22714546957181119D-13,  0.17429758067817779D-10,
     $           0.33925627129335466D-08,  0.22133366350879712D-06,
     $           0.60436959864401697D-05,  0.82442796769693937D-04,
     $           0.64689844467092127D-03,  0.32668591886986535D-02,
     $           0.11610849355475341D-01,  0.31182218709688200D-01,
     $           0.66948999642992045D-01,  0.12014580707255842D+00,
     $           0.18661317032220902D+00,  0.25770930676459221D+00,
     $           0.32288745265914786D+00,  0.37237295940447920D+00,
     $           0.39898061810456932D+00/
       DATA W1/  0.72255033422931014D-20,  0.21267603931508927D-15,
     $           0.72024680550449987D-12,  0.43501249579690905D-09,
     $           0.66428639213464601D-07,  0.33859984074087223D-05,
     $           0.71852578995483689D-04,  0.75651520579630600D-03,
     $           0.45411880456458046D-02,  0.17341527349714676D-01,
     $           0.45895133924925560D-01,  0.89903300721576239D-01,
     $           0.13684968348378760D+00,  0.16725921680811156D+00,
     $           0.16674863145591728D+00,  0.13466123239788594D+00,
     $           0.83908012507896790D-01,  0.34245114925607327D-01,
     $           0.40420631841179627D-02/
       DATA HG2/ 0.25485979166099077D+02,  0.18046505467728980D+02,
     $           0.12771825354869194D+02,  0.87697567302686021D+01,
     $           0.56944233429577552D+01,  0.33691762702432690D+01,
     $           0.16923950797931789D+01,  0.60323635708174870D+00,
     $           0.66702230958194404D-01,  0.15129959781108085D+02,
     $           0.91242480375311789D+01,  0.51961525300544656D+01,
     $           0.25525898026681713D+01,  0.89830283456961770D+00,
     $           0.98747014068481182D-01,  0.85886356890120343D+01,
     $           0.39269635013582872D+01,  0.13390972881263614D+01,
     $           0.14530352150331709D+00/
       DATA HGW/ 0.78281997721158910D-11,  0.10467205795792082D-07,
     $           0.18106544810934304D-05,  0.91811268679294035D-04,
     $           0.18885226302684179D-02,  0.18640042387544652D-01,
     $           0.97301747641315429D-01,  0.28480728566997958D+00,
     $           0.48349569472545555D+00,  0.26585516843563016D-06,
     $           0.85736870435878587D-04,  0.39053905846290619D-02,
     $           0.51607985615883930D-01,  0.26049231026416113D+00,
     $           0.57013523626247958D+00,  0.19960407221136762D-03,
     $           0.17077983007413475D-01,  0.20780232581489188D+00,
     $           0.66114701255824129D+00/
       DATA GAM/ 0.57721566490153286D+00/
*
      IF( X ) 70, 70, 90
   90 XS = X
      Z = 2.0D0 / X
      IF(XS .LE. 0.1) THEN
       NMAX = -176.94/ALOG(0.042*XS)
       NNN = 16
        IF(XS .LE. 0.001) THEN
         T2 = 0.25D0 * X * X
         AI0 = 1.0D0 + T2 * (1.0D0 + 0.25D0 * T2)
         AI1 = 0.5D0 * X * (1.0D0 + 0.5D0 * T2 * (1.0D0 + T2 / 6.0D0))
         DL = DLOG(0.5D0 * X) + GAM
         ADK(1) = -DL * AI0 + T2 * (1.0D0 + 3.0D0 * T2 / 8.0D0)
         ADK(2) = (1.0D0 / X - ADK(1) * AI1) / AI0
         GO TO 333
        ELSE
         GO TO 444
        END IF
      END IF
*
      IF( XS .LE. 1.0 ) THEN
       NMAX = XS * (-15.53 * XS + 34.80) + 28.3
       NNN = 7.0 * XS + 15.0
       GO TO 444
      END IF
      IF( XS .LE. 10.0) THEN
       NMAX = XS * (-0.28 * XS + 7.5 ) + 40.5
       GO TO 555
      END IF
      IF( XS .LE. 20.0) THEN
       NMAX = 2.79 * XS + 63.5
       K = 1
       L = 9
       GO TO 666
      END IF
      IF( XS .LE. 30.0) THEN
       NMAX = 2.24 * XS + 74.1
       K = 10
       L = 15
       GO TO 666
      END IF
      IF( XS .LE. 50.0) THEN
       NMAX = 2.0 * XS + 81.3
       K = 10
       L = 15
       GO TO 666
      END IF
      IF( XS .LE. 100.0) THEN
       NMAX = 1.75 * XS + 94.0
       K = 10
       L = 15
       GO TO 666
      END IF
      IF( XS .LE. 170.0) THEN
       NMAX = 1.62 * XS + 107.5
       K = 16
       L = 19
       GO TO 666
      ELSE
       GO TO 70
      END IF
*
* recurrence relation to get  I0(x) and I1(x)
*
  444 CONTINUE
      T3 = 0.0D0
      T2 = 1.0D-78
      S = 0.0D0
      S0 = 0.0D0
      IF( (NNN/2)*2 .NE. NNN) NNN = NNN + 1
      N2 = NNN-2
      DO 100 I = 1, N2, 2
       N = NNN - I + 1
       T1 = (DFLOAT(N) * T2) * Z + T3
       S = S + T1
       T3 = T2
       T2 = T1
       N = N-1
       T1 = (DFLOAT(N) * T2) * Z + T3
       S = S + T1
       S0 = S0 + T1 / DFLOAT(N-1)
       T3 = T2
       T2 = T1
  100 CONTINUE
      AI1 = 2.0D0 * T2 * Z + T3
      S = S + AI1
      T3 = T2
      T2 = AI1
      AI0 = T2 * Z + T3
      S = S + S + AI0
      BB = DEXP(X) / S
      AI0 = BB * AI0
      AI1 = BB * AI1
      DL = DLOG(0.5D0*X) + GAM
      ADK(1) = -DL * AI0 + 4.0D0 * BB * S0
      ADK(2) = (1.0D0 / X - ADK(1) * AI1) / AI0
      GO TO 333
*
*  Numerical Integration for K0(x) and K1(x)
*
  555 CONTINUE
      T1 = 0.0D0
      T2 = 0.0D0
      X2 = 2.0D0*X
      DO 200 I = 1, 19
       R = DSQRT(X2 + TT2(I))
       T1 = T1 + W0(I) / R
       T2 = T2 + W1(I) * R
  200 CONTINUE
      T3 = DEXP(-X)
      ADK(1) = T3 * T1
      ADK(2) = T2 * T3 / X
      GO TO 333
  666 T1 = 0.0D0
      T2 = 0.0D0
      X2 = 2.0D0 * X
      DO 300 I = K, L
       R = DSQRT(X2 + HG2(I))
       T1 = T1 + HGW(I) / R
       T2 = T2 + HG2(I) * HGW(I) * R
  300 CONTINUE
      T3 = 2.0D0 * DEXP(-X)
      ADK(1) = T3 * T1
      ADK(2) = T2 * T3 / X
*
*  recurrence relation   Kn+1(x) = 2*n/x*Kn(x) + Kn-1(x)
*
  333 CONTINUE
      DO 400 N = 3, NMAX
       ADK(N) = (DFLOAT(N-2) * Z) * ADK(N-1) + ADK(N-2)
  400 CONTINUE
      GO TO 5000
   70 WRITE(6,109) X
  109 FORMAT(1H ,'(SUBR.DKK) THE VALUE OF X IS INVALID',5X,'X=',D15.7)
      NMAX = 1
      ADK(1) = 0.0D0
 5000 NMAX = NMAX - 1
      RETURN
      END
**********

