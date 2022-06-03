MODULE module_gad_ge16

REAL, PARAMETER :: pi=3.1415926535897932384626433

CONTAINS

!===============================================================================
!
!  GE 1.6 XLE Turbine
!
!===============================================================================

!=========================================================================

FUNCTION calc_rpm_ge16( x ) result( rpm )

!_________________________________________________________________________
!
! Data from GE 1.6 XLE
! x =  wind speed
!_________________________________________________________________________


  REAL, INTENT( in ) :: x

  REAL, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws
  REAL, DIMENSION( 0:nws-1 ) :: rp

  REAL :: rpm
  INTEGER :: i

  rpm = 0.0

  rp(0) = 2.49
  rp(1) = 4.98
  rp(2) = 5.00
  rp(3) = 5.00
  rp(4) = 5.00
  rp(5) = 5.97
  rp(6) = 6.97
  rp(7) = 7.96
  rp(8) = 8.96
  rp(9) = 9.95
  rp(10) = 10.95
  rp(11) = 11.95
  rp(12) = 12.94
  rp(13) = 13.94
  rp(14) = 14.93
  rp(15) = 15.93
  rp(16) = 16.92
  rp(17) = 17.76
  rp(18) = 17.76
  rp(19) = 17.76
  rp(20) = 17.76
  rp(21) = 17.76
  rp(22) = 17.76
  rp(23) = 17.76
  rp(24) = 17.76
  rp(25) = 17.76
  rp(26) = 17.76
  rp(27) = 17.76
  rp(28) = 17.76
  rp(29) = 17.76
  rp(30) = 17.76
  rp(31) = 17.76
  rp(32) = 17.76
  rp(33) = 17.76
  rp(34) = 17.76
  rp(35) = 17.76
  rp(36) = 17.76
  rp(37) = 17.76
  rp(38) = 17.76
  rp(39) = 17.76
  rp(40) = 17.76
  rp(41) = 17.76
  rp(42) = 17.76
  rp(43) = 17.76
  rp(44) = 17.76
  rp(45) = 17.76
  rp(46) = 17.76
  rp(47) = 17.76
  rp(48) = 17.76
  rp(49) = 17.76

  ws(0) = 0.5
  ws(1) = 1.0
  ws(2) = 1.5
  ws(3) = 2.0
  ws(4) = 2.5
  ws(5) = 3.0
  ws(6) = 3.5
  ws(7) = 4.0
  ws(8) = 4.5
  ws(9) = 5.0
  ws(10) = 5.5
  ws(11) = 6.0
  ws(12) = 6.5
  ws(13) = 7.0
  ws(14) = 7.5
  ws(15) = 8.0
  ws(16) = 8.5
  ws(17) = 9.0
  ws(18) = 9.5
  ws(19) = 10.0
  ws(20) = 10.5
  ws(21) = 11.0
  ws(22) = 11.5
  ws(23) = 12.0
  ws(24) = 12.5
  ws(25) = 13.0
  ws(26) = 13.5
  ws(27) = 14.0
  ws(28) = 14.5
  ws(29) = 15.0
  ws(30) = 15.5
  ws(31) = 16.0
  ws(32) = 16.5
  ws(33) = 17.0
  ws(34) = 17.5
  ws(35) = 18.0
  ws(36) = 18.5
  ws(37) = 19.0
  ws(38) = 19.5
  ws(39) = 20.0
  ws(40) = 20.5
  ws(41) = 21.0
  ws(42) = 21.5
  ws(43) = 22.0
  ws(44) = 22.5
  ws(45) = 23.0
  ws(46) = 23.5
  ws(47) = 24.0
  ws(48) = 24.5
  ws(49) = 25.0

  DO i = 1,nws-1
     
     IF ( ( x .GE. ws(i-1) ) .AND. ( x .LT. ws(i) ) ) THEN
        rpm = rp(i-1) + (x - ws(i-1))*( ( rp(i) - rp(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 1
     ENDIF

  END DO
  
  IF ( x .LT. ws(0) ) rpm = rp(0)
  IF ( x .EQ. ws(nws-1) ) rpm = rp(nws-1)

1 CONTINUE
 
!PRINT*,x,rpm
!_________________________________________________________________________

END FUNCTION calc_rpm_ge16
!_________________________________________________________________________
 
!=========================================================================

FUNCTION calc_pitch_ge16( x ) RESULT( pitch )

!_________________________________________________________________________
!
! Data from GE 1.6 XLE
! x = wind speeds
!_________________________________________________________________________
 
  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws 
  REAL, DIMENSION( 0:nws-1 ) :: p

  REAL :: pitch
  INTEGER :: i

  pitch = 0.0

  p(0) = -1.50
  p(1) = -1.50
  p(2) = -1.50
  p(3) = -1.50
  p(4) = -1.50
  p(5) = -1.50
  p(6) = -1.50
  p(7) = -1.50
  p(8) = -1.50
  p(9) = -1.50
  p(10) = -1.50
  p(11) = -1.50
  p(12) = -1.50
  p(13) = -1.50
  p(14) = -1.50
  p(15) = -1.50
  p(16) = -1.50
  p(17) = -1.50
  p(18) = -1.50
  p(19) = -0.61
  p(20) = 0.32
  p(21) = 2.48
  p(22) = 4.13
  p(23) = 5.48
  p(24) = 6.69
  p(25) = 7.81
  p(26) = 8.81
  p(27) = 9.78
  p(28) = 10.64
  p(29) = 11.46
  p(30) = 12.26
  p(31) = 13.06
  p(32) = 13.86
  p(33) = 14.69
  p(34) = 15.41
  p(35) = 16.11
  p(36) = 16.77
  p(37) = 17.45
  p(38) = 18.11
  p(39) = 18.78
  p(40) = 19.48
  p(41) = 20.12
  p(42) = 20.71
  p(43) = 21.34
  p(44) = 21.90
  p(45) = 22.49
  p(46) = 23.10
  p(47) = 23.67
  p(48) = 24.24
  p(49) = 24.85
  
  ws(0) = 0.5
  ws(1) = 1.0
  ws(2) = 1.5
  ws(3) = 2.0
  ws(4) = 2.5
  ws(5) = 3.0
  ws(6) = 3.5
  ws(7) = 4.0
  ws(8) = 4.5
  ws(9) = 5.0
  ws(10) = 5.5
  ws(11) = 6.0
  ws(12) = 6.5
  ws(13) = 7.0
  ws(14) = 7.5
  ws(15) = 8.0
  ws(16) = 8.5
  ws(17) = 9.0
  ws(18) = 9.5
  ws(19) = 10.0
  ws(20) = 10.5
  ws(21) = 11.0
  ws(22) = 11.5
  ws(23) = 12.0
  ws(24) = 12.5
  ws(25) = 13.0
  ws(26) = 13.5
  ws(27) = 14.0
  ws(28) = 14.5
  ws(29) = 15.0
  ws(30) = 15.5
  ws(31) = 16.0
  ws(32) = 16.5
  ws(33) = 17.0
  ws(34) = 17.5
  ws(35) = 18.0
  ws(36) = 18.5
  ws(37) = 19.0
  ws(38) = 19.5
  ws(39) = 20.0
  ws(40) = 20.5
  ws(41) = 21.0
  ws(42) = 21.5
  ws(43) = 22.0
  ws(44) = 22.5
  ws(45) = 23.0
  ws(46) = 23.5
  ws(47) = 24.0
  ws(48) = 24.5
  ws(49) = 25.0

  DO i = 1,nws-1

     IF ( ( x .GT. ws(i-1) ) .AND. ( x .LE. ws(i) ) ) THEN 
        pitch = p(i-1) + (x - ws(i-1))*( ( p(i) - p(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 2
     ENDIF
     
  ENDDO

  IF ( x .LE. ws(0) ) pitch = p(0)
  IF ( x .GT. ws(nws-1) ) pitch = p(nws-1)

2 CONTINUE
  
!PRINT*,x,pitch
!_________________________________________________________________________

END FUNCTION calc_pitch_ge16

!=========================================================================

FUNCTION calc_fnft_ge16( v0, rho, blades, bladelen, diameter,           &
                         radius, pitchdeg, rpm, dscale, cscale)         &
                         result( fnft )
!_________________________________________________________________________
!
!  --v0       - "free stream" velocity normal to the turbine rotor plane, [m/s]
!  --rho      - air density, [kg/m^3]
!  --blades   - number of turbine blades, [non-dimensional]
!  --diameter - turbine rotor diameter, [m]
!  --xcoord   - streamwise coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --ycoord   - cross-stream coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --zcoord   - vertical coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!
!  --fnft     - one-dimensional array (two members) with FUNCTION output
!               specific normal and tangential forces per unit area, [N/m^2]
!
     real, intent( in ) :: v0, rho, blades, bladelen, diameter
     real, intent( in ) :: dscale, cscale 
     real, intent( in ) :: radius
     real, intent( in ) :: rpm
     real, intent( in ) :: pitchdeg
     real, dimension( 2 )  :: fnft
     real               :: rps, omega 
     real               :: radmax, radmin
     real               :: an, at
     real               :: anold, atold
     real               :: cord
     real               :: twist
     real               :: xi, xideg, beta, betadeg, psi, psideg
     real               :: vn, vt
     real               :: cl, cd
     real               :: cn, ct
     real               :: solid
     real               :: fac1, fac2, tiploss, hubloss, lossfac
     real               :: vr
     real               :: lift, drag
     real               :: fn, ft
     real               :: epsi, epscrit
     real               :: cd_nac
     integer            :: iter, itmax
!
     rps   = rpm/60.0
     omega = 2.0*pi*rps
!
     radmax = 0.5*diameter
     radmin = radmax - bladelen
!

  fnft(1) = 0.0
  fnft(2) = 0.0

  IF (radius .LT. radmax) THEN !JDM
     
     IF (radius .GE. radmin) THEN !JDM

        an = 0.0
        at = 0.0
!
        iter = 0
        epsi = 1.0        
        epscrit = 1.e-5
        itmax   = 20
!
!    --Incidence angle

        xideg = calc_twist_ge16( radius/dscale ) + pitchdeg
        xi    = xideg*pi/180.0
!
!    --Cord length 

        cord  = cscale * calc_chord_ge16( radius/dscale )
!
!    --Compute induction factors an and at iteratively

        do while ( ( epsi .gt. epscrit ) .and. ( iter .lt. itmax ) ) 
!
           iter = iter + 1
!
!       --Normal and tangential velocity components

           an = 0.02 !JDM This is just the tip loss component of an. This was computed using the
                     !    IDL code as the difference between an computed with and without tip loss
                     !    across the power curve. The value of 0.02 was nearly constant.

           vn = v0*( 1.0 - an )
           vt = omega*radius*( 1.0 + at ) 
 
!       --Advance angle 
  
           psi    = atan2(vn,vt)
           psideg = psi*180.0/pi

! print*,'iter',iter
! print*,'v0,  omega, radius',v0,omega,radius
! print*,'vn, at, vt',vn,at,vt
! print*,'psi',psi
! print*,'------'

!       --Incidence angle  
 
           beta    = psi - xi
           betadeg = beta*180.0/pi

!       --Lift and drag coefficients  
     
           cl = calc_cl_ge16( radius/dscale, betadeg )
           cd = calc_cd_ge16( radius/dscale, betadeg )
  
!       --Normal and tangential force coefficients     
  
           cn = cl*cos( psi ) + cd*sin( psi )
           ct = cl*sin( psi ) - cd*cos( psi )

!       --Solidity factor

           solid = cord*blades / ( 2.0*pi*radius )

!       --Tip and hub loss

!          When psi < 0, fac1 and fac 2 can give bizarre answers, and resulting in tip and hub loss terms - NaN.
!          Some logic should be added to keep things from going crazy
           
!          RSA update: the argument of acos should have an abs. value <= 1. When psi
!          is negative, fac1 and/or fac2 are negative, causing exp(-fac) to be a
!          large positive number, causing acos to return NaN. When psi is positive, fac1 and fac2 are
!          positive, such that exp(-fac) is small and positive, which is ok.
!          Thus, we have added logic around the calls to calc_fnft above such
!          that when v0 < 0 (ie psi < 0), this function is not called and the
!          force is set to 0. The turbine should still yaw into place eventually.

           if ( abs( psi ) .gt. 0.001 ) then

              fac1 = blades / 2.0*( radmax - radius ) / ( radius*sin( psi ) )
              fac2 = blades / 2.0*( radius - radmin ) / ( radmin*sin( psi ) )

              tiploss = (2.0/pi)*acos( exp( -fac1 ) )
              hubloss = (2.0/pi)*acos( exp( -fac2 ) )

              lossfac = tiploss*hubloss

!             --Induction factors

              anold = an
              atold = at

!JDM             an = ( 1. + 4.0*lossfac*sin( psi )*sin( psi )/ ( solid*cn ) )**(-1.0)

              at = ( 4.0*lossfac*sin( psi )*cos( psi )/ ( solid*ct ) - 1.0 )**(-1.0)

! print*,'inside of iteration'
! print*,'iter',iter
! print*,'omega, radius, radmin, radmax',omega,radius,radmin,radmax
! print*,'v0, vn, vt, an, at',v0,vn,vt,an,at
! print*,'psi, fac1, fac2',psi,fac1,fac2
! print*,'tiploss, hubloss, lossfac',tiploss,hubloss,lossfac
! print*,'------'

          else

             anold = an
             atold = at

             an = 0.0
             at = 0.0

print*,'what am I doing in here???'

          endif

!       --Convergence criteria

           epsi = sqrt( ( anold - an )**2.0 + ( atold - at )**2.0 )
  
        end do

!print*,'radius,radmax,radmin',radius,radmax,radmin
!print*,'radius - radmin',radius - radmin
!print*,'fac1,fac2',fac1,fac2
!print*,'psi,sin(psi)',psi,sin(psi)
!print*,'tiploss,hubloss',tiploss,hubloss
!print*,'cl,cd',cl,cd
!print*,'cn,ct',cn,ct

!    --Normal and tangential velocity components

       vn = v0*( 1.0 - an )

       vt = omega*radius*( 1.0 + at ) 

!    --Relative velocity

       vr = sqrt( vn*vn + vt*vt ) 
! 
!    --Advance angle   

       psi    = atan2(vn,vt)
       psideg = psi*180.0/pi
!
!    --Incidence angle   

       beta    = psi - xi
       betadeg = beta*180.0/pi

! print*,'outside of iteration'
! print*,'v0, vn, at, vt',v0,vn,at,vt
! print*,'psideg, betadeg',psideg,betadeg
! print*,'------'

!    --Lift and drag coefficients     
  
       cl = calc_cl_ge16( radius/dscale, betadeg )
       cd = calc_cd_ge16( radius/dscale, betadeg )

!    --Lift and drag forces

       lift = 0.5*rho*vr*vr*cord*cl 
       drag = 0.5*rho*vr*vr*cord*cd 

!    --Specific normal and tangential forces

       fn = lift*cos( psi ) + drag*sin( psi )
       ft = lift*sin( psi ) - drag*cos( psi )

!       print*,'psi',psideg

       fn = blades*fn/(2.0*pi*radius)
       ft = blades*ft/(2.0*pi*radius)

       if( fn .lt. 0.0 ) fn = 0.0
       if( ft .lt. 0.0 ) ft = 0.0

       fnft(1) = fn
       fnft(2) = ft              

!print*,'radius,radmax,radmin',radius,radmax,radmin
!print*,'radius - radmin',radius - radmin
!print*,'fac1,fac2',fac1,fac2
!print*,'psi,sin(psi)',psi,sin(psi)
!print*,'tiploss,hubloss',tiploss,hubloss
!print*,'cl,cd',cl,cd
!print*,'cn,ct',cn,ct
!print*,'fn,ft',fn,ft


     ENDIF !radius .GE. radmin

  ENDIF   !radius .LT. radmax

END FUNCTION calc_fnft_ge16

!=========================================================================

FUNCTION calc_twist_ge16( x ) RESULT( twist )
!_________________________________________________________________________
!
! Computes blade twist as a function of radial distance from hub center
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: ns = 45

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: t

  REAL :: twist 
  INTEGER :: i

  twist = 0.0
  
  r(0) = 1.741282468
  r(1) = 2.411006494
  r(2) = 3.214675325
  r(3) = 4.286233766
  r(4) = 5.357792208
  r(5) = 6.429350649
  r(6) = 7.233019481
  r(7) = 7.768798701
  r(8) = 8.572467532
  r(9) = 9.644025974
  r(10) = 10.71558442
  r(11) = 11.78714286
  r(12) = 12.8587013
  r(13) = 13.93025974
  r(14) = 15.00181818
  r(15) = 16.07337662
  r(16) = 17.14493506
  r(17) = 18.21649351
  r(18) = 19.28805195
  r(19) = 20.35961039
  r(20) = 21.43116883
  r(21) = 22.50272727
  r(22) = 23.57428571
  r(23) = 24.64584416
  r(24) = 25.7174026
  r(25) = 26.78896104
  r(26) = 27.86051948
  r(27) = 28.93207792
  r(28) = 30.00363636
  r(29) = 31.07519481
  r(30) = 32.14675325
  r(31) = 33.21831169
  r(32) = 34.28987013
  r(33) = 35.36142857
  r(34) = 36.43298701
  r(35) = 37.50454545
  r(36) = 38.5761039
  r(37) = 39.51371753
  r(38) = 40.02270779
  r(39) = 40.23701948
  r(40) = 40.45133117
  r(41) = 40.66564286
  r(42) = 40.87995455
  r(43) = 41.09426623
  r(44) = 41.22821104
 
  t(0) = 7.000000000
  t(1) = 7.000000000
  t(2) = 7.160291098
  t(3) = 8.516410557
  t(4) = 11.55353985
  t(5) = 15.2530498
  t(6) = 17.85027745
  t(7) = 19.09683005
  t(8) = 19.72651954
  t(9) = 19.28457508
  t(10) = 17.79127829
  t(11) = 15.79059519
  t(12) = 13.72814049
  t(13) = 12.0495289
  t(14) = 10.70696753
  t(15) = 9.451441234
  t(16) = 8.33614513
  t(17) = 7.39718539
  t(18) = 6.578383117
  t(19) = 5.915462662
  t(20) = 5.394433442
  t(21) = 4.884986039
  t(22) = 4.383692532
  t(23) = 3.895534091
  t(24) = 3.418494156
  t(25) = 2.953569481
  t(26) = 2.504517532
  t(27) = 2.077147403
  t(28) = 1.629176623
  t(29) = 1.180975
  t(30) = 0.788529545
  t(31) = 0.423586364
  t(32) = 0.02829513
  t(33) = -0.413967532
  t(34) = -1.046716234
  t(35) = -1.88081039
  t(36) = -2.424636039
  t(37) = -2.432605396
  t(38) = -2.271112767
  t(39) = -2.097390282
  t(40) = -1.777110244
  t(41) = -1.259585365
  t(42) = -0.494128356
  t(43) = 0.569948071
  t(44) = 1.364513867
  
  DO i = 1,ns-1
 
     IF ( ( x .GT. r(i-1) ) .AND. ( x .LE. r(i) ) ) THEN
        twist = t(i-1) + (x - r(i-1))*( ( t(i) - t(i-1) )/( r(i) - r(i-1) ) )
        goto 1
     ENDIF

1 CONTINUE

     IF ( x .LT. r(0) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius < blade root in calc_twist_ge16.pro'
	PRINT*, 'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF
     
     IF ( x .GT. r(ns-1) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius >  blade length in calc_twist_ge16.pro'
	PRINT*,'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF 
     
  ENDDO
  
!PRINT*,x,twist
  
!_________________________________________________________________________
  
END FUNCTION calc_twist_ge16
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_chord_ge16( x ) RESULT( chord )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE
! x = radial distance from hub center
!_________________________________________________________________________
 

  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: ns = 45

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: c

  REAL :: chord 
  INTEGER :: i

  chord = 0.0
  
  r(0) = 1.741282468
  r(1) = 2.411006494
  r(2) = 3.214675325
  r(3) = 4.286233766
  r(4) = 5.357792208
  r(5) = 6.429350649
  r(6) = 7.233019481
  r(7) = 7.768798701
  r(8) = 8.572467532
  r(9) = 9.644025974
  r(10) = 10.71558442
  r(11) = 11.78714286
  r(12) = 12.8587013
  r(13) = 13.93025974
  r(14) = 15.00181818
  r(15) = 16.07337662
  r(16) = 17.14493506
  r(17) = 18.21649351
  r(18) = 19.28805195
  r(19) = 20.35961039
  r(20) = 21.43116883
  r(21) = 22.50272727
  r(22) = 23.57428571
  r(23) = 24.64584416
  r(24) = 25.7174026
  r(25) = 26.78896104
  r(26) = 27.86051948
  r(27) = 28.93207792
  r(28) = 30.00363636
  r(29) = 31.07519481
  r(30) = 32.14675325
  r(31) = 33.21831169
  r(32) = 34.28987013
  r(33) = 35.36142857
  r(34) = 36.43298701
  r(35) = 37.50454545
  r(36) = 38.5761039
  r(37) = 39.51371753
  r(38) = 40.02270779
  r(39) = 40.23701948
  r(40) = 40.45133117
  r(41) = 40.66564286
  r(42) = 40.87995455
  r(43) = 41.09426623
  r(44) = 41.22821104

  c(0) = 1.91808961
  c(1) = 1.935265611
  c(2) = 2.079926354
  c(3) = 2.413685743
  c(4) = 2.826235036
  c(5) = 3.159994425
  c(6) = 3.304655168
  c(7) = 3.321830049
  c(8) = 3.309611913
  c(9) = 3.270855238
  c(10) = 3.205794149
  c(11) = 3.119088542
  c(12) = 3.01534901
  c(13) = 2.89912368
  c(14) = 2.774879596
  c(15) = 2.64697337
  c(16) = 2.519593262
  c(17) = 2.396300182
  c(18) = 2.27876494
  c(19) = 2.16699464
  c(20) = 2.060706521
  c(21) = 1.959631688
  c(22) = 1.863514437
  c(23) = 1.772111606
  c(24) = 1.685191959
  c(25) = 1.602535601
  c(26) = 1.523933424
  c(27) = 1.449080684
  c(28) = 1.37715292
  c(29) = 1.306805483
  c(30) = 1.236596651
  c(31) = 1.165093086
  c(32) = 1.090869428
  c(33) = 1.014651021
  c(34) = 0.932961792
  c(35) = 0.849479979
  c(36) = 0.770592153
  c(37) = 0.703359905
  c(38) = 0.66574374
  c(39) = 0.640152951
  c(40) = 0.596399181
  c(41) = 0.527487761
  c(42) = 0.427640264
  c(43) = 0.271130226
  c(44) = 0.13999911

  DO i = 1,ns-1
     IF ( ( x .GE. r(i-1) ) .AND. ( x .LT. r(i) ) ) THEN
        chord = c(i-1) + (x - r(i-1))*( ( c(i) - c(i-1) )/( r(i) - r(i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

  IF ( x .LT. r(0) ) chord = 0.0 !No airfoil here, just a cylinder
  IF ( x .EQ. r(ns-1) ) chord = c(ns-1)
  
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius > blade length in calc_chord_ge16.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
1 CONTINUE
  
!PRINT*,x,chord
   
!_________________________________________________________________________
  
END FUNCTION calc_chord_ge16
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl_ge16( x,y ) RESULT( cl )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from GE 1.6 XLE
!
! calc_cl1_ge16 uses data from Cylinder
! calc_cl1_ge16 uses data from DU96-392
! calc_cl1_ge16 uses data from DU96-350
! calc_cl1_ge16 uses data from DU97-W-300
! calc_cl1_ge16 uses data from DU91-W2-250
! calc_cl1_ge16 uses data from FAA-W3-211
! calc_cl1_ge16 uses data from DU96-W-180
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  REAL, PARAMETER :: ns = 8

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cl

  INTEGER :: i

  cl = 0.0
  
  r(0) = 1.741282468
  r(1) = 4.286233766
  r(2) = 8.572467532
  r(3) = 11.78714286
  r(4) = 16.07337662
  r(5) = 24.64584416
  r(6) = 32.14675325
  r(7) = 41.22821104
  
  IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
       cl = calc_cl1_ge16(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r( 2) ) ) THEN
       cl = calc_cl2_ge16(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r( 3) ) ) THEN
       cl = calc_cl3_ge16(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r( 4) ) ) THEN
       cl = calc_cl4_ge16(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r( 5) ) ) THEN
       cl = calc_cl5_ge16(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r( 6) ) ) THEN
       cl = calc_cl6_ge16(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. ( x .LE. r( 7) ) ) THEN
       cl = calc_cl7_ge16(y)
  ENDIF
  
  IF ( x .LT. r(0) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cl_ge16.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cl_ge16.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
!_________________________________________________________________________
  
END FUNCTION calc_cl_ge16
!_________________________________________________________________________


FUNCTION calc_cl1_ge16( y ) RESULT( cl1 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: Cylinder
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cl1

  cl1 = 0.0001
!_________________________________________________________________________
  
END FUNCTION calc_cl1_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl2_ge16( y ) RESULT( cl2 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU96-392
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl2
  INTEGER :: i

  cl2 = 0.0

!                alpha    CL   
  cl(0:1, 0) = [-180.0, -0.201]
  cl(0:1, 1) = [-170.0, 0.516]
  cl(0:1, 2) = [-160.0, 0.408]
  cl(0:1, 3) = [-150.0, 0.716]
  cl(0:1, 4) = [-140.0, 0.825]
  cl(0:1, 5) = [-130.0, 0.754]
  cl(0:1, 6) = [-120.0, 0.593]
  cl(0:1, 7) = [-110.0, 0.373]
  cl(0:1, 8) = [-100.0, 0.113]
  cl(0:1, 9) = [-90.0, -0.163]
  cl(0:1,10) = [-80.0, -0.428]
  cl(0:1,11) = [-70.0, -0.656]
  cl(0:1,12) = [-60.0, -0.828]
  cl(0:1,13) = [-50.0, -0.929]
  cl(0:1,14) = [-40.0, -0.918]
  cl(0:1,15) = [-30.0, -0.99758]
  cl(0:1,16) = [-25.0, -1.07351]
  cl(0:1,17) = [-20.0, -1.00748]
  cl(0:1,18) = [-19.0, -0.95499]
  cl(0:1,19) = [-18.0, -0.87589]
  cl(0:1,20) = [-17.0, -0.821]
  cl(0:1,21) = [-16.0, -0.785]
  cl(0:1,22) = [-15.0, -0.735]
  cl(0:1,23) = [-14.0, -0.685]
  cl(0:1,24) = [-13.0, -0.64333]
  cl(0:1,25) = [-12.0, -0.66]
  cl(0:1,26) = [-11.0, -0.65429]
  cl(0:1,27) = [-10.0, -0.623]
  cl(0:1,28) = [-9.0, -0.611]
  cl(0:1,29) = [-8.0, -0.59571]
  cl(0:1,30) = [-7.0, -0.5675]
  cl(0:1,31) = [-6.0, -0.48727]
  cl(0:1,32) = [-5.0, -0.383]
  cl(0:1,33) = [-4.0, -0.288]
  cl(0:1,34) = [-3.0, -0.17]
  cl(0:1,35) = [-2.0, -0.0575]
  cl(0:1,36) = [-1.0, 0.0675]
  cl(0:1,37) = [0.0, 0.19375]
  cl(0:1,38) = [1.0, 0.335]
  cl(0:1,39) = [2.0, 0.46556]
  cl(0:1,40) = [3.0, 0.59167]
  cl(0:1,41) = [4.0, 0.70833]
  cl(0:1,42) = [5.0, 0.828]
  cl(0:1,43) = [6.0, 0.93]
  cl(0:1,44) = [7.0, 1.015]
  cl(0:1,45) = [8.0, 1.0925]
  cl(0:1,46) = [9.0, 1.15857]
  cl(0:1,47) = [10.0, 1.17]
  cl(0:1,48) = [11.0, 1.17625]
  cl(0:1,49) = [12.0, 1.19556]
  cl(0:1,50) = [13.0, 1.212]
  cl(0:1,51) = [14.0, 1.22]
  cl(0:1,52) = [15.0, 1.229]
  cl(0:1,53) = [16.0, 1.24]
  cl(0:1,54) = [17.0, 1.27333]
  cl(0:1,55) = [18.0, 1.26897]
  cl(0:1,56) = [19.0, 1.25722]
  cl(0:1,57) = [20.0, 1.24454]
  cl(0:1,58) = [25.0, 1.17952]
  cl(0:1,59) = [30.0, 1.14117]
  cl(0:1,60) = [40.0, 1.172]
  cl(0:1,61) = [50.0, 1.128]
  cl(0:1,62) = [60.0, 0.989]
  cl(0:1,63) = [70.0, 0.779]
  cl(0:1,64) = [80.0, 0.525]
  cl(0:1,65) = [90.0, 0.25]
  cl(0:1,66) = [100.0, -0.027]
  cl(0:1,67) = [110.0, -0.28]
  cl(0:1,68) = [120.0, -0.477]
  cl(0:1,69) = [130.0, -0.576]
  cl(0:1,70) = [140.0, -0.515]
  cl(0:1,71) = [150.0, -0.204]
  cl(0:1,72) = [160.0, -0.08]
  cl(0:1,73) = [170.0, -0.33]
  cl(0:1,74) = [180.0, -0.201]
  
  IF ( y .EQ. cl(0,0) ) cl2 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl2 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl2 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl2_ge16.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl2 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl2_ge16.pro'
  ENDIF
  
!print,y,cl2
  
!_________________________________________________________________________
  
END FUNCTION calc_cl2_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl3_ge16( y ) RESULT( cl3 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE DU96-350
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl3
  INTEGER :: i

  cl3 = 0.0

!                alpha    CL   
  cl(0:1, 0) = [-180.0, -0.201]
  cl(0:1, 1) = [-170.0, 0.516]
  cl(0:1, 2) = [-160.0, 0.408]
  cl(0:1, 3) = [-150.0, 0.716]
  cl(0:1, 4) = [-140.0, 0.825]
  cl(0:1, 5) = [-130.0, 0.754]
  cl(0:1, 6) = [-120.0, 0.593]
  cl(0:1, 7) = [-110.0, 0.373]
  cl(0:1, 8) = [-100.0, 0.113]
  cl(0:1, 9) = [-90.0, -0.163]
  cl(0:1,10) = [-80.0, -0.428]
  cl(0:1,11) = [-70.0, -0.656]
  cl(0:1,12) = [-60.0, -0.828]
  cl(0:1,13) = [-50.0, -0.929]
  cl(0:1,14) = [-40.0, -0.918]
  cl(0:1,15) = [-30.0, -0.9973]
  cl(0:1,16) = [-25.0, -1.05]
  cl(0:1,17) = [-20.0, -1.05]
  cl(0:1,18) = [-19.0, -1.04268]
  cl(0:1,19) = [-18.0, -1.03]
  cl(0:1,20) = [-17.0, -1.0275]
  cl(0:1,21) = [-16.0, -1.00143]
  cl(0:1,22) = [-15.0, -0.99]
  cl(0:1,23) = [-14.0, -0.972]
  cl(0:1,24) = [-13.0, -0.94364]
  cl(0:1,25) = [-12.0, -0.91636]
  cl(0:1,26) = [-11.0, -0.87714]
  cl(0:1,27) = [-10.0, -0.82]
  cl(0:1,28) = [-9.0, -0.748]
  cl(0:1,29) = [-8.0, -0.66]
  cl(0:1,30) = [-7.0, -0.58125]
  cl(0:1,31) = [-6.0, -0.50833]
  cl(0:1,32) = [-5.0, -0.4125]
  cl(0:1,33) = [-4.0, -0.288]
  cl(0:1,34) = [-3.0, -0.16]
  cl(0:1,35) = [-2.0, -0.01818]
  cl(0:1,36) = [-1.0, 0.12]
  cl(0:1,37) = [0.0, 0.25111]
  cl(0:1,38) = [1.0, 0.38571]
  cl(0:1,39) = [2.0, 0.51571]
  cl(0:1,40) = [3.0, 0.64]
  cl(0:1,41) = [4.0, 0.77]
  cl(0:1,42) = [5.0, 0.9]
  cl(0:1,43) = [6.0, 1.00429]
  cl(0:1,44) = [7.0, 1.13]
  cl(0:1,45) = [8.0, 1.23333]
  cl(0:1,46) = [9.0, 1.33143]
  cl(0:1,47) = [10.0, 1.41]
  cl(0:1,48) = [11.0, 1.465]
  cl(0:1,49) = [12.0, 1.47333]
  cl(0:1,50) = [13.0, 1.43818]
  cl(0:1,51) = [14.0, 1.42]
  cl(0:1,52) = [15.0, 1.39]
  cl(0:1,53) = [16.0, 1.36]
  cl(0:1,54) = [17.0, 1.34182]
  cl(0:1,55) = [18.0, 1.32509]
  cl(0:1,56) = [19.0, 1.3061]
  cl(0:1,57) = [20.0, 1.28655]
  cl(0:1,58) = [25.0, 1.2474]
  cl(0:1,59) = [30.0, 1.23]
  cl(0:1,60) = [40.0, 1.2]
  cl(0:1,61) = [50.0, 1.128]
  cl(0:1,62) = [60.0, 0.989]
  cl(0:1,63) = [70.0, 0.779]
  cl(0:1,64) = [80.0, 0.525]
  cl(0:1,65) = [90.0, 0.25]
  cl(0:1,66) = [100.0, -0.027]
  cl(0:1,67) = [110.0, -0.28]
  cl(0:1,68) = [120.0, -0.477]
  cl(0:1,69) = [130.0, -0.576]
  cl(0:1,70) = [140.0, -0.515]
  cl(0:1,71) = [150.0, -0.204]
  cl(0:1,72) = [160.0, -0.08]
  cl(0:1,73) = [170.0, -0.33]
  cl(0:1,74) = [180.0, -0.201]
 
  IF ( y .EQ. cl(0,0) ) cl3 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl3 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl3 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl3_ge16.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl3 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl3_ge16.pro'
  ENDIF
  
!print,y,cl3
  
!_________________________________________________________________________
  
END FUNCTION calc_cl3_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl4_ge16( y ) RESULT( cl4 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU97-W-300
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl4
  INTEGER :: i

  cl4 = 0.0

!                alpha    CL  
  cl(0:1, 0) = [-180.0,	-0.201]
  cl(0:1, 1) = [-170.0,	0.516]
  cl(0:1, 2) = [-160.0,	0.408]
  cl(0:1, 3) = [-150.0,	0.716]
  cl(0:1, 4) = [-140.0,	0.825]
  cl(0:1, 5) = [-130.0,	0.754]
  cl(0:1, 6) = [-120.0,	0.593]
  cl(0:1, 7) = [-110.0,	0.373]
  cl(0:1, 8) = [-100.0,	0.113]
  cl(0:1, 9) = [-90.0, -0.163]
  cl(0:1,10) = [-80.0, -0.428]
  cl(0:1,11) = [-70.0, -0.656]
  cl(0:1,12) = [-60.0, -0.828]
  cl(0:1,13) = [-50.0, -0.929]
  cl(0:1,14) = [-40.0, -0.918]
  cl(0:1,15) = [-30.0, -0.9964]
  cl(0:1,16) = [-25.0, -1.07837]
  cl(0:1,17) = [-20.0, -1.06419]
  cl(0:1,18) = [-19.0, -1.03318]
  cl(0:1,19) = [-18.0, -0.98146]
  cl(0:1,20) = [-17.0, -0.93865]
  cl(0:1,21) = [-16.0, -0.90275]
  cl(0:1,22) = [-15.0, -0.86748]
  cl(0:1,23) = [-14.0, -0.83026]
  cl(0:1,24) = [-13.0, -0.80885]
  cl(0:1,25) = [-12.0, -0.84071]
  cl(0:1,26) = [-11.0, -0.76956]
  cl(0:1,27) = [-10.0, -0.69042]
  cl(0:1,28) = [-9.0, -0.61313]
  cl(0:1,29) = [-8.0, -0.53591]
  cl(0:1,30) = [-7.0, -0.61899]
  cl(0:1,31) = [-6.0, -0.51486]
  cl(0:1,32) = [-5.0, -0.37811]
  cl(0:1,33) = [-4.0, -0.24362]
  cl(0:1,34) = [-3.0, -0.11243]
  cl(0:1,35) = [-2.0, 0.03019]
  cl(0:1,35) = [-1.0, 0.16831]
  cl(0:1,37) = [0.0, 0.30182]
  cl(0:1,38) = [1.0, 0.43303]
  cl(0:1,39) = [2.0, 0.56453]
  cl(0:1,40) = [3.0, 0.69746]
  cl(0:1,41) = [4.0, 0.82726]
  cl(0:1,42) = [5.0, 0.9529]
  cl(0:1,43) = [6.0, 1.07608]
  cl(0:1,44) = [7.0, 1.20286]
  cl(0:1,45) = [8.0, 1.31622]
  cl(0:1,46) = [9.0, 1.42223]
  cl(0:1,47) = [10.0, 1.5173]
  cl(0:1,48) = [11.0, 1.59217]
  cl(0:1,49) = [12.0, 1.64198]
  cl(0:1,50) = [13.0, 1.65044]
  cl(0:1,51) = [14.0, 1.43898]
  cl(0:1,52) = [15.0, 1.31154]
  cl(0:1,53) = [16.0, 1.2557]
  cl(0:1,54) = [17.0, 1.21065]
  cl(0:1,55) = [18.0, 1.19452]
  cl(0:1,56) = [19.0, 1.19082]
  cl(0:1,57) = [20.0, 1.18627]
  cl(0:1,58) = [25.0, 1.15953]
  cl(0:1,59) = [30.0, 1.14075]
  cl(0:1,60) = [40.0, 1.172]
  cl(0:1,61) = [50.0, 1.128]
  cl(0:1,62) = [60.0, 0.989]
  cl(0:1,63) = [70.0, 0.779]
  cl(0:1,64) = [80.0, 0.525]
  cl(0:1,65) = [90.0, 0.25]
  cl(0:1,66) = [100.0, -0.027]
  cl(0:1,67) = [110.0, -0.28]
  cl(0:1,68) = [120.0, -0.477]
  cl(0:1,69) = [130.0, -0.576]
  cl(0:1,70) = [140.0, -0.515]
  cl(0:1,71) = [150.0, -0.204]
  cl(0:1,72) = [160.0, -0.08]
  cl(0:1,73) = [170.0, -0.33]
  cl(0:1,74) = [180.0, -0.201]

  IF ( y .EQ. cl(0,0) ) cl4 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl4 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl4 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl4_ge16.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl4 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl4_ge16.pro'
  ENDIF
  
!print,y,cl4
  
!_________________________________________________________________________
  
END FUNCTION calc_cl4_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl5_ge16( y ) RESULT( cl5 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU91-W2-250
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl5
  INTEGER :: i

  cl5 = 0.0

!                alpha    CL  
  cl(0:1, 0) = [-180.0, -0.391]
  cl(0:1, 1) = [-170.0, 0.473]
  cl(0:1, 2) = [-160.0, 0.388]
  cl(0:1, 3) = [-150.0, 0.67]
  cl(0:1, 4) = [-140.0, 0.802]
  cl(0:1, 5) = [-130.0, 0.781]
  cl(0:1, 6) = [-120.0, 0.642]
  cl(0:1, 7) = [-110.0, 0.426]
  cl(0:1, 8) = [-100.0, 0.166]
  cl(0:1, 9) = [-90.0, -0.113]
  cl(0:1,10) = [-80.0, -0.39]
  cl(0:1,11) = [-70.0, -0.641]
  cl(0:1,12) = [-60.0, -0.842]
  cl(0:1,13) = [-50.0, -0.959]
  cl(0:1,14) = [-40.0, -0.949]
  cl(0:1,15) = [-30.0, -0.7908]
  cl(0:1,16) = [-25.0, -0.76914]
  cl(0:1,17) = [-20.0, -0.86195]
  cl(0:1,18) = [-19.0, -0.88561]
  cl(0:1,19) = [-18.0, -0.90719]
  cl(0:1,20) = [-17.0, -0.89945]
  cl(0:1,21) = [-16.0, -0.88535]
  cl(0:1,22) = [-15.0, -0.88533]
  cl(0:1,23) = [-14.0, -0.89206]
  cl(0:1,24) = [-13.0, -0.87282]
  cl(0:1,25) = [-12.0, -0.85062]
  cl(0:1,26) = [-11.0, -0.82853]
  cl(0:1,27) = [-10.0, -0.7875]
  cl(0:1,28) = [-9.0, -0.70253]
  cl(0:1,29) = [-8.0, -0.60846]
  cl(0:1,30) = [-7.0, -0.50599]
  cl(0:1,31) = [-6.0, -0.39332]
  cl(0:1,32) = [-5.0, -0.26141]
  cl(0:1,33) = [-4.0, -0.1215]
  cl(0:1,34) = [-3.0, 0.01296]
  cl(0:1,35) = [-2.0, 0.14204]
  cl(0:1,36) = [-1.0, 0.27193]
  cl(0:1,37) = [0.0, 0.40308]
  cl(0:1,38) = [1.0, 0.53287]
  cl(0:1,39) = [2.0, 0.65963]
  cl(0:1,40) = [3.0, 0.78615]
  cl(0:1,41) = [4.0, 0.91687]
  cl(0:1,42) = [5.0, 1.03669]
  cl(0:1,43) = [6.0, 1.15441]
  cl(0:1,44) = [7.0, 1.26168]
  cl(0:1,45) = [8.0, 1.34893]
  cl(0:1,46) = [9.0, 1.40265]
  cl(0:1,47) = [10.0, 1.45358]
  cl(0:1,48) = [11.0, 1.44287]
  cl(0:1,49) = [12.0, 1.31497]
  cl(0:1,50) = [13.0, 1.25836]
  cl(0:1,51) = [14.0, 1.24959]
  cl(0:1,52) = [15.0, 1.26116]
  cl(0:1,53) = [16.0, 1.25438]
  cl(0:1,54) = [17.0, 1.27415]
  cl(0:1,55) = [18.0, 1.27197]
  cl(0:1,56) = [19.0, 1.24623]
  cl(0:1,57) = [20.0, 1.22049]
  cl(0:1,58) = [25.0, 1.09475]
  cl(0:1,59) = [30.0, 1.019]
  cl(0:1,60) = [40.0, 1.145]
  cl(0:1,61) = [50.0, 1.086]
  cl(0:1,62) = [60.0, 0.919]
  cl(0:1,63) = [70.0, 0.683]
  cl(0:1,64) = [80.0, 0.405]
  cl(0:1,65) = [90.0, 0.106]
  cl(0:1,66) = [100.0, -0.203]
  cl(0:1,67) = [110.0, -0.493]
  cl(0:1,68) = [120.0, -0.741]
  cl(0:1,69) = [130.0, -0.927]
  cl(0:1,70) = [140.0, -1.014]
  cl(0:1,71) = [150.0, -0.903]
  cl(0:1,72) = [160.0, -0.615]
  cl(0:1,73) = [170.0, -0.951]
  cl(0:1,74) = [180.0, -0.391]

  IF ( y .EQ. cl(0,0) ) cl5 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl5 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl5 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl5_ge16.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl5 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl5_ge16.pro'
  ENDIF
  
!print,y,cl5
  
!_________________________________________________________________________
  
END FUNCTION calc_cl5_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl6_ge16( y ) RESULT( cl6 )
!_________________________________________________________________________
!
! Data from  GE 1.6 XLE: FAA-W3-211
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl6
  INTEGER :: i

  cl6 = 0.0

!                alpha    CL  
  cl(0:1, 0) = [-180.0,	-0.391]
  cl(0:1, 1) = [-170.0,	0.473]
  cl(0:1, 2) = [-160.0,	0.388]
  cl(0:1, 3) = [-150.0,	0.67]
  cl(0:1, 4) = [-140.0,	0.802]
  cl(0:1, 5) = [-130.0,	0.781]
  cl(0:1, 6) = [-120.0,	0.642]
  cl(0:1, 7) = [-110.0,	0.426]
  cl(0:1, 8) = [-100.0,	0.166]
  cl(0:1, 9) = [-90.0, -0.113]
  cl(0:1,10) = [-80.0, -0.39]
  cl(0:1,11) = [-70.0, -0.641]
  cl(0:1,12) = [-60.0, -0.842]
  cl(0:1,13) = [-50.0, -0.959]
  cl(0:1,14) = [-40.0, -0.949]
  cl(0:1,15) = [-30.0, -0.78934]
  cl(0:1,16) = [-25.0, -0.77078]
  cl(0:1,17) = [-20.0, -0.87776]
  cl(0:1,18) = [-19.0, -0.90728]
  cl(0:1,19) = [-18.0, -0.93681]
  cl(0:1,20) = [-17.0, -0.9689]
  cl(0:1,21) = [-16.0, -0.97403]
  cl(0:1,22) = [-15.0, -0.96319]
  cl(0:1,23) = [-14.0, -0.95921]
  cl(0:1,24) = [-13.0, -0.94962]
  cl(0:1,25) = [-12.0, -0.92511]
  cl(0:1,26) = [-11.0, -0.88296]
  cl(0:1,27) = [-10.0, -0.84792]
  cl(0:1,28) = [-9.0, -0.78977]
  cl(0:1,29) = [-8.0, -0.69869]
  cl(0:1,30) = [-7.0, -0.57414]
  cl(0:1,31) = [-6.0, -0.4225]
  cl(0:1,32) = [-5.0, -0.29444]
  cl(0:1,33) = [-4.0, -0.16488]
  cl(0:1,34) = [-3.0, -0.03579]
  cl(0:1,35) = [-2.0, 0.09105]
  cl(0:1,36) = [-1.0, 0.22416]
  cl(0:1,37) = [0.0, 0.35062]
  cl(0:1,38) = [1.0, 0.4771]
  cl(0:1,39) = [2.0, 0.6014]
  cl(0:1,40) = [3.0, 0.72624]
  cl(0:1,41) = [4.0, 0.85077]
  cl(0:1,42) = [5.0, 0.97496]
  cl(0:1,43) = [6.0, 1.09842]
  cl(0:1,44) = [7.0, 1.22172]
  cl(0:1,45) = [8.0, 1.33635]
  cl(0:1,46) = [9.0, 1.44121]
  cl(0:1,47) = [10.0, 1.53034]
  cl(0:1,48) = [11.0, 1.603]
  cl(0:1,49) = [12.0, 1.63949]
  cl(0:1,50) = [13.0, 1.42719]
  cl(0:1,51) = [14.0, 1.35731]
  cl(0:1,52) = [15.0, 1.32337]
  cl(0:1,53) = [16.0, 1.31678]
  cl(0:1,54) = [17.0, 1.30742]
  cl(0:1,55) = [18.0, 1.30074]
  cl(0:1,56) = [19.0, 1.29506]
  cl(0:1,57) = [20.0, 1.26458]
  cl(0:1,58) = [25.0, 1.11208]
  cl(0:1,59) = [30.0, 1.019]
  cl(0:1,60) = [40.0, 1.145]
  cl(0:1,61) = [50.0, 1.086]
  cl(0:1,62) = [60.0, 0.919]
  cl(0:1,63) = [70.0, 0.683]
  cl(0:1,64) = [80.0, 0.405]
  cl(0:1,65) = [90.0, 0.106]
  cl(0:1,66) = [100.0, -0.203]
  cl(0:1,67) = [110.0, -0.493]
  cl(0:1,68) = [120.0, -0.741]
  cl(0:1,69) = [130.0, -0.927]
  cl(0:1,70) = [140.0, -1.014]
  cl(0:1,71) = [150.0, -0.903]
  cl(0:1,72) = [160.0, -0.615]
  cl(0:1,73) = [170.0, -0.951]
  cl(0:1,74) = [180.0, -0.391]

  IF ( y .EQ. cl(0,0) ) cl6 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl6 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl6 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl6_ge16.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl6 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl6_ge16.pro'
  ENDIF
  
!print,y,cl6
  
!_________________________________________________________________________
  
END FUNCTION calc_cl6_ge16
!_________________________________________________________________________

!=========================================================================

Function calc_cl7_ge16( y ) RESULT( cl7 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU96-W-180
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl7
  INTEGER :: i

  cl7 = 0.0

!                alpha    CL  
  cl(0:1, 0) = [-180.0,	-0.391]
  cl(0:1, 1) = [-170.0,	0.473]
  cl(0:1, 2) = [-160.0,	0.388]
  cl(0:1, 3) = [-150.0,	0.67]
  cl(0:1, 4) = [-140.0,	0.802]
  cl(0:1, 5) = [-130.0,	0.781]
  cl(0:1, 6) = [-120.0,	0.642]
  cl(0:1, 7) = [-110.0,	0.426]
  cl(0:1, 8) = [-100.0,	0.166]
  cl(0:1, 9) = [-90.0, -0.113]
  cl(0:1,10) = [-80.0, -0.39]
  cl(0:1,11) = [-70.0, -0.641]
  cl(0:1,12) = [-60.0, -0.842]
  cl(0:1,13) = [-50.0, -0.959]
  cl(0:1,14) = [-40.0, -0.949]
  cl(0:1,15) = [-30.0, -0.77361]
  cl(0:1,16) = [-25.0, -0.72714]
  cl(0:1,17) = [-20.0, -0.69374]
  cl(0:1,18) = [-19.0, -0.67096]
  cl(0:1,19) = [-18.0, -0.64819]
  cl(0:1,20) = [-17.0, -0.76169]
  cl(0:1,21) = [-16.0, -1.05192]
  cl(0:1,22) = [-15.0, -1.12389]
  cl(0:1,23) = [-14.0, -1.12465]
  cl(0:1,24) = [-13.0, -1.08084]
  cl(0:1,25) = [-12.0, -1.00934]
  cl(0:1,26) = [-11.0, -0.92576]
  cl(0:1,27) = [-10.0, -0.83747]
  cl(0:1,28) = [-9.0, -0.73187]
  cl(0:1,29) = [-8.0, -0.62133]
  cl(0:1,30) = [-7.0, -0.51124]
  cl(0:1,31) = [-6.0, -0.40038]
  cl(0:1,32) = [-5.0, -0.28642]
  cl(0:1,33) = [-4.0, -0.17079]
  cl(0:1,34) = [-3.0, -0.05372]
  cl(0:1,35) = [-2.0, 0.06568]
  cl(0:1,36) = [-1.0, 0.18778]
  cl(0:1,37) = [0.0, 0.31595]
  cl(0:1,38) = [1.0, 0.43603]
  cl(0:1,39) = [2.0, 0.55565]
  cl(0:1,40) = [3.0, 0.67459]
  cl(0:1,41) = [4.0, 0.79052]
  cl(0:1,42) = [5.0, 0.9094]
  cl(0:1,43) = [6.0, 1.02287]
  cl(0:1,44) = [7.0, 1.1308]
  cl(0:1,45) = [8.0, 1.21453]
  cl(0:1,46) = [9.0, 1.27153]
  cl(0:1,47) = [10.0, 1.31765]
  cl(0:1,48) = [11.0, 1.33099]
  cl(0:1,49) = [12.0, 1.28428]
  cl(0:1,50) = [13.0, 1.25397]
  cl(0:1,51) = [14.0, 1.257]
  cl(0:1,52) = [15.0, 1.2483]
  cl(0:1,53) = [16.0, 1.2595]
  cl(0:1,54) = [17.0, 1.25968]
  cl(0:1,55) = [18.0, 1.26346]
  cl(0:1,56) = [19.0, 1.23931]
  cl(0:1,57) = [20.0, 1.21517]
  cl(0:1,58) = [25.0, 1.09361]
  cl(0:1,59) = [30.0, 1.019]
  cl(0:1,60) = [40.0, 1.145]
  cl(0:1,61) = [50.0, 1.086]
  cl(0:1,62) = [60.0, 0.919]
  cl(0:1,63) = [70.0, 0.683]
  cl(0:1,64) = [80.0, 0.405]
  cl(0:1,65) = [90.0, 0.106]
  cl(0:1,66) = [100.0, -0.203]
  cl(0:1,67) = [110.0, -0.493]
  cl(0:1,68) = [120.0, -0.741]
  cl(0:1,69) = [130.0, -0.927]
  cl(0:1,70) = [140.0, -1.014]
  cl(0:1,71) = [150.0, -0.903]
  cl(0:1,72) = [160.0, -0.615]
  cl(0:1,73) = [170.0, -0.951]
  cl(0:1,74) = [180.0, -0.391]
  
  IF ( y .EQ. cl(0,0) ) cl7 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl7 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl7 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl7_ge16.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl7 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl7_ge16.pro'
  ENDIF
  
!print,y,cl7
  
!_________________________________________________________________________
  
END FUNCTION calc_cl7_ge16
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd_ge16( x,y ) RESULT( cd )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from GE 1.6 XLE
!
! calc_cd1_ge16 uses data from Cylinder
! calc_cd2_ge16 uses data from DU96-392
! calc_cd3_ge16 uses data from DU96-350
! calc_cd4_ge16 uses data from DU97-W-300
! calc_cd5_ge16 uses data from DU91-W2-250
! calc_cd6_ge16 uses data from FAA-W3-211
! calc_cd7_ge16 uses data from DU96-W-180
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  REAL, PARAMETER :: ns = 8

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cd
  INTEGER :: i

  cd = 0.0

  r(0) = 1.741282468
  r(1) = 4.286233766
  r(2) = 8.572467532
  r(3) = 11.78714286
  r(4) = 16.07337662
  r(5) = 24.64584416
  r(6) = 32.14675325
  r(7) = 41.22821104

  IF ( ( x .GT. r( 0) ) .AND. (x .LE. r( 1) ) ) THEN
       cd = calc_cd1_ge16(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. (x .LE. r( 2) ) ) THEN
       cd = calc_cd2_ge16(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. (x .LE. r( 3) ) ) THEN
       cd = calc_cd3_ge16(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. (x .LE. r( 4) ) ) THEN
       cd = calc_cd4_ge16(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. (x .LE. r( 5) ) ) THEN
       cd = calc_cd5_ge16(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. (x .LE. r( 6) ) ) THEN
       cd = calc_cd6_ge16(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. (x .LE. r( 7) ) ) THEN
       cd = calc_cd7_ge16(y)
  ENDIF
  
  IF ( x .LT. r(0) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cd_ge16.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
  IF ( x .GT. r(ns-1) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cd_ge16.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd_ge16
!_________________________________________________________________________

FUNCTION calc_cd1_ge16( y ) RESULT( cd1 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: Cylinder
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cd1

  cd1 = 0.3
!_________________________________________________________________________
  
END FUNCTION calc_cd1_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd2_ge16( y ) RESULT( cd2 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU96-392
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd2
  INTEGER :: i

  cd2 = 0.0

!               alpha    CD   
  cd(0:1, 0) = [-180.0, 0.06592]
  cd(0:1, 1) = [-170.0,	0.10013]
  cd(0:1, 2) = [-160.0,	0.28486]
  cd(0:1, 3) = [-150.0,	0.56687]
  cd(0:1, 4) = [-140.0,	0.86296]
  cd(0:1, 5) = [-130.0,	1.10086]
  cd(0:1, 6) = [-120.0,	1.29394]
  cd(0:1, 7) = [-110.0,	1.43968]
  cd(0:1, 8) = [-100.0, 1.52615]
  cd(0:1, 9) = [-90.0, 1.54302]
  cd(0:1,10) = [-80.0, 1.48906]
  cd(0:1,11) = [-70.0, 1.37344]
  cd(0:1,12) = [-60.0, 1.21055]
  cd(0:1,13) = [-50.0, 1.00804]
  cd(0:1,14) = [-40.0, 0.75172]
  cd(0:1,15) = [-30.0, 0.31189]
  cd(0:1,16) = [-25.0, 0.21]
  cd(0:1,17) = [-20.0, 0.14285]
  cd(0:1,18) = [-19.0, 0.13416]
  cd(0:1,19) = [-18.0, 0.12581]
  cd(0:1,20) = [-17.0, 0.11746]
  cd(0:1,21) = [-16.0, 0.10911]
  cd(0:1,22) = [-15.0, 0.10076]
  cd(0:1,23) = [-14.0, 0.09241]
  cd(0:1,24) = [-13.0, 0.08404]
  cd(0:1,25) = [-12.0, 0.07478]
  cd(0:1,26) = [-11.0, 0.0668]
  cd(0:1,27) = [-10.0, 0.06024]
  cd(0:1,28) = [-9.0, 0.05397]
  cd(0:1,29) = [-8.0, 0.04809]
  cd(0:1,30) = [-7.0, 0.04245]
  cd(0:1,31) = [-6.0, 0.03708]
  cd(0:1,32) = [-5.0, 0.032]
  cd(0:1,33) = [-4.0, 0.02698]
  cd(0:1,34) = [-3.0, 0.02221]
  cd(0:1,35) = [-2.0, 0.01822]
  cd(0:1,36) = [-1.0, 0.01448]
  cd(0:1,37) = [0.0, 0.01351]
  cd(0:1,38) = [1.0, 0.013]
  cd(0:1,39) = [2.0, 0.01266]
  cd(0:1,40) = [3.0, 0.01291]
  cd(0:1,41) = [4.0, 0.01335]
  cd(0:1,42) = [5.0, 0.01439]
  cd(0:1,43) = [6.0, 0.01653]
  cd(0:1,44) = [7.0, 0.02067]
  cd(0:1,45) = [8.0, 0.02843]
  cd(0:1,46) = [9.0, 0.03619]
  cd(0:1,47) = [10.0, 0.04395]
  cd(0:1,48) = [11.0, 0.05171]
  cd(0:1,49) = [12.0, 0.05947]
  cd(0:1,50) = [13.0, 0.06723]
  cd(0:1,51) = [14.0, 0.07499]
  cd(0:1,52) = [15.0, 0.0845]
  cd(0:1,53) = [16.0, 0.09951]
  cd(0:1,54) = [17.0, 0.11827]
  cd(0:1,55) = [18.0, 0.135]
  cd(0:1,56) = [19.0, 0.148]
  cd(0:1,57) = [20.0, 0.165]
  cd(0:1,58) = [25.0, 0.24787]
  cd(0:1,59) = [30.0, 0.36962]
  cd(0:1,60) = [40.0, 0.85506]
  cd(0:1,61) = [50.0, 1.11802]
  cd(0:1,62) = [60.0, 1.32205]
  cd(0:1,63) = [70.0, 1.46615]
  cd(0:1,64) = [80.0, 1.55172]
  cd(0:1,65) = [90.0, 1.57893]
  cd(0:1,66) = [100.0, 1.54624]
  cd(0:1,67) = [110.0, 1.45133]
  cd(0:1,68) = [120.0, 1.292]
  cd(0:1,69) = [130.0, 1.06662]
  cd(0:1,70) = [140.0, 0.77371]
  cd(0:1,71) = [150.0, 0.41076]
  cd(0:1,72) = [160.0, 0.25542]
  cd(0:1,73) = [170.0, 0.0727]
  cd(0:1,74) = [180.0, 0.06592]
  
  IF ( y .EQ. cd(0,0) ) cd2 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd2 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd2 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd2_ge16.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd2 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd2_ge16.pro'
  ENDIF
  
!print,y,cd2
  
!_________________________________________________________________________
  
END FUNCTION calc_cd2_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd3_ge16( y ) RESULT( cd3 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE DU96-350
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd3
  INTEGER :: i

  cd3 = 0.0

!                 alpha    CD   
  cd(0:1, 0) = [-180.0, 0.06821]
  cd(0:1, 1) = [-170.0, 0.1036]
  cd(0:1, 2) = [-160.0, 0.29472]
  cd(0:1, 3) = [-150.0, 0.58649]
  cd(0:1, 4) = [-140.0, 0.89283]
  cd(0:1, 5) = [-130.0, 1.13896]
  cd(0:1, 6) = [-120.0, 1.33872]
  cd(0:1, 7) = [-110.0, 1.48951]
  cd(0:1, 8) = [-100.0, 1.57896]
  cd(0:1, 9) = [-90.0, 1.59643]
  cd(0:1,10) = [-80.0, 1.5406]
  cd(0:1,11) = [-70.0, 1.42097]
  cd(0:1,12) = [-60.0, 1.25245]
  cd(0:1,13) = [-50.0, 1.04293]
  cd(0:1,14) = [-40.0, 0.77774]
  cd(0:1,15) = [-30.0, 0.32274]
  cd(0:1,16) = [-25.0, 0.20447]
  cd(0:1,17) = [-20.0, 0.14779]
  cd(0:1,18) = [-19.0, 0.1388]
  cd(0:1,19) = [-18.0, 0.12975]
  cd(0:1,20) = [-17.0, 0.12083]
  cd(0:1,21) = [-16.0, 0.11191]
  cd(0:1,22) = [-15.0, 0.103]
  cd(0:1,23) = [-14.0, 0.094]
  cd(0:1,24) = [-13.0, 0.082]
  cd(0:1,25) = [-12.0, 0.072]
  cd(0:1,26) = [-11.0, 0.06]
  cd(0:1,27) = [-10.0, 0.045]
  cd(0:1,28) = [-9.0, 0.038]
  cd(0:1,29) = [-8.0, 0.03]
  cd(0:1,30) = [-7.0, 0.025]
  cd(0:1,31) = [-6.0, 0.018]
  cd(0:1,32) = [-5.0, 0.015]
  cd(0:1,33) = [-4.0, 0.01315]
  cd(0:1,34) = [-3.0, 0.01229]
  cd(0:1,35) = [-2.0, 0.01184]
  cd(0:1,36) = [-1.0, 0.01158]
  cd(0:1,37) = [0.0, 0.01142]
  cd(0:1,38) = [1.0, 0.01117]
  cd(0:1,39) = [2.0, 0.01106]
  cd(0:1,40) = [3.0, 0.01123]
  cd(0:1,41) = [4.0, 0.01168]
  cd(0:1,42) = [5.0, 0.0123]
  cd(0:1,43) = [6.0, 0.0131]
  cd(0:1,44) = [7.0, 0.01419]
  cd(0:1,45) = [8.0, 0.01522]
  cd(0:1,46) = [9.0, 0.01675]
  cd(0:1,47) = [10.0, 0.02003]
  cd(0:1,48) = [11.0, 0.0282]
  cd(0:1,49) = [12.0, 0.036]
  cd(0:1,50) = [13.0, 0.0485]
  cd(0:1,51) = [14.0, 0.058]
  cd(0:1,52) = [15.0, 0.069]
  cd(0:1,53) = [16.0, 0.079]
  cd(0:1,54) = [17.0, 0.09268]
  cd(0:1,55) = [18.0, 0.1097]
  cd(0:1,56) = [19.0, 0.11912]
  cd(0:1,57) = [20.0, 0.13296]
  cd(0:1,58) = [25.0, 0.25645]
  cd(0:1,59) = [30.0, 0.38241]
  cd(0:1,60) = [40.0, 0.88465]
  cd(0:1,61) = [50.0, 1.15671]
  cd(0:1,62) = [60.0, 1.3678]
  cd(0:1,63) = [70.0, 1.51689]
  cd(0:1,64) = [80.0, 1.60542]
  cd(0:1,65) = [90.0, 1.63357]
  cd(0:1,66) = [100.0, 1.59975]
  cd(0:1,67) = [110.0, 1.50156]
  cd(0:1,68) = [120.0, 1.33672]
  cd(0:1,69) = [130.0, 1.10354]
  cd(0:1,70) = [140.0, 0.80048]
  cd(0:1,71) = [150.0, 0.42498]
  cd(0:1,72) = [160.0, 0.26426]
  cd(0:1,73) = [170.0, 0.07521]
  cd(0:1,74) = [180.0, 0.06821]
  
  IF ( y .EQ. cd(0,0) ) cd3 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd3 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd3 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd3_ge16.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd3 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd3_ge16.pro'
  ENDIF
  
!print,y,cd3
  
!_________________________________________________________________________
  
END FUNCTION calc_cd3_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd4_ge16( y ) RESULT( cd4 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU97-W-300
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd4
  INTEGER :: i

  cd4 = 0.0

!                alpha    CD  
  cd(0:1, 0) = [-180.0,	0.07098]
  cd(0:1, 1) = [-170.0,	0.10781]
  cd(0:1, 2) = [-160.0,	0.3067]
  cd(0:1, 3) = [-150.0,	0.61034]
  cd(0:1, 4) = [-140.0,	0.92913]
  cd(0:1, 5) = [-130.0,	1.18528]
  cd(0:1, 6) = [-120.0,	1.39316]
  cd(0:1, 7) = [-110.0,	1.55008]
  cd(0:1, 8) = [-100.0,	1.64317]
  cd(0:1, 9) = [-90.0, 1.66135]
  cd(0:1,10) = [-80.0, 1.60325]
  cd(0:1,11) = [-70.0, 1.47876]
  cd(0:1,12) = [-60.0, 1.30338]
  cd(0:1,13) = [-50.0, 1.08534]
  cd(0:1,14) = [-40.0, 0.80936]
  cd(0:1,15) = [-30.0, 0.33579]
  cd(0:1,16) = [-25.0, 0.21278]
  cd(0:1,17) = [-20.0, 0.1538]
  cd(0:1,18) = [-19.0, 0.14444]
  cd(0:1,19) = [-18.0, 0.13527]
  cd(0:1,20) = [-17.0, 0.12609]
  cd(0:1,21) = [-16.0, 0.11692]
  cd(0:1,22) = [-15.0, 0.10774]
  cd(0:1,23) = [-14.0, 0.09857]
  cd(0:1,24) = [-13.0, 0.08939]
  cd(0:1,25) = [-12.0, 0.08022]
  cd(0:1,26) = [-11.0, 0.07105]
  cd(0:1,27) = [-10.0, 0.06187]
  cd(0:1,28) = [-9.0, 0.0527]
  cd(0:1,29) = [-8.0, 0.04352]
  cd(0:1,30) = [-7.0, 0.03435]
  cd(0:1,31) = [-6.0, 0.02517]
  cd(0:1,32) = [-5.0, 0.016]
  cd(0:1,33) = [-4.0, 0.01105]
  cd(0:1,34) = [-3.0, 0.01071]
  cd(0:1,35) = [-2.0, 0.01034]
  cd(0:1,35) = [-1.0, 0.0103]
  cd(0:1,37) = [0.0, 0.01022]
  cd(0:1,38) = [1.0, 0.01032]
  cd(0:1,39) = [2.0, 0.0104]
  cd(0:1,40) = [3.0, 0.01052]
  cd(0:1,41) = [4.0, 0.01079]
  cd(0:1,42) = [5.0, 0.01108]
  cd(0:1,43) = [6.0, 0.01128]
  cd(0:1,44) = [7.0, 0.01179]
  cd(0:1,45) = [8.0, 0.01245]
  cd(0:1,46) = [9.0, 0.01321]
  cd(0:1,47) = [10.0, 0.01435]
  cd(0:1,48) = [11.0, 0.01654]
  cd(0:1,49) = [12.0, 0.01981]
  cd(0:1,50) = [13.0, 0.02797]
  cd(0:1,51) = [14.0, 0.04366]
  cd(0:1,52) = [15.0, 0.06225]
  cd(0:1,53) = [16.0, 0.07903]
  cd(0:1,54) = [17.0, 0.09644]
  cd(0:1,55) = [18.0, 0.11416]
  cd(0:1,56) = [19.0, 0.12397]
  cd(0:1,57) = [20.0, 0.13837]
  cd(0:1,58) = [25.0, 0.26688]
  cd(0:1,59) = [30.0, 0.39796]
  cd(0:1,60) = [40.0, 0.92063]
  cd(0:1,61) = [50.0, 1.20375]
  cd(0:1,62) = [60.0, 1.42343]
  cd(0:1,63) = [70.0, 1.57857]
  cd(0:1,64) = [80.0, 1.67071]
  cd(0:1,65) = [90.0, 1.7]
  cd(0:1,66) = [100.0, 1.66481]
  cd(0:1,67) = [110.0, 1.56262]
  cd(0:1,68) = [120.0, 1.39107]
  cd(0:1,69) = [130.0, 1.14841]
  cd(0:1,70) = [140.0, 0.83304]
  cd(0:1,71) = [150.0, 0.44226]
  cd(0:1,72) = [160.0, 0.27501]
  cd(0:1,73) = [170.0, 0.07827]
  cd(0:1,74) = [180.0, 0.07098]

  IF ( y .EQ. cd(0,0) ) cd4 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd4 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd4 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd4_ge16.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd4 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd4_ge16.pro'
  ENDIF
  
!print,y,cd4
  
!_________________________________________________________________________
  
END FUNCTION calc_cd4_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd5_ge16( y ) RESULT( cd5 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU91-W2-250
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd5
  INTEGER :: i

  cd5 = 0.0

!                alpha    CD  
  cd(0:1, 0) = [-180.0,	0.02596]
  cd(0:1, 1) = [-170.0,	0.10583]
  cd(0:1, 2) = [-160.0,	0.28173]
  cd(0:1, 3) = [-150.0,	0.5693]
  cd(0:1, 4) = [-140.0,	0.9099]
  cd(0:1, 5) = [-130.0,	1.22703]
  cd(0:1, 6) = [-120.0,	1.48434]
  cd(0:1, 7) = [-110.0,	1.66829]
  cd(0:1, 8) = [-100.0,	1.77601]
  cd(0:1, 9) = [-90.0, 1.80848]
  cd(0:1,10) = [-80.0, 1.76697]
  cd(0:1,11) = [-70.0, 1.65188]
  cd(0:1,12) = [-60.0, 1.46377]
  cd(0:1,13) = [-50.0, 1.20657]
  cd(0:1,14) = [-40.0, 0.89373]
  cd(0:1,15) = [-30.0, 0.52112]
  cd(0:1,16) = [-25.0, 0.24591]
  cd(0:1,17) = [-20.0, 0.15229]
  cd(0:1,18) = [-19.0, 0.14145]
  cd(0:1,19) = [-18.0, 0.13059]
  cd(0:1,20) = [-17.0, 0.11959]
  cd(0:1,21) = [-16.0, 0.10499]
  cd(0:1,22) = [-15.0, 0.07348]
  cd(0:1,23) = [-14.0, 0.06281]
  cd(0:1,24) = [-13.0, 0.05203]
  cd(0:1,25) = [-12.0, 0.04099]
  cd(0:1,26) = [-11.0, 0.03145]
  cd(0:1,27) = [-10.0, 0.02427]
  cd(0:1,28) = [-9.0, 0.01802]
  cd(0:1,29) = [-8.0, 0.01327]
  cd(0:1,30) = [-7.0, 0.01061]
  cd(0:1,31) = [-6.0, 0.0089]
  cd(0:1,32) = [-5.0, 0.00814]
  cd(0:1,33) = [-4.0, 0.00781]
  cd(0:1,34) = [-3.0, 0.00759]
  cd(0:1,35) = [-2.0, 0.00754]
  cd(0:1,36) = [-1.0, 0.00751]
  cd(0:1,37) = [0.0, 0.00749]
  cd(0:1,38) = [1.0, 0.00757]
  cd(0:1,39) = [2.0, 0.00769]
  cd(0:1,40) = [3.0, 0.00788]
  cd(0:1,41) = [4.0, 0.00806]
  cd(0:1,42) = [5.0, 0.00831]
  cd(0:1,43) = [6.0, 0.00866]
  cd(0:1,44) = [7.0, 0.00931]
  cd(0:1,45) = [8.0, 0.01128]
  cd(0:1,46) = [9.0, 0.01551]
  cd(0:1,47) = [10.0, 0.02]
  cd(0:1,48) = [11.0, 0.03501]
  cd(0:1,49) = [12.0, 0.05002]
  cd(0:1,50) = [13.0, 0.06502]
  cd(0:1,51) = [14.0, 0.08003]
  cd(0:1,52) = [15.0, 0.09504]
  cd(0:1,53) = [16.0, 0.11005]
  cd(0:1,54) = [17.0, 0.12506]
  cd(0:1,55) = [18.0, 0.14006]
  cd(0:1,56) = [19.0, 0.15507]
  cd(0:1,57) = [20.0, 0.17008]
  cd(0:1,58) = [25.0, 0.24512]
  cd(0:1,59) = [30.0, 0.62939]
  cd(0:1,60) = [40.0, 0.9807]
  cd(0:1,61) = [50.0, 1.28443]
  cd(0:1,62) = [60.0, 1.53704]
  cd(0:1,63) = [70.0, 1.72851]
  cd(0:1,64) = [80.0, 1.84793]
  cd(0:1,65) = [90.0, 1.88889]
  cd(0:1,66) = [100.0, 1.84926]
  cd(0:1,67) = [110.0, 1.73376]
  cd(0:1,68) = [120.0, 1.55362]
  cd(0:1,69) = [130.0, 1.318]
  cd(0:1,70) = [140.0, 1.02302]
  cd(0:1,71) = [150.0, 0.64743]
  cd(0:1,72) = [160.0, 0.30499]
  cd(0:1,73) = [170.0, 0.10766]
  cd(0:1,74) = [180.0, 0.02596]
  
  IF ( y .EQ. cd(0,0) ) cd5 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd5 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd5 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd5_ge16.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd5 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd5_ge16.pro'
  ENDIF
  
!print,y,cd5
  
!_________________________________________________________________________
  
END FUNCTION calc_cd5_ge16
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd6_ge16( y ) RESULT( cd6 )
!_________________________________________________________________________
!
! Data from  GE 1.6 XLE: FAA-W3-211
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd6
  INTEGER :: i

  cd6 = 0.0

!                alpha    CD  
  cd(0:1, 0) = [-180.0,	0.02526]
  cd(0:1, 1) = [-170.0,	0.10297]
  cd(0:1, 2) = [-160.0,	0.27411]
  cd(0:1, 3) = [-150.0,	0.55392]
  cd(0:1, 4) = [-140.0,	0.88531]
  cd(0:1, 5) = [-130.0,	1.19387]
  cd(0:1, 6) = [-120.0,	1.44422]
  cd(0:1, 7) = [-110.0,	1.6232]
  cd(0:1, 8) = [-100.0,	1.72801]
  cd(0:1, 9) = [-90.0, 1.7596]
  cd(0:1,10) = [-80.0, 1.71921]
  cd(0:1,11) = [-70.0, 1.60724]
  cd(0:1,12) = [-60.0, 1.42421]
  cd(0:1,13) = [-50.0, 1.17396]
  cd(0:1,14) = [-40.0, 0.86957]
  cd(0:1,15) = [-30.0, 0.51001]
  cd(0:1,16) = [-25.0, 0.23926]
  cd(0:1,17) = [-20.0, 0.14818]
  cd(0:1,18) = [-19.0, 0.13676]
  cd(0:1,19) = [-18.0, 0.12535]
  cd(0:1,20) = [-17.0, 0.11539]
  cd(0:1,21) = [-16.0, 0.10627]
  cd(0:1,22) = [-15.0, 0.07023]
  cd(0:1,23) = [-14.0, 0.06304]
  cd(0:1,24) = [-13.0, 0.05584]
  cd(0:1,25) = [-12.0, 0.04865]
  cd(0:1,26) = [-11.0, 0.04146]
  cd(0:1,27) = [-10.0, 0.03426]
  cd(0:1,28) = [-9.0, 0.02707]
  cd(0:1,29) = [-8.0, 0.01987]
  cd(0:1,30) = [-7.0, 0.01268]
  cd(0:1,31) = [-6.0, 0.00733]
  cd(0:1,32) = [-5.0, 0.00681]
  cd(0:1,33) = [-4.0, 0.00667]
  cd(0:1,34) = [-3.0, 0.00657]
  cd(0:1,35) = [-2.0, 0.00649]
  cd(0:1,36) = [-1.0, 0.00656]
  cd(0:1,37) = [0.0, 0.00665]
  cd(0:1,38) = [1.0, 0.00676]
  cd(0:1,39) = [2.0, 0.00694]
  cd(0:1,40) = [3.0, 0.00713]
  cd(0:1,41) = [4.0, 0.0074]
  cd(0:1,42) = [5.0, 0.00776]
  cd(0:1,43) = [6.0, 0.0081]
  cd(0:1,44) = [7.0, 0.0087]
  cd(0:1,45) = [8.0, 0.00975]
  cd(0:1,46) = [9.0, 0.0116]
  cd(0:1,47) = [10.0, 0.01478]
  cd(0:1,48) = [11.0, 0.01896]
  cd(0:1,49) = [12.0, 0.03023]
  cd(0:1,50) = [13.0, 0.05487]
  cd(0:1,51) = [14.0, 0.07447]
  cd(0:1,52) = [15.0, 0.09256]
  cd(0:1,53) = [16.0, 0.11066]
  cd(0:1,54) = [17.0, 0.12875]
  cd(0:1,55) = [18.0, 0.14684]
  cd(0:1,56) = [19.0, 0.16493]
  cd(0:1,57) = [20.0, 0.18303]
  cd(0:1,58) = [25.0, 0.27349]
  cd(0:1,59) = [30.0, 0.61238]
  cd(0:1,60) = [40.0, 0.9542]
  cd(0:1,61) = [50.0, 1.24971]
  cd(0:1,62) = [60.0, 1.4955]
  cd(0:1,63) = [70.0, 1.6818]
  cd(0:1,64) = [80.0, 1.79798]
  cd(0:1,65) = [90.0, 1.83784]
  cd(0:1,66) = [100.0, 1.79928]
  cd(0:1,67) = [110.0, 1.68691]
  cd(0:1,68) = [120.0, 1.51163]
  cd(0:1,69) = [130.0, 1.28238]
  cd(0:1,70) = [140.0, 0.99537]
  cd(0:1,71) = [150.0, 0.62994]
  cd(0:1,72) = [160.0, 0.29675]
  cd(0:1,73) = [170.0, 0.10475]
  cd(0:1,74) = [180.0, 0.02526]
  
  IF ( y .EQ. cd(0,0) ) cd6 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd6 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd6 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd6_ge16.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd6 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd6_ge16.pro'
  ENDIF
  
!print,y,cd6
  
!_________________________________________________________________________
  
END FUNCTION calc_cd6_ge16
!_________________________________________________________________________

!=========================================================================

Function calc_cd7_ge16( y ) RESULT( cd7 )
!_________________________________________________________________________
!
! Data from GE 1.6 XLE: DU96-W-180
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 75

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd7
  INTEGER :: i

  cd7 = 0.0

!              alpha    CD  
  cd(0:1, 0) = [-180.0,	0.0263]
  cd(0:1, 1) = [-170.0,	0.10723]
  cd(0:1, 2) = [-160.0,	0.28544]
  cd(0:1, 3) = [-150.0,	0.57681]
  cd(0:1, 4) = [-140.0,	0.9219]
  cd(0:1, 5) = [-130.0,	1.24321]
  cd(0:1, 6) = [-120.0,	1.50391]
  cd(0:1, 7) = [-110.0,	1.69028]
  cd(0:1, 8) = [-100.0,	1.79942]
  cd(0:1, 9) = [-90.0, 1.83232]
  cd(0:1,10) = [-80.0, 1.79026]
  cd(0:1,11) = [-70.0, 1.67366]
  cd(0:1,12) = [-60.0, 1.48307]
  cd(0:1,13) = [-50.0, 1.22248]
  cd(0:1,14) = [-40.0, 0.90551]
  cd(0:1,15) = [-30.0, 0.53612]
  cd(0:1,16) = [-25.0, 0.24915]
  cd(0:1,17) = [-20.0, 0.1543]
  cd(0:1,18) = [-19.0, 0.14242]
  cd(0:1,19) = [-18.0, 0.13053]
  cd(0:1,20) = [-17.0, 0.11958]
  cd(0:1,21) = [-16.0, 0.10864]
  cd(0:1,22) = [-15.0, 0.07023]
  cd(0:1,23) = [-14.0, 0.03181]
  cd(0:1,24) = [-13.0, 0.02568]
  cd(0:1,25) = [-12.0, 0.01812]
  cd(0:1,26) = [-11.0, 0.01567]
  cd(0:1,27) = [-10.0, 0.01352]
  cd(0:1,28) = [-9.0, 0.01226]
  cd(0:1,29) = [-8.0, 0.01111]
  cd(0:1,30) = [-7.0, 0.01017]
  cd(0:1,31) = [-6.0, 0.00937]
  cd(0:1,32) = [-5.0, 0.00861]
  cd(0:1,33) = [-4.0, 0.00784]
  cd(0:1,34) = [-3.0, 0.00711]
  cd(0:1,35) = [-2.0, 0.00639]
  cd(0:1,36) = [-1.0, 0.00605]
  cd(0:1,37) = [0.0, 0.00595]
  cd(0:1,38) = [1.0, 0.00595]
  cd(0:1,39) = [2.0, 0.00602]
  cd(0:1,40) = [3.0, 0.00618]
  cd(0:1,41) = [4.0, 0.00643]
  cd(0:1,42) = [5.0, 0.00677]
  cd(0:1,43) = [6.0, 0.00719]
  cd(0:1,44) = [7.0, 0.00783]
  cd(0:1,45) = [8.0, 0.01017]
  cd(0:1,46) = [9.0, 0.01423]
  cd(0:1,47) = [10.0, 0.01825]
  cd(0:1,48) = [11.0, 0.02197]
  cd(0:1,49) = [12.0, 0.02996]
  cd(0:1,50) = [13.0, 0.05764]
  cd(0:1,51) = [14.0, 0.07911]
  cd(0:1,52) = [15.0, 0.08656]
  cd(0:1,53) = [16.0, 0.09923]
  cd(0:1,54) = [17.0, 0.09986]
  cd(0:1,55) = [18.0, 0.11114]
  cd(0:1,56) = [19.0, 0.13101]
  cd(0:1,57) = [20.0, 0.15088]
  cd(0:1,58) = [25.0, 0.24835]
  cd(0:1,59) = [30.0, 0.63769]
  cd(0:1,60) = [40.0, 0.99363]
  cd(0:1,61) = [50.0, 1.30136]
  cd(0:1,62) = [60.0, 1.5573]
  cd(0:1,63) = [70.0, 1.7513]
  cd(0:1,64) = [80.0, 1.87229]
  cd(0:1,65) = [90.0, 1.91379]
  cd(0:1,66) = [100.0, 1.87364]
  cd(0:1,67) = [110.0, 1.75662]
  cd(0:1,68) = [120.0, 1.5741]
  cd(0:1,69) = [130.0, 1.33538]
  cd(0:1,70) = [140.0, 1.03651]
  cd(0:1,71) = [150.0, 0.65597]
  cd(0:1,72) = [160.0, 0.30901]
  cd(0:1,73) = [170.0, 0.10908]
  cd(0:1,74) = [180.0, 0.0263]

  IF ( y .EQ. cd(0,0) ) cd7 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd7 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd7 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd7_ge16.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd7 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd7_ge16.pro'
  ENDIF
  
!print,y,cd7
  
!_________________________________________________________________________
  
END FUNCTION calc_cd7_ge16
!_________________________________________________________________________

END MODULE module_gad_ge16

