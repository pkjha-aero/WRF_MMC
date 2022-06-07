MODULE module_gad_dtu10

REAL, PARAMETER :: pi=3.1415926535897932384626433

CONTAINS

!===============================================================================
!
!  DTU 10 MW Reference Wind Turbine
!
!===============================================================================

!=========================================================================

FUNCTION calc_rpm_dtu10( x ) result( rpm )

!_________________________________________________________________________
!
! Data from DTU 10 MW
! x =  wind speed
!_________________________________________________________________________


  REAL, INTENT( in ) :: x

  REAL, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws
  REAL, DIMENSION( 0:nws-1 ) :: rp

  REAL :: rpm
  INTEGER :: i

  rpm = 0.0

  rp(0) =  0.03
  rp(1) =  0.62
  rp(2) =  2.70
  rp(3) =  3.85
  rp(4) =  4.84
  rp(5) =  5.82
  rp(6) =  6.00
  rp(7) =  6.00
  rp(8) =  6.00
  rp(9) =  6.00
  rp(10) = 6.00
  rp(11) = 6.00
  rp(12) = 6.00
  rp(13) = 6.00
  rp(14) = 6.00
  rp(15) = 6.33
  rp(16) = 6.72
  rp(17) = 7.12
  rp(18) = 7.51
  rp(19) = 7.91
  rp(20) = 8.30
  rp(21) = 8.83
  rp(22) = 9.60
  rp(23) = 9.60
  rp(24) = 9.60
  rp(25) = 9.60
  rp(26) = 9.60
  rp(27) = 9.60
  rp(28) = 9.60
  rp(29) = 9.60
  rp(30) = 9.60
  rp(31) = 9.60
  rp(32) = 9.60
  rp(33) = 9.60
  rp(34) = 9.60
  rp(35) = 9.60
  rp(36) = 9.60
  rp(37) = 9.60
  rp(38) = 9.60
  rp(39) = 9.60
  rp(40) = 9.60
  rp(41) = 9.60
  rp(42) = 9.60
  rp(43) = 9.60
  rp(44) = 9.60
  rp(45) = 9.60
  rp(46) = 9.60
  rp(47) = 9.60
  rp(48) = 9.60
  rp(49) = 9.60

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

END FUNCTION calc_rpm_dtu10
!_________________________________________________________________________
 
!=========================================================================

FUNCTION calc_pitch_dtu10( x ) RESULT( pitch )

!_________________________________________________________________________
!
! Data from DTU 10 MW
! x = wind speeds
!_________________________________________________________________________
 
  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws 
  REAL, DIMENSION( 0:nws-1 ) :: p

  REAL :: pitch
  INTEGER :: i

  pitch = 0.0

  p(0) =  2.68
  p(1) =  2.68
  p(2) =  2.68
  p(3) =  2.68
  p(4) =  2.68
  p(5) =  2.68
  p(6) =  2.68
  p(7) =  2.68
  p(8) =  2.29
  p(9) =  1.90
  p(10) = 1.38
  p(11) = 0.86
  p(12) = 0.43
  p(13) = 0.00
  p(14) = 0.00
  p(15) = 0.00
  p(16) = 0.00
  p(17) = 0.00
  p(18) = 0.00
  p(19) = 0.00
  p(20) = 0.00
  p(21) = 0.00
  p(22) = 4.17
  p(23) = 5.79
  p(24) = 7.06
  p(25) = 8.14
  p(26) = 9.12
  p(27) = 10.01
  p(28) = 10.86
  p(29) = 11.65
  p(30) = 12.41
  p(31) = 13.15
  p(32) = 13.86
  p(33) = 14.56
  p(34) = 15.24
  p(35) = 15.89
  p(36) = 16.52
  p(37) = 17.14
  p(38) = 17.73
  p(39) = 18.32
  p(40) = 18.90
  p(41) = 19.47
  p(42) = 20.03
  p(43) = 20.58
  p(44) = 21.12
  p(45) = 21.66
  p(46) = 22.18
  p(47) = 22.70
  p(48) = 23.22
  p(49) = 23.72
  
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

END FUNCTION calc_pitch_dtu10

!=========================================================================

FUNCTION calc_fnft_dtu10( v0, rho, blades, bladelen, diameter,           &
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

        xideg = calc_twist_dtu10( radius/dscale ) + pitchdeg
        xi    = xideg*pi/180.0
!
!    --Cord length 

        cord  = cscale * calc_chord_dtu10( radius/dscale )
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
     
           cl = calc_cl_dtu10( radius/dscale, betadeg )
           cd = calc_cd_dtu10( radius/dscale, betadeg )
  
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
  
       cl = calc_cl_dtu10( radius/dscale, betadeg )
       cd = calc_cd_dtu10( radius/dscale, betadeg )

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

END FUNCTION calc_fnft_dtu10

!=========================================================================

FUNCTION calc_twist_dtu10( x ) RESULT( twist )
!_________________________________________________________________________
!
! Computes blade twist as a function of radial distance from hub center
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: ns = 40

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: t

  REAL :: twist 
  INTEGER :: i

  twist = 0.0
  
  r(0) =  2.8000
  r(1) =  5.4430
  r(2) =  8.1798
  r(3) =  11.0028
  r(4) =  13.9033
  r(5) =  16.8712
  r(6) =  19.8957
  r(7) =  22.9646
  r(8) =  26.0653
  r(9) =  29.1846
  r(10) = 32.3089
  r(11) = 35.4245
  r(12) = 38.5179
  r(13) = 41.5760
  r(14) = 44.5862
  r(15) = 47.5369
  r(16) = 50.4172
  r(17) = 53.2174
  r(18) = 55.9292
  r(19) = 58.5453
  r(20) = 61.0600
  r(21) = 63.4685
  r(22) = 65.7676
  r(23) = 67.9552
  r(24) = 70.0303
  r(25) = 71.9930
  r(26) = 73.8442
  r(27) = 75.5858
  r(28) = 77.2203
  r(29) = 78.7507
  r(30) = 80.1806
  r(31) = 81.5138
  r(32) = 82.7547
  r(33) = 83.9076
  r(34) = 84.9770
  r(35) = 85.9675
  r(36) = 86.8836
  r(37) = 87.7299
  r(38) = 88.5107
  r(39) = 89.2000
 
  t(0) =  14.5000
  t(1) =  14.5000
  t(2) =  14.5000
  t(3) =  14.4320
  t(4) =  13.8888
  t(5) =  12.5460
  t(6) =  10.6087
  t(7) =  8.8925
  t(8) =  7.8018
  t(9) =  7.0242
  t(10) = 6.3822
  t(11) = 5.7769
  t(12) = 5.2275
  t(13) = 4.6736
  t(14) = 4.0886
  t(15) = 3.4878
  t(16) = 2.8866
  t(17) = 2.2991
  t(18) = 1.7362
  t(19) = 1.2072
  t(20) = 0.7187
  t(21) = 0.2731
  t(22) = -0.1304
  t(23) = -0.4933
  t(24) = -0.8196
  t(25) = -1.1139
  t(26) = -1.3827
  t(27) = -1.6311
  t(28) = -1.8620
  t(29) = -2.0776
  t(30) = -2.2801
  t(31) = -2.4690
  t(32) = -2.6441
  t(33) = -2.8030
  t(34) = -2.9451
  t(35) = -3.0701
  t(36) = -3.1773
  t(37) = -3.2710
  t(38) = -3.3575
  t(39) = -3.4280
  
  DO i = 1,ns-1
 
     IF ( ( x .GT. r(i-1) ) .AND. ( x .LE. r(i) ) ) THEN
        twist = t(i-1) + (x - r(i-1))*( ( t(i) - t(i-1) )/( r(i) - r(i-1) ) )
        goto 1
     ENDIF

1 CONTINUE

     IF ( x .LT. r(0) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius < blade root in calc_twist_dtu10.pro'
	PRINT*, 'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF
     
     IF ( x .GT. r(ns-1) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius >  blade length in calc_twist_dtu10.pro'
	PRINT*,'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF 
     
  ENDDO
  
!PRINT*,x,twist
  
!_________________________________________________________________________
  
END FUNCTION calc_twist_dtu10
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_chord_dtu10( x ) RESULT( chord )
!_________________________________________________________________________
!
! Data from DTU 10 MW
! x = radial distance from hub center
!_________________________________________________________________________
 

  REAL, INTENT( IN ) :: x

  REAL, PARAMETER :: ns = 40

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: c

  REAL :: chord 
  INTEGER :: i

  chord = 0.0
  
  r(0) =  2.8000
  r(1) =  5.4430
  r(2) =  8.1798
  r(3) =  11.0028
  r(4) =  13.9033
  r(5) =  16.8712
  r(6) =  19.8957
  r(7) =  22.9646
  r(8) =  26.0653
  r(9) =  29.1846
  r(10) = 32.3089
  r(11) = 35.4245
  r(12) = 38.5179
  r(13) = 41.5760
  r(14) = 44.5862
  r(15) = 47.5369
  r(16) = 50.4172
  r(17) = 53.2174
  r(18) = 55.9292
  r(19) = 58.5453
  r(20) = 61.0600
  r(21) = 63.4685
  r(22) = 65.7676
  r(23) = 67.9552
  r(24) = 70.0303
  r(25) = 71.9930
  r(26) = 73.8442
  r(27) = 75.5858
  r(28) = 77.2203
  r(29) = 78.7507
  r(30) = 80.1806
  r(31) = 81.5138
  r(32) = 82.7547
  r(33) = 83.9076
  r(34) = 84.9770
  r(35) = 85.9675
  r(36) = 86.8836
  r(37) = 87.7299
  r(38) = 88.5107
  r(39) = 89.2000
 
  c(0) =  5.3800
  c(1) =  5.3803
  c(2) =  5.3800
  c(3) =  5.4525
  c(4) =  5.6354
  c(5) =  5.8660
  c(6) =  6.0707
  c(7) =  6.1845
  c(8) =  6.2030
  c(9) =  6.1429
  c(10) = 6.0200
  c(11) = 5.8491
  c(12) = 5.6453
  c(13) = 5.4219
  c(14) = 5.1858
  c(15) = 4.9428
  c(16) = 4.6982
  c(17) = 4.4568
  c(18) = 4.2227
  c(19) = 3.9987
  c(20) = 3.7861
  c(21) = 3.5857
  c(22) = 3.3981
  c(23) = 3.2235
  c(24) = 3.0619
  c(25) = 2.9130
  c(26) = 2.7763
  c(27) = 2.6507
  c(28) = 2.5359
  c(29) = 2.4308
  c(30) = 2.3314
  c(31) = 2.2324
  c(32) = 2.1291
  c(33) = 2.0191
  c(34) = 1.9033
  c(35) = 1.7761
  c(36) = 1.6260
  c(37) = 1.4401
  c(38) = 1.1836
  c(39) = 0.6000

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
     PRINT*,'radius > blade length in calc_chord_dtu10.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
1 CONTINUE
  
!PRINT*,x,chord
   
!_________________________________________________________________________
  
END FUNCTION calc_chord_dtu10
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl_dtu10( x,y ) RESULT( cl )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from DTU 10 MW
!
! calc_cl1_dtu10 uses data from circle
! calc_cl2_dtu10 uses data from FFA-W3-600
! calc_cl3_dtu10 uses data from FFA-W3-480
! calc_cl4_dtu10 uses data from FFA-W3-360
! calc_cl5_dtu10 uses data from FFA-W3-301
! calc_cl6_dtu10 uses data from FFA-W3-241
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  REAL, PARAMETER :: ns = 7

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cl

  INTEGER :: i

  cl = 0.0
  
  r(0) = 2.8000
  r(1) = 11.0028
  r(2) = 16.8712
  r(3) = 22.9646
  r(4) = 29.1846
  r(5) = 41.5760
  r(6) = 89.2000
  
  IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
       cl = calc_cl1_dtu10(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r( 2) ) ) THEN
       cl = calc_cl2_dtu10(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r( 3) ) ) THEN
       cl = calc_cl3_dtu10(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r( 4) ) ) THEN
       cl = calc_cl4_dtu10(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r( 5) ) ) THEN
       cl = calc_cl5_dtu10(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r( 6) ) ) THEN
       cl = calc_cl6_dtu10(y)
  ENDIF
  
  IF ( x .LT. r(0) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cl_dtu10.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cl_dtu10.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
!_________________________________________________________________________
  
END FUNCTION calc_cl_dtu10
!_________________________________________________________________________


FUNCTION calc_cl1_dtu10( y ) RESULT( cl1 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: Circle
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cl1

  cl1 = 0.0001
!_________________________________________________________________________
  
END FUNCTION calc_cl1_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl2_dtu10( y ) RESULT( cl2 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: FFA-W3-600
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl2
  INTEGER :: i

  cl2 = 0.0

!                alpha    CL   
  cl(0:1, 0) = [-180.0, 0.000]
  cl(0:1, 1) = [-175.0, 0.1736]
  cl(0:1, 2) = [-170.0, 0.342]
  cl(0:1, 3) = [-165.0, 0.5]
  cl(0:1, 4) = [-160.0, 0.6428]
  cl(0:1, 5) = [-155.0, 0.766]
  cl(0:1, 6) = [-150.0, 0.866]
  cl(0:1, 7) = [-145.0, 0.9397]
  cl(0:1, 8) = [-140.0, 0.9848]
  cl(0:1, 9) = [-135.0, 1.00]
  cl(0:1,10) = [-130.0, 0.9848]
  cl(0:1,11) = [-125.0, 0.9397]
  cl(0:1,12) = [-120.0, 0.866]
  cl(0:1,13) = [-115.0, 0.766]
  cl(0:1,14) = [-110.0, 0.6428]
  cl(0:1,15) = [-105.0, 0.5]
  cl(0:1,16) = [-100.0, 0.342]
  cl(0:1,17) = [-95.0, 0.1736]
  cl(0:1,18) = [-90.0, 0.00]
  cl(0:1,19) = [-85.0, -0.1736]
  cl(0:1,20) = [-80.0, -0.342]
  cl(0:1,21) = [-75.0, -0.5]
  cl(0:1,22) = [-70.0, -0.6428]
  cl(0:1,23) = [-65.0, -0.766]
  cl(0:1,24) = [-60.0, -0.866]
  cl(0:1,25) = [-55.0, -0.9397]
  cl(0:1,26) = [-50.0, -0.9848]
  cl(0:1,27) = [-45.0, -1.000]  
  cl(0:1,28) = [-40.0, -0.9516]
  cl(0:1,29) = [-39.0, -0.9039]
  cl(0:1,30) = [-38.0, -0.8262]
  cl(0:1,31) = [-37.0, -0.7185]
  cl(0:1,32) = [-36.0, -0.6208]
  cl(0:1,33) = [-35.0, -0.5231]
  cl(0:1,34) = [-34.0, -0.4454]
  cl(0:1,35) = [-33.0, -0.3477]
  cl(0:1,36) = [-32.0, -0.2571]
  cl(0:1,37) = [-30.0, -0.1574]
  cl(0:1,38) = [-28.0, -0.0776]
  cl(0:1,39) = [-26.0, 0.0115]
  cl(0:1,40) = [-24.0, 0.1007]
  cl(0:1,41) = [-22.0, 0.1962]
  cl(0:1,42) = [-20.0, 0.2918]
  cl(0:1,43) = [-18.0, 0.3932]
  cl(0:1,44) = [-16.0, 0.4947]
  cl(0:1,45) = [-14.0, 0.6004]
  cl(0:1,46) = [-12.0, 0.7061]
  cl(0:1,47) = [-10.0, 0.788]
  cl(0:1,48) = [-8.0, 0.87]
  cl(0:1,49) = [-6.0, 0.8358]
  cl(0:1,50) = [-4.0, 0.8162]
  cl(0:1,51) = [-2.0, 0.6603]
  cl(0:1,52) = [0.0, 0.5199]
  cl(0:1,53) = [2.0, 0.2636]
  cl(0:1,54) = [4.0, -0.0906]
  cl(0:1,55) = [6.0, 0.1656]
  cl(0:1,56) = [8.0, 0.4475]
  cl(0:1,57) = [10.0, 0.7071]
  cl(0:1,58) = [12.0, 0.954]
  cl(0:1,59) = [14.0, 1.189]
  cl(0:1,60) = [16.0, 1.418]
  cl(0:1,61) = [18.0, 1.639]
  cl(0:1,62) = [20.0, 1.822]
  cl(0:1,63) = [22.0, 1.984]
  cl(0:1,64) = [24.0, 2.099]
  cl(0:1,65) = [26.0, 2.19]
  cl(0:1,66) = [28.0, 2.254]
  cl(0:1,67) = [30.0, 2.273]
  cl(0:1,68) = [32.0, 2.274]
  cl(0:1,69) = [33.0, 2.278]
  cl(0:1,70) = [34.0, 2.266]
  cl(0:1,71) = [35.0, 2.241]
  cl(0:1,72) = [36.0, 2.233]
  cl(0:1,73) = [37.0, 2.213]
  cl(0:1,74) = [38.0, 2.182]
  cl(0:1,75) = [39.0, 2.139]
  cl(0:1,76) = [40.0, 2.095]
  cl(0:1,77) = [45.0, 1.837]
  cl(0:1,78) = [50.0, 1.516]
  cl(0:1,79) = [55.0, 1.201]
  cl(0:1,80) = [60.0, 0.966]
  cl(0:1,81) = [65.0, 0.766]
  cl(0:1,82) = [70.0, 0.6428]
  cl(0:1,83) = [75.0, 0.5]
  cl(0:1,84) = [80.0, 0.342]
  cl(0:1,85) = [85.0, 0.1736]
  cl(0:1,86) = [90.0, 0.000]
  cl(0:1,87) = [95.0, -0.1736]
  cl(0:1,88) = [100.0, -0.342]
  cl(0:1,89) = [105.0, -0.5]
  cl(0:1,90) = [110.0, -0.6428]
  cl(0:1,91) = [115.0, -0.766]
  cl(0:1,92) = [120.0, -0.866]
  cl(0:1,93) = [125.0, -0.9397]
  cl(0:1,94) = [130.0, -0.9848]
  cl(0:1,95) = [135.0, -1.000]
  cl(0:1,96) = [140.0, -0.9848]
  cl(0:1,97) = [145.0, -0.9397]
  cl(0:1,98) = [150.0, -0.866]
  cl(0:1,99) = [155.0, -0.766]
  cl(0:1,100) = [160.0, -0.6428]
  cl(0:1,101) = [165.0, -0.5]
  cl(0:1,102) = [170.0, -0.342]
  cl(0:1,103) = [175.0, -0.1736]
  cl(0:1,104) = [180.0, 0.0000]
  
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
!     PRINT*,'alpha < tabulated range in calc_cl2_dtu10.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl2 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl2_dtu10.pro'
  ENDIF
  
!print,y,cl2
  
!_________________________________________________________________________
  
END FUNCTION calc_cl2_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl3_dtu10( y ) RESULT( cl3 )
!_________________________________________________________________________
!
! Data from DTU 10 MW FFA-W3-480
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl3
  INTEGER :: i

  cl3 = 0.0

!                alpha    CL   
  cl(0:1, 0) = [-180.0, 0.0000]
  cl(0:1, 1) = [-175.0, 0.1736]
  cl(0:1, 2) = [-170.0, 0.3420]
  cl(0:1, 3) = [-165.0, 0.5000]
  cl(0:1, 4) = [-160.0, 0.6428]
  cl(0:1, 5) = [-155.0, 0.7660]
  cl(0:1, 6) = [-150.0, 0.8660]
  cl(0:1, 7) = [-145.0, 0.9397]
  cl(0:1, 8) = [-140.0, 0.9848]
  cl(0:1, 9) = [-135.0, 1.0000]
  cl(0:1,10) = [-130.0, 0.9848]
  cl(0:1,11) = [-125.0, 0.9397]
  cl(0:1,12) = [-120.0, 0.8660]
  cl(0:1,13) = [-115.0, 0.7660]
  cl(0:1,14) = [-110.0, 0.6428]
  cl(0:1,15) = [-105.0, 0.5000]
  cl(0:1,16) = [-100.0, 0.3420]
  cl(0:1,17) = [-95.0, 0.1736]
  cl(0:1,18) = [-90.0, 0.0000]
  cl(0:1,19) = [-85.0, -0.1736]
  cl(0:1,20) = [-80.0, -0.3420]
  cl(0:1,21) = [-75.0, -0.5000]
  cl(0:1,22) = [-70.0, -0.6428]
  cl(0:1,23) = [-65.0, -0.7660]
  cl(0:1,24) = [-60.0, -0.8660]
  cl(0:1,25) = [-55.0, -0.9397]
  cl(0:1,26) = [-50.0, -0.9848]
  cl(0:1,27) = [-45.0, -1.0000]
  cl(0:1,28) = [-40.0, -0.9816]
  cl(0:1,29) = [-39.0, -0.9539]
  cl(0:1,30) = [-38.0, -0.9262]
  cl(0:1,31) = [-37.0, -0.8885]
  cl(0:1,32) = [-36.0, -0.8508]
  cl(0:1,33) = [-35.0, -0.8231]
  cl(0:1,34) = [-34.0, -0.7854]
  cl(0:1,35) = [-33.0, -0.7477]
  cl(0:1,36) = [-32.0, -0.7011]
  cl(0:1,37) = [-30.0, -0.6208]
  cl(0:1,38) = [-28.0, -0.5406]
  cl(0:1,39) = [-26.0, -0.4694]
  cl(0:1,40) = [-24.0, -0.3881]
  cl(0:1,41) = [-22.0, -0.3161]
  cl(0:1,42) = [-20.0, -0.2442]
  cl(0:1,43) = [-18.0, -0.1641]
  cl(0:1,44) = [-16.0, -0.0841]
  cl(0:1,45) = [-14.0, 0.0021]
  cl(0:1,46) = [-12.0, 0.0883]
  cl(0:1,47) = [-10.0, 0.1722]
  cl(0:1,48) = [-8.0, 0.2561]
  cl(0:1,49) = [-6.0, 0.3179]
  cl(0:1,50) = [-4.0, 0.3798]
  cl(0:1,51) = [-2.0, 0.3573]
  cl(0:1,52) = [0.0, 0.3348]
  cl(0:1,53) = [2.0, 0.5652]
  cl(0:1,54) = [4.0, 0.8769]
  cl(0:1,55) = [6.0, 1.0425]
  cl(0:1,56) = [8.0, 0.9487]
  cl(0:1,57) = [10.0, 0.9088]
  cl(0:1,58) = [12.0, 0.9761]
  cl(0:1,59) = [14.0, 1.1130]
  cl(0:1,60) = [16.0, 1.3065]
  cl(0:1,61) = [18.0, 1.5414]
  cl(0:1,62) = [20.0, 1.8049]
  cl(0:1,63) = [22.0, 2.0020]
  cl(0:1,64) = [24.0, 2.1216]
  cl(0:1,65) = [26.0, 2.1916]
  cl(0:1,66) = [28.0, 2.2291]
  cl(0:1,67) = [30.0, 2.2322]
  cl(0:1,68) = [32.0, 2.2188]
  cl(0:1,69) = [33.0, 2.2092]
  cl(0:1,70) = [34.0, 2.1927]
  cl(0:1,71) = [35.0, 2.1625]
  cl(0:1,72) = [36.0, 2.1313]
  cl(0:1,73) = [37.0, 2.0878]
  cl(0:1,74) = [38.0, 2.0435]
  cl(0:1,75) = [39.0, 1.9984]
  cl(0:1,76) = [40.0, 1.9418]
  cl(0:1,77) = [45.0, 1.6775]
  cl(0:1,78) = [50.0, 1.2856]
  cl(0:1,79) = [55.0, 1.0308]
  cl(0:1,80) = [60.0, 0.8660]
  cl(0:1,81) = [65.0, 0.7660]
  cl(0:1,82) = [70.0, 0.6428]
  cl(0:1,83) = [75.0, 0.5000]
  cl(0:1,84) = [80.0, 0.3420]
  cl(0:1,85) = [85.0, 0.1736]
  cl(0:1,86) = [90.0, 0.0000]
  cl(0:1,87) = [95.0, -0.1736]
  cl(0:1,88) = [100.0, -0.3420]
  cl(0:1,89) = [105.0, -0.5000]
  cl(0:1,90) = [110.0, -0.6428]
  cl(0:1,91) = [115.0, -0.7660]
  cl(0:1,92) = [120.0, -0.8660]
  cl(0:1,93) = [125.0, -0.9397]
  cl(0:1,94) = [130.0, -0.9848]
  cl(0:1,95) = [135.0, -1.0000]
  cl(0:1,96) = [140.0, -0.9848]
  cl(0:1,97) = [145.0, -0.9397]
  cl(0:1,98) = [150.0, -0.8660]
  cl(0:1,99) = [155.0, -0.7660]
  cl(0:1,100) = [160.0, -0.6428]
  cl(0:1,101) = [165.0, -0.5000]
  cl(0:1,102) = [170.0, -0.3420]
  cl(0:1,103) = [175.0, -0.1736]
  cl(0:1,104) = [180.0, 0.0000]

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
!     PRINT*,'alpha < tabulated range in calc_cl3_dtu10.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl3 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl3_dtu10.pro'
  ENDIF
  
!print,y,cl3
  
!_________________________________________________________________________
  
END FUNCTION calc_cl3_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl4_dtu10( y ) RESULT( cl4 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: FFA-W3-360
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl4
  INTEGER :: i

  cl4 = 0.0

!                alpha    CL  
  cl(0:1, 0) = [-180.0, 0.0000]
  cl(0:1, 1) = [-175.0, 0.1736]
  cl(0:1, 2) = [-170.0, 0.3420]
  cl(0:1, 3) = [-165.0, 0.5000]
  cl(0:1, 4) = [-160.0, 0.6428]
  cl(0:1, 5) = [-155.0, 0.7660]
  cl(0:1, 6) = [-150.0, 0.8660]
  cl(0:1, 7) = [-145.0, 0.9397]
  cl(0:1, 8) = [-140.0, 0.9848]
  cl(0:1, 9) = [-135.0, 1.0000]
  cl(0:1,10) = [-130.0, 0.9848]
  cl(0:1,11) = [-125.0, 0.9397]
  cl(0:1,12) = [-120.0, 0.8660]
  cl(0:1,13) = [-115.0, 0.7660]
  cl(0:1,14) = [-110.0, 0.6428]
  cl(0:1,15) = [-105.0, 0.5000]
  cl(0:1,16) = [-100.0, 0.3420]
  cl(0:1,17) = [-95.0, 0.1736]
  cl(0:1,18) = [-90.0, 0.0000]
  cl(0:1,19) = [-85.0, -0.1736]
  cl(0:1,20) = [-80.0, -0.3420]
  cl(0:1,21) = [-75.0, -0.5000]
  cl(0:1,22) = [-70.0, -0.6428]
  cl(0:1,23) = [-65.0, -0.7660]
  cl(0:1,24) = [-60.0, -0.8660]
  cl(0:1,25) = [-55.0, -0.9397]
  cl(0:1,26) = [-50.0, -0.9848]
  cl(0:1,27) = [-45.0, -1.0000]
  cl(0:1,28) = [-40.0, -0.9716]
  cl(0:1,29) = [-39.0, -0.9639]
  cl(0:1,30) = [-38.0, -0.9462]
  cl(0:1,31) = [-37.0, -0.9285]
  cl(0:1,32) = [-36.0, -0.9108]
  cl(0:1,33) = [-35.0, -0.8931]
  cl(0:1,34) = [-34.0, -0.8854]
  cl(0:1,35) = [-33.0, -0.8677]
  cl(0:1,36) = [-32.0, -0.8564]
  cl(0:1,37) = [-30.0, -0.8378]
  cl(0:1,38) = [-28.0, -0.8191]
  cl(0:1,39) = [-26.0, -0.7924]
  cl(0:1,40) = [-24.0, -0.7756]
  cl(0:1,41) = [-22.0, -0.7441]
  cl(0:1,42) = [-20.0, -0.7126]
  cl(0:1,43) = [-18.0, -0.6678]
  cl(0:1,44) = [-16.0, -0.6231]
  cl(0:1,45) = [-14.0, -0.5742]
  cl(0:1,46) = [-12.0, -0.5252]
  cl(0:1,47) = [-10.0, -0.4827]
  cl(0:1,48) = [-8.0, -0.4402]
  cl(0:1,49) = [-6.0, -0.2983]
  cl(0:1,50) = [-4.0, -0.1564]
  cl(0:1,51) = [-2.0, 0.1744]
  cl(0:1,52) = [0.0, 0.5053]
  cl(0:1,53) = [2.0, 0.8241]
  cl(0:1,54) = [4.0, 1.1209]
  cl(0:1,55) = [6.0, 1.3897]
  cl(0:1,56) = [8.0, 1.6254]
  cl(0:1,57) = [10.0, 1.8109]
  cl(0:1,58) = [12.0, 1.8589]
  cl(0:1,59) = [14.0, 1.8159]
  cl(0:1,60) = [16.0, 1.7786]
  cl(0:1,61) = [18.0, 1.7560]
  cl(0:1,62) = [20.0, 1.7630]
  cl(0:1,63) = [22.0, 1.8002]
  cl(0:1,64) = [24.0, 1.8495]
  cl(0:1,65) = [26.0, 1.8775]
  cl(0:1,66) = [28.0, 1.8828]
  cl(0:1,67) = [30.0, 1.8689]
  cl(0:1,68) = [32.0, 1.8439]
  cl(0:1,69) = [33.0, 1.8349]
  cl(0:1,70) = [34.0, 1.8297]
  cl(0:1,71) = [35.0, 1.8136]
  cl(0:1,72) = [36.0, 1.7854]
  cl(0:1,73) = [37.0, 1.7669]
  cl(0:1,74) = [38.0, 1.7269]
  cl(0:1,75) = [39.0, 1.6864]
  cl(0:1,76) = [40.0, 1.6458]
  cl(0:1,77) = [45.0, 1.4473]
  cl(0:1,78) = [50.0, 1.1356]
  cl(0:1,79) = [55.0, 0.9808]
  cl(0:1,80) = [60.0, 0.8660]
  cl(0:1,81) = [65.0, 0.7660]
  cl(0:1,82) = [70.0, 0.6428]
  cl(0:1,83) = [75.0, 0.5000]
  cl(0:1,84) = [80.0, 0.3420]
  cl(0:1,85) = [85.0, 0.1736]
  cl(0:1,86) = [90.0, 0.0000]
  cl(0:1,87) = [95.0, -0.1736]
  cl(0:1,88) = [100.0, -0.3420]
  cl(0:1,89) = [105.0, -0.5000]
  cl(0:1,90) = [110.0, -0.6428]
  cl(0:1,91) = [115.0, -0.7660]
  cl(0:1,92) = [120.0, -0.8660]
  cl(0:1,93) = [125.0, -0.9397]
  cl(0:1,94) = [130.0, -0.9848]
  cl(0:1,95) = [135.0, -1.0000]
  cl(0:1,96) = [140.0, -0.9848]
  cl(0:1,97) = [145.0, -0.9397]
  cl(0:1,98) = [150.0, -0.8660]
  cl(0:1,99) = [155.0, -0.7660]
  cl(0:1,100) = [160.0, -0.6428]
  cl(0:1,101) = [165.0, -0.5000]
  cl(0:1,102) = [170.0, -0.3420]
  cl(0:1,103) = [175.0, -0.1736]
  cl(0:1,104) = [180.0, 0.0000]

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
!     PRINT*,'alpha < tabulated range in calc_cl4_dtu10.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl4 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl4_dtu10.pro'
  ENDIF
  
!print,y,cl4
  
!_________________________________________________________________________
  
END FUNCTION calc_cl4_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl5_dtu10( y ) RESULT( cl5 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: FFA-W3-301
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl5
  INTEGER :: i

  cl5 = 0.0

!                alpha    CL  
  cl(0:1, 0) = [-180.0, 0.0000]
  cl(0:1, 1) = [-175.0, 0.1736]
  cl(0:1, 2) = [-170.0, 0.3420]
  cl(0:1, 3) = [-165.0, 0.5000]
  cl(0:1, 4) = [-160.0, 0.6428]
  cl(0:1, 5) = [-155.0, 0.7660]
  cl(0:1, 6) = [-150.0, 0.8660]
  cl(0:1, 7) = [-145.0, 0.9397]
  cl(0:1, 8) = [-140.0, 0.9848]
  cl(0:1, 9) = [-135.0, 1.0000]
  cl(0:1,10) = [-130.0, 0.9848]
  cl(0:1,11) = [-125.0, 0.9397]
  cl(0:1,12) = [-120.0, 0.8660]
  cl(0:1,13) = [-115.0, 0.7660]
  cl(0:1,14) = [-110.0, 0.6428]
  cl(0:1,15) = [-105.0, 0.5000]
  cl(0:1,16) = [-100.0, 0.3420]
  cl(0:1,17) = [-95.0, 0.1736]
  cl(0:1,18) = [-90.0, 0.0000]
  cl(0:1,19) = [-85.0, -0.1736]
  cl(0:1,20) = [-80.0, -0.3420]
  cl(0:1,21) = [-75.0, -0.5000]
  cl(0:1,22) = [-70.0, -0.6428]
  cl(0:1,23) = [-65.0, -0.7660]
  cl(0:1,24) = [-60.0, -0.8660]
  cl(0:1,25) = [-55.0, -0.9397]
  cl(0:1,26) = [-50.0, -0.9848]
  cl(0:1,27) = [-45.0, -1.0000]
  cl(0:1,28) = [-40.0, -1.0216]
  cl(0:1,29) = [-39.0, -1.0239]
  cl(0:1,30) = [-38.0, -1.0162]
  cl(0:1,31) = [-37.0, -1.0185]
  cl(0:1,32) = [-36.0, -1.0108]
  cl(0:1,33) = [-35.0, -1.0031]
  cl(0:1,34) = [-34.0, -0.9954]
  cl(0:1,35) = [-33.0, -0.9877]
  cl(0:1,36) = [-32.0, -0.9835]
  cl(0:1,37) = [-30.0, -0.9703]
  cl(0:1,38) = [-28.0, -0.9672]
  cl(0:1,39) = [-26.0, -0.9441]
  cl(0:1,40) = [-24.0, -0.9310]
  cl(0:1,41) = [-22.0, -0.9047]
  cl(0:1,42) = [-20.0, -0.8784]
  cl(0:1,43) = [-18.0, -0.8459]
  cl(0:1,44) = [-16.0, -0.8134]
  cl(0:1,45) = [-14.0, -0.7728]
  cl(0:1,46) = [-12.0, -0.7322]
  cl(0:1,47) = [-10.0, -0.6935]
  cl(0:1,48) = [-8.0, -0.6547]
  cl(0:1,49) = [-6.0, -0.4507]
  cl(0:1,50) = [-4.0, -0.2467]
  cl(0:1,51) = [-2.0, 0.0295]
  cl(0:1,52) = [0.0, 0.3056]
  cl(0:1,53) = [2.0, 0.5670]
  cl(0:1,54) = [4.0, 0.8199]
  cl(0:1,55) = [6.0, 1.0614]
  cl(0:1,56) = [8.0, 1.2874]
  cl(0:1,57) = [10.0, 1.4840]
  cl(0:1,58) = [12.0, 1.6388]
  cl(0:1,59) = [14.0, 1.7327]
  cl(0:1,60) = [16.0, 1.7142]
  cl(0:1,61) = [18.0, 1.6828]
  cl(0:1,62) = [20.0, 1.6567]
  cl(0:1,63) = [22.0, 1.6444]
  cl(0:1,64) = [24.0, 1.6329]
  cl(0:1,65) = [26.0, 1.6333]
  cl(0:1,66) = [28.0, 1.6175]
  cl(0:1,67) = [30.0, 1.5975]
  cl(0:1,68) = [32.0, 1.5708]
  cl(0:1,69) = [33.0, 1.5758]
  cl(0:1,70) = [34.0, 1.5571]
  cl(0:1,71) = [35.0, 1.5475]
  cl(0:1,72) = [36.0, 1.5268]
  cl(0:1,73) = [37.0, 1.5259]
  cl(0:1,74) = [38.0, 1.5139]
  cl(0:1,75) = [39.0, 1.4918]
  cl(0:1,76) = [40.0, 1.4596]
  cl(0:1,77) = [45.0, 1.2951]
  cl(0:1,78) = [50.0, 1.1256]
  cl(0:1,79) = [55.0, 0.9808]
  cl(0:1,80) = [60.0, 0.8660]
  cl(0:1,81) = [65.0, 0.7660]
  cl(0:1,82) = [70.0, 0.6428]
  cl(0:1,83) = [75.0, 0.5000]
  cl(0:1,84) = [80.0, 0.3420]
  cl(0:1,85) = [85.0, 0.1736]
  cl(0:1,86) = [90.0, 0.0000]
  cl(0:1,87) = [95.0, -0.1736]
  cl(0:1,88) = [100.0, -0.3420]
  cl(0:1,89) = [105.0, -0.5000]
  cl(0:1,90) = [110.0, -0.6428]
  cl(0:1,91) = [115.0, -0.7660]
  cl(0:1,92) = [120.0, -0.8660]
  cl(0:1,93) = [125.0, -0.9397]
  cl(0:1,94) = [130.0, -0.9848]
  cl(0:1,95) = [135.0, -1.0000]
  cl(0:1,96) = [140.0, -0.9848]
  cl(0:1,97) = [145.0, -0.9397]
  cl(0:1,98) = [150.0, -0.8660]
  cl(0:1,99) = [155.0, -0.7660]
  cl(0:1,100) = [160.0, -0.6428]
  cl(0:1,101) = [165.0, -0.5000]
  cl(0:1,102) = [170.0, -0.3420]
  cl(0:1,103) = [175.0, -0.1736]
  cl(0:1,104) = [180.0, 0.0000]

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
!     PRINT*,'alpha < tabulated range in calc_cl5_dtu10.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl5 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl5_dtu10.pro'
  ENDIF
  
!print,y,cl5
  
!_________________________________________________________________________
  
END FUNCTION calc_cl5_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl6_dtu10( y ) RESULT( cl6 )
!_________________________________________________________________________
!
! Data from  DTU 10 MW: FFA-W3-241
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl6
  INTEGER :: i

  cl6 = 0.0

!                alpha    CL  
  cl(0:1, 0) = [-180.0, 0.0000]
  cl(0:1, 1) = [-175.0, 0.1736]
  cl(0:1, 2) = [-170.0, 0.3420]
  cl(0:1, 3) = [-165.0, 0.5000]
  cl(0:1, 4) = [-160.0, 0.6428]
  cl(0:1, 5) = [-155.0, 0.7660]
  cl(0:1, 6) = [-150.0, 0.8660]
  cl(0:1, 7) = [-145.0, 0.9397]
  cl(0:1, 8) = [-140.0, 0.9848]
  cl(0:1, 9) = [-135.0, 1.0000]
  cl(0:1,10) = [-130.0, 0.9848]
  cl(0:1,11) = [-125.0, 0.9397]
  cl(0:1,12) = [-120.0, 0.8660]
  cl(0:1,13) = [-115.0, 0.7660]
  cl(0:1,14) = [-110.0, 0.6428]
  cl(0:1,15) = [-105.0, 0.5000]
  cl(0:1,16) = [-100.0, 0.3420]
  cl(0:1,17) = [-95.0, 0.1736]
  cl(0:1,18) = [-90.0, 0.0000]
  cl(0:1,19) = [-85.0, -0.1736]
  cl(0:1,20) = [-80.0, -0.3420]
  cl(0:1,21) = [-75.0, -0.5000]
  cl(0:1,22) = [-70.0, -0.6428]
  cl(0:1,23) = [-65.0, -0.7660]
  cl(0:1,24) = [-60.0, -0.8660]
  cl(0:1,25) = [-55.0, -0.9397]
  cl(0:1,26) = [-50.0, -0.9848]
  cl(0:1,27) = [-45.0, -1.0000]
  cl(0:1,28) = [-40.0, -1.0316]
  cl(0:1,29) = [-39.0, -1.0439]
  cl(0:1,30) = [-38.0, -1.0462]
  cl(0:1,31) = [-37.0, -1.0485]
  cl(0:1,32) = [-36.0, -1.0508]
  cl(0:1,33) = [-35.0, -1.0531]
  cl(0:1,34) = [-34.0, -1.0554]
  cl(0:1,35) = [-33.0, -1.0677]
  cl(0:1,36) = [-32.0, -1.0726]
  cl(0:1,37) = [-30.0, -1.0917]
  cl(0:1,38) = [-28.0, -1.1008]
  cl(0:1,39) = [-26.0, -1.1101]
  cl(0:1,40) = [-24.0, -1.1294]
  cl(0:1,41) = [-22.0, -1.1233]
  cl(0:1,42) = [-20.0, -1.1171]
  cl(0:1,43) = [-18.0, -1.0870]
  cl(0:1,44) = [-16.0, -1.0569]
  cl(0:1,45) = [-14.0, -1.0238]
  cl(0:1,46) = [-12.0, -0.9908]
  cl(0:1,47) = [-10.0, -0.8594]
  cl(0:1,48) = [-8.0, -0.7279]
  cl(0:1,49) = [-6.0, -0.4690]
  cl(0:1,50) = [-4.0, -0.2101]
  cl(0:1,51) = [-2.0, 0.0428]
  cl(0:1,52) = [0.0, 0.2956]
  cl(0:1,53) = [2.0, 0.5435]
  cl(0:1,54) = [4.0, 0.7867]
  cl(0:1,55) = [6.0, 1.0228]
  cl(0:1,56) = [8.0, 1.2474]
  cl(0:1,57) = [10.0, 1.4575]
  cl(0:1,58) = [12.0, 1.6422]
  cl(0:1,59) = [14.0, 1.7824]
  cl(0:1,60) = [16.0, 1.8167]
  cl(0:1,61) = [18.0, 1.7763]
  cl(0:1,62) = [20.0, 1.6608]
  cl(0:1,63) = [22.0, 1.5659]
  cl(0:1,64) = [24.0, 1.4786]
  cl(0:1,65) = [26.0, 1.4098]
  cl(0:1,66) = [28.0, 1.3403]
  cl(0:1,67) = [30.0, 1.2959]
  cl(0:1,68) = [32.0, 1.2412]
  cl(0:1,69) = [33.0, 1.2108]
  cl(0:1,70) = [34.0, 1.1825]
  cl(0:1,71) = [35.0, 1.1640]
  cl(0:1,72) = [36.0, 1.1352]
  cl(0:1,73) = [37.0, 1.1165]
  cl(0:1,74) = [38.0, 1.0977]
  cl(0:1,75) = [39.0, 1.0788]
  cl(0:1,76) = [40.0, 1.0597]
  cl(0:1,77) = [45.0, 1.0043]
  cl(0:1,78) = [50.0, 0.9756]
  cl(0:1,79) = [55.0, 0.9208]
  cl(0:1,80) = [60.0, 0.8660]
  cl(0:1,81) = [65.0, 0.7660]
  cl(0:1,82) = [70.0, 0.6428]
  cl(0:1,83) = [75.0, 0.5000]
  cl(0:1,84) = [80.0, 0.3420]
  cl(0:1,85) = [85.0, 0.1736]
  cl(0:1,86) = [90.0, 0.0000]
  cl(0:1,87) = [95.0, -0.1736]
  cl(0:1,88) = [100.0, -0.3420]
  cl(0:1,89) = [105.0, -0.5000]
  cl(0:1,90) = [110.0, -0.6428]
  cl(0:1,91) = [115.0, -0.7660]
  cl(0:1,92) = [120.0, -0.8660]
  cl(0:1,93) = [125.0, -0.9397]
  cl(0:1,94) = [130.0, -0.9848]
  cl(0:1,95) = [135.0, -1.0000]
  cl(0:1,96) = [140.0, -0.9848]
  cl(0:1,97) = [145.0, -0.9397]
  cl(0:1,98) = [150.0, -0.8660]
  cl(0:1,99) = [155.0, -0.7660]
  cl(0:1,100) = [160.0, -0.6428]
  cl(0:1,101) = [165.0, -0.5000]
  cl(0:1,102) = [170.0, -0.3420]
  cl(0:1,103) = [175.0, -0.1736]
  cl(0:1,104) = [180.0, 0.0000]

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
!     PRINT*,'alpha < tabulated range in calc_cl6_dtu10.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl6 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl6_dtu10.pro'
  ENDIF
  
!print,y,cl6
  
!_________________________________________________________________________
  
END FUNCTION calc_cl6_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd_dtu10( x,y ) RESULT( cd )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from DTU 10 MW
!
! calc_cd1_dtu10 uses data from circle
! calc_cd2_dtu10 uses data from FFA-W3-600
! calc_cd3_dtu10 uses data from FFA-W3-480
! calc_cd4_dtu10 uses data from FFA-W3-360
! calc_cd5_dtu10 uses data from FFA-W3-301
! calc_cd6_dtu10 uses data from FFA-W3-241
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  REAL, PARAMETER :: ns = 7

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cd
  INTEGER :: i

  cd = 0.0

  r(0) = 2.8000
  r(1) = 11.0028
  r(2) = 16.8712
  r(3) = 22.9646
  r(4) = 29.1846
  r(5) = 41.5760
  r(6) = 89.2000

  IF ( ( x .GT. r( 0) ) .AND. (x .LE. r( 1) ) ) THEN
       cd = calc_cd1_dtu10(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. (x .LE. r( 2) ) ) THEN
       cd = calc_cd2_dtu10(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. (x .LE. r( 3) ) ) THEN
       cd = calc_cd3_dtu10(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. (x .LE. r( 4) ) ) THEN
       cd = calc_cd4_dtu10(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. (x .LE. r( 5) ) ) THEN
       cd = calc_cd5_dtu10(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. (x .LE. r( 6) ) ) THEN
       cd = calc_cd6_dtu10(y)
  ENDIF
  
  IF ( x .LT. r(0) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cd_dtu10.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
  IF ( x .GT. r(ns-1) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cd_dtu10.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd_dtu10
!_________________________________________________________________________

FUNCTION calc_cd1_dtu10( y ) RESULT( cd1 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: Circle
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cd1

  cd1 = 0.6
!_________________________________________________________________________
  
END FUNCTION calc_cd1_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd2_dtu10( y ) RESULT( cd2 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: FFA-WS-600
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd2
  INTEGER :: i

  cd2 = 0.0

!               alpha    CD   
  cd(0:1, 0) = [-180.0, 0.0000]
  cd(0:1, 1) = [-175.0, 0.0099]
  cd(0:1, 2) = [-170.0, 0.0392]
  cd(0:1, 3) = [-165.0, 0.0871]
  cd(0:1, 4) = [-160.0, 0.1521]
  cd(0:1, 5) = [-155.0, 0.2322]
  cd(0:1, 6) = [-150.0, 0.3250]
  cd(0:1, 7) = [-145.0, 0.4277]
  cd(0:1, 8) = [-140.0, 0.5371]
  cd(0:1, 9) = [-135.0, 0.6500]
  cd(0:1,10) = [-130.0, 0.7629]
  cd(0:1,11) = [-125.0, 0.8723]
  cd(0:1,12) = [-120.0, 0.9750]
  cd(0:1,13) = [-115.0, 1.0680]
  cd(0:1,14) = [-110.0, 1.1480]
  cd(0:1,15) = [-105.0, 1.2130]
  cd(0:1,16) = [-100.0, 1.2610]
  cd(0:1,17) = [-95.0, 1.2900]
  cd(0:1,18) = [-90.0, 1.3000]
  cd(0:1,19) = [-85.0, 1.2900]
  cd(0:1,20) = [-80.0, 1.2610]
  cd(0:1,21) = [-75.0, 1.2130]
  cd(0:1,22) = [-70.0, 1.1480]
  cd(0:1,23) = [-65.0, 1.0680]
  cd(0:1,24) = [-60.0, 0.9750]
  cd(0:1,25) = [-55.0, 0.8723]
  cd(0:1,26) = [-50.0, 0.7629]
  cd(0:1,27) = [-45.0, 0.6500]
  cd(0:1,28) = [-40.0, 0.5282]
  cd(0:1,29) = [-39.0, 0.5026]
  cd(0:1,30) = [-38.0, 0.4671]
  cd(0:1,31) = [-37.0, 0.4345]
  cd(0:1,32) = [-36.0, 0.3989]
  cd(0:1,33) = [-35.0, 0.3634]
  cd(0:1,34) = [-34.0, 0.3278]
  cd(0:1,35) = [-33.0, 0.3013]
  cd(0:1,36) = [-32.0, 0.2729]
  cd(0:1,37) = [-30.0, 0.2332]
  cd(0:1,38) = [-28.0, 0.2034]
  cd(0:1,39) = [-26.0, 0.1858]
  cd(0:1,40) = [-24.0, 0.1681]
  cd(0:1,41) = [-22.0, 0.1526]
  cd(0:1,42) = [-20.0, 0.1370]
  cd(0:1,43) = [-18.0, 0.1240]
  cd(0:1,44) = [-16.0, 0.1109]
  cd(0:1,45) = [-14.0, 0.1009]
  cd(0:1,46) = [-12.0, 0.0908]
  cd(0:1,47) = [-10.0, 0.0836]
  cd(0:1,48) = [-8.0, 0.0765]
  cd(0:1,49) = [-6.0, 0.0748]
  cd(0:1,50) = [-4.0, 0.0731]
  cd(0:1,51) = [-2.0, 0.0755]
  cd(0:1,52) = [0.0, 0.0780]
  cd(0:1,53) = [2.0, 0.0780]
  cd(0:1,54) = [4.0, 0.0708]
  cd(0:1,55) = [6.0, 0.0748]
  cd(0:1,56) = [8.0, 0.0859]
  cd(0:1,57) = [10.0, 0.0999]
  cd(0:1,58) = [12.0, 0.1115]
  cd(0:1,59) = [14.0, 0.1207]
  cd(0:1,60) = [16.0, 0.1368]
  cd(0:1,61) = [18.0, 0.1500]
  cd(0:1,62) = [20.0, 0.1689]
  cd(0:1,63) = [22.0, 0.2066]
  cd(0:1,64) = [24.0, 0.3092]
  cd(0:1,65) = [26.0, 0.4344]
  cd(0:1,66) = [28.0, 0.5512]
  cd(0:1,67) = [30.0, 0.6732]
  cd(0:1,68) = [32.0, 0.7756]
  cd(0:1,69) = [33.0, 0.8209]
  cd(0:1,70) = [34.0, 0.8551]
  cd(0:1,71) = [35.0, 0.8862]
  cd(0:1,72) = [36.0, 0.9141]
  cd(0:1,73) = [37.0, 0.9390]
  cd(0:1,74) = [38.0, 0.9709]
  cd(0:1,75) = [39.0, 0.9996]
  cd(0:1,76) = [40.0, 1.0250]
  cd(0:1,77) = [45.0, 1.1420]
  cd(0:1,78) = [50.0, 1.1610]
  cd(0:1,79) = [55.0, 1.1660]
  cd(0:1,80) = [60.0, 1.1750]
  cd(0:1,81) = [65.0, 1.1780]
  cd(0:1,82) = [70.0, 1.1980]
  cd(0:1,83) = [75.0, 1.2130]
  cd(0:1,84) = [80.0, 1.2610]
  cd(0:1,85) = [85.0, 1.2900]
  cd(0:1,86) = [90.0, 1.3000]
  cd(0:1,87) = [95.0, 1.2900]
  cd(0:1,88) = [100.0, 1.2610]
  cd(0:1,89) = [105.0, 1.2130]
  cd(0:1,90) = [110.0, 1.1480]
  cd(0:1,91) = [115.0, 1.0680]
  cd(0:1,92) = [120.0, 0.9750]
  cd(0:1,93) = [125.0, 0.8723]
  cd(0:1,94) = [130.0, 0.7629]
  cd(0:1,95) = [135.0, 0.6500]
  cd(0:1,96) = [140.0, 0.5371]
  cd(0:1,97) = [145.0, 0.4277]
  cd(0:1,98) = [150.0, 0.3250]
  cd(0:1,99) = [155.0, 0.2322]
  cd(0:1,100) = [160.0, 0.1521]
  cd(0:1,101) = [165.0, 0.0871]
  cd(0:1,102) = [170.0, 0.0392]
  cd(0:1,103) = [175.0, 0.0099]
  cd(0:1,104) = [180.0, 0.0000]
  
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
!     PRINT*,'alpha < tabulated range in calc_cd2_dtu10.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd2 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd2_dtu10.pro'
  ENDIF
  
!print,y,cd2
  
!_________________________________________________________________________
  
END FUNCTION calc_cd2_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd3_dtu10( y ) RESULT( cd3 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: FFA-W3-480
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd3
  INTEGER :: i

  cd3 = 0.0

!                 alpha    CD   
  cd(0:1, 0) = [-180.0, 0.0000]
  cd(0:1, 1) = [-175.0, 0.0099]
  cd(0:1, 2) = [-170.0, 0.0392]
  cd(0:1, 3) = [-165.0, 0.0871]
  cd(0:1, 4) = [-160.0, 0.1521]
  cd(0:1, 5) = [-155.0, 0.2322]
  cd(0:1, 6) = [-150.0, 0.3250]
  cd(0:1, 7) = [-145.0, 0.4277]
  cd(0:1, 8) = [-140.0, 0.5371]
  cd(0:1, 9) = [-135.0, 0.6500]
  cd(0:1,10) = [-130.0, 0.7629]
  cd(0:1,11) = [-125.0, 0.8723]
  cd(0:1,12) = [-120.0, 0.9750]
  cd(0:1,13) = [-115.0, 1.0678]
  cd(0:1,14) = [-110.0, 1.1479]
  cd(0:1,15) = [-105.0, 1.2129]
  cd(0:1,16) = [-100.0, 1.2608]
  cd(0:1,17) = [-95.0, 1.2901]
  cd(0:1,18) = [-90.0, 1.3000]
  cd(0:1,19) = [-85.0, 1.2901]
  cd(0:1,20) = [-80.0, 1.2608]
  cd(0:1,21) = [-75.0, 1.2129]
  cd(0:1,22) = [-70.0, 1.1479]
  cd(0:1,23) = [-65.0, 1.0678]
  cd(0:1,24) = [-60.0, 0.9750]
  cd(0:1,25) = [-55.0, 0.8723]
  cd(0:1,26) = [-50.0, 0.7629]
  cd(0:1,27) = [-45.0, 0.6500]
  cd(0:1,28) = [-40.0, 0.5352]
  cd(0:1,29) = [-39.0, 0.5136]
  cd(0:1,30) = [-38.0, 0.4851]
  cd(0:1,31) = [-37.0, 0.4565]
  cd(0:1,32) = [-36.0, 0.4279]
  cd(0:1,33) = [-35.0, 0.3924]
  cd(0:1,34) = [-34.0, 0.3638]
  cd(0:1,35) = [-33.0, 0.3383]
  cd(0:1,36) = [-32.0, 0.3123]
  cd(0:1,37) = [-30.0, 0.2663]
  cd(0:1,38) = [-28.0, 0.2402]
  cd(0:1,39) = [-26.0, 0.2169]
  cd(0:1,40) = [-24.0, 0.1936]
  cd(0:1,41) = [-22.0, 0.1732]
  cd(0:1,42) = [-20.0, 0.1529]
  cd(0:1,43) = [-18.0, 0.1355]
  cd(0:1,44) = [-16.0, 0.1180]
  cd(0:1,45) = [-14.0, 0.1035]
  cd(0:1,46) = [-12.0, 0.0890]
  cd(0:1,47) = [-10.0, 0.0773]
  cd(0:1,48) = [-8.0, 0.0656]
  cd(0:1,49) = [-6.0, 0.0563]
  cd(0:1,50) = [-4.0, 0.0470]
  cd(0:1,51) = [-2.0, 0.0405]
  cd(0:1,52) = [0.0, 0.0341]
  cd(0:1,53) = [2.0, 0.0316]
  cd(0:1,54) = [4.0, 0.0343]
  cd(0:1,55) = [6.0, 0.0451]
  cd(0:1,56) = [8.0, 0.0700]
  cd(0:1,57) = [10.0, 0.0886]
  cd(0:1,58) = [12.0, 0.0993]
  cd(0:1,59) = [14.0, 0.1070]
  cd(0:1,60) = [16.0, 0.1163]
  cd(0:1,61) = [18.0, 0.1317]
  cd(0:1,62) = [20.0, 0.1570]
  cd(0:1,63) = [22.0, 0.2757]
  cd(0:1,64) = [24.0, 0.4224]
  cd(0:1,65) = [26.0, 0.5610]
  cd(0:1,66) = [28.0, 0.6861]
  cd(0:1,67) = [30.0, 0.7958]
  cd(0:1,68) = [32.0, 0.8915]
  cd(0:1,69) = [33.0, 0.9229]
  cd(0:1,70) = [34.0, 0.9472]
  cd(0:1,71) = [35.0, 0.9664]
  cd(0:1,72) = [36.0, 0.9889]
  cd(0:1,73) = [37.0, 1.0073]
  cd(0:1,74) = [38.0, 1.0284]
  cd(0:1,75) = [39.0, 1.0521]
  cd(0:1,76) = [40.0, 1.0769]
  cd(0:1,77) = [45.0, 1.1697]
  cd(0:1,78) = [50.0, 1.1965]
  cd(0:1,79) = [55.0, 1.1957]
  cd(0:1,80) = [60.0, 1.1900]
  cd(0:1,81) = [65.0, 1.1928]
  cd(0:1,82) = [70.0, 1.2029]
  cd(0:1,83) = [75.0, 1.2129]
  cd(0:1,84) = [80.0, 1.2608]
  cd(0:1,85) = [85.0, 1.2901]
  cd(0:1,86) = [90.0, 1.3000]
  cd(0:1,87) = [95.0, 1.2901]
  cd(0:1,88) = [100.0, 1.2608]
  cd(0:1,89) = [105.0, 1.2129]
  cd(0:1,90) = [110.0, 1.1479]
  cd(0:1,91) = [115.0, 1.0678]
  cd(0:1,92) = [120.0, 0.9750]
  cd(0:1,93) = [125.0, 0.8723]
  cd(0:1,94) = [130.0, 0.7629]
  cd(0:1,95) = [135.0, 0.6500]
  cd(0:1,96) = [140.0, 0.5371]
  cd(0:1,97) = [145.0, 0.4277]
  cd(0:1,98) = [150.0, 0.3250]
  cd(0:1,99) = [155.0, 0.2322]
  cd(0:1,100) = [160.0, 0.1521]
  cd(0:1,101) = [165.0, 0.0871]
  cd(0:1,102) = [170.0, 0.0392]
  cd(0:1,103) = [175.0, 0.0099]
  cd(0:1,104) = [180.0, 0.0000]
  
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
!     PRINT*,'alpha < tabulated range in calc_cd3_dtu10.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd3 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd3_dtu10.pro'
  ENDIF
  
!print,y,cd3
  
!_________________________________________________________________________
  
END FUNCTION calc_cd3_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd4_dtu10( y ) RESULT( cd4 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: FFA-W3-360
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd4
  INTEGER :: i

  cd4 = 0.0

!                alpha    CD  
  cd(0:1, 0) = [-180.0, 0.0000]
  cd(0:1, 1) = [-175.0, 0.0099]
  cd(0:1, 2) = [-170.0, 0.0392]
  cd(0:1, 3) = [-165.0, 0.0871]
  cd(0:1, 4) = [-160.0, 0.1521]
  cd(0:1, 5) = [-155.0, 0.2322]
  cd(0:1, 6) = [-150.0, 0.3250]
  cd(0:1, 7) = [-145.0, 0.4277]
  cd(0:1, 8) = [-140.0, 0.5371]
  cd(0:1, 9) = [-135.0, 0.6500]
  cd(0:1,10) = [-130.0, 0.7629]
  cd(0:1,11) = [-125.0, 0.8723]
  cd(0:1,12) = [-120.0, 0.9750]
  cd(0:1,13) = [-115.0, 1.0678]
  cd(0:1,14) = [-110.0, 1.1479]
  cd(0:1,15) = [-105.0, 1.2129]
  cd(0:1,16) = [-100.0, 1.2608]
  cd(0:1,17) = [-95.0, 1.2901]
  cd(0:1,18) = [-90.0, 1.3000]
  cd(0:1,19) = [-85.0, 1.2901]
  cd(0:1,20) = [-80.0, 1.2608]
  cd(0:1,21) = [-75.0, 1.2129]
  cd(0:1,22) = [-70.0, 1.1479]
  cd(0:1,23) = [-65.0, 1.0678]
  cd(0:1,24) = [-60.0, 0.9750]
  cd(0:1,25) = [-55.0, 0.8723]
  cd(0:1,26) = [-50.0, 0.7629]
  cd(0:1,27) = [-45.0, 0.6500]
  cd(0:1,28) = [-40.0, 0.5352]
  cd(0:1,29) = [-39.0, 0.5136]
  cd(0:1,30) = [-38.0, 0.4851]
  cd(0:1,31) = [-37.0, 0.4635]
  cd(0:1,32) = [-36.0, 0.4349]
  cd(0:1,33) = [-35.0, 0.4064]
  cd(0:1,34) = [-34.0, 0.3778]
  cd(0:1,35) = [-33.0, 0.3523]
  cd(0:1,36) = [-32.0, 0.3315]
  cd(0:1,37) = [-30.0, 0.2777]
  cd(0:1,38) = [-28.0, 0.2338]
  cd(0:1,39) = [-26.0, 0.2045]
  cd(0:1,40) = [-24.0, 0.1751]
  cd(0:1,41) = [-22.0, 0.1513]
  cd(0:1,42) = [-20.0, 0.1274]
  cd(0:1,43) = [-18.0, 0.1085]
  cd(0:1,44) = [-16.0, 0.0896]
  cd(0:1,45) = [-14.0, 0.0748]
  cd(0:1,46) = [-12.0, 0.0600]
  cd(0:1,47) = [-10.0, 0.0485]
  cd(0:1,48) = [-8.0, 0.0370]
  cd(0:1,49) = [-6.0, 0.0294]
  cd(0:1,50) = [-4.0, 0.0219]
  cd(0:1,51) = [-2.0, 0.0203]
  cd(0:1,52) = [0.0, 0.0187]
  cd(0:1,53) = [2.0, 0.0188]
  cd(0:1,54) = [4.0, 0.0196]
  cd(0:1,55) = [6.0, 0.0213]
  cd(0:1,56) = [8.0, 0.0240]
  cd(0:1,57) = [10.0, 0.0279]
  cd(0:1,58) = [12.0, 0.0365]
  cd(0:1,59) = [14.0, 0.0760]
  cd(0:1,60) = [16.0, 0.1165]
  cd(0:1,61) = [18.0, 0.1571]
  cd(0:1,62) = [20.0, 0.2063]
  cd(0:1,63) = [22.0, 0.3057]
  cd(0:1,64) = [24.0, 0.4153]
  cd(0:1,65) = [26.0, 0.5163]
  cd(0:1,66) = [28.0, 0.6069]
  cd(0:1,67) = [30.0, 0.6892]
  cd(0:1,68) = [32.0, 0.7625]
  cd(0:1,69) = [33.0, 0.7954]
  cd(0:1,70) = [34.0, 0.8232]
  cd(0:1,71) = [35.0, 0.8446]
  cd(0:1,72) = [36.0, 0.8684]
  cd(0:1,73) = [37.0, 0.8912]
  cd(0:1,74) = [38.0, 0.9066]
  cd(0:1,75) = [39.0, 0.9255]
  cd(0:1,76) = [40.0, 0.9385]
  cd(0:1,77) = [45.0, 1.0290]
  cd(0:1,78) = [50.0, 1.0715]
  cd(0:1,79) = [55.0, 1.0857]
  cd(0:1,80) = [60.0, 1.1000]
  cd(0:1,81) = [65.0, 1.1278]
  cd(0:1,82) = [70.0, 1.1629]
  cd(0:1,83) = [75.0, 1.2129]
  cd(0:1,84) = [80.0, 1.2608]
  cd(0:1,85) = [85.0, 1.2901]
  cd(0:1,86) = [90.0, 1.3000]
  cd(0:1,87) = [95.0, 1.2901]
  cd(0:1,88) = [100.0, 1.2608]
  cd(0:1,89) = [105.0, 1.2129]
  cd(0:1,90) = [110.0, 1.1479]
  cd(0:1,91) = [115.0, 1.0678]
  cd(0:1,92) = [120.0, 0.9750]
  cd(0:1,93) = [125.0, 0.8723]
  cd(0:1,94) = [130.0, 0.7629]
  cd(0:1,95) = [135.0, 0.6500]
  cd(0:1,96) = [140.0, 0.5371]
  cd(0:1,97) = [145.0, 0.4277]
  cd(0:1,98) = [150.0, 0.3250]
  cd(0:1,99) = [155.0, 0.2322]
  cd(0:1,100) = [160.0, 0.1521]
  cd(0:1,101) = [165.0, 0.0871]
  cd(0:1,102) = [170.0, 0.0392]
  cd(0:1,103) = [175.0, 0.0099]
  cd(0:1,104) = [180.0, 0.0000]

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
!     PRINT*,'alpha < tabulated range in calc_cd4_dtu10.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd4 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd4_dtu10.pro'
  ENDIF
  
!print,y,cd4
  
!_________________________________________________________________________
  
END FUNCTION calc_cd4_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd5_dtu10( y ) RESULT( cd5 )
!_________________________________________________________________________
!
! Data from DTU 10 MW: FFA-W3-301
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd5
  INTEGER :: i

  cd5 = 0.0

!                alpha    CD  
  cd(0:1, 0) = [-180.0, 0.0000]
  cd(0:1, 1) = [-175.0, 0.0099]
  cd(0:1, 2) = [-170.0, 0.0392]
  cd(0:1, 3) = [-165.0, 0.0871]
  cd(0:1, 4) = [-160.0, 0.1521]
  cd(0:1, 5) = [-155.0, 0.2322]
  cd(0:1, 6) = [-150.0, 0.3250]
  cd(0:1, 7) = [-145.0, 0.4277]
  cd(0:1, 8) = [-140.0, 0.5371]
  cd(0:1, 9) = [-135.0, 0.6500]
  cd(0:1,10) = [-130.0, 0.7629]
  cd(0:1,11) = [-125.0, 0.8723]
  cd(0:1,12) = [-120.0, 0.9750]
  cd(0:1,13) = [-115.0, 1.0678]
  cd(0:1,14) = [-110.0, 1.1479]
  cd(0:1,15) = [-105.0, 1.2129]
  cd(0:1,16) = [-100.0, 1.2608]
  cd(0:1,17) = [-95.0, 1.2901]
  cd(0:1,18) = [-90.0, 1.3000]
  cd(0:1,19) = [-85.0, 1.2901]
  cd(0:1,20) = [-80.0, 1.2608]
  cd(0:1,21) = [-75.0, 1.2129]
  cd(0:1,22) = [-70.0, 1.1479]
  cd(0:1,23) = [-65.0, 1.0678]
  cd(0:1,24) = [-60.0, 0.9750]
  cd(0:1,25) = [-55.0, 0.8723]
  cd(0:1,26) = [-50.0, 0.7629]
  cd(0:1,27) = [-45.0, 0.6500]
  cd(0:1,28) = [-40.0, 0.5352]
  cd(0:1,29) = [-39.0, 0.5066]
  cd(0:1,30) = [-38.0, 0.4851]
  cd(0:1,31) = [-37.0, 0.4565]
  cd(0:1,32) = [-36.0, 0.4279]
  cd(0:1,33) = [-35.0, 0.3994]
  cd(0:1,34) = [-34.0, 0.3708]
  cd(0:1,35) = [-33.0, 0.3353]
  cd(0:1,36) = [-32.0, 0.3097]
  cd(0:1,37) = [-30.0, 0.2663]
  cd(0:1,38) = [-28.0, 0.2229]
  cd(0:1,39) = [-26.0, 0.1941]
  cd(0:1,40) = [-24.0, 0.1654]
  cd(0:1,41) = [-22.0, 0.1417]
  cd(0:1,42) = [-20.0, 0.1181]
  cd(0:1,43) = [-18.0, 0.0986]
  cd(0:1,44) = [-16.0, 0.0792]
  cd(0:1,45) = [-14.0, 0.0643]
  cd(0:1,46) = [-12.0, 0.0495]
  cd(0:1,47) = [-10.0, 0.0381]
  cd(0:1,48) = [-8.0, 0.0267]
  cd(0:1,49) = [-6.0, 0.0204]
  cd(0:1,50) = [-4.0, 0.0140]
  cd(0:1,51) = [-2.0, 0.0129]
  cd(0:1,52) = [0.0, 0.0118]
  cd(0:1,53) = [2.0, 0.0119]
  cd(0:1,54) = [4.0, 0.0125]
  cd(0:1,55) = [6.0, 0.0136]
  cd(0:1,56) = [8.0, 0.0152]
  cd(0:1,57) = [10.0, 0.0180]
  cd(0:1,58) = [12.0, 0.0224]
  cd(0:1,59) = [14.0, 0.0303]
  cd(0:1,60) = [16.0, 0.0539]
  cd(0:1,61) = [18.0, 0.0954]
  cd(0:1,62) = [20.0, 0.1435]
  cd(0:1,63) = [22.0, 0.2280]
  cd(0:1,64) = [24.0, 0.3148]
  cd(0:1,65) = [26.0, 0.3926]
  cd(0:1,66) = [28.0, 0.4623]
  cd(0:1,67) = [30.0, 0.5266]
  cd(0:1,68) = [32.0, 0.5855]
  cd(0:1,69) = [33.0, 0.6215]
  cd(0:1,70) = [34.0, 0.6562]
  cd(0:1,71) = [35.0, 0.6897]
  cd(0:1,72) = [36.0, 0.7148]
  cd(0:1,73) = [37.0, 0.7464]
  cd(0:1,74) = [38.0, 0.7762]
  cd(0:1,75) = [39.0, 0.7982]
  cd(0:1,76) = [40.0, 0.8197]
  cd(0:1,77) = [45.0, 0.9122]
  cd(0:1,78) = [50.0, 0.9665]
  cd(0:1,79) = [55.0, 0.9957]
  cd(0:1,80) = [60.0, 1.0350]
  cd(0:1,81) = [65.0, 1.0828]
  cd(0:1,82) = [70.0, 1.1479]
  cd(0:1,83) = [75.0, 1.2129]
  cd(0:1,84) = [80.0, 1.2608]
  cd(0:1,85) = [85.0, 1.2901]
  cd(0:1,86) = [90.0, 1.3000]
  cd(0:1,87) = [95.0, 1.2901]
  cd(0:1,88) = [100.0, 1.2608]
  cd(0:1,89) = [105.0, 1.2129]
  cd(0:1,90) = [110.0, 1.1479]
  cd(0:1,91) = [115.0, 1.0678]
  cd(0:1,92) = [120.0, 0.9750]
  cd(0:1,93) = [125.0, 0.8723]
  cd(0:1,94) = [130.0, 0.7629]
  cd(0:1,95) = [135.0, 0.6500]
  cd(0:1,96) = [140.0, 0.5371]
  cd(0:1,97) = [145.0, 0.4277]
  cd(0:1,98) = [150.0, 0.3250]
  cd(0:1,99) = [155.0, 0.2322]
  cd(0:1,100) = [160.0, 0.1521]
  cd(0:1,101) = [165.0, 0.0871]
  cd(0:1,102) = [170.0, 0.0392]
  cd(0:1,103) = [175.0, 0.0099]
  cd(0:1,104) = [180.0, 0.0000]
  
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
!     PRINT*,'alpha < tabulated range in calc_cd5_dtu10.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd5 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd5_dtu10.pro'
  ENDIF
  
!print,y,cd5
  
!_________________________________________________________________________
  
END FUNCTION calc_cd5_dtu10
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd6_dtu10( y ) RESULT( cd6 )
!_________________________________________________________________________
!
! Data from  DTU 10 MW: FFA-W3-241
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL, PARAMETER :: na = 105

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd6
  INTEGER :: i

  cd6 = 0.0

!                alpha    CD  
  cd(0:1, 0) = [-180.0, 0.0000]
  cd(0:1, 1) = [-175.0, 0.0114]
  cd(0:1, 2) = [-170.0, 0.0452]
  cd(0:1, 3) = [-165.0, 0.1005]
  cd(0:1, 4) = [-160.0, 0.1755]
  cd(0:1, 5) = [-155.0, 0.2679]
  cd(0:1, 6) = [-150.0, 0.3750]
  cd(0:1, 7) = [-145.0, 0.4935]
  cd(0:1, 8) = [-140.0, 0.6197]
  cd(0:1, 9) = [-135.0, 0.7500]
  cd(0:1,10) = [-130.0, 0.8803]
  cd(0:1,11) = [-125.0, 1.0065]
  cd(0:1,12) = [-120.0, 1.1250]
  cd(0:1,13) = [-115.0, 1.2321]
  cd(0:1,14) = [-110.0, 1.3245]
  cd(0:1,15) = [-105.0, 1.3995]
  cd(0:1,16) = [-100.0, 1.4548]
  cd(0:1,17) = [-95.0, 1.4886]
  cd(0:1,18) = [-90.0, 1.5000]
  cd(0:1,19) = [-85.0, 1.4886]
  cd(0:1,20) = [-80.0, 1.4548]
  cd(0:1,21) = [-75.0, 1.3995]
  cd(0:1,22) = [-70.0, 1.3245]
  cd(0:1,23) = [-65.0, 1.2321]
  cd(0:1,24) = [-60.0, 1.1250]
  cd(0:1,25) = [-55.0, 1.0065]
  cd(0:1,26) = [-50.0, 0.8803]
  cd(0:1,27) = [-45.0, 0.7500]
  cd(0:1,28) = [-40.0, 0.6095]
  cd(0:1,29) = [-39.0, 0.5845]
  cd(0:1,30) = [-38.0, 0.5517]
  cd(0:1,31) = [-37.0, 0.5267]
  cd(0:1,32) = [-36.0, 0.4937]
  cd(0:1,33) = [-35.0, 0.4609]
  cd(0:1,34) = [-34.0, 0.4279]
  cd(0:1,35) = [-33.0, 0.3869]
  cd(0:1,36) = [-32.0, 0.3347]
  cd(0:1,37) = [-30.0, 0.2862]
  cd(0:1,38) = [-28.0, 0.2277]
  cd(0:1,39) = [-26.0, 0.1750]
  cd(0:1,40) = [-24.0, 0.1423]
  cd(0:1,41) = [-22.0, 0.1183]
  cd(0:1,42) = [-20.0, 0.0944]
  cd(0:1,43) = [-18.0, 0.0772]
  cd(0:1,44) = [-16.0, 0.0601]
  cd(0:1,45) = [-14.0, 0.0468]
  cd(0:1,46) = [-12.0, 0.0335]
  cd(0:1,47) = [-10.0, 0.0242]
  cd(0:1,48) = [-8.0, 0.0150]
  cd(0:1,49) = [-6.0, 0.0126]
  cd(0:1,50) = [-4.0, 0.0103]
  cd(0:1,51) = [-2.0, 0.0099]
  cd(0:1,52) = [0.0, 0.0096]
  cd(0:1,53) = [2.0, 0.0098]
  cd(0:1,54) = [4.0, 0.0103]
  cd(0:1,55) = [6.0, 0.0112]
  cd(0:1,56) = [8.0, 0.0126]
  cd(0:1,57) = [10.0, 0.0146]
  cd(0:1,58) = [12.0, 0.0175]
  cd(0:1,59) = [14.0, 0.0227]
  cd(0:1,60) = [16.0, 0.0357]
  cd(0:1,61) = [18.0, 0.0649]
  cd(0:1,62) = [20.0, 0.1015]
  cd(0:1,63) = [22.0, 0.1411]
  cd(0:1,64) = [24.0, 0.1809]
  cd(0:1,65) = [26.0, 0.2355]
  cd(0:1,66) = [28.0, 0.2998]
  cd(0:1,67) = [30.0, 0.3640]
  cd(0:1,68) = [32.0, 0.4380]
  cd(0:1,69) = [33.0, 0.4752]
  cd(0:1,70) = [34.0, 0.5083]
  cd(0:1,71) = [35.0, 0.5410]
  cd(0:1,72) = [36.0, 0.5737]
  cd(0:1,73) = [37.0, 0.5980]
  cd(0:1,74) = [38.0, 0.6305]
  cd(0:1,75) = [39.0, 0.6545]
  cd(0:1,76) = [40.0, 0.6785]
  cd(0:1,77) = [45.0, 0.7960]
  cd(0:1,78) = [50.0, 0.8960]
  cd(0:1,79) = [55.0, 1.0104]
  cd(0:1,80) = [60.0, 1.1250]
  cd(0:1,81) = [65.0, 1.2321]
  cd(0:1,82) = [70.0, 1.3245]
  cd(0:1,83) = [75.0, 1.3995]
  cd(0:1,84) = [80.0, 1.4548]
  cd(0:1,85) = [85.0, 1.4886]
  cd(0:1,86) = [90.0, 1.5000]
  cd(0:1,87) = [95.0, 1.4886]
  cd(0:1,88) = [100.0, 1.4548]
  cd(0:1,89) = [105.0, 1.3995]
  cd(0:1,90) = [110.0, 1.3245]
  cd(0:1,91) = [115.0, 1.2321]
  cd(0:1,92) = [120.0, 1.1250]
  cd(0:1,93) = [125.0, 1.0065]
  cd(0:1,94) = [130.0, 0.8803]
  cd(0:1,95) = [135.0, 0.7500]
  cd(0:1,96) = [140.0, 0.6197]
  cd(0:1,97) = [145.0, 0.4935]
  cd(0:1,98) = [150.0, 0.3750]
  cd(0:1,99) = [155.0, 0.2679]
  cd(0:1,100) = [160.0, 0.1755]
  cd(0:1,101) = [165.0, 0.1005]
  cd(0:1,102) = [170.0, 0.0452]
  cd(0:1,103) = [175.0, 0.0114]
  cd(0:1,104) = [180.0, 0.0000]
  
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
!     PRINT*,'alpha < tabulated range in calc_cd6_dtu10.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd6 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd6_dtu10.pro'
  ENDIF
  
!print,y,cd6
  
!_________________________________________________________________________
  
END FUNCTION calc_cd6_dtu10
!_________________________________________________________________________

END MODULE module_gad_dtu10

