!WRF
!
MODULE module_tsout

USE module_model_constants

CONTAINS

SUBROUTINE compute_vertical_slice( slices_u, slices_v,                        &
                                   slices_w, slices_t, slices_z,              &
                                   u, v, w, t, z, z_at_w, ht,                 &
                                   num_slices,                                &
                                   slice_heights,                             &
                                   ids, ide, jds, jde, kds, kde,              &
                                   ims, ime, jms, jme, kms, kme,              &
                                   its, ite, jts, jte, kts, kte               )

!=======================================================================
!
!  This subroutine calculates interpolated horizontal slices at specified heights.
!
!======================================================================= 

   IMPLICIT NONE

   REAL, DIMENSION(ims:ime,1:num_slices,jms:jme), INTENT( OUT ) :: slices_u, slices_v, &
                                                                   slices_w, slices_t

   REAL, DIMENSION(1:num_slices), INTENT( OUT ) :: slices_z
   
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(  IN ) :: u, v, w, t, z, z_at_w

   REAL, DIMENSION(ims:ime,jms:jme), INTENT(  IN ) :: ht
   
   INTEGER, INTENT(  IN ) :: num_slices
   
   REAL , DIMENSION (1:num_slices), INTENT( IN )  :: slice_heights
   
   INTEGER , INTENT( IN  ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte

   ! local variables
   
   INTEGER :: i, j, k, k_slice

   REAL :: d_z, d_1, d_2, fac_1, fac_2
   REAL :: slice_height_abs
   REAL :: u_m_1, u_m_2, v_m_1, v_m_2
   
!-----------------------------------------------------------------------
! Executable code starts here
!

   DO k = 1, num_slices
 
     slices_z(k) = slice_heights(k)
!     print*,k,slice_heights(k)
     
   END DO

   
   DO k_slice = 1, num_slices

!      print*,k_slice,slice_heights(k_slice)

      DO i = its, MIN(ite,ide-1)
         DO j = jts, MIN(jte,jde-1)

            slice_height_abs = slice_heights(k_slice) + ht(i,j)
            
            DO k = kts+1, MIN(kte,kde-1)
               
               IF ( z_at_w(i,k,j) .GE. slice_height_abs ) THEN

                   d_z = z_at_w(i,k,j) - z_at_w(i,k-1,j)
                   d_2 = z_at_w(i,k,j) - slice_height_abs
                   d_1 = slice_height_abs - z_at_w(i,k-1,j)
                   fac_2 = d_2/d_z
                   fac_1 = d_1/d_z
             
                   slices_w(i,k_slice,j) = fac_2*w(i,k-1,j) + fac_1*w(i,k,j)

!                   IF ( (i .EQ. its+2) .AND. (j .EQ. jts+2) )print*,k,fac_1*z_at_w(i,k,j)+fac_2*z_at_w(i,k-1,j)
                   
                   GOTO 1
               
               ENDIF

            END DO !k

1 CONTINUE

            DO k = kts+1, MIN(kte,kde-1)
            
               IF ( z(i,k,j) .GE. slice_height_abs ) THEN

                  d_z = z(i,k,j) - z(i,k-1,j)
                  d_2 = z(i,k,j) - slice_height_abs
                  d_1 = slice_height_abs - z(i,k-1,j)

                  fac_2 = d_2/d_z
                  fac_1 = d_1/d_z
               
                  u_m_2 = 0.5*(u(i+1,k,j) + u(i,k,j) ) ! u at cell center level k
                  u_m_1 = 0.5*(u(i+1,k-1,j) + u(i,k-1,j) ) ! u at cell center level k-1
                  v_m_2 = 0.5*(v(i,k,j+1) + v(i,k,j) ) ! v at cell center level k
                  v_m_1 = 0.5*(v(i,k-1,j+1) + v(i,k-1,j) ) ! v at cell center level k-1

                  slices_u(i,k_slice,j) = fac_2*u_m_1 + fac_1*u_m_2
                  slices_v(i,k_slice,j) = fac_2*v_m_1 + fac_1*v_m_2
                  slices_t(i,k_slice,j) = fac_2*t(i,k-1,j) + fac_1*t(i,k,j)

!                   IF ( (i .EQ. its+2) .AND. (j .EQ. jts+2) )print*,k,fac_1*z(i,k,j)+fac_2*z(i,k-1,j)
                  
                  GOTO 2
               
               ENDIF

            END DO !k
         
2       CONTINUE
            
         END DO
      END DO
   
   END DO !k_slice
   
   RETURN

END SUBROUTINE compute_vertical_slice


  
SUBROUTINE cell_centered_calc( uts, vts, wts, tts, pts, tkets, zts,       &
                               t11ts, t22ts, t33ts, t12ts, t13ts, t23ts,  &
                               h3ts, usts, z0ts, powerts,                 &
                               u, v, w, t, p, pb, tke, ph, phb,           &
                               t11, t22, t33, t12, t13, t23,              &
                               h3, ust, z0, turb_power, n_turbines,       &
                               ids, ide, jds, jde, kds, kde,              &
                               ims, ime, jms, jme, kms, kme,              &
                               its, ite, jts, jte, kts, kte                )

!=======================================================================
!
!  This subroutine calculates cell centered parameters for reduced output
!
!======================================================================= 

   IMPLICIT NONE

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT( OUT ) :: uts, vts, wts, &
                                                       tts, pts, tkets, zts, &
                                                       t11ts, t22ts, t33ts,  &
                                                       t12ts, t13ts, t23ts,  &
                                                       h3ts 

   REAL, DIMENSION(ims:ime,jms:jme), INTENT( OUT ) :: usts, z0ts

   REAL, DIMENSION(n_turbines), INTENT( OUT ) :: powerts	

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT( IN ) :: u, v, w,           &
                                                             t, p, pb,          &
                                                             tke, ph, phb, t11, &
                                                             t22, t33, t12,     & 
                                                             t13, t23, h3 
                                                         
   REAL, DIMENSION(ims:ime,jms:jme), INTENT( IN ) :: ust, z0 
   
   REAL, DIMENSION(n_turbines), INTENT( IN ) :: turb_power

   INTEGER,   INTENT( IN ) :: n_turbines    ! number of turbines
      
   INTEGER , INTENT( IN  ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte

! local variables
   INTEGER :: i, j, k, n

!-----------------------------------------------------------------------
! executable starts here
!

   DO i = its, ite
      DO j = jts, jte
         
         DO k=kts,MIN(kte,kde-1)
            uts(i,k,j)  = 0.5 * ( u(i,k,j) + u(i+1,k,j) )
            vts(i,k,j)  = 0.5 * ( v(i,k,j) + v(i,k,j+1) )
            wts(i,k,j)  = 0.5 * ( w(i,k,j) + w(i,k+1,j) )
            tts(i,k,j)  = t(i,k,j)
            pts(i,k,j)  = p(i,k,j) + pb(i,k,j)
            tkets(i,k,j) = tke(i,k,j)
            t11ts(i,k,j) = t11(i,k,j)
            t22ts(i,k,j) = t22(i,k,j)
            t33ts(i,k,j) = t33(i,k,j)
            t12ts(i,k,j) = 0.25 * ( t12(i,k,j)   + t12(i+1,k,j) + t12(i,k,j+1) + t12(i+1,k,j+1) )
            t13ts(i,k,j) = 0.5 * ( t13(i,k,j) + t13(i+1,k,j) ) 
            t23ts(i,k,j) = 0.5 * ( t23(i,k,j) + t23(i,k,j+1) ) 
            h3ts(i,k,j)  = h3(i,k,j)
            zts(i,k,j)   = ( phb(i,k,j) + ph(i,k,j) )/g
         ENDDO
         
         t13ts(i,kde,j) = 0.5 * ( t13(i,kde,j) + t13(i+1,kde,j) ) 
         t23ts(i,kde,j) = 0.5 * ( t23(i,kde,j) + t23(i,kde,j+1) ) 
         h3ts(i,kde,j)  = h3(i,kde,j) 
         zts(i,kde,j)   = ( phb(i,kde,j) + ph(i,kde,j) )/g
                   
         usts(i,j) = ust(i,j)  
         z0ts(i,j) = z0(i,j)  
         
      END DO
   END DO
   
   DO n = 1,n_turbines

     powerts(n) = turb_power(n)

   END DO

   RETURN

END SUBROUTINE cell_centered_calc

END MODULE module_tsout
