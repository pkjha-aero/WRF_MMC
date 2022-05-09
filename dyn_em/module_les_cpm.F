! WRF:MODEL_LAYER:PHYSICS
 
    MODULE module_les_cpm

    USE module_model_constants    

#ifdef DM_PARALLEL
    USE module_dm
#endif

    
    CONTAINS

!!=======================================================================

    SUBROUTINE force_down_meso_pblh( meso_pblh, pblh,                     &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     its, ite, jts, jte, kts, kte         )

!-----------------------------------------------------------------------
!
! Assign pbl height to m_pblh array to be forced down for nested LES
! using the cell perturbation method
!
    IMPLICIT NONE

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL , DIMENSION( ims:ime, jms:jme ), INTENT( IN  ) :: pblh
    REAL , DIMENSION( ims:ime, jms:jme ), INTENT( OUT ) :: meso_pblh

    INTEGER :: i,j
         
    DO j = jts, jte
       DO i = its, ite

          meso_pblh(i,j) = pblh(i,j)

       END DO
    END DO
 
!    print*,'in m_pblh, m_pblh(its,jts)',m_pblh(its,jts)   

    END SUBROUTINE force_down_meso_pblh

!=======================================================================
 
    SUBROUTINE calc_cpm_t( cpm_opt, cpm_lim_z, cpm_amp, cpm_dt,  &
                           cpm_nb, cpm_sb, cpm_eb, cpm_wb,       &
                           cpm_ngc_h, cpm_ncells_h,              &
                           cpm_noff_ew_w, cpm_noff_ew_e,         &
                           cpm_noff_ns_s, cpm_noff_ns_n,         &
                           cpm_nshift_ew, cpm_nshift_ns,         &
                           cpm_ngc_v, cpm_noff_v,                &
                           prttms, prtdt, prtseed, pert_t,       &
                           meso_pblh_opt, meso_pblh,             &
                           t, u, v, rdz,                         &
                           dx, dt,                               &
                           ids, ide, jds, jde, kds, kde,         &
                           ims, ime, jms, jme, kms, kme,         &
                           its, ite, jts, jte, kts, kte          )

!-----------------------------------------------------------------------
!
!  Stochastic Cell Perturbation Module. Applies perturbations directly to
!  potential temperature  based on the procedure outlined
!  in Munoz-Esparza et al Phys. Fluids, 2020
!
!  More description to follow ...
!
      
    IMPLICIT NONE

#ifdef DM_PARALLEL
    INCLUDE 'mpif.h'
#endif

    INTEGER, INTENT( IN    )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: t         ! potential temperature                 [k]
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: pert_t    ! potential temperature perturbation    [k]
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: u         ! zonal wind component                  [m/s]
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: v         ! meridional wind component             [m/s]
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN    ) :: rdz       ! inverse vertical grid spacing         [1/m]
    REAL, DIMENSION( ims:ime, jms:jme ),          INTENT( IN    ) :: meso_pblh ! PBL height from messoscale PBL scheme [m]
    REAL, DIMENSION( kms:kme ),                   INTENT( INOUT ) :: prttms    ! time since last perturbation refresh  [s]
    REAL, DIMENSION( kms:kme ),                   INTENT( INOUT ) :: prtdt     ! time scale to refresh perturbations   [s]
    
    INTEGER, DIMENSION( kms:kme ),                INTENT( INOUT ) :: prtseed   ! 1D variable to store variable-length seed array

    INTEGER, INTENT( IN    ) :: cpm_opt                         ! which perturbation option to use
    INTEGER, INTENT( IN    ) :: cpm_nb, cpm_sb, cpm_eb, cpm_wb  ! specific boundaries to apply perturbations to 
    INTEGER, INTENT( IN    ) :: cpm_ngc_h, cpm_ngc_v            ! Number of grid cells per cpm cell in horizontal and vertical directions
    INTEGER, INTENT( IN    ) :: cpm_ncells_h                    ! Number of CPM cells in both horizontal directions
    INTEGER, INTENT( IN    ) :: cpm_noff_ew_w, cpm_noff_ew_e    ! Number of grid cells from east-west boundaries to start (w) and end (e) perturbations
    INTEGER, INTENT( IN    ) :: cpm_noff_ns_s, cpm_noff_ns_n    ! Number of grid cells from north-south boundaries to start (s) and end (n) perturbations
    INTEGER, INTENT( IN    ) :: cpm_noff_v                      ! Number of grid cells from bottom boundary to start perturbations
    INTEGER, INTENT( IN    ) :: cpm_nshift_ew, cpm_nshift_ns    ! Number of units to shift perturbations in east-weat and north-south directions
    INTEGER, INTENT( IN    ) :: meso_pblh_opt                   ! whether to use mesoscale PBL height (1) or not (0)
    
    REAL, INTENT( IN    ) :: cpm_lim_z                          ! Max height to apply cpm when meso_pbl_opt=0; lower bound when meso_pbl_opt=1
    REAL, INTENT( IN    ) :: cpm_amp                            ! which perturbation option to use
    REAL, INTENT( IN    ) :: cpm_dt                             ! timescale to apply perturbations (>0). Set =0 for automatic advective timescale method

    REAL,INTENT( IN    ) :: dt                                  ! model time step
    REAL,INTENT( IN    ) :: dx                                  ! horizontal grid spacing

    REAL, DIMENSION( its:ite, kts:kte, jts:jte ) :: z           ! height above surface at midpoint levels   
    REAL, DIMENSION( its:ite, jts:jte ) :: pblh                 ! planetary boundary layer height   
    
    INTEGER :: i, j, k, big_k, slab_k, m                        ! loop variables
    INTEGER :: i_start, i_end, j_start, j_end                   ! patch endpoints
    INTEGER :: north, south, east, west                         ! designate which boundaries to apply perturbations
    INTEGER :: i_seed                                           ! size of seed
    INTEGER :: seedsum                                          ! variable for summing the seed
    INTEGER :: ni, nj                               !           ! number of perturbation cells in i and j directions (for random number query)
    INTEGER :: n_slabs_k                                        ! number of perturbations cells in horizontal and vertical directions
    INTEGER :: k_slab_start, k_slab_end                         ! vertical indices over which to apply perturbations
    INTEGER :: k_geo                                            ! k index at which h_wg is computed
    INTEGER :: idum, jdum 
    REAL    :: h_geo                                            ! height at which to estimate ug
    REAL    :: ugeosum, vgeosum                                 !
    REAL    :: ugeolbsum, vgeolbsum                             !
    REAL    :: ugeolbavg, vgeolbavg, wsgeolbavg                 !
    REAL    :: uslabsum, vslabsum                               !
    REAL    :: uslablbsum, vslablbsum                           !
    REAL    :: uslablbavg, vslablbavg, wsslablbavg              !
    REAL    :: anglelbavg                                       !
    REAL    :: num_pts_in_sum                                   ! Variables for computing averages
    REAL    :: tpertmag                                         ! Magnitude of thermal perturbation
    REAL    :: dz                                               ! Depth
    REAL    :: ek_opt = 0.20                                    ! optimal value of the perturbation Ekman #
    REAL    :: pio2 = piconst/2.0  
    REAL    :: pio4 = piconst/4.0        
    REAL    :: max_pblh, max_pert_z 
    REAL    :: min_slab_z
    REAL    :: lambda
    REAL    :: pblh_check_time
    REAL    :: counter
    
    INTEGER, dimension( : ), allocatable :: seed                                       ! random number seed
    REAL, dimension( :, : ), allocatable :: perts_wb,perts_eb,perts_sb,perts_nb   ! random number arrays for each lateral boundary
  
#ifdef DM_PARALLEL
    INTEGER :: ierr
    INTEGER :: tag
    INTEGER :: master
    INTEGER :: status(MPI_STATUS_SIZE)
#endif

#ifdef DM_PARALLEL
    master = 0
    tag    = 0
#endif

! END DECLARATIONS

    n_slabs_k = kde/cpm_ngc_v
    
    ! LOOP OVER ALL VERTICAL PERTURBATION SLABS TO SEE IF IT IS TIME TO START OR REFRESH

    lambda = 0.875 ! factor to reduce advective refresh time slghtly to account for relaxation at lateral edges (21/24) 

    pblh_check_time = 300.0      ! Check every 5 mins to see if PBLH has changed
    
    DO slab_k = 1, n_slabs_k     ! Outer k-loop over number of vertical perturbation slabs
       
       prttms(slab_k) = prttms(slab_k) + dt
       
       IF ( prttms(slab_k) .GE. prtdt(slab_k) ) THEN !prdt should be zero at startup
          
          prttms(slab_k) = dt

          prtdt(slab_k) = pblh_check_time !CHECK IF PBLH HAS CHANGED
          
          print*,'Computing new perturbations, slab_k = ',slab_k
          
          ! ONLY COMPUTE/APPLY PERTURBATIONS FOR SLABS WITH A GLOBAL Z-VALUE BELOW GLOBAL PBLH MAXIMUM
          
          i_start = its
          i_end   = MIN(ite,ide)
          j_start = jts
          j_end   = MIN(jte,jde)
          
          IF (meso_pblh_opt .EQ. 0 ) THEN !SET PBLH TO A DESIRED VALUE
             
             DO j=j_start, j_end
                DO i=i_start, i_end
                   pblh(i,j) = cpm_lim_z
                END DO
             END DO
             
          ENDIF
          
          IF (meso_pblh_opt .EQ. 1 ) THEN !SET PBLH TO A DESIRED VALUE
             
             DO j=j_start, j_end
                DO i=i_start, i_end
                   pblh(i,j) = MAX(meso_pblh(i,j),cpm_lim_z)
                END DO
             END DO
             
          ENDIF
          
          ! COMPUTE THE LOCAL HEIGHT ABOVE THE SURFACE AT EACH GRID POINT
          
          DO j=j_start, j_end
             DO i=i_start, i_end
                z(i,kts,j)= 1.0/rdz(i,kts,j)
                DO k=kts+1,kde-1  
                   z(i,k,j) = z(i,k-1,j) + 1.0/rdz(i,k,j)
                END DO
             END DO
          END DO
          
          k_slab_start = MIN( (slab_k - 1)*cpm_ngc_v + 1,kde-1 ) + cpm_noff_v         !LOWEST K-VALUE IN SLAB
          k_slab_end = MIN( k_slab_start + cpm_ngc_v - 1, kde-1 )                     !HIGHEST K-VALUE IN SLAB    
          
          max_pblh = MAXVAL( pblh(i_start:i_end,j_start:j_end) )        
          min_slab_z = MINVAL(z(i_start:i_end,k_slab_start,j_start:j_end))

          idum = 1
          jdum = 1
          
#ifdef DM_PARALLEL
          
          CALL wrf_dm_maxval_real(max_pblh,idum,jdum)
          
          CALL wrf_dm_minval_real(min_slab_z,idum,jdum)
          
#endif

          IF (meso_pblh_opt .EQ. 0 ) THEN !SET max_pert_z to cpm_lim_z in namelist

               max_pert_z = max_pblh

          ELSE                            !Keep the perturbations a little bit below PBLH

               max_pert_z = MAX(cpm_lim_z,max_pblh*0.90) 
               
          ENDIF    

          
           IF ( min_slab_z .LE. max_pert_z ) THEN  ! IF ANY K VALUE IN THE SLAB IS BELOW THE MAXIMUM PBL HEIGHT,
                                                   ! THEN THAT GRID POINT MAY REQUIRE A PERTURBATION
             
             h_geo = 1.20*max_pblh    ! NOW THAT WE HAVE PBLH, CALCULATE UG, VG DOMAIN AVERAGE

             ugeosum = 0.0
             vgeosum = 0.0
             uslabsum = 0.0
             vslabsum = 0.0
             num_pts_in_sum = 0.0
             
             DO j=j_start, j_end
                
                DO i=i_start, i_end

                   k_geo = kde-1     ! Max possible value if loop below does not find it (PBL deep relative to domain)

                   DO k=kts,kde-2                   
                      
                      IF ( ( z(i,k,j) .LE. h_geo ) .AND. ( z(i,k+1,j) .GT. h_geo ) )  k_geo = k
                      
                   END DO
                   
                   ugeosum = ugeosum + u(i,k_geo,j)
                   vgeosum = vgeosum + v(i,k_geo,j)
                   uslabsum = uslabsum + u(i,k_slab_end,j)
                   vslabsum = vslabsum + v(i,k_slab_end,j)
                   
                   num_pts_in_sum = num_pts_in_sum + 1.0
                   
                END DO
                
             END DO
             
#ifdef DM_PARALLEL
             
             ugeolbsum  = wrf_dm_sum_real(ugeosum)
             
             vgeolbsum  = wrf_dm_sum_real(vgeosum)
             
             uslablbsum  = wrf_dm_sum_real(uslabsum)
             
             vslablbsum  = wrf_dm_sum_real(vslabsum)
             
             counter = wrf_dm_sum_real(num_pts_in_sum)
             
#endif
             
             ugeolbavg = ugeolbsum/counter
             
             vgeolbavg = vgeolbsum/counter
             
             uslablbavg = uslablbsum/counter
             
             vslablbavg = vslablbsum/counter
             
             
             wsgeolbavg = sqrt( ugeolbavg*ugeolbavg + vgeolbavg*vgeolbavg )
             
             wsslablbavg = sqrt( uslablbavg*uslablbavg + vslablbavg*vslablbavg )
             
             
             anglelbavg = atan( abs( uslablbavg )/abs( vslablbavg ) )
             
             IF (anglelbavg .GT. pio4 ) anglelbavg = pio2 - anglelbavg 
             
             north = cpm_nb
             south = cpm_sb
             east = cpm_eb
             west = cpm_wb

             IF ( (cpm_nb + cpm_sb + cpm_eb + cpm_wb ) .EQ. 0 ) THEN ! Determine boundaries to perturb based on inflow
                IF ( vgeolbsum .LT. 0.0 ) north = 1
                IF ( vgeolbsum .GT. 0.0 ) south = 1
                IF ( ugeolbsum .LT. 0.0 ) east =  1
                IF ( ugeolbsum .GT. 0.0 ) west = 1
             END IF
             
             IF (wsslablbavg .EQ. 0.0) THEN !Prevent division by zero
                
                print*,'something wrong in calculating wind speed scaling in calc_cpm_t. Stopping.'
                STOP
                
             ENDIF

             IF (cpm_amp .EQ. 0.0) THEN
                
                tpertmag = (wsgeolbavg*wsgeolbavg)/(ek_opt*cp)

             ELSE

                tpertmag = cpm_amp
                
             ENDIF

             IF (cpm_dt .EQ. 0.0 ) THEN
             
                prtdt(slab_k) = (lambda/cos(anglelbavg))*cpm_ngc_h*cpm_ncells_h*dx/wsslablbavg

             ELSE
                 
                prtdt(slab_k) = cpm_dt

             ENDIF
              
             ! CALCULATE (NEW) PERTURBATIONS ===============================================================================
             
             ni = (ide-1+abs(cpm_nshift_ew))/cpm_ngc_h + 1 
             nj = (jde-1+abs(cpm_nshift_ns))/cpm_ngc_h + 1
             
             ! Allocate perturbation cell array for each lateral boundary 
             
             ALLOCATE( perts_wb(1:nj,1:cpm_ncells_h) )
             ALLOCATE( perts_eb(1:nj,1:cpm_ncells_h) )
             ALLOCATE( perts_sb(1:ni,1:cpm_ncells_h) )
             ALLOCATE( perts_nb(1:ni,1:cpm_ncells_h) )
             
             CALL RANDOM_SEED(size=i_seed)       ! 1. Get the size of the seed, each processor.
             
             ALLOCATE( seed(1:i_seed) )          ! 2. Allocate an array to hold the seed, each processor.
             
#ifdef DM_PARALLEL
             
             IF ( mytask .EQ. master ) THEN
                
                seedsum = 0                      ! 3a. Sum the stored seed (up to i_seed) to see if it has been assigned.
                DO k = 1,i_seed                  !     If so, now master has the current prtseed value in seed.
                   seed(k) = prtseed(k)
                   seedsum = seedsum + seed(k)
                END DO
                
                IF (seedsum .EQ. 0 ) THEN        !3b. If the seed is empty, get the first seed, which is now only on master.
                    
                   CALL RANDOM_SEED(get=seed)
                   
                ENDIF
                
             ENDIF
             
             CALL MPI_BCAST(seed,i_seed,MPI_REAL,master,MPI_COMM_WORLD,ierr) ! Broadcast seed from master to all
             
             CALL RANDOM_SEED(put=seed) !Use same seed value so all processors receive idential random numbers
             
             CALL RANDOM_NUMBER(perts_wb)
             CALL RANDOM_NUMBER(perts_eb)
             CALL RANDOM_NUMBER(perts_sb)
             CALL RANDOM_NUMBER(perts_nb)
             
             IF ( mytask .EQ. master ) THEN! Must get a new seed and store into prtseed so the sequence at the next perturbation update begins with new seed. 
                
                CALL RANDOM_SEED(get=seed)
                
                DO k = 1,i_seed
                   prtseed(k) = seed(k)
                ENDDO!
                
             ENDIF
             
#endif

             ! APPLY PERTURBATINS TO T ALONG SPECIFIED LATERAL BOUNDARIES

             IF ( west .EQ. 1 ) THEN
                
                IF (its .LE. ids + cpm_ngc_h*cpm_ncells_h) THEN 

                   DO j = MAX(jts,jds + cpm_noff_ns_s), MIN(jte,jde - 1 - cpm_noff_ns_n)

                      IF ( ( (j-1+cpm_nshift_ns)/cpm_ngc_h+1 .GT. nj )  .OR. ( (j-1+cpm_nshift_ns)/cpm_ngc_h+1 .LT. 1 ) ) THEN
                         
                         PRINT*,'Error: cpm_nshift_ns is too large or too small in module_les_cpm.F '
                         PRINT*,'range of perturbation cells = 1:',nj
                         PRINT*,'j index value = ',j
                         PRINT*,'perts_wbindex requested = ',(j-1+cpm_nshift_ns)/cpm_ngc_h+1
                         PRINT*,'Stopping'
                         STOP
                         
                      ENDIF
                      
                      DO i = its, MIN(ite, ide-1)

                         IF ( ( i .GT. cpm_noff_ew_w ) .AND. ( i .LT. ide - 1 - cpm_noff_ew_e ) )  THEN 
                         
                            m = (i-1+cpm_nshift_ew)/cpm_ngc_h+1
                            
                            IF ( m .LE. cpm_ncells_h ) THEN
                               
                               DO k = k_slab_start, k_slab_end
                                  
                                  pert_t(i,k,j) =       (perts_wb(((j-1+cpm_nshift_ns)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag !Output variable only, for debugging.
                                  
                                  t(i,k,j) = t(i,k,j) + (perts_wb(((j-1+cpm_nshift_ns)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                                  
                               END DO ! k
                               
                            ENDIF
                            
                         ENDIF
                         
                      END DO ! i
                      
                   END DO! j
                   
                ENDIF
                
             ENDIF
             
             IF ( east .EQ. 1 ) THEN
                
                IF (ite .GE. ide - 1 - cpm_ngc_h*cpm_ncells_h) THEN 

                   DO j = MAX(jts,jds + cpm_noff_ns_s), MIN(jte,jde - 1 - cpm_noff_ns_n)

                      IF ( ( (j-1+cpm_nshift_ns)/cpm_ngc_h+1 .GT. nj )  .OR. ( (j-1+cpm_nshift_ns)/cpm_ngc_h+1 .LT. 1 ) ) THEN
                         
                         PRINT*,'Error: cpm_nshift_ns is too large or too small in module_les_cpm.F '
                         PRINT*,'range of perturbation cells = 1:',nj
                         PRINT*,'j index value = ',j
                         PRINT*,'perts_eb index requested = ',(j-1+cpm_nshift_ns)/cpm_ngc_h+1
                         PRINT*,'Stopping'
                         STOP
                         
                      ENDIF
                      
                      DO i = MIN(ite, ide-1), its, -1
                         
                         m = (ide-i-1)/cpm_ngc_h+1
                         
                         IF ( m .LE. cpm_ncells_h ) THEN
                            
                            DO k = k_slab_start, k_slab_end
                               
                               pert_t(i,k,j) =       (perts_eb(((j-1+cpm_nshift_ns)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                               
                               t(i,k,j) = t(i,k,j) + (perts_eb(((j-1+cpm_nshift_ns)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                               
                            END DO ! k
                            
                         ENDIF
                         
                      END DO ! i
                      
                   END DO ! j
                   
                ENDIF
                
             ENDIF
             
             IF ( south .EQ. 1 ) THEN
                
                IF (jts .LE. jds + cpm_ngc_h*cpm_ncells_h) THEN 

                   DO i = MAX(its,ids + cpm_noff_ew_w), MIN(ite,ide - 1 - cpm_noff_ew_e)

                      IF ( ( (i-1+cpm_nshift_ew)/cpm_ngc_h+1 .GT. ni )  .OR. ( (i-1+cpm_nshift_ew)/cpm_ngc_h+1 .LT. 1 ) ) THEN
                         
                         PRINT*,'Error: cpm_nshift_ew is too large or too small in module_les_cpm.F '
                         PRINT*,'range of perturbation cells = 1:',ni
                         PRINT*,'i index value = ',i
                         PRINT*,'perts_sb index requested = ',(i-1+cpm_nshift_ew)/cpm_ngc_h+1
                         PRINT*,'Stopping'
                         STOP
                         
                      ENDIF
                      
                      DO j = jts, MIN(jte, jde-1)

                        IF ( ( j .GT. cpm_noff_ns_s ) .AND. ( j .LT. jde - 1 - cpm_noff_ns_n ) )  THEN 
                         
                         m = (j-1+cpm_nshift_ns)/cpm_ngc_h+1

                            IF ( m .LE. cpm_ncells_h ) THEN

                               print*,'i,j,m',i,j,m,(perts_sb(((i-1+cpm_nshift_ew)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                               
                               DO k = k_slab_start, k_slab_end
                                
                                  pert_t(i,k,j) =       (perts_sb(((i-1+cpm_nshift_ew)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                               
                                  t(i,k,j) = t(i,k,j) + (perts_sb(((i-1+cpm_nshift_ew)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                               
                              END DO ! k
                            
                            ENDIF

                         ENDIF
                         
                      END DO ! i
                      
                   END DO ! j
                   
                ENDIF
                
             ENDIF
             
             IF ( north .EQ. 1 ) THEN
                
                IF (jte .GE. jde - 1 - cpm_ngc_h*cpm_ncells_h) THEN 

                   DO i = MAX(its,ids + cpm_noff_ew_w), MIN(ite,ide - 1 - cpm_noff_ew_e)

                      IF ( ( (i-1+cpm_nshift_ew)/cpm_ngc_h+1 .GT. ni )  .OR. ( (i-1+cpm_nshift_ew)/cpm_ngc_h+1 .LT. 1 ) ) THEN
                         
                         PRINT*,'Error: cpm_nshift_ew is too large or too small in module_les_cpm.F '
                         PRINT*,'range of perturbation cells = 1:',ni
                         PRINT*,'i index value = ',i
                         PRINT*,'perts_nb index requested = ',(i-1+cpm_nshift_ns)/cpm_ngc_h+1
                         PRINT*,'Stopping'
                         STOP
                         
                      ENDIF
                      
                      DO j = MIN(jte, jde-1),jts,-1
                         
                         m = (jde-j-1)/cpm_ngc_h+1
                         
                         IF ( m .LE. cpm_ncells_h ) THEN
                            
                            DO k = k_slab_start, k_slab_end

                               pert_t(i,k,j) =       (perts_nb(((i-1+cpm_nshift_ew)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                               
                               t(i,k,j) = t(i,k,j) + (perts_nb(((i-1+cpm_nshift_ew)/cpm_ngc_h+1),m)-0.5)*2.0*tpertmag
                               
                            END DO ! k
                            
                         ENDIF
                         
                      END DO ! i
                      
                   END DO ! j
                   
                ENDIF
                
             ENDIF
             
             DEALLOCATE(seed)
             
             DEALLOCATE( perts_wb)
             DEALLOCATE( perts_eb )
             DEALLOCATE( perts_sb )
             DEALLOCATE( perts_nb )
             
200          CONTINUE
             
             
          END IF ! IF ( min_slab_z .GE. max_pblh ) 
          
       END IF !(  prttms(slab_k) .GE. prtdt(slab_k) ) THEN
   
    END DO !Big outer k-loop over number of vertical perturbation slabs
        
   
!=================================================================================

END SUBROUTINE calc_cpm_t

!=======================================================================



!=======================================================================
!=======================================================================
!=======================================================================

    END MODULE module_les_cpm

!=======================================================================
!=======================================================================
