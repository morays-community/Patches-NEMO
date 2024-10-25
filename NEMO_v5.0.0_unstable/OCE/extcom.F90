MODULE extcom
   !!======================================================================
   !!                       ***  MODULE  extcom  ***
   !! Demo external comm module: manage connexion with external codes 
   !!======================================================================
   !! History :  5.0.0  ! 2024-07  (A. Barge)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   naminf          : machine learning models formulation namelist
   !!   extcom_init : initialization of Machine Learning based models
   !!   ext_comm        : ML based models
   !!   inf_snd         : send data to external trained model
   !!   inf_rcv         : receive data from external trained model
   !!----------------------------------------------------------------------
   USE oce             ! ocean fields
   USE dom_oce         ! ocean domain fields
   USE extfld          ! working fields for external models
   USE cpl_oasis3      ! OASIS3 coupling
   USE timing
   USE iom
   USE in_out_manager
   USE lib_mpp

   IMPLICIT NONE
   PRIVATE

   PUBLIC extcom_alloc          ! function called in extcom_init 
   PUBLIC extcom_dealloc        ! function called in extcom_final
   PUBLIC extcom_init        ! routine called in nemogcm.F90
   PUBLIC ext_comm           ! routine called in stpmlf.F90
   PUBLIC extcom_final       ! routine called in nemogcm.F90

   INTEGER, PARAMETER ::   jps_sst = 1    ! sea surface temperature
   INTEGER, PARAMETER ::   jps_uu  = 2     ! velocity
   INTEGER, PARAMETER ::   jps_ext = 2    ! total number of sendings

   INTEGER, PARAMETER ::   jpr_sst = 1   ! modified sea surface temperature
   INTEGER, PARAMETER ::   jpr_uu  = 2   ! modified velocity
   INTEGER, PARAMETER ::   jpr_ext = 2   ! total number of receivings 

   INTEGER, PARAMETER ::   jpext = MAX(jps_ext,jpr_ext) ! Maximum number of exchanges

   TYPE( DYNARR ), SAVE, DIMENSION(jpext) ::  extsnd, extrcv  ! sent/received fields

   !!-------------------------------------------------------------------------
   !!                    Namelist for the Inference Models
   !!-------------------------------------------------------------------------
   LOGICAL , PUBLIC ::   ln_ext    !: activate module for inference models
   !!-------------------------------------------------------------------------

   !! Substitution
#  include "read_nml_substitute.h90"
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION extcom_alloc()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION extcom_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      INTEGER :: jn
      !!----------------------------------------------------------------------
      ierr = 0
      !
      DO jn = 1, jpext
         IF( srcv(nmodext)%fld(jn)%laction ) ALLOCATE( extrcv(jn)%z3(jpi,jpj,srcv(nmodext)%fld(jn)%nlvl), STAT=ierr )
         IF( ssnd(nmodext)%fld(jn)%laction ) ALLOCATE( extsnd(jn)%z3(jpi,jpj,ssnd(nmodext)%fld(jn)%nlvl), STAT=ierr )
         extcom_alloc = MAX(ierr,0)
      END DO
      !
   END FUNCTION extcom_alloc

   
   INTEGER FUNCTION extcom_dealloc()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION extcom_dealloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      INTEGER :: jn
      !!----------------------------------------------------------------------
      ierr = 0
      !
      DO jn = 1, jpext
         IF( srcv(nmodext)%fld(jn)%laction ) DEALLOCATE( extrcv(jn)%z3, STAT=ierr )
         IF( ssnd(nmodext)%fld(jn)%laction ) DEALLOCATE( extsnd(jn)%z3, STAT=ierr )
         extcom_dealloc = MAX(ierr,0)
      END DO
      !
   END FUNCTION extcom_dealloc


   SUBROUTINE extcom_init 
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE extcom_init  ***
      !!
      !! ** Purpose :   Initialisation of the models that rely on external models
      !!
      !! ** Method  :   * Read naminf namelist
      !!                * create data for models
      !!----------------------------------------------------------------------
      !
      INTEGER ::   ios   ! Local Integer
      !!
      NAMELIST/namext/  ln_ext
      !!----------------------------------------------------------------------
      !
      ! ================================ !
      !      Namelist informations       !
      ! ================================ !
      !
      READ_NML_REF(numnam,namext)
      READ_NML_CFG(numnam,namext)
      IF( lwm ) WRITE( numond, namext )
      !
      IF( lwp ) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*)'extcom_init : Setting external model'
         WRITE(numout,*)'~~~~~~~~~~~'
      END IF
      IF ( lwp .AND. ln_ext ) THEN
         WRITE(numout,*)'   Namelist namext'
         WRITE(numout,*)'      Module used       ln_ext        = ', ln_ext
      ENDIF
      !
      IF( ln_ext .AND. .NOT. lk_oasis )   CALL ctl_stop( 'extcom_init : External models coupled via OASIS, but key_oasis3 disabled' )
      !
      !
      ! ======================================== !
      !     Define exchange needs for Models     !
      ! ======================================== !
      !
      ALLOCATE( srcv(nmodext)%fld(jpr_ext) )
      !
      ! default definitions of ssnd snd srcv
      srcv(nmodext)%fld(:)%laction = .FALSE.  ;  srcv(nmodext)%fld(:)%clgrid = 'T'  ;  srcv(nmodext)%fld(:)%nsgn = 1.
      srcv(nmodext)%fld(:)%nct = 1  ;  srcv(nmodext)%fld(:)%nlvl = 1
      !
      ALLOCATE( ssnd(nmodext)%fld(jps_ext) )
      !
      ssnd(nmodext)%fld(:)%laction = .FALSE.  ;  ssnd(nmodext)%fld(:)%clgrid = 'T'  ;  ssnd(nmodext)%fld(:)%nsgn = 1.
      ssnd(nmodext)%fld(:)%nct = 1  ;  ssnd(nmodext)%fld(:)%nlvl = 1
      
      IF( ln_ext ) THEN
      
         ! -------------------------------- !
         !      Kenigson et al. (2022)      !
         ! -------------------------------- !

         ! sending of sea surface temparature
         ssnd(nmodext)%fld(jps_sst)%clname = 'E_OUT_0'
         ssnd(nmodext)%fld(jps_sst)%laction = .TRUE.

         ! sending of 3 first levels of velocity
         ssnd(nmodext)%fld(jps_uu)%clname = 'E_OUT_1'
         ssnd(nmodext)%fld(jps_uu)%laction = .TRUE.
         ssnd(nmodext)%fld(jps_uu)%clgrid = 'U'
         ssnd(nmodext)%fld(jps_uu)%nlvl = 3

         ! reception of modified sea surface temperature
         srcv(nmodext)%fld(jpr_sst)%clname = 'E_IN_0'
         srcv(nmodext)%fld(jpr_sst)%laction = .TRUE.

         ! reception of 3 first levels of modified velocity
         srcv(nmodext)%fld(jpr_uu)%clname = 'E_IN_1'
         srcv(nmodext)%fld(jpr_uu)%laction = .TRUE.
         srcv(nmodext)%fld(jpr_uu)%clgrid = 'U'
         srcv(nmodext)%fld(jpr_uu)%nlvl = 3

         ! ------------------------------ !
         ! ------------------------------ !

      END IF
      ! 
      ! ================================= !
      !   Define variables for coupling
      ! ================================= !
      CALL cpl_var(jpr_ext, jps_ext, 1, nmodext)
      !
      IF( extcom_alloc() /= 0 )  CALL ctl_stop( 'STOP', 'extcom_alloc : unable to allocate arrays' )
      IF( extfld_alloc() /= 0 )  CALL ctl_stop( 'STOP', 'extfld_alloc : unable to allocate arrays' ) 
      !
   END SUBROUTINE extcom_init


   SUBROUTINE ext_comm( kt, Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE ext_comm  ***
      !!
      !! ** Purpose :   update the ocean data with the coupled models
      !!
      !! ** Method  :   *  
      !!                * 
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa ! ocean time level indices
      !
      INTEGER :: isec, info, jn                       ! local integer
      REAL(wp), DIMENSION(jpi,jpj,jpk)   ::  zdata    ! sending buffer
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('ext_comm')
      !
      isec = ( kt - nit000 ) * NINT( rn_Dt )       ! Date of exchange 
      info = OASIS_idle
      !
      ! ------  Prepare data to send ------
      !
      ! Sea Surface Temperature
      IF( ssnd(nmodext)%fld(jps_sst)%laction ) THEN
         extsnd(jps_sst)%z3(:,:,1:ssnd(nmodext)%fld(jps_sst)%nlvl) = ts(:,:,1:ssnd(nmodext)%fld(jps_sst)%nlvl,jp_tem,Kmm)
      ENDIF  
      !
      ! 3 first levels of sea velocity
      IF( ssnd(nmodext)%fld(jps_uu)%laction ) THEN
         extsnd(jps_uu)%z3(:,:,1:ssnd(nmodext)%fld(jps_uu)%nlvl) = uu(:,:,1:ssnd(nmodext)%fld(jps_uu)%nlvl,Kbb)
      ENDIF
      !
      ! ========================
      !   Proceed all sendings
      ! ========================
      !
      DO jn = 1, jps_ext
         IF ( ssnd(nmodext)%fld(jn)%laction ) THEN
            CALL cpl_snd( nmodext, jn, isec, extsnd(jn)%z3(A2D(0),:), info)
         ENDIF
      END DO
      !
      ! .... some external operations ....
      !
      ! ==========================
      !   Proceed all receptions
      ! ==========================
      !
      DO jn = 1, jpr_ext
         IF( srcv(nmodext)%fld(jn)%laction ) THEN
            CALL cpl_rcv( nmodext, jn, isec, extrcv(jn)%z3(A2D(0),:), info)
         ENDIF
      END DO
      !
      ! ------ Distribute receptions  ------
      !
      ! Modified sea Surface Temperature
      IF( srcv(nmodext)%fld(jpr_sst)%laction ) THEN
         ext_ts_2D(:,:) = extrcv(jpr_sst)%z3(:,:,1)
         CALL iom_put( 'Ext_SST', ext_ts_2D(:,:) )
      ENDIF
      !
      ! Modified 3 first levels of velocity
      IF( srcv(nmodext)%fld(jpr_uu)%laction ) THEN
         ext_u_3D(:,:,1:srcv(nmodext)%fld(jpr_uu)%nlvl) = extrcv(jpr_uu)%z3(:,:,1:srcv(nmodext)%fld(jpr_uu)%nlvl)
         CALL iom_put( 'Ext_uu', ext_u_3D(:,:,:) )
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('ext_comm')
      !
   END SUBROUTINE ext_comm


   SUBROUTINE extcom_final
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE extcom_final  ***
      !!
      !! ** Purpose :   Free memory used for extcom modules
      !!
      !! ** Method  :   * Deallocate arrays
      !!----------------------------------------------------------------------
      !
      IF( extcom_dealloc() /= 0 )     CALL ctl_stop( 'STOP', 'extcom_dealloc : unable to free memory' )
      IF( extfld_dealloc() /= 0 )  CALL ctl_stop( 'STOP', 'extfld_dealloc : unable to free memory' )      
      !
   END SUBROUTINE extcom_final 
   !!=======================================================================
END MODULE extcom
