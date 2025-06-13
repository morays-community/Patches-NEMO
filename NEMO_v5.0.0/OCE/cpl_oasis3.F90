MODULE cpl_oasis3
   !!======================================================================
   !!                    ***  MODULE cpl_oasis  ***
   !! Coupled O/A : coupled ocean-atmosphere case using OASIS3-MCT
   !!=====================================================================
   !! History :  1.0  !  2004-06  (R. Redler, NEC Laboratories Europe, Germany) Original code
   !!             -   !  2004-11  (R. Redler, NEC Laboratories Europe; N. Keenlyside, W. Park, IFM-GEOMAR, Germany) revision
   !!             -   !  2004-11  (V. Gayler, MPI M&D) Grid writing
   !!            2.0  !  2005-08  (R. Redler, W. Park) frld initialization, paral(2) revision
   !!             -   !  2005-09  (R. Redler) extended to allow for communication over root only
   !!             -   !  2006-01  (W. Park) modification of physical part
   !!             -   !  2006-02  (R. Redler, W. Park) buffer array fix for root exchange
   !!            3.4  !  2011-11  (C. Harris) Changes to allow mutiple category fields
   !!            3.6  !  2014-11  (S. Masson) OASIS3-MCT
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   'key_oasis3'                    coupled Ocean/Atmosphere via OASIS3-MCT
   !!----------------------------------------------------------------------
   !!   cpl_init     : initialization of coupled mode communication
   !!   cpl_domain   : definition of grid
   !!   cpl_var      : definition of fields
   !!   cpl_define   : finalize definition
   !!   cpl_snd      : snd out fields in coupled mode
   !!   cpl_rcv      : receive fields in coupled mode
   !!   cpl_finalize : finalize the coupled mode communication
   !!----------------------------------------------------------------------
#if defined key_oasis3
   USE mod_oasis                    ! OASIS3-MCT module
#endif
#if defined key_xios
   USE xios                         ! I/O server
#endif
   USE par_oce                      ! ocean parameters
   USE dom_oce                      ! ocean space and time domain
   USE in_out_manager               ! I/O manager
   USE lbclnk                       ! ocean lateral boundary conditions (or mpp link)

   IMPLICIT NONE
   PRIVATE

   PUBLIC   cpl_init
   PUBLIC   cpl_domain
   PUBLIC   cpl_var
   PUBLIC   cpl_define
   PUBLIC   cpl_snd
   PUBLIC   cpl_rcv
   PUBLIC   cpl_freq
   PUBLIC   cpl_finalize

#if ! defined key_oasis3
   INTERFACE oasis_get
      MODULE PROCEDURE oasis_get_2D, oasis_get_3D
   END INTERFACE

   INTERFACE oasis_put
      MODULE PROCEDURE oasis_put_2D, oasis_put_3D
   END INTERFACE
#endif

   INTEGER, PUBLIC            ::   OASIS_Rcv  = 1    !: return code if received field
   INTEGER, PUBLIC            ::   OASIS_idle = 0    !: return code if nothing done by oasis
   INTEGER                    ::   ncomp_id          ! id returned by oasis_init_comp
   INTEGER                    ::   nid_part          ! id returned by oasis_def_partition
   INTEGER                    ::   nishape(4)        ! shape of arrays passed to PSMILe
   INTEGER                    ::   nerror            ! return error code
#if ! defined key_oasis3
   ! OASIS Variables not used. defined only for compilation purpose
   INTEGER                    ::   OASIS_Out         = -1
   INTEGER                    ::   OASIS_REAL        = -1
   INTEGER                    ::   OASIS_Ok          = -1
   INTEGER                    ::   OASIS_In          = -1
   INTEGER                    ::   OASIS_Sent        = -1
   INTEGER                    ::   OASIS_SentOut     = -1
   INTEGER                    ::   OASIS_ToRest      = -1
   INTEGER                    ::   OASIS_ToRestOut   = -1
   INTEGER                    ::   OASIS_Output      = -1
   INTEGER                    ::   OASIS_Recvd       = -1
   INTEGER                    ::   OASIS_RecvOut     = -1
   INTEGER                    ::   OASIS_FromRest    = -1
   INTEGER                    ::   OASIS_FromRestOut = -1
   INTEGER                    ::   OASIS_Input       = -1

   LOGICAL, PUBLIC            ::   lk_oasis = .FALSE. !: OASIS not used
#else
   LOGICAL, PUBLIC            ::   lk_oasis = .TRUE.  !: OASIS used
#endif

   INTEGER                    ::   ncplmodel    ! Maximum number of models to/from which NEMO is potentialy sending/receiving data
   INTEGER, PUBLIC, PARAMETER ::   nmaxcat=5    ! Maximum number of coupling fields
   INTEGER, PUBLIC, PARAMETER ::   nmaxcpl=5    ! Maximum number of coupling fields
   INTEGER, PUBLIC, PARAMETER ::   nmodmax=2    ! Maximum number of identified modules   
   INTEGER, PUBLIC, PARAMETER ::   nmodsbc=1    ! module ID #1 : surface boundary condition
   INTEGER, PUBLIC, PARAMETER ::   nmodext=2    ! module ID #2 : demo external communication module

   TYPE, PUBLIC ::   FLD_CPL               !: Type for coupling field information
      LOGICAL               ::   laction   ! To be coupled or not
      CHARACTER(len = 8)    ::   clname    ! Name of the coupling field
      CHARACTER(len = 1)    ::   clgrid    ! Grid type
      REAL(wp)              ::   nsgn      ! Control of the sign change
      INTEGER, DIMENSION(nmaxcat,nmaxcpl) ::   nid   ! Id of the field (no more than 9 categories and 9 extrena models)
      INTEGER               ::   nct       ! Number of categories in field
      INTEGER               ::   ncplmodel ! Maximum number of models to/from which this variable may be sent/received
      INTEGER               ::   nlvl      ! number of depth levels (size of third dimension for 3D coupling)
   END TYPE FLD_CPL

   TYPE, PUBLIC :: MOD_FLD_CPL             !: Type to sort coupling field information between calling modules
      TYPE(FLD_CPL), DIMENSION(:), ALLOCATABLE,  PUBLIC :: fld
   END TYPE MOD_FLD_CPL

   TYPE(MOD_FLD_CPL), DIMENSION(:), ALLOCATABLE, PUBLIC ::  srcv, ssnd   !: Coupling field informations

   TYPE, PUBLIC ::   DYNARR                           !: Type to create batches of coupled fields
      REAL(wp), POINTER, DIMENSION(:,:,:) ::   z3 
   END TYPE DYNARR

   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   exfld   ! Temporary buffer for receiving

   !!----------------------------------------------------------------------
   !! NEMO/OCE 5.0, NEMO Consortium (2024)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE cpl_init( cd_modname, kl_comm )
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_init  ***
      !!
      !! ** Purpose :   Initialize coupled mode communication for ocean
      !!    exchange between AGCM, OGCM and COUPLER. (OASIS3 software)
      !!
      !! ** Method  :   OASIS3 MPI communication
      !!--------------------------------------------------------------------
      CHARACTER(len = *), INTENT(in   ) ::   cd_modname   ! model name as set in namcouple file
      INTEGER           , INTENT(  out) ::   kl_comm      ! local communicator of the model
      !!--------------------------------------------------------------------

      ! WARNING: No write in numout in this routine
      !============================================

      !------------------------------------------------------------------
      ! 1st Initialize the OASIS system for the application
      !------------------------------------------------------------------
      CALL oasis_init_comp ( ncomp_id, TRIM(cd_modname), nerror )
      IF( nerror /= OASIS_Ok ) &
         CALL oasis_abort (ncomp_id, 'cpl_init', 'Failure in oasis_init_comp')

      !------------------------------------------------------------------
      ! 2nd Get an MPI communicator for OCE local communication
      !------------------------------------------------------------------

      CALL oasis_get_localcomm ( kl_comm, nerror )
      IF( nerror /= OASIS_Ok ) &
         CALL oasis_abort (ncomp_id, 'cpl_init','Failure in oasis_get_localcomm' )

      !------------------------------------------------------------------
      ! 3rd Initialize coupling information arrays
      !------------------------------------------------------------------
      ALLOCATE( srcv(nmodmax) , ssnd(nmodmax) )
      !
   END SUBROUTINE cpl_init


   SUBROUTINE cpl_domain
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_domain  ***
      !!
      !! ** Purpose :   Define grid information for ocean
      !!    exchange between AGCM, OGCM and COUPLER. (OASIS3 software)
      !!
      !! ** Method  :   OASIS3 MPI communication
      !!--------------------------------------------------------------------
      INTEGER :: paral(5)       ! OASIS3 box partition
      !!--------------------------------------------------------------------
      !
      ! ... Define the shape for the area that excludes the halo as we don't want them to be "seen" by oasis
      !
      nishape(1) = 1
      nishape(2) = Ni_0
      nishape(3) = 1
      nishape(4) = Nj_0
      !
      ! ... Allocate memory for data exchange
      !
      ALLOCATE(exfld(Ni_0, Nj_0, jpk), stat = nerror)        ! allocate only inner domain (without halos)
      IF( nerror > 0 ) THEN
         CALL oasis_abort ( ncomp_id, 'cpl_domain', 'Failure in allocating exfld')   ;   RETURN
      ENDIF

      !
      ! -----------------------------------------------------------------
      ! ... Define the partition, excluding halos as we don't want them to be "seen" by oasis
      ! -----------------------------------------------------------------

      paral(1) = 2                                          ! box partitioning
      paral(2) = Ni0glo * (mjg(Njs0,0)-1) + mig(Nis0,0)-1   ! NEMO lower left corner global offset, without halos
      paral(3) = Ni_0                                       ! local extent in i, excluding halos
      paral(4) = Nj_0                                       ! local extent in j, excluding halos
      paral(5) = Ni0glo                                     ! global extent in x, excluding halos

      IF( sn_cfctl%l_oasout ) THEN
         WRITE(numout,*) ' multiexchg: paral (1:5)', paral
         WRITE(numout,*) ' multiexchg: Ni_0, Nj_0 =', Ni_0, Nj_0
         WRITE(numout,*) ' multiexchg: Nis0, Nie0, nimpp =', Nis0, Nie0, nimpp
         WRITE(numout,*) ' multiexchg: Njs0, Nje0, njmpp =', Njs0, Nje0, njmpp
      ENDIF
      !
      CALL oasis_def_partition ( nid_part, paral, nerror, Ni0glo*Nj0glo )   ! global number of points, excluding halos
      !
   END SUBROUTINE cpl_domain


   SUBROUTINE cpl_var( krcv, ksnd, kcplmodel, kmod )
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_var  ***
      !!
      !! ** Purpose :   Define field information for ocean
      !!    exchange between AGCM, OGCM and COUPLER. (OASIS3 software)
      !!
      !! ** Method  :   OASIS3 MPI communication
      !!--------------------------------------------------------------------
      INTEGER, INTENT(in) ::   krcv, ksnd     ! Number of received and sent coupling fields
      INTEGER, INTENT(in) ::   kcplmodel      ! Maximum number of models to/from which NEMO is potentialy sending/receiving data
      INTEGER, INTENT(in) ::   kmod           ! calling module ID
      !
      INTEGER :: ji,jc,jm       ! local loop indicees
      INTEGER :: isnd, ircv
      CHARACTER(LEN=64) :: zclname
      CHARACTER(LEN=2) :: cli2
      !!--------------------------------------------------------------------

      IF(lwp) THEN
         WRITE(numout,*)
         IF( kmod == nmodsbc ) WRITE(numout,*) 'cpl_var : initialization in coupled ocean/atmosphere case'
         IF( kmod == nmodext ) WRITE(numout,*) 'cpl_var : initialization in external communication module'
         WRITE(numout,*) '~~~~~~~'
      END IF

      ncplmodel = kcplmodel
      IF( kcplmodel > nmaxcpl ) THEN
         CALL oasis_abort ( ncomp_id, 'cpl_var', 'ncplmodel is larger than nmaxcpl, increase nmaxcpl')   ;   RETURN
      ENDIF
      !
      ! ... Announce send variables.
      !
      isnd = 0
      ssnd(kmod)%fld(:)%ncplmodel = kcplmodel
      !
      DO ji = 1, ksnd
         IF( ssnd(kmod)%fld(ji)%laction ) THEN

            IF( ssnd(kmod)%fld(ji)%nct > nmaxcat ) THEN
               CALL oasis_abort ( ncomp_id, 'cpl_var', 'Number of categories of '//   &
                  &               TRIM(ssnd(kmod)%fld(ji)%clname)//' is larger than nmaxcat, increase nmaxcat' )
               RETURN
            ENDIF

            DO jc = 1, ssnd(kmod)%fld(ji)%nct
               DO jm = 1, kcplmodel

                  IF( ssnd(kmod)%fld(ji)%nct .GT. 1 ) THEN
                     WRITE(cli2,'(i2.2)') jc
                     zclname = TRIM(ssnd(kmod)%fld(ji)%clname)//'_cat'//cli2
                  ELSE
                     zclname = ssnd(kmod)%fld(ji)%clname
                  ENDIF
                  IF( kcplmodel  > 1 ) THEN
                     WRITE(cli2,'(i2.2)') jm
                     zclname = 'model'//cli2//'_'//TRIM(zclname)
                  ENDIF
#if defined key_agrif
                  IF( agrif_fixed() /= 0 ) THEN
                     zclname=TRIM(Agrif_CFixed())//'_'//TRIM(zclname)
                  ENDIF
#endif
                  IF( sn_cfctl%l_oasout ) WRITE(numout,*) "Define", ji, jc, jm, " "//TRIM(zclname), " for ", OASIS_Out
                  CALL oasis_def_var (ssnd(kmod)%fld(ji)%nid(jc,jm), zclname, nid_part   , (/ 2, ssnd(kmod)%fld(ji)%nlvl /),   &
                     &                OASIS_Out          , nishape , OASIS_REAL, nerror )
                  IF( nerror /= OASIS_Ok ) THEN
                     WRITE(numout,*) 'Failed to define transient ', ji, jc, jm, " "//TRIM(zclname)
                     CALL oasis_abort ( ssnd(kmod)%fld(ji)%nid(jc,jm), 'cpl_var', 'Failure in oasis_def_var' )
                  ENDIF
                  IF( ssnd(kmod)%fld(ji)%nid(jc,jm) /= -1 ) THEN
                     IF( sn_cfctl%l_oasout ) WRITE(numout,*) "Variable defined in the namcouple"
                     isnd = isnd + 1
                  ELSEIF( ssnd(kmod)%fld(ji)%nid(jc,jm) == -1 ) THEN
                     IF(lwp) WRITE(numout,*) "   "//TRIM(ssnd(kmod)%fld(ji)%clname)//" NOT defined in the namcouple, coupling disabled"
                     ssnd(kmod)%fld(ji)%laction = .FALSE. 
                  ENDIF
               END DO
            END DO
         ENDIF
      END DO
      !
      ! ... Announce received variables.
      !
      ircv = 0
      srcv(kmod)%fld(:)%ncplmodel = kcplmodel
      !
      DO ji = 1, krcv
         IF( srcv(kmod)%fld(ji)%laction ) THEN

            IF( srcv(kmod)%fld(ji)%nct > nmaxcat ) THEN
               CALL oasis_abort ( ncomp_id, 'cpl_var', 'Number of categories of '//   &
                  &               TRIM(srcv(kmod)%fld(ji)%clname)//' is larger than nmaxcat, increase nmaxcat' )
               RETURN
            ENDIF

            DO jc = 1, srcv(kmod)%fld(ji)%nct
               DO jm = 1, kcplmodel

                  IF( srcv(kmod)%fld(ji)%nct .GT. 1 ) THEN
                     WRITE(cli2,'(i2.2)') jc
                     zclname = TRIM(srcv(kmod)%fld(ji)%clname)//'_cat'//cli2
                  ELSE
                     zclname = srcv(kmod)%fld(ji)%clname
                  ENDIF
                  IF( kcplmodel  > 1 ) THEN
                     WRITE(cli2,'(i2.2)') jm
                     zclname = 'model'//cli2//'_'//TRIM(zclname)
                  ENDIF
#if defined key_agrif
                  IF( agrif_fixed() /= 0 ) THEN
                     zclname=TRIM(Agrif_CFixed())//'_'//TRIM(zclname)
                  ENDIF
#endif
                  IF( sn_cfctl%l_oasout ) WRITE(numout,*) "Define", ji, jc, jm, " "//TRIM(zclname), " for ", OASIS_In
                  CALL oasis_def_var (srcv(kmod)%fld(ji)%nid(jc,jm), zclname, nid_part   , (/ 2, srcv(kmod)%fld(ji)%nlvl /),   &
                     &                OASIS_In           , nishape , OASIS_REAL, nerror )
                  IF( nerror /= OASIS_Ok ) THEN
                     WRITE(numout,*) 'Failed to define transient ', ji, jc, jm, " "//TRIM(zclname)
                     CALL oasis_abort ( srcv(kmod)%fld(ji)%nid(jc,jm), 'cpl_var', 'Failure in oasis_def_var' )
                  ENDIF
                  IF( srcv(kmod)%fld(ji)%nid(jc,jm) /= -1 ) THEN
                     IF( sn_cfctl%l_oasout ) WRITE(numout,*) "Variable defined in the namcouple"
                     ircv = ircv + 1
                  ELSEIF( srcv(kmod)%fld(ji)%nid(jc,jm) == -1 ) THEN 
                     IF(lwp) WRITE(numout,*) "   "//TRIM(srcv(kmod)%fld(ji)%clname)//" NOT defined in the namcouple, coupling disabled"
                     srcv(kmod)%fld(ji)%laction = .FALSE.
                  ENDIF                  
               END DO
            END DO
         ENDIF
      END DO
      !
      IF(lwp) THEN
         WRITE(numout,*) '   Number of variables to receive: ', ircv
         WRITE(numout,*) '   Number of variables to send: ', isnd
      END IF  
      !      
   END SUBROUTINE cpl_var


   SUBROUTINE cpl_define
      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_define  ***
      !!
      !! ** Purpose :   Finalize definition for ocean
      !!    exchange between AGCM, OGCM and COUPLER. (OASIS3 software)
      !!
      !! ** Method  :   OASIS3 MPI communication
      !!--------------------------------------------------------------------
      LOGICAL :: llenddef       ! should we call xios_oasis_enddef and oasis_enddef?
      !!--------------------------------------------------------------------           
      !
      ! Finish oasis definition phase
      !
#if defined key_agrif
      IF( Agrif_Root() ) THEN   ! Warning: Agrif_Nb_Fine_Grids not yet defined -> must use Agrif_Root_Only()
         llenddef = Agrif_Root_Only()   ! true of no nested grid
      ELSE                      ! Is it the last nested grid?
         llenddef = agrif_fixed() == Agrif_Nb_Fine_Grids()
      ENDIF
#else
      llenddef = .TRUE.
#endif
      IF( llenddef ) THEN
#if defined key_xios
         CALL xios_oasis_enddef()   ! see "Joint_usage_OASIS3-MCT_XIOS.pdf" on XIOS wiki page
#endif
         CALL oasis_enddef(nerror)
         IF( nerror /= OASIS_Ok )   CALL oasis_abort ( ncomp_id, 'cpl_define', 'Failure in oasis_enddef')
      ENDIF
      !
   END SUBROUTINE cpl_define


   SUBROUTINE cpl_snd( kmod, kid, kstep, pdata, kinfo )
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_snd  ***
      !!
      !! ** Purpose : - At each coupling time-step,this routine sends fields
      !!      like sst or ice cover to the coupler or remote application.
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in   ) ::   kmod      ! calling module ID
      INTEGER                   , INTENT(in   ) ::   kid       ! variable index in the array
      INTEGER                   , INTENT(  out) ::   kinfo     ! OASIS3 info argument
      INTEGER                   , INTENT(in   ) ::   kstep     ! ocean time-step in seconds
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::   pdata
      !!
      INTEGER                                   ::   jc,jm     ! local loop index
      LOGICAL                                   ::   ll3D      ! flag for 3D coupling
      !!--------------------------------------------------------------------
      !
      ! Default values
      ll3D = .FALSE.
      IF( ssnd(kmod)%fld(kid)%nlvl > 1 ) ll3D = .TRUE.

      ! check size
      IF( COUNT( SHAPE(pdata(:,:,1)) /= (/Ni_0,Nj_0/) ) > 0 )   &
         CALL oasis_abort( ncomp_id, 'cpl_snd', TRIM(ssnd(kmod)%fld(kid)%clname)//' array must be given without halo' )

      !
      ! snd data to OASIS3
      !
      DO jc = 1, ssnd(kmod)%fld(kid)%nct
         DO jm = 1, ssnd(kmod)%fld(kid)%ncplmodel

            IF( ssnd(kmod)%fld(kid)%nid(jc,jm) /= -1 ) THEN   ! exclude halos from data sent to oasis

               IF( .NOT. ll3D ) THEN   ! send 2D or 3D fields
                  CALL oasis_put ( ssnd(kmod)%fld(kid)%nid(jc,jm), kstep, pdata(1:Ni_0, 1:Nj_0,jc), kinfo )
               ELSE 
                  CALL oasis_put ( ssnd(kmod)%fld(kid)%nid(jc,jm), kstep, pdata(1:Ni_0, 1:Nj_0,1:ssnd(kmod)%fld(kid)%nlvl), kinfo )
               ENDIF

               IF ( sn_cfctl%l_oasout ) THEN
                  IF ( kinfo == OASIS_Sent     .OR. kinfo == OASIS_ToRest .OR.   &
                     & kinfo == OASIS_SentOut  .OR. kinfo == OASIS_ToRestOut .OR. kinfo == OASIS_Output ) THEN
                     WRITE(numout,*) '****************'
                     WRITE(numout,*) 'oasis_put: Outgoing ', ssnd(kmod)%fld(kid)%clname
                     WRITE(numout,*) 'oasis_put: ivarid ', ssnd(kmod)%fld(kid)%nid(jc,jm)
                     WRITE(numout,*) 'oasis_put:  kstep ', kstep
                     WRITE(numout,*) 'oasis_put:   info ', kinfo
                     WRITE(numout,*) '     - Minimum value is ', MINVAL(pdata(1:Ni_0,1:Nj_0,jc))
                     WRITE(numout,*) '     - Maximum value is ', MAXVAL(pdata(1:Ni_0,1:Nj_0,jc))
                     WRITE(numout,*) '     -     Sum value is ',    SUM(pdata(1:Ni_0,1:Nj_0,jc))
                     WRITE(numout,*) '****************'
                  ENDIF
               ENDIF

            ENDIF

         ENDDO
      ENDDO
      !
    END SUBROUTINE cpl_snd


   SUBROUTINE cpl_rcv( kmod, kid, kstep, pdata, kinfo, pmask )
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_rcv  ***
      !!
      !! ** Purpose : - At each coupling time-step,this routine receives fields
      !!      like stresses and fluxes from the coupler or remote application.
      !!----------------------------------------------------------------------
      INTEGER                   , INTENT(in   )           ::   kmod      ! calling module ID
      INTEGER                   , INTENT(in   )           ::   kid       ! variable index in the array
      INTEGER                   , INTENT(in   )           ::   kstep     ! ocean time-step in seconds
      INTEGER                   , INTENT(  out)           ::   kinfo     ! OASIS3 info argument
      REAL(wp), DIMENSION(:,:,:), INTENT(inout)           ::   pdata     ! IN to keep the value if nothing is done
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ), OPTIONAL ::   pmask     ! coupling mask
      !!
      INTEGER                                             ::   jc,jm     ! local loop index
      INTEGER                                             ::   ib, iu(2) ! depth level indexes
      LOGICAL                                             ::   llaction, ll_1st, ll_mask, ll3D
      !!--------------------------------------------------------------------
      !
      ! Default values
      ll_mask = .FALSE.
      IF( PRESENT( pmask ) ) ll_mask = .TRUE.
      !
      ll3D = .FALSE.
      IF( srcv(kmod)%fld(kid)%nlvl > 1 ) ll3D = .TRUE.
      !
      ! receive local data from OASIS3 on every process
      !
      kinfo = OASIS_idle
      !
      DO jc = 1, srcv(kmod)%fld(kid)%nct
         ll_1st = .TRUE.

         DO jm = 1, srcv(kmod)%fld(kid)%ncplmodel

            IF( srcv(kmod)%fld(kid)%nid(jc,jm) /= -1 ) THEN

               IF( .NOT. ll3D ) THEN   ! Receive 2D or 3D field
                  CALL oasis_get ( srcv(kmod)%fld(kid)%nid(jc,jm), kstep, exfld(:,:,1), kinfo )
                  ib = jc
                  iu(1) = 1
                  iu(2) = jc
               ELSE
                  CALL oasis_get ( srcv(kmod)%fld(kid)%nid(jc,jm), kstep, exfld(:,:,1:srcv(kmod)%fld(kid)%nlvl), kinfo )
                  ib = 1
                  iu(:) = srcv(kmod)%fld(kid)%nlvl
                ENDIF

               llaction =  kinfo == OASIS_Recvd   .OR. kinfo == OASIS_FromRest .OR.   &
                  &        kinfo == OASIS_RecvOut .OR. kinfo == OASIS_FromRestOut .OR. kinfo == OASIS_Input

               IF ( sn_cfctl%l_oasout )   &
                  &  WRITE(numout,*) "llaction, kinfo, kstep, ivarid: " , llaction, kinfo, kstep, srcv(kmod)%fld(kid)%nid(jc,jm)

               IF( llaction ) THEN   ! data received from oasis do not include halos

                  kinfo = OASIS_Rcv
                  IF( ll_mask ) exfld(:,:,1:iu(1)) = exfld(:,:,1:iu(1)) * pmask(1:Ni_0,1:Nj_0,jm:jm)

                  IF( ll_1st ) THEN
                     pdata(1:Ni_0,1:Nj_0,ib:iu(2)) = exfld(:,:,1:iu(1))
                     ll_1st = .FALSE.
                  ELSE
                     pdata(1:Ni_0,1:Nj_0,ib:iu(2)) = pdata(1:Ni_0,1:Nj_0,ib:iu(2)) + exfld(:,:,1:iu(1))
                  ENDIF

                  IF ( sn_cfctl%l_oasout ) THEN
                     WRITE(numout,*) '****************'
                     WRITE(numout,*) 'oasis_get: Incoming ', srcv(kmod)%fld(kid)%clname
                     WRITE(numout,*) 'oasis_get: ivarid '  , srcv(kmod)%fld(kid)%nid(jc,jm)
                     WRITE(numout,*) 'oasis_get:   kstep', kstep
                     WRITE(numout,*) 'oasis_get:   info ', kinfo
                     WRITE(numout,*) '     - Minimum value is ', MINVAL(pdata(1:Ni_0,1:Nj_0,jc))
                     WRITE(numout,*) '     - Maximum value is ', MAXVAL(pdata(1:Ni_0,1:Nj_0,jc))
                     WRITE(numout,*) '     -     Sum value is ',    SUM(pdata(1:Ni_0,1:Nj_0,jc))
                     WRITE(numout,*) '****************'
                  ENDIF

               ENDIF

            ENDIF

         ENDDO

         !--- WARNING: here we have no halos
         !    BUT in some cases of the North Fold, we need to update the last line or half of the last line of the inner domain.
         !    We need to call lbc_lnk (with ldfull = .TRUE.) that is working without halos
         IF( .NOT. ll_1st ) THEN
            CALL lbc_lnk( 'cpl_oasis3', pdata(:,:,ib:iu(2)), srcv(kmod)%fld(kid)%clgrid, srcv(kmod)%fld(kid)%nsgn, ldfull = .TRUE. )
         ENDIF
         !!clem: mettre T instead of clgrid

      ENDDO
      !
   END SUBROUTINE cpl_rcv


   INTEGER FUNCTION cpl_freq( cdfieldname , kmod )
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_freq  ***
      !!
      !! ** Purpose : - send back the coupling frequency for a particular field
      !!----------------------------------------------------------------------
      CHARACTER(len = *), INTENT(in) ::   cdfieldname    ! field name as set in namcouple file
      INTEGER           , INTENT(in) ::   kmod           ! calling module ID
      !!
      INTEGER               :: id
      INTEGER               :: info
      INTEGER, DIMENSION(1) :: itmp
      INTEGER               :: ji,jm     ! local loop index
      INTEGER               :: mop
      !!----------------------------------------------------------------------
      cpl_freq = 0   ! defaut definition
      id = -1        ! defaut definition
      !
      DO ji = 1, SIZE(ssnd)
         IF(ssnd(kmod)%fld(ji)%laction ) THEN
            DO jm = 1, ncplmodel
               IF( ssnd(kmod)%fld(ji)%nid(1,jm) /= -1 ) THEN
                  IF( TRIM(cdfieldname) == TRIM(ssnd(kmod)%fld(ji)%clname) ) THEN
                     id = ssnd(kmod)%fld(ji)%nid(1,jm)
                     mop = OASIS_Out
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      DO ji = 1, SIZE(srcv)
         IF(srcv(kmod)%fld(ji)%laction ) THEN
            DO jm = 1, ncplmodel
               IF( srcv(kmod)%fld(ji)%nid(1,jm) /= -1 ) THEN
                  IF( TRIM(cdfieldname) == TRIM(srcv(kmod)%fld(ji)%clname) ) THEN
                     id = srcv(kmod)%fld(ji)%nid(1,jm)
                     mop = OASIS_In
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      !
      IF( id /= -1 ) THEN
#if ! defined key_oa3mct_v1v2
         CALL oasis_get_freqs(id, mop, 1, itmp, info)
#else
         CALL oasis_get_freqs(id,      1, itmp, info)
#endif
         cpl_freq = itmp(1)
      ENDIF
      !
   END FUNCTION cpl_freq


   SUBROUTINE cpl_finalize
      !!---------------------------------------------------------------------
      !!              ***  ROUTINE cpl_finalize  ***
      !!
      !! ** Purpose : - Finalizes the coupling. If MPI_init has not been
      !!      called explicitly before cpl_init it will also close
      !!      MPI communication.
      !!----------------------------------------------------------------------
      !
      DEALLOCATE( exfld , ssnd , srcv )
      IF(nstop == 0) THEN
         CALL oasis_terminate( nerror )
      ELSE
         CALL oasis_abort( ncomp_id, "cpl_finalize", "NEMO ABORT STOP" )
      ENDIF
      !
   END SUBROUTINE cpl_finalize

#if ! defined key_oasis3

   !!----------------------------------------------------------------------
   !!   No OASIS Library          OASIS3 Dummy module...
   !!----------------------------------------------------------------------

   SUBROUTINE oasis_init_comp(k1,cd1,k2)
      CHARACTER(*), INTENT(in   ) ::  cd1
      INTEGER     , INTENT(  out) ::  k1,k2
      k1 = -1 ; k2 = -1
      WRITE(numout,*) 'oasis_init_comp: Error you sould not be there...', cd1
   END SUBROUTINE oasis_init_comp

   SUBROUTINE oasis_abort(k1,cd1,cd2)
      INTEGER     , INTENT(in   ) ::  k1
      CHARACTER(*), INTENT(in   ) ::  cd1,cd2
      WRITE(numout,*) 'oasis_abort: Error you sould not be there...', cd1, cd2
   END SUBROUTINE oasis_abort

   SUBROUTINE oasis_get_localcomm(k1,k2)
      INTEGER     , INTENT(  out) ::  k1,k2
      k1 = -1 ; k2 = -1
      WRITE(numout,*) 'oasis_get_localcomm: Error you sould not be there...'
   END SUBROUTINE oasis_get_localcomm

   SUBROUTINE oasis_def_partition(k1,k2,k3,k4)
      INTEGER     , INTENT(  out) ::  k1,k3
      INTEGER     , INTENT(in   ) ::  k2(5)
      INTEGER     , INTENT(in   ) ::  k4
      k1 = k2(1) ; k3 = k2(5)+k4
      WRITE(numout,*) 'oasis_def_partition: Error you sould not be there...'
   END SUBROUTINE oasis_def_partition

   SUBROUTINE oasis_def_var(k1,cd1,k2,k3,k4,k5,k6,k7)
      CHARACTER(*), INTENT(in   ) ::  cd1
      INTEGER     , INTENT(in   ) ::  k2,k3(2),k4,k5(2,2),k6
      INTEGER     , INTENT(  out) ::  k1,k7
      k1 = -1 ; k7 = -1
      WRITE(numout,*) 'oasis_def_var: Error you sould not be there...', cd1
   END SUBROUTINE oasis_def_var

   SUBROUTINE oasis_enddef(k1)
      INTEGER     , INTENT(  out) ::  k1
      k1 = -1
      WRITE(numout,*) 'oasis_enddef: Error you sould not be there...'
   END SUBROUTINE oasis_enddef

   SUBROUTINE oasis_put_2D(k1,k2,p1,k3)
      REAL(wp), DIMENSION(:,:), INTENT(in   ) ::  p1
      INTEGER                 , INTENT(in   ) ::  k1,k2
      INTEGER                 , INTENT(  out) ::  k3
      k3 = -1
      WRITE(numout,*) 'oasis_put: Error you sould not be there...'
   END SUBROUTINE oasis_put_2D

   SUBROUTINE oasis_put_3D(k1,k2,p1,k3)
      REAL(wp), DIMENSION(:,:,:), INTENT(in   ) ::  p1
      INTEGER                   , INTENT(in   ) ::  k1,k2
      INTEGER                   , INTENT(  out) ::  k3
      k3 = -1
      WRITE(numout,*) 'oasis_put: Error you sould not be there...'
   END SUBROUTINE oasis_put_3D

   SUBROUTINE oasis_get_2D(k1,k2,p1,k3)
      REAL(wp), DIMENSION(:,:), INTENT(  out) ::  p1
      INTEGER                 , INTENT(in   ) ::  k1,k2
      INTEGER                 , INTENT(  out) ::  k3
      p1(1,1) = -1. ; k3 = -1
      WRITE(numout,*) 'oasis_get: Error you sould not be there...'
   END SUBROUTINE oasis_get_2D

   SUBROUTINE oasis_get_3D(k1,k2,p1,k3)
      REAL(wp), DIMENSION(:,:,:), INTENT(  out) ::  p1
      INTEGER                   , INTENT(in   ) ::  k1,k2
      INTEGER                   , INTENT(  out) ::  k3
      p1(1,1,1) = -1. ; k3 = -1
      WRITE(numout,*) 'oasis_get: Error you sould not be there...'
   END SUBROUTINE oasis_get_3D   

   SUBROUTINE oasis_get_freqs(k1,k5,k2,k3,k4)
      INTEGER              , INTENT(in   ) ::  k1,k2
      INTEGER, DIMENSION(1), INTENT(  out) ::  k3
      INTEGER              , INTENT(  out) ::  k4,k5
      k3(1) = k1 ; k4 = k2 ; k5 = k2
      WRITE(numout,*) 'oasis_get_freqs: Error you sould not be there...'
   END SUBROUTINE oasis_get_freqs

   SUBROUTINE oasis_terminate(k1)
      INTEGER     , INTENT(  out) ::  k1
      k1 = -1
      WRITE(numout,*) 'oasis_terminate: Error you sould not be there...'
   END SUBROUTINE oasis_terminate

#endif

   !!=====================================================================
END MODULE cpl_oasis3
