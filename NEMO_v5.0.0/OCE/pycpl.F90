MODULE pycpl
   !!======================================================================
   !!                       ***  MODULE  pycpl  ***
   !! Python coupling module : manage coupling with external Python codes deployed with Eophis
   !!======================================================================
   !! History :  4.2.1  ! 2025-11  (A. Barge)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   init_python_coupling     : Initialize coupling with Python
   !!   send_to_python           : send fields to external Python model
   !!   receive_from_python      : receive fields from external Python model
   !!   finalize_python_coupling : Free memory
   !!----------------------------------------------------------------------
   USE cpl_oasis3      
   USE eophis_def
   USE dom_oce
   USE par_oce
   USE lib_mpp
   USE in_out_manager
   USE timing

   IMPLICIT NONE
   PUBLIC

   INTERFACE send_to_python
      MODULE PROCEDURE send_to_python_3d, send_to_python_2d
   END INTERFACE send_to_python

   INTERFACE receive_from_python
      MODULE PROCEDURE receive_from_python_3d, receive_from_python_2d
   END INTERFACE receive_from_python

#  include "do_loop_substitute.h90"

CONTAINS

   SUBROUTINE init_python_coupling()
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE init_python_coupling  ***
      !!
      !! ** Purpose :   Initialisation of the coupling that relies on eophis
      !!
      !! ** Method  :   * Read eophis namelist
      !!                * Define exchanges
      !!                * Configure OASIS
      !!----------------------------------------------------------------------
      ! I/O
      ! local variables
      INTEGER :: ios, jpexch
      INTEGER :: jsnd = 1, jrcv = 1
      TYPE(eophis_var), POINTER :: curr_var
      !!----------------------------------------------------------------------
      !
      ! ===============
      !    Initialize
      ! ===============
      !
      IF( .NOT. lk_oasis )   CALL ctl_stop( 'init_python_coupling : key_oasis3 disabled' )
      !
      IF( lwp ) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*)'init_python_coupling : Setting Python models'
         WRITE(numout,*)'~~~~~~~~~~~~~~~~~~~~'
      END IF
      !
      IF( lwp ) WRITE(numout,*) '      Reading Eophis namelist'
      !
      CALL build_eophis_list(mpi_comm_oce)
      jpexch = count_eophis_var()
      !
      ALLOCATE( srcv(nmodext)%fld(jpexch) , ssnd(nmodext)%fld(jpexch) )
      !
      ! ==================================== !
      !     Define exchanges from Eophis     !
      ! ==================================== !
      !
      IF( lwp ) WRITE(numout,*) '      Configure OASIS for inferences module'
      !
      ! default definitions of ssnd snd srcv
      srcv(nmodext)%fld(:)%laction = .FALSE.  ;  srcv(nmodext)%fld(:)%clgrid = 'T'  ;  srcv(nmodext)%fld(:)%nsgn = 1.
      srcv(nmodext)%fld(:)%nct = 1  ;  srcv(nmodext)%fld(:)%nlvl = 1
      !
      ssnd(nmodext)%fld(:)%laction = .FALSE.  ;  ssnd(nmodext)%fld(:)%clgrid = 'T'  ;  ssnd(nmodext)%fld(:)%nsgn = 1.
      ssnd(nmodext)%fld(:)%nct = 1  ;  ssnd(nmodext)%fld(:)%nlvl = 1
      !
      !
      CALL first_eophis_var(curr_var)
      DO WHILE (associated(curr_var))
         IF(.NOT.curr_var%in) THEN
            ssnd(nmodext)%fld(jsnd)%clname = curr_var%alias
            ssnd(nmodext)%fld(jsnd)%laction = .TRUE.
            ssnd(nmodext)%fld(jsnd)%nlvl = curr_var%nlvl
            curr_var%idx = jsnd
            jsnd = jsnd + 1
         ELSE
            srcv(nmodext)%fld(jrcv)%clname = curr_var%alias
            srcv(nmodext)%fld(jrcv)%laction = .TRUE.
            srcv(nmodext)%fld(jrcv)%nlvl = curr_var%nlvl
            curr_var%idx = jrcv
            jrcv = jrcv + 1
         ENDIF
         CALL eophis_next_var(curr_var)
      END DO
      !
      ! ===================== !
      !    Configure OASIS    !
      ! ===================== !
      CALL cpl_var(jpexch, jpexch, 1, nmodext)
      !
   END SUBROUTINE init_python_coupling


   SUBROUTINE send_to_python_3d(varname,to_send,kt)
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE send_to_python ***
      !!
      !! ** Purpose :   Proceed OASIS sending from Eophis definition
      !!
      !! ** Arguments : CHAR varname : name of the field to send
      !!                REAL(:,:,:) to_send  : Array to send
      !!                INT kt : ocean time step
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      INTEGER, INTENT(in)           ::  kt             ! ocean time step
      CHARACTER(len=*), INTENT(in)  :: varname
      REAL(wp), DIMENSION(:,:,:), INTENT(in) ::  to_send
      ! local variables
      INTEGER :: isec, info
      INTEGER :: is, ie, js, je
      TYPE(eophis_var), POINTER :: curr_var
      !!----------------------------------------------------------------------
      !
      ! Date of exchange
      isec = ( kt - nit000 ) * NINT( rn_Dt )
      info = OASIS_idle
      !
      ! Array bounds
      is = 1 + nn_hls - (jpi - SIZE(to_send,1)) / 2 
      js = 1 + nn_hls - (jpj - SIZE(to_send,2)) / 2 
      ie = jpi - nn_hls - (jpi - SIZE(to_send,1)) / 2 
      je = jpj - nn_hls - (jpj - SIZE(to_send,2)) / 2 
      !
      ! Get Eophis variable
      CALL find_eophis_var(varname,curr_var)
      IF (.NOT.associated(curr_var)) THEN
         CALL ctl_stop( 'send_to_python : unrecognized variable name ', TRIM(varname) )
      END IF
      !
      ! OASIS layer
      IF (curr_var%in) THEN
         CALL ctl_stop( 'send_to_python : function called for incoming variable '//TRIM(varname) )
      ELSE
         CALL cpl_snd(nmodext, curr_var%idx, isec, to_send(is:ie,js:je,1:ssnd(nmodext)%fld(curr_var%idx)%nlvl), info)
      END IF
      !
   END SUBROUTINE send_to_python_3d


   SUBROUTINE send_to_python_2d(varname,to_send,kt)
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE send_to_python ***
      !!
      !! ** Purpose :   Proceed OASIS sending from Eophis definition
      !!
      !! ** Arguments : CHAR varname : name of the field to send
      !!                REAL(:,:) to_send  : Array to send
      !!                INT kt : ocean time step
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      INTEGER, INTENT(in)           ::  kt             ! ocean time step
      CHARACTER(len=*), INTENT(in)  :: varname
      REAL(wp), DIMENSION(:,:), INTENT(in) ::  to_send
      ! local variables
      INTEGER :: isec, info 
      INTEGER :: is, ie, js, je
      TYPE(eophis_var), POINTER :: curr_var
      REAL(wp), DIMENSION(A2D(0),1) :: zbuf
      !!----------------------------------------------------------------------
      !
      ! Date of exchange
      isec = ( kt - nit000 ) * NINT( rn_Dt )
      info = OASIS_idle
      !
      ! Array bounds
      is = 1 + nn_hls - (jpi - SIZE(to_send,1)) / 2 
      js = 1 + nn_hls - (jpj - SIZE(to_send,2)) / 2 
      ie = jpi - nn_hls - (jpi - SIZE(to_send,1)) / 2 
      je = jpj - nn_hls - (jpj - SIZE(to_send,2)) / 2 
      !
      ! Get Eophis variable
      CALL find_eophis_var(varname,curr_var)
      IF (.NOT.associated(curr_var)) THEN
         CALL ctl_stop( 'send_to_python : unrecognized variable name '//TRIM(varname) )
      END IF
      !
      ! OASIS layer
      IF (curr_var%in) THEN
         CALL ctl_stop( 'send_to_python : function called for incoming variable '//TRIM(varname) )
      ELSE
         zbuf(A2D(0),1) = to_send(is:ie,js:je)
         CALL cpl_snd(nmodext, curr_var%idx, isec, zbuf, info)
      END IF
      !
   END SUBROUTINE send_to_python_2d


   SUBROUTINE receive_from_python_3d(varname,to_rcv,kt)
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE receive_from_python  ***
      !!
      !! ** Purpose :   Proceed OASIS receiving from Eophis definition
      !!
      !! ** Arguments : CHAR varname : name of the field to receive
      !!                REAL(:,:,:) to_rcv : Array in which store received field
      !!                INT kt : ocean time step
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      INTEGER, INTENT(in)           ::  kt
      CHARACTER(len=*), INTENT(in)  :: varname
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::  to_rcv
      ! local variables
      INTEGER :: info, isec
      INTEGER :: is, ie, js, je
      TYPE(eophis_var), POINTER :: curr_var
      !!----------------------------------------------------------------------
      !
      ! Date of exchange
      isec = ( kt - nit000 ) * NINT( rn_Dt )
      info = OASIS_idle
      !
      ! Array bounds
      is = 1 + nn_hls - (jpi - SIZE(to_rcv,1)) / 2 
      js = 1 + nn_hls - (jpj - SIZE(to_rcv,2)) / 2 
      ie = jpi - nn_hls - (jpi - SIZE(to_rcv,1)) / 2 
      je = jpj - nn_hls - (jpj - SIZE(to_rcv,2)) / 2 
      !
      ! Get Eophis variable
      CALL find_eophis_var(varname,curr_var)
      IF (.NOT.associated(curr_var)) THEN
         CALL ctl_stop( 'receive_from_python : unrecognized variable name '//TRIM(varname) )
      END IF
      !
      ! OASIS layer
      IF (.NOT. curr_var%in) THEN
         CALL ctl_stop( 'receive_from_python : function called for outcoming variable '//TRIM(varname) )
      ELSE
         CALL cpl_rcv(nmodext, curr_var%idx, isec, to_rcv(is:ie,js:je,1:srcv(nmodext)%fld(curr_var%idx)%nlvl), info)
      END IF
      !
   END SUBROUTINE receive_from_python_3d


   SUBROUTINE receive_from_python_2d(varname,to_rcv,kt)
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE receive_from_python  ***
      !!
      !! ** Purpose :   Proceed OASIS receiving from Eophis definition
      !!
      !! ** Arguments : CHAR varname : name of the field to receive
      !!                REAL(:,:) to_rcv : Array in which store received field
      !!                INT kt : ocean time step
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      INTEGER, INTENT(in)           ::  kt
      CHARACTER(len=*), INTENT(in)  :: varname
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::  to_rcv
      ! local variables
      INTEGER :: info, isec
      INTEGER :: is, ie, js, je
      TYPE(eophis_var), POINTER :: curr_var
      REAL(wp), DIMENSION(A2D(0),1) :: zbuf
      !!----------------------------------------------------------------------
      !
      ! Date of exchange
      isec = ( kt - nit000 ) * NINT( rn_Dt )
      info = OASIS_idle
      !
      ! Get Eophis variable
      CALL find_eophis_var(varname,curr_var)
      IF (.NOT.associated(curr_var)) THEN
         CALL ctl_stop( 'receive_from_python : unrecognized variable name '//TRIM(varname) )
      END IF
      !
      ! Array bounds
      is = 1 + nn_hls - (jpi - SIZE(to_rcv,1)) / 2 
      js = 1 + nn_hls - (jpj - SIZE(to_rcv,2)) / 2 
      ie = jpi - nn_hls - (jpi - SIZE(to_rcv,1)) / 2 
      je = jpj - nn_hls - (jpj - SIZE(to_rcv,2)) / 2 
      !
      ! OASIS layer
      IF (.NOT. curr_var%in) THEN
         CALL ctl_stop( 'receive_from_python : function called for outcoming variable '//TRIM(varname) )
      ELSE
         ! save value if nothing is done
         zbuf(A2D(0),1) = to_rcv(is:ie,js:je)
         CALL cpl_rcv(nmodext, curr_var%idx, isec, zbuf, info)
         to_rcv(is:ie,js:je) = zbuf(A2D(0),1)
      END IF
      !
   END SUBROUTINE receive_from_python_2d


   SUBROUTINE finalize_python_coupling
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE finalize_python_coupling  ***
      !!
      !! ** Purpose :   Free memory used for Python coupling
      !!
      !! ** Method  :   * Deallocate arrays
      !!----------------------------------------------------------------------
      !
      DEALLOCATE( srcv(nmodext)%fld , ssnd(nmodext)%fld )
      CALL purge_eophis()
      !
   END SUBROUTINE finalize_python_coupling

END MODULE pycpl
