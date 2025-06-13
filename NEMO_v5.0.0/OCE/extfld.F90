MODULE extfld
   !!======================================================================
   !!                       ***  MODULE extfld  ***
   !! Inferences module :   variables defined in core memory
   !!======================================================================
   !! History :  4.2  ! 2023-09  (A. Barge)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   extfld_alloc : allocation of fields arrays for inferences module (infmod)
   !!----------------------------------------------------------------------        
   !!=====================================================
   USE par_oce        ! ocean parameters
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   extfld_alloc   ! routine called in infmod.F90
   PUBLIC   extfld_dealloc ! routine called in infmod.F90

   !!----------------------------------------------------------------------
   !!                    2D Inference Module fields
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)  :: ext_ts_2D    !: dummy field to store 2D inferences

   !!----------------------------------------------------------------------
   !!                    3D Inference Module fields
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: ext_u_3D  !: dummy field to store 3D inferences

CONTAINS

   INTEGER FUNCTION extfld_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION extfld_alloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      ALLOCATE( ext_ts_2D(jpi,jpj) , ext_u_3D(jpi,jpj,jpk)  , STAT=ierr )
      extfld_alloc = ierr
      !
   END FUNCTION

   
   INTEGER FUNCTION extfld_dealloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION extfld_dealloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      DEALLOCATE( ext_ts_2D , ext_u_3D  , STAT=ierr )
      extfld_dealloc = ierr
      !
   END FUNCTION

END MODULE extfld
