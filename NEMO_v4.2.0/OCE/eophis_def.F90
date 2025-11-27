MODULE eophis_def
   !!======================================================================
   !!                       ***  MODULE  eophis_def  ***
   !! Definition of the structure for Eophis coupling variables properties.
   !! The Eophis variables properties are basic types (integer, character)
   !! that are encapsulated into structures. The structures are stored in
   !! a chained list starting with 'eophis_list'
   !!======================================================================
   !! History :  2025-10  (A. Barge)  Original code
   !!----------------------------------------------------------------------

   !!-------------------------- MODULE API --------------------------------
   !! build_eophis_list      : Build eophis_list from Eophis namelist
   !! count_eophis_var       : Return number of elements in eophis_list
   !! first_eophis_var       : Get a pointer on eophis_list
   !! eophis_next_var        : Return next element from an element
   !! new_eophis_var         : Add new Eophis coupling variable in the eophis_list
   !! find_eophis_var        : Find Eophis coupling variable in the eophis_list
   !! purge_eophis           : Remove Eophis coupling variables in the eophis_list
   !!----------------------------------------------------------------------

   USE MPI

   IMPLICIT NONE
   PUBLIC

   ! Eophis coupling variable structure
   TYPE eophis_var
      CHARACTER(len=32) :: name                   ! Eophis variable name
      CHARACTER(len=32) :: alias                  ! Alias used by OASIS on Fortran side to manipulate the Eophis variable
      INTEGER :: nlvl                             ! Number of level of which the variable should exchanged
      INTEGER :: idx                              ! Hash index to identify the variable (may be useful in some implementation)
      LOGICAL :: in                               ! From model point of view - True : incoming variables , False : outcoming variables
      TYPE(eophis_var), POINTER, PRIVATE :: next  ! Next pointer - private to avoid breaking the chain outside of the module
   END TYPE eophis_var

   ! First eophis variable - can only be modified through Module API
   TYPE(eophis_var), POINTER, PRIVATE :: eophis_list => NULL()

    
CONTAINS

   SUBROUTINE build_eophis_list(model_comm)
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE build_eophis_lst  ***
      !!
      !! ** Purpose :   Build the entire eophis list from Eophis namelist
      !!
      !! ** Methods :  Read namelist in parallel or sequential context
      !!               Usr Eophis list methods to build it
      !!
      !! ** Arguments : INT model_comm : MPI communicator with which read the namelist. Sequential if not given
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      INTEGER, INTENT(in), OPTIONAL  :: model_comm
      ! local variables
      INTEGER, PARAMETER :: MAXL = 32
      INTEGER :: rank, ierr, ios
      INTEGER :: nb_var, i
      INTEGER, ALLOCATABLE :: cpl_lvls(:)
      CHARACTER(len=MAXL), ALLOCATABLE :: cpl_names(:), cpl_aliases(:)
      LOGICAL, ALLOCATABLE :: cpl_ins(:)
      LOGICAL :: paral
      !
      NAMELIST /nameophis_nb/ nb_var
      NAMELIST /nameophis_var/ cpl_names, cpl_aliases, cpl_lvls, cpl_ins
      !!----------------------------------------------------------------------

      ! 1. Parallel or sequential
      ! -------------------------
      IF (.NOT. present(model_comm)) THEN
         paral = .FALSE.
         rank = 0
      ELSE
         paral = .TRUE.
         CALL MPI_COMM_RANK(model_comm, rank, ierr)
      END IF

      ! 2. Read namelist
      ! ----------------
      ! Read number of variables
      IF (rank == 0) THEN
         OPEN(unit=11, file='eophis_nml', status='old', action='read', iostat=ios)
         IF (ios /= 0) PRINT*, 'Error in opening eophis_nml'

         READ(11, nml=nameophis_nb, iostat=ios)
         IF (ios /= 0) PRINT*, 'Error in reading nameophis_nb in eophis_nml'
         IF (nb_var .LE. 0) PRINT*, 'Error in nameophis_nb: nb_var must be strictly positive'
      END IF

      ! Communicate number of variables
      IF (paral) THEN
         CALL MPI_BCAST(nb_var, 1, MPI_INTEGER, 0, model_comm, ierr)
      END IF

      ! Allocate arrays
      ALLOCATE(cpl_names(nb_var),cpl_aliases(nb_var),cpl_lvls(nb_var),cpl_ins(nb_var))

      ! Read variables
      IF (rank == 0) THEN
         READ(11, nml=nameophis_var, iostat=ios)
         IF (ios /= 0) PRINT*, 'Error in reading nameophis_var in eophis_nml'

         CLOSE(11)
      END IF

      ! Communicate variables
      IF (paral) THEN
         CALL MPI_BCAST(cpl_names, nb_var*MAXL, MPI_CHARACTER, 0, model_comm, ierr)
         CALL MPI_BCAST(cpl_aliases, nb_var*MAXL, MPI_CHARACTER, 0, model_comm, ierr)
         CALL MPI_BCAST(cpl_lvls, nb_var, MPI_INTEGER, 0, model_comm, ierr)
         CALL MPI_BCAST(cpl_ins, nb_var, MPI_LOGICAL, 0, model_comm, ierr)
      END IF

      ! 3. Build eophis list
      ! --------------------
      CALL purge_eophis()
      DO i = 1, nb_var
         CALL new_eophis_var(cpl_names(i), cpl_aliases(i), cpl_lvls(i), cpl_ins(i))
      END DO

      ! 4. Free memory
      ! --------------
      DEALLOCATE(cpl_names,cpl_aliases,cpl_lvls,cpl_ins)

   END SUBROUTINE build_eophis_list


   INTEGER FUNCTION count_eophis_var()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION count_eophis_var  ***
      !!
      !! ** Purpose :  Return number of variables in eophis_list
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      ! local variables
      TYPE(eophis_var), POINTER :: curr_var
      !!----------------------------------------------------------------------

      ! Init
      count_eophis_var = 0
      curr_var => eophis_list

      ! Browse list
      DO WHILE (associated(curr_var))
         count_eophis_var = count_eophis_var + 1
         curr_var => curr_var%next
      END DO

   END FUNCTION count_eophis_var


   SUBROUTINE first_eophis_var(curr_var)
      !!----------------------------------------------------------------------
      !!             ***  SUBROUTINE first_eophis_var  ***
      !!
      !! ** Purpose :  Return a pointer on first element of (private) eophis_list
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      ! local variables
      TYPE(eophis_var), POINTER :: curr_var
      !!----------------------------------------------------------------------

      curr_var => eophis_list

   END SUBROUTINE first_eophis_var


   SUBROUTINE eophis_next_var(curr_var)
      !!----------------------------------------------------------------------
      !!             ***  SUBROUTINE first_eophis_var  ***
      !!
      !! ** Purpose :  Return the next element (private) from an element
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      ! local variables
      TYPE(eophis_var), POINTER :: curr_var
      !!----------------------------------------------------------------------

      curr_var => curr_var%next

   END SUBROUTINE eophis_next_var


   SUBROUTINE new_eophis_var(name,alias,nlvl,lin,idx,new_ptr)
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE new_eophis_var  ***
      !!
      !! ** Purpose :   Add new Eophis coupling variable in the eophis_list
      !!
      !! ** Arguments : CHAR name : name of the Eophis variable to add
      !!                CHAR alias : alias of the Eophis variable to add
      !!                INT nlvl : depth level of the Eophis variable to add
      !!                INT ixd : hash index of the Eophis variable to add, imposed by default
      !!                TYPE(eophis_var) new_ptr : optional pointer on the new Eophis variable
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      CHARACTER(len=*), INTENT(in)        :: name, alias
      INTEGER, INTENT(in)                 :: nlvl
      LOGICAL, INTENT(in)                 :: lin
      INTEGER, INTENT(in), OPTIONAL       :: idx
      TYPE(eophis_var), POINTER, OPTIONAL :: new_ptr
      ! local variables
      TYPE(eophis_var), POINTER :: new_var, last_var
      INTEGER :: count
      !!----------------------------------------------------------------------

      ! Init
      count = 1

      ! Find the last non allocated pointer
      IF (.NOT. associated(eophis_list)) THEN
         ALLOCATE(new_var)
         eophis_list => new_var
         new_var => eophis_list
      ELSE
         count = count + 1
         last_var => eophis_list
         DO WHILE (associated(last_var%next))
            count = count + 1
            last_var => last_var%next
         END DO
         ALLOCATE(last_var%next)
         new_var => last_var%next
      END IF

      ! Default values
      IF (present(idx)) THEN
         count = idx
      END IF

      ! Fill values
      new_var%name = name
      new_var%alias = alias
      new_var%nlvl = nlvl
      new_var%idx = count
      new_var%in = lin
      new_var%next => NULL()

      ! Return pointer
      IF (present(new_ptr)) THEN
         new_ptr => new_var
      END IF
          
   END SUBROUTINE new_eophis_var


   SUBROUTINE find_eophis_var(name,res_ptr)
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE find_eophis_var  ***
      !!
      !! ** Purpose :  find and return an Eophis coupling variable in the eophis_list from its name
      !!
      !! ** Arguments : CHAR name : name of the Eophis variable to find
      !!                TYPE(eophis_var) res_ptr : pointer on the found Eophis variable
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      CHARACTER(len=*), INTENT(in)    :: name
      TYPE(eophis_var), POINTER       :: res_ptr
      ! local variables
      TYPE(eophis_var), POINTER :: curr_var
      LOGICAL :: done, found
      !!----------------------------------------------------------------------

      ! Init
      done = .FALSE.
      found = .FALSE.
      res_ptr => NULL()

      ! Beginning of the list
      IF (associated(eophis_list)) THEN
         curr_var => eophis_list
      ELSE
         curr_var => NULL()
         done = .TRUE.
      END IF

      ! Search
      DO WHILE ((.NOT.done).AND.(.NOT.found))
         IF (curr_var%name .EQ. name) THEN
            found = .TRUE.
            res_ptr => curr_var
         END IF

         IF (associated(curr_var%next)) THEN
            curr_var => curr_var%next
         ELSE
            done = .TRUE.
         END IF
      END DO

   END SUBROUTINE find_eophis_var


   SUBROUTINE purge_eophis()
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE purge_eophis  ***
      !!
      !! ** Purpose :  Destroy all variable in eophis_list
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      ! I/O
      ! local variables
      TYPE(eophis_var), POINTER :: curr_var, to_del
      !!----------------------------------------------------------------------

      ! Browse the list and remove the elements
      DO WHILE (associated(eophis_list))
         to_del => eophis_list
         IF (associated(eophis_list%next)) THEN
            eophis_list => eophis_list%next
            to_del%next => NULL()
         ELSE
            eophis_list => NULL()
         END IF
         DEALLOCATE(to_del)
      END DO

   END SUBROUTINE purge_eophis

END MODULE eophis_def

