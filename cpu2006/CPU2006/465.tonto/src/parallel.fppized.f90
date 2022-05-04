module PARALLEL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: destroy_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use REALMAT_MODULE, only: uncompress_from_triangle_
   use REALMAT_MODULE, only: compress_to_triangle_
   use REALMAT_MODULE, only: tri_size_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: destroy_

   use CPXVEC_MODULE, only: create_
   use CPXVEC_MODULE, only: destroy_
  implicit none

! Macros file
! $Id: macros,v 1.37.2.11 2003/10/14 02:28:29 reaper Exp $

!----------------------------------------------------------------------
! Kind parameters for value types
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Size parameters for value types
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Value types ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Memory/Call stack management macros ...
! These define STACK, CHECK, as well as ENSURE, DIE and WARN ...
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Precondition and Postcodition macros ...
! These define ENSURE as well as DIE and WARN ...
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Error macros ...
! These define only DIE and WARN ...
!----------------------------------------------------------------------

!####   define pure
!####   define elemental
!----------------------------------------------------------------------
! Profiling macros ...
! These define START_TIMER, STOP_TIMER ...
!----------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Some compilers cannot handle Fortran 95 features ...
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Some Fortran 95 compilers can't handle pure routines ...
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Some Fortran 95 compilers can't handle default initialisation properly ...
!-------------------------------------------------------------------------------

!----------------------------------------------------------------------
! Convenience macros ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Fundamental constants and conversion factors ...
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Program constants ...
! The first word identifies the module to which the constant belongs
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Program default input orders for vectors of derived type
! The first word identifies the module to which the constant belongs
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Derived non-array types ... (in alphabetical order)
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Parameterised abstract array types ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Derived array types ...
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Type sizes ...
!----------------------------------------------------------------------
   private

   public    sum_vectors_
   interface sum_vectors_
      module procedure sum_vectors
      module procedure sum_vectors_1
   end interface

   public    synchronise_processors_
   interface synchronise_processors_
      module procedure synchronise_processors
   end interface

   public    this_proc_
   interface this_proc_
      module procedure this_proc
   end interface

   public    sum_matrices_
   interface sum_matrices_
      module procedure sum_matrices
   end interface

   public    finalise_
   interface finalise_
      module procedure finalise
   end interface

   public    n_proc_
   interface n_proc_
      module procedure n_proc
   end interface

   public    broadcast_
   interface broadcast_
      module procedure broadcast
      module procedure broadcast_1
      module procedure broadcast_2
      module procedure broadcast_3
      module procedure broadcast_4
      module procedure broadcast_5
      module procedure broadcast_6
      module procedure broadcast_7
      module procedure broadcast_8
      module procedure broadcast_9
      module procedure broadcast_10
      module procedure broadcast_11
      module procedure broadcast_12
      module procedure broadcast_13
      module procedure broadcast_14
      module procedure broadcast_15
      module procedure broadcast_16
   end interface

   public    sum_symmetric_matrices_
   interface sum_symmetric_matrices_
      module procedure sum_symmetric_matrices
   end interface

   public    do_io_
   interface do_io_
      module procedure do_io
   end interface

   public    initialise_
   interface initialise_
      module procedure initialise
   end interface

contains

  subroutine initialise(self)
    type(parallel_type) :: self
   ! Initialise the parallel environment.

    self%nprocs = 1
    self%rank = 0
    self%do_parallel = .false.
  end subroutine

  subroutine finalise(self)
    type(parallel_type) :: self
   ! Finalise the parallel environment.

    self%do_parallel = .false.

  end subroutine

!*******************************************************************************
! Inquiry routines.
!*******************************************************************************

  pure function n_proc(self) result(res)
    type(parallel_type) :: self
   ! Return the number of processors available.
    intent(in) :: self
    integer(kind=kind(1)) :: res
    if (self%do_parallel) then
      res = self%nprocs
    else
      res = 1
    end if

  end function

  pure function this_proc(self) result(res)
    type(parallel_type) :: self
   ! Return the index of this processor.  First index is zero!
    intent(in) :: self
    integer(kind=kind(1)) :: res
    if (self%do_parallel) then
      res = self%rank
    else
      res = 0
    end if

  end function

!*******************************************************************************
! Summation routines.
!*******************************************************************************

  subroutine sum_symmetric_matrices(self,mat)
    type(parallel_type) :: self
   ! This routine adds the versions of "mat" from all processors, and gives the
   ! result to all processors.  The lower triangle of the resultant symmetric
   ! matrix is forced to be the same as the upper triangle.
    real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: mat
    real(kind=kind(1.0d0)), dimension(:), pointer :: invec,outvec
    integer(kind=kind(1)) :: tri_size

    if (self%do_parallel) then
      self%mpi_status = 0
      tri_size = tri_size_(mat)
      call create_(invec,tri_size)
      call create_(outvec,tri_size)
      call compress_to_triangle_(mat,invec)
      call die_(tonto,"PARALLEL:sum_symmetric_matrices ... wtf?")
      call uncompress_from_triangle_(mat,outvec)
      call destroy_(outvec)
      call destroy_(invec)
    else
       ! do nothing, just return what we were given.
    end if

  end subroutine

  subroutine sum_matrices(self,mat)
    type(parallel_type) :: self
   ! This routine adds the versions of "mat" from all processors, and gives the
   ! result to all processors.
    real(kind=kind(1.0d0)), dimension(:,:), intent(inout) :: mat
    real(kind=kind(1.0d0)), dimension(:,:), pointer :: tempmat

    if (self%do_parallel) then
      self%mpi_status = 0
      call create_(tempmat,size(mat,1),size(mat,2))
      call die_(tonto,"PARALLEL:sum_matrices ... wtf?")
      call destroy_(tempmat)
    else
       ! do nothing, just return what we were given.
    end if

  end subroutine

  subroutine sum_vectors(self,vec)
    type(parallel_type) :: self
   ! This routine adds the versions of "vec" from all processors, and gives the
   ! result to all processors.
    real(kind=kind(1.0d0)), dimension(:), intent(inout) :: vec
    real(kind=kind(1.0d0)), dimension(:), pointer :: tempvec

    if (self%do_parallel) then
      self%mpi_status = 0
      call create_(tempvec,size(vec))
      call die_(tonto,"PARALLEL:sum_vectors ... wtf?")
      call destroy_(tempvec)
    else
       ! do nothing, just return what we were given.
    end if

  end subroutine

  subroutine sum_vectors_1(self,vec)
    type(parallel_type) :: self
   ! This routine adds the versions of "vec" from all processors, and gives the
   ! result to all processors.
    complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(inout) :: vec
    complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: tempvec

    if (self%do_parallel) then
      self%mpi_status = 0
      call create_(tempvec,size(vec))
      call die_(tonto,"PARALLEL:sum_vectors_1 ... wtf?")
      call destroy_(tempvec)
    else
       ! do nothing, just return what we were given.
    end if

  end subroutine

!*******************************************************************************
! Broadcast variables to all processors.
!*******************************************************************************

  subroutine broadcast(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    integer(kind=kind(1)) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast ... wtf?")
    end if

  end subroutine

  subroutine broadcast_1(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    character(*) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_1 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_2(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    real(kind=kind(1.0d0)) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_2 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_3(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    complex(kind=kind((1.0d0,1.0d0))) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_3 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_4(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    logical(kind=kind(.true.)) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_4 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_5(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    real(kind=kind(1.0d0)), dimension(:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_5 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_6(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    integer(kind=kind(1)), dimension(:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_6 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_7(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_7 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_8(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    real(kind=kind(1.0d0)), dimension(:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_8 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_9(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    integer(kind=kind(1)), dimension(:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_9 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_10(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_10 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_11(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    real(kind=kind(1.0d0)), dimension(:,:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_11 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_12(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    integer(kind=kind(1)), dimension(:,:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_12 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_13(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_13 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_14(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    real(kind=kind(1.0d0)), dimension(:,:,:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_14 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_15(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_15 ... wtf?")
    end if

  end subroutine

  subroutine broadcast_16(self,var,proc)
    type(parallel_type) :: self
   ! Broadcast variable "var" from processor "proc" to all the other processors.
    complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: var
    integer(kind=kind(1)), intent(in) :: proc

    if (self%do_parallel) then
      self%mpi_status = 0
      call die_(tonto,"PARALLEL:broadcast_16 ... wtf?")
    end if

  end subroutine

  function do_io(self) result(res)
    type(parallel_type) :: self
   ! Return whether or not this processor should do the I/O operation.
    logical(kind=kind(.true.)) :: res

    res = self%rank == 0 .or. (.not. self%do_parallel)

  end function

  subroutine synchronise_processors(self)
    type(parallel_type) :: self
   ! Synchronise all processors at this point.

  end subroutine

end
