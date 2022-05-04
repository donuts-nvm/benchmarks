!---------------------------------------------------------------------------
!
!  FILE : Unformatted sequential advancing I/O, for fast mass data storage
!
!  NOTE: If ascii files are required consider using TEXTFILE.
!
!  A file is referred to only by a STR name. Unit numbers are not required.
!  Creation of a file object does not lead to creation of the actual file
!  on the computer, it merely creates the label for the file.
!  The actual file may already exist. If it does not exist, then an
!  FILE_open command will bring it into existence. Otherwise, FILE_open
!  will open the existing file. Closing a file does not delete the actual
!  file unless specifically requested.
!
!  When reading or writing an object to the file, it is assumed that
!  each object occupied one abstract "record". After a recor5d is written,
!  it may not be overwritten without destroying all data objects in the
!  following records. It is recommended that multiple data objects which
!  are logically related be stored in different files with appropriate names
!  for each file which indicate the relationship of the data objects
!  within them.
!
!  Strings are regarded as type STR for purposes of output to the file.
!
!  If the read or write statements give a segmentation fault for large
!  arrays or matrices, try increasing your stack size.
!
! Copyright (C) Dylan Jayatilaka, 1997
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA  02111-1307, USA.
!
! $Id: file.foo,v 1.13.2.6 2003/11/13 05:34:39 reaper Exp $
!---------------------------------------------------------------------------

module FILE_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: create_copy_
   use INTVEC_MODULE, only: destroy_

   use INT_MODULE, only: to_str_

   use PARALLEL_MODULE, only: broadcast_
   use PARALLEL_MODULE, only: do_io_

   use CPXVEC_MODULE, only: create_
   use CPXVEC_MODULE, only: create_copy_
   use CPXVEC_MODULE, only: destroy_

   use UNITNUMBER_MODULE, only: free_
   use UNITNUMBER_MODULE, only: get_
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

   public    write_buffered_cpxmat3_
   interface write_buffered_cpxmat3_
      module procedure write_buffered_cpxmat3
   end interface

   public    read_realmat4_
   interface read_realmat4_
      module procedure read_realmat4
   end interface

   public    write_buffered_cpxmat4_
   interface write_buffered_cpxmat4_
      module procedure write_buffered_cpxmat4
   end interface

   public    write_buffered_cpxmat5_
   interface write_buffered_cpxmat5_
      module procedure write_buffered_cpxmat5
   end interface

   public    write_cpxvec_
   interface write_cpxvec_
      module procedure write_cpxvec
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    read_cpx_
   interface read_cpx_
      module procedure read_cpx
   end interface

   public    write_mat3_
   interface write_mat3_
      module procedure write_mat3
   end interface

   public    write_mat_
   interface write_mat_
      module procedure write_mat
   end interface

   public    flush_int_buffer_
   interface flush_int_buffer_
      module procedure flush_int_buffer
   end interface

   public    close_
   interface close_
      module procedure close
   end interface

   public    read_buffered_mat3_
   interface read_buffered_mat3_
      module procedure read_buffered_mat3
   end interface

   public    write_mat4_
   interface write_mat4_
      module procedure write_mat4
   end interface

   public    read_buffered_mat4_
   interface read_buffered_mat4_
      module procedure read_buffered_mat4
   end interface

   public    read_realmat_
   interface read_realmat_
      module procedure read_realmat
   end interface

   public    get_real_buffer_
   interface get_real_buffer_
      module procedure get_real_buffer
   end interface

   public    read_int_
   interface read_int_
      module procedure read_int
   end interface

   public    write_cpxmat_
   interface write_cpxmat_
      module procedure write_cpxmat
   end interface

   public    flush_real_buffer_
   interface flush_real_buffer_
      module procedure flush_real_buffer
   end interface

   public    read_buffered_intvec_
   interface read_buffered_intvec_
      module procedure read_buffered_intvec
   end interface

   public    write_buffered_vec_
   interface write_buffered_vec_
      module procedure write_buffered_vec
   end interface

   public    read_buffered_cpxmat3_
   interface read_buffered_cpxmat3_
      module procedure read_buffered_cpxmat3
   end interface

   public    read_buffered_cpxmat4_
   interface read_buffered_cpxmat4_
      module procedure read_buffered_cpxmat4
   end interface

   public    read_buffered_cpxmat5_
   interface read_buffered_cpxmat5_
      module procedure read_buffered_cpxmat5
   end interface

   public    read_intvec_
   interface read_intvec_
      module procedure read_intvec
   end interface

   public    backspace_
   interface backspace_
      module procedure backspace
   end interface

   public    write_real_
   interface write_real_
      module procedure write_real
   end interface

   public    read_buffered_real_
   interface read_buffered_real_
      module procedure read_buffered_real
   end interface

   public    move_to_end_
   interface move_to_end_
      module procedure move_to_end
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    read_buffered_intmat_
   interface read_buffered_intmat_
      module procedure read_buffered_intmat
   end interface

   public    write_buffered_mat_
   interface write_buffered_mat_
      module procedure write_buffered_mat
   end interface

   public    get_int_buffer_
   interface get_int_buffer_
      module procedure get_int_buffer
   end interface

   public    write_buffered_mat3_
   interface write_buffered_mat3_
      module procedure write_buffered_mat3
   end interface

   public    read_real_
   interface read_real_
      module procedure read_real
   end interface

   public    write_buffered_mat4_
   interface write_buffered_mat4_
      module procedure write_buffered_mat4
   end interface

   public    read_buffered_vec_
   interface read_buffered_vec_
      module procedure read_buffered_vec
   end interface

   public    is_open_
   interface is_open_
      module procedure is_open
   end interface

   public    move_to_record_
   interface move_to_record_
      module procedure move_to_record
   end interface

   public    read_intmat_
   interface read_intmat_
      module procedure read_intmat
   end interface

   public    read_bin_
   interface read_bin_
      module procedure read_bin
   end interface

   public    write_cpx_
   interface write_cpx_
      module procedure write_cpx
   end interface

   public    write_buffered_intvec_
   interface write_buffered_intvec_
      module procedure write_buffered_intvec
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    close_and_delete_
   interface close_and_delete_
      module procedure close_and_delete
   end interface

   public    delete_
   interface delete_
      module procedure delete
   end interface

   public    open_
   interface open_
      module procedure open
   end interface

   public    read_buffered_mat_
   interface read_buffered_mat_
      module procedure read_buffered_mat
   end interface

   public    write_int_
   interface write_int_
      module procedure write_int
   end interface

   public    read_str_
   interface read_str_
      module procedure read_str
   end interface

   public    write_buffered_real_
   interface write_buffered_real_
      module procedure write_buffered_real
   end interface

   public    read_cpxmat3_
   interface read_cpxmat3_
      module procedure read_cpxmat3
   end interface

   public    position_
   interface position_
      module procedure position
   end interface

   public    read_cpxmat4_
   interface read_cpxmat4_
      module procedure read_cpxmat4
   end interface

   public    read_cpxmat5_
   interface read_cpxmat5_
      module procedure read_cpxmat5
   end interface

   public    write_buffered_intmat_
   interface write_buffered_intmat_
      module procedure write_buffered_intmat
   end interface

   public    flush_cpx_buffer_
   interface flush_cpx_buffer_
      module procedure flush_cpx_buffer
   end interface

   public    write_buffered_cpx_
   interface write_buffered_cpx_
      module procedure write_buffered_cpx
   end interface

   public    read_buffered_cpxvec_
   interface read_buffered_cpxvec_
      module procedure read_buffered_cpxvec
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    write_intvec_
   interface write_intvec_
      module procedure write_intvec
   end interface

   public    read_cpxvec_
   interface read_cpxvec_
      module procedure read_cpxvec
   end interface

   public    write_bin_
   interface write_bin_
      module procedure write_bin
   end interface

   public    write_buffered_int_
   interface write_buffered_int_
      module procedure write_buffered_int
   end interface

   public    is_for_reading_
   interface is_for_reading_
      module procedure is_for_reading
   end interface

   public    read_buffered_cpxmat_
   interface read_buffered_cpxmat_
      module procedure read_buffered_cpxmat
   end interface

   public    move_to_position_
   interface move_to_position_
      module procedure move_to_position
   end interface

   public    unit_used_
   interface unit_used_
      module procedure unit_used
   end interface

   public    get_cpx_buffer_
   interface get_cpx_buffer_
      module procedure get_cpx_buffer
   end interface

   public    read_buffered_cpx_
   interface read_buffered_cpx_
      module procedure read_buffered_cpx
   end interface

   public    write_intmat_
   interface write_intmat_
      module procedure write_intmat
   end interface

   public    write_cpxmat3_
   interface write_cpxmat3_
      module procedure write_cpxmat3
   end interface

   public    skip_
   interface skip_
      module procedure skip
   end interface

   public    read_cpxmat_
   interface read_cpxmat_
      module procedure read_cpxmat
   end interface

   public    write_str_
   interface write_str_
      module procedure write_str
   end interface

   public    write_cpxmat4_
   interface write_cpxmat4_
      module procedure write_cpxmat4
   end interface

   public    write_cpxmat5_
   interface write_cpxmat5_
      module procedure write_cpxmat5
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    exists_
   interface exists_
      module procedure exists
   end interface

   public    read_buffered_int_
   interface read_buffered_int_
      module procedure read_buffered_int
   end interface

   public    write_buffered_cpxvec_
   interface write_buffered_cpxvec_
      module procedure write_buffered_cpxvec
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    rewind_
   interface rewind_
      module procedure rewind
   end interface

   public    write_buffered_cpxmat_
   interface write_buffered_cpxmat_
      module procedure write_buffered_cpxmat
   end interface

   public    write_vec_
   interface write_vec_
      module procedure write_vec
   end interface

   public    read_realvec_
   interface read_realvec_
      module procedure read_realvec
   end interface

   public    read_realmat3_
   interface read_realmat3_
      module procedure read_realmat3
   end interface

   public get_; interface get_
      module procedure read_str
      module procedure read_real
      module procedure read_cpx
      module procedure read_int
      module procedure read_bin
      module procedure read_intvec
      module procedure read_realvec
      module procedure read_cpxvec
      module procedure read_realmat
      module procedure read_realmat3
      module procedure read_realmat4
      module procedure read_intmat
      module procedure read_cpxmat
      module procedure read_cpxmat3
      module procedure read_cpxmat4
      module procedure read_cpxmat5
   end interface

   public read_; interface read_
      module procedure read_str
      module procedure read_real
      module procedure read_cpx
      module procedure read_int
      module procedure read_bin
      module procedure read_intvec
      module procedure read_realvec
      module procedure read_cpxvec
      module procedure read_realmat
      module procedure read_realmat3
      module procedure read_realmat4
      module procedure read_intmat
      module procedure read_cpxmat
      module procedure read_cpxmat3
      module procedure read_cpxmat4
      module procedure read_cpxmat5
   end interface

   public write_; interface write_
      module procedure write_int
      module procedure write_real
      module procedure write_cpx
      module procedure write_str
      module procedure write_bin
      module procedure write_vec
      module procedure write_intvec
      module procedure write_cpxvec
      module procedure write_mat
      module procedure write_mat3
      module procedure write_mat4
      module procedure write_intmat
      module procedure write_cpxmat
      module procedure write_cpxmat3
      module procedure write_cpxmat4
      module procedure write_cpxmat5
   end interface

   public put_; interface put_
      module procedure write_int
      module procedure write_real
      module procedure write_cpx
      module procedure write_str
      module procedure write_bin
      module procedure write_vec
      module procedure write_intvec
      module procedure write_cpxvec
      module procedure write_mat
      module procedure write_mat3
      module procedure write_mat4
      module procedure write_intmat
      module procedure write_cpxmat
      module procedure write_cpxmat3
      module procedure write_cpxmat4
      module procedure write_cpxmat5
   end interface

   public write_buffered_; interface write_buffered_
    module procedure write_buffered_real
    module procedure write_buffered_int
    module procedure write_buffered_cpx
    module procedure write_buffered_vec
    module procedure write_buffered_intvec
    module procedure write_buffered_cpxvec
    module procedure write_buffered_mat
    module procedure write_buffered_intmat
    module procedure write_buffered_cpxmat
    module procedure write_buffered_mat3
    module procedure write_buffered_cpxmat3
    module procedure write_buffered_mat4
    module procedure write_buffered_cpxmat4
    module procedure write_buffered_cpxmat5
  end interface

   public read_buffered_; interface read_buffered_
    module procedure read_buffered_real
    module procedure read_buffered_int
    module procedure read_buffered_cpx
    module procedure read_buffered_vec
    module procedure read_buffered_intvec
    module procedure read_buffered_cpxvec
    module procedure read_buffered_mat
    module procedure read_buffered_intmat
    module procedure read_buffered_cpxmat
    module procedure read_buffered_mat3
    module procedure read_buffered_cpxmat3
    module procedure read_buffered_mat4
    module procedure read_buffered_cpxmat4
    module procedure read_buffered_cpxmat5
  end interface

contains

!  *****************************
!  File creation type operations
!  *****************************

   subroutine create(self,name)
    type(file_type) :: self
    ! Create a file-label object. Does not open the file.  Sets the filename if
    ! present.
      pointer :: self
      character(*), optional :: name
      type(unitnumber_type) :: unit

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)
      if (present(name)) then
        self%name = name
        call get_(unit,self%unit)
      end if

   end subroutine

   subroutine destroy(self)
    type(file_type) :: self
    ! Destroy a file-label object
      pointer :: self
      type(unitnumber_type) :: unit

      if (.not. associated(self)) then;   return; end if
      if (is_open_(self) .and. unit_used_(self)) call close_(self)
      call free_(unit,self%unit)
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(file_type) :: self
    ! Nullify the pointer parts

      nullify( self%real_buffer )
      nullify( self%int_buffer )
      nullify( self%cpx_buffer )

   end subroutine

   subroutine destroy_ptr_part(self)
    type(file_type) :: self
    ! Destroy pointer parts

      call destroy_(self%real_buffer)
      call destroy_(self%int_buffer)
      call destroy_(self%cpx_buffer)

   end subroutine

   subroutine create_copy(self,f)
    type(file_type) :: self
    ! Create a copy a file "f"
      pointer :: self
       type(file_type) :: f

      call create_(self)
      call copy_(self,f)

   end subroutine

   subroutine copy(self,f)
    type(file_type) :: self
    ! Copy a file "f"
      type(file_type) :: f

      self = f
      if (associated(f%int_buffer)) &
         call create_copy_(self%int_buffer,f%int_buffer)
      if (associated(f%real_buffer)) &
         call create_copy_(self%real_buffer,f%real_buffer)
      if (associated(f%cpx_buffer)) &
         call create_copy_(self%cpx_buffer,f%cpx_buffer)

   end subroutine

   subroutine set_defaults(self)
    type(file_type) :: self
    ! Set up defaults

      self%record = 1
      self%io_status = 0
      self%file_status = "unknown"
      self%action      = "readwrite"
      self%buffered    = .false.
      self%real_buffer_pos = 1
      self%int_buffer_pos = 1
      self%cpx_buffer_pos = 1

   end subroutine

   subroutine open(self,for,buffered,type)
    type(file_type) :: self
    ! Open the file. The file object must already be created.
    ! If present, "for" indicated whether the file is "read_write", "read-only"
    ! or "write-only"
    ! If present, "buffered" indicated whether the file is to be buffered, and
    ! the "type" of the buffer must also be specified as "real" or "int"
     character(*), optional :: for
     logical(kind=kind(.true.)),    optional :: buffered
     character(*), optional :: type

     if (present(for)) then
        select case (for)
           case("readwrite ","read-write") ;              self%action = "readwrite"
           case("read      ","reading   ","read-only ") ; self%action = "read"
           case("write     ","writing   ","write-only") ; self%action = "write"
           case default; allocate(tonto%known_keywords(8))
           tonto%known_keywords(1) = "readwrite "
           tonto%known_keywords(2) = "read-write"
           tonto%known_keywords(3) = "read      "
           tonto%known_keywords(4) = "reading   "
           tonto%known_keywords(5) = "read-only "
           tonto%known_keywords(6) = "write     "
           tonto%known_keywords(7) = "writing   "
           tonto%known_keywords(8) = "write-only"
           call unknown_(tonto,for,"FILE:open")
           deallocate(tonto%known_keywords)
        end select
     end if
     self%file_status = "new"
     if (exists_(self)) self%file_status = "old"
     if (do_io_(tonto_parallel)) then
       open( unit   = self%unit,       &
           file   = self%name,         &
           status = self%file_status,  &
           access = "sequential",  &
           form   = "unformatted", &
 ! <---------
           action = self%action,       &
           iostat = self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call ensure_(tonto,self%io_status==0,"FILE:open ... error opening "//trim(self%file_status)//" file "//self%name)
     if (present(buffered)) self%buffered = buffered
     if (self%buffered) then
        call ensure_(tonto,present(type),"FILE:open ... type of buffer must be specified")
        call ensure_(tonto,self%action/="readwrite","FILE:open ... buffer must be either read-only or write-only")
        select case (type)
           case("real")
             call create_(self%real_buffer,1024)
             if (self%action == "read") call get_real_buffer_(self)
           case("int")
             call create_(self%int_buffer,1024)
             if (self%action == "read") call get_int_buffer_(self)
           case("cpx")
             call create_(self%cpx_buffer,1024)
             if (self%action == "read") call get_cpx_buffer_(self)
           case default
             call die_(tonto,"FILE:open ... unknown buffer type, "//trim(type))
        end select
     end if

   end subroutine

   subroutine close(self)
    type(file_type) :: self
    ! Close the file

     call warn_if_(tonto,.not. is_open_(self),"FILE:close ... file is not open")
     if (self%action=="write") then
       if (self%real_buffer_pos > 1) call flush_real_buffer_(self)
       if (self%int_buffer_pos > 1) call flush_int_buffer_(self)
       if (self%cpx_buffer_pos > 1) call flush_cpx_buffer_(self)
     end if
     call destroy_ptr_part_(self)
     if (do_io_(tonto_parallel)) then
       close(unit=self%unit,iostat=self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)

   end subroutine

   subroutine close_and_delete(self)
    type(file_type) :: self
    ! Close the file and delete it from the file system

     call warn_if_(tonto,.not. is_open_(self),"FILE:close_and_delete ... file is not open")
     if (self%action=="write") then
       if (self%real_buffer_pos > 1) call flush_real_buffer_(self)
       if (self%int_buffer_pos > 1) call flush_int_buffer_(self)
       if (self%cpx_buffer_pos > 1) call flush_cpx_buffer_(self)
     end if
     call destroy_ptr_part_(self)
     if (do_io_(tonto_parallel)) then
      close(unit=self%unit,status="delete",iostat=self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)

   end subroutine

   subroutine delete(self)
    type(file_type) :: self
    ! Delete the file from the file system

      if (.not. is_open_(self)) call open_(self)
      call close_and_delete_(self)

   end subroutine

!  **************************
!  Data input type operations
!  **************************

   subroutine read_str(self,value)
    type(file_type) :: self
    ! Read a default str from the file into variable "value"
     character(*) :: value

     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) value
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,value,0)
     call ensure_(tonto,self%io_status==0,"FILE:read_str ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1

   end subroutine

   subroutine read_int(self,value)
    type(file_type) :: self
    ! Read an integer from the file into variable "value"
     integer(kind=kind(1)) :: value

     if (self%buffered) then
       call read_buffered_int_(self,value)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) value
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,value,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_int ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_real(self,value)
    type(file_type) :: self
    ! Read a real from the file into variable "value"
     real(kind=kind(1.0d0)) :: value

     if (self%buffered) then
       call read_buffered_real_(self,value)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) value
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,value,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_real ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_cpx(self,value)
    type(file_type) :: self
    ! Read a complex real(kind=kind(1.0d0)) from the file into variable "value"
     complex(kind=kind((1.0d0,1.0d0))) :: value

     if (self%buffered) then
       call read_buffered_cpx_(self,value)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) value
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,value,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_cpx ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_bin(self,value)
    type(file_type) :: self
    ! Read a logical from the file into variable "value"
     logical(kind=kind(.true.)) :: value

     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) value
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,value,0)
     call ensure_(tonto,self%io_status==0,"FILE:read_bin ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1

   end subroutine

   subroutine read_realvec(self,vec)
    type(file_type) :: self
    ! Read a vector from the file into variable "vec"
     real(kind=kind(1.0d0)), dimension(:) :: vec

     if (self%buffered) then
       call read_buffered_vec_(self,vec)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) vec
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,vec,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_realvec ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_intvec(self,vec)
    type(file_type) :: self
    ! Read a vector from the file into variable "vec"
     integer(kind=kind(1)), dimension(:) :: vec

     if (self%buffered) then
       call read_buffered_intvec_(self,vec)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) vec
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,vec,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_intvec ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_cpxvec(self,vec)
    type(file_type) :: self
    ! Read a vector from the file into variable "vec"
     complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: vec

     if (self%buffered) then
       call read_buffered_cpxvec_(self,vec)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) vec
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,vec,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_cpxvec ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_realmat(self,mat)
    type(file_type) :: self
    ! Read a matrix from the file into variable "mat"
     real(kind=kind(1.0d0)), dimension(:,:) :: mat

     if (self%buffered) then
       call read_buffered_mat_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_realmat ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_intmat(self,mat)
    type(file_type) :: self
    ! Read an integer matrix from the file into variable "mat"
     integer(kind=kind(1)), dimension(:,:) :: mat

     if (self%buffered) then
       call read_buffered_intmat_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_intmat ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_cpxmat(self,mat)
    type(file_type) :: self
    ! Read a complex matrix from the file into variable "mat"
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: mat

     if (self%buffered) then
       call read_buffered_cpxmat_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_cpxmat ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_realmat3(self,mat)
    type(file_type) :: self
    ! Read a matrix from the file into variable "mat"
     real(kind=kind(1.0d0)), dimension(:,:,:) :: mat

     if (self%buffered) then
       call read_buffered_mat3_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_realmat3 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_cpxmat3(self,mat)
    type(file_type) :: self
    ! Read a complex matrix from the file into variable "mat"
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: mat

     if (self%buffered) then
       call read_buffered_cpxmat3_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_cpxmat3 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_realmat4(self,mat)
    type(file_type) :: self
    ! Read a matrix from the file into variable "mat"
     real(kind=kind(1.0d0)), dimension(:,:,:,:) :: mat

     if (self%buffered) then
       call read_buffered_mat4_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_realmat4 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_cpxmat4(self,mat)
    type(file_type) :: self
    ! Read a complex matrix from the file into variable "mat"
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: mat

     if (self%buffered) then
       call read_buffered_cpxmat4_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_cpxmat4 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

   subroutine read_cpxmat5(self,mat)
    type(file_type) :: self
    ! Read a complex matrix from the file into variable "mat"
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: mat

     if (self%buffered) then
       call read_buffered_cpxmat5_(self,mat)
     else
       if (do_io_(tonto_parallel)) then
         read(unit=self%unit,iostat=self%io_status) mat
       end if
       call broadcast_(tonto_parallel,self%io_status,0)
       call broadcast_(tonto_parallel,mat,0)
       call ensure_(tonto,self%io_status==0,"FILE:read_cpxmat5 ... read error, error="// trim(to_str_(self%io_status)))
       self%record = self%record + 1
     end if

   end subroutine

!  ********************
!  Data output routines
!  ********************

   subroutine write_int(self,value)
    type(file_type) :: self
    ! Write an integer to the file as a record
      integer(kind=kind(1)) :: value

      if (self%buffered) then
         call write_buffered_int_(self,value)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) value
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_int ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_real(self,value)
    type(file_type) :: self
    ! Write a real(kind=kind(1.0d0)) to the file as a record
      real(kind=kind(1.0d0)) :: value

      if (self%buffered) then
         call write_buffered_real_(self,value)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) value
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_real ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_cpx(self,value)
    type(file_type) :: self
    ! Write a cpx to the file as a record
      complex(kind=kind((1.0d0,1.0d0))) :: value

      if (self%buffered) then
         call write_buffered_cpx_(self,value)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) value
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_cpx ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_str(self,value)
    type(file_type) :: self
    ! Write a str to the file as a record
      character(*) :: value
      character(len(value)) :: str1

      str1 = value
      if (do_io_(tonto_parallel)) then
        write(unit=self%unit,iostat=self%io_status) str1
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      call ensure_(tonto,self%io_status==0,"FILE:write_str ... write error, error="// trim(to_str_(self%io_status)))
      self%record = self%record + 1

   end subroutine

   subroutine write_bin(self,value)
    type(file_type) :: self
    ! Write a bin to the file as a record
      logical(kind=kind(.true.)) :: value

      if (do_io_(tonto_parallel)) then
        write(unit=self%unit,iostat=self%io_status) value
      end if
      call broadcast_(tonto_parallel,self%io_status,0)
      call ensure_(tonto,self%io_status==0,"FILE:write_bin ... write error, error="// trim(to_str_(self%io_status)))
      self%record = self%record + 1

   end subroutine

   subroutine write_vec(self,vec)
    type(file_type) :: self
    ! Write a vector to the file as a single record
      real(kind=kind(1.0d0)), dimension(:) :: vec

      if (self%buffered) then
         call write_buffered_vec_(self,vec)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) vec
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_vec ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_intvec(self,vec)
    type(file_type) :: self
    ! Write a vector to the file as a single record
      integer(kind=kind(1)), dimension(:), intent(in) :: vec

      if (self%buffered) then
         call write_buffered_intvec_(self,vec)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) vec
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_intvec ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_cpxvec(self,vec)
    type(file_type) :: self
    ! Write a vector to the file as a single record
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: vec

      if (self%buffered) then
         call write_buffered_cpxvec_(self,vec)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) vec
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_cpxvec ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_intmat(self,mat)
    type(file_type) :: self
    ! Write a matrix to the file as a single record
      integer(kind=kind(1)), dimension(:,:) :: mat

      if (self%buffered) then
         call write_buffered_intmat_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_intmat ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_mat(self,mat)
    type(file_type) :: self
    ! Write a matrix to the file as a single record
      real(kind=kind(1.0d0)), dimension(:,:) :: mat

      if (self%buffered) then
         call write_buffered_mat_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_mat ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_mat3(self,mat)
    type(file_type) :: self
    ! Write a matrix to the file as a single record
      real(kind=kind(1.0d0)), dimension(:,:,:) :: mat

      if (self%buffered) then
         call write_buffered_mat3_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_mat3 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_mat4(self,mat)
    type(file_type) :: self
    ! Write a matrix to the file as a single record
      real(kind=kind(1.0d0)), dimension(:,:,:,:) :: mat

      if (self%buffered) then
        call write_buffered_mat4_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_mat4 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_cpxmat(self,mat)
    type(file_type) :: self
    ! Write a complex matrix to the file as a single record
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: mat

      if (self%buffered) then
         call write_buffered_cpxmat_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_cpxmat ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_cpxmat3(self,mat)
    type(file_type) :: self
    ! Write a complex 3d matrix to the file as a single record
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: mat

      if (self%buffered) then
         call write_buffered_cpxmat3_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_cpxmat3 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_cpxmat4(self,mat)
    type(file_type) :: self
    ! Write a complex 4d matrix to the file as a single record
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: mat

      if (self%buffered) then
         call write_buffered_cpxmat4_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_cpxmat4 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

   subroutine write_cpxmat5(self,mat)
    type(file_type) :: self
    ! Write a complex 5d matrix to the file as a single record
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: mat

      if (self%buffered) then
         call write_buffered_cpxmat5_(self,mat)
      else
        if (do_io_(tonto_parallel)) then
          write(unit=self%unit,iostat=self%io_status) mat
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        call ensure_(tonto,self%io_status==0,"FILE:write_cpxmat5 ... write error, error="// trim(to_str_(self%io_status)))
        self%record = self%record + 1
      end if

   end subroutine

!  **********************************
!  File repositioning type operations
!  **********************************

  subroutine rewind(self)
    type(file_type) :: self
   ! Rewind the file

    if (do_io_(tonto_parallel)) then
      rewind(unit=self%unit,iostat=self%io_status)
    end if
    call broadcast_(tonto_parallel,self%io_status,0)
    call ensure_(tonto,self%io_status==0,"FILE:rewind ... rewind error, error="// trim(to_str_(self%io_status)))
    self%record = 1

  end subroutine

  subroutine backspace(self)
    type(file_type) :: self
   ! Backspace the file one record of the file. Backspacing before record 1 has
   ! no effect.

    if (do_io_(tonto_parallel)) then
      backspace(unit=self%unit,iostat=self%io_status)
    end if
    call broadcast_(tonto_parallel,self%io_status,0)
    call ensure_(tonto,self%io_status==0,"FILE:backspace ... backspace error, error="// trim(to_str_(self%io_status)))
    self%record = max(1,self%record-1)

  end subroutine

   subroutine skip(self)
    type(file_type) :: self
    ! Skip over the next record of the file. An error is generated if at the end
    ! of the file

     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status)
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call ensure_(tonto,self%io_status==0,"FILE:skip ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1

   end subroutine

   subroutine move_to_end(self)
    type(file_type) :: self
    ! Move to the end of the file, say for adding more data

      do
        if (do_io_(tonto_parallel)) then
          read(unit=self%unit,iostat=self%io_status)
        end if
        call broadcast_(tonto_parallel,self%io_status,0)
        if (self%io_status/=0) exit
        self%record = self%record + 1
      end do

   end subroutine

   subroutine move_to_position(self,pos)
    type(file_type) :: self
    ! Move to a particular record position in the file
      integer(kind=kind(1)) :: pos

      call move_to_record_(self,pos)

   end subroutine

   subroutine move_to_record(self,rec)
    type(file_type) :: self
    ! Move to a particular record position in the file
      integer(kind=kind(1)) :: rec

      call ensure_(tonto,rec>=1,"FILE:move_to_record ... can't move to non-positive record")
      if (rec<position_(self)) then
         do
            call backspace_(self)
            if (position_(self)==rec) exit
         end do
      else if (rec>position_(self)) then
         do
            call skip_(self)
            if (position_(self)==rec) exit
         end do
      end if

   end subroutine

!  ***************
!  Inquiry methods
!  ***************

   function exists(self,name) result(res)
    type(file_type) :: self
    ! Returns true if the file exists on the file system.  If present, "name" is
    ! used, otherwise .name is used.
      logical(kind=kind(.true.)) :: res
      character(*), intent(in), optional :: name

      if (present(name)) then
        if (do_io_(tonto_parallel)) then
          inquire(file=name,exist=res)
        end if
      else
        if (do_io_(tonto_parallel)) then
          inquire(file=self%name,exist=res)
        end if
      end if
      call broadcast_(tonto_parallel,res,0)

   end function

   function is_open(self) result(res)
    type(file_type) :: self
    ! Returns true if the file has been opened
      logical(kind=kind(.true.)) :: res
       ! inquire(unit=.unit,opened=res)

      if (do_io_(tonto_parallel)) then
        inquire(file=self%name,opened=res)
      end if
      call broadcast_(tonto_parallel,res,0)

   end function

   function unit_used(self) result(res)
    type(file_type) :: self
    ! Returns true if the file unit is in use
      logical(kind=kind(.true.)) :: res

      if (do_io_(tonto_parallel)) then
        inquire(unit=self%unit,opened=res)
      end if
      call broadcast_(tonto_parallel,res,0)

   end function

!   created result(res)
!   ! Returns true if the file object has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if the file object has *not* been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   function position(self) result(res)
    type(file_type) :: self
    ! Return record position of the file
      integer(kind=kind(1)) :: res

      res = self%record

   end function

   function is_for_reading(self) result(res)
    type(file_type) :: self
    ! Return whether the file is opened for reading.
     logical(kind=kind(.true.)) :: res
     character(5) :: reading

     if (do_io_(tonto_parallel)) then
       inquire(unit=self%unit,read=reading)
     end if
     call broadcast_(tonto_parallel,reading,0)
     if (trim(reading) == "YES") then
       res = .true.
     else
       res = .false.
     end if

   end function

!   is_real_buffered result(res)
!   ! Returns true if the file is using real buffering
!      res :: logical(kind=kind(.true.))
!      res = associated(.real_buffer)
!   end

!   is_int_buffered result(res)
!   ! Returns true if the file is using integer(kind=kind(1)) buffering
!      res :: logical(kind=kind(.true.))
!      res = associated(.int_buffer)
!   end

!***************************************************
! Buffered routines
!***************************************************

   subroutine flush_real_buffer(self)
    type(file_type) :: self
    ! Writes the real_buffer to disk, and positions the pointer at the start of
    ! the buffer.

     if (do_io_(tonto_parallel)) then
       write(unit=self%unit,iostat=self%io_status) self%real_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call ensure_(tonto,self%io_status==0,"FILE:flush_real_buffer ... write error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%real_buffer_pos = 1

   end subroutine

   subroutine flush_int_buffer(self)
    type(file_type) :: self
    ! Writes the int_buffer to disk, and positions the pointer at the start of
    ! the buffer.

     if (do_io_(tonto_parallel)) then
       write(unit=self%unit,iostat=self%io_status) self%int_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call ensure_(tonto,self%io_status==0,"FILE:flush_int_buffer ... write error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%int_buffer_pos = 1

   end subroutine

   subroutine flush_cpx_buffer(self)
    type(file_type) :: self
    ! Writes the cpx_buffer to disk, and positions the pointer at the start of
    ! the buffer.

     if (do_io_(tonto_parallel)) then
       write(unit=self%unit,iostat=self%io_status) self%cpx_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call ensure_(tonto,self%io_status==0,"FILE:flush_cpx_buffer ... write error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%cpx_buffer_pos = 1

   end subroutine

   subroutine get_real_buffer(self)
    type(file_type) :: self
    ! Reads the real_buffer from disk, and positions the pointer at the start of
    ! the buffer.

     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) self%real_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,self%real_buffer,0)
     call ensure_(tonto,self%io_status==0,"FILE:get_real_buffer ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%real_buffer_pos = 1

   end subroutine

   subroutine get_int_buffer(self)
    type(file_type) :: self
    ! Reads the int_buffer from disk, and positions the pointer at the start of
    ! the buffer.

     call ensure_(tonto,associated(self%int_buffer),"FILE:get_int_buffer ... buffer not created")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) self%int_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,self%int_buffer,0)
     call ensure_(tonto,self%io_status==0,"FILE:get_int_buffer ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%int_buffer_pos = 1

   end subroutine

   subroutine get_cpx_buffer(self)
    type(file_type) :: self
    ! Reads the cpx_buffer from disk, and positions the pointer at the start of
    ! the buffer.

     call ensure_(tonto,associated(self%cpx_buffer),"FILE:get_cpx_buffer ... buffer not created")
     if (do_io_(tonto_parallel)) then
       read(unit=self%unit,iostat=self%io_status) self%cpx_buffer
     end if
     call broadcast_(tonto_parallel,self%io_status,0)
     call broadcast_(tonto_parallel,self%cpx_buffer,0)
     call ensure_(tonto,self%io_status==0,"FILE:get_cpx_buffer ... read error, error="// trim(to_str_(self%io_status)))
     self%record = self%record + 1
     self%cpx_buffer_pos = 1

   end subroutine

   subroutine write_buffered_real(self,the_real)
    type(file_type) :: self
    ! Writes "the_real" to the buffer, and writes buffer to disk when full.
     real(kind=kind(1.0d0)), intent(in) :: the_real

     call ensure_(tonto,associated(self%real_buffer),"FILE:write_buffered_real ... buffer not created")
     self%real_buffer( self%real_buffer_pos ) = the_real
     self%real_buffer_pos = self%real_buffer_pos + 1
     if ( self%real_buffer_pos > 1024) call flush_real_buffer_(self)

   end subroutine

   subroutine read_buffered_real(self,the_real)
    type(file_type) :: self
    ! Reads "the_real" from the buffer, and reads buffer from disk when empty.
     real(kind=kind(1.0d0)), intent(out) :: the_real

     if ( self%real_buffer_pos > 1024) call get_real_buffer_(self)
     the_real = self%real_buffer( self%real_buffer_pos )
     self%real_buffer_pos = self%real_buffer_pos + 1

   end subroutine

   subroutine write_buffered_int(self,the_int)
    type(file_type) :: self
    ! Writes "the_int" to the buffer, and writes buffer to disk when full.
     integer(kind=kind(1)), intent(in) :: the_int

     self%int_buffer( self%int_buffer_pos ) = the_int
     self%int_buffer_pos = self%int_buffer_pos + 1
     if ( self%int_buffer_pos > 1024) call flush_int_buffer_(self)

   end subroutine

   subroutine read_buffered_int(self,the_int)
    type(file_type) :: self
    ! Reads the integer from the buffer, and reads buffer from disk when empty.
     integer(kind=kind(1)), intent(out) :: the_int

     if ( self%int_buffer_pos > 1024) call get_int_buffer_(self)
     the_int = self%int_buffer( self%int_buffer_pos )
     self%int_buffer_pos = self%int_buffer_pos + 1

   end subroutine

   subroutine write_buffered_cpx(self,the_cpx)
    type(file_type) :: self
    ! Writes "the_cpx" to the buffer, and writes buffer to disk when full.
     complex(kind=kind((1.0d0,1.0d0))), intent(in) :: the_cpx

     call ensure_(tonto,associated(self%cpx_buffer),"FILE:write_buffered_cpx ... buffer not created")
     self%cpx_buffer( self%cpx_buffer_pos ) = the_cpx
     self%cpx_buffer_pos = self%cpx_buffer_pos + 1
     if ( self%cpx_buffer_pos > 1024) call flush_cpx_buffer_(self)

   end subroutine

   subroutine read_buffered_cpx(self,the_cpx)
    type(file_type) :: self
    ! Reads "the_cpx" from the buffer, and reads buffer from disk when empty.
     complex(kind=kind((1.0d0,1.0d0))), intent(out) :: the_cpx

     if ( self%cpx_buffer_pos > 1024) call get_cpx_buffer_(self)
     the_cpx = self%cpx_buffer( self%cpx_buffer_pos )
     self%cpx_buffer_pos = self%cpx_buffer_pos + 1

   end subroutine

   subroutine write_buffered_vec(self,the_vec)
    type(file_type) :: self
    ! Writes the vector to the buffer, and writes buffer to disk when full.
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: the_vec
     integer(kind=kind(1)) :: vec_length,vec_pos,buffer_top,vec_top,put_length

     vec_length = size(the_vec)
     vec_pos=1
     do
       put_length=min(1024 - self%real_buffer_pos + 1,vec_length-vec_pos+1)
       buffer_top = self%real_buffer_pos + put_length - 1
       vec_top = vec_pos + put_length - 1
       self%real_buffer( self%real_buffer_pos:buffer_top) = the_vec(vec_pos:vec_top)
       self%real_buffer_pos = buffer_top + 1
       vec_pos = vec_top + 1
       if ( self%real_buffer_pos > 1024) call flush_real_buffer_(self)
       if (vec_pos > vec_length) exit
     end do

   end subroutine

   subroutine read_buffered_vec(self,the_vec)
    type(file_type) :: self
    ! Reads the vector from the buffer, and reads buffer from disk when empty.
     real(kind=kind(1.0d0)), dimension(:), intent(out) :: the_vec
     integer(kind=kind(1)) :: vec_length,vec_pos,buffer_top,vec_top,get_length

     vec_length = size(the_vec)
     vec_pos=1
     do
       if ( self%real_buffer_pos > 1024) call get_real_buffer_(self)
       get_length=min(1024 - self%real_buffer_pos + 1,vec_length-vec_pos+1)
       buffer_top = self%real_buffer_pos + get_length - 1
       vec_top = vec_pos + get_length - 1
       the_vec(vec_pos:vec_top) = self%real_buffer( self%real_buffer_pos:buffer_top)
       self%real_buffer_pos = buffer_top + 1
       vec_pos = vec_top + 1
       if (vec_pos > vec_length) exit
     end do

   end subroutine

   subroutine write_buffered_intvec(self,the_intvec)
    type(file_type) :: self
    ! Writes the intvector to the buffer, and writes buffer to disk when full.
     integer(kind=kind(1)), dimension(:), intent(in) :: the_intvec
     integer(kind=kind(1)) :: intvec_length,intvec_pos,buffer_top,intvec_top,put_length

     intvec_length = size(the_intvec)
     intvec_pos=1
     do
       put_length=min(1024 - self%int_buffer_pos + 1,intvec_length-intvec_pos+1)
       buffer_top = self%int_buffer_pos + put_length - 1
       intvec_top = intvec_pos + put_length - 1
       self%int_buffer( self%int_buffer_pos:buffer_top) = the_intvec(intvec_pos:intvec_top)
       self%int_buffer_pos = buffer_top + 1
       intvec_pos = intvec_top + 1
       if ( self%int_buffer_pos > 1024) call flush_int_buffer_(self)
       if (intvec_pos > intvec_length) exit
     end do

   end subroutine

   subroutine read_buffered_intvec(self,the_intvec)
    type(file_type) :: self
    ! Reads the intvector from the buffer, and reads buffer from disk when empty.
     integer(kind=kind(1)), dimension(:), intent(out) :: the_intvec
     integer(kind=kind(1)) :: intvec_length,intvec_pos,buffer_top,intvec_top,get_length

     intvec_length = size(the_intvec)
     intvec_pos=1
     do
       if ( self%int_buffer_pos > 1024) call get_int_buffer_(self)
       get_length=min(1024 - self%int_buffer_pos + 1,intvec_length-intvec_pos+1)
       buffer_top = self%int_buffer_pos + get_length - 1
       intvec_top = intvec_pos + get_length - 1
       the_intvec(intvec_pos:intvec_top) = self%int_buffer( self%int_buffer_pos:buffer_top)
       self%int_buffer_pos = buffer_top + 1
       intvec_pos = intvec_top + 1
       if (intvec_pos > intvec_length) exit
     end do

   end subroutine

   subroutine write_buffered_cpxvec(self,the_cpxvec)
    type(file_type) :: self
    ! Writes the cvector to the buffer, and writes buffer to disk when full.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(in) :: the_cpxvec
     integer(kind=kind(1)) :: cpxvec_length,cpxvec_pos,buffer_top,cpxvec_top,put_length

     cpxvec_length = size(the_cpxvec)
     cpxvec_pos=1
     do
       put_length=min(1024 - self%cpx_buffer_pos + 1,cpxvec_length-cpxvec_pos+1)
       buffer_top = self%cpx_buffer_pos + put_length - 1
       cpxvec_top = cpxvec_pos + put_length - 1
       self%cpx_buffer( self%cpx_buffer_pos:buffer_top) = the_cpxvec(cpxvec_pos:cpxvec_top)
       self%cpx_buffer_pos = buffer_top + 1
       cpxvec_pos = cpxvec_top + 1
       if ( self%cpx_buffer_pos > 1024) call flush_cpx_buffer_(self)
       if (cpxvec_pos > cpxvec_length) exit
     end do

   end subroutine

   subroutine read_buffered_cpxvec(self,the_cpxvec)
    type(file_type) :: self
    ! Reads the cvector from the buffer, and reads buffer from disk when empty.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:), intent(out) :: the_cpxvec
     integer(kind=kind(1)) :: cpxvec_length,cpxvec_pos,buffer_top,cpxvec_top,get_length

     cpxvec_length = size(the_cpxvec)
     cpxvec_pos=1
     do
       if ( self%cpx_buffer_pos > 1024) call get_cpx_buffer_(self)
       get_length=min(1024 - self%cpx_buffer_pos + 1,cpxvec_length-cpxvec_pos+1)
       buffer_top = self%cpx_buffer_pos + get_length - 1
       cpxvec_top = cpxvec_pos + get_length - 1
       the_cpxvec(cpxvec_pos:cpxvec_top) = self%cpx_buffer( self%cpx_buffer_pos:buffer_top)
       self%cpx_buffer_pos = buffer_top + 1
       cpxvec_pos = cpxvec_top + 1
       if (cpxvec_pos > cpxvec_length) exit
     end do

   end subroutine

   subroutine write_buffered_mat(self,mat)
    type(file_type) :: self
    ! Writes the mat to the buffer, and writes buffer to disk when full.
     real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: mat
     integer(kind=kind(1)) :: b,bsize

     bsize=size(mat,2)
     do b=1,bsize
       call write_buffered_vec_(self,mat(:,b))
     end do

   end subroutine

   subroutine read_buffered_mat(self,mat)
    type(file_type) :: self
    ! Reads the mat from the buffer.
     real(kind=kind(1.0d0)), dimension(:,:), intent(out) :: mat
     integer(kind=kind(1)) :: b,bsize

     bsize=size(mat,2)
     do b=1,bsize
       call read_buffered_vec_(self,mat(:,b))
     end do

   end subroutine

   subroutine write_buffered_intmat(self,mat)
    type(file_type) :: self
    ! Writes the intmat to the buffer, and writes buffer to disk when full.
     integer(kind=kind(1)), dimension(:,:), intent(in) :: mat
     integer(kind=kind(1)) :: a,b,asize,bsize

     asize=size(mat,1);    bsize=size(mat,2)
     do a=1,asize
       do b=1,bsize
         call write_buffered_int_(self,mat(a,b))
       end do
     end do

   end subroutine

   subroutine read_buffered_intmat(self,mat)
    type(file_type) :: self
    ! Reads the intmat from the buffer.
     integer(kind=kind(1)), dimension(:,:), intent(out) :: mat
     integer(kind=kind(1)) :: a,b,asize,bsize

     asize=size(mat,1);    bsize=size(mat,2)
     do a=1,asize
       do b=1,bsize
         call read_buffered_int_(self,mat(a,b))
       end do
     end do

   end subroutine

   subroutine write_buffered_cpxmat(self,mat)
    type(file_type) :: self
    ! Writes the cpxmat to the buffer, and writes buffer to disk when full.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(in) :: mat
     integer(kind=kind(1)) :: b,bsize

     bsize=size(mat,2)
     do b=1,bsize
       call write_buffered_cpxvec_(self,mat(:,b))
     end do

   end subroutine

   subroutine read_buffered_cpxmat(self,mat)
    type(file_type) :: self
    ! Reads the cpxmat from the buffer.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), intent(out) :: mat
     integer(kind=kind(1)) :: b,bsize

     bsize=size(mat,2)
     do b=1,bsize
       call read_buffered_cpxvec_(self,mat(:,b))
     end do

   end subroutine

   subroutine write_buffered_mat3(self,mat)
    type(file_type) :: self
    ! Writes the mat3 to the buffer, and writes buffer to disk when full.
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(in) :: mat
     integer(kind=kind(1)) :: b,c,bsize,csize

     bsize=size(mat,2);     csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call write_buffered_vec_(self,mat(:,b,c))
       end do
     end do

   end subroutine

   subroutine read_buffered_mat3(self,mat)
    type(file_type) :: self
    ! Reads the mat3 from the buffer.
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(out) :: mat
     integer(kind=kind(1)) :: b,c,bsize,csize

     bsize=size(mat,2);     csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call read_buffered_vec_(self,mat(:,b,c))
       end do
     end do

   end subroutine

   subroutine write_buffered_cpxmat3(self,mat)
    type(file_type) :: self
    ! Writes the cpxmat3 to the buffer, and writes buffer to disk when full.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), intent(in) :: mat
     integer(kind=kind(1)) :: b,c,bsize,csize

     bsize=size(mat,2);    csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call write_buffered_cpxvec_(self,mat(:,b,c))
       end do
     end do

   end subroutine

   subroutine read_buffered_cpxmat3(self,mat)
    type(file_type) :: self
    ! Reads the cpxmat3 from the buffer.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:), intent(out) :: mat
     integer(kind=kind(1)) :: b,c,bsize,csize

     bsize=size(mat,2);     csize=size(mat,3)
     do c=1,csize
       do b=1,bsize
         call read_buffered_cpxvec_(self,mat(:,b,c))
       end do
     end do

   end subroutine

   subroutine write_buffered_mat4(self,mat)
    type(file_type) :: self
    ! Writes the mat4 to the buffer, and writes buffer to disk when full.
     real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(in) :: mat
     integer(kind=kind(1)) :: b,c,d,bsize,csize,dsize

     bsize=size(mat,2);     csize=size(mat,3);    dsize=size(mat,4)
     do d=1,dsize
       do c=1,csize
         do b=1,bsize
           call write_buffered_vec_(self,mat(:,b,c,d))
         end do
       end do
     end do

   end subroutine

   subroutine read_buffered_mat4(self,mat)
    type(file_type) :: self
    ! Reads the mat4 from the buffer.
     real(kind=kind(1.0d0)), dimension(:,:,:,:), intent(out) :: mat
     integer(kind=kind(1)) :: b,c,d,bsize,csize,dsize

     bsize=size(mat,2);     csize=size(mat,3);    dsize=size(mat,4)
     do d=1,dsize
       do c=1,csize
         do b=1,bsize
           call read_buffered_vec_(self,mat(:,b,c,d))
         end do
       end do
     end do

   end subroutine

   subroutine write_buffered_cpxmat4(self,mat)
    type(file_type) :: self
    ! Writes the mat4 to the buffer, and writes buffer to disk when full.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), intent(in) :: mat
     integer(kind=kind(1)) :: a,b,c,d,asize,bsize,csize,dsize

     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             call write_buffered_cpx_(self,mat(a,b,c,d))
           end do
         end do
       end do
     end do

   end subroutine

   subroutine read_buffered_cpxmat4(self,mat)
    type(file_type) :: self
    ! Reads the mat4 from the buffer.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:), intent(out) :: mat
     integer(kind=kind(1)) :: a,b,c,d,asize,bsize,csize,dsize

     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             call read_buffered_cpx_(self,mat(a,b,c,d))
           end do
         end do
       end do
     end do

   end subroutine

   subroutine write_buffered_cpxmat5(self,mat)
    type(file_type) :: self
    ! Writes the mat5 to the buffer, and writes buffer to disk when full.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:), intent(in) :: mat
     integer(kind=kind(1)) :: a,b,c,d,e,asize,bsize,csize,dsize,esize

     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     esize=size(mat,5)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             do e=1,esize
               call write_buffered_cpx_(self,mat(a,b,c,d,e))
             end do
           end do
         end do
       end do
     end do

   end subroutine

   subroutine read_buffered_cpxmat5(self,mat)
    type(file_type) :: self
    ! Reads the mat5 from the buffer.
     complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:), intent(out) :: mat
     integer(kind=kind(1)) :: a,b,c,d,e,asize,bsize,csize,dsize,esize

     asize=size(mat,1);    bsize=size(mat,2)
     csize=size(mat,3);    dsize=size(mat,4)
     esize=size(mat,5)
     do a=1,asize
       do b=1,bsize
         do c=1,csize
           do d=1,dsize
             do e=1,esize
               call read_buffered_cpx_(self,mat(a,b,c,d,e))
             end do
           end do
         end do
       end do
     end do

   end subroutine

end
