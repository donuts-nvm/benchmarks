!---------------------------------------------------------------------------
!
!  DIIS:  DIIS Extrapolation technique for vectors
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
! $Id: diis.foo,v 1.2.2.4 2003/10/13 04:13:10 reaper Exp $
!---------------------------------------------------------------------------

module DIIS_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use OPMATRIX_MODULE, only: uncompress_
   use OPMATRIX_MODULE, only: compress_
   use OPMATRIX_MODULE, only: number_kind_

   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: index_of_largest_value_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: to_str_

   use ARCHIVE_MODULE, only: set_format_
   use ARCHIVE_MODULE, only: set_root_name_
   use ARCHIVE_MODULE, only: set_name_
   use ARCHIVE_MODULE, only: copy_
   use ARCHIVE_MODULE, only: file_name_
   use ARCHIVE_MODULE, only: destroy_ptr_part_
   use ARCHIVE_MODULE, only: set_defaults_
   use ARCHIVE_MODULE, only: read_
   use ARCHIVE_MODULE, only: nullify_ptr_part_
   use ARCHIVE_MODULE, only: delete_
   use ARCHIVE_MODULE, only: exists_
   use ARCHIVE_MODULE, only: write_
   use ARCHIVE_MODULE, only: set_

   use REAL_MODULE, only: times_

   use REALMAT_MODULE, only: solve_linear_equation_
   use REALMAT_MODULE, only: get_diagonal_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: create_copy_
   use REALMAT_MODULE, only: symmetric_reflect_
   use REALMAT_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: text_
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

   private    get_old_diis_matrix_
   interface get_old_diis_matrix_
      module procedure get_old_diis_matrix
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   private    save_diis_mat_
   interface save_diis_mat_
      module procedure save_diis_mat
   end interface

   public    set_archive_root_name_
   interface set_archive_root_name_
      module procedure set_archive_root_name
   end interface

   public    get_error_item_
   interface get_error_item_
      module procedure get_error_item
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    save_pair_
   interface save_pair_
      module procedure save_pair
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    dimension_
   interface dimension_
      module procedure dimension
   end interface

   public    set_archive_format_
   interface set_archive_format_
      module procedure set_archive_format
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    remake_diis_matrix_
   interface remake_diis_matrix_
      module procedure remake_diis_matrix
   end interface

   public    extrapolate_
   interface extrapolate_
      module procedure extrapolate
      module procedure extrapolate_1
   end interface

   private    make_diis_matrix_
   interface make_diis_matrix_
      module procedure make_diis_matrix
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    cleanup_
   interface cleanup_
      module procedure cleanup
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    set_keep_
   interface set_keep_
      module procedure set_keep
   end interface

   private    next_replacement_
   interface next_replacement_
      module procedure next_replacement
   end interface

   public    get_item_
   interface get_item_
      module procedure get_item
      module procedure get_item_1
      module procedure get_item_2
   end interface

   public    delete_archives_
   interface delete_archives_
      module procedure delete_archives
   end interface

   public    set_archive_name_
   interface set_archive_name_
      module procedure set_archive_name
   end interface

   private    get_diis_mat_
   interface get_diis_mat_
      module procedure get_diis_mat
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   private    worst_item_
   interface worst_item_
      module procedure worst_item
   end interface

   private    update_
   interface update_
      module procedure update
   end interface

   public    get_parameter_item_
   interface get_parameter_item_
      module procedure get_parameter_item
   end interface

   public    save_item_
   interface save_item_
      module procedure save_item
      module procedure save_item_1
      module procedure save_item_2
   end interface

contains

   subroutine create(self,root_name,name,diis_kind,format,keep)
    type(diis_type) :: self
    ! Create the type(diis_type) object, but no archive name
      pointer :: self
      character(128), optional :: root_name,name
      character(128), optional :: diis_kind,format
      integer(kind=kind(1)), intent(in), optional :: keep

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)
      call set_(self%archive,root_name,name,diis_kind,format)
      call set_keep_(self,keep)

   end subroutine

   subroutine destroy(self)
    type(diis_type) :: self
    ! Destroy the type(diis_type) object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call delete_archives_(self)
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   subroutine nullify_ptr_part(self)
    type(diis_type) :: self
    ! Nullify the pointer parts of self

      nullify(self%coeff)
      call nullify_ptr_part_(self%archive)
      nullify(self%error_items)
      nullify(self%parameter_items)
      nullify(self%diis_matrix)
      nullify(self%constraint_matrix)
      nullify(self%density_matrix)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(diis_type) :: self
    ! Destroy the pointer parts of self

      call destroy_(self%coeff)
      if (self%in_core) then
        call destroy_(self%error_items)
        call destroy_(self%parameter_items)
        call destroy_(self%diis_matrix)
        call destroy_(self%constraint_matrix)
        call destroy_(self%density_matrix)
      end if
      call destroy_ptr_part_(self%archive)

   end subroutine

   subroutine create_copy(self,diis)
    type(diis_type) :: self
    ! Make a copy of the diis object
      pointer :: self
      type(diis_type) :: diis

      call create_(self)
      call copy_(self,diis)

   end subroutine

   subroutine copy(self,diis)
    type(diis_type) :: self
    ! Make a copy of the diis object
      type(diis_type) :: diis

      self = diis
      call nullify_ptr_part_(self)
      if (associated(diis%error_items)) call create_copy_(self%error_items,diis%error_items)
      if (associated(diis%parameter_items)) call create_copy_(self%parameter_items,diis%parameter_items)
      if (associated(diis%diis_matrix)) call create_copy_(self%diis_matrix,diis%diis_matrix)
      if (associated(diis%constraint_matrix)) call create_copy_(self%constraint_matrix,diis%constraint_matrix)
      if (associated(diis%density_matrix)) call create_copy_(self%density_matrix,diis%density_matrix)
      call copy_(self%archive,diis%archive)
      if (associated(diis%coeff)) call create_copy_(self%coeff,diis%coeff)

   end subroutine

   subroutine delete_archives(self)
    type(diis_type) :: self
    ! Delete the archive files on disk.
     integer(kind=kind(1)) :: i
     character(128) :: name

     call destroy_(self%error_items)
     call destroy_(self%parameter_items)
     call destroy_(self%diis_matrix)
     call destroy_(self%constraint_matrix)
     call destroy_(self%density_matrix)
     if (.not. self%in_core) then
       name = self%archive%name
       call set_name_(self%archive, trim(name) // ",mat" )
       if (exists_(self%archive))  call delete_(self%archive)
       do i=1, self%n_vec
         call set_name_(self%archive,trim(name) // ",p" // trim(to_str_(i)))
         if (exists_(self%archive))  call delete_(self%archive)
         call set_name_(self%archive,trim(name) // ",e" // trim(to_str_(i)))
         if (exists_(self%archive))  call delete_(self%archive)
       end do
       call set_name_(self%archive,trim(name))
     end if
     self%new=0
     self%n_vec = 0
     self%error_length = 0.0d0

   end subroutine

   subroutine cleanup(self)
    type(diis_type) :: self
    ! Cleanup the type(diis_type) files and matrices, but don't resort back to default
    ! parameters.

     call delete_archives_(self)
     call destroy_ptr_part_(self)
     self%n_vec = 0
     self%new = 0
     self%error_length = 0.0d0
     call destroy_(self%coeff)

   end subroutine

   subroutine set_defaults(self)
    type(diis_type) :: self
    ! Set up the default settings

      self%keep  = 8
      self%n_vec = 0
      self%new = 0
      self%error_length = 0.0d0
      call set_defaults_(self%archive)
      call destroy_ptr_part_(self)  ! ensure ptr parts are nullified first !
      self%in_core = .true.

   end subroutine

   subroutine set_keep(self,keep)
    type(diis_type) :: self
    ! Set the number of type(diis_type) objects to keep
      integer(kind=kind(1)), intent(in) :: keep

      self%keep = keep

   end subroutine

   subroutine set_archive_root_name(self,root_name)
    type(diis_type) :: self
    ! Set the type(diis_type) archive to have root name "root_name".
      character(*) :: root_name

      call set_root_name_(self%archive,root_name)

   end subroutine

   subroutine set_archive_name(self,name)
    type(diis_type) :: self
    ! Set the type(diis_type) archive name.
      character(*) :: name

      call set_name_(self%archive,name)

   end subroutine

   subroutine set_archive_format(self,format)
    type(diis_type) :: self
    ! Set the type(diis_type) archive format.
      character(*) :: format

      call set_format_(self%archive,format)

   end subroutine

!*************************************************************************

   function worst_item(self) result(res)
    type(diis_type) :: self
    ! Return the index of the item with the worst error in the type(diis_type) archive
      integer(kind=kind(1)) :: res
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: mat1
      real(kind=kind(1.0d0)), dimension(:), pointer :: diag
      integer(kind=kind(1)) :: dim

      dim = dimension_(self)
      call create_(diag,dim)
      call create_(mat1,dim,dim)
      call get_old_diis_matrix_(self,mat1)
      call get_diagonal_(mat1,diag)              ! the error vector lengths^2
      res = index_of_largest_value_(diag)    ! Worst item has the largest error
      call destroy_(mat1)
      call destroy_(diag)

   end function

   function next_replacement(self) result(res)
    type(diis_type) :: self
    ! Return the index of the next item in the type(diis_type) expansion
    ! which is to be replaced/saved
      integer(kind=kind(1)) :: res

      if (self%n_vec<=self%keep) then
         res = self%n_vec
      else
         res = worst_item_(self)
      end if

   end function

   subroutine save_pair(self,par,err,item)
    type(diis_type) :: self
    ! Save/replace the current vectors on an archive with item number "item",
    ! or item number ".new" if "item" is not present.
      real(kind=kind(1.0d0)), dimension(:) :: par,err
      integer(kind=kind(1)), optional :: item
      integer(kind=kind(1)) :: i

      i = self%new
      if (present(item)) i = item
      call save_item_(self,err,"e",i)
      call save_item_(self,par,"p",i)

   end subroutine

   subroutine save_item(self,mat,name,i)
    type(diis_type) :: self
    ! Save "mat" with "name" and number "i" to disk.
     real(kind=kind(1.0d0)), dimension(:,:) :: mat
     character(*) :: name
     integer(kind=kind(1)), optional, intent(in) :: i
     character(128) :: old_name

     if (self%in_core) then
       select case(name)
         case ("mat")
           call destroy_(self%diis_matrix)
           call create_(self%diis_matrix,size(mat,1),size(mat,2))
           self%diis_matrix(:,:) = mat
         case default
           call die_(tonto,"DIIS:save_item ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call write_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if

   end subroutine

   subroutine save_item_1(self,vec,name,i)
    type(diis_type) :: self
    ! Save "mat" with "name" and number "i" to disk.
     real(kind=kind(1.0d0)), dimension(:) :: vec
     character(*) :: name
     integer(kind=kind(1)), optional, intent(in) :: i
     integer(kind=kind(1)) :: ii
     character(128) :: old_name

     ii = 1
     if (present(i)) ii=i
     if (self%in_core) then
       select case(name)
         case ("p")
           if (.not. associated(self%parameter_items)) call create_(self%parameter_items,size(vec),self%keep)
           self%parameter_items(:,ii) = vec
         case ("e")
           if (.not. associated(self%error_items)) call create_(self%error_items,size(vec),self%keep)
           self%error_items(:,ii) = vec
         case ("constraint")
           if (.not. associated(self%constraint_matrix)) call create_(self%constraint_matrix,size(vec),self%keep)
           self%constraint_matrix(:,ii) = vec
         case default
           call die_(tonto,"DIIS:save_item_1 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call write_(self%archive,vec)
       call set_name_(self%archive,old_name)
     end if

   end subroutine

   subroutine save_item_2(self,mat,name,i)
    type(diis_type) :: self
    ! Save "mat" with "name" and number "i" to disk.
     type(opmatrix_type) :: mat
     character(*) :: name
     integer(kind=kind(1)), optional, intent(in) :: i
     integer(kind=kind(1)) :: ii
     character(128) :: old_name

     ii = 1
     if (self%in_core) then
       select case(name)
         case ("density")
           select case (number_kind_(mat))
             case ("real")
               call compress_(mat)
               if (.not. associated(self%density_matrix))&
                 call create_(self%density_matrix,size(mat%triangle),self%keep)
               self%density_matrix(:,ii) = mat%triangle
               call uncompress_(mat)
             case ("complex")
               call compress_(mat)
               if (.not. associated(self%density_matrix)) &
                 call create_(self%density_matrix,size(mat%square),self%keep)
               self%density_matrix(:,ii) = mat%square
               call uncompress_(mat)
           end select
         case default
           call die_(tonto,"DIIS:save_item_2 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call write_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if

   end subroutine

   subroutine get_item(self,mat,name,i)
    type(diis_type) :: self
    ! Read "mat" with "name" and number "i" to disk.
     real(kind=kind(1.0d0)), dimension(:,:) :: mat
     character(*) :: name
     integer(kind=kind(1)), optional, intent(in) :: i
     character(128) :: old_name

     if (self%in_core) then
       select case(name)
         case ("mat")
           mat = self%diis_matrix(:,:)
         case default
           call die_(tonto,"DIIS:get_item ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call read_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if

   end subroutine

   subroutine get_item_1(self,vec,name,i)
    type(diis_type) :: self
    ! Read "mat" with "name" and number "i" to disk.
     real(kind=kind(1.0d0)), dimension(:) :: vec
     character(*) :: name
     integer(kind=kind(1)), optional, intent(in) :: i
     character(128) :: old_name
     integer(kind=kind(1)) :: ii

     ii = 1
     if (present(i)) ii=i
     if (self%in_core) then
       select case(name)
         case ("p")
           vec = self%parameter_items(:,ii)
         case ("e")
           vec = self%error_items(:,ii)
         case ("constraint")
           vec = self%constraint_matrix(:,ii)
         case ("density")
           vec = self%density_matrix(:,ii)
         case default
           call die_(tonto,"DIIS:get_item_1 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call read_(self%archive,vec)
       call set_name_(self%archive,old_name)
     end if

   end subroutine

   subroutine get_item_2(self,mat,name,i)
    type(diis_type) :: self
    ! Read "mat" with "name" and number "i" to disk.
     type(opmatrix_type) :: mat
     character(*) :: name
     integer(kind=kind(1)), optional, intent(in) :: i
     character(128) :: old_name

     if (self%in_core) then
       select case(name)
         case default
           call die_(tonto,"DIIS:get_item_2 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call read_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if

   end subroutine

   subroutine get_error_item(self,i,err)
    type(diis_type) :: self
    ! Get the error item "i" in vector "err"
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)), dimension(:) :: err

      call get_item_(self,err,"e",i)

   end subroutine

   subroutine get_parameter_item(self,i,par)
    type(diis_type) :: self
    ! Get the error item "i" in vector "err"
      integer(kind=kind(1)) :: i
      real(kind=kind(1.0d0)), dimension(:) :: par

      call get_item_(self,par,"p",i)

   end subroutine

   subroutine save_diis_mat(self,mat)
    type(diis_type) :: self
    ! Save the type(diis_type) matrix to disk
      real(kind=kind(1.0d0)), dimension(:,:) :: mat

      call save_item_(self,mat,"mat")

   end subroutine

   subroutine get_diis_mat(self,mat)
    type(diis_type) :: self
    ! Get the type(diis_type) matrix from disk
      real(kind=kind(1.0d0)), dimension(:,:) :: mat

      call get_item_(self,mat,"mat")

   end subroutine

   subroutine get_old_diis_matrix(self,mat)
    type(diis_type) :: self
    ! Get the old type(diis_type) matrix to "mat", if it is smaller
      real(kind=kind(1.0d0)), dimension(:,:) :: mat

      if (self%n_vec==1) then
         mat = 0.0d0
      else if (self%n_vec<=self%keep) then
         call get_diis_mat_(self,mat(1:self%n_vec,1:self%n_vec))
      else
        call get_diis_mat_(self,mat)
      end if

   end subroutine

   subroutine make_diis_matrix(self,mat,err,old_err)
    type(diis_type) :: self
    ! Make the current diis matrix "mat" using "err" as the current error vector
    ! for item ".new". "old_err" is used to hold the old error vectors.
      real(kind=kind(1.0d0)), dimension(:,:) :: mat
      real(kind=kind(1.0d0)), dimension(:) :: err,old_err
      integer(kind=kind(1)) :: dim,old

      call get_old_diis_matrix_(self,mat)
      dim = dimension_(self)
      do old = 1,dim-1                          ! Calculate the type(diis_type) matrix
         call get_error_item_(self,old,old_err)
          ! max and min are because we are doing only the upper triangle -
          ! symmetric reflect later....
         mat(max(self%new,old),min(self%new,old)) = dot_product(err,old_err)
         mat(dim,old) = 1.0d0
      end do
      mat(dim,dim) = 0.0d0
      call symmetric_reflect_(mat)
      call save_diis_mat_(self,mat)
      self%error_length = sqrt(mat(self%new,self%new))

   end subroutine

   subroutine remake_diis_matrix(self,err_vec_dim)
    type(diis_type) :: self
    ! Rebuild the type(diis_type) matrix using the error vectors on disk.  Also resets
    ! ".new".  "dimension" is the length of an error vector.
      integer(kind=kind(1)), intent(in) :: err_vec_dim
      real(kind=kind(1.0d0)), dimension(:), pointer :: err_i,err_j
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: mat
      integer(kind=kind(1)) :: dim,i,j

      dim = dimension_(self)
      call create_(err_i,err_vec_dim)
      call create_(err_j,err_vec_dim)
      call create_(mat,dim,dim)
      mat = 0.0d0
      do i=1,dim-1
        call get_error_item_(self,i,err_i)
        do j=1,i
          call get_error_item_(self,j,err_j)
          mat(i,j) = dot_product(err_i,err_j)
        end do
        mat(dim,i) = 1.0d0
      end do
      mat(dim,dim) = 0.0d0
      call symmetric_reflect_(mat)
      self%new   = next_replacement_(self)
      call save_diis_mat_(self,mat)
      self%error_length = sqrt(mat(self%new,self%new))
      call destroy_(err_j)
      call destroy_(err_i)
      call destroy_(mat)

   end subroutine

   subroutine update(self,par,old_par,coeff)
    type(diis_type) :: self
    ! Update the parameter vector "par", using "old_par" as space
      real(kind=kind(1.0d0)), dimension(:) :: par,old_par,coeff
      integer(kind=kind(1)) :: old,dim

      dim = dimension_(self)
      par = 0.0d0
      do old = 1,dim-1                          ! Form revised parameter vector
         call get_parameter_item_(self,old,old_par)
         par = par + coeff(old)*old_par
      end do

   end subroutine

   function dimension(self) result(res)
    type(diis_type) :: self
    ! Return the dimension of the type(diis_type) matrix
      integer(kind=kind(1)) :: res

      res = min(self%n_vec,self%keep)+1

   end function

   subroutine extrapolate(self,par,err)
    type(diis_type) :: self
    ! type(diis_type) extrapolation of "par", using "err" as the error vector.
      real(kind=kind(1.0d0)), dimension(:) :: par,err
      integer(kind=kind(1)) :: dim,j
      real(kind=kind(1.0d0)), dimension(:), pointer :: rhs,coeff,diag
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: mat1

      call ensure_(tonto,file_name_(self%archive)/=" ","DIIS:extrapolate ... no archive name specified")
      self%n_vec = self%n_vec+1
      dim    = dimension_(self)
      self%new   = next_replacement_(self)
      call save_pair_(self,par,err,item=self%new)
      call create_(mat1,dim,dim)
      call make_diis_matrix_(self,mat1,err,par)   ! Calculate the new diis matrix
      call times_(mat1(self%new,self%new),1.02d0)     ! Weight the last vector a bit more
      call get_parameter_item_(self,self%new,par)
      if (self%n_vec>1) then
         call create_(coeff,dim)
         call create_(rhs,dim)
         coeff = 0.0d0                   ! the diis coefficients
         rhs = 0.0d0                     ! right hand side of the linear equations
         rhs(dim) = 1.0d0

          ! Rescale using the diagonals to make solution more numerically stable.
         call create_(diag,dim-1)
         call get_diagonal_(mat1(1:dim-1,1:dim-1),diag)
         forall (j=1:dim-1)
           mat1(1:dim-1,j) = mat1(1:dim-1,j)/diag(:)
         end forall
         mat1(1:dim-1,dim) = 10.0d0**(-8)/diag(:)
         call destroy_(diag)

         mat1(dim,:) = 1.0d0
         mat1(dim,dim) = 0.0d0

         call solve_linear_equation_(mat1,rhs,coeff)       ! Solve diis equations

         call destroy_(rhs)
         call update_(self,par,err,coeff)
         call get_error_item_(self,self%new,err)                    ! Put back old error item
         call destroy_(coeff)
      end if
      call destroy_(mat1)

   end subroutine

   subroutine extrapolate_1(self,par,err)
    type(diis_type) :: self
    ! type(diis_type) extrapolation of "par", using "err" as the error vector.
      real(kind=kind(1.0d0)), dimension(:,:) :: par,err
      real(kind=kind(1.0d0)), dimension(:), pointer :: par_vec,err_vec

      call create_(par_vec,size(par))
      call create_(err_vec,size(err))
      par_vec = reshape(par,(/size(par)/))
      err_vec = reshape(err,(/size(err)/))
      call extrapolate_(self,par_vec,err_vec)
      par = reshape(par_vec,(/size(par,1),size(par,2)/))
      err = reshape(err_vec,(/size(err,1),size(err,2)/))
      call destroy_(err_vec)
      call destroy_(par_vec)

   end subroutine

!***************************************************************************

   subroutine put(self,out)
    type(diis_type) :: self
    ! Prints out the type(diis_type) data to file "out"
     type(textfile_type) :: out

     call flush_(out)
     call text_(out,"DIIS data: ")
     call show_(out,"Archive root name         = ", self%archive%root_name)
     call show_(out,"No to keep                = ", self%keep,real_width=.true.)
     call show_(out,"Stored in core            = ", self%in_core,real_width=.true.)
     call flush_(out)

   end subroutine

end
