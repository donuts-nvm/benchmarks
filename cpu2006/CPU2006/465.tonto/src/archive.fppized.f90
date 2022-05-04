!-------------------------------------------------------------------------------
!
! ARCHIVE: For archiving tonto objects to disk
!
! A polymorphic file object which can store ascii or binary format.
! Archives with the string "ascii" somewhere in their name are assumed to be
! ascii files, otherwise they are binary files.
!
! An archive can't be used for input and output at the same time.
!
! Read and write routines are used to deal with entire single objects.
! The archive is closed after the requested IO operation.
!
! If the above behaviour is not what is needed, the archive can be opened
! for multiple object I/O in one archive (say, a list of integral blocks)
! and you can deal by hand with appropriate component of the archive.
!
! Copyright (C) Dylan Jayatilaka, 1998
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
! $Id: archive.foo,v 1.22.2.9 2003/10/15 09:19:36 reaper Exp $
!-------------------------------------------------------------------------------

module ARCHIVE_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: normalise_
   use REALVEC_MODULE, only: norm_

   use OPVECTOR_MODULE, only: spinorbital_kind_
   use OPVECTOR_MODULE, only: create_
   use OPVECTOR_MODULE, only: destroy_

   use FILE_MODULE, only: write_buffered_
   use FILE_MODULE, only: delete_
   use FILE_MODULE, only: read_
   use FILE_MODULE, only: open_
   use FILE_MODULE, only: exists_
   use FILE_MODULE, only: create_
   use FILE_MODULE, only: create_copy_
   use FILE_MODULE, only: destroy_
   use FILE_MODULE, only: close_

   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: delete_
   use TEXTFILE_MODULE, only: open_
   use TEXTFILE_MODULE, only: exists_
   use TEXTFILE_MODULE, only: create_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: close_
   use TEXTFILE_MODULE, only: set_real_width_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: set_use_labels_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: set_real_style_
   use TEXTFILE_MODULE, only: destroy_
   use TEXTFILE_MODULE, only: set_real_precision_

   use OPMATRIX_MODULE, only: spinorbital_kind_
   use OPMATRIX_MODULE, only: create_
   use OPMATRIX_MODULE, only: destroy_

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

   public    read_cmat5_
   interface read_cmat5_
      module procedure read_cmat5
   end interface

   public    set_genre_
   interface set_genre_
      module procedure set_genre
   end interface

   public    set_name_
   interface set_name_
      module procedure set_name
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    write_gnuplot_
   interface write_gnuplot_
      module procedure write_gnuplot
      module procedure write_gnuplot_1
   end interface

   public    write_mat3_
   interface write_mat3_
      module procedure write_mat3
   end interface

   public    write_mat_
   interface write_mat_
      module procedure write_mat
   end interface

   public    close_
   interface close_
      module procedure close
   end interface

   public    write_mat4_
   interface write_mat4_
      module procedure write_mat4
   end interface

   public    write_opvector_
   interface write_opvector_
      module procedure write_opvector
   end interface

   public    read_cmat_
   interface read_cmat_
      module procedure read_cmat
   end interface

   public    read_mat_vec_
   interface read_mat_vec_
      module procedure read_mat_vec
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    read_int_
   interface read_int_
      module procedure read_int
   end interface

   public    set_root_
   interface set_root_
      module procedure set_root
   end interface

   public    read_mat3_
   interface read_mat3_
      module procedure read_mat3
   end interface

   public    read_vec_
   interface read_vec_
      module procedure read_vec
   end interface

   public    read_mat4_
   interface read_mat4_
      module procedure read_mat4
   end interface

   public    write_opmatrix_
   interface write_opmatrix_
      module procedure write_opmatrix
   end interface

   public    read_opvector_
   interface read_opvector_
      module procedure read_opvector
   end interface

   public    write_real_
   interface write_real_
      module procedure write_real
   end interface

   public    write_vec_mat_
   interface write_vec_mat_
      module procedure write_vec_mat
   end interface

   public    read_mat_
   interface read_mat_
      module procedure read_mat
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    set_root_name_
   interface set_root_name_
      module procedure set_root_name
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    set_
   interface set_
      module procedure set
   end interface

   public    file_name_
   interface file_name_
      module procedure file_name
   end interface

   public    write_cmat3_
   interface write_cmat3_
      module procedure write_cmat3
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    exists_
   interface exists_
      module procedure exists
   end interface

   public    write_cmat4_
   interface write_cmat4_
      module procedure write_cmat4
   end interface

   public    write_cmat5_
   interface write_cmat5_
      module procedure write_cmat5
   end interface

   public    read_real_
   interface read_real_
      module procedure read_real
   end interface

   public    read_opmatrix_
   interface read_opmatrix_
      module procedure read_opmatrix
   end interface

   public    read_vec_vec_
   interface read_vec_vec_
      module procedure read_vec_vec
   end interface

   public    write_mat_vec_
   interface write_mat_vec_
      module procedure write_mat_vec
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    delete_
   interface delete_
      module procedure delete
   end interface

   public    write_cvec_
   interface write_cvec_
      module procedure write_cvec
   end interface

   public    open_
   interface open_
      module procedure open
   end interface

   public    write_int_
   interface write_int_
      module procedure write_int
   end interface

   public    delete_all_genres_
   interface delete_all_genres_
      module procedure delete_all_genres
   end interface

   public    write_vec_
   interface write_vec_
      module procedure write_vec
   end interface

   public    set_format_
   interface set_format_
      module procedure set_format
   end interface

   public    read_cvec_
   interface read_cvec_
      module procedure read_cvec
   end interface

   private    is_a_text_file_
   interface is_a_text_file_
      module procedure is_a_text_file
   end interface

   public    read_cmat3_
   interface read_cmat3_
      module procedure read_cmat3
   end interface

   public    write_cmat_
   interface write_cmat_
      module procedure write_cmat
   end interface

   public    read_cmat4_
   interface read_cmat4_
      module procedure read_cmat4
   end interface

   public read_; interface read_
      module procedure read_int
      module procedure read_real
      module procedure read_vec
      module procedure read_cvec
      module procedure read_mat
      module procedure read_mat3
      module procedure read_mat4
      module procedure read_cmat
      module procedure read_cmat3
      module procedure read_cmat4
      module procedure read_cmat5
      module procedure read_vec_vec
      module procedure read_mat_vec
      module procedure read_opvector
      module procedure read_opmatrix
   end interface

   public write_; interface write_
      module procedure write_int
      module procedure write_real
      module procedure write_vec
      module procedure write_cvec
      module procedure write_mat
      module procedure write_mat3
      module procedure write_mat4
      module procedure write_cmat
      module procedure write_cmat3
      module procedure write_cmat4
      module procedure write_cmat5
      module procedure write_vec_mat
      module procedure write_mat_vec
      module procedure write_opvector
      module procedure write_opmatrix
   end interface

contains

   subroutine create(self,root_name,name,genre,format)
    type(archive_type) :: self
    ! Create an archive object with main name "root_name" and sub name "name".
    ! "genre" is used to identify components of type(opmatrix_type) and type(opvector_type) objects.
    ! "format" is used to identify file format (e.g. ascii). The default is
    ! binary.
      pointer :: self
      character(*), optional :: root_name,name
      character(*), optional :: genre,format

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_(self,root_name,name,genre,format)

   end subroutine

   subroutine copy(self,archive)
    type(archive_type) :: self
    ! Make a copy
     type(archive_type) :: archive

     self = archive
     call nullify_ptr_part_(self)
     if (associated(archive%file)) call create_copy_(self%file,archive%file)

   end subroutine

   subroutine destroy(self)
    type(archive_type) :: self
    ! Destroy an opmatrix object
      pointer :: self

      if ( .not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)
      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(archive_type) :: self
    ! Nullify the pointer parts of the opmatrix object

      nullify(self%file)
      nullify(self%textfile)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(archive_type) :: self
    ! Destroy the pointer parts of the opmatrix object

      call destroy_(self%file)
      call destroy_(self%textfile)

   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has not been created
!      self :: pointer
!      res :: logical(kind=kind(.true.))
!      res = .not. associated(self)
!   end

   subroutine set_defaults(self)
    type(archive_type) :: self
    ! Set up a default archive object

      self%root_name = "unknown"
      self%name      = "unknown"
      self%genre      = " "  ! generic genre
      self%format    = " "  ! assume binary format

   end subroutine

   subroutine set(self,root_name,name,genre,format)
    type(archive_type) :: self
    ! Reset an archive to have main name "root_name", sub name "name"
    ! "genre" (if present) is used to identify components of type(opmatrix_type) and type(opvector_type)
    ! objects.
    ! "format" (if present) is used to identify file format (e.g. ascii). The
    ! default is binary.
    ! Otherwise use defaults.
      character(*), optional :: root_name,name
      character(*), optional :: genre,format

      call set_defaults_(self)
      if (present(root_name)) self%root_name = root_name
      if (present(name))      self%name      = name
      if (present(genre))      self%genre      = genre
      if (present(format))    self%format    = format

   end subroutine

   subroutine set_root_name(self,root_name)
    type(archive_type) :: self
    ! Set an archive to have sub name "name".
      character(*) :: root_name

      call destroy_ptr_part_(self)
      self%root_name = root_name

   end subroutine

   subroutine set_root(self,root_name)
    type(archive_type) :: self
    ! Set an archive to have sub name "name".
      character(*) :: root_name

      call destroy_ptr_part_(self)
      self%root_name = root_name

   end subroutine

   subroutine set_name(self,name)
    type(archive_type) :: self
    ! Set an archive to have sub name "name".
      character(*) :: name

      call destroy_ptr_part_(self)
      self%name = name

   end subroutine

   subroutine set_genre(self,genre)
    type(archive_type) :: self
    ! Set an archive to have genre "genre".
      character(*) :: genre

      call destroy_ptr_part_(self)
      self%genre = genre

   end subroutine

   subroutine set_format(self,format)
    type(archive_type) :: self
    ! Set an archive to have format "format".
      character(*) :: format

      call destroy_ptr_part_(self)
      self%format = format

   end subroutine

   function file_name(self,genre) result(res)
    type(archive_type) :: self
    ! Return the archive file name from string ".root_name" by prepending the
    ! archive header ".name". If present "genre" is also appended. If present,
    ! ".format" is also appended.
      character(*), optional :: genre
      character(128) :: res
      character(128) :: k

      res = trim(self%root_name) // "." // self%name
      k = self%genre
      if (present(genre)) k = genre
      if (k/=" ")       res = trim(res) // "," // trim(k)
      if (self%format/=" ") res = trim(res) // "," // self%format

   end function

   function is_a_text_file(self) result(res)
    type(archive_type) :: self
    ! Return true if the file is a text file
      logical(kind=kind(.true.)) :: res

      res = includes_(self%format,"ascii")

   end function

   function exists(self,genre) result(res)
    type(archive_type) :: self
    ! Return .true. if the archive exists in some form on disk.
      character(*), optional :: genre
      logical(kind=kind(.true.)) :: res

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         res = exists_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         res = exists_(self%file)
         call destroy_(self%file)
      end if

   end function

   subroutine open(self,for,buffered,type,genre)
    type(archive_type) :: self
    ! Open archive file
      character(*) :: for
      character(*), optional :: type
      logical(kind=kind(.true.)), optional :: buffered
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call open_(self%textfile,for)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for,buffered,type)
      end if

   end subroutine

   subroutine close(self)
    type(archive_type) :: self
    ! Close *and* destroy the archive file part.

      if (associated(self%textfile)) then
          call close_(self%textfile)
          call destroy_(self%textfile)
      end if
      if (associated(self%file)) then
          call close_(self%file)
          call destroy_(self%file)
      end if

   end subroutine

   subroutine delete(self,genre)
    type(archive_type) :: self
    ! Delete archive file if it exists, *and* destroy the archive file part
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         if (exists_(self%textfile)) call delete_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         if (exists_(self%file)) call delete_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine delete_all_genres(self)
    type(archive_type) :: self
    ! Delete all genres of archive file, if they exists.

      call delete_(self)
      call delete_(self,"restricted")
      call delete_(self,"unrestricted")
      call delete_(self,"alpha")
      call delete_(self,"beta")
      call delete_(self,"general")
      call delete_(self,"restricted_complex")
      call delete_(self,"unrestricted_complex")
      call delete_(self,"alpha_complex")
      call delete_(self,"beta_complex")
      call delete_(self,"general_complex")
      call delete_(self,"complex_restricted")
      call delete_(self,"complex_unrestricted")
      call delete_(self,"complex_alpha")
      call delete_(self,"complex_beta")
      call delete_(self,"complex_general")

   end subroutine

!  ************************************
!  Read routines: read an entire object
!  ************************************

   subroutine read_int(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, vector "item".
      integer(kind=kind(1)) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_int ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_int ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="int")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_real(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, vector "item".
      real(kind=kind(1.0d0)) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_real ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_real ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_vec(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, vector "item".
      real(kind=kind(1.0d0)), dimension(:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_vec ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_vec ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_cvec(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, complex vector "item".
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_cvec ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_cvec ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_mat(self,item,genre,order)
    type(archive_type) :: self
    ! Read from the archive, matrix "item". "order" is the input order for text
    ! files
      real(kind=kind(1.0d0)), dimension(:,:) :: item
      character(*), optional :: genre,order

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_mat ... no text archive "// trim(self%textfile%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item,order)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_mat ... no binary archive "// trim(self%file%name))
         call open_(self%file,for="read-only",buffered=.true.,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_mat3(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, matrix "item". "order" is the input order for text
    ! files
      real(kind=kind(1.0d0)), dimension(:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_mat3 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_mat3 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_mat4(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, matrix "item". "order" is the input order for text
    ! files
      real(kind=kind(1.0d0)), dimension(:,:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_mat4 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_mat4 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="real")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_cmat(self,item,genre,order)
    type(archive_type) :: self
    ! Read from the archive, complex matrix "item". "order" is the input order
    ! for text files
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: item
      character(*), optional :: genre,order

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_cmat ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item,order)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_cmat ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_cmat3(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, matrix "item". "order" is the input order for text
    ! files
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_cmat3 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_cmat3 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_cmat4(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, matrix "item". "order" is the input order for text
    ! files
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_cmat4 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_cmat4 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_cmat5(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive, matrix "item". "order" is the input order for text
    ! files
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_cmat5 ... no text archive "// trim(self%name))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call ensure_(tonto,exists_(self%file),"ARCHIVE:read_cmat5 ... no binary archive "// trim(self%name))
         call open_(self%file,for="read-only",buffered=.true.,type="cpx")
         call read_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_vec_vec(self,item1,item2)
    type(archive_type) :: self
    ! Read from the archive, vector "item1" and matrix "item2"
      real(kind=kind(1.0d0)), dimension(:) :: item1
      real(kind=kind(1.0d0)), dimension(:,:) :: item2

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_vec_vec ... no text archive "// trim(file_name_(self)))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item1)
         call read_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         if (.not. exists_(self%file)) &
            call die_(tonto,"ARCHIVE:read_vec_vec ... no binary archive "// trim(file_name_(self)))
         call open_(self%file,for="read-only",buffered=.true.,type="real")
         call read_(self%file,item1)
         call read_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_mat_vec(self,item1,item2)
    type(archive_type) :: self
    ! Read from the archive, matrix "item1" and vector "item2"
      real(kind=kind(1.0d0)), dimension(:,:) :: item1
      real(kind=kind(1.0d0)), dimension(:) :: item2

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         call ensure_(tonto,exists_(self%textfile),"ARCHIVE:read_mat_vec ... no text archive "// trim(file_name_(self)))
         call open_(self%textfile,for="read")
         call read_(self%textfile,item1)
         call read_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         if (.not. exists_(self%file)) &
            call die_(tonto,"ARCHIVE:read_mat_vec ... no binary archive "// trim(file_name_(self)))
         call open_(self%file,for="read-only",buffered=.true.,type="real")
         call read_(self%file,item1)
         call read_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine read_opvector(self,item,genre)
    type(archive_type) :: self
    ! Read from the archive operator vector "item". If present, "genre" is the
    ! genre of "item" to be read in.
      type(opvector_type) :: item
      character(*), optional :: genre
      character(128) :: itemgenre

      if (present(genre) ) then;  itemgenre = genre
      else if (self%genre/=" ") then; itemgenre = self%genre
      else;                      itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted")
            call destroy_(item,"restricted")
            call create_(item,"restricted")
            call read_(self, item%restricted,genre="restricted")
         case ("unrestricted")
            call destroy_(item,"unrestricted")
            call create_(item,"unrestricted")
            call read_(self, item%alpha,genre="alpha")
            call read_(self, item%beta,genre="beta")
         case ("alpha")
            call destroy_(item,"alpha")
            call create_(item,"alpha")
            call read_(self, item%alpha,genre="alpha")
         case ("beta")
            call destroy_(item,"beta")
            call create_(item,"beta")
            call read_(self, item%beta,genre="beta")
         case ("general")
            call destroy_(item,"general")
            call create_(item,"general")
            call read_(self, item%general,genre="general")
         case default
            call die_(tonto,"ARCHIVE:read_opvector ... unknown genre, "//trim(itemgenre))
      end select
!      if (.is_a_text_file) .write(item,itemgenre)

   end subroutine

   subroutine read_opmatrix(self,item,genre,order)
    type(archive_type) :: self
    ! Read from the archive operator matrix "item". If present, "genre" is the
    ! genre of "item" to be read in. "order" indicated the input order for ascii
    ! files.
      type(opmatrix_type) :: item
      character(*), optional :: genre,order
      character(128) :: itemgenre

      if (present(genre))   then; itemgenre = genre
      else if (self%genre/=" ") then; itemgenre = self%genre
      else;                      itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted")
            call destroy_(item,"restricted")
            call create_(item,"restricted")
            call read_(self, item%restricted,"restricted",order)
         case ("unrestricted")
            call destroy_(item,"unrestricted")
            call create_(item,"unrestricted")
            call read_(self, item%alpha,"alpha",order)
            call read_(self, item%beta,"beta",order)
         case ("alpha")
            call destroy_(item,"alpha")
            call create_(item,"alpha")
            call read_(self, item%alpha,"alpha",order)
         case ("beta")
            call destroy_(item,"beta")
            call create_(item,"beta")
            call read_(self, item%beta,"beta",order)
         case ("general")
            call destroy_(item,"general")
            call create_(item,"general")
            call read_(self, item%general,"general",order)
         case ("restricted_complex","complex_restricted")
            call destroy_(item,"restricted_complex")
            call create_(item,"restricted_complex")
            call read_(self, item%restricted_complex,"restricted_complex",order)
         case ("unrestricted_complex","complex_unrestricted")
            call destroy_(item,"unrestricted_complex")
            call create_(item,"unrestricted_complex")
            call read_(self, item%alpha_complex,"alpha_complex",order)
            call read_(self, item%beta_complex,"beta_complex",order)
         case ("alpha_complex","complex_alpha")
            call destroy_(item,"alpha_complex")
            call create_(item,"alpha_complex")
            call read_(self, item%alpha_complex,"alpha_complex",order)
         case ("beta_complex","complex_beta")
            call destroy_(item,"beta_complex")
            call create_(item,"beta_complex")
            call read_(self, item%beta_complex,"beta_complex",order)
         case ("general_complex","complex_general")
            call destroy_(item,"general_complex")
            call create_(item,"general_complex")
            call read_(self, item%general_complex,"general_complex",order)
         case default
            call die_(tonto,"ARCHIVE:read_opmatrix ... unknown genre, "//trim(itemgenre))
      end select
!      if (.is_a_text_file) .write(item,genre,order)

   end subroutine

!  **************
!  Write routines
!  **************

   subroutine write_int(self,item,genre)
    type(archive_type) :: self
    ! Write to the archive, matrix "item".
      integer(kind=kind(1)) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="int")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_real(self,item,genre)
    type(archive_type) :: self
    ! Write to the archive, matrix "item".
      real(kind=kind(1.0d0)) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_vec(self,item,genre,format)
    type(archive_type) :: self
    ! Write to the archive, vector "item". "format" is the output format wanted
    ! for text files
      real(kind=kind(1.0d0)), dimension(:) :: item
      character(*), optional :: genre,format
      character(128) :: fmt

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         fmt = "row"; if (present(format)) fmt = format
         call put_(self%textfile,item,fmt)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_cvec(self,item,genre,format)
    type(archive_type) :: self
    ! Write to the archive, complex vector "item". "format" is the output format
    ! wanted for text files
      complex(kind=kind((1.0d0,1.0d0))), dimension(:) :: item
      character(*), optional :: genre,format
      character(128) :: fmt

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         fmt = "row"; if (present(format)) fmt = format
         call put_(self%textfile,item,fmt)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_mat(self,item,genre,order)
    type(archive_type) :: self
    ! Write to the archive, matrix "item". "order" is the output format wanted
    ! for text files
      real(kind=kind(1.0d0)), dimension(:,:) :: item
      character(*), optional :: genre,order
      character(128) :: ord

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         ord = "by_row"; if (present(order)) ord = order
         call put_(self%textfile,item,ord)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_mat3(self,item,genre)
    type(archive_type) :: self
    ! Write to the archive, matrix "item".
      real(kind=kind(1.0d0)), dimension(:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_mat4(self,item,genre)
    type(archive_type) :: self
    ! Write to the archive, matrix "item".
      real(kind=kind(1.0d0)), dimension(:,:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="real")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_cmat(self,item,genre,order)
    type(archive_type) :: self
    ! Write to the archive, complex matrix "item". "order" is the output format
    ! wanted for text files
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:) :: item
      character(*), optional :: genre,order
      character(128) :: ord

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         ord = "by_row"; if (present(order)) ord = order
         call put_(self%textfile,item,ord)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_cmat3(self,item,genre)
    type(archive_type) :: self
    ! Write to the archive, complex matrix "item".
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_cmat4(self,item,genre)
    type(archive_type) :: self
    ! Write to the archive, complex matrix "item".
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_cmat5(self,item,genre)
    type(archive_type) :: self
    ! Write to the archive, complex matrix "item".
      complex(kind=kind((1.0d0,1.0d0))), dimension(:,:,:,:,:) :: item
      character(*), optional :: genre

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self,genre)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self,genre)))
         call open_(self%file,for="write-only",buffered=.true.,type="cpx")
         call write_buffered_(self%file,item)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_vec_mat(self,item1,item2)
    type(archive_type) :: self
    ! Write to the archive, vector "item1" and matrix "item2".
      real(kind=kind(1.0d0)), dimension(:) :: item1
      real(kind=kind(1.0d0)), dimension(:,:) :: item2

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item1)
         call put_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         call open_(self%file,for="write-only",buffered=.true.,type="real")
         call write_buffered_(self%file,item1)
         call write_buffered_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_mat_vec(self,item1,item2)
    type(archive_type) :: self
    ! Write to the archive, matrix "item1" and vector "item2".
      real(kind=kind(1.0d0)), dimension(:,:) :: item1
      real(kind=kind(1.0d0)), dimension(:) :: item2

      if (is_a_text_file_(self)) then
         call create_(self%textfile,trim(file_name_(self)))
         call set_real_style_(self%textfile,"e")
         call open_(self%textfile,for="write")
         call set_use_labels_(self%textfile,.false.)
         call put_(self%textfile,item1)
         call put_(self%textfile,item2)
         call close_(self%textfile)
         call destroy_(self%textfile)
      else
         call create_(self%file,trim(file_name_(self)))
         call open_(self%file,for="write-only",buffered=.true.,type="real")
         call write_buffered_(self%file,item1)
         call write_buffered_(self%file,item2)
         call close_(self%file)
         call destroy_(self%file)
      end if

   end subroutine

   subroutine write_opvector(self,item,genre,order)
    type(archive_type) :: self
    ! Write to the archive operator vector "item". "order" indicated the output
    ! order for ascii files.
      type(opvector_type) :: item
      character(*), optional :: genre,order
      character(128) :: itemgenre

      if (present(genre)) then; itemgenre = genre
      else;                    itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted  "); call write_(self, item%restricted,"restricted",order)
         case ("unrestricted"); call write_(self, item%alpha,"alpha",order)
                                call write_(self, item%beta,"beta",order)
         case ("alpha       "); call write_(self, item%alpha,"alpha",order)
         case ("beta        "); call write_(self, item%beta,"beta",order)
         case ("general     "); call write_(self, item%general,"general",order)
         case default;          allocate(tonto%known_keywords(5))
         tonto%known_keywords(1) = "restricted  "
         tonto%known_keywords(2) = "unrestricted"
         tonto%known_keywords(3) = "alpha       "
         tonto%known_keywords(4) = "beta        "
         tonto%known_keywords(5) = "general     "
         call unknown_(tonto,itemgenre,"ARCHIVE:write_opvector")
         deallocate(tonto%known_keywords)
      end select

   end subroutine

   subroutine write_opmatrix(self,item,genre,order)
    type(archive_type) :: self
    ! Write to the archive operator vector "item". "order" indicates the output
    ! order for ascii files.
      type(opmatrix_type) :: item
      character(*), optional :: genre,order
      character(128) :: itemgenre

      if (present(genre)) then; itemgenre = genre
      else;                    itemgenre = spinorbital_kind_(item)
      end if
      select case (itemgenre)
         case ("restricted");           call write_(self,item%restricted,"restricted",order)
         case ("unrestricted");         call write_(self,item%alpha,"alpha",order)
                                        call write_(self,item%beta,"beta",order)
         case ("alpha");                call write_(self,item%alpha,"alpha",order)
         case ("beta");                 call write_(self,item%beta,"beta",order)
         case ("general");              call write_(self,item%general,"general",order)
         case ("restricted_complex");   call write_(self,item%restricted_complex,"restricted_complex",order)
         case ("unrestricted_complex"); call write_(self,item%alpha_complex,"alpha_complex",order)
                                        call write_(self,item%beta_complex,"beta_complex",order)
         case ("alpha_complex");        call write_(self,item%alpha_complex,"alpha_complex",order)
         case ("beta_complex");         call write_(self,item%beta_complex,"beta_complex",order)
         case ("general_complex");      call write_(self,item%general_complex,"general_complex",order)
         case default;                  call die_(tonto,"ARCHIVE:write_opmatrix ... unknown genre, "//trim(itemgenre))
      end select

   end subroutine

!  **********************
!  Write gnuplot routines
!  **********************

   subroutine write_gnuplot(self,density,n_x,n_y,n_z)
    type(archive_type) :: self
    ! Write a "density" vector corresponding to a grid with dimensions "n_x",
    ! "n_y", "n_z" in gnuplot format (suitable for an splot).
      real(kind=kind(1.0d0)), dimension(:) :: density
      integer(kind=kind(1)) :: n_x,n_y,n_z
      integer(kind=kind(1)) :: n,x,y,z

      call ensure_(tonto,is_a_text_file_(self),"ARCHIVE:write_gnuplot ... must open a ascii file")
      call open_(self,for="writing")
      call set_real_style_(self%textfile,"e")
      call set_real_width_(self%textfile,30)
      call set_real_precision_(self%textfile,15)
      n = 1
      do z = 1,n_z
         if (n_z>1) call show_(self%textfile," z = ",z)
         do y = 1,n_y
         do x = 1,n_x
            call put_(self%textfile,density(n),flush=1)
            n = n+1
         end do
         call flush_(self%textfile)
         end do
      end do
      call close_(self)

   end subroutine

   subroutine write_gnuplot_1(self,j,n_x,n_y,n_z,norm,normalise)
    type(archive_type) :: self
    ! Write a current density vector "j" corresponding to a grid with dimensions
    ! "n_x", "n_y", "n_z" in gnuplot format (suitable for an splot).
    ! If "norm" is present and .true. write the norm of the vector field
    ! If "normalise" is present and .true. write the normalised of the vector field
       real(kind=kind(1.0d0)), dimension(:,:) :: j
      integer(kind=kind(1)) :: n_x,n_y,n_z
      logical(kind=kind(.true.)), optional :: norm,normalise
      integer(kind=kind(1)) :: n,x,y,z
      logical(kind=kind(.true.)) :: write_norm,write_normalised
       real(kind=kind(1.0d0)), dimension(3) :: v

   call ensure_(tonto,is_a_text_file_(self),"ARCHIVE:write_gnuplot_1 ... must open a ascii file")
      call open_(self,for="writing")
      call set_real_style_(self%textfile,"e")
      call set_real_width_(self%textfile,30)
      call set_real_precision_(self%textfile,15)
      write_norm = .false.; if (present(norm)) write_norm = norm
      write_normalised = .false.; if (present(normalise)) write_normalised = normalise
      n = 1
      do z = 1,n_z
         if (n_z>1) then
            call put_(self%textfile," z = ")
            call put_(self%textfile,z)
            call flush_(self%textfile)
         end if
         do y = 1,n_y
         do x = 1,n_x
            v = j(n,:)
            if (write_norm) then
               call put_(self%textfile, norm_(v))
            else
               if (write_normalised) call normalise_(v)
               call put_(self%textfile,v(1))
               call put_(self%textfile,v(2))
               call put_(self%textfile,v(3))
            end if
            call flush_(self%textfile)
            n = n+1
         end do
         call flush_(self%textfile)
         end do
      end do
      call close_(self)

   end subroutine

end
