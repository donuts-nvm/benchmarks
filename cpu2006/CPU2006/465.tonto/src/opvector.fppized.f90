!-------------------------------------------------------------------------------
!
! OPVECTOR: Diagonals of operator matrices, e.g. eigenvalues an occupation numbers
!
! Provide a basis set (matrix) representation of the diagonal of a one-electron
! quantum mechanical operator. Can cope with polymorphic types of basis
! representations, including restricted, unrestricted, and general basis
! orbitals. Complex types aren't needed sice operators are hermitian.
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
! $Id: opvector.foo,v 1.16.2.3 2003/09/18 09:27:36 dylan Exp $
!-------------------------------------------------------------------------------

module OPVECTOR_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: set_beta_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: beta_
   use REALVEC_MODULE, only: set_alpha_
   use REALVEC_MODULE, only: alpha_
   use REALVEC_MODULE, only: destroy_

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

   public    no_of_occupied_
   interface no_of_occupied_
      module procedure no_of_occupied
   end interface

   public    set_
   interface set_
      module procedure set
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    set_to_
   interface set_to_
      module procedure set_to
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    destroyed_
   interface destroyed_
      module procedure destroyed
   end interface

   public    zero_
   interface zero_
      module procedure zero
   end interface

   public    spinorbital_kind_
   interface spinorbital_kind_
      module procedure spinorbital_kind
   end interface

   public    all_destroyed_
   interface all_destroyed_
      module procedure all_destroyed
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    any_created_
   interface any_created_
      module procedure any_created
   end interface

   public    convert_to_
   interface convert_to_
      module procedure convert_to
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
      module procedure destroy_1
   end interface

   public    created_
   interface created_
      module procedure created
   end interface

contains

   subroutine create(self,n_bf,opveckind)
    type(opvector_type) :: self
    ! Create an opmatrix object, in a basis set with "n_bf" spatial orbitals
      pointer :: self
      integer(kind=kind(1)) :: n_bf
      character(*), optional :: opveckind

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_(self,n_bf)
      if (present(opveckind)) call create_(self,opveckind)

   end subroutine

   subroutine destroy(self)
    type(opvector_type) :: self
    ! Destroy an opmatrix object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine create_1(self,opveckind)
    type(opvector_type) :: self
    ! Create the appropriate spinorbital kind of the opvector
      character(*) :: opveckind
      integer(kind=kind(1)) :: n_bf

      n_bf = self%n_bf
      call ensure_(tonto,self%n_bf>=0,"OPVECTOR:create_1 ... number of basis functions is undefined")
      select case (opveckind)
         case ("restricted","restricted_complex");     call create_(self%restricted,n_bf)
         case ("unrestricted","unrestricted_complex"); call create_(self%alpha,n_bf)
                                                       call create_(self%beta,n_bf)
         case ("alpha","alpha_complex");               call create_(self%alpha,n_bf)
         case ("beta","beta_complex");                 call create_(self%beta,n_bf)
         case ("general","general_complex");           call create_(self%general,2*n_bf)
         case default; call die_(tonto,"OPVECTOR:create_1 ... unknown spinorbital kind, " // trim(opveckind))
      end select

   end subroutine

   subroutine destroy_1(self,opveckind)
    type(opvector_type) :: self
    ! Destroy the appropriate spinorbital kind of the opmatrix
      character(*) :: opveckind

      select case (opveckind)
         case ("restricted","restriced_complex");       call destroy_(self%restricted)
         case ("unrestricted","unrestricted_complex");  call destroy_(self%alpha); call destroy_(self%beta)
         case ("alpha","alpha_complex");                call destroy_(self%alpha)
         case ("beta","beta_complex");                  call destroy_(self%beta)
         case ("general","general_complex");            call destroy_(self%general)
         case default; call die_(tonto,"OPVECTOR:destroy_1 ... unknown spinorbital kind, " // trim(opveckind))
      end select

   end subroutine

   subroutine nullify_ptr_part(self)
    type(opvector_type) :: self
    ! Nullify the pointer parts of the opmatrix object

      nullify(self%restricted)
      nullify(self%alpha)
      nullify(self%beta)
      nullify(self%general)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(opvector_type) :: self
    ! Destroy the pointer parts of the opmatrix object

      call destroy_(self%restricted)
      call destroy_(self%alpha)
      call destroy_(self%beta)
      call destroy_(self%general)

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

   function created(self,opveckind) result(res)
    type(opvector_type) :: self
    ! Returns true if subkind "opveckind" of self has been created
      pointer :: self
      character(*) :: opveckind
      logical(kind=kind(.true.)) :: res

      if (.not. associated(self)) then
                                    res = .false.;   return
      else
         select case (opveckind)
            case ("restricted");    res = associated(self%restricted)
            case ("unrestricted");  res = associated(self%alpha)
            case ("general");       res = associated(self%general)
            case default; call die_(tonto,"OPVECTOR:created ... unknown spinorbital kind, " // trim(opveckind))
         end select
      end if

   end function

   function destroyed(self,opveckind) result(res)
    type(opvector_type) :: self
    ! Returns true if subkind "opveckind" of self has *not* been created
     pointer :: self
     character(*) :: opveckind
     logical(kind=kind(.true.)) :: res

     if (.not. associated(self)) then
                                 res = .true.;   return
     else
       select case (opveckind)
         case ("restricted");    res = .not. associated(self%restricted)
         case ("unrestricted");  res = .not. associated(self%alpha)
         case ("general");       res = .not. associated(self%general)
         case default; call die_(tonto,"OPVECTOR:destroyed ... unknown spinorbital kind, " // trim(opveckind))
       end select
     end if

   end function

   function any_created(self) result(res)
    type(opvector_type) :: self
    ! Returns true if any component of self has been created
      logical(kind=kind(.true.)) :: res
       logical(kind=kind(.true.)), dimension(3) :: c

      c(1) = associated(self%restricted)
      c(2) = associated(self%alpha)
      c(3) = associated(self%general)
      res = any(c)

   end function

   function all_destroyed(self) result(res)
    type(opvector_type) :: self
    ! Returns true if all components of self have been destroyed
      logical(kind=kind(.true.)) :: res
       logical(kind=kind(.true.)), dimension(3) :: d

      d(1) = .not. associated(self%restricted)
      d(2) = .not. associated(self%alpha)
      d(3) = .not. associated(self%general)
      res = all(d)

   end function

   subroutine create_copy(self,v)
    type(opvector_type) :: self
    ! Create an opvector object
      pointer :: self
       type(opvector_type), intent(in) :: v

      call create_(self,v%n_bf)
      call set_to_(self,v)

   end subroutine

   subroutine set(self,n_bf)
    type(opvector_type) :: self
    ! Set with "n_bf" spatial orbitals
      integer(kind=kind(1)), intent(in) :: n_bf

      self%n_bf = n_bf

   end subroutine

   subroutine set_to(self,v)
    type(opvector_type) :: self
    ! Set self to "v".
       type(opvector_type) :: v

      call set_(self, v%n_bf)
      if ( all_destroyed_(v)) then;   return; end if
      if ( associated(v%restricted)) then
         call destroy_(self,"restricted"); call create_(self,"restricted")
         self%restricted = v%restricted
      end if
      if ( associated(v%alpha)) then
         call destroy_(self,"alpha"); call create_(self,"alpha")
          self%alpha = v%alpha
      end if
      if ( associated(v%beta)) then
         call destroy_(self,"beta"); call create_(self,"beta")
         self%beta = v%beta
      end if
      if ( associated(v%general)) then
         call destroy_(self,"general"); call create_(self,"general")
         self%general = v%general
      end if

   end subroutine

   function spinorbital_kind(self) result(res)
    type(opvector_type) :: self
    ! Return the kind of spinorbitals used in the representation.
    ! The simplest spinorbital kind in use is the one returned.
      character(128) :: res

      if      ( associated(self%restricted) ) then; res = "restricted"
      else if ( associated(self%alpha) )      then; res = "unrestricted"
      else if ( associated(self%general) )    then; res = "general"
      else; call die_(tonto,"OPVECTOR:spinorbital_kind ... no object created")
      end if

   end function

   subroutine zero(self)
    type(opvector_type) :: self
    ! Set self to zero
      character(128) :: opveckind

      opveckind = spinorbital_kind_(self)
      select case(opveckind)
         case("restricted");      self%restricted = 0.0d0
         case("unrestricted");    self%alpha = 0.0d0; self%beta = 0.0d0
         case("general");         self%general = 0.0d0
         case default; call die_(tonto,"OPVECTOR:zero ... unknown spinorbital kind, " // trim(opveckind))
      end select

   end subroutine

   subroutine convert_to(self,newkind)
    type(opvector_type) :: self
    ! Convert self to a new basis kind "newkind"
     character(*) :: newkind
     character(128) :: oldkind

     oldkind = spinorbital_kind_(self)
     if (newkind==oldkind) then;   return; end if
     call create_(self,newkind)
     select case (oldkind)
       case("restricted")
         select case (newkind)
           case("unrestricted")
             self%alpha = self%restricted
             self%beta  = self%restricted
           case("general")
             self%general = 0.0d0
             call set_alpha_(self%general,self%restricted)
             call set_beta_(self%general,self%restricted)
           case default
             call die_(tonto,"OPVECTOR:convert_to ... cant convert kind " // trim(oldkind) // " to kind " // trim(newkind))
         end select
       case("unrestricted")
         select case (newkind)
           case("restricted")
             self%restricted = self%alpha
           case("general")
             call set_alpha_(self%general,self%alpha)
             call set_beta_(self%general,self%beta)
           case default
             call die_(tonto,"OPVECTOR:convert_to ... cant convert kind " // trim(oldkind) // " to kind " // trim(newkind))
         end select
       case("general")
         select case (newkind)
           case("unrestricted")
             self%alpha = alpha_(self%general)
             self%beta  = beta_(self%general)
           case default
             call die_(tonto,"OPVECTOR:convert_to ... cant convert kind " // trim(oldkind) // " to kind " // trim(newkind))
         end select
       case default; call die_(tonto,"OPVECTOR:convert_to ... cant convert old kind " // trim(oldkind))
     end select
     call destroy_(self,oldkind)

   end subroutine

   function no_of_occupied(self,opveckind,tol) result(res)
    type(opvector_type) :: self
    ! Returns the number of non-zero "occupied" elements, i.e. all those elements
    ! greater than 10.0d0**(-7).  If "opveckind" is present, the number of occupied
    ! elements is returned for that kind (the default is determined by the
    ! .spinorbital_kind). if "tol" is present, it is used instead of 10.0d0**(-7) to
    ! determine what is occupied.
      intent(in) :: self
      character(*), optional, intent(in) :: opveckind
      real(kind=kind(1.0d0)), optional, intent(in) :: tol
      integer(kind=kind(1)) :: res
      character(128) :: itemkind
      real(kind=kind(1.0d0)) :: eps

      itemkind = spinorbital_kind_(self)
      if (present(opveckind)) itemkind = opveckind
      eps = 10.0d0**(-7)
      if (present(tol)) eps = tol
      select case (itemkind)
         case ("restricted")
            call ensure_(tonto,associated(self%restricted),"OPVECTOR:no_of_occupied ... no restricted part")
            res = count(self%restricted>=eps)
         case ("unrestricted")
            call ensure_(tonto,associated(self%alpha),"OPVECTOR:no_of_occupied ... no alpha part")
            call ensure_(tonto,associated(self%beta),"OPVECTOR:no_of_occupied ... no beta part")
            res = count(self%alpha>=eps) + count(self%beta>=eps)
         case ("alpha")
            call ensure_(tonto,associated(self%alpha),"OPVECTOR:no_of_occupied ... no alpha part")
            res = count(self%alpha>=eps)
         case ("beta")
            call ensure_(tonto,associated(self%beta),"OPVECTOR:no_of_occupied ... no beta part")
            res = count(self%beta>=eps)
         case ("general")
            call ensure_(tonto,associated(self%general),"OPVECTOR:no_of_occupied ... no general part")
            res = count(self%general>=eps)
         case default
            call die_(tonto,"OPVECTOR:no_of_occupied ... unknown kind, "//trim(itemkind))
      end select

   end function

end
