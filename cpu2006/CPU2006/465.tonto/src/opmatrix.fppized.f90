!-------------------------------------------------------------------------------
!
! OPMATRIX: Operator matrices.
!
! Provide a basis set (matrix) representation of a one-electron quantum
! mechanical operator. Can cope with polymorphic types of basis representations,
! including restricted, unrestricted, and general basis orbital matrices.
! Also, complex types for all these matrices.
!
! Basically, its a polymorphic matrix type.
!
! Copyright (C) Dylan Jayatilaka 1998
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
! $Id: opmatrix.foo,v 1.25.2.2 2003/09/11 07:51:31 reaper Exp $
!-------------------------------------------------------------------------------

module OPMATRIX_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: equals_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: norm_
   use REALVEC_MODULE, only: beta_
   use REALVEC_MODULE, only: alpha_
   use REALVEC_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: flush_

   use CPXMAT_MODULE, only: schmidt_orthonormalise_
   use CPXMAT_MODULE, only: plus_scaled_mat_
   use CPXMAT_MODULE, only: create_
   use CPXMAT_MODULE, only: beta_beta_set_to_
   use CPXMAT_MODULE, only: symmetrically_orthonormalise_
   use CPXMAT_MODULE, only: compress_to_square_
   use CPXMAT_MODULE, only: uncompress_from_square_
   use CPXMAT_MODULE, only: to_scaled_mat_
   use CPXMAT_MODULE, only: alpha_alpha_set_to_
   use CPXMAT_MODULE, only: trace_of_product_
   use CPXMAT_MODULE, only: minus_
   use CPXMAT_MODULE, only: destroy_
   use CPXMAT_MODULE, only: plus_

   use REAL_MODULE, only: equals_

   use REALMAT_MODULE, only: schmidt_orthonormalise_
   use REALMAT_MODULE, only: compress_to_triangle_
   use REALMAT_MODULE, only: beta_beta_put_to_
   use REALMAT_MODULE, only: plus_scaled_mat_
   use REALMAT_MODULE, only: beta_beta_
   use REALMAT_MODULE, only: create_
   use REALMAT_MODULE, only: beta_beta_set_to_
   use REALMAT_MODULE, only: symmetrically_orthonormalise_
   use REALMAT_MODULE, only: is_square_
   use REALMAT_MODULE, only: to_scaled_mat_
   use REALMAT_MODULE, only: alpha_alpha_put_to_
   use REALMAT_MODULE, only: alpha_alpha_set_to_
   use REALMAT_MODULE, only: uncompress_from_triangle_
   use REALMAT_MODULE, only: trace_of_product_
   use REALMAT_MODULE, only: minus_
   use REALMAT_MODULE, only: alpha_alpha_
   use REALMAT_MODULE, only: destroy_
   use REALMAT_MODULE, only: plus_

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

   public    uncompress_
   interface uncompress_
      module procedure uncompress
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
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
      module procedure create_2
   end interface

   public    symmetrically_orthonormalise_
   interface symmetrically_orthonormalise_
      module procedure symmetrically_orthonormalise
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    number_kind_
   interface number_kind_
      module procedure number_kind
   end interface

   public    damp_
   interface damp_
      module procedure damp
   end interface

   public    compress_
   interface compress_
      module procedure compress
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    expectation_
   interface expectation_
      module procedure expectation
   end interface

   public    convert_to_
   interface convert_to_
      module procedure convert_to
      module procedure convert_to_1
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
      module procedure destroy_1
   end interface

   public    plus_
   interface plus_
      module procedure plus
   end interface

   public    schmidt_orthonormalise_
   interface schmidt_orthonormalise_
      module procedure schmidt_orthonormalise
   end interface

   public    plus_scaled_
   interface plus_scaled_
      module procedure plus_scaled
   end interface

   public    destroyed_
   interface destroyed_
      module procedure destroyed
   end interface

   public    guess_scf_kind_
   interface guess_scf_kind_
      module procedure guess_scf_kind
   end interface

   public    zero_
   interface zero_
      module procedure zero
   end interface

   public    to_scaled_
   interface to_scaled_
      module procedure to_scaled
   end interface

   public    spinorbital_kind_
   interface spinorbital_kind_
      module procedure spinorbital_kind
   end interface

   public    all_destroyed_
   interface all_destroyed_
      module procedure all_destroyed
   end interface

   public    any_created_
   interface any_created_
      module procedure any_created
   end interface

   public    minus_
   interface minus_
      module procedure minus
   end interface

   public    created_
   interface created_
      module procedure created
   end interface

   public    l_compress_
   interface l_compress_
      module procedure l_compress
   end interface

contains

   subroutine create(self,n_bf)
    type(opmatrix_type) :: self
    ! Create an opmatrix object, in a basis set with "n_bf" spatial orbitals
      pointer :: self
      integer(kind=kind(1)), intent(in), optional :: n_bf

      nullify(self)
      allocate(self)

      call nullify_ptr_part_(self)
      call set_(self,n_bf)

   end subroutine

   subroutine create_1(self,n_bf,opmatkind)
    type(opmatrix_type) :: self
    ! Create an opmatrix object, in a basis set with "n_bf" spatial orbitals
      pointer :: self
      integer(kind=kind(1)), intent(in) :: n_bf
      character(*) :: opmatkind

      call create_(self,n_bf)
      call create_(self,opmatkind)

   end subroutine

   subroutine create_copy(self,m)
    type(opmatrix_type) :: self
    ! Create an opmatrix object
      pointer :: self
       type(opmatrix_type), intent(in) :: m

      call create_(self,m%n_bf)
      call set_to_(self,m)

   end subroutine

   subroutine destroy(self)
    type(opmatrix_type) :: self
    ! Destroy an opmatrix object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine create_2(self,opmatkind)
    type(opmatrix_type) :: self
    ! Create the appropriate spinorbital kind the opmatrix
      character(*), intent(in) :: opmatkind
      integer(kind=kind(1)) :: n_bf

      n_bf = self%n_bf
   call ensure_(tonto,n_bf > 0,"OPMATRIX:create_2 ... bad number of basis functions.")
      select case (opmatkind)
         case ("restricted");           call create_(self%restricted,n_bf,n_bf)
         case ("unrestricted");         call create_(self%alpha,n_bf,n_bf)
                                        call create_(self%beta,n_bf,n_bf)
         case ("alpha");                call create_(self%alpha,n_bf,n_bf)
         case ("beta");                 call create_(self%beta,n_bf,n_bf)
         case ("general");              call create_(self%general,2*n_bf,2*n_bf)
         case ("restricted_complex");   call create_(self%restricted_complex,n_bf,n_bf)
         case ("unrestricted_complex"); call create_(self%alpha_complex,n_bf,n_bf)
                                        call create_(self%beta_complex,n_bf,n_bf)
         case ("alpha_complex");        call create_(self%alpha_complex,n_bf,n_bf)
         case ("beta_complex");         call create_(self%beta_complex,n_bf,n_bf)
         case ("general_complex");      call create_(self%general_complex,2*n_bf,2*n_bf)
         case default;   call die_(tonto,"OPMATRIX:create_2 ... unknown kind, "//trim(opmatkind))
      end select

   end subroutine

   subroutine destroy_1(self,opmatkind)
    type(opmatrix_type) :: self
    ! Destroy the appropriate spinorbital kind of the opmatrix
      character(*) :: opmatkind

      select case (opmatkind)
         case ("restricted");           call destroy_(self%restricted)
         case ("unrestricted");         call destroy_(self%alpha); call destroy_(self%beta)
         case ("alpha");                call destroy_(self%alpha)
         case ("beta");                 call destroy_(self%beta)
         case ("general");              call destroy_(self%general)
         case ("restricted_complex");   call destroy_(self%restricted_complex)
         case ("unrestricted_complex"); call destroy_(self%alpha_complex)
                                        call destroy_(self%beta_complex)
         case ("alpha_complex");        call destroy_(self%alpha_complex)
         case ("beta_complex");         call destroy_(self%beta_complex)
         case ("general_complex");      call destroy_(self%general_complex)
         case ("all");                  call destroy_ptr_part_(self)
         case default;   call die_(tonto,"OPMATRIX:destroy_1 ... unknown kind, "//trim(opmatkind))
      end select

   end subroutine

   subroutine nullify_ptr_part(self)
    type(opmatrix_type) :: self
    ! Nullify the pointer parts of the opmatrix object

      nullify(self%restricted)
      nullify(self%alpha)
      nullify(self%beta)
      nullify(self%general)
      nullify(self%restricted_complex)
      nullify(self%alpha_complex)
      nullify(self%beta_complex)
      nullify(self%general_complex)
      nullify(self%triangle)
      nullify(self%square)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(opmatrix_type) :: self
    ! Destroy the pointer parts of the opmatrix object

      call destroy_(self%restricted)
      call destroy_(self%alpha)
      call destroy_(self%beta)
      call destroy_(self%general)
      call destroy_(self%restricted_complex)
      call destroy_(self%alpha_complex)
      call destroy_(self%beta_complex)
      call destroy_(self%general_complex)
      call destroy_(self%triangle)
      call destroy_(self%square)

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

   function any_created(self) result(res)
    type(opmatrix_type) :: self
    ! Returns true if any component of self has been created
      logical(kind=kind(.true.)) :: res
      logical(kind=kind(.true.)), dimension(6) :: c

      c(1) = associated(self%restricted)
      c(2) = associated(self%alpha)
      c(3) = associated(self%general)
      c(4) = associated(self%restricted_complex)
      c(5) = associated(self%alpha_complex)
      c(6) = associated(self%general_complex)
      res = any(c)

   end function

   function all_destroyed(self) result(res)
    type(opmatrix_type) :: self
    ! Returns true if all components of self have been destroyed
      logical(kind=kind(.true.)) :: res
       logical(kind=kind(.true.)), dimension(6) :: d

      d(1) = .not. associated(self%restricted)
      d(2) = .not. associated(self%alpha)
      d(3) = .not. associated(self%general)
      d(4) = .not. associated(self%restricted_complex)
      d(5) = .not. associated(self%alpha_complex)
      d(6) = .not. associated(self%general_complex)
      res = all(d)

   end function

   function created(self,opmatkind) result(res)
    type(opmatrix_type) :: self
    ! Returns true if subkind "opmatkind" of self has been created
      pointer :: self
      character(*) :: opmatkind
      logical(kind=kind(.true.)) :: res

      if (.not. associated(self)) then
                                           res = .false.;   return
      else
         select case (opmatkind)
            case ("restricted");           res = associated(self%restricted)
            case ("unrestricted");         res = associated(self%alpha)
            case ("general");              res = associated(self%general)
            case ("restricted_complex");   res = associated(self%restricted_complex)
            case ("unrestricted_complex"); res = associated(self%alpha_complex)
            case ("general_complex");      res = associated(self%general_complex)
            case default;   call die_(tonto,"OPMATRIX:created ... unknown kind, "//trim(opmatkind))
         end select
      end if

   end function

   function destroyed(self,opmatkind) result(res)
    type(opmatrix_type) :: self
    ! Returns true if subkind "opmatkind" of self has *not* been created
     character(*) :: opmatkind
     logical(kind=kind(.true.)) :: res

       select case (opmatkind)
         case ("restricted");           res = .not. associated(self%restricted)
         case ("unrestricted");         res = .not. associated(self%alpha)
         case ("general");              res = .not. associated(self%general)
         case ("restricted_complex");   res = .not. associated(self%restricted_complex)
         case ("unrestricted_complex"); res = .not. associated(self%alpha_complex)
         case ("general_complex");      res = .not. associated(self%general_complex)
         case default;   call die_(tonto,"OPMATRIX:destroyed ... unknown kind, "//trim(opmatkind))
       end select

   end function

   subroutine set(self,n_bf)
    type(opmatrix_type) :: self
    ! Set with "n_bf" spatial orbitals
      integer(kind=kind(1)), intent(in), optional :: n_bf

      call set_defaults_(self)
      if (present(n_bf)) self%n_bf = n_bf

   end subroutine

   subroutine set_defaults(self)
    type(opmatrix_type) :: self
    ! Set defaults

      self%n_bf = 0

   end subroutine

   subroutine set_to(self,m)
    type(opmatrix_type) :: self
    ! Set self to "m".
      type(opmatrix_type), intent(in) :: m
     integer(kind=kind(1)) :: old_n_bf

     old_n_bf = self%n_bf
     call set_(self,m%n_bf)
     if (associated(m%restricted)) then
        if (old_n_bf /= self%n_bf)     call destroy_(self,"restricted")
        if (.not. associated(self%restricted)) call create_(self,"restricted")
        self%restricted = m%restricted
     end if
     if (associated(m%alpha)) then
        if (old_n_bf /= self%n_bf) call destroy_(self,"alpha")
        if (.not. associated(self%alpha))  call create_(self,"alpha")
        self%alpha = m%alpha
     end if
     if (associated(m%beta)) then
        if (old_n_bf /= self%n_bf) call destroy_(self,"beta")
        if (.not. associated(self%beta))   call create_(self,"beta")
        self%beta = m%beta
     end if
     if (associated(m%general)) then
        if (old_n_bf /= self%n_bf)  call destroy_(self,"general")
        if (.not. associated(self%general)) call create_(self,"general")
        self%general = m%general
     end if
     if (associated(m%restricted_complex)) then
        if (old_n_bf /= self%n_bf)                call destroy_(self,"restricted_complex")
        if (destroyed_(self,"restricted_complex")) call create_(self,"restricted_complex")
        self%restricted_complex = m%restricted_complex
     end if
     if (associated(m%alpha_complex)) then
        if (old_n_bf /= self%n_bf)           call destroy_(self,"alpha_complex")
        if (destroyed_(self,"alpha_complex")) call create_(self,"alpha_complex")
        self%alpha_complex = m%alpha_complex
     end if
     if (associated(m%beta_complex)) then
        if (old_n_bf /= self%n_bf)          call destroy_(self,"beta_complex")
        if (destroyed_(self,"beta_complex")) call create_(self,"beta_complex")
        self%beta_complex = m%beta_complex
     end if
     if (associated(m%general_complex)) then
        if (old_n_bf /= self%n_bf)             call destroy_(self,"general_complex")
        if (destroyed_(self,"general_complex")) call create_(self,"general_complex")
        self%general_complex = m%general_complex
     end if

   end subroutine

   function spinorbital_kind(self) result(res)
    type(opmatrix_type) :: self
    ! Return the kind of spinorbitals used in the representation
    ! The simplest spinorbital kind in use is the one returned.
      character(128) :: res

      if      (associated(self%restricted) )         then; res = "restricted"
      else if (associated(self%alpha) )              then; res = "unrestricted"
      else if (associated(self%general) )            then; res = "general"
      else if (associated(self%restricted_complex) ) then; res = "restricted_complex"
      else if (associated(self%alpha_complex) )      then; res = "unrestricted_complex"
      else if (associated(self%general_complex) )    then; res = "general_complex"
      else; call die_(tonto,"OPMATRIX:spinorbital_kind ... no object created")
      end if

   end function

   function number_kind(self) result(res)
    type(opmatrix_type) :: self
    ! Return the kind of numbers used in the representation
      character(128) :: res

      if      ( associated(self%restricted) )         then; res = "real"
      else if ( associated(self%alpha) )              then; res = "real"
      else if ( associated(self%general) )            then; res = "real"
      else if ( associated(self%restricted_complex) ) then; res = "complex"
      else if ( associated(self%alpha_complex) )      then; res = "complex"
      else if ( associated(self%general_complex) )    then; res = "complex"
      else; call die_(tonto,"OPMATRIX:number_kind ... no object created")
      end if

   end function

   function guess_scf_kind(self) result(res)
    type(opmatrix_type) :: self
    ! Guess the kind of SCF to be used from the spinorbitals used in the
    ! representation.
      character(128) :: res

      if      ( associated(self%restricted) )         then; res = "rhf"
      else if ( associated(self%alpha) )              then; res = "uhf"
      else if ( associated(self%general) )            then; res = "ghf"
      else if ( associated(self%restricted_complex) ) then; res = "rchf"
      else if ( associated(self%alpha_complex) )      then; res = "uchf"
      else if ( associated(self%general_complex) )    then; res = "gchf"
      else; call die_(tonto,"OPMATRIX:guess_scf_kind ... no object created")
      end if

   end function

   subroutine zero(self)
    type(opmatrix_type) :: self
    ! Set self to zero
      character(128) :: opmatkind

      opmatkind = spinorbital_kind_(self)
      select case(opmatkind)
         case("restricted");           self%restricted = 0.0d0
         case("unrestricted");         self%alpha = 0.0d0; self%beta = 0.0d0
         case("alpha");                self%alpha = 0.0d0
         case("beta");                 self%beta = 0.0d0
         case("general");              self%general = 0.0d0
         case("restricted_complex");   self%restricted = 0.0d0
         case("unrestricted_complex"); self%alpha_complex = 0.0d0
                                       self%beta_complex = 0.0d0
         case("alpha_complex");        self%alpha_complex = 0.0d0
         case("beta_complex");         self%beta_complex = 0.0d0
         case("general_complex");      self%general_complex = 0.0d0
         case default;   call die_(tonto,"OPMATRIX:zero ... unknown kind, "//trim(opmatkind))
      end select

   end subroutine

   subroutine convert_to(self,newkind,factor)
    type(opmatrix_type) :: self
    ! Convert self to a new basis kind "newkind".  To convert MO's please use
    ! the next routine.
      character(*) :: newkind
      real(kind=kind(1.0d0)), optional :: factor
      real(kind=kind(1.0d0)) :: fac
      character(128) :: oldkind

      fac = 1.0d0
      if (present(factor)) fac = factor
      oldkind = spinorbital_kind_(self)
      if (newkind==oldkind) then;   return; end if
      call create_(self,newkind)
      select case (oldkind)
         case("restricted")
            select case (newkind)
               case("unrestricted")
                  self%alpha = fac*self%restricted
                  self%beta  = fac*self%restricted
               case("general")
                  self%general = 0.0d0
                  call alpha_alpha_set_to_(self%general,self%restricted,fac)
                  call beta_beta_set_to_(self%general,self%restricted,fac)
               case("restricted_complex")
                  self%restricted_complex = self%restricted
               case("unrestricted_complex")
                  self%alpha_complex = fac*self%restricted
                  self%beta_complex  = fac*self%restricted
               case("general_complex")
                  self%general_complex = 0.0d0
                  call alpha_alpha_set_to_(self%general_complex,self%restricted,fac)
                  call beta_beta_set_to_(self%general_complex,self%restricted,fac)
               case default
                  call die_(tonto,"OPMATRIX:convert_to ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
            end select
         case("unrestricted")
            select case (newkind)
               case("restricted")
                  self%restricted = fac*self%alpha
               case("general")
                  self%general = 0.0d0
                  call alpha_alpha_set_to_(self%general,self%alpha)
                  call beta_beta_set_to_(self%general,self%beta)
               case("unrestricted_complex")
                  self%alpha_complex = self%alpha
                  self%beta_complex  = self%beta
               case("general_complex")
                  self%general_complex = 0.0d0
                  call alpha_alpha_set_to_(self%general_complex,self%alpha)
                  call beta_beta_set_to_(self%general_complex,self%beta)
               case default
                  call die_(tonto,"OPMATRIX:convert_to ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
            end select
         case("general")
            select case (newkind)
               case("unrestricted")
                  self%alpha = alpha_alpha_(self%general)
                  self%beta  = beta_beta_(self%general)
               case("general_complex")
                  self%general_complex = self%general
               case default
                  call die_(tonto,"OPMATRIX:convert_to ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
            end select
         case default; call die_(tonto,"OPMATRIX:convert_to ... cant convert old kind "//trim(oldkind))
      end select
      call destroy_(self,oldkind)

   end subroutine

   subroutine convert_to_1(self,newkind,na,nb,quantization_axis)
    type(opmatrix_type) :: self
    ! Convert self (which is regarded to be a set of MO's) to a
    ! new basis kind "newkind"
     character(*) :: newkind
     integer(kind=kind(1)) :: na,nb
     real(kind=kind(1.0d0)), dimension(3), optional :: quantization_axis
     logical(kind=kind(.true.)) :: quantize
     character(128) :: oldkind
     integer(kind=kind(1)) :: d,ne
     real(kind=kind(1.0d0)) :: lam,tmp,ar,br,bi

     d = self%n_bf
     ne = na + nb
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
             self%general(  1:d,     1:2*nb-1:2) = self%restricted(:,   1:nb)
             self%general(d+1: ,     2:2*nb  :2) = self%restricted(:,   1:nb)
             self%general(  1:d,2*nb+1:ne      ) = self%restricted(:,nb+1:na)
             self%general(d+1: ,  ne+1:2*na    ) = self%restricted(:,nb+1:na)
             self%general(1:d  ,2*na+1:2*d -1:2) = self%restricted(:,na+1: d)
             self%general(d+1: ,2*na+2:2*d   :2) = self%restricted(:,na+1: d)
           case("restricted_complex")
             self%restricted_complex = self%restricted
           case("unrestricted_complex")
             self%alpha_complex = self%restricted
             self%beta_complex  = self%restricted
           case("general_complex")
             self%general_complex = 0.0d0
             self%general_complex(  1:d,     1:2*nb-1:2) = self%restricted(:,   1:nb)
             self%general_complex(d+1: ,     2:2*nb  :2) = self%restricted(:,   1:nb)
             self%general_complex(  1:d,2*nb+1:ne      ) = self%restricted(:,nb+1:na)
             self%general_complex(d+1: ,  ne+1:2*na    ) = self%restricted(:,nb+1:na)
             self%general_complex(  1:d,2*na+1:2*d -1:2) = self%restricted(:,na+1: d)
             self%general_complex(d+1: ,2*na+2:2*d   :2) = self%restricted(:,na+1: d)
           case default
             call die_(tonto,"OPMATRIX:convert_to_1 ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
         end select
       case("unrestricted")
         select case (newkind)
           case("restricted","alpha")
             self%restricted = self%alpha  ! Loss of information here
             call warn_(tonto,"OPMATRIX:convert_to_1 ... Conversion from unrestricted to restricted ignores beta orbitals")
           case("beta")
             self%restricted = self%beta   ! Loss of information here
             call warn_(tonto,"OPMATRIX:convert_to_1 ... Conversion from unrestricted to restricted ignores alpha orbitals")
           case("general")
             self%general = 0.0d0
             self%general(  1:d,     1:2*nb-1:2) = self%alpha(:,   1:nb)
             self%general(d+1: ,     2:2*nb  :2) = self%beta (:,   1:nb)
             self%general(  1:d,2*nb+1:ne      ) = self%alpha(:,nb+1:na)
             self%general(d+1: ,  ne+1:2*na    ) = self%beta (:,nb+1:na)
             self%general(  1:d,2*na+1:2*d -1:2) = self%alpha(:,na+1: d)
             self%general(d+1: ,2*na+2:2*d   :2) = self%beta (:,na+1: d)
           case("unrestricted_complex")
             self%alpha_complex = self%alpha
             self%beta_complex  = self%beta
           case("general_complex")
             self%general_complex = 0.0d0
             quantize = .false.
             if (present(quantization_axis)) &
               quantize = .not. equals_(quantization_axis,(/0.0d0,0.0d0,1.0d0/))
             if (quantize) then
               lam = norm_(quantization_axis)
               tmp = abs(quantization_axis(3)+lam)  ! always positive anyway
               if (equals_(tmp,0.0d0)) then
                  ar = 0.0d0
                  br = 1.0d0
                  bi = 0.0d0
               else
                  ar = sqrt(tmp/(2.0d0*lam))
                  br = 1.0d0/sqrt(2.0d0*lam*tmp)
                  bi = br
                  br = br*quantization_axis(1)
                  bi = bi*quantization_axis(2)
               end if
               self%general_complex(d+1: ,     1:  na) = cmplx(br,bi,kind=kind((1.0d0,1.0d0)))*self%alpha(:,   1:na)
               self%general_complex(  1:d,     1:  na) =           ar*self%alpha(:,   1:na)
               self%general_complex(d+1: ,  ne+1:nb+d) = cmplx(br,bi,kind=kind((1.0d0,1.0d0)))*self%alpha(:,na+1: d)
               self%general_complex(  1:d,  ne+1:nb+d) =           ar*self%alpha(:,na+1: d)
               br = -br
               self%general_complex(  1:d,  na+1:ne  ) = cmplx(br,bi,kind=kind((1.0d0,1.0d0)))*self%beta (:,   1:nb)
               self%general_complex(d+1: ,  na+1:ne  ) =           ar*self%beta (:,   1:nb)
               self%general_complex(  1:d,nb+d+1:    ) = cmplx(br,bi,kind=kind((1.0d0,1.0d0)))*self%beta (:,nb+1: d)
               self%general_complex(d+1: ,nb+d+1:    ) =           ar*self%beta (:,nb+1: d)
             else
               self%general_complex(  1:d,     1:na  ) = self%alpha(:,   1:na)
               self%general_complex(d+1: ,  na+1:ne  ) = self%beta (:,   1:nb)
               self%general_complex(  1:d,  ne+1:nb+d) = self%alpha(:,na+1:  )
               self%general_complex(d+1: ,nb+d+1:    ) = self%beta (:,nb+1:  )
             end if
    call flush_(stdout)
    call text_(stdout,"alpha orbs:"); call put_(stdout,self%alpha)
    call flush_(stdout)
    call text_(stdout,"beta  orbs:"); call put_(stdout,self%beta)
    call flush_(stdout)
    call text_(stdout,"cmplx orbs:"); call put_(stdout,self%general_complex)

           case default
             call die_(tonto,"OPMATRIX:convert_to_1 ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
         end select
       case("general")
         select case (newkind)
           case("unrestricted")
             call alpha_alpha_put_to_(self%general,self%alpha)
             call beta_beta_put_to_(self%general,self%beta)
             call warn_(tonto,"OPMATRIX:convert_to_1 ... Conversion from general to unrestricted not recommended!")
           case("general_complex")
             self%general_complex = self%general
           case default
             call die_(tonto,"OPMATRIX:convert_to_1 ... cant convert kind "//trim(oldkind)//" to kind "//trim(newkind))
         end select
       case default; call die_(tonto,"OPMATRIX:convert_to_1 ... cant convert old kind "//trim(oldkind))
     end select
     call destroy_(self,oldkind)

   end subroutine

   function l_compress(self,opmatkind) result(res)
    type(opmatrix_type) :: self
    ! Return the length of the compressed object
      integer(kind=kind(1)) :: res
      character(*), optional :: opmatkind
       character(128) :: k

      if (present(opmatkind)) then; k = opmatkind
      else;                    k = spinorbital_kind_(self)
      end if
      select case (k)
         case("restricted");           res = 0.50d0*self%n_bf*(self%n_bf+1)
         case("unrestricted");         res = self%n_bf*(self%n_bf+1)
         case("general");              res = self%n_bf*(2*self%n_bf+1)
         case("restricted_complex");   res = self%n_bf*self%n_bf
         case("unrestricted_complex"); res = 2*self%n_bf*self%n_bf
         case("general_complex");      res = 4*self%n_bf*self%n_bf
         case default;   call die_(tonto,"OPMATRIX:l_compress ... unknown kind, "//trim(k))
      end select

   end function

   subroutine compress(self,opmatkind)
    type(opmatrix_type) :: self
    ! Compress the opmatrix into a triangle or square vector to save space
      character(*), optional :: opmatkind
       character(128) :: k

      k = spinorbital_kind_(self)
      if (present(opmatkind)) k = opmatkind
      select case (k)
         case("restricted")
            call destroy_(self%triangle)
            call create_(self%triangle,l_compress_(self,k))
            call compress_to_triangle_(self%restricted,self%triangle)
            call destroy_(self%restricted)
         case("unrestricted")
            call destroy_(self%triangle)
            call create_(self%triangle,l_compress_(self,k))
            call compress_to_triangle_(self%alpha, alpha_(self%triangle))
            call compress_to_triangle_(self%beta, beta_(self%triangle))
            call destroy_(self%alpha)
            call destroy_(self%beta)
         case("general")
            call destroy_(self%triangle)
            call create_(self%triangle,l_compress_(self,k))
            call compress_to_triangle_(self%general,self%triangle)
         case("restricted_complex")
            call destroy_(self%square)
            call create_(self%square,l_compress_(self,k))
            call compress_to_square_(self%restricted_complex,self%square)
            call destroy_(self%restricted_complex)
         case("unrestricted_complex")
            call destroy_(self%square)
            call create_(self%square,l_compress_(self,k))
            call compress_to_square_(self%alpha_complex, alpha_(self%square))
            call compress_to_square_(self%beta_complex, beta_(self%square))
            call destroy_(self%alpha_complex)
            call destroy_(self%beta_complex)
         case("general_complex")
            call destroy_(self%square)
            call create_(self%square,l_compress_(self,k))
            call compress_to_square_(self%general_complex, self%square)
            call destroy_(self%general_complex)
      end select

   end subroutine

   subroutine uncompress(self)
    type(opmatrix_type) :: self
    ! Uncompress the opmatrix
      integer(kind=kind(1)) :: l_compress

      l_compress = -1
      if (associated(self%triangle)) l_compress = size(self%triangle)
      if (associated(self%square))   l_compress = size(self%square)
      if (l_compress==l_compress_(self,"restricted")) then
            call destroy_(self,"restricted")
            call create_(self,"restricted")
            call uncompress_from_triangle_(self%restricted,self%triangle)
            call destroy_(self%triangle)
      else if (l_compress==l_compress_(self,"unrestricted")) then
            call destroy_(self,"unrestricted")
            call create_(self,"unrestricted")
            call uncompress_from_triangle_(self%alpha, alpha_(self%triangle))
            call uncompress_from_triangle_(self%beta, beta_(self%triangle))
            call destroy_(self%triangle)
      else if (l_compress==l_compress_(self,"general")) then
            call destroy_(self,"general")
            call create_(self,"general")
            call uncompress_from_triangle_(self%general,self%triangle)
            call destroy_(self%triangle)
      else if (l_compress==l_compress_(self,"restricted_complex")) then
            call destroy_(self,"restricted_complex")
            call create_(self,"restricted_complex")
            call uncompress_from_square_(self%restricted_complex,self%square)
            call destroy_(self%square)
      else if (l_compress==l_compress_(self,"unrestricted_complex")) then
            call destroy_(self,"unrestricted_complex")
            call create_(self,"unrestricted_complex")
            call uncompress_from_square_(self%alpha_complex, alpha_(self%square))
            call uncompress_from_square_(self%beta_complex, beta_(self%square))
            call destroy_(self%square)
      else if (l_compress==l_compress_(self,"general_complex")) then
            call destroy_(self,"general_complex")
            call create_(self,"general_complex")
            call uncompress_from_square_(self%general_complex,self%square)
            call destroy_(self%square)
      end if

   end subroutine

   subroutine schmidt_orthonormalise(self,S,scale)
    type(opmatrix_type) :: self
    ! Schmidt Orthonormalise self.  (For molecular orbitals).
      real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: S
      real(kind=kind(1.0d0)), optional :: scale
      character(128) :: opmatkind
      integer(kind=kind(1)) :: n
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: SS

      opmatkind = spinorbital_kind_(self)
      select case(opmatkind)
         case("restricted")
                  call schmidt_orthonormalise_(self%restricted,S,scale)
         case("unrestricted")
                  call schmidt_orthonormalise_(self%alpha,S)
                  call schmidt_orthonormalise_(self%beta,S)
         case("alpha")
                  call schmidt_orthonormalise_(self%alpha,S,scale)
         case("beta")
                  call schmidt_orthonormalise_(self%beta,S,scale)
         case("general")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = 0.0d0
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call schmidt_orthonormalise_(self%general,SS,scale)
                  call destroy_(SS)
         case("restricted_complex")
                  call schmidt_orthonormalise_(self%restricted,S,scale)
         case("unrestricted_complex")
                  call schmidt_orthonormalise_(self%alpha_complex,S)
                  call schmidt_orthonormalise_(self%beta_complex,S)
         case("alpha_complex")
                  call schmidt_orthonormalise_(self%alpha_complex,S,scale)
         case("beta_complex")
                  call schmidt_orthonormalise_(self%beta_complex,S,scale)
         case("general_complex")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = 0.0d0
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call schmidt_orthonormalise_(self%general_complex,SS,scale)
                  call destroy_(SS)
         case default;   call die_(tonto,"OPMATRIX:schmidt_orthonormalise ... unknown kind, "//trim(opmatkind))
      end select

   end subroutine

   subroutine symmetrically_orthonormalise(self,S)
    type(opmatrix_type) :: self
    ! Symmetrically orthonormalise self.  (For molecular orbitals).
    ! "S" is the AO overlap matrix
       real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: S
      character(128) :: opmatkind
       integer(kind=kind(1)) :: n
      real(kind=kind(1.0d0)), dimension(:,:), pointer :: SS

      opmatkind = spinorbital_kind_(self)
      select case(opmatkind)
         case("restricted")
                  call symmetrically_orthonormalise_(self%restricted,S)
         case("unrestricted")
                  call symmetrically_orthonormalise_(self%alpha,S)
                  call symmetrically_orthonormalise_(self%beta,S)
         case("alpha")
                  call symmetrically_orthonormalise_(self%alpha,S)
         case("beta")
                  call symmetrically_orthonormalise_(self%beta,S)
         case("general")
                  call ensure_(tonto,size(SS,1)==2*size(S,1),"OPMATRIX:symmetrically_orthonormalise ... wrong size, S")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = 0.0d0
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call symmetrically_orthonormalise_(self%general,SS)
                  call destroy_(SS)
         case("restricted_complex")
                  call symmetrically_orthonormalise_(self%restricted,S)
         case("unrestricted_complex")
                  call symmetrically_orthonormalise_(self%alpha_complex,S)
                  call symmetrically_orthonormalise_(self%beta_complex,S)
         case("alpha_complex")
                  call symmetrically_orthonormalise_(self%alpha_complex,S)
         case("beta_complex")
                  call symmetrically_orthonormalise_(self%beta_complex,S)
         case("general_complex")
                  call ensure_(tonto,size(SS,1)==2*size(S,1),"OPMATRIX:symmetrically_orthonormalise ... wrong size, S")
                  n = 2*size(S,1)
                  call create_(SS,n,n)
                  SS = 0.0d0
                  call alpha_alpha_set_to_(SS,S)
                  call beta_beta_set_to_(SS,S)
                  call symmetrically_orthonormalise_(self%general_complex,SS)
                  call destroy_(SS)
         case default;   call die_(tonto,"OPMATRIX:symmetrically_orthonormalise ... unknown kind, "//trim(opmatkind))
      end select

   end subroutine

   subroutine plus(self,X)
    type(opmatrix_type) :: self
    ! Add X to self.
      type(opmatrix_type), intent(in) :: X
     character(128) :: opmatkind

   call ensure_(tonto,spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:plus ... incompatible kinds")
     opmatkind = spinorbital_kind_(X)
     select case(opmatkind)
       case("restricted")
         call plus_(self%restricted,X%restricted)
       case("unrestricted")
         call plus_(self%alpha,X%alpha)
         call plus_(self%beta,X%beta)
       case("alpha")
         call plus_(self%alpha,X%alpha)
       case("beta")
         call plus_(self%beta,X%beta)
       case("general")
         call plus_(self%general,X%general)
       case("restricted_complex")
         call plus_(self%restricted,X%restricted)
       case("unrestricted_complex")
         call plus_(self%alpha_complex,X%alpha_complex)
         call plus_(self%beta_complex,X%beta_complex)
       case("alpha_complex")
         call plus_(self%alpha_complex,X%alpha_complex)
       case("beta_complex")
         call plus_(self%beta_complex,X%beta_complex)
       case("general_complex")
         call plus_(self%general_complex,X%general_complex)
       case default;   call die_(tonto,"OPMATRIX:plus ... unknown kind, "//trim(opmatkind))
     end select

   end subroutine

   subroutine minus(self,X)
    type(opmatrix_type) :: self
    ! Subtract X from self.
      type(opmatrix_type), intent(in) :: X
     character(128) :: opmatkind

   call ensure_(tonto,spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:minus ... incompatible kinds")
     opmatkind = spinorbital_kind_(self)
     select case(opmatkind)
       case("restricted")
         call minus_(self%restricted,X%restricted)
       case("unrestricted")
         call minus_(self%alpha,X%alpha)
         call minus_(self%beta,X%beta)
       case("alpha")
         call minus_(self%alpha,X%alpha)
       case("beta")
         call minus_(self%beta,X%beta)
       case("general")
         call minus_(self%general,X%general)
       case("restricted_complex")
         call minus_(self%restricted,X%restricted)
       case("unrestricted_complex")
         call minus_(self%alpha_complex,X%alpha_complex)
         call minus_(self%beta_complex,X%beta_complex)
       case("alpha_complex")
         call minus_(self%alpha_complex,X%alpha_complex)
       case("beta_complex")
         call minus_(self%beta_complex,X%beta_complex)
       case("general_complex")
         call minus_(self%general_complex,X%general_complex)
       case default; call die_(tonto,"OPMATRIX:minus ... unknown spinorbital kind, " // trim(opmatkind))
     end select

   end subroutine

   subroutine to_scaled(self,fac,X)
    type(opmatrix_type) :: self
    ! Set self to scaled X.
      type(opmatrix_type), intent(in) :: X
     real(kind=kind(1.0d0)), intent(in) :: fac
     character(128) :: opmatkind

   call ensure_(tonto,spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:to_scaled ... incompatible kinds")
     opmatkind = spinorbital_kind_(self)
     select case(opmatkind)
       case("restricted")
         call to_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted")
         call to_scaled_mat_(self%alpha,fac,X%alpha)
         call to_scaled_mat_(self%beta,fac,X%beta)
       case("alpha")
         call to_scaled_mat_(self%alpha,fac,X%alpha)
       case("beta")
         call to_scaled_mat_(self%beta,fac,X%beta)
       case("general")
         call to_scaled_mat_(self%general,fac,X%general)
       case("restricted_complex")
         call to_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted_complex")
         call to_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
         call to_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("alpha_complex")
         call to_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
       case("beta_complex")
         call to_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("general_complex")
         call to_scaled_mat_(self%general_complex,fac,X%general_complex)
       case default;   call die_(tonto,"OPMATRIX:to_scaled ... unknown kind, "//trim(opmatkind))
     end select

   end subroutine

   subroutine plus_scaled(self,fac,X)
    type(opmatrix_type) :: self
    ! Set self to scaled X.
      type(opmatrix_type), intent(in) :: X
     real(kind=kind(1.0d0)), intent(in) :: fac
     character(128) :: opmatkind

   call ensure_(tonto,spinorbital_kind_(self)==spinorbital_kind_(X),"OPMATRIX:plus_scaled ... incompatible kinds")
     opmatkind = spinorbital_kind_(self)
     select case(opmatkind)
       case("restricted")
         call plus_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted")
         call plus_scaled_mat_(self%alpha,fac,X%alpha)
         call plus_scaled_mat_(self%beta,fac,X%beta)
       case("alpha")
         call plus_scaled_mat_(self%alpha,fac,X%alpha)
       case("beta")
         call plus_scaled_mat_(self%beta,fac,X%beta)
       case("general")
         call plus_scaled_mat_(self%general,fac,X%general)
       case("restricted_complex")
         call plus_scaled_mat_(self%restricted,fac,X%restricted)
       case("unrestricted_complex")
         call plus_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
         call plus_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("alpha_complex")
         call plus_scaled_mat_(self%alpha_complex,fac,X%alpha_complex)
       case("beta_complex")
         call plus_scaled_mat_(self%beta_complex,fac,X%beta_complex)
       case("general_complex")
         call plus_scaled_mat_(self%general_complex,fac,X%general_complex)
       case default;   call die_(tonto,"OPMATRIX:plus_scaled ... unknown kind, "//trim(opmatkind))
     end select

   end subroutine

   subroutine damp(self,old,mix)
    type(opmatrix_type) :: self
    ! Constructs the new self matrix by combining it with the "old" one using a
    ! "mix" factor, i.e.  self = (1-mix)*self + mix*old.
     intent(inout) :: self
     type(opmatrix_type), intent(in) :: old
     real(kind=kind(1.0d0)), intent(in) :: mix

     call to_scaled_(self,1.0d0-mix,self)
     call plus_scaled_(self,mix,old)

   end subroutine

   function expectation(self,X) result(res)
    type(opmatrix_type) :: self
    ! Get the expectation value of the matrix "X", i.e:
    !    res = Trace ( X  .self )
      real(kind=kind(1.0d0)), dimension(:,:) :: X
      real(kind=kind(1.0d0)) :: res
      character(128) :: opmatkind

   call ensure_(tonto,size(X,1)==self%n_bf,"OPMATRIX:expectation ... wrong X dimension")
   call ensure_(tonto,is_square_(X),"OPMATRIX:expectation ... X is not square")
      opmatkind = spinorbital_kind_(self)
      select case (opmatkind)
        case ("restricted")
           res = trace_of_product_(self%restricted,X)
        case ("unrestricted")
           res = trace_of_product_(self%alpha,X)
           res =  trace_of_product_(self%beta,X) + res
        case ("general_complex")
           res = trace_of_product_(self%general_complex,X)
        case default; call die_(tonto,"OPMATRIX:expectation ... not implemented, "//trim(opmatkind))
      end select

   end function

end
