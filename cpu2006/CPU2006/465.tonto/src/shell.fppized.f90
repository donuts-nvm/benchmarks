!-------------------------------------------------------------------------------
!
! SHELL: used to describe contracted cartesian gaussian shells.
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
! $Id: shell.foo,v 1.19.2.3 2003/11/13 05:33:21 reaper Exp $
!-------------------------------------------------------------------------------

module SHELL_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use REALVEC_MODULE, only: same_as_
   use REALVEC_MODULE, only: normalising_factors_
   use REALVEC_MODULE, only: create_
   use REALVEC_MODULE, only: create_copy_
   use REALVEC_MODULE, only: destroy_

   use INT_MODULE, only: double_factorial_
   use INT_MODULE, only: to_str_

   use STR_MODULE, only: is_int_
   use STR_MODULE, only: split_
   use STR_MODULE, only: to_lower_case_
   use STR_MODULE, only: to_int_
   use STR_MODULE, only: includes_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: set_default_units_
   use TEXTFILE_MODULE, only: next_str_
   use TEXTFILE_MODULE, only: skip_next_item_
   use TEXTFILE_MODULE, only: redirect_
   use TEXTFILE_MODULE, only: next_item_
   use TEXTFILE_MODULE, only: show_
   use TEXTFILE_MODULE, only: revert_
   use TEXTFILE_MODULE, only: put_
   use TEXTFILE_MODULE, only: read_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: read_ptr_
   use TEXTFILE_MODULE, only: move_to_previous_item_
   use TEXTFILE_MODULE, only: reverted_
   use TEXTFILE_MODULE, only: dash_

   use STRVEC_MODULE, only: create_copy_
   use STRVEC_MODULE, only: destroy_
   use STRVEC_MODULE, only: index_of_matching_bracket_

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

   public    read_keys_
   interface read_keys_
      module procedure read_keys
   end interface

   private    read_l_int_
   interface read_l_int_
      module procedure read_l_int
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    norm_
   interface norm_
      module procedure norm
   end interface

   public    read_ex_
   interface read_ex_
      module procedure read_ex
   end interface

   public    same_as_
   interface same_as_
      module procedure same_as
   end interface

   public    put_table_header_
   interface put_table_header_
      module procedure put_table_header
   end interface

   public    destroy_ptr_part_
   interface destroy_ptr_part_
      module procedure destroy_ptr_part
   end interface

   public    table_width_
   interface table_width_
      module procedure table_width
   end interface

   public    read_keywords_
   interface read_keywords_
      module procedure read_keywords
   end interface

   public    read_junk_ex_cc_
   interface read_junk_ex_cc_
      module procedure read_junk_ex_cc
   end interface

   public    unnormalise_
   interface unnormalise_
      module procedure unnormalise
   end interface

   public    process_keyword_
   interface process_keyword_
      module procedure process_keyword
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    process_keys_
   interface process_keys_
      module procedure process_keys
   end interface

   public    renormalise_
   interface renormalise_
      module procedure renormalise
   end interface

   public    put_table_footer_
   interface put_table_footer_
      module procedure put_table_footer
   end interface

   public    make_contraction_matrix_
   interface make_contraction_matrix_
      module procedure make_contraction_matrix
   end interface

   public    read_ex_cc_
   interface read_ex_cc_
      module procedure read_ex_cc
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

   public    set_
   interface set_
      module procedure set
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    clear_keys_
   interface clear_keys_
      module procedure clear_keys
   end interface

   private    read_l_chr_
   interface read_l_chr_
      module procedure read_l_chr
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    keys_created_
   interface keys_created_
      module procedure keys_created
   end interface

   public    set_keys_
   interface set_keys_
      module procedure set_keys
   end interface

   public    read_cc_
   interface read_cc_
      module procedure read_cc
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    read_junk_
   interface read_junk_
      module procedure read_junk
   end interface

   public    read_l_
   interface read_l_
      module procedure read_l
   end interface

   public    l_chr_
   interface l_chr_
      module procedure l_chr
   end interface

   public    read_units_
   interface read_units_
      module procedure read_units
   end interface

   public    set_n_comp_
   interface set_n_comp_
      module procedure set_n_comp
   end interface

   public    n_prim_
   interface n_prim_
      module procedure n_prim
   end interface

   public    read_n_cc_
   interface read_n_cc_
      module procedure read_n_cc
   end interface

   character(128), dimension(:), pointer, private :: keys => NULL()

contains

!  ******************
!  Allocation methods
!  ******************

   subroutine create(self)
    type(shell_type) :: self
    ! Create a shell object
      pointer :: self

      allocate(self)

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   subroutine destroy(self)
    type(shell_type) :: self
    ! Destroy a shell object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      call destroy_ptr_part_(self)

      deallocate(self)

   end subroutine

   subroutine nullify_ptr_part(self)
    type(shell_type) :: self
    ! Nullify the pointer part of a shell object

      nullify(self%ex)
      nullify(self%cc)

   end subroutine

   subroutine destroy_ptr_part(self)
    type(shell_type) :: self
    ! Destroy pointer part of a shell object

      call destroy_(self%ex)
      call destroy_(self%cc)

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

   subroutine create_copy(self,shell)
    type(shell_type) :: self
    ! Create a copy of "shell".
      pointer :: self
      type(shell_type) :: shell

      call create_(self)
      call copy_(self,shell)

   end subroutine

   subroutine copy(self,shell)
    type(shell_type) :: self
    ! Copy a shell object. Make sure pointer parts are nullified or
    ! destroyed, as you like, before using this.
      type(shell_type), intent(in) :: shell

      self = shell
      call create_copy_(self%ex,shell%ex)
      call create_copy_(self%cc,shell%cc)

   end subroutine

   subroutine set(self,shell)
    type(shell_type) :: self
    ! Set a shell object
      type(shell_type) :: shell

      self = shell

   end subroutine

   subroutine set_defaults(self)
    type(shell_type) :: self
    ! Set a default shell

      self%l = 0
      self%n_cc = 0
      self%n_comp = 0

   end subroutine

   subroutine set_n_comp(self)
    type(shell_type) :: self
    ! Set the number of components

      self%n_comp = (self%l+1)*(self%l+2)/2

   end subroutine

!   update ::: private
!   ! Update the shell data
!      call die_if_(tonto,.ex.destroyed,"no expononents")
!      call die_if_(tonto,.cc.destroyed,"no contractions")
!      call ensure_(tonto,stdin.default_units==" ","default units still set")
!   end

!   n_comp_sum result (res)
!   ! No. of cartesian components up to shell with momentum .l
!     res :: integer(kind=kind(1))
!     res = (.l+1)*(.l+2)*(.l+3)/6
!   end

!  ************
!  I/O Routines
!  ************

   recursive subroutine read_keywords(self)
    type(shell_type) :: self
    ! Read data from "stdin" using keyword style input.
    ! The following code is inherited from OBJECT
     character(128) :: word

     call ensure_(tonto,next_item_(stdin)=="{","SHELL:read_keywords ... expecting open bracket symbol, {")
     call read_(stdin,word)
     do                  ! Loop over input keywords
       call read_(stdin,word)
       call to_lower_case_(word)
       if (word=="}")      exit
       if (reverted_(stdin)) exit
       call process_keyword_(self,word)
     end do

   end subroutine

   subroutine process_keyword(self,keyword)
    type(shell_type) :: self
    ! Process command "keyword". Any required data needed by the "keyword" is
    ! inputted from "stdin".
     character(*), intent(in) :: keyword
     character(128) :: word

     word = keyword
     call to_lower_case_(word)
     select case (word)
       case ("}           ");  ! exit surrounding loop
       case ("cc=         "); call read_cc_(self)
       case ("ex=         "); call read_ex_(self)
       case ("ex,cc=      "); call read_ex_cc_(self)
       case ("junk,ex,cc= "); call read_junk_ex_cc_(self)
       case ("l=          "); call read_l_(self)
       case ("l_chr=      "); call read_l_chr_(self)
       case ("l_int=      "); call read_l_int_(self)
       case ("n_cc=       "); call read_n_cc_(self)
       case ("put         "); call put_(self)
       case ("units=      "); call read_units_(self)
        ! These are only for making custom tables for the list type
       case ("flush       "); call flush_(stdout)
       case ("put_l       "); call put_(stdout,self%l)
       case ("put_l_chr   "); call put_(stdout,l_chr_(self),int_width=.true.)
       case ("put_l_int   "); call put_(stdout,self%l)
       case ("put_n_cc    "); call put_(stdout,self%n_cc)
       case ("put_n_prim  "); call put_(stdout,n_prim_(self))
       case ("put_norm    "); call put_(stdout,norm_(self))
       case default;         allocate(tonto%known_keywords(18))
       tonto%known_keywords(1) = "}           "
       tonto%known_keywords(2) = "cc=         "
       tonto%known_keywords(3) = "ex=         "
       tonto%known_keywords(4) = "ex,cc=      "
       tonto%known_keywords(5) = "junk,ex,cc= "
       tonto%known_keywords(6) = "l=          "
       tonto%known_keywords(7) = "l_chr=      "
       tonto%known_keywords(8) = "l_int=      "
       tonto%known_keywords(9) = "n_cc=       "
       tonto%known_keywords(10) = "put         "
       tonto%known_keywords(11) = "units=      "
       tonto%known_keywords(12) = "flush       "
       tonto%known_keywords(13) = "put_l       "
       tonto%known_keywords(14) = "put_l_chr   "
       tonto%known_keywords(15) = "put_l_int   "
       tonto%known_keywords(16) = "put_n_cc    "
       tonto%known_keywords(17) = "put_n_prim  "
       tonto%known_keywords(18) = "put_norm    "
       call unknown_(tonto,word,"SHELL:process_keyword")
       deallocate(tonto%known_keywords)
     end select

   end subroutine

   subroutine read_units(self)
    type(shell_type) :: self
    ! Read a string which describes the units to be used
    ! The following code is inherited from OBJECT

      call set_default_units_(stdin,next_str_(stdin))

   end subroutine

   subroutine read_junk(self)
    type(shell_type) :: self
    ! Read in a junk string, useful for ignoring a field
    ! The following code is inherited from OBJECT

      call skip_next_item_(stdin)

   end subroutine

   subroutine read_l(self)
    type(shell_type) :: self
    ! Read in the l symbol
      character(128) :: word

      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else;                  call read_l_chr_(self)
      end if

   end subroutine

   subroutine read_l_int(self)
    type(shell_type) :: self
    ! Read in the l integer
      character(128) :: word

      call read_(stdin,word)
      call ensure_(tonto,is_int_(word),"SHELL:read_l_int ... expecting an integer for L")
      self%l = to_int_(word)
      call set_n_comp_(self)

   end subroutine

   subroutine read_l_chr(self)
    type(shell_type) :: self
    ! Read in the l symbol
      character(128) :: word
      character(1) :: l_c
      integer(kind=kind(1)) :: l

      call read_(stdin,word)
      call ensure_(tonto,len_trim(word)==1,"SHELL:read_l_chr ... unknown L symbol")
      l_c = word
      call to_lower_case_(l_c)
      select case (l_c)
         case ("s");   l = 0
         case ("p");   l = 1
         case ("d");   l = 2
         case ("f");   l = 3
         case ("g");   l = 4
         case default; l = 4 + iachar(l_c)-iachar("g")
      end select
      self%l = l
      call set_n_comp_(self)

   end subroutine

   subroutine read_n_cc(self)
    type(shell_type) :: self
    ! Read in the number of contraction coefficients

      call read_(stdin,self%n_cc)
      call ensure_(tonto,self%n_cc>0,"SHELL:read_n_cc ... n_cc must be positive")

   end subroutine

   subroutine read_ex(self)
    type(shell_type) :: self
    ! Read in the exponents

      call ensure_(tonto,self%n_cc>0,"SHELL:read_ex ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%ex),"SHELL:read_ex ... ex already entered")
      call create_(self%ex,self%n_cc)
      call read_(stdin,self%ex)

   end subroutine

   subroutine read_cc(self)
    type(shell_type) :: self
    ! Read in the contraction coefficients

      call ensure_(tonto,self%n_cc>0,"SHELL:read_cc ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%cc),"SHELL:read_cc ... cc already entered")
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%cc)

   end subroutine

   subroutine read_ex_cc(self)
    type(shell_type) :: self
    ! Read in the exponents and contractions

      call ensure_(tonto,self%n_cc>0,"SHELL:read_ex_cc ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%ex),"SHELL:read_ex_cc ... ex already entered")
      call ensure_(tonto,.not. associated(self%cc),"SHELL:read_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      call read_(stdin,self%ex,self%cc)

   end subroutine

   subroutine read_junk_ex_cc(self)
    type(shell_type) :: self
    ! Read in the exponents and contractions preceded by a junk string
      integer(kind=kind(1)) :: i

      call ensure_(tonto,self%n_cc>0,"SHELL:read_junk_ex_cc ... n_cc must be entered first")
      call ensure_(tonto,.not. associated(self%ex),"SHELL:read_junk_ex_cc ... ex already entered")
      call ensure_(tonto,.not. associated(self%cc),"SHELL:read_junk_ex_cc ... ex already entered")
      call create_(self%ex,self%n_cc)
      call create_(self%cc,self%n_cc)
      do i = 1,self%n_cc
         call skip_next_item_(stdin)
         call read_(stdin,self%ex(i))
         call read_(stdin,self%cc(i))
      end do

   end subroutine

!  ********************
!  Key related routines
!  ********************

   subroutine read_keys(self)
    type(shell_type) :: self
    ! Read the "keys".
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call read_ptr_(stdin,keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine process_keys(self)
    type(shell_type) :: self
    ! Process each of the words in the "keys" list.
    ! The following code is inherited from OBJECT
      integer(kind=kind(1)) :: k,l,n_key
      character(128) :: keyword
      character(128), dimension(:), pointer :: internal

      call ensure_(tonto,associated(keys),"SHELL:process_keys ... no keys")
      n_key = size(keys)
      k = 0
      do
         k = k + 1
         keyword = keys(k)
         if (keyword=="}") exit
         if (keyword=="{") then
            l = index_of_matching_bracket_(keys(k:),"{")
            call ensure_(tonto,l>0,"SHELL:process_keys ... no matching closing brace, }")
            internal => keys(k:k+l-1)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call revert_(stdin)
            k = k+l-1
         else if (includes_(keyword," ")) then
            internal => split_(keyword)
            call redirect_(stdin,internal)
            call read_keywords_(self)
            call destroy_(internal)
            call revert_(stdin)
         else
            call process_keyword_(self,keyword)
         end if
         if (k==n_key) exit
      end do

   end subroutine

   function keys_created(self) result(res)
    type(shell_type) :: self
    ! Return .true. if the list-element keys are created.
      logical(kind=kind(.true.)) :: res
    ! The following code is inherited from OBJECT

      res = associated(keys)

   end function

   subroutine set_keys(self,the_keys)
    type(shell_type) :: self
    ! This is for setting the "keys" externally.
     character(len=*), dimension(:) :: the_keys
    ! The following code is inherited from OBJECT

     call clear_keys_(self)
     call create_copy_(keys,the_keys)
     call ignore_memory_leak_(tonto,memory_blocks_gained=1)

   end subroutine

   subroutine clear_keys(self)
    type(shell_type) :: self
    ! This is for destroying the "keys" externally.
    ! The following code is inherited from OBJECT

     if (associated(keys)) then
       call destroy_(keys)
       call ignore_memory_leak_(tonto,memory_blocks_gained=-1)
     end if

   end subroutine

   subroutine put_table_footer(self)
    type(shell_type) :: self
    ! Output a table footer from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
    ! The following code is inherited from OBJECT

     call dash_(stdout,width=table_width_(self))

   end subroutine

   subroutine put_table_header(self)
    type(shell_type) :: self
    ! Output a table header from the list of "keys". NOTE: not all keywords need
    ! contribute to the banner - any unrecognised keyword is skipped.
     character(128) :: word
     integer(kind=kind(1)) :: width,k

     width = table_width_(self)

     if (width > 0) then
       call dash_(stdout,width=width)
       do k = 1,size(keys)
         word = keys(k)
         call to_lower_case_(word)
         select case (word)
           case ("flush       "); call flush_(stdout); exit
           case ("put_l       "); call put_(stdout,"l",int_width=.true.)
           case ("put_l_chr   "); call put_(stdout,"l",int_width=.true.)
           case ("put_l_int   "); call put_(stdout,"l",int_width=.true.)
           case ("put_n_cc    "); call put_(stdout,"n_cc",int_width=.true.)
           case ("put_n_prim  "); call put_(stdout,"n_prim",int_width=.true.)
           case ("put_norm    "); call put_(stdout,"norm")
           case default
         end select
         if (k==size(keys)) then
           call flush_(stdout)  ! In case they didn't write one.
           call warn_(tonto,"SHELL:put_table_header ... no flush keyword - you may later overrun the output buffer")
         end if
       end do
       call dash_(stdout,width=width)
     end if

   end subroutine

   function table_width(self) result(res)
    type(shell_type) :: self
    ! Return how wide a table is, based on "keys".  Note that not all keywords
    ! need to contribute to the banner - any unrecognised keyword is skipped.
     integer(kind=kind(1)) :: res
     integer(kind=kind(1)) :: int_dash,real_dash,k
     character(128) :: word

     int_dash = 0
     real_dash = 0
     call ensure_(tonto,associated(keys),"SHELL:table_width ... no keywords")
     do k = 1,size(keys)
       word = keys(k)
       call to_lower_case_(word)
       select case (word)
         case ("}           ");  ! exit surrounding loop
         case ("put_l       "); int_dash = int_dash + 1
         case ("put_l_chr   "); int_dash = int_dash + 1
         case ("put_l_int   "); int_dash = int_dash + 1
         case ("put_n_cc    "); int_dash = int_dash + 1
         case ("put_n_prim  "); int_dash = int_dash + 1
         case ("put_norm    "); real_dash = real_dash + 1
         case ("flush       "); exit
         case default
       end select
     end do
     res = int_dash * stdout%int_width + real_dash * stdout%real_width

   end function

!  *************
!  Input methods
!  *************

   function same_as(self,sh) result(same)
    type(shell_type) :: self
    ! Return .true. if the shell "self" is the same as "sh".
      type(shell_type) :: sh
      logical(kind=kind(.true.)) :: same

      same = self%l==sh%l .and. self%n_comp==sh%n_comp &
         .and. same_as_(self%ex,sh%ex) .and. same_as_(self%cc,sh%cc)

   end function

   function l_chr(self) result(res)
    type(shell_type) :: self
    ! Return a character representation for the angular mtm
      character(1) :: res
      integer(kind=kind(1)) :: l

      l = self%l
      select case (l)
         case (0); res="s"
         case (1); res="p"
         case (2); res="d"
         case (3); res="f"
         case (4); res="g"
         case default;
            call die_if_(tonto,l>23,"SHELL:l_chr ... angular momentum too large:"// trim(to_str_(l)))
            res = achar(l-4+iachar("g"))
      end select

   end function

   subroutine make_contraction_matrix(self,ccm)
    type(shell_type) :: self
    ! Return the contraction coefficient matrix. Size of ccm is (.n_prim,.n_comp).
      real(kind=kind(1.0d0)), dimension(:,:) :: ccm
      integer(kind=kind(1)) :: b,p,i
      real(kind=kind(1.0d0)) :: f
      real(kind=kind(1.0d0)), dimension(:), pointer :: fac

      call create_(fac, self%n_comp )
      call normalising_factors_(fac, self%l )
      do b = 1,self%n_comp        ! do over basis components
         p = b                ! primitive p
         f = fac(b)
         do i = 1,self%n_cc       ! do over contractions
            ccm(p,b) = f*self%cc(i)
            p = p + self%n_comp
         end do
      end do
      call destroy_(fac)

   end subroutine

   pure function norm(self) result(res)
    type(shell_type) :: self
    ! Return the norm of the shell, assuming that the existing contraction
    ! coefficients are with respect to NORMALISED gaussians
      intent(in) :: self
      real(kind=kind(1.0d0)) :: res
      integer(kind=kind(1)) :: i,j
      real(kind=kind(1.0d0)) :: sum,a,b,ab
      sum = 0.0d0
      do i = 1,self%n_cc
         a = self%ex(i)
         do j = 1,i-1
            b = self%ex(j)
            ab = 2.0d0*sqrt(a*b)/(a+b);
            sum = sum + 2.0d0*self%cc(i)*self%cc(j)*ab**(self%l+1.0d0+0.50d0);
         end do
         sum = sum + self%cc(i)*self%cc(i)
      end do
      res = sqrt(sum) * ((2.0d0*3.141592653589793d0)**(3.0d0/4.0d0))

   end function

   subroutine unnormalise(self)
    type(shell_type) :: self
    ! Unnormalise self as if all components in the shell were x^l, and also
    ! assuming the existing contraction coefficients are initially with respect
    ! to raw UNNORMALISED gaussians. It is up to you to correct this factor with
    ! appropriate double factorial square roots for other components.

       ! Take out the normalisation of each primitive.
       ! The double factorial should be: fac = 1/sqrt(df(nx)*df(ny)*df(nz))
       ! where n are the cartesian powers of the basis component

       self%cc(:) = self%cc(:) * (4.0d0*self%ex(:))**(0.50d0*self%l+0.50d0+0.25d0) &
                * (1.0d0/(norm_(self)*sqrt(double_factorial_(self%l))))

   end subroutine

   subroutine renormalise(self)
    type(shell_type) :: self
    ! Normalise self as if all components in the shell were x^l, and also
    ! assuming the existing contraction coefficients are with respect to raw
    ! unnormalised gaussians. This will undo routine "unnormalise".
       ! The ((2.0d0*3.141592653589793d0)**(3.0d0/4.0d0)) / .norm factor is to make the cc=1 for a
       ! shell with one primitive

      self%cc(:) = self%cc(:) / ((4.0d0*self%ex(:))**(0.50d0*self%l+0.50d0+0.25d0)) &
               * (sqrt(double_factorial_(self%l)) * ((2.0d0*3.141592653589793d0)**(3.0d0/4.0d0)) / norm_(self))

   end subroutine

   pure function n_prim(self) result(res)
    type(shell_type) :: self
    ! Return the number of primitive gaussians in the shell
      intent(in) :: self
      integer(kind=kind(1)) :: res
      res = self%n_comp*self%n_cc

   end function

   subroutine put(self)
    type(shell_type) :: self
    ! Put the shell information to "stdout"
       integer(kind=kind(1)) :: i

      call flush_(stdout)
      call show_(stdout,"L quantum number = ",self%l)
      call dash_(stdout,int_fields=1,real_fields=2)
      call put_(stdout,"N", int_width=.true.)
      call put_(stdout,"Exponents")
      call put_(stdout,"Contraction")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=2)
      do i = 1,self%n_cc
         call put_(stdout,i)
         call put_(stdout, self%ex(i))
         call put_(stdout, self%cc(i))
         call flush_(stdout)
      end do

   end subroutine

end
