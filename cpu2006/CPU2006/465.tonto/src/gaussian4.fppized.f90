!---------------------------------------------------------------------------
!
!  GAUSSIAN4 : Quartets of gaussian functions
!
! Copyright (C) Dylan Jayatilaka, 1999
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
! $Id: gaussian4.foo,v 1.19.2.1 2003/03/06 10:40:56 dylan Exp $
!
!---------------------------------------------------------------------------

module GAUSSIAN4_MODULE

   use TYPES_MODULE
   use SYSTEM_MODULE

   use INTVEC_MODULE, only: create_
   use INTVEC_MODULE, only: destroy_

   use TEXTFILE_MODULE, only: stdin
   use TEXTFILE_MODULE, only: stdout
   use TEXTFILE_MODULE, only: text_
   use TEXTFILE_MODULE, only: flush_
   use TEXTFILE_MODULE, only: show_

   use RYS_MODULE, only: get_weights_
   use RYS_MODULE, only: create_
   use RYS_MODULE, only: destroy_

   use INT_MODULE, only: make_gaussian_xyz_indices_

   use REALMAT5_MODULE, only: create_
   use REALMAT5_MODULE, only: destroy_

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

   public    make_ERI_derivatives_
   interface make_ERI_derivatives_
      module procedure make_ERI_derivatives
   end interface

   private    transfer_2d_ints_
   interface transfer_2d_ints_
      module procedure transfer_2d_ints
   end interface

   public    set_
   interface set_
      module procedure set
      module procedure set_1
      module procedure set_2
   end interface

   public    create_
   interface create_
      module procedure create
      module procedure create_1
   end interface

   public    differentiate_
   interface differentiate_
      module procedure differentiate
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface

   public    make_spin_orbit_ints_
   interface make_spin_orbit_ints_
      module procedure make_spin_orbit_ints
   end interface

   public    make_spin_spin_dipole_ints_
   interface make_spin_spin_dipole_ints_
      module procedure make_spin_spin_dipole_ints
   end interface

   public    put_
   interface put_
      module procedure put
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   public    make_ERI_ints_
   interface make_ERI_ints_
      module procedure make_ERI_ints
   end interface

   public    form_2d_ints_
   interface form_2d_ints_
      module procedure form_2d_ints
   end interface

   public    copy_
   interface copy_
      module procedure copy
   end interface

contains

   subroutine create(self)
    type(gaussian4_type) :: self
    ! Create
      pointer :: self

      nullify(self)
      allocate(self)

   end subroutine

   subroutine create_1(self,Ga,Gb,Gc,Gd)
    type(gaussian4_type) :: self
    ! Create and set to "Ga" ... "Gd"
      pointer :: self
      type(gaussian_type) :: Ga,Gb,Gc,Gd

      nullify(self)
      allocate(self)

      call set_(self,Ga,Gb,Gc,Gd)

   end subroutine

   subroutine destroy(self)
    type(gaussian4_type) :: self
    ! Destroy
      pointer :: self

      if (.not. associated(self)) then;   return; end if

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

   subroutine create_copy(self,G)
    type(gaussian4_type) :: self
    ! Create a copy of "G"
      pointer :: self
       type(gaussian4_type) :: G

      call create_(self)
      call copy_(self,G)

   end subroutine

   subroutine copy(self,G)
    type(gaussian4_type) :: self
    ! Set the gaussian quartet object to "G"
      type(gaussian4_type) :: G

      self = G

   end subroutine

   subroutine set(self,Ga,Gb,Gc,Gd)
    type(gaussian4_type) :: self
    ! Set the gaussian pair object to "Ga" and "Gb"
      type(gaussian_type) :: Ga,Gb,Gc,Gd

      self%a%l   = Ga%l
      self%a%pos = Ga%pos
      self%a%ex  = Ga%ex
      self%b%l   = Gb%l
      self%b%pos = Gb%pos
      self%b%ex  = Gb%ex
      self%c%l   = Gc%l
      self%c%pos = Gc%pos
      self%c%ex  = Gc%ex
      self%d%l   = Gd%l
      self%d%pos = Gd%pos
      self%d%ex  = Gd%ex

   end subroutine

   subroutine set_1(self,l_a,pos_a,ex_a,l_b,pos_b,ex_b,l_c,pos_c,ex_c,l_d,pos_d,ex_d)
    type(gaussian4_type) :: self
    ! Set a gaussian object
      integer(kind=kind(1)) :: l_a,l_b,l_c,l_d
      real(kind=kind(1.0d0)), dimension(3) :: pos_a,pos_b,pos_c,pos_d
      real(kind=kind(1.0d0)) :: ex_a,ex_b,ex_c,ex_d

      self%a%l   = l_a
      self%a%pos = pos_a
      self%a%ex  = ex_a
      self%b%l   = l_b
      self%b%pos = pos_b
      self%b%ex  = ex_b
      self%c%l   = l_c
      self%c%pos = pos_c
      self%c%ex  = ex_c
      self%d%l   = l_d
      self%d%pos = pos_d
      self%d%ex  = ex_d

   end subroutine

   subroutine set_2(self,ex_a,ex_b,ex_c,ex_d)
    type(gaussian4_type) :: self
    ! Set the exponents of the pair
      real(kind=kind(1.0d0)), optional :: ex_a,ex_b,ex_c,ex_d

      if (present(ex_a)) self%a%ex = ex_a
      if (present(ex_b)) self%b%ex = ex_b
      if (present(ex_c)) self%c%ex = ex_c
      if (present(ex_d)) self%d%ex = ex_d

   end subroutine

   subroutine make_ERI_ints(self,I)
    type(gaussian4_type) :: self
    ! Make ERI matrix "I" using Rys decomposition of 1/r_{12}.
       real(kind=kind(1.0d0)), dimension(:,:,:,:) :: I
      real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: Ix,Iy,Iz
      integer(kind=kind(1)), dimension(:), pointer :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      type(rys_type), pointer :: rys
      real(kind=kind(1.0d0)), dimension(3) :: AB,CD,P,Q,PA,QC,QP
      real(kind=kind(1.0d0)) :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      integer(kind=kind(1)) :: l_e,l_f,n_a,n_b,n_c,n_d,n_roots

      l_e = self%a%l + self%b%l
      l_f = self%c%l + self%d%l
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,self%b%l+1,l_f+1,self%d%l+1)
      call create_(Iy,n_roots,l_e+1,self%b%l+1,l_f+1,self%d%l+1)
      call create_(Iz,n_roots,l_e+1,self%b%l+1,l_f+1,self%d%l+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = 1.0d0/zeta
      einv = 1.0d0/eta
      zeinv = 1.0d0/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
       !!!!!!!!!!!!!!!!!!!!
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1), rys%r, rys%w,rho,zinv,einv,PA,QC,QP)
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD)
       ! Form the integrals
      I = sum(Ix(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)
       !!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = 34.98683665524973d0*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
      I = fac*I

   end subroutine

   pure subroutine transfer_2d_ints(self,Ix,Iy,Iz,AB,CD,max_b,max_d)
    type(gaussian4_type) :: self
    ! Use the transfer relation to put momenta on centres B and D to get all
    ! the 2d integrals "Ix", "Iy" and "Iz". If present, "max_b" and "max_d"
    ! are the maximum l-values desired for centers b and d, respectively.
    ! You must ensure that the "Ii" arrays are big enough, in this case.
      intent(in) :: self
      real(kind=kind(1.0d0)), dimension(:,:,:,:,:), intent(inout) :: Ix,Iy,Iz
      real(kind=kind(1.0d0)), dimension(3), intent(in) :: AB,CD
      integer(kind=kind(1)), intent(in), optional :: max_b,max_d
      integer(kind=kind(1)) :: le1,lf1,l_a,l_b,l_c,l_d,a,b,c,d,a1,b1,c1,d1,la1
      l_a = self%a%l; l_b = self%b%l; l_c = self%c%l; l_d = self%d%l
      if (present(max_b)) l_b=max_b
      if (present(max_d)) l_d=max_d
      if (l_b==0 .and. l_d==0) then;  return; end if
      if (l_b/=0) then
         le1 = l_a + l_b + 1
         do b = 1,l_b
         do a = 1,le1 - b
            b1 = b   + 1
            a1 = a   + 1
            Ix(:,a,b1,:,1) = Ix(:,a1,b,:,1) + AB(1)*Ix(:,a,b,:,1)
            Iy(:,a,b1,:,1) = Iy(:,a1,b,:,1) + AB(2)*Iy(:,a,b,:,1)
            Iz(:,a,b1,:,1) = Iz(:,a1,b,:,1) + AB(3)*Iz(:,a,b,:,1)
         end do
         end do
      end if
      if (l_d/=0) then
       !  la1 = l_a + 1
         la1 = l_a + l_b - self%b%l + 1  ! increase more along a as for b
         lf1 = l_c + l_d + 1
         do d = 1,l_d
         do c = 1,lf1 - d
            d1 = d   + 1
            c1 = c   + 1
            Ix(:,1:la1,:,c,d1) = Ix(:,1:la1,:,c1,d) + CD(1)*Ix(:,1:la1,:,c,d)
            Iy(:,1:la1,:,c,d1) = Iy(:,1:la1,:,c1,d) + CD(2)*Iy(:,1:la1,:,c,d)
            Iz(:,1:la1,:,c,d1) = Iz(:,1:la1,:,c1,d) + CD(3)*Iz(:,1:la1,:,c,d)
         end do
         end do
      end if

   end subroutine

   pure subroutine form_2d_ints(self,Ix,Iy,Iz,t2,wt,rho,zinv,einv,PA,QC,QP,max_e,max_f)
    type(gaussian4_type) :: self
    ! Forms the two dimensional integrals "Ix", "Iy" and "Iz" with momenta only
    ! on centres A and C, using Rys roots "t2" and weights "wt".
    ! Other variables are intermediates, to avoid calculations: see make_ERI_ints.
    ! If present, "max_e" and "max_f" are used for the maximum angular momenta
    ! of centers (a+b) and (c+d) respectively -- for derivative integrals.
     intent(in) :: self
     real(kind=kind(1.0d0)), dimension(:,:,:), intent(inout) :: Ix,Iy,Iz
     real(kind=kind(1.0d0)), dimension(:), intent(in) :: t2,wt
     real(kind=kind(1.0d0)), intent(in) :: rho,zinv,einv
     real(kind=kind(1.0d0)), dimension(3), intent(in) :: PA,QC,QP
     integer(kind=kind(1)), intent(in), optional :: max_e,max_f
     real(kind=kind(1.0d0)), dimension(size(t2)) :: ret,rzt,ce,cf,bb,ce1,cf1
     integer(kind=kind(1)) :: l_e,l_f,e,f,e1,f1,fp1,ep1,k
     l_e = self%a%l + self%b%l
     l_f = self%c%l + self%d%l
     if (present(max_e)) l_e = max_e
     if (present(max_f)) l_f = max_f
     Ix(:,1,1) = 1.0d0
     Iy(:,1,1) = 1.0d0
     Iz(:,1,1) = 1.0d0
     if (l_e/=0 .or. l_f/=0) then
       if (l_f>0) then
         ret = rho*t2*einv
         Ix(:,1,2) = QC(1) - ret(:) * QP(1)
         Iy(:,1,2) = QC(2) - ret(:) * QP(2)
         Iz(:,1,2) = QC(3) - ret(:) * QP(3)
         if (l_f>1) then
           cf = (1.0d0 - ret) * 0.50d0 * einv
           do f = 2,l_f
             f1  = f - 1
             fp1 = f + 1
             cf1 = f1 * cf
             Ix(:,1,fp1) = Ix(:,1,2) * Ix(:,1,f) + cf1 * Ix(:,1,f1)
             Iy(:,1,fp1) = Iy(:,1,2) * Iy(:,1,f) + cf1 * Iy(:,1,f1)
             Iz(:,1,fp1) = Iz(:,1,2) * Iz(:,1,f) + cf1 * Iz(:,1,f1)
           end do
         end if
       end if
       if (l_e>0) then
         rzt = rho * t2 * zinv
         Ix(:,2,1) = PA(1) + rzt(:) * QP(1)
         Iy(:,2,1) = PA(2) + rzt(:) * QP(2)
         Iz(:,2,1) = PA(3) + rzt(:) * QP(3)
         if (l_e>1) then
           ce = (1.0d0 - rzt) * 0.50d0 * zinv
           do e = 2, l_e
             e1  = e - 1
             ep1 = e + 1
             ce1 = e1 * ce
             Ix(:,ep1,1) = Ix(:,2,1) * Ix(:,e,1) + ce1 * Ix(:,e1,1)
             Iy(:,ep1,1) = Iy(:,2,1) * Iy(:,e,1) + ce1 * Iy(:,e1,1)
             Iz(:,ep1,1) = Iz(:,2,1) * Iz(:,e,1) + ce1 * Iz(:,e1,1)
           end do
         end if
       end if
       if (l_f>0 .and. l_e>0) then
         bb = 0.50d0*einv*rzt
         Ix(:,2,2)=Ix(:,1,2)*Ix(:,2,1)+bb
         Iy(:,2,2)=Iy(:,1,2)*Iy(:,2,1)+bb
         Iz(:,2,2)=Iz(:,1,2)*Iz(:,2,1)+bb
         if (l_f>1) then
           do f=2,l_f
             f1  = f - 1
             fp1 = f + 1
             cf1 = f1 * cf
             Ix(:,2,fp1) = Ix(:,1,2) * Ix(:,2,f) + cf1 * Ix(:,2,f1) + bb *Ix(:,1,f)
             Iy(:,2,fp1) = Iy(:,1,2) * Iy(:,2,f) + cf1 * Iy(:,2,f1) + bb *Iy(:,1,f)
             Iz(:,2,fp1) = Iz(:,1,2) * Iz(:,2,f) + cf1 * Iz(:,2,f1) + bb *Iz(:,1,f)
           end do
         end if
         if (l_e>1) then
           do e = 2, l_e
             e1  =e - 1
             ep1 =e + 1
             ce1 =e1 * ce
             do f=2, l_f + 1
               f1 = f - 1
               Ix(:,ep1,f) = Ix(:,2,1)*Ix(:,e,f)+ce1*Ix(:,e1,f)+f1*bb(:)*Ix(:,e,f1)
               Iy(:,ep1,f) = Iy(:,2,1)*Iy(:,e,f)+ce1*Iy(:,e1,f)+f1*bb(:)*Iy(:,e,f1)
               Iz(:,ep1,f) = Iz(:,2,1)*Iz(:,e,f)+ce1*Iz(:,e1,f)+f1*bb(:)*Iz(:,e,f1)
             end do
           end do
         end if
       end if
     end if
      ! Multiply Iz by the weight
     do k = 1,size(Iz,1)
        Iz(k,:,:) = Iz(k,:,:)*wt(k)
     end do

   end subroutine

   subroutine make_spin_orbit_ints(self,Sx,Sy,Sz,Ox,Oy,Oz)
    type(gaussian4_type) :: self
    ! Make the same-spin orbit integrals "Sx" "Sy" "Sz"  and the
    ! other spin orbit integrals  "Ox" "Oy" "Oz" using Rys method.
    ! Reference: Bearpark et al., Mol. Phys. 80, p. 479 (1993)
      real(kind=kind(1.0d0)), dimension(:,:,:,:) :: Sx,Sy,Sz,Ox,Oy,Oz
      real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: Ix,Iy,Iz,LLx,LLy,LLz,RRx,RRy,RRz
      integer(kind=kind(1)), dimension(:), pointer :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      type(rys_type), pointer :: rys
      real(kind=kind(1.0d0)), dimension(3) :: AB,CD,P,Q,PA,QC,QP
      real(kind=kind(1.0d0)) :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      integer(kind=kind(1)) :: l_e,l_f,l_a,l_b,l_c,l_d,n_a,n_b,n_c,n_d,n_roots

      l_a = self%a%l + 1; l_b = self%b%l + 1
      l_c = self%c%l + 1; l_d = self%d%l + 1
      l_e = self%a%l + self%b%l + 1; l_f = self%c%l + self%d%l + 1  ! One higherfor differentiating
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)    ! Basic intermediate integrals
      call create_(Iy,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      call create_(Iz,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = 1.0d0/zeta
      einv = 1.0d0/eta
      zeinv = 1.0d0/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Ix = 0.0d0; Iy = 0.0d0; Iz = 0.0d0
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1), rys%r, rys%w,rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                             ! one unit higher for differentiating
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD,max_b=l_b,max_d=l_d)  ! a & c will automatically be one unit higher
      call create_(LLx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
      call create_(LLy,n_roots,l_a,l_b,l_c,l_d)
      call create_(LLz,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRx,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRy,n_roots,l_a,l_b,l_c,l_d)
      call create_(RRz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,Ix,"a",LLx); call differentiate_(self,Iy,"a",LLy); call differentiate_(self,Iz,"a",LLz)
      call differentiate_(self,Ix,"b",RRx); call differentiate_(self,Iy,"b",RRy); call differentiate_(self,Iz,"b",RRz)
       ! Form the same-spin orbit integrals

      Sx = sum(Ix(:,ax,bx,cx,dx)*(LLy(:,ay,by,cy,dy)*RRz(:,az,bz,cz,dz)-RRy(:,ay,by,cy,dy)*LLz(:,az,bz,cz,dz)),dim=1)
      Sy = sum(Iy(:,ay,by,cy,dy)*(LLz(:,az,bz,cz,dz)*RRx(:,ax,bx,cx,dx)-RRz(:,az,bz,cz,dz)*LLx(:,ax,bx,cx,dx)),dim=1)
      Sz = sum(Iz(:,az,bz,cz,dz)*(LLx(:,ax,bx,cx,dx)*RRy(:,ay,by,cy,dy)-RRx(:,ax,bx,cx,dx)*LLy(:,ay,by,cy,dy)),dim=1)

!     do d=1,n_d
!       dix=dx(d)
!       diy=dy(d)
!       diz=dz(d)
!       do c=1,n_c
!         cix=cx(c)
!         ciy=cy(c)
!         ciz=cz(c)
!         do b=1,n_b
!           bix=bx(b)
!           biy=by(b)
!           biz=bz(b)
!           do a=1,n_a
!             aix=ax(a)
!             aiy=ay(a)
!             aiz=az(a)
!             Sxn=0.0d0
!             Syn=0.0d0
!             Szn=0.0d0
!             do n=1,n_roots
!               LLxn = LLx(n,aix,bix,cix,dix)
!               LLyn = LLy(n,aiy,biy,ciy,diy)
!               LLzn = LLz(n,aiz,biz,ciz,diz)
!               RRxn = RRx(n,aix,bix,cix,dix)
!               RRyn = RRy(n,aiy,biy,ciy,diy)
!               RRzn = RRz(n,aiz,biz,ciz,diz)
!               Sxn = Sxn + Ix(n,aix,bix,cix,dix)*(LLyn*RRzn - RRyn*LLzn)
!               Syn = Syn + Iy(n,aiy,biy,ciy,diy)*(LLzn*RRxn - RRzn*LLxn)
!               Szn = Szn + Iz(n,aiz,biz,ciz,diz)*(LLxn*RRyn - RRxn*LLyn)
!             end
!             Sx(a,b,c,d)=Sxn
!             Sy(a,b,c,d)=Syn
!             Sz(a,b,c,d)=Szn
!           end
!         end
!       end
!     end

      call differentiate_(self,Ix,"c",LLx); call differentiate_(self,Iy,"c",LLy); call differentiate_(self,Iz,"c",LLz)
      call differentiate_(self,Ix,"d",RRx); call differentiate_(self,Iy,"d",RRy); call differentiate_(self,Iz,"d",RRz)
       ! Form the other spin orbit integrals

      Ox = sum(Ix(:,ax,bx,cx,dx)*(LLy(:,ay,by,cy,dy)*RRz(:,az,bz,cz,dz)-RRy(:,ay,by,cy,dy)*LLz(:,az,bz,cz,dz)),dim=1)
      Oy = sum(Iy(:,ay,by,cy,dy)*(LLz(:,az,bz,cz,dz)*RRx(:,ax,bx,cx,dx)-RRz(:,az,bz,cz,dz)*LLx(:,ax,bx,cx,dx)),dim=1)
      Oz = sum(Iz(:,az,bz,cz,dz)*(LLx(:,ax,bx,cx,dx)*RRy(:,ay,by,cy,dy)-RRx(:,ax,bx,cx,dx)*LLy(:,ay,by,cy,dy)),dim=1)

!     do d=1,n_d
!       dix=dx(d)
!       diy=dy(d)
!       diz=dz(d)
!       do c=1,n_c
!         cix=cx(c)
!         ciy=cy(c)
!         ciz=cz(c)
!         do b=1,n_b
!           bix=bx(b)
!           biy=by(b)
!           biz=bz(b)
!           do a=1,n_a
!             aix=ax(a)
!             aiy=ay(a)
!             aiz=az(a)
!             Sxn=0.0d0
!             Syn=0.0d0
!             Szn=0.0d0
!             do n=1,n_roots
!               LLxn = LLx(n,aix,bix,cix,dix)
!               LLyn = LLy(n,aiy,biy,ciy,diy)
!               LLzn = LLz(n,aiz,biz,ciz,diz)
!               RRxn = RRx(n,aix,bix,cix,dix)
!               RRyn = RRy(n,aiy,biy,ciy,diy)
!               RRzn = RRz(n,aiz,biz,ciz,diz)
!               Oxn = Oxn + Ix(n,aix,bix,cix,dix)*(LLyn*RRzn - RRyn*LLzn)
!               Oyn = Oyn + Iy(n,aiy,biy,ciy,diy)*(LLzn*RRxn - RRzn*LLxn)
!               Ozn = Ozn + Iz(n,aiz,biz,ciz,diz)*(LLxn*RRyn - RRxn*LLyn)
!             end
!             Ox(a,b,c,d)=Oxn
!             Oy(a,b,c,d)=Oyn
!             Oz(a,b,c,d)=Ozn
!           end
!         end
!       end
!     end

      call destroy_(RRz); call destroy_(RRy); call destroy_(RRx)
      call destroy_(LLz); call destroy_(LLy); call destroy_(LLx)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = 34.98683665524973d0*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
      Sx = fac*Sx; Sy = fac*Sy; Sz = fac*Sz
      Ox = fac*Ox; Oy = fac*Oy; Oz = fac*Oz

   end subroutine

   subroutine differentiate(self,I,index,ID)
    type(gaussian4_type) :: self
    ! Differentiate a gaussian-integral matrix "I" with respect to the
    ! *nuclear* coordinate on basis function "index", which can be
    ! "a" "b" "c" or "d" depending on which center is differntiated.
    ! Place the result in "ID".
      real(kind=kind(1.0d0)), dimension(:,:,:,:,:) :: I,ID
      character(*) :: index
      integer(kind=kind(1)) :: i_a,i_b,i_c,i_d,d_a,d_b,d_c,d_d,a,b,c,d
      real(kind=kind(1.0d0)) :: a2,b2,c2,d2

      i_a = ubound(I,2);  i_b = ubound(I,3);  i_c = ubound(I,4);  i_d = ubound(I,5)
      d_a = ubound(ID,2); d_b = ubound(ID,3); d_c = ubound(ID,4); d_d = ubound(ID,5)
      ID = 0.0d0
      select case (index)
         case("a")
            call ensure_(tonto,i_a>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            call ensure_(tonto,i_a>d_a,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            a2 = 2.0d0*self%a%ex
            ID(:,1,:,:,:) = a2*I(:,2  ,1:d_b,1:d_c,1:d_d)
            do a = 2,d_a
            ID(:,a,:,:,:) = a2*I(:,a+1,1:d_b,1:d_c,1:d_d) - (a-1)*I(:,a-1,1:d_b,1:d_c,1:d_d)
            end do
         case("b")
            call ensure_(tonto,i_b>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            call ensure_(tonto,i_b>d_b,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            b2 = 2.0d0*self%b%ex
            ID(:,:,1,:,:) = b2*I(:,1:d_a,2  ,1:d_c,1:d_d)
            do b = 2,d_b
            ID(:,:,b,:,:) = b2*I(:,1:d_a,b+1,1:d_c,1:d_d) - (b-1)*I(:,1:d_a,b-1,1:d_c,1:d_d)
            end do
         case("c")
            call ensure_(tonto,i_c>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            call ensure_(tonto,i_c>d_c,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            c2 = 2.0d0*self%c%ex
            ID(:,:,:,1,:) = c2*I(:,1:d_a,1:d_b,2  ,1:d_d)
            do c = 2,d_c
            ID(:,:,:,c,:) = c2*I(:,1:d_a,1:d_b,c+1,1:d_d) - (c-1)*I(:,1:d_a,1:d_b,c-1,1:d_d)
            end do
         case("d")
            call ensure_(tonto,i_d>1,"GAUSSIAN4:differentiate ... I array too small to differentiate")
            call ensure_(tonto,i_d>d_d,"GAUSSIAN4:differentiate ... I and ID arrays are incompatible")
            d2 = 2.0d0*self%d%ex
            ID(:,:,:,:,1) = d2*I(:,1:d_a,1:d_b,1:d_c,2  )
            do d = 2,d_d
            ID(:,:,:,:,d) = d2*I(:,1:d_a,1:d_b,1:d_c,d+1) - (d-1)*I(:,1:d_a,1:d_b,1:d_c,d-1)
            end do
      end select

   end subroutine

   subroutine make_spin_spin_dipole_ints(self,Dxx,Dyy,Dzz,Dxy,Dxz,Dyz)
    type(gaussian4_type) :: self
    ! Make the spin spin magnetic dipole-dipole integrals "Dij" using Rys method.
    ! Reference: None. But see Bearpark et al., Mol. Phys. 80, p. 479 (1993) for
    ! inspiration.
      real(kind=kind(1.0d0)), dimension(:,:,:,:) :: Dxx,Dyy,Dzz,Dxy,Dxz,Dyz
      real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: Ix,Iy,Iz,Lx,Ly,Lz,Rx,Ry,Rz,LL,LR,RR
      integer(kind=kind(1)), dimension(:), pointer :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      type(rys_type), pointer :: rys
      real(kind=kind(1.0d0)), dimension(3) :: AB,CD,P,Q,PA,QC,QP
      real(kind=kind(1.0d0)) :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      real(kind=kind(1.0d0)) :: Dxx_abcd,Dyy_abcd,Dzz_abcd,Dxy_abcd,Dxz_abcd,Dyz_abcd,Ixyn,Ixzn,Iyzn
      real(kind=kind(1.0d0)) :: Lxn,Lyn,Lzn,Ixn,Iyn,Izn,Rxn,Ryn,Rzn
      integer(kind=kind(1)) :: l_e,l_f,l_a,l_b,l_c,l_d,n_a,n_b,n_c,n_d,n_roots
      integer(kind=kind(1)) :: a,b,c,d,n,aix,aiy,aiz,bix,biy,biz,cix,ciy,ciz,dix,diy,diz

      l_a = self%a%l + 1; l_b = self%b%l + 1
      l_c = self%c%l + 1; l_d = self%d%l + 1
      l_e = self%a%l + self%b%l + 2; l_f = self%c%l + self%d%l     ! Two higher for differentiating A, B.
      n_roots = (l_e+l_f+2)/2
      call create_(Ix,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)   ! Basic intermediate integrals
      call create_(Iy,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)
      call create_(Iz,n_roots,l_e+1,l_b+2,l_f+1,l_d+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = 1.0d0/zeta
      einv = 1.0d0/eta
      zeinv = 1.0d0/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Ix = 0.0d0; Iy = 0.0d0; Iz = 0.0d0
      call form_2d_ints_(self,Ix(:,:,1,:,1),Iy(:,:,1,:,1),Iz(:,:,1,:,1), rys%r, rys%w,rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                     ! two units higher on B for differentiating
      call transfer_2d_ints_(self,Ix,Iy,Iz,AB,CD,max_b=l_b+1)  ! A will automatically be 2 units higher
      call create_(Lx,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Ly,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Lz,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Rx,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Ry,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(Rz,n_roots,l_a+1,l_b+1,l_c,l_d)
      call create_(LL,n_roots,l_a  ,l_b  ,l_c,l_d)
      call create_(LR,n_roots,l_a  ,l_b  ,l_c,l_d)
      call create_(RR,n_roots,l_a  ,l_b  ,l_c,l_d)
      call differentiate_(self,Ix,"a",Lx); call differentiate_(self,Ix,"b",Rx)
      call differentiate_(self,Iy,"a",Ly); call differentiate_(self,Iy,"b",Ry)
      call differentiate_(self,Iz,"a",Lz); call differentiate_(self,Iz,"b",Rz)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call differentiate_(self,Lx,"a",LL); call differentiate_(self,Rx,"b",RR)
      call differentiate_(self,Lx,"b",LR); LR = 2.0d0*LR

!     Dxx = sum(LL(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     LR(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     RR(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dxx_abcd = 0.0d0
              do n=1,n_roots
                Iyzn = Iy(n,aiy,biy,ciy,diy) * Iz(n,aiz,biz,ciz,diz)
                Dxx_abcd = Dxx_abcd + Iyzn * (LL(n,aix,bix,cix,dix) + &
                                              LR(n,aix,bix,cix,dix) + &
                                              RR(n,aix,bix,cix,dix))
              end do
              Dxx(a,b,c,d) = Dxx_abcd
            end do
          end do
        end do
      end do

      call differentiate_(self,Ly,"a",LL); call differentiate_(self,Ry,"b",RR)
      call differentiate_(self,Ly,"b",LR); LR = 2.0d0*LR

!     Dyy = sum(Ix(:,ax,bx,cx,dx)*LL(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*LR(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*RR(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dyy_abcd = 0.0d0
              do n=1,n_roots
                Ixzn = Ix(n,aix,bix,cix,dix) * Iz(n,aiz,biz,ciz,diz)
                Dyy_abcd = Dyy_abcd + Ixzn * (LL(n,aiy,biy,ciy,diy) + &
                                              LR(n,aiy,biy,ciy,diy) + &
                                              RR(n,aiy,biy,ciy,diy))
              end do
              Dyy(a,b,c,d) = Dyy_abcd
            end do
          end do
        end do
      end do

      call differentiate_(self,Lz,"a",LL); call differentiate_(self,Rz,"b",RR)
      call differentiate_(self,Lz,"b",LR); LR = 2.0d0*LR

!     Dzz = sum(Ix(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*LL(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*LR(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Iz(:,ay,by,cy,dy)*RR(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dzz_abcd = 0.0d0
              do n=1,n_roots
                Ixyn = Ix(n,aix,bix,cix,dix) * Iy(n,aiy,biy,ciy,diy)
                Dzz_abcd = Dzz_abcd + Ixyn * (LL(n,aiz,biz,ciz,diz) + &
                                              LR(n,aiz,biz,ciz,diz) + &
                                              RR(n,aiz,biz,ciz,diz))
              end do
              Dzz(a,b,c,d) = Dzz_abcd
            end do
          end do
        end do
      end do

      call destroy_(RR); call destroy_(LR); call destroy_(LL)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!     Dxy = sum(Lx(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Lx(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Iz(:,az,bz,cz,dz),dim=1)
!     Dxz = sum(Lx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Lx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Rx(:,ax,bx,cx,dx)*Iy(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz),dim=1)
!     Dyz = sum(Ix(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Ly(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Lz(:,az,bz,cz,dz) &
!         +     Ix(:,ax,bx,cx,dx)*Ry(:,ay,by,cy,dy)*Rz(:,az,bz,cz,dz),dim=1)

      do d=1,n_d
        dix=dx(d)
        diy=dy(d)
        diz=dz(d)
        do c=1,n_c
          cix=cx(c)
          ciy=cy(c)
          ciz=cz(c)
          do b=1,n_b
            bix=bx(b)
            biy=by(b)
            biz=bz(b)
            do a=1,n_a
              aix=ax(a)
              aiy=ay(a)
              aiz=az(a)
              Dxy_abcd = 0.0d0
              Dxz_abcd = 0.0d0
              Dyz_abcd = 0.0d0
              do n=1,n_roots
                Lxn = Lx(n,aix,bix,cix,dix)
                Lyn = Ly(n,aiy,biy,ciy,diy)
                Lzn = Lz(n,aiz,biz,ciz,diz)
                Ixn = Ix(n,aix,bix,cix,dix)
                Iyn = Iy(n,aiy,biy,ciy,diy)
                Izn = Iz(n,aiz,biz,ciz,diz)
                Rxn = Rx(n,aix,bix,cix,dix)
                Ryn = Ry(n,aiy,biy,ciy,diy)
                Rzn = Rz(n,aiz,biz,ciz,diz)
                Dxy_abcd = Dxy_abcd + Izn * (Lxn*Lyn + Lxn*Ryn + Rxn*Lyn + Rxn*Ryn)
                Dxz_abcd = Dxz_abcd + Iyn * (Lxn*Lzn + Lxn*Rzn + Rxn*Lzn + Rxn*Rzn)
                Dyz_abcd = Dyz_abcd + Ixn * (Lyn*Lzn + Lyn*Rzn + Ryn*Lzn + Ryn*Rzn)
              end do
              Dxy(a,b,c,d) = Dxy_abcd
              Dxz(a,b,c,d) = Dxz_abcd
              Dyz(a,b,c,d) = Dyz_abcd
            end do
          end do
        end do
      end do

       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(Rz); call destroy_(Ry); call destroy_(Rx)
      call destroy_(Lz); call destroy_(Ly); call destroy_(Lx)
      call destroy_(rys)
      call destroy_(Iz); call destroy_(Iy); call destroy_(Ix)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = 34.98683665524973d0*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
      Dxx = fac*Dxx; Dyy = fac*Dyy; Dzz = fac*Dzz
      Dxy = fac*Dxy; Dxz = fac*Dxz; Dyz = fac*Dyz

   end subroutine

   subroutine make_ERI_derivatives(self,AA,BB,CC,DD)
    type(gaussian4_type) :: self
    ! Make the ERI gradient integrals "AA", "BB", "CC" and "DD". The last dimension
    ! determines whether the derivative is x, y, or z.
      real(kind=kind(1.0d0)), dimension(:,:,:,:,:), optional :: AA,BB,CC,DD
      real(kind=kind(1.0d0)), dimension(:,:,:,:,:), pointer :: IIx,IIy,IIz,AAx,AAy,AAz,BBx,BBy,BBz,CCx,CCy,CCz,DDx,DDy,DDz
      integer(kind=kind(1)), dimension(:), pointer :: ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz
      type(rys_type), pointer :: rys
      real(kind=kind(1.0d0)), dimension(3) :: AB,CD,P,Q,PA,QC,QP
      real(kind=kind(1.0d0)) :: zeta,zinv,eta,einv,zeinv,rho,xx,AB2,CD2,fac
      integer(kind=kind(1)) :: l_e,l_f,l_a,l_b,l_c,l_d,n_a,n_b,n_c,n_d,n_roots

      l_a = self%a%l + 1; l_b = self%b%l + 1
      l_c = self%c%l + 1; l_d = self%d%l + 1
      l_e = self%a%l + self%b%l + 1; l_f = self%c%l + self%d%l + 1  ! One higher for differentiating
      n_roots = (l_e+l_f+2)/2
      call create_(IIx,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)    ! Basic intermediate integrals
      call create_(IIy,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      call create_(IIz,n_roots,l_e+1,l_b+1,l_f+1,l_d+1)
      n_a = (self%a%l+1)*(self%a%l+2)/2
      n_b = (self%b%l+1)*(self%b%l+2)/2
      n_c = (self%c%l+1)*(self%c%l+2)/2
      n_d = (self%d%l+1)*(self%d%l+2)/2
      call create_(ax,n_a); call create_(ay,n_a); call create_(az,n_a); call make_gaussian_xyz_indices_(self%a%l,ax,ay,az)
      call create_(bx,n_b); call create_(by,n_b); call create_(bz,n_b); call make_gaussian_xyz_indices_(self%b%l,bx,by,bz)
      call create_(cx,n_c); call create_(cy,n_c); call create_(cz,n_c); call make_gaussian_xyz_indices_(self%c%l,cx,cy,cz)
      call create_(dx,n_d); call create_(dy,n_d); call create_(dz,n_d); call make_gaussian_xyz_indices_(self%d%l,dx,dy,dz)
      zeta = self%a%ex + self%b%ex
      eta  = self%c%ex + self%d%ex
      zinv = 1.0d0/zeta
      einv = 1.0d0/eta
      zeinv = 1.0d0/(zeta+eta)
      rho  = zeta*eta*zeinv
      AB  = self%a%pos - self%b%pos
      CD  = self%c%pos - self%d%pos
      P   = (self%a%ex*self%a%pos + self%b%ex*self%b%pos)*zinv
      Q   = (self%c%ex*self%c%pos + self%d%ex*self%d%pos)*einv
      PA  = P - self%a%pos
      QC  = Q - self%c%pos
      QP  = Q - P
      xx = rho*(QP(1)*QP(1)+QP(2)*QP(2)+QP(3)*QP(3))
      call create_(rys,n_roots)
      call get_weights_(rys,xx)
      AB2 = AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      CD2 = CD(1)*CD(1)+CD(2)*CD(2)+CD(3)*CD(3)
      fac = 34.98683665524973d0*sqrt(zeinv)*zinv*einv*exp(-self%a%ex*self%b%ex*AB2*zinv -self%c%ex*self%d%ex*CD2*einv)
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IIx = 0.0d0; IIy = 0.0d0; IIz = 0.0d0
      call form_2d_ints_(self,IIx(:,:,1,:,1),IIy(:,:,1,:,1),IIz(:,:,1,:,1),rys%r,rys%w,rho,zinv,einv,PA,QC,QP,l_e,l_f)
                                                                ! one unit higher for differentiating
      call transfer_2d_ints_(self,IIx,IIy,IIz,AB,CD,max_b=l_b,max_d=l_d)  ! a & c will automatically be one unit higher
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(AA)) then
      call create_(AAx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
      call create_(AAy,n_roots,l_a,l_b,l_c,l_d)
      call create_(AAz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"a",AAx); call differentiate_(self,IIy,"a",AAy); call differentiate_(self,IIz,"a",AAz)
      AA(:,:,:,:,1) = sum(AAx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      AA(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*AAy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      AA(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*AAz(:,az,bz,cz,dz),dim=1)
      call destroy_(AAz); call destroy_(AAy); call destroy_(AAx)
      AA = fac*AA
      end if
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(BB)) then
      call create_(BBx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
      call create_(BBy,n_roots,l_a,l_b,l_c,l_d)
      call create_(BBz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"b",BBx); call differentiate_(self,IIy,"b",BBy); call differentiate_(self,IIz,"b",BBz)
      BB(:,:,:,:,1) = sum(BBx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      BB(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*BBy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      BB(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*BBz(:,az,bz,cz,dz),dim=1)
      call destroy_(BBz); call destroy_(BBy); call destroy_(BBx)
      BB = fac*BB
      end if
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(CC)) then
      call create_(CCx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
      call create_(CCy,n_roots,l_a,l_b,l_c,l_d)
      call create_(CCz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"c",CCx); call differentiate_(self,IIy,"c",CCy); call differentiate_(self,IIz,"c",CCz)
      CC(:,:,:,:,1) = sum(CCx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      CC(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*CCy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      CC(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*CCz(:,az,bz,cz,dz),dim=1)
      call destroy_(CCz); call destroy_(CCy); call destroy_(CCx)
      CC = fac*CC
      end if
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (present(DD)) then
      call create_(DDx,n_roots,l_a,l_b,l_c,l_d)  ! Derivative integrals
      call create_(DDy,n_roots,l_a,l_b,l_c,l_d)
      call create_(DDz,n_roots,l_a,l_b,l_c,l_d)
      call differentiate_(self,IIx,"d",DDx); call differentiate_(self,IIy,"d",DDy); call differentiate_(self,IIz,"d",DDz)
      DD(:,:,:,:,1) = sum(DDx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      DD(:,:,:,:,2) = sum(IIx(:,ax,bx,cx,dx)*DDy(:,ay,by,cy,dy)*IIz(:,az,bz,cz,dz),dim=1)
      DD(:,:,:,:,3) = sum(IIx(:,ax,bx,cx,dx)*IIy(:,ay,by,cy,dy)*DDz(:,az,bz,cz,dz),dim=1)
      call destroy_(DDz); call destroy_(DDy); call destroy_(DDx)
      DD = fac*DD
      end if
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call destroy_(rys)
      call destroy_(IIz); call destroy_(IIy); call destroy_(IIx)
      call destroy_(dz); call destroy_(dy); call destroy_(dx)
      call destroy_(cz); call destroy_(cy); call destroy_(cx)
      call destroy_(bz); call destroy_(by); call destroy_(bx)
      call destroy_(az); call destroy_(ay); call destroy_(ax)

   end subroutine

   subroutine put(self,out)
    type(gaussian4_type) :: self
    ! Put the object to file "out"
      type(textfile_type) :: out

      call flush_(out)
      call text_(out,"GAUSSIAN4 output:")
      call flush_(out)
      call show_(out,"l_a   =",self%a%l,real_width=.true.)
      call show_(out,"l_b   =",self%b%l,real_width=.true.)
      call show_(out,"l_c   =",self%c%l,real_width=.true.)
      call show_(out,"l_d   =",self%d%l,real_width=.true.)
      call show_(out,"Ra    =",self%a%pos(1),self%a%pos(2),self%a%pos(3))
      call show_(out,"Rb    =",self%b%pos(1),self%b%pos(2),self%b%pos(3))
      call show_(out,"Rc    =",self%c%pos(1),self%c%pos(2),self%c%pos(3))
      call show_(out,"Rd    =",self%d%pos(1),self%d%pos(2),self%d%pos(3))
      call show_(out,"alpha =",self%a%ex)
      call show_(out,"beta  =",self%b%ex)
      call show_(out,"gamma =",self%c%ex)
      call show_(out,"delta =",self%d%ex)

   end subroutine

end
