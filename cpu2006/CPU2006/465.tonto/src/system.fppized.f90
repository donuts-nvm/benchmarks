!-------------------------------------------------------------------------------
!
! SYSTEM: Contains system level routines, including memory management and error
! messages.  It allows for the graceful termination of a program.
!
! An error status is simply an integer, the value is set to 1 if the program
! terminates, or -1 if a warning condition is encountered.
!
! The memory part stores the total and maximum memory used (in bytes), and the
! the total and maximum number of blocks of memory allocated.
! A memory limit is also stored.  It is a fatal error to use more than the
! allocated limit.
!
! The file part contains the file name and record number of the last acessed
! file, in the event that this file should cause an error, the exact position
! will be known.
!
! A standard system object, "tonto", is provided to hold system information
! in the current program. In most cases it should not be neccesary to
! create any other system objects.
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
! $Id: system.foo,v 1.28.2.8 2003/10/09 09:16:34 reaper Exp $
!-------------------------------------------------------------------------------

module SYSTEM_MODULE

   use TYPES_MODULE

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

   private    expand_time_stacks_
   interface expand_time_stacks_
      module procedure expand_time_stacks
   end interface

   private    report_stack_info_
   interface report_stack_info_
      module procedure report_stack_info
   end interface

   private    expand_all_stacks_
   interface expand_all_stacks_
      module procedure expand_all_stacks
   end interface

   public    nullify_ptr_part_
   interface nullify_ptr_part_
      module procedure nullify_ptr_part
   end interface

   public    flush_buffer_
   interface flush_buffer_
      module procedure flush_buffer
   end interface

   public    start_show_
   interface start_show_
      module procedure start_show
   end interface

   private    expand_str_
   interface expand_str_
      module procedure expand_str
   end interface

   public    delete_memory_
   interface delete_memory_
      module procedure delete_memory
   end interface

   public    ensure_
   interface ensure_
      module procedure ensure
   end interface

   public    die_
   interface die_
      module procedure die
   end interface

   public    initialize_
   interface initialize_
      module procedure initialize
   end interface

   private    report_timing_info_
   interface report_timing_info_
      module procedure report_timing_info
   end interface

   public    add_memory_
   interface add_memory_
      module procedure add_memory
   end interface

   public    set_error_output_unit_
   interface set_error_output_unit_
      module procedure set_error_output_unit
   end interface

   public    ignore_memory_leak_
   interface ignore_memory_leak_
      module procedure ignore_memory_leak
   end interface

   private    check_if_leaky_
   interface check_if_leaky_
      module procedure check_if_leaky
   end interface

   private    expand_real_
   interface expand_real_
      module procedure expand_real
   end interface

   private    report_keyword_info_
   interface report_keyword_info_
      module procedure report_keyword_info
   end interface

   private    expand_int_
   interface expand_int_
      module procedure expand_int
   end interface

   public    set_defaults_
   interface set_defaults_
      module procedure set_defaults
   end interface

   public    die_if_
   interface die_if_
      module procedure die_if
   end interface

   private    report_stack_see_info_
   interface report_stack_see_info_
      module procedure report_stack_see_info
   end interface

   public    unstack_
   interface unstack_
      module procedure unstack
   end interface

   public    stop_timer_
   interface stop_timer_
      module procedure stop_timer
   end interface

   private    report_io_file_info_
   interface report_io_file_info_
      module procedure report_io_file_info
   end interface

   public    create_
   interface create_
      module procedure create
   end interface

   public    warn_if_
   interface warn_if_
      module procedure warn_if
   end interface

   public    start_timer_
   interface start_timer_
      module procedure start_timer
   end interface

   private    check_exceeded_
   interface check_exceeded_
      module procedure check_exceeded
   end interface

   public    destroy_
   interface destroy_
      module procedure destroy
   end interface

   private    quick_sort_decreasing_
   interface quick_sort_decreasing_
      module procedure quick_sort_decreasing
   end interface

   public    end_show_
   interface end_show_
      module procedure end_show
   end interface

   public    stack_
   interface stack_
      module procedure stack
   end interface

   public    unknown_
   interface unknown_
      module procedure unknown
      module procedure unknown_1
   end interface

   public    set_error_output_file_
   interface set_error_output_file_
      module procedure set_error_output_file
   end interface

   private    unit_conversion_factor_
   interface unit_conversion_factor_
      module procedure unit_conversion_factor
   end interface

   public    warn_
   interface warn_
      module procedure warn
   end interface

   public    check_
   interface check_
      module procedure check
   end interface

   public    report_
   interface report_
      module procedure report
   end interface

   private    index_for_routine_
   interface index_for_routine_
      module procedure index_for_routine
   end interface

   type(system_type), target, public, save :: tonto
   type(parallel_type), public, save :: tonto_parallel

contains

!  *****************
!  Creation routines
!  *****************

   subroutine create(self,memory_limit)
    type(system_type) :: self
    !  Create a memory manager object with soft limit given in "limit"
      pointer :: self
      integer(kind=kind(1)), optional :: memory_limit

      allocate(self)
      call nullify_ptr_part_(self)
      call set_defaults_(self,memory_limit)
      call add_memory_(tonto,11*4+3*128+2*8+4)  ! increment internal memory manager

   end subroutine

   subroutine destroy(self)
    type(system_type) :: self
    !  Destroy the memory manager object
      pointer :: self

      if (.not. associated(self)) then;   return; end if
      deallocate(self)
      call delete_memory_(tonto,11*4+3*128+2*8+4)

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
    type(system_type) :: self
    ! Nullify the pointer parts of self
      intent(inout) :: self

      nullify(self%io_file)
      nullify(self%call_stack)
      nullify(self%memory_used_stack)
      nullify(self%time_call_stack)
      nullify(self%time_strt_stack)
      nullify(self%time_for_routine)
      nullify(self%name_for_routine)

   end subroutine

   subroutine set_defaults(self,memory_limit)
    type(system_type) :: self
    !  Set defaults
      intent(inout) :: self
      integer(kind=kind(1)), optional :: memory_limit

      self%error_status = 0
      self%error_output_unit = 6
      self%memory_used = 0
      self%memory_blocks_used = 0
      self%max_memory_used = 0
      self%max_memory_blocks_used = 0
      self%memory_limit = 6*8000000
      self%memory_limit_exceeded = .false.
      self%memory_leak_detected = .false.
      self%memory_leak_level = 0
      self%memory_units = "Words"
      self%stack_level = 0
      self%max_stack_level = 0
      self%stack_show_level = -1
      self%show_call_stack = .false.
       ! Timing stuff ...
      self%time_stack_level = 0
      self%n_timed_routines = 0
      self%time_limit = 0.0d0
      if (present(memory_limit)) self%memory_limit = memory_limit

   end subroutine

   subroutine initialize(self)
    type(system_type) :: self
    !  Initialise the system object and set defaults

      call nullify_ptr_part_(self)
      call set_defaults_(self)

   end subroutine

   function unit_conversion_factor(self) result(res)
    type(system_type) :: self
    ! Change the units used to display the memory
      intent(in) :: self
      integer(kind=kind(1)) :: res

      select case(self%memory_units)
         case("Words");  res = 8
         case("Bytes");  res = 1
         case("MBytes"); res = 1000000
         case("MWords"); res = 8000000
      end select

   end function

!   reset_limit(limit)
!   !  Reset memory limit for the memory manager
!   !  "limit" is in bytes.
!      self :: intent(inout)
!      limit :: integer(kind=kind(1)), intent(in)
!      fac :: integer(kind=kind(1))
!      fac = .unit_conversion_factor
!      .memory_limit = fac*limit
!   end

!  ****************
!  Error operations
!  ****************

!   reset_error_status
!   ! Reset the error flag
!      .error_status = 0
!   end

   subroutine set_error_output_unit(self,number)
    type(system_type) :: self
    ! Set the error unit "number"
      integer(kind=kind(1)) :: number

      self%error_output_unit = number

   end subroutine

   subroutine set_error_output_file(self,file)
    type(system_type) :: self
    ! Set the error output file to "file"
      type(textfile_type) :: file

      self%error_output_unit = file%unit

   end subroutine

!  **************
!  Error messages
!  **************

   subroutine die(self,message)
    type(system_type) :: self
    ! Set the error flag to 1 and terminate the program with a message
      character(*) :: message

      self%error_status = 1
      if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a)") &
         "Error in routine "// trim(message)  ! message should include the routine name via foo
      end if
      call report_stack_see_info_(self)
      call report_io_file_info_(self)
      call report_keyword_info_(self)
      call report_stack_info_(self)
      stop

   end subroutine

   subroutine die_if(self,condition,message)
    type(system_type) :: self
    ! Set the error flag to 1 and terminate the program with a message
    ! provided "condition" is .true.
      logical(kind=kind(.true.)) :: condition
      character(*) :: message

      if (condition) call die_(self,message)

   end subroutine

   subroutine unknown(self,word,name,options)
    type(system_type) :: self
    ! Set the error flag to 1 and terminate the program with a message
    ! "Unknown option". The list of known keywords is dumped.
      character(*), intent(in) :: word
      character(*), intent(in) :: name
      character(len=*), dimension(:), intent(in) :: options

      self%error_status = 1
      if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a)") &
        "Error in routine "// trim(name) // " ... unknown option " // trim(word)
      end if
      call report_stack_see_info_(self)
      call report_io_file_info_(self)
      call report_keyword_info_(self,options)
      call report_stack_info_(self)
      stop

   end subroutine

   subroutine unknown_1(self,word,name)
    type(system_type) :: self
    ! Set the error flag to 1 and terminate the program with a message
    ! "Unknown option". The list of known keywords is dumped.
      character(*), intent(in) :: word
      character(*), intent(in) :: name

      self%error_status = 1
      if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a)") &
        "Error in routine "// trim(name) // " ... unknown option " // trim(word)
      end if
      call report_stack_see_info_(self)
      call report_io_file_info_(self)
      if (associated(self%known_keywords)) call report_keyword_info_(self,self%known_keywords)
      call report_stack_info_(self)
      stop

   end subroutine

   subroutine warn(self,message,iostat,use_stack_name)
    type(system_type) :: self
    ! Set the error flag to -1 and issue a warning message.
    ! If "use_stack_name" is present, the call stacl routine name is used
    ! in the warning messsage, assuming that the call stack is associated.
      character(*) :: message
      integer(kind=kind(1)), intent(in), optional :: iostat
      logical(kind=kind(.true.)), intent(in), optional :: use_stack_name
      character(128) :: name

      self%error_status = -1
      if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
        write(self%error_output_unit,*)
      end if
      if (present(use_stack_name)) then
         if (associated(self%call_stack)) then
            name = self%call_stack(self%stack_level)
            if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
              write(self%error_output_unit,"(a)") &
              "Warning in routine "// trim(name) // " ... " // trim(message)
            end if
         else
            if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
              write(self%error_output_unit,"(a)") &
              "Warning: " // trim(message)
            end if
         end if
      else
        if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
           write(self%error_output_unit,"(a)") &
           "Warning in routine "// trim(message)  ! message should include the routine name
        end if
      end if
!      .report_stack_see_info
      if (present(iostat)) then
        if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
           write(self%error_output_unit,"(a,i4)") "Fortran error ",iostat
        end if
      end if
      call flush_buffer_(self)

   end subroutine

   subroutine warn_if(self,condition,message,iostat)
    type(system_type) :: self
    ! If "condition" is true, issue a warning and continue, but set the error
    ! flag to -1 and
      logical(kind=kind(.true.)), intent(in) :: condition
      character(*), intent(in) :: message
      integer(kind=kind(1)), intent(in), optional :: iostat

      if (condition) call warn_(self,message,iostat)

   end subroutine

   subroutine ensure(self,condition,message)
    type(system_type) :: self
    ! Ensure "condition" is true, otherwise set the error flag to 1 and
    ! terminate the program with a "message"
      logical(kind=kind(.true.)), intent(in) :: condition
      character(*), intent(in) :: message

      if (.not. condition) call die_(self,message)

   end subroutine

   subroutine report_stack_see_info(self)
    type(system_type) :: self
    ! Report information about how to compile to see call stack management
    ! information
      integer(kind=kind(1)) :: unit

      if (associated(self%call_stack)) then;   return; end if
      unit = self%error_output_unit
      if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
        write(unit,"(a)") " "
!        write(unit,"(a)") "To see a routine call stack locating this error more precisely,"
!        write(unit,"(a)") "try compiling with the USE_CALL_STACK_MANAGEMENT macro. See the"
!        write(unit,"(a)") "manual for more details"
      end if

   end subroutine

!  *********************************************************
!  Call stack management, memory reporting and leak checking
!  *********************************************************

   pure subroutine add_memory(self,used)
    type(system_type) :: self
    !  Add memory usage data to the memory manager
      intent(inout) :: self
      integer(kind=kind(1)), intent(in) :: used
      self%memory_used = self%memory_used + used
      self%max_memory_used = max(self%memory_used,self%max_memory_used)
      self%memory_blocks_used = self%memory_blocks_used + 1
      self%max_memory_blocks_used = max(self%memory_blocks_used,self%max_memory_blocks_used)
      self%memory_used_stack(1:self%stack_level) = self%memory_used_stack(1:self%stack_level) + used

   end subroutine

   pure subroutine delete_memory(self,used)
    type(system_type) :: self
    !  Delete memory usage data to the memory manager
      intent(inout) :: self
      integer(kind=kind(1)), intent(in) :: used
      self%memory_used = self%memory_used - used
      self%memory_blocks_used = self%memory_blocks_used - 1
      self%memory_used_stack(1:self%stack_level) = self%memory_used_stack(1:self%stack_level) - used

   end subroutine

   subroutine stack(self,routine_name)
    type(system_type) :: self
    ! Add another level to the call stack. Placed at the start of every
    ! non-pure routine. (Pure routines may not have I/O which this has).
      intent(inout) :: self
      character(*), optional :: routine_name

      self%stack_level = self%stack_level + 1
      call expand_all_stacks_(self)
      self%memory_used_stack(self%stack_level) = 0
      if (present(routine_name)) then
        self%call_stack(self%stack_level) = routine_name
      else
        self%call_stack(self%stack_level) = "Unknown routine"
      end if
      if (self%show_call_stack) then
        if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
          write(self%error_output_unit,"(a)") repeat("   ",self%stack_level)//trim(routine_name)//" {"
        end if
      end if
      call flush_buffer_(self)
      self%memory_leak_detected = .false.  ! reset any memory leaks
      if (self%stack_level<self%memory_leak_level) self%memory_leak_level = 0
       ! allow leak reports again at higher levels

   end subroutine

   subroutine unstack(self)
    type(system_type) :: self
    ! Remove a level from the call level. Placed at the end of every non-pure
    ! routine, *including* leaky routines.  If the current level exceeds
    ! stack_show_level then a stack report is produced --- provided that
    ! show_call_stack is not set; but if it is set, then an indented stack report
    ! is made instead.
     intent(inout) :: self
     logical(kind=kind(.true.)) :: report_stack,show_call_stack
     character(128) :: routine_name
     integer(kind=kind(1)) :: mem
     character(9) :: memory
     integer(kind=kind(1)) :: l

     l = self%stack_level
     if (l<=0) then
        if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
          write(self%error_output_unit,*)
          write(self%error_output_unit,"(a,I2)") &
          "Warning in routine SYSTEM:unstack ... stack level is not positive, ", l
        end if
        call report_io_file_info_(self)
        call report_stack_info_(self,full_report=.true.)
        stop
     else if (l>self%max_stack_level) then
        write(self%error_output_unit,*)
        write(self%error_output_unit,"(a,2I2)") &
        "Warning in routine SYSTEM:unstack ... stack level greater than max, ", &
        l,self%max_stack_level
        stop
     else
        show_call_stack =             &
 ! Show indented call stack if:
           self%show_call_stack           &
 ! ... switch was set
        .and. l >=self%stack_show_level        ! ... stack is greater than check
        report_stack =                &
 ! Report tabular stack info if:
            l >= self%stack_show_level    &
 ! ... greater than check level
        .and. self%stack_show_level > 0     &
 ! ... check level was set
        .and. .not. show_call_stack          ! ... .not. doing indented view
        if (report_stack) then
           call report_stack_info_(self)
        else if (show_call_stack) then
           routine_name = self%call_stack(l)
           mem = self%memory_used_stack(l)/unit_conversion_factor_(self)
           if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
             write(memory,"(I9)") mem
           end if
           if (mem==0) then
             if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
               write(self%error_output_unit,"(a)") &
                repeat("   ",l)//trim(routine_name)//" } "//adjustl(memory)
             end if
           else
             if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
               write(self%error_output_unit,"(a)") &
                repeat("---",l)//trim(routine_name)//" } "//adjustl(memory)
             end if
           end if
        end if
     end if
     self%call_stack(l) = " "
     self%stack_level   = self%stack_level - 1
     call flush_buffer_(self)

   end subroutine

   subroutine check(self)
    type(system_type) :: self
    ! Check for memory leaks at this level. Placed at the end of every non-pure,
    ! non-leaky routine. A stack report is produced only if there is a leak, .and.
    ! if the level is greater than the check_level set by start_leak_test (since
    ! for levels less or equal to than the check_level, unstack produces a stack
    ! report).

      if (self%stack_level<=0) then
        if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
          write(self%error_output_unit,*)
          write(self%error_output_unit,"(a,I2)") &
          "Warning in routine SYSTEM:check ... stack level is not positive, ", &
          self%stack_level
        end if
        call report_io_file_info_(self)
        call report_stack_info_(self,full_report=.true.)
        stop
      end if
      call check_exceeded_(self)
      call check_if_leaky_(self)
      call unstack_(self)

   end subroutine

   subroutine check_exceeded(self)
    type(system_type) :: self
    !  Checks whether memory limit is exceeded
      character(128) :: name

      name = self%call_stack(self%stack_level)
      if (self%memory_used>self%memory_limit .and. .not. self%memory_limit_exceeded) then
         call warn_(self,"Memory limit exceeded in routine "//trim(name))
         call report_(self)
         self%memory_limit_exceeded = .true.
      end if

   end subroutine

   subroutine check_if_leaky(self)
    type(system_type) :: self
    ! Checks whether there is a memory leak, and if so produce a warning.
    ! A stack report is made only if the stack_show_level was not set,
    ! or if it was set, it the leak occurs below the stack_show_level
    ! (since otherwise the unstack routine will make a stack report).
      logical(kind=kind(.true.)) :: produce_warning,produce_report
      integer(kind=kind(1)) :: l

      l = self%stack_level
      produce_warning =  &
 ! produce warning if:
         self%memory_used_stack(l) /= 0    &
 ! ... there is a leak at this level,
         .and. .not. self%memory_leak_detected &
 ! ... it hasn't yet been seen,
         .and. l > self%memory_leak_level       ! ... it wasn't reported already
      if (produce_warning) then
         if (self%memory_used_stack(l)>0) call warn_(self,"memory leak",use_stack_name=.true.)
         if (self%memory_used_stack(l)<0) call warn_(self,"memory sink",use_stack_name=.true.)
         self%memory_leak_detected = .true.
         if (self%memory_leak_level==0) then
            self%memory_leak_level = self%stack_level - 1
         else
            self%memory_leak_level = min(self%memory_leak_level,self%stack_level-1)
         end if
      end if
      produce_report  =  &
 ! produce stack report if:
         produce_warning               &
 ! ... there was a warning made .and.
         .and. (0 > self%stack_show_level    &
 ! ... stack level was not set .or.
         .or.   l < self%stack_show_level)      ! ... unstack makes no report
      if (produce_report) then
         call report_stack_info_(self)
      end if

   end subroutine

   subroutine ignore_memory_leak(self,memory_blocks_gained)
    type(system_type) :: self
    ! If called, this routine will reset any memory leak in the current
    ! procedure at the point of call. This is highly dangerous and should be used
    ! only in special cases when you are sure the leak can be tolerated.
    ! "memory_blocks_gained" is the number of blocks that were gained in the leak
    ! process: it is negative for a loss in memory, and positive for a gain in
    ! memory.
      integer(kind=kind(1)) :: memory_blocks_gained

      if (.not. associated(self%memory_used_stack)) then;   return; end if
      call delete_memory_(self,self%memory_used_stack(self%stack_level))
      self%memory_blocks_used = self%memory_blocks_used + 1 - memory_blocks_gained

   end subroutine

   subroutine start_show(self,depth,show_call_stack)
    type(system_type) :: self
    ! Start printing out the memory stack for all routines that are called (i.e.
    ! at the next level). This command is undone by the "end_show" routine. If
    ! "depth" is present, the show starts at the current level plus "depth".
    ! If "show_call_stack" is present and .false., then a tabular style output is
    ! shown rather than the default indented style.
      integer(kind=kind(1)), optional :: depth
      logical(kind=kind(.true.)), optional :: show_call_stack

      self%stack_show_level = self%stack_level + 1
      if (present(depth)) self%stack_show_level = self%stack_level + depth
      self%show_call_stack = .true.
      if (present(show_call_stack)) self%show_call_stack = .false.

   end subroutine

   subroutine end_show(self)
    type(system_type) :: self
    ! Ends memory leak testing at this level

      self%stack_show_level = -1
      self%show_call_stack = .false.

   end subroutine

   subroutine expand_all_stacks(self)
    type(system_type) :: self
    ! Expand both stacks to a length at least equal to ".stack_level".
      integer(kind=kind(1)) :: dim

      dim = self%stack_level - self%max_stack_level
      if (dim>0) then
         call expand_int_(self,self%memory_used_stack,dim)
         call expand_str_(self,self%call_stack,dim)
         self%max_stack_level = self%stack_level
      end if

   end subroutine

   subroutine report(self,out)
    type(system_type) :: self
    !  Report memory usage
     type(textfile_type), optional :: out
     integer(kind=kind(1)) :: unit,fac

     if (self%max_memory_used /= 0) then
       unit = tonto%error_output_unit
       if (present(out)) unit = out%unit
       fac = unit_conversion_factor_(self)
       call report_stack_info_(self,out)
       call report_timing_info_(self)
     end if
     call flush_buffer_(self)

   end subroutine

   subroutine report_stack_info(self,out,full_report)
    type(system_type) :: self
    ! Report memory stack usage. Use the unit number for file "out", if present.
    ! If present and .true., "full_report" requests a full stack output.
     type(textfile_type), optional :: out
     logical(kind=kind(.true.)), optional :: full_report
     integer(kind=kind(1)) :: unit,fac,l
     logical(kind=kind(.true.)) :: full

     if (self%max_stack_level /= 0) then
       unit = tonto%error_output_unit
       if (present(out)) unit = out%unit
       full = .false.
       if (present(full_report)) full = full_report
       fac = unit_conversion_factor_(self)
       if (self%stack_level>0 .and. associated(self%call_stack)) then
         if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
           write(unit,*)
           write(unit,'(a)') "Routine call stack:"
           write(unit,*)
           write(unit,'("   Call   Routine name        ",a37)') "Memory Used"
         end if
         do l = 1,self%stack_level
         end do
       end if
       if (full .and. self%max_stack_level>self%stack_level .and. associated(self%call_stack)) then
           if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
             write(unit, &
           '("   ----------------------------------------------------------------")')
           end if
         do l = self%stack_level+1,self%max_stack_level
           if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
              write(unit,'(3x,i3,".",3x,a48,i9)') &
              l,adjustl(self%call_stack(l)),self%memory_used_stack(l)/fac
           end if
         end do
       end if
     end if
     call flush_buffer_(self)

   end subroutine

   subroutine report_io_file_info(self)
    type(system_type) :: self
    ! Report info about the most recent open file.
     integer(kind=kind(1)) :: unit
     character(256) :: cursor
     integer(kind=kind(1)) :: item_end

     if (.not. associated(self%io_file)) then;   return; end if
     unit = tonto%error_output_unit
     item_end = max(1,self%io_file%buffer%item_end)
     if (item_end>0) cursor = repeat("-",item_end-1)//"^"
     if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
       write(unit,*)
       write(unit,'("File name   = ",a)')  trim(self%io_file%name)
       write(unit,'("Line number = ",i4)') self%io_file%record
       write(unit,'("File buffer = ",a)')  trim(self%io_file%buffer%string)
     end if
     if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
       if (item_end>0) then
         write(unit,'("Cursor -------",a)')  trim(cursor)
       end if
     end if
     call flush_buffer_(self)

   end subroutine

   subroutine report_keyword_info(self,options)
    type(system_type) :: self
    ! Report info about the most recent keywords used
      character(len=*), dimension(:), optional :: options
      integer(kind=kind(1)) :: n
      integer(kind=kind(1)) :: unit

      unit = tonto%error_output_unit
      if (present(options)) then
        if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
          write(unit,*)
          write(unit,'("Allowed keyword options:")')
          write(unit,*)
          do n = 1,size(options)
            write(unit,'("   ",a)') trim(options(n))
          end do
        end if
      end if
      call flush_buffer_(self)

   end subroutine

   subroutine start_timer(self,routine_name)
    type(system_type) :: self
    ! Start timing routine "routine_name". Placed at the start of every routine
    ! when profiling is requested.
      character(*) :: routine_name
      real(kind=kind(1.0d0)) :: start_time

      self%time_stack_level = self%time_stack_level + 1
      call cpu_time(start_time)
      call expand_time_stacks_(self,routine_name,start_time)

   end subroutine

   subroutine stop_timer(self,routine_name)
    type(system_type) :: self
    ! Start the timer for routine "routine_name". Placed at the start of every
    ! routine profiling is requested.
      character(*) :: routine_name
      integer(kind=kind(1)) :: index,i
      real(kind=kind(1.0d0)) :: start_time,finish_time,elapsed_time,total_time,total
       ! Get the finishing time

      call cpu_time(finish_time)
       ! Get the starting time and elapsed time
      start_time = self%time_strt_stack(self%time_stack_level)
      elapsed_time = finish_time - start_time
       ! Increment the elapsed time for the current routine
      index = index_for_routine_(self,routine_name)
      self%time_for_routine(index) = self%time_for_routine(index) + elapsed_time
       ! Remove this elapsed time from all the parent routines
      do i = 1,self%time_stack_level-1
         index = self%time_call_stack(i)  ! This is a parent routine index
         self%time_for_routine(index) = self%time_for_routine(index) - elapsed_time
      end do
       ! See whether to stop program if total time exceeded
      if (self%time_limit>0.0d0) then
          ! Get total time: Add up total time so far
         total_time = 0.0d0
         do i = 1,self%n_timed_routines
            total = self%time_for_routine(i)
            if (total<0.0d0) cycle  ! These have not completed
            total_time = total_time + total
         end do
          ! Make a report and stop if total time exceeded
         if (total_time>self%time_limit) then
            call report_timing_info_(self)
            stop
         end if
      end if
       ! Remove the routine from the call stack
      self%time_call_stack(self%time_stack_level) = 0
      self%time_stack_level = self%time_stack_level - 1
     ! write(*,*) "---- returning, time_stack_level=",.time_stack_level

   end subroutine

   subroutine expand_time_stacks(self,routine_name,start_time)
    type(system_type) :: self
    ! Expand all the time stacks (if required) by adding "routine_name" in the
    ! ".name_for_routine" stack, for example. Also add the starting time.
     character(*) :: routine_name
     real(kind=kind(1.0d0)) :: start_time
     integer(kind=kind(1)) :: index,dim
      ! First expand the list of akll timed routines

     index = index_for_routine_(self,routine_name)
     if (index==0) then   ! NEW routine.
        dim = self%n_timed_routines
        if (associated(self%time_for_routine)) dim = dim - size(self%time_for_routine)
        if (dim>=0) then  ! Not enough space? Expand stack ...
           call expand_real_(self,self%time_for_routine,dim+1)
           call expand_str_(self, self%name_for_routine,dim+1)
        end if
        index = self%n_timed_routines + 1
        self%n_timed_routines = index
        self%time_for_routine(index) = 0.0d0
        self%name_for_routine(index) = routine_name
        write(*,*) "NEW=",index,"name=",trim(routine_name)
     else
        write(*,*) "old=",index,"name=",trim(routine_name)
     end if
      ! Now expand the timed call stack ...
     dim = self%time_stack_level
     if (associated(self%time_call_stack)) dim = dim - size(self%time_call_stack)
     if (dim>0) then
        call expand_int_(self, self%time_call_stack,dim)
        call expand_real_(self,self%time_strt_stack,dim)
     end if
     self%time_call_stack(self%time_stack_level) = index
     self%time_strt_stack(self%time_stack_level) = start_time

   end subroutine

   function index_for_routine(self,routine_name) result(index)
    type(system_type) :: self
    ! Return the "index" for routine "routine_name" in the ".name_for_routine"
    ! stack.
     character(*) :: routine_name
     integer(kind=kind(1)) :: index
     integer(kind=kind(1)) :: i

     index = 0
     do i = 1,self%n_timed_routines
        if (self%name_for_routine(i)/=routine_name) cycle
        index = i
        exit
     end do

   end function

   subroutine report_timing_info(self,out,full_report)
    type(system_type) :: self
    ! Report routine timing info i.e. a profile. If present, use the unit number for file
    ! "out", otherwise use "tonto.error_output_unit".  If present and .true.,
    ! "full_report" requests a full stack output, otherwise only the top 20 are
    ! reported.
     type(textfile_type), optional :: out
     logical(kind=kind(.true.)), optional :: full_report
     logical(kind=kind(.true.)) :: full
     integer(kind=kind(1)), dimension(:), pointer :: order
     real(kind=kind(1.0d0)) :: total_time,total
     integer(kind=kind(1)) :: unit,i,j,n_routine

     if (self%n_timed_routines == 0) then;   return; end if
     if (tonto_parallel%rank > 0 .and. tonto_parallel%do_parallel) then;   return; end if
      ! Process arguments ...
     unit = tonto%error_output_unit
     if (present(out)) unit = out%unit
     full = .true.
     if (present(full_report)) full = full_report
     if (full) then; n_routine = self%n_timed_routines
     else;           n_routine = min(20,self%n_timed_routines)
     end if
      ! Set any negative times to zero and sort
     total_time = 0.0d0
     do i = 1,self%n_timed_routines
        total = self%time_for_routine(i)
        if (total<0.0d0) self%time_for_routine(i) = 0.0d0
        total_time = total_time + total
     end do
     allocate(order(self%n_timed_routines))
     call quick_sort_decreasing_(self,self%time_for_routine,order)
     write(unit,*)
     write(unit,'(a)') "Routine call stack:"
     write(unit,*)
     write(unit,'("   Call   ",a43,a7,a7)') "   Time","% total"
     do i = 1,n_routine
        j = order(i)
        write(unit,'(3x,i3,".",3x,a43,f7.3,f7.3)') &
                         i, &
                         adjustl(self%name_for_routine(i)), &
                         self%time_for_routine(i), &
                         self%time_for_routine(i)*100d0/total_time
     end do
     deallocate(order)
     call flush_buffer_(self)

   end subroutine

   subroutine flush_buffer(self,unit)
    type(system_type) :: self
    ! Flush the output
     integer(kind=kind(1)), intent(in), optional :: unit
     integer(kind=kind(1)) :: f_unit

     if (present(unit)) then
       f_unit = unit
     else
       f_unit = tonto%error_output_unit
     end if
     if (tonto_parallel%rank == 0 .or. (.not. tonto_parallel%do_parallel)) then
     end if

   end subroutine

! ************************************
! These would be inherited if possible
! ************************************

   subroutine expand_real(self,stack,dim)
    type(system_type) :: self
    ! Expands "stack" by amount "dim". Contents are retained.
    ! Elements added are set to zero.
     real(kind=kind(1.0d0)), dimension(:), pointer :: stack
     integer(kind=kind(1)), intent(in) :: dim
     real(kind=kind(1.0d0)), dimension(:), pointer :: old_stack
     integer(kind=kind(1)) :: n

   call ensure_(tonto,dim>0,"SYSTEM:expand_real ... cannot expand stack by less than 1")
     if (.not. associated(stack)) then
       ! write(*,*) "NOT associated, dim=",dim
        allocate(stack(dim))
        stack = 0.0d0
     else
        n = size(stack)
        old_stack => stack
       ! write(*,*) "Associated, copying, n=",n," dim=",dim
        nullify(stack)
        allocate(stack(n+dim))
        stack(1:n) = old_stack
        stack(n+1:n+dim) = 0.0d0
        deallocate(old_stack)
     end if

   end subroutine

   subroutine expand_int(self,stack,dim)
    type(system_type) :: self
    ! Expands "stack" by amount "dim". Contents are retained.
    ! Elements added are set to zero.
     integer(kind=kind(1)), dimension(:), pointer :: stack
     integer(kind=kind(1)), intent(in) :: dim
     integer(kind=kind(1)), dimension(:), pointer :: old_stack
     integer(kind=kind(1)) :: n

   call ensure_(tonto,dim>0,"SYSTEM:expand_int ... cannot expand stack by less than 1")
     if (.not. associated(stack)) then
        allocate(stack(dim))
        stack = 0
     else
        n = size(stack)
        old_stack => stack
        nullify(stack)
        allocate(stack(n+dim))
        stack(1:n) = old_stack
        stack(n+1:n+dim) = 0
        deallocate(old_stack)
     end if

   end subroutine

   subroutine expand_str(self,stack,dim)
    type(system_type) :: self
    ! Expands "stack" by amount "dim". Contents are retained.
    ! Elements added are set to blank.
     character(128), dimension(:), pointer :: stack
     integer(kind=kind(1)), intent(in) :: dim
     character(128), dimension(:), pointer :: old_stack
     integer(kind=kind(1)) :: n

   call ensure_(tonto,dim>0,"SYSTEM:expand_str ... cannot expand stack by less than 1")
     if (.not. associated(stack)) then
        allocate(stack(dim))
        stack = " "
     else
        n = size(stack)
        old_stack => stack
        nullify(stack)
        allocate(stack(n+dim))
        stack(1:n) = old_stack
        stack(n+1:n+dim) = " "
        deallocate(old_stack)
     end if

   end subroutine

   recursive subroutine quick_sort_decreasing(self,vec,indices)
    type(system_type) :: self
    ! Return the indices which sort vector from largest to smallest, i.e. on
    ! return "vec(indices)" is sorted. NOTE: vec is *not* sorted.
      real(kind=kind(1.0d0)), dimension(:) :: vec
      integer(kind=kind(1)), dimension(:), intent(inout) :: indices
      integer(kind=kind(1)), dimension(:), pointer :: list,small,equal,large,small_indices,equal_indices,large_indices
      integer(kind=kind(1)) :: n, i, ns, ne, nl
      real(kind=kind(1.0d0)) :: chosen

      if (size(indices)<=1) then;   return; end if
      n = size(indices)
      allocate(list(n)); list = (/(i,i=1,n)/)
      chosen = vec(1)
      ns = count(vec>chosen)
      nl = count(vec<chosen)
      ne = n - ns - nl
      allocate(small(ns)); allocate(small_indices(ns))
      allocate(equal(ne)); allocate(equal_indices(ne))
      allocate(large(nl)); allocate(large_indices(nl))
      small = pack(list,vec >chosen)  ! indices of large vec elements
      equal = pack(list,vec==chosen)  ! indices of equal vec elements
      large = pack(list,vec <chosen)  ! indices of small vec elements
      small_indices = indices(small)
      equal_indices = indices(equal)
      large_indices = indices(large)
      if (ns>1) call quick_sort_decreasing_(self,vec(small),small_indices)
      if (nl>1) call quick_sort_decreasing_(self,vec(large),large_indices)
      indices(1:ns)       = small_indices
      indices(ns+1:ns+ne) = equal_indices
      indices(ns+ne+1:)   = large_indices
      deallocate(large_indices); deallocate(large)
      deallocate(equal_indices); deallocate(equal)
      deallocate(small_indices); deallocate(small)
      deallocate(list)

   end subroutine

end
