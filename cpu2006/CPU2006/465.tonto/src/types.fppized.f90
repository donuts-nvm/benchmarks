!-------------------------------------------------------------------------------
!
! TYPES : used to deposit all derived types used in TONTO.
!
! A separate module is required so that two modules can use each others types
! even if they can't use each others routines by with a "use" statement. In the
! C++ language every one of TONTO's objects are "friendly" with each other.
!
! (c) dylan jayatilaka, university of western australia, 1998
!
! $Id: types.foo,v 1.59.2.34 2004/04/21 09:12:56 reaper Exp $
!
!-------------------------------------------------------------------------------

module TYPES_MODULE

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
   public

   type system_type

   integer(kind=kind(1)) :: error_status = 0
    ! Set non-zero on error.

   integer(kind=kind(1)) :: error_output_unit = 6
    ! The file unit number for error messages.

   integer(kind=kind(1)) :: memory_used = 0
    ! The amount of memory used by the program so far.

   integer(kind=kind(1)) :: memory_blocks_used = 0
    ! The current number of active memory blocks allocated using "create"
    ! commands

   integer(kind=kind(1)) :: max_memory_used = 0
    ! The maximum amount of memory used so far.

   integer(kind=kind(1)) :: max_memory_blocks_used = 0
    ! The maximum number of memory blocks allocated used by the program so far.

   integer(kind=kind(1)) :: memory_limit = 6*8000000
    ! A soft memory limit. A warning is generated if this limit is exceeded bu
    ! the program is not stopped.

   logical(kind=kind(.true.)) :: memory_limit_exceeded = .false.
    ! Set .true. if the soft memory limit is exceeded.

   logical(kind=kind(.true.)) :: memory_leak_detected = .false.
    ! Set .true. if a memory leak has been detected. This is to prevent cascading
    ! memory leak reports from parent routines. However, this switch is set back
    ! to .false. whenever a new routine is entered at a level below where the
    ! current leak occured, so it is not so useful.

   integer(kind=kind(1)) :: memory_leak_level = 0
    ! Gives the memory leak level below which leaks are not reported. This is to
    ! ensure that the same memory leak is not reported at lower levels. The
    ! variable is reset to 0 whenever a new routine is entered at a level lower
    ! than the leak.

   character(128) :: memory_units = "Words"
    ! The units for acconting for memory usage.

   integer(kind=kind(1)) :: stack_level = 0
    ! The current call-stack level.

   integer(kind=kind(1)) :: max_stack_level = 0
    ! The maximum call-stack level.

   integer(kind=kind(1)) :: stack_show_level = -1
    ! Enable printing of the call-stack, for all routines at this level or higher
    ! in the call-stack.

   integer(kind=kind(1)), dimension(:), pointer :: memory_used_stack => NULL()
    ! An array which stores the amount of memory used by each routine at a given
    ! call-level in the call-stack.

   character(128), dimension(:), pointer :: call_stack => NULL()
    ! Stores the name of each subroutine at each call-level, for routine traceback.

   logical(kind=kind(.true.)) :: show_call_stack = .false.
    ! If .true., tells whether to show an indented call-stack as the program runs.
    ! The memory gained or lost by each routine is also displayed.  Be warned,
    ! setting this switch produces copious output.

   type(textfile_type), pointer :: io_file => NULL()
    ! Last I/O file used

   integer(kind=kind(1)) :: time_stack_level = 0
    ! The current time call-stack level. This is essentially just the same as
    ! stack_level. It is not the same since we may want to check timings when the
    ! ENSURE statements are turned on -- call_stack always implies ENSURE.

   integer(kind=kind(1)), dimension(:), pointer :: time_call_stack => NULL()
    ! Stores the *indices* of the routines called at each call-level.  This is
    ! used for subracting the childrens elspased times from parent routines.

   real(kind=kind(1.0d0)), dimension(:), pointer :: time_strt_stack => NULL()
    ! Stores the starting times for all routines in the time_call_stack.

   integer(kind=kind(1)) :: n_timed_routines = 0
    ! The number of routines that have currently been called and are being timed.

   real(kind=kind(1.0d0)), dimension(:), pointer :: time_for_routine => NULL()
    ! The time taken for a particular routine -- all routines called in the
    ! program will be timed in this stack.

   character(128), dimension(:), pointer :: name_for_routine => NULL()
    ! The name for a particular timed routine -- all routines called in the
    ! program will be named in this stack.

   real(kind=kind(1.0d0)) :: time_limit = 0.0d0
    ! Set a total time limit on a profile run. A zero value means no time limit.
    ! If the time limit is exceeded the programs stops and dumps a profile up to
    ! that point. Pretty good, huh.

   character(128), dimension(:), pointer :: known_keywords => NULL()
    ! List of known keywords in the current case statement, if applicable
    ! This allows a nice error message to be returned saying what the allowed
    ! options in the case statement were.

   end type

  type parallel_type

    logical(kind=kind(.true.)) :: do_parallel = .false.
     ! Whether or not to do parallel stuff.

    integer(kind=kind(1)) :: rank
     ! The number of the current processor

    integer(kind=kind(1)) :: nprocs = 1
     ! Number of processors

    integer(kind=kind(1)), dimension(:), pointer :: mpi_status => NULL()
     ! Status field used for all MPI routines.

  end type

   type time_type

   integer(kind=kind(1)), dimension(5) :: started
    ! Contains real start time, in Julian day,h,m,s,ms

   integer(kind=kind(1)), dimension(5) :: stopped
    ! Contains real stop time, in Julian day,h,m,s,ms

   real(kind=kind(1.0d0)) :: start_cpu
    ! Contains CPU start time, in seconds

   real(kind=kind(1.0d0)) :: stop_cpu
    ! Contains CPU stop  time, in seconds

   end type

   type buffer_type

   integer(kind=kind(1)) :: item_start
    ! The position of the first character of the last item processed in
    ! the buffer string

   integer(kind=kind(1)) :: item_end
    ! The position of the last character of the last item processed in
    ! the buffer string

   integer(kind=kind(1)) :: item_index
    ! The item number of the last item processed in the buffer string

   integer(kind=kind(1)) :: n_items
    ! The total number of items in the string

   logical(kind=kind(.true.)) :: analysed = .false.
    ! True, if the buffer string has been analysed

   character(128) :: comment_chars = "!#"
    ! The comment character symbols (concatenated) to be used in the
    ! buffer string

   character(128) :: quote_chars = "'"""
    ! The quote character symbols (concatenated) to be used in the
    ! buffer string

   character(256) :: string = " "
    ! This is the actual buffer string

   end type

   type unitnumber_type

   integer(kind=kind(1)) :: unit
    ! The unit number

   end type

   type file_type

   character(128) :: name = "unknown"
    ! The name of the file

   integer(kind=kind(1)) :: unit = 0
    ! The unit number

   integer(kind=kind(1)) :: record = 1
    ! The current record of the output

   integer(kind=kind(1)) :: io_status = 0
    ! Set non-zero if there is an error condition

   character(128) :: action = "readwrite"
    ! The type of action performed on this file

   character(128) :: file_status = "unknown"
    ! File status

   logical(kind=kind(.true.)) :: buffered = .false.
    ! Whether to use buffering if the file is for numbers

   integer(kind=kind(1)) :: int_buffer_pos = 1
    ! The integer buffer position marker

   integer(kind=kind(1)), dimension(:), pointer :: int_buffer => NULL()
    ! The integer buffer which is flushed to the file when full

   integer(kind=kind(1)) :: real_buffer_pos = 1
    ! The real buffer position marker

   real(kind=kind(1.0d0)), dimension(:), pointer :: real_buffer => NULL()
    ! The real buffer which is flushed to the file when full

   integer(kind=kind(1)) :: cpx_buffer_pos = 1
    ! The cpx buffer position marker

   complex(kind=kind((1.0d0,1.0d0))), dimension(:), pointer :: cpx_buffer => NULL()
    ! The cpx buffer which is flushed to the file when full

   end type

   type textfile_type

   character(128) :: name = "unknown"
    ! The name of the file

   character(128) :: action = "unknown"
    ! The action status of the file, "read" or "write"

   integer(kind=kind(1)) :: unit
    ! The unit number

   integer(kind=kind(1)) :: record
    ! The current record of the output

   integer(kind=kind(1)) :: io_status
    ! Set non-zero if there is an error condition

   logical(kind=kind(.true.)) :: ignore_end_of_file = .false.
    ! Set to .true. if the end of file is not to be regarded as an error.

   integer(kind=kind(1)) :: no_of_lines = -1
    ! The number of lines in the file. This is only set if the end-of-file is
    ! encountered.

   integer(kind=kind(1)) :: n_fields = 5
    ! No of fields (i.e. columns) to use for outputting the variable

   integer(kind=kind(1)) :: int_width = 9
    ! Width of an integer field

   integer(kind=kind(1)) :: real_width = 16
    ! Width of a real field

   integer(kind=kind(1)) :: real_precision = 6
    ! No. of decimal places to use for outputing a real variable

   character(128) :: real_style = " f"
    ! Fortran style character for a real variable

   integer(kind=kind(1)) :: margin_width = 0
    ! Margin width

   logical(kind=kind(.true.)) :: use_labels = .true.
    ! Whether to use column and row labels on vectors and matrices

   character(128) :: default_units = " "
    ! Each real(kind=kind(1.0d0)) number is assumed to have "default_units", and are converted
    ! into generic (internal) units, usually A.U.

   character(128) :: comment_chars = "!#"
    ! The comment character symbols (concatenated) to be used in the textfile

   character(128) :: quote_chars = "'"""
    ! The quote character symbols (concatenated) to be used in the textfile

   type(buffer_type) :: buffer
    ! The output buffer which holds one line of output to be flushed

   character(128), dimension(:), pointer :: internal => NULL()
    ! Used to keep a record of of the entire file, internally

   type(textfile_type), pointer :: saved => NULL()
    ! Used to keep a record of a previous file, say for input redirect

   end type

   type archive_type

   character(128) :: root_name
    ! Archive root name (usually, the name of the job)

   character(128) :: name
    ! Archive name (usually, the name of the entity in the archive)

   character(128) :: genre
    ! Archive basis genre for opmatrix objects, or a qualifier for name

   character(128) :: format
    ! Archive format (blank for binary)

   type(file_type), pointer :: file => NULL()
    ! Binary file part

   type(textfile_type), pointer :: textfile => NULL()
    ! Text file part

   end type

   type cif_type

   type(textfile_type), pointer :: file => NULL()
    ! The actual type(cif_type) file to be processed

   character(128) :: data_block_name = " "
    ! The name of the data block

   integer(kind=kind(1)) :: start_of_data = 1
    ! The starting line of the data block

   integer(kind=kind(1)) :: end_of_data = 0
    ! The end line of the data block

   logical(kind=kind(.true.)) :: data_block_found = .false.
    ! Set .true. if a data block has been sucessfully found

   end type

   type rys_type

   integer(kind=kind(1)) :: nroots
    ! No. of rys roots

   real(kind=kind(1.0d0)), dimension(:), pointer :: w => NULL()
    ! The Rys weights

   real(kind=kind(1.0d0)), dimension(:), pointer :: r => NULL()
    ! The Rys roots

   end type

   type intvec__type

   integer(kind=kind(1)), dimension(:), pointer :: element => NULL()
    ! Encapsulated ivec type

   end type

   type realvec__type

   real(kind=kind(1.0d0)), dimension(:), pointer :: element => NULL()
    ! Encapsulated vec type

   end type

   type realmat__type

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: element => NULL()
    ! Encapsulated mat type

   end type

   type realmat3__type

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: element => NULL()
    ! Encapsulated mat3 type

   end type

   type realmat4__type

   real(kind=kind(1.0d0)), dimension(:,:,:,:), pointer :: element => NULL()
    ! Encapsulated mat4 type

   end type

   type opvector_type

   integer(kind=kind(1)) :: n_bf
    ! No of spatial basis functions (i.e. AO's)

   real(kind=kind(1.0d0)), dimension(:), pointer :: restricted => NULL()
    ! Restricted spinorbital representation

   real(kind=kind(1.0d0)), dimension(:), pointer :: alpha => NULL()
    ! Alpha part of a DODS representation

   real(kind=kind(1.0d0)), dimension(:), pointer :: beta => NULL()
    ! Beta  part of a DODS representation

   real(kind=kind(1.0d0)), dimension(:), pointer :: general => NULL()
    ! General mixed alpha-beta matrix representation

   end type

   type opmatrix_type

   integer(kind=kind(1)) :: n_bf
    ! No of real spatial basis functions (i.e. AO's)

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: restricted => NULL()
    ! Restricted spinorbital representation

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: alpha => NULL()
    ! Alpha part of a DODS representation

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: beta => NULL()
    ! Beta  part of a DODS representation

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: general => NULL()
    ! General mixed alpha-beta matrix representation

   complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: restricted_complex => NULL()
    ! Complex restricted representation

   complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: alpha_complex => NULL()
    ! Complex alpha part of a DODS representation

   complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: beta_complex => NULL()
    ! Complex beta  part of a DODS representation

   complex(kind=kind((1.0d0,1.0d0))), dimension(:,:), pointer :: general_complex => NULL()
    ! Complex general mixed alpha-beta matrix representation

   real(kind=kind(1.0d0)), dimension(:), pointer :: triangle => NULL()
    ! Space saving for symmetric matrices: lower triangle only

   real(kind=kind(1.0d0)), dimension(:), pointer :: square => NULL()
    ! Space saving for hermitian matrices: lower triangle is real part, upper is imaginary

   end type

   type intvecinthash_type

   integer(kind=kind(1)) :: n_keys
    ! The number of keys

   integer(kind=kind(1)) :: n_size
    ! The size of the keys and values arrays.

   logical(kind=kind(.true.)) :: reverse_search
    ! If .true., the search for values is performed from the largest key to the
    ! smallest.

   integer(kind=kind(1)), dimension(:,:), pointer :: keys => NULL()
    ! The array of keys, stored contigously by columns.

   integer(kind=kind(1)), dimension(:), pointer :: values => NULL()
    ! The array of values.

   end type

   type intvecintvechash_type

   integer(kind=kind(1)) :: n_keys
    ! The number of keys

   integer(kind=kind(1)) :: n_size
    ! The size of the keys and values arrays.

   logical(kind=kind(.true.)) :: reverse_search
    ! If .true., the search for values is performed from the largest key to the
    ! smallest.

   integer(kind=kind(1)), dimension(:,:), pointer :: keys => NULL()
    ! The array of keys, stored contigously by columns.

   integer(kind=kind(1)), dimension(:,:), pointer :: values => NULL()
    ! The array of values.

   end type

   type reflection_type

   integer(kind=kind(1)) :: h
    ! Miller h index for the reflection

   integer(kind=kind(1)) :: k
    ! Miller k index for the reflection

   integer(kind=kind(1)) :: l
    ! Miller l index for the reflection

   real(kind=kind(1.0d0)) :: F_exp
    ! Experimental structure factor

   real(kind=kind(1.0d0)) :: F_sigma
    ! Experimental error in the structure factor

   complex(kind=kind((1.0d0,1.0d0))) :: F_calc
    ! Calculated complex structure factor without including corrections

   real(kind=kind(1.0d0)) :: F_pred
    ! Calculated structure factor including scale and extinction corrections

   real(kind=kind(1.0d0)) :: I_exp
    ! Experimental intensity

   real(kind=kind(1.0d0)) :: I_sigma
    ! Experimental error in the intensity

   real(kind=kind(1.0d0)) :: I_pred
    ! Calculated intensity including scale and extinction corrections

   end type

   type diis_type

   integer(kind=kind(1)) :: keep = 8
    ! No. of parameter vectors to keep for use in extrapolation

   integer(kind=kind(1)) :: n_vec = 0
    ! The number of parameter vectors currently available for extrapolation

   integer(kind=kind(1)) :: new = 0
    ! The integer identifier for the new (current) parameter vector

   real(kind=kind(1.0d0)) :: error_length = 0.0d0
    ! The length of the type(diis_type) error vector

   type(archive_type) :: archive
    ! Archive used to store the type(diis_type) parameter and error vectors

   real(kind=kind(1.0d0)), dimension(:), pointer :: coeff => NULL()
    ! The type(diis_type) coefficients which combine/extrapolate the stored parameter
    ! vectors

   logical(kind=kind(.true.)) :: in_core = .false.
    ! Whether to store data in memory instead of on disk
   real(kind=kind(1.0d0)), dimension(:,:), pointer :: error_items => NULL()
   real(kind=kind(1.0d0)), dimension(:,:), pointer :: parameter_items => NULL()
   real(kind=kind(1.0d0)), dimension(:,:), pointer :: diis_matrix => NULL()
   real(kind=kind(1.0d0)), dimension(:,:), pointer :: constraint_matrix => NULL()
   real(kind=kind(1.0d0)), dimension(:,:), pointer :: density_matrix => NULL()

   end type

   type gaussian_type

   integer(kind=kind(1)) :: l = 0
    ! l quantum number for the gaussian

   real(kind=kind(1.0d0)) :: ex = 0.0d0
    ! Exponent for the gaussian

   real(kind=kind(1.0d0)), dimension(3) :: pos = 0.0d0
    ! Position of the gaussian
   end type

   type gaussian2_type

   type(gaussian_type) :: a
    ! Shell "a" of the pair

   type(gaussian_type) :: b
    ! Shell "b" of the pair

   end type

   type gaussian4_type

   type(gaussian_type) :: a
    ! Shell "a" of the quartet

   type(gaussian_type) :: b
    ! Shell "b" of the quartet

   type(gaussian_type) :: c
    ! Shell "c" of the quartet

   type(gaussian_type) :: d
    ! Shell "d" of the quartet

   end type

   type shell_type

   integer(kind=kind(1)) :: l = 0
    ! l quantum number for the gaussian shell

   integer(kind=kind(1)) :: n_comp = 0
    ! No. of components for the gaussian shell

   integer(kind=kind(1)) :: n_cc = 0
    ! No. of contractions for the gaussian shell

   real(kind=kind(1.0d0)), dimension(:), pointer :: ex => NULL()
    ! Exponent vector

   real(kind=kind(1.0d0)), dimension(:), pointer :: cc => NULL()
    ! Contraction coefficient vector

   end type

   type shell1_type

   integer(kind=kind(1)) :: l = 0
    ! l quantum number of the shell

   integer(kind=kind(1)) :: n_comp = 0
    ! No. of components for the shell

   integer(kind=kind(1)) :: n_cc = 0
    ! No. of contraction coefficients for the shell

   real(kind=kind(1.0d0)), dimension(3) :: pos = 0.0d0
    ! Position of the shell

   real(kind=kind(1.0d0)), dimension(:), pointer :: ex => NULL()
    ! Exponent vector

   real(kind=kind(1.0d0)), dimension(:), pointer :: cc => NULL()
    ! Contraction coefficient vector

   end type

   type shell2_type

   type(shell1_type) :: a
    ! Shell "a" of the pair

   type(shell1_type) :: b
    ! Shell "b" of the pair

   integer(kind=kind(1)) :: n_gaussian_pairs
    ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc

   integer(kind=kind(1)) :: l_max
    ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)

   integer(kind=kind(1)) :: l_min
    ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)

   integer(kind=kind(1)) :: l_sum
    ! Sum of the angular momenta on each shell, .a.l+.b.l

   real(kind=kind(1.0d0)) :: kappa_max
    ! Maximum of .a.cc * .b.cc * kappa_ab for the two electron integrals.

   real(kind=kind(1.0d0)), dimension(:), pointer :: exponent_sum => NULL()
    ! The sum of all gaussian pair exponents as a flattened array with shell "a"
    ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)

   real(kind=kind(1.0d0)), dimension(:), pointer :: exponent_inv => NULL()
    ! The inverse of the sum of the gaussian pair exponents, as a flattened
    ! array, 1/(.a.ex(:)+.b.ex(:))

   real(kind=kind(1.0d0)), dimension(:), pointer :: a_exponent_inv => NULL()
    ! The product of the exponent of shell "a" with the inverse of the sum of the
    ! gaussian pair exponents, as a flattened array, .a.ex(:)/(.a.ex(:)+.b.ex(:))

   real(kind=kind(1.0d0)), dimension(:), pointer :: b_exponent_inv => NULL()
    ! The product of the exponent of shell "b" with the inverse of the sum of the
    ! gaussian pair exponents, as a flattened array, .b.ex(:)/(.a.ex(:)+.b.ex(:))

   real(kind=kind(1.0d0)), dimension(:), pointer :: cc_prefactor => NULL()
    ! The contraction coefficient product with an exponential part for each
    ! gaussian pair as a flattened array with shell "a" gaussian components
    ! incrementing most rapidly. Useful for integral evaluation.

   real(kind=kind(1.0d0)), dimension(:), pointer :: normalising_factors => NULL()
    ! The product of the normalisation coefficients, as a flattened array,
    ! .a.l.normalising_factors * .b.l.normalising_factors.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: pair_center => NULL()
    ! The center of the product gaussian of gaussians .a and .b.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: center_diff => NULL()
    ! The difference between pair_center and the position of the gaussian of
    ! higher angular momentum out of .a and .b.

   end type

   type shellpair_type

   type(shell_type) :: a
    ! Shell "a" of the pair

   type(shell_type) :: b
    ! Shell "b" of the pair

   integer(kind=kind(1)) :: n_gaussian_pairs
    ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc

   integer(kind=kind(1)) :: l_max
    ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)

   integer(kind=kind(1)) :: l_min
    ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)

   integer(kind=kind(1)) :: l_sum
    ! Sum of the angular momenta on each shell

   real(kind=kind(1.0d0)), dimension(:), pointer :: exponent_sum => NULL()
    ! The sum of all gaussian pair exponents as a flattened array with shell "a"
    ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)

   real(kind=kind(1.0d0)), dimension(:), pointer :: exponent_inv => NULL()
    ! The inverse of the sum of the gaussian pair exponents, as a flattened
    ! array, 1/(.a.ex(:)+.b.ex(:))

   real(kind=kind(1.0d0)), dimension(:), pointer :: a_exponent_inv => NULL()
    ! The product of the exponent of shell "a" with the inverse of the sum of the
    ! gaussian pair exponents, as a flattened array, .a.ex(:)/(.a.ex(:)+.b.ex(:))

   real(kind=kind(1.0d0)), dimension(:), pointer :: b_exponent_inv => NULL()
    ! The product of the exponent of shell "b" with the inverse of the sum of the
    ! gaussian pair exponents, as a flattened array, .b.ex(:)/(.a.ex(:)+.b.ex(:))

   real(kind=kind(1.0d0)), dimension(:), pointer :: cc_prefactor => NULL()
    ! The contraction coefficient product with an exponential part for each
    ! gaussian pair as a flattened array with shell "a" gaussian components
    ! incrementing most rapidly. Useful for integral evaluation.

   real(kind=kind(1.0d0)), dimension(:), pointer :: normalising_factors => NULL()
    ! The product of the normalisation coefficients, as a flattened array,
    ! .a.l.normalising_factors * .b.l.normalising_factors.

   integer(kind=kind(1)), dimension(:,:,:), pointer :: hrr_index_larger => NULL()
    ! The mapping of the cartesian angular momenta to a single array, from
    ! max(.a.l,.b.l) to .a.l+.b.l.  In the HRR, this is the index for the
    ! component with the larger angular momentum.

   integer(kind=kind(1)), dimension(:,:,:), pointer :: hrr_index_smaller => NULL()
    ! The mapping of the cartesian angular momenta to a single array, from zero
    ! up to .a.l+.b.l.  In the HRR, this is the index for the component with the
    ! smaller angular momentum.

   integer(kind=kind(1)), dimension(:,:), pointer :: hrr_components => NULL()
    ! Cartesian components of the angular momenta from zero up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: hrr_comp_to_use => NULL()
    ! Which cartesian component of angular momentum to use for the HRR for the
    ! angular momenta between zero up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: form_3dints_x_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .l_max up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: form_3dints_y_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .l_max up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: form_3dints_z_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .l_max up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: form_3dints_yz_rms_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .l_max up to .a.l+.b.l.  This version is for the
    ! reduced multiplication scheme, where the y and z arrays have been collapsed
    ! into one product array.

   end type

   type shell4_type

   type(shell1_type) :: a
    ! Shell "a" of the quartet

   type(shell1_type) :: b
    ! Shell "b" of the quartet

   type(shell1_type) :: c
    ! Shell "c" of the quartet

   type(shell1_type) :: d
    ! Shell "d" of the quartet

   end type

   type shellquartet_type

   type(shellpair_type), pointer :: ab => NULL()
    ! Pair "ab" of the quartet

   type(shellpair_type), pointer :: cd => NULL()
    ! Pair "cd" of the quartet

   end type

   type shell1quartet_type

   type(shell_type), pointer :: a => NULL()
    ! Shell "a" of the quartet

   type(shell_type), pointer :: b => NULL()
    ! Shell "b" of the quartet

   type(shell_type), pointer :: c => NULL()
    ! Shell "c" of the quartet

   type(shell_type), pointer :: d => NULL()
    ! Shell "d" of the quartet

   real(kind=kind(1.0d0)), dimension(3) :: pos_a
    ! Position of the "a" shell.

   real(kind=kind(1.0d0)), dimension(3) :: pos_b
    ! Position of the "b" shell

   real(kind=kind(1.0d0)), dimension(3) :: pos_c
    ! Position of the "c" shell

   real(kind=kind(1.0d0)), dimension(3) :: pos_d
    ! Position of the "d" shell

   logical(kind=kind(.true.)) :: ab_nullify
    ! Whether to nullify the precalculated .ab vectors or destroy them, i.e.
    ! whether they were pointer assigned or created/calculated.

   logical(kind=kind(.true.)) :: cd_nullify
    ! Whether to nullify the precalculated .cd vectors or destroy them, i.e.
    ! whether they were pointer assigned or created/calculated.

   integer(kind=kind(1)) :: ab_n_gaussian_pairs
    ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc

   integer(kind=kind(1)) :: ab_l_max
    ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)

   integer(kind=kind(1)) :: ab_l_min
    ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)

   integer(kind=kind(1)) :: ab_l_sum
    ! Sum of the angular momenta on each shell, .a.l+.b.l

   real(kind=kind(1.0d0)) :: ab_kappa_max
    ! Maximum of .a.cc * .b.cc * kappa_ab for the two electron integrals.

   real(kind=kind(1.0d0)), dimension(:), pointer :: ab_exponent_sum => NULL()
    ! The sum of all gaussian pair exponents as a flattened array with shell "a"
    ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)

   real(kind=kind(1.0d0)), dimension(:), pointer :: ab_cc_prefactor => NULL()
    ! The contraction coefficient product with an exponential part for each
    ! gaussian pair as a flattened array with shell "a" gaussian components
    ! incrementing most rapidly. Useful for integral evaluation.

   real(kind=kind(1.0d0)), dimension(:), pointer :: ab_normalising_factors => NULL()
    ! The product of the normalisation coefficients, as a flattened array,
    ! .a.l.normalising_factors * .b.l.normalising_factors.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: ab_pair_center => NULL()
    ! The center of the product gaussian of gaussians .a and .b.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: ab_center_diff => NULL()
    ! The difference between ab_pair_center and the position of the gaussian of
    ! higher angular momentum out of .a and .b.

   integer(kind=kind(1)) :: cd_n_gaussian_pairs
    ! The number of gaussian pair products in the shell pair, .c.n_cc*.d.n_cc

   integer(kind=kind(1)) :: cd_l_max
    ! Maximum of the angular momenta on each shell, max(.c.l,.d.l)

   integer(kind=kind(1)) :: cd_l_min
    ! Minimum of the angular momenta on each shell, min(.c.l,.d.l)

   integer(kind=kind(1)) :: cd_l_sum
    ! Sum of the angular momenta on each shell, .c.l+.d.l

   real(kind=kind(1.0d0)) :: cd_kappa_max
    ! Maximum of .c.cc * .d.cc * kappa_cd for the two electron integrals.

   real(kind=kind(1.0d0)), dimension(:), pointer :: cd_exponent_sum => NULL()
    ! The sum of all gaussian pair exponents as a flattened array with shell "c"
    ! exponents incrementing most rapidly, .c.ex(:)+.d.ex(:)

   real(kind=kind(1.0d0)), dimension(:), pointer :: cd_cc_prefactor => NULL()
    ! The contraction coefficient product with an exponential part for each
    ! gaussian pair as a flattened array with shell "c" gaussian components
    ! incrementing most rapidly. Useful for integral evaluation.

   real(kind=kind(1.0d0)), dimension(:), pointer :: cd_normalising_factors => NULL()
    ! The product of the normalisation coefficients, as a flattened array,
    ! .c.l.normalising_factors * .d.l.normalising_factors.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: cd_pair_center => NULL()
    ! The center of the product gaussian of gaussians .c and .d.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: cd_center_diff => NULL()
    ! The difference between cd_pair_center and the position of the gaussian of
    ! higher angular momentum out of .c and .d.

   integer(kind=kind(1)), dimension(:,:,:), pointer :: ab_hrr_index_larger => NULL()
    ! The mapping of the cartesian angular momenta to a single array, from
    ! max(.a.l,.b.l) to .a.l+.b.l.  In the HRR, this is the index for the
    ! component with the larger angular momentum.

   integer(kind=kind(1)), dimension(:,:,:), pointer :: ab_hrr_index_smaller => NULL()
    ! The mapping of the cartesian angular momenta to a single array, from zero
    ! up to .a.l+.b.l.  In the HRR, this is the index for the component with the
    ! smaller angular momentum.

   integer(kind=kind(1)), dimension(:,:), pointer :: ab_hrr_components => NULL()
    ! Cartesian components of the angular momenta from zero up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: ab_hrr_comp_to_use => NULL()
    ! Which cartesian component of angular momentum to use for the HRR for the
    ! angular momenta between zero up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: ab_form_3dints_x_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .ab_l_max up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: ab_form_3dints_y_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .ab_l_max up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: ab_form_3dints_z_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .ab_l_max up to .a.l+.b.l.

   integer(kind=kind(1)), dimension(:), pointer :: ab_form_3dints_yz_rms_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .ab_l_max up to .a.l+.b.l.  This version is for the
    ! reduced multiplication scheme, where the y and z arrays have been collapsed
    ! into one product array.

   integer(kind=kind(1)), dimension(:,:,:), pointer :: cd_hrr_index_larger => NULL()
    ! The mapping of the cartesian angular momenta to a single array, from
    ! max(.c.l,.d.l) to .c.l+.d.l.  In the HRR, this is the index for the
    ! component with the larger angular momentum.

   integer(kind=kind(1)), dimension(:,:,:), pointer :: cd_hrr_index_smaller => NULL()
    ! The mapping of the cartesian angular momenta to a single array, from zero
    ! up to .c.l+.d.l.  In the HRR, this is the index for the component with the
    ! smaller angular momentum.

   integer(kind=kind(1)), dimension(:,:), pointer :: cd_hrr_components => NULL()
    ! Cartesian components of the angular momenta from zero up to .c.l+.d.l.

   integer(kind=kind(1)), dimension(:), pointer :: cd_hrr_comp_to_use => NULL()
    ! Which cartesian component of angular momentum to use for the HRR for the
    ! angular momenta between zero up to .c.l+.d.l.

   integer(kind=kind(1)), dimension(:), pointer :: cd_form_3dints_x_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .cd_l_max up to .c.l+.d.l.

   integer(kind=kind(1)), dimension(:), pointer :: cd_form_3dints_y_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .cd_l_max up to .c.l+.d.l.

   integer(kind=kind(1)), dimension(:), pointer :: cd_form_3dints_z_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .cd_l_max up to .c.l+.d.l.

   integer(kind=kind(1)), dimension(:), pointer :: cd_form_3dints_yz_rms_indices => NULL()
    ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
    ! angular momenta from .cd_l_max up to .c.l+.d.l.  This version is for the
    ! reduced multiplication scheme, where the y and z arrays have been collapsed
    ! into one product array.

   real(kind=kind(1.0d0)) :: r2ab
    ! The distance between shells .a and .b.

   real(kind=kind(1.0d0)) :: r2cd
    ! The distance between shells .c and .d.

   end type

   type basis_type

   character(128) :: label
    ! Unique label for the basis set

   integer(kind=kind(1)) :: n_shell
    ! No. of shells in the basis, equal to size(shell)

   integer(kind=kind(1)) :: n_bf
    ! No. of basis functions for the shell

   integer(kind=kind(1)) :: n_prim
    ! No. of primitives for the shell

   type(shell_type), dimension(:), pointer :: shell => NULL()
    ! The list of gaussian shells in the basis set

   end type

   type interpolator_type

   character(128) :: interp_kind = "linear"
    ! The kind of interpolation used (usually "linear"). Also allowed is
    ! "logarithmic"

   integer(kind=kind(1)) :: n_data = 0
    ! The number of "data_points" and "values" in the table

   real(kind=kind(1.0d0)), dimension(:), pointer :: data_point => NULL()
    ! The list of data points, from smallest to largest.

   real(kind=kind(1.0d0)), dimension(:), pointer :: data_value => NULL()
    ! The list of values corresponding to each data value.

   real(kind=kind(1.0d0)) :: spacing = 0.0d0
    ! The spacing between data points, if using an even spaced grid.
    ! (this is the case if spacing in non-zero)

   logical(kind=kind(.true.)) :: finalised = .false.
    ! Set .true. if the object is ready for use

   end type

   type slatershell_type

   integer(kind=kind(1)) :: l = 0
    ! l quantum number

   integer(kind=kind(1)) :: n_comp = 0
    ! The number of l-components, normally 2*l+1 for spherical type slater
    ! functions.

   integer(kind=kind(1)) :: n_orb = 0
    ! The number of generally contracted orbitals.

   integer(kind=kind(1)) :: n_prim = 0
    ! No. of exponents/contractions (i.e. the number of primitives for one
    ! *single* angular momentum shell component, e.g. p_x).

   integer(kind=kind(1)), dimension(:), pointer :: n => NULL()
    ! The n quantum numbers.

   real(kind=kind(1.0d0)), dimension(:), pointer :: z => NULL()
    ! The exponent zeta for each slater function

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: c => NULL()
    ! The contraction coefficient matrix -- this is a generally contracted slater
    ! shell. The size of .dim1 is "n_cc". The size of .dim2 is "n_orb".

   character(128), dimension(:), pointer :: orb_kind => NULL()
    ! The kinds of the orbitals (1s, 2s, 2p, etc.), if available.
    ! It's length is "n_orb".

   integer(kind=kind(1)), dimension(:), pointer :: occupancy => NULL()
    ! The occupany number of each contracted orbital, if available.
    ! It's length is "n_orb".

   end type

   type slaterbasis_type

   character(128) :: label
    ! Unique label for the basis set

   character(128) :: configuration
    ! A STR representation of the configuration.

   integer(kind=kind(1)) :: n_shell
    ! No. of shells in the basis, equal to size(shell)

   integer(kind=kind(1)) :: n_bf
    ! No. of basis functions for the shell

   integer(kind=kind(1)) :: n_prim
    ! No. of primitives for the shell

   type(slatershell_type), dimension(:), pointer :: shell => NULL()
    ! The list of gaussian shells in the basis set

   type(interpolator_type), pointer :: interpolator => NULL()
    ! An interpolator object, used for calculating the atomic density at a
    ! certain radius, from e.g. the coppensbasis

   end type

   type coppensorbital_type

   character(128) :: orb_kind
    ! The kind of the orbital (1s, 2s, 2p, etc.)

   integer(kind=kind(1)) :: occupancy
    ! The number of electrons in the orbital.

   integer(kind=kind(1)) :: n_fun
    ! The number of contracted fitting functions

   integer(kind=kind(1)), dimension(:), pointer :: n => NULL()
    ! The n quantum numbers

   real(kind=kind(1.0d0)), dimension(:), pointer :: c => NULL()
    ! The contraction coefficient

   real(kind=kind(1.0d0)), dimension(:), pointer :: z => NULL()
    ! The exponent zeta for each slater function

   end type

   type coppensbasis_type

   character(128) :: label
    ! The basis label

   integer(kind=kind(1)) :: n_orb
    ! The number of orbitals in the basis

   integer(kind=kind(1)) :: n_prim
    ! The number of primitive functions in the basis

   type(coppensorbital_type), dimension(:), pointer :: orbital => NULL()
    ! The list of fitted Slater atomic orbitals, Coppens style.

   type(interpolator_type), pointer :: interpolator => NULL()
    ! An interpolator object, used for calculating the atomic density at a
    ! certain radius, from e.g. the coppensbasis

   end type

   type atom_type

   character(128) :: label = "?"
    ! The label for the atom (not necessarily unique)

   integer(kind=kind(1)) :: atomic_number
    ! The atomic number

   real(kind=kind(1.0d0)), dimension(3) :: pos
    ! Atom position

   character(128) :: axis_system = "cartesian"
    ! Specifies the coordinate axis system

   real(kind=kind(1.0d0)) :: U_iso = 0.0d0
    ! The isotropic thermal smearing value for the atom

   real(kind=kind(1.0d0)), dimension(3,3) :: thermal_tensor = 0.0d0
    ! The thermal tensor for the atom

   character(128) :: thermal_axis_system = "cartesian"
    ! Specifies the thermal tensor coordinate system

   character(128) :: basis_label = " "
    ! The label of the basis set to match to.

   type(basis_type), pointer :: basis => NULL()
    ! The basis for the atom

   type(slaterbasis_type), pointer :: slaterbasis => NULL()
    ! The Slater function basis set for the atom. This includes occupancies and
    ! can be used for Hirshfeld surface plots and sum-of-spherical atoms densities.

   type(coppensbasis_type), pointer :: coppensbasis => NULL()
    ! The Coppens-style fitted relativistic orbital basis for the atom. This is
    ! used for Hirshfeld surface plots and sum-of-spherical atoms densities.

   type(opmatrix_type), pointer :: density_matrix => NULL()
    ! The density matrix for the atom

   type(opmatrix_type), pointer :: natural_orbitals => NULL()
    ! The natural orbitals for the atom

   type(opvector_type), pointer :: occupation_numbers => NULL()
    ! The occupation numbers for the atom

   real(kind=kind(1.0d0)) :: energy
    ! The energy of the isolated atom

   integer(kind=kind(1)) :: group
    ! An integer which describes thr group to which the atom belongs

   integer(kind=kind(1)) :: sequence_number = 0
    ! The sequence number (unique within on molecule, only one chain allowed yet)

   character(128) :: residue_atom_name = "?"
    ! The unique name for the atom in its (protein-)residue

   character(128) :: residue_name = "UNK"
    ! The residue name (must be part of residue table for pdbfile as input)

   character(128) :: mm_forcefield_name = "?"
    ! The name of the forcefield (e.g. amber, sybyl,...)

   character(128) :: mm_atom_type = "?"
    ! The atom type, used to define the force field potential

   real(kind=kind(1.0d0)) :: mm_charge = 0.0d0
    ! The atomic charge (relative to the force field!)

   real(kind=kind(1.0d0)), dimension(3) :: restraining_position = 0.0d0
    ! A position used for restrained geometry optimization

   real(kind=kind(1.0d0)) :: restraining_force_constant = 0.0d0
    ! A force constant used for restrained geometry optimisations

   real(kind=kind(1.0d0)) :: site_occupancy = 1.0d0
    ! The crystallographic site occupancy.

   end type

   type plotgrid_type

   character(128) :: plot_kind = " "
    ! The type of plot calculation wanted

   integer(kind=kind(1)) :: orbital = 0
    ! The orbital to plot (if any)

   integer(kind=kind(1)) :: n_x = 75
    ! The number of points on the x-axis

   integer(kind=kind(1)) :: n_y = 75
    ! The number of points on the y-axis

   integer(kind=kind(1)) :: n_z = 1
    ! The number of points on the z-axis

   integer(kind=kind(1)) :: n_pt = 0
    ! The total no. of points in the plot

   type(atom_type), dimension(:), pointer :: atom => NULL()
    ! List of atoms which can be used to define grid aces and positions.

   integer(kind=kind(1)) :: centre_atom = 0
    ! Use this atom as the centre of the plot

   integer(kind=kind(1)) :: x_atom_1, x_atom_2
    ! These atoms define the x-axis of the plot

   integer(kind=kind(1)) :: y_atom_1, y_atom_2
    ! These atoms define the y-axis of the plot (made orthogonal to the x-axis)

   integer(kind=kind(1)) :: z_atom_1, z_atom_2
    ! These atoms define the z-axis of the plot.

   real(kind=kind(1.0d0)) :: del = 1.0d0
    ! The distance between axis points in the plot

   real(kind=kind(1.0d0)), dimension(3) :: centre = 0.0d0
    ! Centre of the plot

   real(kind=kind(1.0d0)), dimension(3) :: origin = 0.0d0
    ! Bottom left point of the plot

   real(kind=kind(1.0d0)), dimension(3) :: x_axis = (/ 1.0d0,0.0d0,0.0d0/)
    ! x-axis of the plot

   real(kind=kind(1.0d0)), dimension(3) :: y_axis = (/0.0d0, 1.0d0,0.0d0/)
    ! y-axis of the plot

   real(kind=kind(1.0d0)), dimension(3) :: z_axis = (/0.0d0,0.0d0, 1.0d0/)
    ! z-axis of the plot

   real(kind=kind(1.0d0)), dimension(3) :: width = (/ 2.0d0, 2.0d0,0.0d0/)
    ! The widths (in a.u.) of each axis of the plot

   real(kind=kind(1.0d0)), dimension(3) :: offset = 0.0d0
    ! The centre of the plot is offset by this amount

   logical(kind=kind(.true.)) :: x_width_set = .true.
    ! If False the x_width, i.e. width(1), is to be calculated from the size of
    ! the inputted x_axis vector; or if a width has *not* been inputted.

   logical(kind=kind(.true.)) :: y_width_set = .true.
    ! If False the y_width, i.e. width(2), is to be calculated from the size of
    ! the inputted y_axis vector; or if a width has *not* been inputted.

   logical(kind=kind(.true.)) :: z_width_set = .true.
    ! If False the z_width, i.e. width(3), is to be calculated from the size of
    ! the inputted z_axis vector; or if a width has *not* been inputted.

   logical(kind=kind(.true.)) :: x_axis_defined = .false.
    ! Flag set true if the user has defined the x_axis in the input

   logical(kind=kind(.true.)) :: y_axis_defined = .false.
    ! Flag set true if the user has defined the y_axis in the input

   logical(kind=kind(.true.)) :: z_axis_defined = .false.
    ! Flag set true if the user has defined the z_axis in the input

   real(kind=kind(1.0d0)), dimension(3) :: box_centre
    ! The centre of the bounding box, defined as the centre of an optional
    ! inputted atom list.

   real(kind=kind(1.0d0)), dimension(3) :: bounding_box
    ! The bounding box for the molecule, defined from an optional atom list.

   real(kind=kind(1.0d0)) :: box_scale_factor
    ! Scale factor for the bounding box.  Usually leave it at 1.

   real(kind=kind(1.0d0)), dimension(3,3) :: box_axes
    ! The axes of the bounding box, defined as the principal axes of the shape
    ! tensor (essentially the unit weighted moment of inertia tensor). Defined
    ! from an optionally inputted atom list.

   real(kind=kind(1.0d0)) :: desired_separation = 0.0d0
    ! The desired separation between grid points.

   end type

   type dftgrid_type

   character(128) :: spherical_grid_kind = "lebedev"
    ! Identifier for the kind of spherical grid

   character(128) :: radial_grid_kind = "gauss-chebyshev"
    ! Type of radial grid

   integer(kind=kind(1)) :: spherical_grid_order = 35
    ! Order of the spherical grid

   integer(kind=kind(1)) :: radial_grid_order = 60
    ! Order of the radial grid

   integer(kind=kind(1)) :: n_spherical_pts = 0
    ! No of spherical grid points

   integer(kind=kind(1)) :: n_radial_pts = 0
    ! No of radial grid points

   integer(kind=kind(1)) :: n_pts = 0
    ! No of integration grid points

   real(kind=kind(1.0d0)) :: becke_m_partition_power = 2.0d0
    ! Used in smoothing the partition boundary

   real(kind=kind(1.0d0)) :: gauss_chebyshev_alpha = 3.0d0
    ! Gauss-Chebychev radial grid parameters

   real(kind=kind(1.0d0)) :: gauss_chebyshev_m = 1.0d0
    ! ?

   real(kind=kind(1.0d0)) :: euler_maclaurin_alpha = 2.0d0
    ! Euler-Maclaurin radial grid parameters

   real(kind=kind(1.0d0)) :: euler_maclaurin_m = 2.0d0
    !

   type(archive_type) :: archive
    ! Archive to store the generated grids

   logical(kind=kind(.true.)) :: finalized = .false.
    ! Set to true if the type(dftgrid_type) has been "set" using set_grid_data

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: single_atom_points => NULL()
    ! The dft grid points at the origin for a single atom.

   real(kind=kind(1.0d0)), dimension(:), pointer :: single_atom_weights => NULL()
    ! The dft grid weights at the origin for a single atom.

   end type

   type irrep_type

   character(4) :: label
    ! Irrep label

   integer(kind=kind(1)) :: dimension
    ! Irrep dimension

   real(kind=kind(1.0d0)), dimension(:), pointer :: character => NULL()
    ! Characters for the irrep

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: mat => NULL()
    ! Representation matrices for the irrep

   end type

   type pointgroup_type

   character(4) :: symbol
    ! symmetry symbol

   character(4) :: ID_symbol
    ! group id symbol

   integer(kind=kind(1)) :: ID_number
    ! group id number

   integer(kind=kind(1)) :: axis_order
    ! principal axis order

   integer(kind=kind(1)) :: order
    ! order of the group

   integer(kind=kind(1)) :: n_irrep
    ! no of irreducible representations

   integer(kind=kind(1)) :: n_gen
    ! No. of generators

   logical(kind=kind(.true.)) :: has_complex_irreps
    ! true if the group has complex irreps

   integer(kind=kind(1)), dimension(:,:), pointer :: table => NULL()
    ! group multiplication table

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: mat => NULL()
    ! 3x3 representation matrices

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: ptr => NULL()
    ! 3x3 representation matrices for p functions, same as mat

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: dtr => NULL()
    ! 6x6 representation matrices for d functions

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: ftr => NULL()
    ! 10x10 representation matrices for f functions

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: gtr => NULL()
    ! 15x15 representation matrices for g functions

   integer(kind=kind(1)), dimension(:), pointer :: inverse => NULL()
    ! Indices of inverse group elements

   type(irrep_type), dimension(:), pointer :: irrep => NULL()
    ! List of irrrducible representations

   end type

   type unitcell_type

   real(kind=kind(1.0d0)), dimension(3) :: angle
    ! The cell angles  (in radians)

   real(kind=kind(1.0d0)), dimension(3) :: length
    ! The cell lengths (in bohr)

   real(kind=kind(1.0d0)) :: volume
    ! The cell volumes (bohr^3)

   real(kind=kind(1.0d0)), dimension(3,3) :: direct_matrix
    ! Direct lattice cell matrix (bohr). The columns are vectors of the three
    ! cell axes.

   real(kind=kind(1.0d0)), dimension(3,3) :: inverse_matrix
    ! Inverse direct lattice cell matrix (bohr^{-1})

   real(kind=kind(1.0d0)), dimension(3,3) :: reciprocal_matrix
    ! Reciprocal lattice cell matrix (bohr^{-1}).

   real(kind=kind(1.0d0)), dimension(3,3) :: direct_U_matrix
    ! Converts thermal tensors from crystal to cartesian systems.

   real(kind=kind(1.0d0)), dimension(3,3) :: reciprocal_U_matrix
    ! Converts thermal tensors from cartesian to crystal systems.

   logical(kind=kind(.true.)) :: info_made
    ! Set .true. if all the above information is consistent.

   end type

   type spacegroup_type

   character(128) :: IT_symbol
    ! International Table (Hermann-Maguin) symmetry symbol

   integer(kind=kind(1)) :: IT_group_number
    ! International tables group number

   character(128) :: Hall_symbol
    ! Hall notation symbol

   character(128) :: HM_symbol
    ! Hermann-Mauguin notation symbol

   character(128) :: Schoenflies_symbol
    ! Hermann-Mauguin notation symbol

   character(1) :: lattice_symbol
    ! Lattice symmetry symbol

   integer(kind=kind(1)) :: lattice_symbol_index
    ! Lattice symbol index number

   character(128) :: lattice_type
    ! Lattice type

   logical(kind=kind(.true.)) :: centrosymmetric
    ! True if center of symmetry present

   integer(kind=kind(1)), dimension(3) :: axis_order
    ! Order of each crystal axis

   character(len=1), dimension(3) :: axis_symbol
    ! Superscript rotation axis symbol

   integer(kind=kind(1)), dimension(3) :: axis_symbol_index
    ! Indicates the numberical index corresponding to the axis symbol x, y, z ...

   logical(kind=kind(.true.)), dimension(3) :: has_axis_bar
    ! True if bar for this axis

   logical(kind=kind(.true.)), dimension(3) :: has_translation
    ! True if translation subscript present for this axis

   character(len=1), dimension(3,3) :: translation_symbol
    ! Subscript translation symbols for each axis

   integer(kind=kind(1)), dimension(3) :: origin_shift
    ! Subscript translation symbols for each axis

   integer(kind=kind(1)) :: nL
    ! No. of translational lattice generators

   integer(kind=kind(1)) :: nR
    ! No. of rotation generators

   integer(kind=kind(1)) :: nG
    ! No. of generators

   integer(kind=kind(1)) :: n_seitz
    ! No. of Seitz matrices

   real(kind=kind(1.0d0)), dimension(:,:,:), pointer :: seitz => NULL()
    ! The spacegroup Seitz matrices.

   integer(kind=kind(1)) :: n_unique
    ! No. of equivalent positions unrelated by traslation or inversion

   integer(kind=kind(1)), dimension(:), pointer :: unique_symop => NULL()
    ! Index of unique symops not related by translation or inversion

   integer(kind=kind(1)), dimension(:), pointer :: map_to_unique => NULL()
    ! Maps symop to a unique symop. Negative indicates inversion

   logical(kind=kind(.true.)) :: analysed
    ! Set .true. if the spacegroup symbol has been succesfully analysed

   end type

   type crystal_type

   character(128) :: data_kind
    ! Kind of diffraction experiment used.

   type(spacegroup_type) :: spacegroup
    ! The crystal spacegroup

   type(unitcell_type) :: unitcell
    ! The crystal unit cell

   integer(kind=kind(1)) :: n_fragment_atoms
    ! No. of atoms in the inputted cell fragment, used to calculate structure
    ! factors, or do wavefunction fitting.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: fragment_geometry => NULL()
    ! The geometry for a molecular fragment in the crystal (in the crystal
    ! coordiante system) used to calculate structure factors or do wavefunction
    ! fitting. IMPORTANT NOTE --- this is not to be confused with the
    ! "asymmetric_unit_geometry"; it may include symmetry non-unique atoms.

   integer(kind=kind(1)) :: n_fragment_cell_atoms
    ! No. of atoms in the whole unit cell which are generated from
    ! "fragment_geometry" by spacegroup symmetry operations. This will be the whole
    ! unit cell if the "fragment_geometry" includes all asymmetric unit cell.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: fragment_cell_geometry => NULL()
    ! The geometry of all atoms in the unit cell generated from
    ! "fragment_geometry" by the spacegroup symmetry operations. It will usually
    ! be the full "unit_cell_geometry", but may not be.

   integer(kind=kind(1)), dimension(:), pointer :: symop_for_fragment_cell_atom => NULL()
    ! symop_for_unit_cell_atom :: INTVEC* => NULL()
    ! "symop_for_fragment_cell_atom(a)" is the index of the spacegroup symmetry
    ! operation that generates atom position "fragment_cell_geometry(:,a)" from a
    ! unique atom position in "fragment_geometry".

   integer(kind=kind(1)), dimension(:), pointer :: atom_for_fragment_cell_atom => NULL()
    ! fragment_atom_for :: INTVEC* => NULL()
    ! "atom_for_fragment_cell_atom(a)" is the index of the unique fragment atom
    ! in "fragment_geometry" which generates atom position
    ! "fragment_cell_geometry(:,a)", using "symop_for_fragment_cell_atom(a)".

   integer(kind=kind(1)) :: n_unique_fragment_atoms
    ! n_unique_unit_cell_atoms :: integer(kind=kind(1))
    ! No. of unique fragment atoms.

   integer(kind=kind(1)), dimension(:), pointer :: unique_fragment_atom => NULL()
    ! unique_atom :: INTVEC* => NULL()
    ! The list of symmetry-unique fragment atoms. This list may be a subset of
    ! the asymmetric unit -- but it usually will be the asymmetric unit.

   integer(kind=kind(1)), dimension(:), pointer :: unique_symop_for_fragment_atom => NULL()
    ! unique_atom_symop_for :: INTVEC* => NULL()
    ! "unique_symop_for_fragment_atom(a)" is the index of the spacegroup symmetry
    ! operation which maps the atom "unique_atom_for_fragment_atom(a)" onto the
    ! "a"-th atom position, fragment_geometry(:,a).

   integer(kind=kind(1)), dimension(:), pointer :: unique_atom_for_fragment_atom => NULL()
    ! unique_atom_for :: INTVEC* => NULL()
    ! "unique_atom_for_fragment_atom(a)" is the index of the symmetry-unique atom
    ! position in "fragment_geometry" which generates the position
    ! "fragment_geometry(:,a)".

   integer(kind=kind(1)) :: n_reduced_symops
    ! No. of symops needed to make the unit_cell_geometry from fragment_geometry
    ! OBSOLETE

   integer(kind=kind(1)), dimension(:), pointer :: reduced_symop => NULL()
    ! Indices of the reduced symops in the spacegroup seitz list, i.e. those
    ! symops which make distinctly different fragment_geometries when transformed
    ! back to the unit cell. The unit symop is part of the reduced_symop list.
    ! OBSOLETE

   integer(kind=kind(1)) :: n_cluster_symops
    ! No. of cluster symops which generate distinctly different fragment geometries.
    ! OBSOLETE

   integer(kind=kind(1)), dimension(:), pointer :: cluster_symop => NULL()
    ! Indices of the cluster symops in the spacegroup seitz list. These are the
    ! same as the reduced_symop's, except that the fragment_geometry and its
    ! symmetry transform are not mapped back to the unit cell
    ! OBSOLETE

   integer(kind=kind(1)) :: n_inverted_symops
    ! Number of symmetry operations related by inversion

   integer(kind=kind(1)), dimension(:), pointer :: inverted_symop => NULL()
    ! Indices of the unique symops related by inversion

   integer(kind=kind(1)), dimension(:), pointer :: translated_symop => NULL()
    ! Indices of the unique symops related by translation

   integer(kind=kind(1)) :: n_unique_SF_symops
    ! Number of unique symmetry operations not related by inversion or
    ! translation. This is used to save work in structure factor (SF)
    ! calculations.

   integer(kind=kind(1)), dimension(:), pointer :: unique_SF_symop => NULL()
    ! Indices of the unique symops not related by inversion or translation.
    ! This is used to save work in structure factor (SF) calculations.

   real(kind=kind(1.0d0)), dimension(:), pointer :: repetition_factor => NULL()
    ! The partition factors for fragment_geometry Useful to get structure
    ! factor contributions from a small portion of the fragment.

   real(kind=kind(1.0d0)) :: Z
    ! The crystallographic Z factor for the molecular cell fragment in the
    ! unitcell

   logical(kind=kind(.true.)) :: reduced_group_info_made
    ! Set .true. if the reduced group information has been made

   integer(kind=kind(1)) :: n_asymmetric_unit_atoms
    ! No. of atoms in the asymmetric unit of the unit cell.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: asymmetric_unit_geometry => NULL()
    ! The asymmetric unit cell geometry. Usually inputted from a type(cif_type) file. This
    ! may or may not be the same as fragment_cell_geometry. It is .not. used for
    ! structure factor calculations, but for cluster generation.

   integer(kind=kind(1)) :: n_unit_cell_atoms
    ! Total no. of unit cell atoms.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: unit_cell_geometry => NULL()
    ! The full unit cell geometry, in the crystal coordiante system, generated
    ! from the asymmetric_unit_geometry.

   integer(kind=kind(1)), dimension(:), pointer :: symop_for_unit_cell_atom => NULL()
    ! "symop_for_unit_cell_atom(a)" is the index of the spacegroup symmetry
    ! operation that generates atom position "unit_cell_geometry(:,a)" from a
    ! unique atom position in "asymmetric_unit_geometry".

   integer(kind=kind(1)), dimension(:), pointer :: atom_for_unit_cell_atom => NULL()
    ! "atom_for_unit_cell_atom(a)" is the index of the unique unit cell atom
    ! in "asymmetric_unit_geometry" which generates atom position
    ! "unit_cell_geometry(:,a)", using "symop_for_unit_cell_atom(a)".

   type(reflection_type), dimension(:), pointer :: reflections => NULL()
    ! The structure factor information

   real(kind=kind(1.0d0)) :: scale_factor
    ! Scale factor to apply to the predicted structure factors

   real(kind=kind(1.0d0)) :: exp_scale_factor
    ! Fixed scale factor to apply to the experimental structure factors

   logical(kind=kind(.true.)) :: optimise_scale
    ! True if an overall scaler factor is to be optimised, for use in calculating
    ! F_pred

   logical(kind=kind(.true.)) :: synthesize_sigma_I
    ! True if artificial sigma(I) errors are to be gereated from poisson
    ! statistics and used in calculating agreement statistics

   logical(kind=kind(.true.)) :: optimise_extinction
    ! True if extinction is to be optimised.

   logical(kind=kind(.true.)) :: correct_dispersion
    ! True if dispersion is to be corrected

   real(kind=kind(1.0d0)) :: extinction_factor
    ! Secondary extinction factor

   real(kind=kind(1.0d0)) :: wavelength
    ! Experiment wavelength

   character(128) :: thermal_smearing_model
    ! Thermal smearing model for ft integrals

   character(128) :: partition_model
    ! Model for partitioning fragments of the molecule

   integer(kind=kind(1)) :: n_param
    ! No of fitting parameters used

   end type

   type cluster_type

   real(kind=kind(1.0d0)) :: radius = 0.0d0
    ! The radius of the cluster. This number determines the maximum acceptable
    ! distance between an atom in fragment_geometry and its crystal transformed
    ! image. It defines the cluster.

   character(128) :: add_criteria = "fragment"
    ! The add criteria, i.e. whether to add atoms by whole clusters within a
    ! certain distance (radius) of the starting fragment, or by individual atoms
    ! within a certain distance of the starting fragment.

   logical(kind=kind(.true.)) :: start_with_fragment = .false.
    ! Start building the cluster from the initial fragment geometry

   logical(kind=kind(.true.)) :: defragment = .true.
    ! If .true., the cluster ends are defragmented, i.e. any atoms which are bonded
    ! at the ends of the cluster are included into the cluster.

   integer(kind=kind(1)) :: n_atoms
    ! The number of atoms in the cluster

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: geometry => NULL()
    ! The (3 x .n_atoms) sized array of cluster atom positions

   type(crystal_type), pointer :: crystal => NULL()
    ! The crystal information used to generate the cluster

   type(atom_type), dimension(:), pointer :: asymmetric_cell_atom => NULL()
    ! The atom list data associated with the *asymmetric* unit cell_geometry (see
    ! below) used to generate the cluster atom positions.

   integer(kind=kind(1)) :: n_fragment_atoms
    ! The number of fragment atoms used to build the cluster

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: fragment_geometry => NULL()
    ! The (3 x .n_fragment_atoms) sized array of fragment atom positions used to
    ! generate the cluster.

   integer(kind=kind(1)) :: n_symop
    ! The number of symmetry operators used to generate the cluster

   integer(kind=kind(1)), dimension(:,:), pointer :: symop => NULL()
    ! The (4 x .n_symop) sized list of symmetry operators used to generate the
    ! cluster. symop(1,q) is the index of the Seitz operator, while symop(2:4,q)
    ! is the translation vector applied (in the crystal axis coordinate system).

   real(kind=kind(1.0d0)), dimension(3) :: fragment_width
    ! The width of the crystal fragment, to the nearest unit cell

   integer(kind=kind(1)), dimension(3) :: fragment_offset
    ! The center point of the fragment_geometry, to the nearest unit cell

   integer(kind=kind(1)), dimension(:), pointer :: symop_for_atom => NULL()
    ! symop_for_atom(a) is the *index* "s" of the symmetry operation in .symop,
    ! .symop(:,s), used to generate the cluster atom "a", whose positon is given
    ! by .geometry(:,a).

   integer(kind=kind(1)), dimension(:), pointer :: parent_for_atom => NULL()
    ! parent_for_atom(a) is the index of the unique asymmetric unit cell atoms in
    ! *used to generate* the cluster atom "a" using one of the symmetry
    ! operations in .symop (specifically the symop with index .symop_for_atom(a)).

   integer(kind=kind(1)), dimension(:,:), pointer :: atom_for_cell_atom => NULL()
    ! atom_for_cell_atom(a,s) is the index of the cluster atom in .geometry which
    ! is *generated by* asymmetric cell atom "a", whose position is in
    ! .crystal.asymmetric_cell_geometry(:,a), by the symop with index "s",
    ! .symop(:,s). This is the inverse information array of parent_for_atom(a).

   real(kind=kind(1.0d0)), dimension(:), pointer :: minimum_distance_to_atom => NULL()
    ! minimum_distance_to_atom(a) is the minimum distance from the cluster
    ! atom "a", whose position is given in .geometry(:,a), to the crystal
    ! fragment, whose geometry is given in .fragment_geometry

   integer(kind=kind(1)), dimension(:), pointer :: closest_fragment_atom_to_atom => NULL()
    ! closest_fragment_atom_to_atom(a) is the index of the atom in
    ! .fragment_geometry which is closest to the cluster atom "a", whose positon
    ! is .geometry(:,a).

   logical(kind=kind(.true.)), dimension(:), pointer :: is_fragment_atom => NULL()
    ! is_fragment_atom(a) is .true. if .geometry(:,a) is the position of a fragment
    ! atom, i.e. if "a" is the index of a fragm,ent atom.

   logical(kind=kind(.true.)), dimension(:), pointer :: symop_is_redundant => NULL()
    ! symop_is_redundant(q) is .true. if .symop(:,q) does not generate any new
    ! cluster atom (cluster atom positions are stored in .geometry). Instead,
    ! symops earlier in the .symop list are able to generate the atoms that symop
    ! "q" can generate.

   real(kind=kind(1.0d0)), dimension(:), pointer :: partition_factor => NULL()
    ! A list of partition factors which can be applied to a density matrix in
    ! order to partition it. This array can be generated automatically, or it can
    ! be explicitly inputted.

   logical(kind=kind(.true.)) :: info_made = .false.
    ! Set to true if the routine make_info has been called

   end type

   type scfdata_type

   character(128) :: scf_kind
    ! The kind of SCF calculation to perform

   character(128) :: dft_exchange
    ! The DFT exchange functional to be used

   character(128) :: dft_correlation
    ! The DFT correlation functional to be used

   logical(kind=kind(.true.)) :: dft_non_local_exchange
    ! .true. if the exchange functional is non local, otherwise false.

   logical(kind=kind(.true.)) :: dft_non_local_correlation
    ! .true. if the correlation functional is non local, otherwise false.

   character(128) :: initial_density
    ! The kind of density matrix to start the SCF calc

   character(128) :: initial_mos
    ! The kind of initial MO's to start the SCF calc

   real(kind=kind(1.0d0)) :: nuclear_energy
    ! Nuclear repulsion energy for the associated molecule

   real(kind=kind(1.0d0)) :: kinetic_energy
    ! The kinetic energy for the associated molecule

   real(kind=kind(1.0d0)) :: dft_energy_correction
    ! The DFT energy correction to the SCF energy for the associated molecule

   real(kind=kind(1.0d0)) :: energy
    ! The SCF energy for the associated molecule

   real(kind=kind(1.0d0)) :: old_energy
    ! The SCF energy from the previous SCF cycle

   real(kind=kind(1.0d0)) :: difference
    ! The change in the SCF energy between cycles

   real(kind=kind(1.0d0)) :: convergence
    ! A number which measures the convergnece of the SCF

   real(kind=kind(1.0d0)) :: diis_convergence
    ! A limit below which the type(diis_type) is deemed converged

   integer(kind=kind(1)) :: diis_start_iteration = 3
    ! Which iteration of the SCF to start the type(diis_type)

   logical(kind=kind(.true.)) :: using_rough_convergence
    ! Whether to apply lower integral accuracy

   real(kind=kind(1.0d0)) :: rough_convergence
    ! How much to converge to before increasing integral accuracy

   real(kind=kind(1.0d0)) :: rough_diis_convergence
    ! How much to converge the type(diis_type) error to before increasing integral accuracy

   integer(kind=kind(1)) :: iteration
    ! The interation count for the SCF procedure

   integer(kind=kind(1)) :: total_iterations
    ! The total interation count for the SCF procedure, which does not get reset
    ! after each lambda increment.

   integer(kind=kind(1)) :: lambda_iteration
    ! Lambda iteration count for the x-ray SCF procedure

   integer(kind=kind(1)) :: min_iterations
    ! The minimum number of SCF iterations to perform

   integer(kind=kind(1)) :: max_iterations
    ! The maximum number of SCF iterations to perform

   real(kind=kind(1.0d0)) :: lambda
    ! The initial lambda value to use in an x-ray SCF procedure

   real(kind=kind(1.0d0)) :: lambda_max
    ! The maximum lambda value to use in an x-ray SCF procedure

   real(kind=kind(1.0d0)) :: lambda_step
    ! The value to step the lambda value between lambda cycles in an x-ray SCF
    ! calc.

   real(kind=kind(1.0d0)) :: fit_value
    ! The value of 2.718281828459045d0 + lambda * chi2 in an x-ray SCF calc.

   real(kind=kind(1.0d0)) :: old_fit_value
    ! The value of 2.718281828459045d0 + lambda * chi2 of the previous iteration in an x-ray SCF
    ! calc.

   real(kind=kind(1.0d0)) :: F_chi2
    ! Chi^2 agreement statistic for an x-ray SCF calc

   real(kind=kind(1.0d0)) :: old_F_chi2
    ! Chi^2 agreement statistic for an x-ray SCF calc of the previous iteration

   real(kind=kind(1.0d0)) :: F_gof
    ! Goodness-of-fit agreement statistic for x-ray SCF calc

   real(kind=kind(1.0d0)) :: F_r_factor
    ! R-factor agreement statistic for x-ray SCF calc

   real(kind=kind(1.0d0)) :: F_weighted_r_factor
    ! The weighted r-factor agreement statistic

   logical(kind=kind(.true.)) :: test
    ! Test flag. Set True if some test procedure is to be executed

   logical(kind=kind(.true.)) :: direct
    ! True if using direct SCF

   logical(kind=kind(.true.)) :: using_delta_build
    ! True if using an incremental (delta) fock build as advocated by Almlof.

   logical(kind=kind(.true.)) :: using_fock_diis
    ! True if using type(diis_type) extrapolation for fock matrices

   logical(kind=kind(.true.)) :: using_MO_diis
    ! True if using type(diis_type) extrapolation for molecular orbitals

   logical(kind=kind(.true.)) :: using_damping
    ! True if using damping

   logical(kind=kind(.true.)) :: using_level_shift
    ! True if using level shifting

   logical(kind=kind(.true.)) :: using_camp_king
    ! True if using Camp-King converger

   integer(kind=kind(1)) :: camp_king_iters
    ! How many iterations the Camp-King converger took in SCF cycle

   logical(kind=kind(.true.)) :: using_dynamic_damping
    ! True if using the Dynamic Damper

   real(kind=kind(1.0d0)) :: dynamic_damp_factor
    ! Damp factor used by the Dynamic Damper

   type(diis_type) :: diis
    ! For diis extrapolation (usually Fock matrix type(diis_type) extrapolation)

   real(kind=kind(1.0d0)) :: diis_error
    ! The type(diis_type) error

   real(kind=kind(1.0d0)) :: old_diis_error
    ! The type(diis_type) error of the previous iteration

   logical(kind=kind(.true.)) :: using_diis_auto_start
    ! Set .true. if starting type(diis_type) automatically based on the size of
    ! the diis error

   integer(kind=kind(1)) :: damp_finish
    ! Iteration when density matrix damping is turned off

   real(kind=kind(1.0d0)) :: damp_factor
    ! The damping factor to use

   integer(kind=kind(1)) :: level_shift_finish
    ! Iteration when level shifting is turned off

   real(kind=kind(1.0d0)) :: level_shift
    ! Value to level shift the virtual orbitals

   logical(kind=kind(.true.)) :: output
    ! True if output is wanted

   logical(kind=kind(.true.)) :: nddo
    ! Neglect of diatomic differential overlap

   logical(kind=kind(.true.)) :: nudo
    ! Neglect of unconnected differential overlap

   logical(kind=kind(.true.)) :: pie
    ! Projective integral expansion method (PIE) developed by Mayer.
    ! See Mayer, CPL 332, 381 (2000).

   logical(kind=kind(.true.)) :: using_bl_term
    ! Switch on/off the B.L term (complex SCF reqd.)

   logical(kind=kind(.true.)) :: using_bs_term
    ! Switch on/off the B.S term

   logical(kind=kind(.true.)) :: using_bs_t_term
    ! Switch on/off the (B.S) T_e term

   logical(kind=kind(.true.)) :: using_aa_term
    ! Switch on/off the A.A diamagnetic term

   logical(kind=kind(.true.)) :: using_1e_sl_term
    ! Switch on/off the 1 electron S.L term

   logical(kind=kind(.true.)) :: using_1e_srxa_term
    ! Switch on/off the 1 electron diamagnetic term

   logical(kind=kind(.true.)) :: using_1e_zora_term
    ! Switch on/off the 1 electron ZORA terms

   real(kind=kind(1.0d0)) :: sl_1e_factor
    ! Factor to apply to the 1-electron S.L terms

   logical(kind=kind(.true.)) :: using_2e_sl_term
    ! Switch on/off the 2 electron S.L terms

   real(kind=kind(1.0d0)) :: sl_2e_factor
    ! Factor to apply to the 2-electron S.L terms

   real(kind=kind(1.0d0)) :: eri_limit
    ! Cutoff for the two electron integrals

   real(kind=kind(1.0d0)) :: old_eri_cutoff
    ! Previous iteration's cutoff for the two electron integrals, used for
    ! detecting if an incremental fock build is required.

   real(kind=kind(1.0d0)), dimension(3) :: quantization_axis = 0.0d0
    ! Quantization axis for GCHF, if using initial MO's as a guess

   logical(kind=kind(.true.)) :: group
    ! Set .true. if doing a noninteracting group SCF calculation

   logical(kind=kind(.true.)) :: using_MO_gradient_update
    ! Set .true. if using the gradient of the orbital coefficients to
    ! update the coefficients

   real(kind=kind(1.0d0)) :: MO_gradient_stepsize
    ! The stepisize to use for updating the orbitals

   real(kind=kind(1.0d0)) :: max_update_stepsize
    ! The maximum update stepsize to use for updating any SCF object, e.g. the orbitals

   end type

   type colour_type

   character(128) :: name
    ! The standard colour name for this colour.

   integer(kind=kind(1)), dimension(3) :: RGB255
    ! The RGB triple for this colour as a triple of integers between 0 and 255.

   end type

   type colourfunction_type

   integer(kind=kind(1)) :: n_data
    ! The number of data values (and their associated colours) used to make
    ! the colour function

   real(kind=kind(1.0d0)), dimension(:), pointer :: data => NULL()
    ! The list of data values, from smallest to largest

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: RGB => NULL()
    ! The RGB values corresponding to each data value. Their norm should be
    ! between zero and 3.

   logical(kind=kind(.true.)) :: finalised
    ! Set .true. if the object is ready for use

   end type

   type marchingcube_type

   real(kind=kind(1.0d0)), dimension(3,0:7) :: vertex_pos
    ! A (3 x 0:7) dimensioned list of the actual coordinates of each cube vertex.
    ! The second index is the standard marching cubes vertex number.

   real(kind=kind(1.0d0)), dimension(0:7) :: value_at_vertex
    ! A (0:7) dimensioned list of the values of the function at each cube vertex.

   real(kind=kind(1.0d0)), dimension(3,0:7) :: vertex_gradient
    ! A (3 x 0:7) dimensioned list of the vertex gradients at each cube vertex.
    ! The second index is the standard marching cubes vertex number.

   real(kind=kind(1.0d0)), dimension(3,3,0:7) :: vertex_hessian
    ! A (3 x 3 x 0:7) dimensioned list of the hessian at each cube vertex.
    ! The third index is the standard marching cubes vertex number.

   real(kind=kind(1.0d0)) :: side_length
    ! The length of each side of the cube

   real(kind=kind(1.0d0)) :: iso_value
    ! The isovalue to be used for the isosurface

   real(kind=kind(1.0d0)) :: accuracy
    ! The function accuracy to which each isosurface point is determined

   integer(kind=kind(1)) :: case
    ! The case number of this marching cube, for lookup in edge_table and
    ! triangle_table

   integer(kind=kind(1)) :: interior_case
    ! The case number corresponding to a cube being wholly within a surface.
    ! Normally this is when the interior of the surface has larger values than
    ! the exterior, and in this case the default is 0.

   integer(kind=kind(1)) :: exterior_case
    ! The case number corresponding to a cube being wholly outside a surface.
    ! Normally this is when the interior of the surface has smaller values than
    ! the exterior, and in this case the default is 255.

   integer(kind=kind(1)) :: edge_bit_string
    ! The edge bit string for the marching cube. This is just the appropriate
    ! element of the edge_table i.e. if the bit is set, then that edge crosses
    ! the isosurface and must be interpolated.

   integer(kind=kind(1)) :: skip_bit_string
    ! A bit string which tells whether to skip processing certain cube edges.
    ! Can be used when certain edge vertex points are already done.

   integer(kind=kind(1)) :: cube_bit_string
    ! The grad bit string for the marching cube. This tells which *vertices* of
    ! the cube cross the isosurface and must be interpolated.

   integer(kind=kind(1)) :: n_edge
    ! The number of marching cube edges that need to be considered on this
    ! marching cube

   integer(kind=kind(1)) :: n_triangle
    ! The number of triangles formed on this marching cube

   integer(kind=kind(1)) :: n_pt
    ! The number of points up until this cube was analysed. Used to keep a
    ! list of unique indices to define triangle vertices and points.

   integer(kind=kind(1)), dimension(3,5) :: triangle_edge_index
    ! The (3 x .n_triangle) dimensioned list of 3-edges on the marching cube
    ! used to form triangles. This is just the appropriate row from the
    ! triangle_table. The maximum number of triangles is 5.

   integer(kind=kind(1)), dimension(3,5) :: triangle_vertex_index
    ! The (3,.n_triangle) dimensioned list of groups of 3 *unique* triangle
    ! vertex *indices*. These indices point to a unique list of points. The
    ! maximum number of triangles is 5.

   real(kind=kind(1.0d0)), dimension(3,0:11) :: edge_vertex_pos
    ! A (3 x 0:11) dimensioned list of the interpolated triangle vertex
    ! coordinates on each edge of the cube (if that edge is needed).

   real(kind=kind(1.0d0)), dimension(3,0:11) :: edge_vertex_gradient
    ! A (3 x 0:11) dimensioned list of the interpolated triangle vertex normals
    ! on each edge of the cube (if that edge is needed).

   real(kind=kind(1.0d0)), dimension(3,3,0:11) :: edge_vertex_hessian
    ! A (3 x 3 x 0:11) dimensioned list of the interpolated triangle vertex
    ! hessians on each edge of the cube (if that edge is needed).

   real(kind=kind(1.0d0)), dimension(0:11) :: edge_mean_curvature
    ! A (0:11) dimensioned list of the (interpolated) mean curvatures on each
    ! edge of the cube that crosses the isosurface (if that edge is needed).

   real(kind=kind(1.0d0)), dimension(0:11) :: edge_gaussian_curvature
    ! A (0:11) dimensioned list of the (interpolated) gaussian curvatures on each
    ! edge of the cube that crosses the isosurface (if that edge is needed).

   integer(kind=kind(1)), dimension(0:11) :: edge_vertex_index
    ! A (0:11) dimensioned list of the *unique* triangle vertex *indices* => NULL()
    ! for each edge of the marching cube (if that edge is needed).

   integer(kind=kind(1)), dimension(12) :: vertex_edge_index
    ! A list of the edge indices for each unique triangle vertex index.
    ! Essentially, this is the reverse mapping of edge_vertex_index.

   end type

   type isosurface_type

   character(128) :: iso_kind
    ! The kind of isosurface plot, if known. This helps in deciding which way the
    ! normals of the isosurface should point.

   character(128) :: triangulation_method
    ! The method used to triangulate the isosurface.

   real(kind=kind(1.0d0)) :: iso_value
    ! The isovalue to be used for the isosurface

   type(plotgrid_type) :: grid
    ! The isosurface plotgrid

   integer(kind=kind(1)) :: n_pt
    ! The number of isosurface points

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: point => NULL()
    ! A (3 x n_pt) list of points on the isosurface

   integer(kind=kind(1)) :: n_face
    ! The number of triangulated faces on the isosurface

   integer(kind=kind(1)), dimension(:,:), pointer :: face => NULL()
    ! A (3 x n_face) list of the triangular faces of the surface. Each face is
    ! represented by three integers which move in an anticlockwise direction when
    ! viwed from the outside.

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: point_gradient => NULL()
    ! A (3 x n_pt) list of the function gradient vectors for every point on the
    ! isosurface

   real(kind=kind(1.0d0)), dimension(:), pointer :: point_mean_curvature => NULL()
    ! A (3 x n_pt) list of the mean surface curvatures for every point on the
    ! isosurface

   real(kind=kind(1.0d0)), dimension(:), pointer :: point_gaussian_curvature => NULL()
    ! A (3 x n_pt) list of the gaussian surface curvatures for every point on the
    ! isosurface

   real(kind=kind(1.0d0)) :: volume
    ! The best estimate of the interior volume of the isosurface (the average of
    ! .volume_min and .volume_max).

   real(kind=kind(1.0d0)) :: volume_min
    ! A lower bound to the interior volume of the isosurface.

   real(kind=kind(1.0d0)) :: volume_max
    ! An upper bound to the interior volume of the isosurface.

   integer(kind=kind(1)) :: n_skip
    ! The number of function evaluations skipped (recursive method only)

   integer(kind=kind(1)) :: level
    ! The current level to which the initial box has been divided, in the
    ! recursive marching cube algorithm.

   integer(kind=kind(1)) :: final_level
    ! The final level to which the initial box must be divided, in the
    ! recursive marching cube algorithm.

   integer(kind=kind(1)) :: scan_level
    ! The level to which the initial box must be divided, in the recursive
    ! marching cube algorithm, to achieve resolution of all important features.

   real(kind=kind(1.0d0)) :: del
    ! The current box side length in the recursive marching cubes algorithm.

   integer(kind=kind(1)) :: x
    ! The (partial) x-coordinate of the box in the recursive marching cubes
    ! algorithm.  This is a binary number, with each bit from the right
    ! representing a segment double the size to which the current box belongs.

   integer(kind=kind(1)) :: y
    ! The (partial) y-coordinate of the box in the recursive marching cubes
    ! algorithm.  This is a binary number, with each bit from the right
    ! representing a box double the size to which the current box belongs.

   integer(kind=kind(1)) :: z
    ! The (partial) z-coordinate of the box in the recursive marching cubes
    ! algorithm.  This is a binary number, with each bit from the right
    ! representing a box double the size to which the current box belongs.

   type(intvecintvechash_type), pointer :: hash => NULL()
    ! A hash table storing marching cube edge_vertex_index information as a
    ! function of the cube coordinates, [.x,.y,.z]

   logical(kind=kind(.true.)) :: big_interior
    ! Set .true. if the interior of the isosurface is bigger than the exterior,
    ! i.e. if the point_normals are to be reversed on output. This switch could
    ! probably be determined automatically, assuming the botton left had corner
    ! of the plot (the first point) was "outside".

!  shift :: REALVEC* => NULL()
!  ! A list of distances representing how far each point shifted from its
!  ! estimate to come to the isosurface.

!  adjoining_face :: INTMAT* => NULL()
!  ! A (3 x n_face) list of the three adjoining faces for a particular face

!  adjoining_edge :: INTMAT* => NULL()
!  ! A (3 x n_face) list of the adjoining edge for the adjoining faces for
!  ! a particular face

!  ok :: BINVEC* => NULL()
!  ! A list of switches telling if each face is acceptably smooth.

!  ok_neighbours :: BINVEC* => NULL()
!  ! A list of switched telling if all three neighbours of each face is acceptably smooth.

!  n_skip :: integer(kind=kind(1))
!  ! The number of faces which are acceptapbly smooth, and can be skippped

!  smallness :: real(kind=kind(1.0d0))
!  ! The maximum acceptable distance between triangulated points.
!  ! Used as a face smallness criteria.

!  flatness :: real(kind=kind(1.0d0))
!  ! The maximum acceptable distance between a triangulated point and its ray origin
!  ! i.e. its "shift" (see above).  Used as a face flatness criteria.

!  accuracy :: real(kind=kind(1.0d0))
!  ! The accuracy to which each isosurface point is determined

   character(128) :: surface_property
    ! The name of a surface property to plot or calculate

   real(kind=kind(1.0d0)), dimension(:), pointer :: surface_property_values => NULL()
    ! The values of .surface_property as evaluated on the surface.

   logical(kind=kind(.true.)) :: chop_surface_property_range
    ! Whether to use surface_property_cutoff_range, rather than scaling the
    ! smallest value to zero and the largest to one.

   real(kind=kind(1.0d0)), dimension(2) :: surface_property_cutoff_range
    ! The cutoffs for values of .surface_property for colouring.  Values of the
    ! property outside this range will be chopped.

   real(kind=kind(1.0d0)), dimension(3) :: surface_point
    ! A special point which lies on or near the isosurface, used for calculations
    ! of connected area, for example.

   real(kind=kind(1.0d0)) :: surface_property_lower_bound
    ! A lower bound to the property value, for calculating surface areas.

   real(kind=kind(1.0d0)) :: surface_property_upper_bound
    ! A upper bound to the property value, for calculating surface areas.

   type(colourfunction_type), pointer :: colour => NULL()
    ! A colourfunction used for colouring the isourface

   type(atom_type), dimension(:), pointer :: atom => NULL()
    ! A list of atoms associated with ths isosurface.

   logical(kind=kind(.true.)) :: use_interpolator
    ! If set .true., then the routine used to calculate the isosurface may use
    ! interpolation tables, rather than exact values, if possible.

   end type

   type roby_type

   character(128) :: roby_kind
    ! The kind of Roby calculation to perform

   real(kind=kind(1.0d0)), dimension(:), pointer :: n1 => NULL()
    ! Roby population for each atom-group

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: n2 => NULL()
    ! Roby pair population for each atom-group pair
    ! NOTE: these are *not* shared populations

   real(kind=kind(1.0d0)) :: n_shared
    ! Roby multiple shared population for a specified atom-group

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: bond_index => NULL()
    ! Gould Bond indices for each pair of atoms

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: percent_covalency => NULL()
    ! The % covalency of the given bond pair

   real(kind=kind(1.0d0)), dimension(:), pointer :: gould_charge => NULL()
    ! Roby-Gould charges

   real(kind=kind(1.0d0)), dimension(:), pointer :: cruickshank_charge => NULL()
    ! Cruikshank-Avramedes charges

   real(kind=kind(1.0d0)), dimension(:), pointer :: summed_n2 => NULL()
    ! Summed atom-group pair populations

   real(kind=kind(1.0d0)), dimension(:), pointer :: summed_n3 => NULL()
    ! Summed atom-group triple populations

   integer(kind=kind(1)), dimension(:), pointer :: atom_list => NULL()
    ! The indices of a list of roby atoms to be used in some way
    ! for population analysis

   type(intvec__type), dimension(:), pointer :: atom_group => NULL()
    ! The indices of the roby atoms defining different spaces

   logical(kind=kind(.true.)) :: analyse_all_atom_pairs
    ! If set .true., the bond index information is printed out ONLY for every
    ! atom pair which is considered "bonded". Otherwise all pairs of atoms are
    ! analysed. This is onlyeffective when an atom_list is defined.

   real(kind=kind(1.0d0)) :: bond_scale_factor
    ! Used to multiply the sum of the Bragg-Slater radii for two atoms,
    ! to determine a distance cutoff within which the atoms are regarded
    ! to be bonded

   real(kind=kind(1.0d0)) :: covalent_cutoff
    ! Angles (in radians) greater than this are ignored when calculating
    ! the covalent bond index

   real(kind=kind(1.0d0)) :: ionic_cutoff
    ! Angles (in radians) greater than this are ignored when calculating
    ! the ionic bond index

   real(kind=kind(1.0d0)) :: pi_on_2_cutoff
    ! Angles (in radians) greater than this are regareded as pi/2

   real(kind=kind(1.0d0)) :: zero_cutoff
    ! Angles (in radians) *less* than this are regareded as zero

   real(kind=kind(1.0d0)) :: occupied_ANO_cutoff
    ! Atomic natural orbitals with occupations less than this number
    ! are regarded as unoccupied. This number is used to define the Roby
    ! atomic projector

   logical(kind=kind(.true.)) :: output_theta_info
    ! If set .true., the bond index information is printed out for every
    ! space V_theta. See the paper for details.

   integer(kind=kind(1)), dimension(:), pointer :: atom_a => NULL()
    ! The indices of the atoms defining space V_A

   integer(kind=kind(1)), dimension(:), pointer :: atom_b => NULL()
    ! The indices of the atoms defining space V_B

   integer(kind=kind(1)), dimension(:), pointer :: atom_ab => NULL()
    ! The indices of the roby atoms defining spaces V_A and V_B

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: theta_C => NULL()
    ! The matrix of the covalent theta orbitals

   real(kind=kind(1.0d0)), dimension(:), pointer :: eval_C => NULL()
    ! The array of the covalent theta eigenvalues

   real(kind=kind(1.0d0)), dimension(:), pointer :: theta_angle => NULL()
    ! The array of covalent theta angles

   integer(kind=kind(1)), dimension(:), pointer :: pair => NULL()
    ! An array which pairs the positive eigenvalues in eval_C with the
    ! negative eigenvalues, thus definining each theta subspace

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: theta_I => NULL()
    ! The matrix of the covalent theta orbitals

   real(kind=kind(1.0d0)), dimension(:), pointer :: eval_I => NULL()
    ! The matrix of the covalent theta eigenvalues

   real(kind=kind(1.0d0)), dimension(:), pointer :: pop_C => NULL()
    ! Covalent theta orbital popualtions

   real(kind=kind(1.0d0)), dimension(:), pointer :: pop_I => NULL()
    ! Covalent theta orbital popualtions

   real(kind=kind(1.0d0)), dimension(:), pointer :: pop_A => NULL()
    ! Atom "A" theta orbital popualtions

   real(kind=kind(1.0d0)), dimension(:), pointer :: pop_B => NULL()
    ! Atom "B" theta orbital popualtions

   real(kind=kind(1.0d0)), dimension(:), pointer :: covalent_index => NULL()
    ! The vector of each covalent theta bond index

   real(kind=kind(1.0d0)), dimension(:), pointer :: ionic_index => NULL()
    ! The vector of each covalent theta bond index

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: proportion_a => NULL()
    ! The proportion to partition for atom A, between two atoms (A,B)
    ! using Gould's probabilistic scheme

   integer(kind=kind(1)) :: charge
    ! The total charge on the molecule

   integer(kind=kind(1)) :: mult
    ! The spin multiplicity of the molecule

   type(opmatrix_type), pointer :: rho => NULL()
    ! The density matrix of the molecule

   real(kind=kind(1.0d0)), dimension(:,:), pointer :: overlap_matrix => NULL()
    ! The full molecular overlap matrix for Roby analysis

   type(atom_type), dimension(:), pointer :: atom => NULL()
    ! The actual list of atoms to be used in the Roby calculations
    ! Usually this will come from a type(mol_type) object

   end type

   type mol_type

   character(128) :: name
    ! Name of molecule

   integer(kind=kind(1)) :: charge
    ! Electric charge of the molecule

   logical(kind=kind(.true.)) :: minimal_io = .true.
    ! Outputs no data files unless requested

   integer(kind=kind(1)) :: mult
    ! Spin multiplicity of the molecule

   real(kind=kind(1.0d0)), dimension(3) :: E_field
    ! Applied electric field in atomic units

   real(kind=kind(1.0d0)), dimension(3) :: B_field
    ! Applied magnetic field in atomic units

   real(kind=kind(1.0d0)), dimension(3) :: gauge_origin
    ! Global gauge origin for magnetic field

   type(atom_type), dimension(:), pointer :: atom => NULL()
    ! List of atoms in molecule

   character(128) :: basis_set_kind
    ! A suffix string representing the name of the basis set class
    ! to be used for each atom

   type(basis_type), dimension(:), pointer :: basis => NULL()
    ! List of basis sets used

   type(slaterbasis_type), dimension(:), pointer :: slaterbasis => NULL()
    ! List of Slater basis sets used

   type(coppensbasis_type), dimension(:), pointer :: coppensbasis => NULL()
    ! List of coppens basis sets used

   logical(kind=kind(.true.)) :: basis_info_made
    ! Set .true. if the gaussian basis set info has been made.

   type(plotgrid_type), pointer :: grid => NULL()
    ! Rectangular grid data, for plots

   type(isosurface_type), pointer :: isosurface => NULL()
    ! An object used for creating triangluated meshes for isosurface plots.

   type(dftgrid_type), pointer :: dftgrid => NULL()
    ! DFT integration grid data

   type(crystal_type), pointer :: crystal => NULL()
    ! Crystal data for the enclosing crystal

   type(cluster_type), pointer :: cluster => NULL()
    ! Crystal cluster data

   type(cif_type), pointer :: cif => NULL()
    ! Crystallographic Information file (type(cif_type)) object

   type(pointgroup_type), pointer :: pointgroup => NULL()
    ! Pointgroup symmetry of the molecule

   type(mol_type), pointer :: saved => NULL()
    ! For saving self and temporarily using an alternative in its place

   integer(kind=kind(1)) :: n_e
    ! No of electrons

   integer(kind=kind(1)) :: n_a
    ! No of alpha electrons

   integer(kind=kind(1)) :: n_b
    ! No of beta electrons

   integer(kind=kind(1)) :: n_atom
    ! No of atoms

   integer(kind=kind(1)) :: n_atom_kind
    ! No of atoms of a different kind

   integer(kind=kind(1)) :: n_basis
    ! No of basis sets

   integer(kind=kind(1)) :: n_shell
    ! Total number of shells in the molecular basis set

   integer(kind=kind(1)) :: n_shell_pairs
    ! Total number of shell pairs in the molecular basis set

   integer(kind=kind(1)) :: n_bf
    ! Total number of basis functions in the molecular basis set

   integer(kind=kind(1)) :: n_prim
    ! Total number of primitives in the molecular basis set

   integer(kind=kind(1)) :: n_unique_shells
    ! Number of unique shells in the basis set.

   integer(kind=kind(1)), dimension(:), pointer :: atom_for_shell => NULL()
    ! Atom index for molecular shell index array

   integer(kind=kind(1)), dimension(:), pointer :: atom_shell_for_shell => NULL()
    ! Atom shell index for molecular shell index array

   integer(kind=kind(1)), dimension(:), pointer :: first_shell_for_atom => NULL()
    ! First molecule shell index for an atom

   integer(kind=kind(1)), dimension(:), pointer :: last_shell_for_atom => NULL()
    ! Last molecule shell index for an atom

   integer(kind=kind(1)), dimension(:), pointer :: basis_shell_for_shell => NULL()
    ! Map a shell of the molecule to a shell of the basis set

   integer(kind=kind(1)), dimension(:), pointer :: first_basis_fn_for_shell => NULL()
    ! First basis function for a given shell

   integer(kind=kind(1)), dimension(:), pointer :: last_basis_fn_for_shell => NULL()
    ! Last basis function for a given shell

   integer(kind=kind(1)), dimension(:), pointer :: first_basis_fn_for_atom => NULL()
    ! For the atom basis function limits

   integer(kind=kind(1)), dimension(:), pointer :: last_basis_fn_for_atom => NULL()
    ! For the atom basis function limits

   type(shellpair_type), dimension(:), pointer :: precomputed_basis_shellpair => NULL()
    ! Precomputed stuff for shellpairs of the basis set, to speed up
    ! later calculations of gaussian integrals.

   integer(kind=kind(1)), dimension(:), pointer :: atom_kind => NULL()
    ! The unique kind of each atom in .atom

   integer(kind=kind(1)), dimension(:), pointer :: unique_atom => NULL()
    ! List of the unique atoms (of different kind)

   type(scfdata_type), pointer :: scfdata => NULL()
    ! SCF data object

   type(opvector_type), pointer :: orbital_energies => NULL()
    ! The orbital energies

   type(opmatrix_type), pointer :: molecular_orbitals => NULL()
    ! The real molecular orbitals

   type(opmatrix_type), pointer :: density_matrix => NULL()
    ! The real density matrix

   type(opmatrix_type), pointer :: natural_orbitals => NULL()
    ! The natural orbitals

   type(opvector_type), pointer :: occupation_numbers => NULL()
    ! The natural orbital occupation numbers

   type(opmatrix_type), pointer :: fock_matrix => NULL()
    ! The real fock matrix

   type(opmatrix_type), pointer :: constraint_matrix => NULL()
    ! The real constraint matrix

   type(opmatrix_type), pointer :: old_molecular_orbitals => NULL()
    ! The real molecular orbitals from the previous SCF iteration

   type(opmatrix_type), pointer :: old_density_matrix => NULL()
    ! The real density matrix from the previous SCF iteration

   type(opmatrix_type), pointer :: old_fock_matrix => NULL()
    ! The real fock matrix from the previous SCF iteration

   type(opmatrix_type), pointer :: old_constraint_matrix => NULL()
    ! The real constraint matrix from the previous SCF iteration

   type(intvec__type), dimension(:), pointer :: atom_group => NULL()
    ! A list of atom group indices, for group property decomposition

   real(kind=kind(1.0d0)), dimension(:), pointer :: atom_group_energy => NULL()
    ! The SCF energy of each atom group

   integer(kind=kind(1)), dimension(:), pointer :: group_charges => NULL()
    ! A list of charges for each group in the molecule, defined in atom_groups.

   logical(kind=kind(.true.)) :: optimise_thermals
    ! Whether to optimise the thermal parameters

   type(roby_type), pointer :: roby => NULL()
    ! A Roby data object

   character(128) :: CIF_file_name = " "
    ! The name of a type(cif_type) file to be used for input

   character(128) :: CIF_data_block_name = " "
    ! The name of a type(cif_type) file data block, to be used for processing

   character(128) :: CX_file_name = " "
    ! The name of the Crystal Explorer (CX) output file

   end type

end module
