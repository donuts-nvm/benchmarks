# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 2.6

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canoncical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Bootstrap.cmk/cmake

# The command to remove a file.
RM = /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Bootstrap.cmk/cmake -E remove -f

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper

# Include any dependencies generated for this target.
include Source/kwsys/CMakeFiles/cmsys_c.dir/depend.make

# Include the progress variables for this target.
include Source/kwsys/CMakeFiles/cmsys_c.dir/progress.make

# Include the compile flags for this target's objects.
include Source/kwsys/CMakeFiles/cmsys_c.dir/flags.make

Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o: Source/kwsys/CMakeFiles/cmsys_c.dir/flags.make
Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o: /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/ProcessUNIX.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles $(CMAKE_PROGRESS_1)
	@echo "Building C object Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -DKWSYS_C_HAS_PTRDIFF_T=1 -DKWSYS_C_HAS_SSIZE_T=1 -o CMakeFiles/cmsys_c.dir/ProcessUNIX.o   -c /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/ProcessUNIX.c

Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.i: cmake_force
	@echo "Preprocessing C source to CMakeFiles/cmsys_c.dir/ProcessUNIX.i"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -DKWSYS_C_HAS_PTRDIFF_T=1 -DKWSYS_C_HAS_SSIZE_T=1 -E /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/ProcessUNIX.c > CMakeFiles/cmsys_c.dir/ProcessUNIX.i

Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.s: cmake_force
	@echo "Compiling C source to assembly CMakeFiles/cmsys_c.dir/ProcessUNIX.s"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -DKWSYS_C_HAS_PTRDIFF_T=1 -DKWSYS_C_HAS_SSIZE_T=1 -S /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/ProcessUNIX.c -o CMakeFiles/cmsys_c.dir/ProcessUNIX.s

Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.requires:
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.requires

Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.provides: Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.requires
	$(MAKE) -f Source/kwsys/CMakeFiles/cmsys_c.dir/build.make Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.provides.build
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.provides

Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.provides.build: Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.provides.build

Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o: Source/kwsys/CMakeFiles/cmsys_c.dir/flags.make
Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o: /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Base64.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles $(CMAKE_PROGRESS_2)
	@echo "Building C object Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/cmsys_c.dir/Base64.o   -c /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Base64.c

Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.i: cmake_force
	@echo "Preprocessing C source to CMakeFiles/cmsys_c.dir/Base64.i"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -E /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Base64.c > CMakeFiles/cmsys_c.dir/Base64.i

Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.s: cmake_force
	@echo "Compiling C source to assembly CMakeFiles/cmsys_c.dir/Base64.s"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -S /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Base64.c -o CMakeFiles/cmsys_c.dir/Base64.s

Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.requires:
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.requires

Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.provides: Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.requires
	$(MAKE) -f Source/kwsys/CMakeFiles/cmsys_c.dir/build.make Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.provides.build
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.provides

Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.provides.build: Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.provides.build

Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o: Source/kwsys/CMakeFiles/cmsys_c.dir/flags.make
Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o: /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/MD5.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles $(CMAKE_PROGRESS_3)
	@echo "Building C object Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/cmsys_c.dir/MD5.o   -c /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/MD5.c

Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.i: cmake_force
	@echo "Preprocessing C source to CMakeFiles/cmsys_c.dir/MD5.i"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -E /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/MD5.c > CMakeFiles/cmsys_c.dir/MD5.i

Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.s: cmake_force
	@echo "Compiling C source to assembly CMakeFiles/cmsys_c.dir/MD5.s"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -S /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/MD5.c -o CMakeFiles/cmsys_c.dir/MD5.s

Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.requires:
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.requires

Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.provides: Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.requires
	$(MAKE) -f Source/kwsys/CMakeFiles/cmsys_c.dir/build.make Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.provides.build
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.provides

Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.provides.build: Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.provides.build

Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o: Source/kwsys/CMakeFiles/cmsys_c.dir/flags.make
Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o: /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Terminal.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles $(CMAKE_PROGRESS_4)
	@echo "Building C object Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/cmsys_c.dir/Terminal.o   -c /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Terminal.c

Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.i: cmake_force
	@echo "Preprocessing C source to CMakeFiles/cmsys_c.dir/Terminal.i"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -E /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Terminal.c > CMakeFiles/cmsys_c.dir/Terminal.i

Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.s: cmake_force
	@echo "Compiling C source to assembly CMakeFiles/cmsys_c.dir/Terminal.s"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -S /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/Terminal.c -o CMakeFiles/cmsys_c.dir/Terminal.s

Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.requires:
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.requires

Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.provides: Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.requires
	$(MAKE) -f Source/kwsys/CMakeFiles/cmsys_c.dir/build.make Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.provides.build
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.provides

Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.provides.build: Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.provides.build

Source/kwsys/CMakeFiles/cmsys_c.dir/System.o: Source/kwsys/CMakeFiles/cmsys_c.dir/flags.make
Source/kwsys/CMakeFiles/cmsys_c.dir/System.o: /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/System.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles $(CMAKE_PROGRESS_5)
	@echo "Building C object Source/kwsys/CMakeFiles/cmsys_c.dir/System.o"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/cmsys_c.dir/System.o   -c /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/System.c

Source/kwsys/CMakeFiles/cmsys_c.dir/System.i: cmake_force
	@echo "Preprocessing C source to CMakeFiles/cmsys_c.dir/System.i"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -E /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/System.c > CMakeFiles/cmsys_c.dir/System.i

Source/kwsys/CMakeFiles/cmsys_c.dir/System.s: cmake_force
	@echo "Compiling C source to assembly CMakeFiles/cmsys_c.dir/System.s"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -S /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/System.c -o CMakeFiles/cmsys_c.dir/System.s

Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.requires:
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.requires

Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.provides: Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.requires
	$(MAKE) -f Source/kwsys/CMakeFiles/cmsys_c.dir/build.make Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.provides.build
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.provides

Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.provides.build: Source/kwsys/CMakeFiles/cmsys_c.dir/System.o
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.provides.build

Source/kwsys/CMakeFiles/cmsys_c.dir/String.o: Source/kwsys/CMakeFiles/cmsys_c.dir/flags.make
Source/kwsys/CMakeFiles/cmsys_c.dir/String.o: /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/String.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles $(CMAKE_PROGRESS_6)
	@echo "Building C object Source/kwsys/CMakeFiles/cmsys_c.dir/String.o"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/cmsys_c.dir/String.o   -c /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/String.c

Source/kwsys/CMakeFiles/cmsys_c.dir/String.i: cmake_force
	@echo "Preprocessing C source to CMakeFiles/cmsys_c.dir/String.i"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -E /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/String.c > CMakeFiles/cmsys_c.dir/String.i

Source/kwsys/CMakeFiles/cmsys_c.dir/String.s: cmake_force
	@echo "Compiling C source to assembly CMakeFiles/cmsys_c.dir/String.s"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && /usr/bin/gcc  $(C_DEFINES) $(C_FLAGS) -S /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys/String.c -o CMakeFiles/cmsys_c.dir/String.s

Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.requires:
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.requires

Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.provides: Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.requires
	$(MAKE) -f Source/kwsys/CMakeFiles/cmsys_c.dir/build.make Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.provides.build
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.provides

Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.provides.build: Source/kwsys/CMakeFiles/cmsys_c.dir/String.o
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.provides.build

# Object files for target cmsys_c
cmsys_c_OBJECTS = \
"CMakeFiles/cmsys_c.dir/ProcessUNIX.o" \
"CMakeFiles/cmsys_c.dir/Base64.o" \
"CMakeFiles/cmsys_c.dir/MD5.o" \
"CMakeFiles/cmsys_c.dir/Terminal.o" \
"CMakeFiles/cmsys_c.dir/System.o" \
"CMakeFiles/cmsys_c.dir/String.o"

# External object files for target cmsys_c
cmsys_c_EXTERNAL_OBJECTS =

Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o
Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o
Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o
Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o
Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/System.o
Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/String.o
Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/build.make
Source/kwsys/libcmsys_c.a: Source/kwsys/CMakeFiles/cmsys_c.dir/link.txt
	@echo "Linking C static library libcmsys_c.a"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && $(CMAKE_COMMAND) -P CMakeFiles/cmsys_c.dir/cmake_clean_target.cmake
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/cmsys_c.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Source/kwsys/CMakeFiles/cmsys_c.dir/build: Source/kwsys/libcmsys_c.a
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/build

Source/kwsys/CMakeFiles/cmsys_c.dir/requires: Source/kwsys/CMakeFiles/cmsys_c.dir/ProcessUNIX.o.requires
Source/kwsys/CMakeFiles/cmsys_c.dir/requires: Source/kwsys/CMakeFiles/cmsys_c.dir/Base64.o.requires
Source/kwsys/CMakeFiles/cmsys_c.dir/requires: Source/kwsys/CMakeFiles/cmsys_c.dir/MD5.o.requires
Source/kwsys/CMakeFiles/cmsys_c.dir/requires: Source/kwsys/CMakeFiles/cmsys_c.dir/Terminal.o.requires
Source/kwsys/CMakeFiles/cmsys_c.dir/requires: Source/kwsys/CMakeFiles/cmsys_c.dir/System.o.requires
Source/kwsys/CMakeFiles/cmsys_c.dir/requires: Source/kwsys/CMakeFiles/cmsys_c.dir/String.o.requires
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/requires

Source/kwsys/CMakeFiles/cmsys_c.dir/clean:
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys && $(CMAKE_COMMAND) -P CMakeFiles/cmsys_c.dir/cmake_clean.cmake
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/clean

Source/kwsys/CMakeFiles/cmsys_c.dir/depend:
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/kwsys /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/kwsys/CMakeFiles/cmsys_c.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Source/kwsys/CMakeFiles/cmsys_c.dir/depend

