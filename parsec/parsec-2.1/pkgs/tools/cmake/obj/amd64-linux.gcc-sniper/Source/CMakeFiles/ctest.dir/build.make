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
include Source/CMakeFiles/ctest.dir/depend.make

# Include the progress variables for this target.
include Source/CMakeFiles/ctest.dir/progress.make

# Include the compile flags for this target's objects.
include Source/CMakeFiles/ctest.dir/flags.make

Source/CMakeFiles/ctest.dir/ctest.o: Source/CMakeFiles/ctest.dir/flags.make
Source/CMakeFiles/ctest.dir/ctest.o: /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/ctest.cxx
	$(CMAKE_COMMAND) -E cmake_progress_report /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles $(CMAKE_PROGRESS_1)
	@echo "Building CXX object Source/CMakeFiles/ctest.dir/ctest.o"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source && /usr/bin/g++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/ctest.dir/ctest.o -c /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/ctest.cxx

Source/CMakeFiles/ctest.dir/ctest.i: cmake_force
	@echo "Preprocessing CXX source to CMakeFiles/ctest.dir/ctest.i"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source && /usr/bin/g++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/ctest.cxx > CMakeFiles/ctest.dir/ctest.i

Source/CMakeFiles/ctest.dir/ctest.s: cmake_force
	@echo "Compiling CXX source to assembly CMakeFiles/ctest.dir/ctest.s"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source && /usr/bin/g++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source/ctest.cxx -o CMakeFiles/ctest.dir/ctest.s

Source/CMakeFiles/ctest.dir/ctest.o.requires:
.PHONY : Source/CMakeFiles/ctest.dir/ctest.o.requires

Source/CMakeFiles/ctest.dir/ctest.o.provides: Source/CMakeFiles/ctest.dir/ctest.o.requires
	$(MAKE) -f Source/CMakeFiles/ctest.dir/build.make Source/CMakeFiles/ctest.dir/ctest.o.provides.build
.PHONY : Source/CMakeFiles/ctest.dir/ctest.o.provides

Source/CMakeFiles/ctest.dir/ctest.o.provides.build: Source/CMakeFiles/ctest.dir/ctest.o
.PHONY : Source/CMakeFiles/ctest.dir/ctest.o.provides.build

# Object files for target ctest
ctest_OBJECTS = \
"CMakeFiles/ctest.dir/ctest.o"

# External object files for target ctest
ctest_EXTERNAL_OBJECTS =

bin/ctest: Source/CMakeFiles/ctest.dir/ctest.o
bin/ctest: Source/libCTestLib.a
bin/ctest: Source/libCMakeLib.a
bin/ctest: Utilities/cmxmlrpc/libcmXMLRPC.a
bin/ctest: Utilities/cmcurl/libcmcurl.a
bin/ctest: Source/kwsys/libcmsys.a
bin/ctest: Utilities/cmexpat/libcmexpat.a
bin/ctest: Utilities/cmzlib/libcmzlib.a
bin/ctest: Utilities/cmtar/libcmtar.a
bin/ctest: Utilities/cmcompress/libcmcompress.a
bin/ctest: Source/CMakeFiles/ctest.dir/build.make
bin/ctest: Source/CMakeFiles/ctest.dir/link.txt
	@echo "Linking CXX executable ../bin/ctest"
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/ctest.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Source/CMakeFiles/ctest.dir/build: bin/ctest
.PHONY : Source/CMakeFiles/ctest.dir/build

Source/CMakeFiles/ctest.dir/requires: Source/CMakeFiles/ctest.dir/ctest.o.requires
.PHONY : Source/CMakeFiles/ctest.dir/requires

Source/CMakeFiles/ctest.dir/clean:
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source && $(CMAKE_COMMAND) -P CMakeFiles/ctest.dir/cmake_clean.cmake
.PHONY : Source/CMakeFiles/ctest.dir/clean

Source/CMakeFiles/ctest.dir/depend:
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Source /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/CMakeFiles/ctest.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Source/CMakeFiles/ctest.dir/depend

