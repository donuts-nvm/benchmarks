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

# Utility rule file for Nightly.

CMakeFiles/Nightly:
	/usr/bin/ctest -D Nightly

Nightly: CMakeFiles/Nightly
Nightly: CMakeFiles/Nightly.dir/build.make
.PHONY : Nightly

# Rule to build all files generated by this target.
CMakeFiles/Nightly.dir/build: Nightly
.PHONY : CMakeFiles/Nightly.dir/build

CMakeFiles/Nightly.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Nightly.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Nightly.dir/clean

CMakeFiles/Nightly.dir/depend:
	cd /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/CMakeFiles/Nightly.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Nightly.dir/depend

