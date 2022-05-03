# This file is configured at cmake time, and loaded at cpack time.
# To pass variables to cpack from cmake, they must be configured
# in this file.  

if(CPACK_GENERATOR MATCHES "NSIS")
  # set the install/unistall icon used for the installer itself
  # There is a bug in NSI that does not handle full unix paths properly. 
  SET(CPACK_NSIS_MUI_ICON "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Utilities/Release\\CMakeLogo.ico")
  SET(CPACK_NSIS_MUI_UNIICON "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Utilities/Release\\CMakeLogo.ico")
  # set the package header icon for MUI
  SET(CPACK_PACKAGE_ICON "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/src/Utilities/Release\\CMakeInstall.bmp")
  # tell cpack to create links to the doc files
  SET(CPACK_NSIS_MENU_LINKS
    "doc/cmake-2.6/CMakeSetup.html" "CMakeSetup Help"
    "doc/cmake-2.6/cmake.html" "CMake Help"
    "doc/cmake-2.6/cmake-properties.html"
    "CMake Properties and Variables Help"
    "doc/cmake-2.6/ctest.html" "CTest Help"
    "doc/cmake-2.6/cmake-modules.html" "CMake Modules Help"
    "doc/cmake-2.6/cmake-commands.html" "CMake Commands Help"
    "doc/cmake-2.6/cpack.html" "CPack Help"
    "http://www.cmake.org" "CMake Web Site"
    )
  # tell cpack the executables you want in the start menu as links
  SET(CPACK_PACKAGE_EXECUTABLES "CMakeSetup" "CMake" )
  # tell cpack to create a desktop link to CMakeSetup
  SET(CPACK_CREATE_DESKTOP_LINKS "CMakeSetup")
  SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\CMakeSetup.exe")
  SET(CPACK_NSIS_DISPLAY_NAME "CMake 2.6 a cross-platform, open-source build system")
  SET(CPACK_NSIS_HELP_LINK "http:\\\\www.cmake.org")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\www.kitware.com")
  SET(CPACK_NSIS_CONTACT )
  SET(CPACK_NSIS_MODIFY_PATH ON)
endif(CPACK_GENERATOR MATCHES "NSIS")
# include the cpack options for qt dialog if they exisit
# they might not if qt was not enabled for the build
INCLUDE("/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/pkgs/tools/cmake/obj/amd64-linux.gcc-sniper/Source/QtDialog/QtDialogCPack.cmake" OPTIONAL)

if(CPACK_GENERATOR MATCHES "CygwinSource")
  # when packaging source make sure the .build directory is not included
    SET(CPACK_SOURCE_IGNORE_FILES
      "/CVS/" "/\\.build/" "/\\.svn/" "\\.swp$" "\\.#" "/#" "~$")
endif(CPACK_GENERATOR MATCHES "CygwinSource")

if("${CPACK_GENERATOR}" STREQUAL "PackageMaker")
  if(CMAKE_PACKAGE_QTGUI)                       
    set(CPACK_PACKAGE_DEFAULT_LOCATION "/Applications")
  else(CMAKE_PACKAGE_QTGUI)                     
    set(CPACK_PACKAGE_DEFAULT_LOCATION "/usr")
  endif(CMAKE_PACKAGE_QTGUI)                    
endif("${CPACK_GENERATOR}" STREQUAL "PackageMaker")
