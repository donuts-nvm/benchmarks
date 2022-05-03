@echo off
rem
rem port_progress.bat
rem
rem Copyright 2008 Standard Performance Evaluation Corporation
rem  All Rights Reserved
rem
rem $Id: port_progress.bat 6019 2008-04-07 19:27:17Z cloyce $
rem

setlocal

set list_files=

FOR %%D IN (%*) DO call :GET_LIST %%D

specutil port_progress %list_files%

:GET_LIST
   set list_files=%list_files% %1


