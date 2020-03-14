:: set VCVARSALL=h:\vs2017\VC\Auxiliary\Build\vcvarsall.bat
:: set CYG_BIN=h:\cygwin\bin

echo get VS build environment
set VCVARS_PLATFORM=x64
call %VCVARSALL% %VCVARS_PLATFORM%

pushd %~dp0
%CYG_BIN%\bash prepare_libffi.sh

if errorlevel 1 exit /B %ERRORLEVEL%
exit /b
