@echo off
REM %1 = C wrapper base name
REM %2 = output directory for compilation files

REM @copy Wrapper.hs+,, Wrapper.hs > NUL
if "%1"=="" goto NOPARAM

if "%2"=="" (
    set WrapperOutputDir=.\
) else (
    set WrapperOutputDir=%2\
)

if not exist %WrapperOutputDir% mkdir %WrapperOutputDir%

set WrapperHS=src\%1.hs
set WrapperHSO=%WrapperOutputDir%%1-hs.o
set WrapperHSHI=%WrapperOutputDir%%1-hs.hi
if exist %WrapperHSO% del %WrapperHSO%
echo module %1 where > %WrapperHS%
ghc -c %WrapperHS% -o %WrapperHSO% -ohi %WrapperHSHI%
if %errorlevel% neq 0 goto END

set WrapperC=cbits\%1.c
set WrapperCO=%WrapperOutputDir%%1-c.o
if exist %WrapperCO% del %WrapperCO%
gcc -c %WrapperC% -I include -o %WrapperCO%
if %errorlevel% neq 0 goto END

set WrapperO=%WrapperOutputDir%%1.o
ld -r %WrapperCO% %WrapperHSO% -o %WrapperO%
goto END

:NOPARAM
echo Missing C wrapper name as parameter

:END
set WrapperHS=
set WrapperHSO=
set WrapperC=
set WrapperCO=
set WrapperO=
set WrapperOuputDir=
