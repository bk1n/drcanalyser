@echo off
setlocal

:: Get the hostname
set HOSTNAME=%COMPUTERNAME%

echo Device name is %HOSTNAME%

set SCRIPT_DIR=%~dp0
cd /d %SCRIPT_DIR%

echo Working directory %cd%

:: Check the hostname and execute commands accordingly
if "%HOSTNAME%"=="BKPC" (
    echo Identified Home PC ^(BKPC^)
    mklink /D "data" "F:\Documents\OneDrive - University of Edinburgh\projects\PhD\viability_assays\data" 
    mklink /D "figures" "F:\Documents\OneDrive - University of Edinburgh\projects\PhD\viability_assays\figures" 
) else if "%HOSTNAME%"=="MVM-IGC-D0060" (
    echo Identified Work PC ^(MVM-IGC-D0060^)
    mklink /D "data" "C:\Users\s1754085\OneDrive - University of Edinburgh\projects\PhD\viability_assays\data" 
    mklink /D "figures" "C:\Users\s1754085\OneDrive - University of Edinburgh\projects\PhD\viability_assays\figures" 
) else if "%HOSTNAME%"=="BKLT" (
    echo Identified Laptop ^(BKLT^)
    mklink /D "data" "C:\Users\bgkin\OneDrive - University of Edinburgh\projects\PhD\viability_assays\data" 
    mklink /D "figures" "C:\Users\bgkin\OneDrive - University of Edinburgh\projects\PhD\viability_assays\figures" 
) else (
    echo Device name not matched
    pause
)

timeout /t 10

endlocal