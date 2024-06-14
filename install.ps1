@echo off
setlocal

REM Get the directory of the script
set SCRIPT_DIR=%~dp0

REM Set relative paths
set SOURCE_NVIM_DIR=%SCRIPT_DIR%nvim
set TARGET_NVIM_DIR=%LocalAppData%\nvim

set SOURCE_SETTINGS_JSON=%SCRIPT_DIR%settings.json
set TARGET_SETTINGS_JSON=%LocalAppData%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json

REM Check and remove existing nvim directory
if exist "%TARGET_NVIM_DIR%" (
    echo Removing existing nvim directory...
    rmdir /S /Q "%TARGET_NVIM_DIR%"
    )

REM Copy nvim directory
echo Copying nvim directory...
xcopy "%SOURCE_NVIM_DIR%" "%TARGET_NVIM_DIR%" /E /I /Q /Y

REM Check and remove existing settings.json file
if exist "%TARGET_SETTINGS_JSON%" (
    echo Removing existing settings.json file...
    del /Q "%TARGET_SETTINGS_JSON%"
    )

REM Copy settings.json file
echo Copying settings.json file...
copy "%SOURCE_SETTINGS_JSON%" "%TARGET_SETTINGS_JSON%" /Y

echo Files copied successfully.
pause
endlocal

