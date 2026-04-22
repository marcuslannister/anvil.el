@echo off
REM anvil-hook.bat — Claude Code lifecycle hook wrapper (Windows)
REM
REM Usage:
REM   anvil-hook EVENT [ARG1 [ARG2 [ARG3 ...]]]
REM
REM Forwards to emacsclient -e so the running Emacs daemon
REM (with anvil-session loaded) performs the snapshot / event-log
REM work.  See scripts/anvil-hook (the POSIX sibling) for the
REM quoted-output handling note — on Windows we print the raw
REM elisp `prin1' form since Claude Code hooks on Windows already
REM pass through unquoted.

setlocal EnableDelayedExpansion

set EVENT=%~1
if "%EVENT%"=="" (
  echo anvil-hook: EVENT argument required 1>&2
  exit /b 2
)

shift
set ARGS=
:loop
if "%~1"=="" goto done
set ARG=%~1
set ARG=!ARG:\=\\!
set ARG=!ARG:"=\"!
set ARGS=!ARGS! "!ARG!"
shift
goto loop
:done

set EMACSCLIENT=emacsclient
if defined ANVIL_EMACSCLIENT set EMACSCLIENT=%ANVIL_EMACSCLIENT%

"%EMACSCLIENT%" -e "(anvil-session-hook-dispatch '%EVENT%%ARGS%)"

endlocal
