@echo off
if exist "Settings.GIMS" (
exit
) else (
type NUL > Settings.GIMS
)