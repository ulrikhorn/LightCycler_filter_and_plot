@echo off
REM Launch Shiny app from R portable
SET R_HOME=.\R-Portable
.\R-Portable\bin\Rscript.exe -e "shiny::runApp('app', launch.browser=TRUE)"
pause
