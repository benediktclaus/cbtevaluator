Rscript "~config"\evaluate-all-patients.R
if exist "~config"\protocol.txt del "~config"\protocol.txt
ren "~config"\evaluate-all-patients.Rout protocol.txt
del .Rdata
del Rplots.pdf

pause