R CMD BATCH "~config"\evaluate-all-patients.R
ren "~config"\evaluate-all-patients.Rout protocol.txt
del .Rdata
del Rplots.pdf

pause