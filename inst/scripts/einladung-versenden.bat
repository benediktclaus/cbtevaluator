R CMD BATCH "03 Einladung"\einladung-script.R
if exist "03 Einladung"\protocol.txt del "03 Einladung"\protocol.txt
ren "03 Einladung"\einladung-script.Rout protocol.txt
del .Rdata

pause