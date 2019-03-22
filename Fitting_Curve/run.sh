# INPUT NAMA FILE 
MAIN="main_lagrange.f90"
MODUL="Fitting.f90"


OUT=${MAIN:0:-4}".out"

gfortran -o $OUT $MAIN $MODUL
./$OUT