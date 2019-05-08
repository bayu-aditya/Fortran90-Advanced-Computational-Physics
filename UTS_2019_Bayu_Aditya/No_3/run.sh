# INPUT NAMA FILE 
MAIN="main_program.f90"
MODUL1="Fitting.f90"


OUT=${MAIN:0:-4}".out"

gfortran -o $OUT $MAIN $MODUL1
./$OUT