# INPUT NAMA FILE 
MAIN="main_program.f90"
MODUL1="Fitting.f90"
MODUL2="Nonlinear_Equation.f90"


OUT=${MAIN:0:-4}".out"

gfortran -o $OUT $MAIN $MODUL1 $MODUL2
./$OUT