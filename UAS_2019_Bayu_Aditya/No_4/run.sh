# NAMA FILE
MAIN="main.f90"
MODULE1="eigen.f90"
MODULE2="Nonlinear_Equation.f90"

OUT=${MAIN:0:-4}".out"

gfortran -o $OUT $MAIN $MODULE1 $MODULE2
./$OUT