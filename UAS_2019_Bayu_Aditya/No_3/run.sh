# NAMA FILE
MAIN="main.f90"
MODULE="Partial_Differential_Equation.f90"

OUT=${MAIN:0:-4}".out"

gfortran -o $OUT $MAIN $MODULE
./$OUT