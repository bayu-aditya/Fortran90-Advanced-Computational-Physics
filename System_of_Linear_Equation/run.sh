# NAMA FILE
MAIN="main_LU_decomposition.f90"
MODULE="System_of_Linear_Equation.f90"

OUT=${MAIN:0:-4}".out"

gfortran -o $OUT $MAIN $MODULE
./$OUT