# NAMA FILE
MAIN="main.f90"
MODULE1="Integral.f90"
MODULE2="Fitting.f90"

OUT=${MAIN:0:-4}".out"

gfortran -o $OUT $MAIN $MODULE1 $MODULE2
./$OUT