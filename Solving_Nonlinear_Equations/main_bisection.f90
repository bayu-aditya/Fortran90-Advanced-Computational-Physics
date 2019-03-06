! Author : Bayu Aditya
! Copyright (c) 2019

program test_bisection
    use nonlinear_equation

    implicit none
    real :: a, b, hasil
    real, external :: func

    a = -1.0
    b = 0
    call bisection(func, a, b, hasil)
    write (*,*) "Hasil akar penyelesaian dari bisection : "
    write (*,10) hasil
    10 format (F10.7)
end program test_bisection



real function func(x)
    implicit none
    real, intent(in) :: x
    !func = sin(x)
    func = 8.0 - 4.5*(x - sin(x))
end function func