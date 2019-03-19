program main_gauss_jordan
    use system_of_linear_equation
    
    implicit none
    real, dimension(3,3) :: matriks

    call gauss_jordan(matriks)

end program main_gauss_jordan