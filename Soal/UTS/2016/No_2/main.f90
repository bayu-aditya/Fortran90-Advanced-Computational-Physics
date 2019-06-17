program nomer_2
    use system_of_linear_equation
    implicit none
    real(8), allocatable, dimension(:,:) :: matriksA
    real(8), allocatable, dimension(:) :: matriksB, hasil
    integer :: n

    call import_matriks("matriksA.dat", matriksA, n)
    call print_matriks(matriksA)
    allocate(hasil(n))

    call import_vektor("matriksB.dat", matriksB)
    call print_vektor(matriksB)

    call gauss_jordan(matriksA, matriksB, hasil)
    call print_vektor(hasil)

end program nomer_2