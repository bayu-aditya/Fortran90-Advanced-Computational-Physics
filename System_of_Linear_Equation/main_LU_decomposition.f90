program main_LU_decomposition
    use system_of_linear_equation
    
    implicit none
    real, allocatable, dimension(:,:) :: matriksA
    real, allocatable, dimension(:,:) :: matriksL, matriksU
    integer :: n

    ! Import matriks A dari file 'matriksA.dat'
    call import_matriks('matriksA.dat', matriksA, n)
    allocate(matriksL(n,n))
    allocate(matriksU(n,n))
    matriksU = 0
    matriksL = 0
    
    write(*,*) "Matriks A"
    call print_matriks(matriksA)
    write(*,*) "Matriks L"
    call print_matriks(matriksL)
    write(*,*) "Matriks U"
    call print_matriks(matriksU)

    ! Proses LU Decomposition
    call LU_decomposition(matriksA, matriksL, matriksU)
    
    write(*,*) "Matriks A setelah LU decomposition"
    call print_matriks(matriksA)
    write(*,*) "Matriks L setelah LU decomposition"
    call print_matriks(matriksL)
    write(*,*) "Matriks U setelah LU decomposition"
    call print_matriks(matriksU)
    
end program main_LU_decomposition