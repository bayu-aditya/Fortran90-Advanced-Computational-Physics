program main_LU_decomposition
    use system_of_linear_equation
    
    implicit none
    real, allocatable, dimension(:,:) :: matriksA
    real, allocatable, dimension(:) :: matriksB
    real, allocatable, dimension(:) :: hasil
    real, allocatable, dimension(:,:) :: matriksL, matriksU
    integer :: n

    ! Import matriks A dari file 'matriksA.dat'
    call import_matriks('matriksA.dat', matriksA, n)
    call import_vektor('matriksB.dat', matriksB)
    allocate(matriksL(n,n))
    allocate(matriksU(n,n))
    allocate(hasil(n))
    matriksU = 0
    matriksL = 0
    hasil = 0
    
    write(*,*) "Matriks A"
    call print_matriks(matriksA)
    write(*,*) "Matriks B"
    call print_vektor(matriksB)
    write(*,*) "Matriks L"
    call print_matriks(matriksL)
    write(*,*) "Matriks U"
    call print_matriks(matriksU)

    ! Proses LU Decomposition
    call LU_decomposition(matriksA, matriksL, matriksU)
    call substitusi_LU_decomp(matriksL, matriksU, matriksB, hasil)
    
    write(*,*) "Matriks A setelah LU decomposition"
    call print_matriks(matriksA)
    write(*,*) "Matriks B setelah LU decomposition"
    call print_vektor(matriksB)
    write(*,*) "Matriks L setelah LU decomposition"
    call print_matriks(matriksL)
    write(*,*) "Matriks U setelah LU decomposition"
    call print_matriks(matriksU)
    write(*,*) "Hasil LU decomposition"
    call print_vektor(hasil)
    
    deallocate(matriksA)
    deallocate(matriksB)
    deallocate(matriksL)
    deallocate(matriksU)
end program main_LU_decomposition