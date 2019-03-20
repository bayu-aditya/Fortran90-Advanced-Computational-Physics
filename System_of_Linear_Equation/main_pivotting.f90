program matriks_pivoting
    use system_of_linear_equation
    
    implicit none
    real, allocatable, dimension(:,:) :: matriksA
    real, allocatable, dimension(:) :: matriksB
    !real, allocatable, dimension(:) :: hasil
    integer :: n

    call import_matriks('matriksA.dat', matriksA, n)
    call import_vektor('matriksB.dat', matriksB)
    !allocate(hasil(n))

    ! Menampilkan matriks dan vektor hasil sebelum proses
    write(*,*) "Matriks A"
    call print_matriks(matriksA)
    write(*,*) "Matriks B"
    call print_vektor(matriksB)
    !write(*,*) "Hasil"
    !hasil = 0
    !call print_vektor(hasil)

    ! Proses gauss Jordan
    call pivot(matriksA, matriksB, int(1))

    ! Menampilkan matriks dan vektor hasil setelah proses
    write(*,*) "Matriks A sesudah proses"
    call print_matriks(matriksA)
    write(*,*) "Matriks B sesudah proses"
    call print_vektor(matriksB)
    !write(*,*) "Hasil"
    !call print_vektor(hasil)

    deallocate(matriksA)
    deallocate(matriksB)
    !deallocate(hasil)
end program matriks_pivoting