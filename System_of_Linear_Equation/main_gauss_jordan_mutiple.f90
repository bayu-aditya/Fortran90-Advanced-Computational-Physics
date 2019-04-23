program main_gauss_jordan
    use system_of_linear_equation
    
    implicit none
    integer, parameter :: DBL = 8
    real(8), allocatable, dimension(:,:) :: matriksA
    real(8), allocatable, dimension(:,:) :: matriksBB
    real(8), allocatable, dimension(:,:) :: hasil
    integer :: m, n

!========================= Proses Gauss Jordan ==========================
    write(*,*) "========================= Proses Gauss Jordan =========================="

    call import_matriks('matriksA.dat', matriksA, n)
    call import_matriks_nonpersegi('matriksBB.dat', matriksBB, m, n)
    allocate(hasil(m, n))

    ! Menampilkan matriks dan vektor hasil sebelum proses
    write(*,*) "Matriks A"
    call print_matriks(matriksA)
    write(*,*) "Matriks B"
    call print_matriks(matriksBB)
    write(*,*) "Hasil"
    hasil = 0.0_DBL
    call print_matriks(hasil)

    ! Proses gauss Jordan
    call gauss_jordan_multiple(matriksA, matriksBB, hasil)

    ! Menampilkan matriks dan vektor hasil setelah proses
    ! write(*,*) "Matriks A sesudah proses"
    ! call print_matriks(matriksA)
    ! write(*,*) "Matriks B sesudah proses"
    ! call print_matriks(matriksBB)
    write(*,*) "Hasil"
    call print_matriks(hasil)

    deallocate(matriksA)
    deallocate(matriksBB)
    deallocate(hasil)
end program main_gauss_jordan