program main_eigen
    use eigen
    implicit none
    real(8), allocatable, dimension(:,:) :: matriks_square
    real(8), allocatable, dimension(:) :: eigenval
    integer :: i
    integer :: n

    call import_matriks("Data/matriks.dat", matriks_square, n)
    allocate(eigenval(n))

    ! call power_method(matriks_square, eigenval)
    call sekular(matriks_square, eigenval)
    
    ! Menampilkan Hasil dari Nilai EIgenvalues
    write(*,'(A)') "========================== HASIL =========================="
    write(*,'(A)') "Matriks yang di proses"
    call print_matriks(matriks_square)
    write(*,'(A)') "Didapat nilai dari Eigenvalues Sebesar"
    do i = 1, n
        write(*,'(A, I1, F20.15)') "Eigenvalue ke-", i, eigenval(i)
    end do

end program main_eigen