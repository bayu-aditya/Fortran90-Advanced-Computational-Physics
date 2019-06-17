program main_eigen
    use eigen
    implicit none
    real(8), allocatable, dimension(:,:) :: matriks_square
    real(8), allocatable, dimension(:) :: eigenval
    integer :: n

    call import_matriks("Data/matriks.dat", matriks_square, n)
    allocate(eigenval(n))

    ! call power_method(matriks_square, eigenval)
    call sekular(matriks_square, eigenval)
    write(*,*) eigenval

end program main_eigen