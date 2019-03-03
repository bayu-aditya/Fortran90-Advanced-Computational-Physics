program membuat_matriks
!-----------------------------------------------------------------------
!       Author : Bayu Aditya                                           |
!       Copyright (c) 2019                                             |
!-----------------------------------------------------------------------
    use matriks_module
    implicit none
    real, dimension(10,5) :: matriks
    real, dimension(10,5) :: matriks_new
    integer :: i, j
    integer ::status

    ! membuat elemen matriks
    do j = 1, 5
        do i = 1, 10
            matriks(i,j) = i + j
        end do
    end do

    call print_matriks(matriks)

    ! menuliskan matriks ke dalam file 'matriks.dat'
    open(unit = 3, file = 'matriks.dat', status = 'old', action = 'write', &
         iostat = status)
    do i = lbound(matriks, 1), ubound(matriks, 1)
        write(3,*) (matriks(i, j), j = lbound(matriks, 2), ubound(matriks, 2))
    end do
    close(unit = 3)

    write(*,*) " "
    
    ! load matriks dari file 'matriks.dat' ke dalam variable
    open(unit = 5, file = 'matriks.dat', status = 'old', action = 'read', &
         iostat = status)
    do i = lbound(matriks_new, 1), ubound(matriks_new, 1)
        read(5,*) (matriks_new(i, j), j = lbound(matriks_new, 2), ubound(matriks_new, 2))
    end do
    close(unit = 5)

    call print_matriks(matriks_new)

end program membuat_matriks