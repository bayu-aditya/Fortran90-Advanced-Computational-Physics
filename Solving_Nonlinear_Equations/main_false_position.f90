program metode_false_position
    implicit none
    integer, parameter :: dpr = kind(1.0D0)

    real :: angka_single
    real(dpr) :: angka_double
    integer :: i

    angka_double = 0

    do i = 1,20
        angka_single = i**10
        angka_double = i**10_dpr
        write(*,*) i, angka_single, angka_double
    end do

end program metode_false_position