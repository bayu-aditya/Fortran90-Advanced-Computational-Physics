program main_least_square
    use fitting_curve
    implicit none
    integer, parameter :: DBL = 8
    real(KIND=DBL), allocatable, dimension(:) :: dataX
    real(KIND=DBL), allocatable, dimension(:) :: dataY
    real(KIND=DBL) :: x, hasil

    call import_data('data.dat', dataX, dataY)

    ! Untuk melihat hasil dari least square di suatu titik X
    x = 30_DBL
    hasil = least_square(x, dataX, dataY, 9)
    write(*,*) " Hasil ", hasil

    ! Untuk membuat plot dari interpolasi lagrange
    !call least_square_plot(dataX, dataY, 9, 'Hasil_orde_9.dat')

    deallocate(dataX)
    deallocate(dataY)
end program main_least_square