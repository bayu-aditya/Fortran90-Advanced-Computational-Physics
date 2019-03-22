program main_least_square
    use fitting_curve
    implicit none
    integer, parameter :: DBL = 8
    real(KIND=8), allocatable, dimension(:) :: dataX
    real(KIND=8), allocatable, dimension(:) :: dataY
    real(KIND=8) :: x, hasil

    call import_data('data.dat', dataX, dataY)

    ! Untuk melihat hasil dari interpolasi lagrange di suatu titik X
    x = 30_DBL
    hasil = lagrange(X, dataX, dataY)

    ! Untuk membuat plot dari interpolasi lagrange
    call lagrange_plot(dataX, dataY, "Lagrange_plot.dat")

    deallocate(dataX)
    deallocate(dataY)
end program main_least_square