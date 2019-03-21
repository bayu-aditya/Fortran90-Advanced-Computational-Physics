program main_least_square
    use fitting_curve
    implicit none

    real(KIND=8), allocatable, dimension(:) :: dataX
    real(KIND=8), allocatable, dimension(:) :: dataY

    call import_data('data.dat', dataX, dataY)

    call least_square(dataX, dataY)

    deallocate(dataX)
    deallocate(dataY)
end program main_least_square