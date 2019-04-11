program main_lagrange_kubik
    use fitting_curve
    implicit none   
    real(KIND=8), allocatable, dimension(:) :: dataX, dataY

    call import_data('data.dat', dataX, dataY)

    call hermite_kubik_plot(dataX, dataY, 'hermite_kubik.dat')
    call lagrange_kubik_plot(dataX, dataY, 'lagrange_kubik.dat')
end program main_lagrange_kubik