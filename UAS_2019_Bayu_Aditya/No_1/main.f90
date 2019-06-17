program nomor_1
    use integral
    implicit none
    real(8), external :: fungsi
    real(8) :: hasil_trapezoid

    hasil_trapezoid = trapezoid(fungsi, 10.d0, 55.d0, 1000)
    write(*,'(A)') "========================= HASIL ========================="
    write(*,'(A, F20.15)') "Hasil dari Integral Trapezoid =", hasil_trapezoid
end program nomor_1

double precision function fungsi(x)
    use fitting_curve
    implicit none
    real(8), intent(in) :: x
    real(8), allocatable, dimension(:) :: dataX, dataY

    ! Import data eksperimen
    call import_data("Data/data_eksperimen.dat", dataX, dataY)

    ! Fitting dengan menggunakan Lagrange dasar
    fungsi = lagrange(x, dataX, dataY)
    call lagrange_plot(dataX, dataY, "Data/lagrange_plot.dat")

end function fungsi