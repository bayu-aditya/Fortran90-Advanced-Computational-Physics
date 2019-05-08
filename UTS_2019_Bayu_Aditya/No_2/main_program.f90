program nomor_2
    use fitting_curve
    implicit none
    real(8), allocatable, dimension(:) :: dataX, polarisasi

    ! Import data eksperimen dari files
    call import_data("data_eksperimen.dat", dataX, polarisasi)

    ! Fitting data eksperimen dengan menggunakan Eliminsi Gauss Jordan (Least Square)
    call least_square_plot(dataX, polarisasi, 3, "Hasil_Gauss_orde3.dat")
    call least_square_plot(dataX, polarisasi, 7, "Hasil_Gauss_orde7.dat")
    call least_square_plot(dataX, polarisasi, 11, "Hasil_Gauss_orde11.dat")
    call least_square_plot(dataX, polarisasi, 15, "Hasil_Gauss_orde15.dat")
end program nomor_2