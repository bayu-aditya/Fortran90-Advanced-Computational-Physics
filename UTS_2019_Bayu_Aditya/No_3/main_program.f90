program nomor_3
    use fitting_curve
    implicit none
    real(8), allocatable, dimension(:) :: dataX, polarisasi

    ! Import data eksperimen dari files
    call import_data("data_eksperimen.dat", dataX, polarisasi)

    ! Interpolasi data eksperimen dengan menggunakan Hermite Kubik tanpa data turunan
    call hermite_kubik_plot(dataX, polarisasi, "Hasil_Interpolasi_Hermite_Kubik.dat")

end program nomor_3