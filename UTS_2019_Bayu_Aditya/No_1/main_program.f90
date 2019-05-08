program nomor_1
    use nonlinear_equation
    implicit none
    integer, parameter :: DBL = 8
    real(8), external :: fungsi
    real(8) :: akar_fungsi

    ! Mencari akar fungsi dengan menggunakan metode Secant
    ! Fungsi yang digunakan merupakan interpolasi lagrange dasar dengan hanya
    !   menggunakan 3 data dari kiri (untuk menghindari OVERFITTING seperti pada
    !   gambar yang telah dilampirkan)
    call secant(fungsi, -0.8_DBL, 0.7_DBL, akar_fungsi)

    ! Menampilkan nilai x saat P bernilai 0
    write(*,*) 
    write(*,'(A, F20.16)') "Didapat bahwa P bernilai nol saat x = ", akar_fungsi
end program nomor_1

double precision function fungsi(x)
    use fitting_curve
    implicit none
    real(8), intent(in) :: x
    real(8), allocatable, dimension(:) :: dataX, polarisasi

    ! Import data eksperimen
    call import_data("data_eksperimen.dat", dataX, polarisasi)

    ! Fitting dengan menggunakan Lagrange dasar
    fungsi = lagrange(x, dataX(:3), polarisasi(:3))
    call lagrange_plot(dataX(:3), polarisasi(:3), "lagrange_plot_3_data.dat")
    call lagrange_plot(dataX, polarisasi, "lagrange_plot_full_data.dat")

end function fungsi