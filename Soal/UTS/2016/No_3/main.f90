program nomer_3
    use nonlinear_equation
    implicit none
    integer, parameter :: DBL = 8
    real(8), external :: fungsi
    real(8) :: hasil

    ! Fitting data dengan menggunakan Interpolasi Hermite Kubik
    ! Memasukkan fitting sebagai fungsi ke dalam bisection
    call bisection(fungsi, 112.0_DBL, 120.0_DBL, hasil)
    write(*,*) "Didapat Hasilnya adalah : ", Hasil
    write(*,*)

    ! Memasukkan fitting sebagai fungsi ke dalam false_position
    call false_position(fungsi, 112.0_DBL, 120.0_DBL, hasil)
    write(*,*) "Didapat Hasilnya adalah : ", Hasil
    write(*,*)

    ! Memasukkan fitting sebagai fungsi ke dalam secant
    call secant(fungsi, 112.0_DBL, 120.0_DBL, hasil)
    write(*,*) "Didapat Hasilnya adalah : ", Hasil
    write(*,*)
end program nomer_3

double precision function fungsi(x)
    use fitting_curve
    implicit none
    real(8), intent(in) :: x
    real(8), allocatable, dimension(:) :: theta, observable

    ! Import data dari files
    call import_data("data.dat", theta, observable)
    fungsi = hermite_kubik(x, theta, observable)
end function fungsi