program main_integral
    use integral
    implicit none
    real(8), external :: fungsi
    real(8) :: hasil_trap, hasil_13, hasil_38

    hasil_trap = trapezoid(fungsi, 0.0_DBL, 2.0_DBL, 33)
    write(*,*) hasil_trap
    hasil_13 = simpson13(fungsi, 0.0_DBL, 2.0_DBL, 11)
    write(*,*) hasil_13
    hasil_38 = simpson38(fungsi, 0.0_DBL, 2.0_DBL, 11)
    write(*,*) hasil_38
end program main_integral

double precision function fungsi(x)
    real(8), intent(in) :: x
    integer, parameter :: DBL = 8
    fungsi = exp(x/2_DBL)*sin(x)
end function