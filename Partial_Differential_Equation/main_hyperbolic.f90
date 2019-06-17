program main_hyperbolic
    use pde
    implicit none
    integer, parameter :: DBL = 8
    type(hyperbolic) :: param
    real(8) :: T, mu

    T = 10.0_DBL            ! Newton
    mu = 0.001_DBL          ! kg

    param%x0    = 0.0_DBL
    param%xn    = 100.0_DBL
    param%b     = 0.0_DBL
    param%v     = sqrt(T/mu)
    param%name  = "Data/Hasil_hyperbolic.dat"

    write(*,*) sqrt(T/mu)
    
    call pde_hyperbolic(fungsi, param)
    
    contains
    double precision function fungsi(x)
        ! Fungsi saat 
        ! psi(x, t=0)
        implicit none
        integer, parameter :: DBL = 8
        real(8), intent(in) :: x

        if ((x >=0.0_DBL) .and. (x <= 20.0_DBL)) then
            fungsi = 0.05_DBL*x
        else
            fungsi = 1.25_DBL - 0.0125_DBL*x
        end if
    end function fungsi
end program main_hyperbolic