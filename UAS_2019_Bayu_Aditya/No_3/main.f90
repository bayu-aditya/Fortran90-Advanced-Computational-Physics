program main_hyperbolic
    use pde
    implicit none
    integer, parameter :: DBL = 8
    type(hyperbolic) :: param
    real(8) :: T, mu

    T = 10.0_DBL            ! Newton
    mu = 0.001_DBL          ! kg/m

    param%x0    = 0.0_DBL
    param%xn    = 1.0_DBL
    param%b     = 0.0_DBL
    param%v     = sqrt(T/mu)
    param%name  = "Data/Hasil_hyperbolic.dat"
    
    call pde_hyperbolic(fungsi, param)
    
    contains
    double precision function fungsi(x)
        ! Fungsi saat 
        ! psi(x, t=0)
        implicit none
        integer, parameter :: DBL = 8
        real(8), parameter :: pi = 3.14159265358979
        real(8), intent(in) :: x

    fungsi = sin(0.01d0*pi*x*100.d0)         ! X dan Y dalam Meter
    end function fungsi
end program main_hyperbolic