program main_parabolic
    use pde
    implicit none
    integer, parameter :: DBL = 8
    ! real(8) :: fungsi, fun0, fx0t, fxnt
    type(parabolic) :: param
    ! real(8) :: gamma, x0, xn, t0, tm
    ! integer :: n, m

    param%gamma = 0.001_DBL
    param%f0    = 25.d0
    param%x0t   = 0.d0
    param%xnt   = 110.d0
    param%x0    = 0.0_DBL
    param%xn    = 1.d0
    param%t0    = 0.d0
    param%tm    = 15.d0
    param%n     = 100
    param%m     = 300
    param%name_output = 'hasil.dat'

    ! call pde_parabolic(fungsi, fun0, fx0t, fxnt, gamma, &
    ! x0, xn, t0, tm, n, m, 'hasil.dat')

    call pde_parabolic(fungsi, param)
    
    contains
    double precision function fungsi(x, t)
    implicit none
        real(8), intent(in) :: x, t
        fungsi = 0.0_DBL
    end function fungsi
end program main_parabolic