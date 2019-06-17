program odeIVP
    use ode
    implicit none
    real(8), external :: fungsi_orde2_1
    real(8), external :: fungsi_orde2_2
    real(8) :: y0, u0

    ! ------------------------------- l = 1 --------------------------------------
    ! Kondisi awal
    y0 = 0.000003213_DBL
    u0 = 0.002_DBL
    ! solusi persamaan differential orde 2 kondisi awal
    !   dari 0.001 < x < 10.0 sebanyak 10000 titik
    call euler_modified2(fungsi_orde2_1, y0, u0, 0.001_DBL, 11.0_DBL, 10000, 'Data/euler_orde2_1.dat')

    ! ------------------------------- l = 2 --------------------------------------
    ! Kondisi awal
    y0 = 0.000000001995_DBL
    u0 = 0.000001928_DBL
    ! solusi persamaan differential orde 2 kondisi awal
    !   dari 0.001 < x < 10.0 sebanyak 10000 titik
    call euler_modified2(fungsi_orde2_2, y0, u0, 0.001_DBL, 11.0_DBL, 10000, 'Data/euler_orde2_2.dat')
end program odeIVP

double precision function fungsi_orde2_1(x, y, u)
    ! Persamaan Diferensial Biasa Orde 2
    ! Kondisi awal (Initial Value Problem)
    implicit none
    integer, parameter :: DBL = 8 
    real(8), intent(in) :: x, y, u
    
    real(8), parameter :: V0 = 600.d0
    real(8), parameter :: lambda = 1.5d0
    real(8), parameter :: E = 200.d0
    real(8), parameter :: m = 938.27d0
    real(8), parameter :: hbar = 197.33d0
    real(8), parameter :: l = 1.d0

    fungsi_orde2_1 = -2.d0*(u/x) - (2.d0*m*E/hbar**2.d0)*y &
                    + (2.d0*m*V0/hbar**2.d0)*(exp(-lambda*x)*y/x) &
                    + l*(l+1.d0)*(y/x**2.d0)
end function fungsi_orde2_1

double precision function fungsi_orde2_2(x, y, u)
    ! Persamaan Diferensial Biasa Orde 2
    ! Kondisi awal (Initial Value Problem)
    implicit none
    integer, parameter :: DBL = 8 
    real(8), intent(in) :: x, y, u
    
    real(8), parameter :: V0 = 600.d0
    real(8), parameter :: lambda = 1.5d0
    real(8), parameter :: E = 200.d0
    real(8), parameter :: m = 938.27d0
    real(8), parameter :: hbar = 197.33d0
    real(8), parameter :: l = 2.d0

    fungsi_orde2_2 = -2.d0*(u/x) - (2.d0*m*E/hbar**2.d0)*y &
                    + (2.d0*m*V0/hbar**2.d0)*(exp(-lambda*x)*y/x) &
                    + l*(l+1.d0)*(y/x**2.d0)
end function fungsi_orde2_2