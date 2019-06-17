program odeIVP
    use ode
    implicit none
    real(8), external :: fungsi
    real(8), external :: fungsi_orde2
    real(8) :: y0, u0

    ! kondisi awal saat y(0)
    y0 = 1.0_DBL
    ! solusi persamaan differential orde 1 kondisi awal
    ! dari 0 < x < 3 sebanyak 100 titik dengan nama file output "hasil.dat"
    call euler(fungsi, y0, 0.0_DBL, 1.0_DBL, 100, 'euler.dat')
    call euler_modified(fungsi, y0, 0.0_DBL, 1.0_DBL, 100, 'euler_mod.dat')
    call RK4(fungsi, y0, 0.0_DBL, 1.0_DBL, 100, 'RK4.dat')

    
    ! Kondisi awal saat y(0) dan y'(0) = u(0)
    y0 = 1.0_DBL
    u0 = 0.0_DBL
    ! solusi persamaan differential orde 2 kondisi awal
    ! dari 0 < x < 2 sebanyak 100 titik dengan nama file output "hasil.dat"
    ! call euler_modified2(fungsi, y0, u0, 0.0_DBL, 1.0_DBL, 1000, 'euler_orde2.dat')
    ! call RK4_orde2(fungsi, y0, u0, 0.0_DBL, 1.0_DBL, 1000, 'RK4_orde2.dat')
end program odeIVP

double precision function fungsi(x, y)
    ! Persamaan Diferensial Biasa Orde 1
    ! Kondisi awal (Initial Value Problem)
    implicit none
    integer, parameter :: DBL = 8
    real(8), intent(in) :: x, y

    ! fungsi = 0.5_DBL*(10.0_DBL*x - 8.0_DBL*x*y)
    fungsi = x**2.d0 - exp(y)*sin(x)
end function

! double precision function fungsi_orde2(x, y, u)
!     ! Persamaan Diferensial Biasa Orde 2
!     ! Kondisi awal (Initial Value Problem)
!     implicit none
!     integer, parameter :: DBL = 8 
!     real(8), intent(in) :: x, y, u

!     fungsi_orde2 = 5.0_DBL*x - 4.0_DBL*x*u - 5.0_DBL*y
! end function fungsi_orde2