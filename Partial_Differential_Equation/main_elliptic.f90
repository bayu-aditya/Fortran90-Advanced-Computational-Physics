program main_elliptic
    use pde
    implicit none
    type(elliptic) :: param

    param%x0 = 0.d0
    param%xn = 2.d0
    param%y0 = 0.d0
    param%ym = 1.d0
    param%h = 0.1d0
    param%epsilon = 0.01d0
    param%name = "Data/Hasil_Elliptic.dat"

    call pde_elliptic(func, fun_x0, fun_xn, fun_y0, fun_ym, param)

    contains
    double precision function func(x, y)
        implicit none
        real(8), intent(in) :: x, y
        func = (x**2.d0 + y**2.d0)*exp(x*y)
    end function func
    double precision function fun_x0(y)
        implicit none
        real(8), intent(in) :: y
        fun_x0 = 1.d0
    end function fun_x0
    double precision function fun_xn(y)
        implicit none
        real(8), intent(in) :: y
        fun_xn = exp(2.d0*y)
    end function fun_xn
    double precision function fun_y0(x)
        implicit none
        real(8), intent(in) :: x
        fun_y0 = 1.d0
    end function fun_y0
    double precision function fun_ym(x)
        implicit none
        real(8), intent(in) :: x
        fun_ym = exp(x)
    end function fun_ym
end program main_elliptic