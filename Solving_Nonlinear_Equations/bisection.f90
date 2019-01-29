program test_bisection
    implicit none
    real :: a, b, hasil
    real, external :: func

    a = 1.0
    b = 4.0
    call bisection(func, a, b, hasil)
    write (*,*) hasil
end program test_bisection

real function func(x)
    implicit none
    real, intent(in) :: x
    func = 8.0 - 4.5*(x - sin(x))
end function func

subroutine bisection(func, a, b, x)
    implicit none
    real :: func
    real, intent(in) :: a, b
    real, intent(out) :: x
    real :: tolerance, tol_i, tol_old, a_temp, b_temp
    integer :: i

    a_temp = a
    b_temp = b
    tolerance = 0.00001
    
    i = 1
    tol_old = (a + b)/2.0
    write (*,20) "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x", "Toleransi"
    20 format (A7, 5(A20))
    do
        if (func(a_temp)*func(b_temp) < 0) then
            x = (a_temp + b_temp)/2.0
            if (func(a_temp)*func(x) < 0) then
                b_temp = x
            else
                a_temp = x
            end if
        else if (func(a_temp)*func(x) > 0) then
            write (*,*) "tidak ada solusi di daerah a hingga b"
            exit
        else
            write (*,*) "solusi ", x
            exit
        end if
        tol_i = (b_temp + a_temp)/2.0
        write (*,10) i, a_temp, b_temp, x, func(x), abs(tol_i - tol_old)
        10 format (I7, 5(F20.7))
        if (abs(tol_i - tol_old) < tolerance) then
            write (*,*) "Sudah mencapai batas toleransi"
            exit
        end if
        tol_old = tol_i
        i = i + 1
    end do
end subroutine bisection