program metode_secant
    use nonlinear_equation

    implicit none
    INTEGER, PARAMETER :: dp=KIND(1.0D0)
    real(8), external :: func
    real(8) :: batas_kiri, batas_kanan
    real(8) :: hasil

    batas_kiri = 0.72_dp
    batas_kanan = 0.75_dp

    call bisection_dp(func, batas_kiri, batas_kanan, hasil)
    write(*,'(A, F25.15)') "hasil adalah ", hasil
    write(*,*)

    call false_position(func, batas_kiri, batas_kanan, hasil)
    write(*,'(A, F25.15)') "hasil adalah ", hasil
    write(*,*)

    call secant(func, batas_kiri, batas_kanan, hasil)
    write(*,'(A, F25.15)') "hasil adalah ", hasil
end program metode_secant


double precision function func(x)
    implicit none
    real(8), intent(in) :: x
    real(8) :: cos

    func = cos(x) - x
end function func