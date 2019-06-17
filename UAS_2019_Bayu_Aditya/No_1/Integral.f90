! Author : Bayu Aditya
! Copyright (c) 2019

module integral
    implicit none
    integer, parameter :: DBL = 8
    contains
    double precision function trapezoid(func, batas_bawah, batas_atas, n_trap)
        ! Sumber algoritma :
        !   Metode Numerik 2 - Imam Fachruddin
        implicit none
        real(8) :: func                         ! fungsi yang akan dihitung
        real(8), intent(in) :: batas_atas       ! batas bawah integral
        real(8), intent(in) :: batas_bawah      ! batas atas integral
        integer, intent(in) :: n_trap           ! banyaknya trapezoid

        real(8) :: h, sum
        integer :: i
        sum = 0.0_DBL
        h = (batas_atas - batas_bawah)/DBLE(n_trap)
        do i = 0, n_trap
            if ((i == 0) .or. (i == n_trap)) then
                sum = sum + 0.5_DBL*func(batas_bawah + DBLE(i)*h)
            else
                sum = sum + func(batas_bawah + DBLE(i)*h)
            end if
        end do
        trapezoid = h*sum
    end function trapezoid


    double precision function simpson13(func, batas_bawah, batas_atas, n_2)
        ! Sumber algoritma :
        !   Numerical Methods - Amos Gilat
        implicit none
        real(8) :: func                         ! fungsi yang akan dihitung
        real(8), intent(in) :: batas_bawah      ! batas bawah integral
        real(8), intent(in) :: batas_atas       ! batas atas integral
        integer, intent(in) :: n_2              ! banyaknya segment 2

        real(8) :: h, sum
        integer :: i, n
        n = 2*n_2       ! karena simpson38 satu segment = 2 segment kecil

        h = (batas_atas - batas_bawah)/DBLE(n)
        sum = func(batas_bawah)
        do i = 2, n, 2
            sum = sum + 4.0_DBL*func(batas_bawah + h*DBLE(i-1))
        end do
        do i = 3, n-1, 2
            sum = sum + 2.0_DBL*func(batas_bawah + h*DBLE(i-1))
        end do
        sum = sum + func(batas_atas)
        simpson13 = (h/3.0_DBL)*sum
    end function simpson13


    double precision function simpson38(func, batas_bawah, batas_atas, n_3)
        ! Sumber algoritma :
        !   Numerical Methods - Amos Gilat
        implicit none
        real(8) :: func                         ! fungsi yang akan dihitung
        real(8), intent(in) :: batas_bawah      ! batas bawah integral
        real(8), intent(in) :: batas_atas       ! batas atas integral
        integer, intent(in) :: n_3                ! banyaknya segment 3

        real(8) :: h, sum, fxi, fxi1
        integer :: i, n
        n = 3*n_3       ! karena simpson38 satu segment = 3 segment kecil

        h = (batas_atas - batas_bawah)/DBLE(n)
        sum = func(batas_bawah)
        do i = 2, n-1, 3
            fxi = func(batas_bawah + h*DBLE(i-1))
            fxi1 = func(batas_bawah + h*DBLE(i))
            sum = sum + 3.0_DBL*(fxi + fxi1)
        end do
        do i = 4, n-2, 3
            fxi = func(batas_bawah + h*DBLE(i-1))
            sum = sum + 2.0_DBL*fxi
        end do
        sum = sum + func(batas_atas)
        simpson38 = (3.0_DBL*h/8.0_DBL)*sum
    end function simpson38
end module integral