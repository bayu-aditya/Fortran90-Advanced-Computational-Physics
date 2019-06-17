! Author : Bayu Aditya
! Copyright (c) 2019

module ode
    implicit none
    integer, parameter :: DBL = 8
    contains
    ! --------------------------------------------------------------------------------
    !                   Initial Value Problem (PDB Kondisi Awal)                     |
    ! ------------------------------- Orde 1 -----------------------------------------
    subroutine euler(func, y0, x0, xn, n, nama_file_output)
        implicit none
        real(8) :: func
        real(8), intent(in) :: y0
        real(8), intent(in) :: x0, xn
        integer, intent(in) :: n
        character(*), intent(in) :: nama_file_output

        integer :: i
        real(8) :: h
        real(8), dimension(0:n) :: X, Y
        h = (xn - x0)/DBLE(n)
        X(0) = x0
        Y(0) = y0
        do i = 1, n
            X(i) = X(0) + DBLE(i)*h
            Y(i) = Y(i-1) + h*func(X(i-1), Y(i-1))
        end do
        
        open(unit = 13, file = nama_file_output, action='write')
        do i = 0, n
            write(13, '(F25.15, A, F25.15)') X(i), ",", Y(i)
        end do
        close(13)
    end subroutine euler


    subroutine euler_modified(func, y0, x0, xn, n, nama_file_output)
        implicit none
        real(8) :: func
        real(8), intent(in) :: y0
        real(8), intent(in) :: x0, xn
        integer, intent(in) :: n
        character(*), intent(in) :: nama_file_output

        integer :: i
        real(8) :: h
        real(8), dimension(0:n) :: X, Y
        real(8) :: fx0y0, fx1y1
        h = (xn - x0)/DBLE(n)
        X(0) = x0
        Y(0) = y0
        do i = 1, n
            X(i) = X(0) + DBLE(i)*h
            fx0y0 = func(X(i-1), Y(i-1))
            fx1y1 = func(X(i), h*fx0y0)
            Y(i) = Y(i-1) + 0.5_DBL*h*(fx0y0 + fx1y1)
        end do
        
        open(unit = 13, file = nama_file_output, action='write')
        do i = 0, n
            write(13, '(F25.15, A, F25.15)') X(i), ",", Y(i)
        end do
        close(13)
    end subroutine euler_modified


    subroutine RK4(func, y0, x0, xn, n, nama_file_output)
        implicit none
        real(8) :: func
        real(8), intent(in) :: y0
        real(8), intent(in) :: x0, xn
        integer, intent(in) :: n
        character(*), intent(in) :: nama_file_output

        integer :: i
        real(8) :: h
        real(8), dimension(0:n) :: X, Y
        real(8) :: f0, f1, f2, f3
        h = (xn - x0)/DBLE(n)
        X(0) = x0
        Y(0) = y0
        do i = 1, n
            X(i) = X(0) + DBLE(i)*h
            f0 = func(X(i-1), Y(i-1))
            f1 = func(X(i-1)+0.5_DBL*h, Y(i-1)+0.5_DBL*h*f0)
            f2 = func(X(i-1)+0.5_DBL*h, Y(i-1)+0.5_DBL*h*f1)
            f3 = func(X(i-1)+h, Y(i-1)+h*f2)
            Y(i) = Y(i-1) + (1.0_DBL/6.0_DBL)*h*(f0 + 2.0_DBL*(f1+f2) + f3) 
        end do
        
        open(unit = 13, file = nama_file_output, action='write')
        do i = 0, n
            write(13, '(F25.15, A, F25.15)') X(i), ",", Y(i)
        end do
        close(13)
    end subroutine RK4


    ! --------------------------------------------------------------------------------
    !                   Initial Value Problem (PDB Kondisi Awal)                     |
    ! ------------------------------- Orde 2 -----------------------------------------
    subroutine euler_modified2(func, y0, u0, x0, xn, n, nama_file_output)
        implicit none
        real(8) :: func
        real(8), intent(in) :: y0, u0
        real(8), intent(in) :: x0, xn
        integer, intent(in) :: n
        character(*), intent(in) :: nama_file_output

        integer :: i
        real(8) :: h
        real(8), dimension(0:n) :: X, Y, U
        real(8) :: f0, f1
        h = (xn - x0)/DBLE(n)
        X(0) = x0
        Y(0) = y0
        U(0) = u0
        do i = 1, n
            X(i) = X(0) + DBLE(i)*h
            f0 = func(X(i-1), Y(i-1), U(i-1))
            U(i) = U(i-1) + h*f0
            f1 = func(X(i), Y(i-1)+h*U(i-1), U(i))
            Y(i) = Y(i-1) + 0.5_DBL*h*(U(i-1) + U(i))
            U(i) = U(i-1) + 0.5_DBL*h*(f0 + f1)
        end do
        
        open(unit = 13, file = nama_file_output, action='write')
        do i = 0, n
            write(13, '(F25.15, A, F25.15, A, F25.15)') X(i), ",", Y(i), ",", U(i)
        end do
        close(13)
        write(*,'(2A)') "[INFO] File berhasil disimpan di ", nama_file_output
    end subroutine euler_modified2


    subroutine RK4_orde2(func, y0, u0, x0, xn, n, nama_file_output)
        implicit none
        real(8) :: func
        real(8), intent(in) :: y0, u0
        real(8), intent(in) :: x0, xn
        integer, intent(in) :: n
        character(*), intent(in) :: nama_file_output

        integer :: i
        real(8) :: h
        real(8), dimension(0:n) :: X, Y, U
        real(8) :: f0, f1, f2, f3, u_0, u_1, u_2, u_3
        h = (xn - x0)/DBLE(n)
        X(0) = x0
        Y(0) = y0
        U(0) = u0
        do i = 1, n
            u_0 = U(i-1)
            X(i) = X(0) + DBLE(i)*h
            f0 = func(X(i-1), Y(i-1), u_0)
            u_1 = u_0 + 0.5_DBL*h*f0
            f1 = func(X(i)+0.5_DBL*h, Y(i-1)+0.5_DBL*h*u_0, u_1)
            u_2 = u_0 + 0.5_DBL*h*f1
            f2 = func(X(i)+0.5_DBL*h, Y(i-1)+0.5_DBL*h*u_1, u_2)
            u_3 = u_0 + h*f2
            f3 = func(X(i)+h, Y(i-1)+h*u_2, u_3)
            Y(i) = Y(i-1) + (1.0_DBL/6.0_DBL)*h*(u_0 + 2.0_DBL*(u_1+u_2) + u_3)
            U(i) = u_0 + (1.0_DBL/6.0_DBL)*h*(f0 + 2.0_DBL*(f1+f2) + f3)
        end do
        
        open(unit = 13, file = nama_file_output, action='write')
        do i = 0, n
            write(13, '(F25.15, A, F25.15, A, F25.15)') X(i), ",", Y(i), ",", U(i)
        end do
        close(13)
    end subroutine
end module ode