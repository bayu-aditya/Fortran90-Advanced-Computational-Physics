module eigen
    implicit none
    contains
    subroutine power_method(matriks, eigenvalues)
        implicit none
        real(8), dimension(:,:), intent(in) :: matriks
        real(8), dimension(:), intent(out) :: eigenvalues

        real(8), allocatable, dimension(:) :: X, Y, Y0
        real(8) :: eig, eig0
        real(8) :: error
        integer :: i, n

        n = ubound(matriks,1)
        allocate(X(n))
        allocate(Y(n))
        allocate(Y0(n))
        X = 2.d0

        do i = 1, 50
            if (i == 1) then
                Y = matmul(matriks, X)
                Y0 = Y
            else if (i == 2) then
                X = Y0
                Y = matmul(matriks, X)
                eig = (dot_product(X,Y))/(dot_product(X,Y0))
                eig0 = eig
                Y0 = Y
            else
                X = Y0
                Y = matmul(matriks, X)
                eig = (dot_product(X,Y))/(dot_product(X,Y0))
                error = abs(1.d0 - eig0/eig)
                write(*,*) i, error, eig
                eig0 = eig
                Y0 = Y
            end if
        end do
    end subroutine power_method


    subroutine sekular(mat, eigenvalues)
        implicit none
        real(8), dimension(:,:), intent(in) :: mat          ! Matriks Persegi
        real(8), dimension(:), intent(out) :: eigenvalues

        integer :: n

        n = ubound(mat, 1)

        if (n == 2) then
            call sweep(sekular_n2, -1000.d0, 1000.d0, n, eigenvalues)
        else if (n == 3) then
            call sweep(sekular_n3, -1000.d0, 1000.d0, n, eigenvalues)
        end if

        contains
        subroutine sweep(func, batas_bawah, batas_atas, n, eigenvalues)
            use nonlinear_equation
            implicit none
            real(8) :: func
            real(8), intent(in) :: batas_bawah, batas_atas
            integer, intent(in) :: n
            real(8), dimension(n), intent(out) :: eigenvalues
            real(8), parameter :: step = 0.1d0
            real(8) :: a, eig
            integer :: i
            a = batas_bawah
            i = 1
            do 
                if (a > batas_atas) exit
                a = a + step
                if (func(a)*func(a-step) < 0.d0) then
                    ! call bisection(func, a-step, a, eig)
                    ! call false_position(func, a-step, a, eig)
                    call secant(func, a-step, a, eig)
                    eigenvalues(i) = eig
                    i = i + 1
                    write(*,*)      ! Memberikan space antar penyelesaian eign
                end if
            end do
        end subroutine sweep

        double precision function sekular_n2(a)
            implicit none
            real(8), intent(in) :: a
            sekular_n2 = (a-mat(1,1)) * (a-mat(2,2)) - mat(1,2)*mat(2,1)
        end function sekular_n2

        double precision function sekular_n3(a)
            implicit none
            real(8), intent(in) :: a
            sekular_n3 = (a-mat(1,1)) * (a-mat(2,2)) * (a-mat(3,3)) &
                    - (a-mat(1,1))*mat(2,3)*mat(3,2) &
                    - (a-mat(2,2))*mat(1,3)*mat(3,1) &
                    - (a-mat(3,3))*mat(1,2)*mat(2,1) &
                    - mat(1,2)*mat(2,3)*mat(3,1) - mat(1,3)*mat(3,2)*mat(2,1)
        end function sekular_n3
    end subroutine sekular


!+------------------------------------------------------------------------------+
!                              MATRIX TOOLS SECTION                             |
!+------------------------------------------------------------------------------+
    subroutine import_matriks(nama_file, matriks, n)
        implicit none
        character(*), intent(in) :: nama_file
        real(8), allocatable, dimension(:,:), intent(out) :: matriks
        integer, intent(out) :: n
        integer :: i, j

        open(unit = 3, file = nama_file, action = 'read')
        read(3,*) n
        allocate(matriks(n,n))
        do i = 1, n
            read(3,*) (matriks(i,j), j = 1, n)
        end do
    end subroutine import_matriks
end module eigen