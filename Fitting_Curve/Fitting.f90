module fitting_curve
    implicit none
    integer, private, parameter :: SGL = 4
    integer, private, parameter :: DBL = 8

    contains
!+-----------------------------------------------------------------------------------------+
!                         FITTING DATA : Lagrange                                          |
!+-----------------------------------------------------------------------------------------+
    subroutine lagrange_plot(dataX, dataY, name_output)
        implicit none
        real(KIND=DBL), dimension(:), intent(in) :: dataX
        real(KIND=DBL), dimension(:), intent(in) :: dataY
        character(*), intent(in) :: name_output

        integer, parameter :: n_data_plot = 500
        real(KIND=DBL), dimension(0:n_data_plot) :: X, Y
        real(KIND=DBL) :: range, min, max, diff
        integer :: i

        ! Data untuk Sumbu X
        range = maxval(dataX) - minval(dataX)
        min = minval(dataX) - range/5_DBL
        max = maxval(dataX) + range/5_DBL

        diff = (max - min)/n_data_plot
        X(0) = min
        do i = 1, n_data_plot
            X(i) = min + diff*DBLE(i)
            Y(i) = lagrange(X(i), dataX, dataY)
        end do

        ! Membuat data koordinat titik plot ke dalam file
        open(unit = 13, file = name_output, action='write')
        do i = 0, n_data_plot
            write(13, '(F25.15, A, F25.15)') X(i), ",", Y(i)
        end do
        close(13)

        write(*, '(3(A))') "[INFO] Data interpolasi lagrange '", name_output, "' berhasil dibuat"
    end subroutine lagrange_plot


    double precision function lagrange(x, dataX, dataY)
        implicit none
        real(KIND=DBL), intent(in) :: x
        real(KIND=DBL), dimension(:), intent(in) :: dataX
        real(KIND=DBL), dimension(:), intent(in) :: dataY
        
        real(KIND=DBL) :: sum, prod
        integer :: i, j
        integer :: n

        n = ubound(dataX, 1)

        sum = 0_DBL
        do i = 1, n
            prod = 1_DBL
            do j = 1, n
                if (i /= j) prod = prod * ((x - dataX(j)) / (dataX(i) - dataX(j)))
            end do

            sum = sum + prod*dataY(i)
        end do
        lagrange = sum
    end function lagrange


!+-----------------------------------------------------------------------------------------+
!                         FITTING DATA : Least Square                                      |
!+-----------------------------------------------------------------------------------------+
    subroutine least_square_plot(dataX, dataY, orde, name_output)
        implicit none
        real(KIND=DBL), dimension(:), intent(in) :: dataX
        real(KIND=DBL), dimension(:), intent(in) :: dataY
        integer :: orde
        character(*), intent(in) :: name_output

        integer, parameter :: n_data_plot = 500
        real(KIND=DBL), dimension(0:n_data_plot) :: X, Y
        real(KIND=DBL) :: range, min, max, diff
        integer :: i

        ! Data untuk Sumbu X
        range = maxval(dataX) - minval(dataX)
        min = minval(dataX) - range/5_DBL
        max = maxval(dataX) + range/5_DBL

        diff = (max - min)/n_data_plot
        X(0) = min
        do i = 1, n_data_plot
            X(i) = min + diff*DBLE(i)
            Y(i) = least_square(X(i), dataX, dataY, orde)
        end do

        ! Membuat data koordinat titik plot ke dalam file
        open(unit = 13, file = name_output, action='write')
        do i = 0, n_data_plot
            write(13, '(F25.15, A, F25.15)') X(i), ",", Y(i)
        end do
        close(13)
        
        write(*, '(3(A))') "[INFO] Data Least Square '", name_output, "' berhasil dibuat"
    end subroutine least_square_plot


    double precision function least_square(x, dataX, dataY, orde)
        implicit none
        real(KIND=DBL), intent(in) :: x
        real(KIND=DBL), dimension(:), intent(in) :: dataX
        real(KIND=DBL), dimension(:), intent(in) :: dataY
        integer, intent(in) :: orde

        real(KIND=DBL), allocatable, dimension(:,:) :: matC
        real(KIND=DBL), allocatable, dimension(:) :: matB
        real(KIND=DBL), allocatable, dimension(:) :: koef_polinomial
        real(KIND=DBL) :: sum
        integer :: i, j, k
        integer :: n

        n = ubound(dataX, 1)
        if (orde > n-1) then 
            write(*,'(A)') "[PERINGATAN] Input Orde lebih besar dari jumlah data !" 
            STOP
        end if

        allocate(matC(0:orde, 0:orde))
        allocate(matB(0:orde))
        allocate(koef_polinomial(0:orde))

        matC = 0_DBL
        matB = 0_DBL
        koef_polinomial = 0_DBL

        ! Membuat matriks C (Persegi) dan matriks B (Kolom)
        do k = 0, orde
            do j = 0, orde
                sum = 0_DBL
                do i = 1, n
                    sum = sum + dataX(i)**(j+k)
                end do
                matC(k,j) = sum
            end do

            sum = 0_DBL
            do i = 1, n
                sum = sum + dataY(i)*dataX(i)**k
            end do
            matB(k) = sum
        end do
        
        ! Proses Gauss Jordan
        call gauss_jordan(matC, matB, koef_polinomial)

        ! ------------------ ANALYSIS SECTION ---------------------------
        !write(*,*) "matriks C"
        !call print_matriks(matC)
        !write(*,*) "matriks B"
        !call print_vektor(matB)
        !call print_hasil(hasil)
        ! -------------- END OF ANALYSIS SECTION ------------------------

        ! Mendapatkan f(x)
        sum = 0_DBL
        do j = 0, orde
            sum = sum + koef_polinomial(j)*x**(j)
        end do
        least_square = sum

        deallocate(matC)
        deallocate(matB)
        deallocate(koef_polinomial)
    end function least_square


!+-------------------------------------------------------------------------------------------+
!                     SOLVE SYSTEM OF LINEAR EQUATION SECTION                                |
!+-------------------------------------------------------------------------------------------+
    subroutine gauss_jordan(matriksA, matriksB, x)
        implicit none
        real(KIND=DBL), dimension(:,:), intent(inout) :: matriksA
        real(KIND=DBL), allocatable, dimension(:,:) :: matA_dummy     ! matriks A ke k
        real(KIND=DBL), allocatable, dimension(:,:) :: matA_dummy_1   ! matriks A ke k-1

        real(KIND=DBL), dimension(:), intent(inout) :: matriksB
        real(KIND=DBL), allocatable, dimension(:) :: matB_dummy       ! matriks B ke k
        real(KIND=DBL), allocatable, dimension(:) :: matB_dummy_1     ! matriks B ke k-1

        real(KIND=DBL), dimension(:), intent(out) :: x                ! hasil dari x1, x2, ..., xn
        
        real(KIND=DBL) :: sum
        integer :: m, n
        integer :: i, j, k

        m = ubound(matriksA, 1)
        n = ubound(matriksA, 2)
        allocate(matA_dummy(m, n))
        allocate(matA_dummy_1(m, n))
        allocate(matB_dummy(m))
        allocate(matB_dummy_1(m))

        matA_dummy_1 = matriksA
        matA_dummy = matriksA
        matB_dummy_1 = matriksB
        matB_dummy = matriksB

        ! Proses membuat matriks A menjadi matriks upper beserta matriks B mangikuti prosesnya
        do k = 1, n-1
            if (matA_dummy_1(k,k) == 0) then 
                call pivot(matA_dummy_1, matB_dummy_1, k)
                !call print_matriks(matA_dummy_1)
                !call print_vektor(matB_dummy_1)
                matA_dummy(k,:) = matA_dummy_1(k,:)
                matB_dummy(k) = matB_dummy_1(k)
            end if
            do i = k+1, m
                do j = k, n
                    matA_dummy(i,j) = matA_dummy_1(i,j) - matA_dummy_1(i,k)*matA_dummy_1(k,j)/matA_dummy_1(k,k)
                    matB_dummy(i) = matB_dummy_1(i) - matA_dummy_1(i,k)*matB_dummy_1(k)/matA_dummy_1(k,k)
                end do
            end do
            !write(*,*) k
            !call print_matriks(matA_dummy)
            !call print_vektor(matB_dummy)
            matA_dummy_1 = matA_dummy
            matB_dummy_1 = matB_dummy
        end do

        matriksA = matA_dummy
        matriksB = matB_dummy
        deallocate(matA_dummy)
        deallocate(matA_dummy_1)
        deallocate(matB_dummy)
        deallocate(matB_dummy_1)

        ! Subtitusi mundur (karena m = n)
        x(n) = matriksB(n)/matriksA(n,n)
        do j = 1, n-1
            sum = 0_DBL
            do k = n-j+1, n
                sum = sum + matriksA(n-j, k)*x(k)
            end do
            x(n-j) = (matriksB(n-j) - sum) / (matriksA(n-j, n-j))
        end do
    end subroutine gauss_jordan


!+--------------------------------------------------------------------------------------------+
!                              PIVOTING SECTION                                               |
!+--------------------------------------------------------------------------------------------+
    subroutine pivot(matriksA, matriksB, k)
        implicit none
        real(KIND=DBL), dimension(:,:), intent(inout) :: matriksA
        real(KIND=DBL), dimension(:), intent(inout) :: matriksB
        integer, intent(in) :: k
        integer :: n
        n = ubound(matriksA, 1)

        call tukar_baris(matriksA, k, k+1)
        call swap(matriksB(k), matriksB(k+1))
    end subroutine pivot


    subroutine tukar_baris(matriks, baris_asal, baris_tujuan)
        implicit none
        integer, intent(in) :: baris_asal, baris_tujuan
        real(KIND=DBL), dimension(:,:), intent(inout) :: matriks
        integer :: j

        do j = 1, ubound(matriks, 1)
            call swap(matriks(baris_asal, j), matriks(baris_tujuan, j))
        end do
        write(*,"(2(A, I3), A)") "[INFO] Proses Tukar Baris ", baris_asal, " dan ", baris_tujuan, " Berhasil"
    end subroutine tukar_baris


    subroutine swap(a, b)
        implicit none
        real(KIND=DBL), intent(inout) :: a, b
        real(KIND=DBL) :: temp

        temp = a
        a = b
        b = temp
    end subroutine swap


!+---------------------------------------------------------------------------------------------+
!                               DATA SECTIONS                                                  |
!+---------------------------------------------------------------------------------------------+
    subroutine import_data(filename, dataX, dataY)
        implicit none
        character(*), intent(in) :: filename
        real(KIND=DBL), allocatable, dimension(:), intent(out) :: dataX
        real(KIND=DBL), allocatable, dimension(:), intent(out) :: dataY
        real(KIND=DBL), allocatable, dimension(:,:) :: dataXY
        integer :: i, j, n

        open(unit = 8, file = filename, action = 'read')
        read(8,*) n

        allocate(dataX(n))
        allocate(dataY(n))
        allocate(dataXY(n,n))

        do i = 1, n
            read(8,*) (dataXY(i,j), j = 1, 2)
        end do

        write(*,'(A, I3, A)') "[INFO] Input ", n, " data"
        close(8)

        dataX = dataXY(:,1)
        dataY = dataXY(:,2)
        
        deallocate(dataXY)
    end subroutine import_data


    subroutine print_matriks(matriks)
        implicit none
        real(KIND=DBL), dimension(:,:), intent(in) :: matriks
        integer :: i, j

        do i = lbound(matriks, 1), ubound(matriks, 1)
            write(*,*) (matriks(i,j), j = lbound(matriks, 2), ubound(matriks, 2))
        end do
        write(*,*)
    end subroutine print_matriks


    subroutine print_vektor(data)
        implicit none
        real(KIND=DBL), dimension(:), intent(in) :: data
        integer :: i, n

        n = ubound(data, 1)
        do i = 1, n
            write(*,*) data(i)
        end do
        write(*,*)
    end subroutine print_vektor


    subroutine print_hasil(data)
        implicit none
        real(KIND=DBL), dimension(:), intent(in) :: data
        integer :: i, n

        write(*,'(A)') "=============== HASIL FITTING DATA ================"

        n = ubound(data, 1)
        do i = 1, n
            write(*,'(A, I2, ES30.15)') "Koefisien Pangkat-", i-1, data(i)
        end do
        write(*,*)
    end subroutine print_hasil
end module fitting_curve