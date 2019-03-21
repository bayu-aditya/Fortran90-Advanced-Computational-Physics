module fitting_curve
    implicit none
    integer, parameter :: SGL = 4
    integer, parameter :: DBL = 8

    contains
!+-----------------------------------------------------------------------------------------+
!                               LEAST SQUARE                                               |
!+-----------------------------------------------------------------------------------------+
    subroutine least_square(dataX, dataY)
        implicit none
        real(KIND=DBL), dimension(:), intent(in) :: dataX
        real(KIND=DBL), dimension(:), intent(in) :: dataY

        real(KIND=DBL), allocatable, dimension(:,:) :: matC
        real(KIND=DBL), allocatable, dimension(:) :: matB

        real(KIND=DBL), allocatable, dimension(:) :: hasil
        real(KIND=DBL) :: sum
        integer :: i, j, k
        integer :: n

        n = ubound(dataX, 1)
        allocate(matC(0:n-1, 0:n-1))
        allocate(matB(0:n-1))
        allocate(hasil(n))

        matC = 0_DBL
        matB = 0_DBL
        hasil = 0_DBL

        do k = 0, n-1
            do j = 0, n-1
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

        write(*,*) "matriks C"
        call print_matriks(matC)

        write(*,*) "matriks B"
        call print_vektor(matB)

        ! Proses Gauss Jordan
        call gauss_jordan(matC, matB, hasil)
        call print_hasil(hasil)

        deallocate(matC)
        deallocate(matB)
        deallocate(hasil)

        write(*,'(A)') "[INFO] Proses Least Square selesai"
    end subroutine least_square


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

        ! Menampilkan informasi bahwa proses Gauss Jordan telah selesai
        write(*,'(A)') "[INFO] Proses Gauss Jordan Selesai"
        write(*,*)
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

        write(*,'(A, I3, A)') "[INFO] Input ", n, " baris"
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