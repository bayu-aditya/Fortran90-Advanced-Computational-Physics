module system_of_linear_equation
    implicit none

    contains
!+------------------------------------------------------------------------------+
!                     SOLVE SYSTEM OF LINEAR EQUATION SECTION                   |
!+------------------------------------------------------------------------------+
    subroutine gauss_jordan(matriksA, matriksB, x)
        implicit none
        real(8), dimension(:,:), intent(inout) :: matriksA
        real(8), allocatable, dimension(:,:) :: matA_dummy     ! matriks A ke k
        real(8), allocatable, dimension(:,:) :: matA_dummy_1   ! matriks A ke k-1

        real(8), dimension(:), intent(inout) :: matriksB
        real(8), allocatable, dimension(:) :: matB_dummy       ! matriks B ke k
        real(8), allocatable, dimension(:) :: matB_dummy_1     ! matriks B ke k-1

        real(8), dimension(:), intent(out) :: x                ! hasil dari x1, x2, ..., xn
        
        real(8) :: sum
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
            sum = 0
            do k = n-j+1, n
                sum = sum + matriksA(n-j, k)*x(k)
            end do
            x(n-j) = (matriksB(n-j) - sum) / (matriksA(n-j, n-j))
        end do

        ! Menampilkan informasi bahwa proses Gauss Jordan telah selesai
        write(*,'(A)') "[INFO] Proses Gauss Jordan Selesai"
        write(*,*)
    end subroutine gauss_jordan


    subroutine LU_decomposition(matriksA, matriksL, matriksU)
        implicit none
        real(8), dimension(:,:), intent(in) :: matriksA
        real(8), dimension(:,:), intent(out) :: matriksL
        real(8), dimension(:,:), intent(out) :: matriksU

        real(8), allocatable, dimension(:,:) :: matA
        real(8) :: sum
        integer :: n
        integer :: i, j, k

        n = ubound(matriksA, 1)
        matriksL = 0
        matriksU = 0

        allocate(matA(n, n))
        matA = matriksA

        do j = 1, n
            do i = j, n
                sum = 0
                do k = 1, j-1
                    sum = sum + matriksL(i,k)*matriksU(k,j)
                end do
                matriksL(i,j) = matA(i,j) - sum
            end do

            if (matriksL(j,j) == 0) then
                call tukar_baris(matriksL, j, j+1)
                call tukar_baris(matA, j, j+1)
                !call pivot(matA, matriksB, j)
            end if

            do i = j, n
                sum = 0
                do k = 1, j-1
                    sum = sum + matriksL(j,k)*matriksU(k,i)
                end do
                matriksU(j,i) = (matA(j,i) - sum) / matriksL(j,j)
            end do
        end do

        deallocate(matA)
    end subroutine LU_decomposition


    subroutine substitusi_LU_decomp(matriksL, matriksU, matriksB, matriksX)
        implicit none
        real(8), dimension(:,:), intent(in) :: matriksL, matriksU
        real(8), dimension(:), intent(in) :: matriksB
        real(8), dimension(:), intent(out) :: matriksX

        real(8), allocatable, dimension(:) :: matriksY
        real(8) :: sum
        integer :: i, j
        integer :: n

        n = ubound(matriksL, 1)
        allocate(matriksY(n))
        matriksY = 0

        ! Substitusi maju untuk menghasilkan matriks Y
        matriksY(1) = matriksB(1) / matriksL(1,1)
        do i = 2, n
            sum = 0.0
            do j = 1, i-1
                sum = sum + matriksL(i,j)*matriksY(j)
            end do
            matriksY(i) = (matriksB(i) - sum) / matriksL(i,i)
        end do
        write(*,'(A)') "Matriks Y"
        call print_vektor(matriksY)

        ! Substitusi mundur untuk menghasilkan matriks X
        matriksX(n) = matriksY(n)
        do i = 1, n-1
            sum = 0
            do j = n-i+1, n
                sum = sum + matriksU(n-i,j)*matriksX(j)
            end do
            matriksX(n-i) = matriksY(n-i) - sum
        end do
        write(*,'(A)') "Matriks X (Hasil)"
        call print_vektor(matriksX)

        deallocate(matriksY)
    end subroutine substitusi_LU_decomp


    subroutine iterasi_jacobi(matriksA, matriksB, x)
        implicit none
        real(8), dimension(:,:), intent(in) :: matriksA
        real(8), dimension(:), intent(in) :: matriksB
        real(8), allocatable, dimension(:), intent(out) :: x
        real(8), allocatable, dimension(:) :: x_old
        real(8) :: sum, tol_i
        real(8), parameter :: tolerance = 0.001
        integer :: n
        integer :: i, j
        integer :: flags
        integer :: index

        n = ubound(matriksA, 1)
        allocate(x(n))
        allocate(x_old(n))

        x_old = 1
        index = 0

        do
            index = index + 1
            flags = 0

            do i = 1, n
                sum = 0
                do j = 1, n
                    if (j /= i) sum = sum + matriksA(i,j)*x_old(j)
                end do

                x(i) = (1/matriksA(i,i))*(matriksB(i) - sum)
            end do

            ! menghitung toleransi
            do j = 1, n
                tol_i = abs(1 - x_old(j)/x(j))
                if (tol_i < tolerance) flags = flags + 1
                write(*,*) tol_i
            end do
            write(*,*) x

            write(*,*) "iterasi ke -", index, "success"
            write(*,*)
            if (flags == n) exit

            x_old = x
        end do

        deallocate(x_old)
    end subroutine iterasi_jacobi


!+------------------------------------------------------------------------------+
!                              PIVOTING SECTION                                 |
!+------------------------------------------------------------------------------+
    subroutine pivot(matriksA, matriksB, k)
        implicit none
        real(8), dimension(:,:), intent(inout) :: matriksA
        real(8), dimension(:), intent(inout) :: matriksB
        integer, intent(in) :: k
        integer :: n
        n = ubound(matriksA, 1)

        call tukar_baris(matriksA, k, k+1)
        call swap(matriksB(k), matriksB(k+1))
    end subroutine pivot


    subroutine tukar_baris(matriks, baris_asal, baris_tujuan)
        implicit none
        integer, intent(in) :: baris_asal, baris_tujuan
        real(8), dimension(:,:), intent(inout) :: matriks
        integer :: j

        do j = 1, ubound(matriks, 1)
            call swap(matriks(baris_asal, j), matriks(baris_tujuan, j))
        end do
        write(*,"(2(A, I3), A)") "[INFO] Proses Tukar Baris ", baris_asal, " dan ", baris_tujuan, " Berhasil"
    end subroutine tukar_baris


    subroutine swap(a, b)
        implicit none
        real(8), intent(inout) :: a, b
        real(8) :: temp

        temp = a
        a = b
        b = temp
    end subroutine swap


!+------------------------------------------------------------------------------+
!                              MATRIX TOOLS SECTION                             |
!+------------------------------------------------------------------------------+
    subroutine import_matriks(nama_file, matriks, n)
        implicit none
        character(*), intent(in) :: nama_file
        real(8), allocatable, dimension(:,:), intent(out) :: matriks
        integer :: i, j, n

        open(unit = 3, file = nama_file, action = 'read')
        read(3,*) n
        allocate(matriks(n,n))
        do i = 1, n
            read(3,*) (matriks(i,j), j = 1, n)
        end do
    end subroutine import_matriks

    subroutine import_vektor(nama_file, vektor)
        implicit none
        character(*), intent(in) :: nama_file
        real(8), allocatable, dimension(:), intent(out) :: vektor
        integer :: j, n

        open(unit = 4, file = nama_file, action = 'read')
        read(4,*) n
        allocate(vektor(n))
        read(4,*) (vektor(j), j = 1, n)
    end subroutine import_vektor


    subroutine print_matriks(matriks)
        implicit none
        real(8), dimension(:,:), intent(in) :: matriks
        integer :: i, j

        do i = lbound(matriks, 1), ubound(matriks, 1)
            write(*,*) (matriks(i,j), j = lbound(matriks, 2), ubound(matriks, 2))
        end do
        write(*,*)
    end subroutine print_matriks


    subroutine print_vektor(vektor)
        implicit none
        real(8), dimension(:), intent(in) :: vektor
        integer :: i

        do i = lbound(vektor, 1), ubound(vektor, 1)
            write(*,*) vektor(i)
        end do
        write(*,*)
    end subroutine print_vektor
end module system_of_linear_equation