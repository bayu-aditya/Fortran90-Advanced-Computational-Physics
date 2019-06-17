! Author : Bayu Aditya
! Copyright (c) 2019

module pde
    integer, private, parameter :: DBL = 8
    real(8), private, parameter :: pi = 3.14159265358979
    type :: elliptic
        real(8) :: x0, xn           ! Sumbu x untuk x0 < x < xn
        real(8) :: y0, ym           ! SUmbu y untuk y0 < y < yn
        real(8) :: h                ! Jarak antar grid
        real(8) :: epsilon          ! Batas Toleransi Iterasi
        character(len=50) :: name   ! nama file output
    end type elliptic

    type :: parabolic
        real(8) :: gamma
        real(8) :: f0
        real(8) :: x0t, xnt
        real(8) :: x0, xn
        real(8) :: t0, tm
        integer :: n, m
        character(len=50) :: name_output
    end type parabolic
    
    type :: hyperbolic
        real(8) :: x0, xn           ! Batas sumbu x dari x0 sampai xn
        real(8) :: b                ! dpsi/dt | t=0 (ada di modul) default = 0
        real(8) :: v                ! Cepat rambat Gelombang
        character(len=50) :: name   ! Nama File Output
    end type hyperbolic


    contains
    subroutine pde_elliptic(func, fun_x0, fun_xn, fun_y0, fun_ym, param)
        implicit none
        real(8) :: func             ! Fungsi di suku bagian kanan
        real(8) :: fun_x0           ! Batas saat T(x=0, y) ; f(y)
        real(8) :: fun_xn           ! Batas saat T(x=n, y) ; f(y)
        real(8) :: fun_y0           ! Batas saat T(x, y=0) ; f(x)
        real(8) :: fun_ym           ! Batas saat T(x, y=m) ; f(x)
        type(elliptic) :: param

        real(8), allocatable, dimension(:,:) :: psi, psi0
        real(8) :: x0, xn           ! Sumbu x untuk x0 < x < xn
        real(8) :: y0, ym           ! SUmbu y untuk y0 < y < yn
        real(8) :: h                ! Jarak antar grid
        real(8) :: epsilon          ! Batas Toleransi
        character(len=50) :: name   ! Nama File Output

        integer :: n, m
        integer :: i, j, k
        real(8), allocatable, dimension(:) :: X, Y
        real(8) :: error

        x0 = param%x0
        xn = param%xn
        y0 = param%y0
        ym = param%ym
        h = param%h
        epsilon = param%epsilon
        name = param%name

        n = int((xn - x0)/(h))
        m = int((ym - y0)/(h))
        allocate(psi0(0:n, 0:m))
        allocate(psi(0:n, 0:m))
        allocate(X(0:n))
        allocate(Y(0:m))
        psi0 = 0_DBL
        psi = 0_DBL

        ! Membuat Sumbu X dan Y
        do i = 0, n
            X(i) = x0 + h*DBLE(i)
        end do
        do j = 0, m
            Y(j) = y0 + h*DBLE(j)
        end do

        ! Kondisi Batas
        do i = 0, n
            do j = 0, m
                if (i == 0) then
                    psi(i,j) = fun_x0(Y(j))
                else if (i == n) then
                    psi(i,j) = fun_xn(Y(j))
                else if (j == 0) then
                    psi(i,j) = fun_y0(X(i))
                else if (j == m) then
                    psi(i,j) = fun_ym(X(i))
                end if
            end do
        end do
        psi0 = psi

        ! Solusi di dalam grid
        do k = 1, 1000
            do i = 1, n-1
                do j = 1, m-1
                    psi(i,j) = -(h**2_DBL*func(X(i),Y(j))/4_DBL) + (1.d0/4.d0)*&
                                (psi0(i+1,j) + psi0(i-1,j) + psi0(i,j+1) + psi0(i,j-1))
                end do
            end do

            ! Mencari nilai kesalahan mutlak semu (I/O)
            call relatif_semu_2d(psi0, psi, error)
            write(*,*) k, error
            if (error <= epsilon) then
                write(*,'(A)') "[INFO] Error sudah dibawah batas kovergen"
                exit
            end if
            psi0 = psi
        end do

        ! Menuliskan hasil ke dalam file
        open(unit = 13, file = name, action='write')
        write(13, '(A25, A, A25, A, A25)') "Posisi X", ",", "Posisi Y", ",", "Fungsi"
        do j = 0, m
            do i = 0, n
                write(13, '(F25.15, A, F25.15, A, F25.15)') X(i), ",", Y(j),&
                ",", psi(i,j)
            end do
        end do
        close(13)

        deallocate(psi)
        deallocate(psi0)
    end subroutine pde_elliptic


    ! subroutine pde_parabolic(func, param)
    !     implicit none
    !     real(8) :: func
    !     type(parabolic), intent(in) :: param

    !     real(8) :: gamma
    !     real(8) :: f0, x0t, xnt
    !     real(8) :: x0, xn, t0, tm
    !     integer :: n, m
    !     character(len=50) :: name_output
    !     real(8) :: hx, ht
    !     integer :: i, j
    !     real(8), allocatable, dimension(:,:) :: psi

    !     ! inisialisasi parameter
    !     gamma   = param%gamma
    !     f0      = param%f0
    !     x0t     = param%x0t
    !     xnt     = param%xnt
    !     x0      = param%x0
    !     xn      = param%xn
    !     t0      = param%t0
    !     tm      = param%tm
    !     n       = param%n
    !     m       = param%m
    !     name_output = param%name_output

    !     ! Menghitung jarak antar variabel x dan t, dan inisitalisasi matriks psi
    !     hx = (xn - x0)/DBLE(n)
    !     ht = (tm - t0)/DBLE(m)
    !     allocate(psi(0:n, 0:m+1))
    !     psi = 0.d0

    !     ! Nilai awal psi(x,0) saat t = 0
    !     do i = 0, n
    !         psi(i,0) = f0
    !     end do
    !     ! Kondisi batas psi(0,t) dan psi(n,t)
    !     do j = 0, m
    !         psi(0,j) = x0t
    !         psi(n,j) = xnt
    !     end do

    !     ! Menentukan nilai psi(x,t)
    !     do j = 0, m
    !     ! do
    !         ! j = 1, m
    !         ! allocate(psi(0:n,0:j))
    !         do i = 1, n-1
    !             psi(i,j+1) = -func(i,j)*gamma*ht + psi(i,j) + &
    !                         (gamma*ht/(hx**2.d0))*(psi(i+1,j) - 2.d0*psi(i,j) + psi(i-1,j))
    !         end do
    !         if (j >= 1) then
    !             write(*,*) "error : ", abs(sum(psi(:,j))/DBLE(n) - sum(psi(:,j-1))/DBLE(n))
    !         end if
    !     end do

    !     ! Menuliskan hasil ke dalam file
    !     open(unit = 13, file = name_output, action='write')
    !     write(13, '(A25, A, A25, A, A25)') "Posisi", ",", "Waktu", ",", "Suhu"
    !     do j = 0, m
    !         do i = 0, n
    !             write(13, '(F25.15, A, F25.15, A, F25.15)') hx*dble(i), ",", ht*dble(j),&
    !             ",", psi(i,j)
    !         end do
    !     end do
    !     close(13)
    !     deallocate(psi)
    ! end subroutine pde_parabolic


    subroutine pde_hyperbolic(func_x0, param)
        ! Asumsi -4pi rho(r) = 0
        implicit none
        real(8) :: func_x0
        type(hyperbolic) :: param
        ! banyaknya grid sumbu x
        integer, parameter :: n = 10          ! MAINKAN PARAMETER       

        real(8), dimension(0:n) :: psi, psi_prev, psi_prev_1
        real(8), dimension(0:n) :: X
        real(8) :: t
        real(8) :: hx, ht
        integer :: i, j

        real(8) :: x0, xn
        real(8) :: b
        real(8) :: v
        character(len=50) :: name

        x0      = param%x0
        xn      = param%xn
        b       = param%b
        v       = param%v
        name    = param%name

        hx = (xn - x0)/DBLE(n)
        ht = 0.00050_DBL                      ! MAINKAN PARAMETER (kecil)
        
        ! Persiapan data output
        open(unit = 13, file = name, action='write')
        write(13, '(A25, A, A25, A, A25)') "Posisi", ",", "Waktu", ",", "Fungsi"

        ! Kondisi awal
        do i = 0, n
            ! Membuat sumbu X
            X(i) = x0 + hx*DBLE(i)
            ! Membuat fungsi psi(x, t=0)
            psi_prev(i) = func_x0(X(i))
            psi_prev_1(i) = psi_prev(i)
            psi(i) = psi_prev(i)
            write(13, '(F25.15, A, F25.15, A, F25.15)') X(i), ",", 0.0_DBL,&
            ",", psi(i)
        end do

    ! Special Case saat j = 0
        j = 0
        t = ht*DBLE(j+1)
        do i = 0, n
            if ((i == 0) .or.(i == n)) then
                psi(i) = 0.0_DBL
            else
            psi(i) = b*ht + psi_prev(i) + (((v*ht)**2.0_DBL)/(2.0_DBL*hx**2.0_DBL))*&
                        (psi_prev(i+1) - 2.0_DBL*psi_prev(i) + psi_prev(i-1))
            end if
            write(13, '(F25.15, A, F25.15, A, F25.15)') X(i), ",", t,&
            ",", psi(i)

            psi_prev = psi
        end do

    ! Case saat j > 0
        do j = 1, 300                 ! MAINKAN PARAMETER (kebutuhan)
            t = ht*DBLE(j+1)
            do i = 0, n
                if ((i == 0) .or.(i == n)) then
                    psi(i) = 0.0_DBL
                else
                psi(i) = 2.0_DBL*psi_prev(i) - psi_prev_1(i) + &
                            (((v*ht)**2.0_DBL)/(2.0_DBL*hx**2.0_DBL))*&
                            (psi_prev(i+1) - 2.0_DBL*psi_prev(i) + psi_prev(i-1))
                end if
                write(13, '(F25.15, A, F25.15, A, F25.15)') X(i), ",", t,&
                ",", psi(i)

                psi_prev_1 = psi_prev
                psi_prev = psi
            end do
        end do
        write(*,'(2A)') "[INFO] File berhasil di tulis ke dalam ", name
        
        close(13)
    end subroutine pde_hyperbolic


    ! --------------------------------------------------------------------------
    !                           Analysis Section                               |
    ! --------------------------------------------------------------------------
    subroutine relatif_semu_2d(matriks_prev, matriks_now, error)
        implicit none
        real(8), dimension(:,:), intent(in) :: matriks_prev, matriks_now
        real(8), intent(out) :: error
        real(8), allocatable, dimension(:,:) :: selisih
        integer :: i, j

        allocate(selisih(ubound(matriks_prev,1),ubound(matriks_prev,2)))
        do i = 1, ubound(matriks_prev,1)
            do j = 1, ubound(matriks_prev,2)
                selisih(i,j) = abs((matriks_prev(i,j)-matriks_now(i,j))&
                                /matriks_now(i,j))
            end do
        end do
        error = maxval(selisih)
    end subroutine relatif_semu_2d
end module pde