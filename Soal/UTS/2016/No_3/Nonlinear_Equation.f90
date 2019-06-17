! Author : Bayu Aditya
! Copyright (c) 2019

module nonlinear_equation
    
    implicit none
    INTEGER, PARAMETER :: dpr=KIND(1.0D0)
    real(8), parameter, private :: tolerance_double = 0.00000000000001_dpr
    real, parameter, private :: tolerance = 0.00001

    contains
        subroutine bisection(func_dp, batas_bawah_dp, batas_atas_dp, x_dp)
            implicit none
            real(8) :: func_dp                          ! fungsi yang akan dikerjakan
            real(8), intent(in) :: batas_atas_dp        ! batas nilai atas
            real(8), intent(in) :: batas_bawah_dp       ! batas nilai bawah
            real(8), intent(out) :: x_dp                ! hasil x saat f(x) ~ 0
            real(8) :: tol_i                            ! nilai toleransi iterasi ke - i
            real(8) :: a_temp, b_temp                   ! batas dummy
            real(8) :: x_old                            ! nilai x untuk iterasi sebelumnya
            integer :: i                                ! indeks iterasi looping
            call header(1)                              ! 1 merupakan code Bisection

            ! Proses memasukkan nilai dan batas awal
            a_temp = batas_bawah_dp
            b_temp = batas_atas_dp
            x_old = (b_temp + a_temp)/2.0
            
            ! Iterasi ke - 0
            i = 0
            write (*,'(A10, 5(A25))') "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x", "Toleransi"
            write (*,'(I10, 4(F25.15))') i, a_temp, b_temp, x_old, func_dp(x_old)

            ! Kondisi apabila tidak terdapat solusi atau lebih dari satu solusi di batas a dan b
            if (func_dp(a_temp)*func_dp(b_temp) > 0) then
                do
                    write (*,"(A,F10.3, A, F10.3)") "tidak ada solusi atau lebih dari 1 solusi di daerah ", a_temp," hingga", b_temp
                    write (*,"(A)") "Masukkan batas fungsi yang baru (format : batas_kiri, batas_kanan)"
                    read (*,*) a_temp, b_temp
                    
                    if (func_dp(a_temp)*func_dp(b_temp) < 0) then
                        i = 0
                        write (*,"(A10, 4(A25))") "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x"
                        write (*,'(I10, 4(F25.15))') i, a_temp, b_temp, x_old, func_dp(x_old)
                        exit
                    end if
                
                end do
            end if
            
            ! Kondisi apabila terdapat 1 solusi di antara a dan b
            do
                i = i + 1

                ! Kondisi saat nilai x iterasi ke-i sudah merupakan nilai exact
                if (func_dp(x_old) == 0.0) then
                    write (*,*) "solusi ", x_old
                    exit
                end if
                
                ! UPDATE NILAI BATAS
                ! Kondisi saat solusi : a < solusi < x-iterasi-sebelumnya
                if (func_dp(a_temp)*func_dp(x_old) < 0) then
                    b_temp = x_old
                ! Kondisi saat solusi : x-iterasi-sebelumnya < solusi < b
                else
                    a_temp = x_old
                end if

                ! Update nilai x dari batas yang baru
                x_dp = (a_temp + b_temp)/2.0
                
                ! Nilai kesalahan relatif semu untuk iterasi ke-i
                tol_i = abs((x_old - x_dp) / x_dp)
                write (*,'(I10, 5(F25.15))') i, a_temp, b_temp, x_dp, func_dp(x_dp), tol_i

                ! Kondisi saat kesalahan relatif semu < toleransi yang ditentukan
                if (tol_i < tolerance_double) then
                    write (*,*) "Sudah mencapai batas toleransi"
                    exit
                end if

                ! Update nilai x dari iterasi ke-i
                x_old = x_dp
            end do
        end subroutine bisection


        subroutine false_position(func_dp, batas_bawah_dp, batas_atas_dp, hasil_dp)
            implicit none
            real(8) :: func_dp
            real(8), intent(in) :: batas_bawah_dp, batas_atas_dp
            real(8), intent(out) :: hasil_dp
            real(8) :: a_temp, b_temp
            real(8) :: x_i, x_old
            real(8) :: tol_i
            integer :: i
            call header(2)
            
            ! Iterasi untuk i = 0 (awal)
            a_temp = batas_bawah_dp
            b_temp = batas_atas_dp
            x_old = (a_temp*func_dp(b_temp) - b_temp*func_dp(a_temp)) / (func_dp(b_temp) - func_dp(a_temp))
            write (*,'(A10, 5(A25))') "Iterasi(i)", "Batas kiri", "Batas kanan", "Titik x_i", "Fungsi di x_i", "Toleransi"
            write (*,'(I10, 4(F25.15))') 0, a_temp, b_temp, x_old, func_dp(x_old)

            ! Iterasi ke - i (looping)
            i = 1
            do
                if (func_dp(a_temp)*func_dp(x_old) > 0) then
                    a_temp = x_old
                else if (func_dp(a_temp)*func_dp(x_old) < 0) then
                    b_temp = x_old
                end if
                x_i = (a_temp*func_dp(b_temp) - b_temp*func_dp(a_temp)) / (func_dp(b_temp) - func_dp(a_temp))
                tol_i = abs((x_old - x_i) / x_i)
                write (*,'(I10, 5(F25.15))') i, a_temp, b_temp, x_old, func_dp(x_old), tol_i

                ! saat kesalahan relatif semu kurang dari toleransi -> iterasi dihentikan                
                if (tol_i < tolerance_double) exit
                
                x_old = x_i
                i = i + 1
            end do
            ! nilai akar merupakan titik x ke - i
            hasil_dp = x_i
        end subroutine false_position


        subroutine newton_raphson(func_dp, func_der_dp, titik_awal_dp, hasil_dp)
            implicit none
            real(8) :: func_dp, func_der_dp
            real(8), intent(in) :: titik_awal_dp
            real(8), intent(out) :: hasil_dp
            real(8) :: x_old, x_i
            real(8) :: tol_i
            integer :: i
            call header(3)

            x_old = titik_awal_dp
            write (*,'(A10, 4(A25))') "Iterasi(i)", "Titik x_i-1", "Titik x_i", "Fungsi di x_i", "Toleransi"
            
            i = 1
            do
                x_i = x_old - (func_dp(x_old) / func_der_dp(x_old))
                tol_i = abs((x_old - x_i) / (x_i))
                
                write (*,'(I10, 4(F25.15))') i, x_old, x_i, func_dp(x_i), tol_i
                
                if (tol_i < tolerance_double) exit

                x_old = x_i
                i = i+1
            end do
            hasil_dp = x_i
        end subroutine newton_raphson


        subroutine secant(func_dp, titik_1_dp, titik_2_dp, hasil_dp)
            implicit none
            
            real(8) :: func_dp                                  ! fungsi yang akan dikerjakan
            real(8), intent(in) :: titik_1_dp, titik_2_dp       ! menentukan 2 titik x awal
            real(8), intent(out) :: hasil_dp                    ! hasil x saat f(x) ~ 0
            real(8) :: x_i_2, x_i_1                             ! titik x(i-2) dan x(i-1)
            real(8) :: x_i                                      ! titik x(i)
            real(8) :: tol_i                                    ! nilai toleransi iterasi ke-i
            integer :: i                                        ! indeks iterasi looping
            call header(4)                                      ! 2 merupakan code secant
        
            ! iterasi untuk i = 0 (awal)
            x_i_2 = titik_1_dp                         ! titik x i-2
            x_i_1 = titik_2_dp                         ! titik x i-1
            x_i = (x_i_2*func_dp(x_i_1) - x_i_1*func_dp(x_i_2)) / (func_dp(x_i_1) - func_dp(x_i_2))
            write (*,'(A10, 5(A25))') "Iterasi(i)", "Titik x_i-2", "Titik x_i-1", "Titik x_i", "Fungsi di x_i", "Toleransi"
            write (*,'(I10, 4(F25.15))') 0, x_i_2, x_i_1, x_i, func_dp(x_i)
        
            ! iterasi ke - i (looping)
            i = 1
            do
                x_i_2 = x_i_1
                x_i_1 = x_i
                x_i = (x_i_2*func_dp(x_i_1) - x_i_1*func_dp(x_i_2)) / (func_dp(x_i_1) - func_dp(x_i_2))
                tol_i = abs((x_i_1 - x_i) / (x_i))
                write (*,'(I10, 5(F25.15))') i, x_i_2, x_i_1, x_i, func_dp(x_i), tol_i
                
                ! saat kesalahan relatif semu kurang dari toleransi -> iterasi dihentikan
                if (tol_i < tolerance_double) exit
        
                i = i + 1
            end do
            ! nilai akar merupakan titik x ke - i
            hasil_dp = x_i
        end subroutine

        subroutine header(metode)
            implicit none
            integer, intent(in) :: metode

            write (*,*) "----------------------------------------------------------------------------------------------------------"
            if (metode == 1) then
                write (*,*) "|      Metode : Bisection"
            else if (metode == 2) then
                write (*,*) "|      Metode : False Position"
            else if (metode == 3) then
                write (*,*) "|      Metode : Newton Rapshon"
            else if (metode == 4) then
                write (*,*) "|      Metode : Secant"
            end if
            write (*,*) "|          Author : Bayu Aditya"
            write (*,*) "|          Copyright (c) 2019"
            write (*,*) "----------------------------------------------------------------------------------------------------------"
        end subroutine
end module nonlinear_equation