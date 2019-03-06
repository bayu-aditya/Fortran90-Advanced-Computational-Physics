! Author : Bayu Aditya
! Copyright (c) 2019

module nonlinear_equation

    implicit none
    real, parameter, private :: tolerance = 0.00001
    
    contains
        subroutine bisection(func, batas_bawah, batas_atas, x)
            implicit none
            real :: func                            ! fungsi yang akan dikerjakan
            real, intent(in) :: batas_atas          ! batas nilai atas
            real, intent(in) :: batas_bawah         ! batas nilai bawah
            real, intent(out) :: x                  ! hasil x saat f(x) ~ 0
            real :: tol_i                           ! nilai toleransi iterasi ke - i
            real :: a_temp, b_temp                  ! batas dummy
            real :: x_old                           ! nilai x untuk iterasi sebelumnya
            integer :: i                            ! indeks iterasi looping
            call header(1)                          ! 1 merupakan code Bisection

            ! Proses memasukkan nilai dan batas awal
            a_temp = batas_bawah
            b_temp = batas_atas
            x_old = (b_temp + a_temp)/2.0
            
            ! Iterasi ke - 0
            i = 0
            write (*,20) "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x", "Toleransi"
            20 format (A7, 5(A20))
            write (*,10) i, a_temp, b_temp, x_old, func(x_old)
            10 format (I7, 4(F20.10))

            ! Kondisi apabila tidak terdapat solusi atau lebih dari satu solusi di batas a dan b
            if (func(a_temp)*func(b_temp) > 0) then
                do
                    write (*,"(A,F10.3, A, F10.3)") "tidak ada solusi atau lebih dari 1 solusi di daerah ", a_temp," hingga", b_temp
                    write (*,"(A)") "Masukkan batas fungsi yang baru (format : batas_kiri, batas_kanan)"
                    read (*,*) a_temp, b_temp
                    
                    if (func(a_temp)*func(b_temp) < 0) then
                        i = 0
                        write (*,20) "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x", "Toleransi"
                        write (*,10) i, a_temp, b_temp, x_old, func(x_old)
                        exit
                    end if
                
                end do
            end if
            
            ! Kondisi apabila terdapat 1 solusi di antara a dan b
            do
                i = i + 1

                ! Kondisi saat nilai x iterasi ke-i sudah merupakan nilai exact
                if (func(x_old) == 0.0) then
                    write (*,*) "solusi ", x_old
                    exit
                end if
                
                ! UPDATE NILAI BATAS
                ! Kondisi saat solusi : a < solusi < x-iterasi-sebelumnya
                if (func(a_temp)*func(x_old) < 0) then
                    b_temp = x_old
                ! Kondisi saat solusi : x-iterasi-sebelumnya < solusi < b
                else
                    a_temp = x_old
                end if

                ! Update nilai x dari batas yang baru
                x = (a_temp + b_temp)/2.0
                
                ! Nilai kesalahan relatif semu untuk iterasi ke-i
                tol_i = abs((x_old - x)/x)
                write (*,30) i, a_temp, b_temp, x, func(x), tol_i
                30 format (I7, 5(F20.10))

                ! Kondisi saat kesalahan relatif semu < toleransi yang ditentukan
                if (tol_i < tolerance) then
                    write (*,*) "Sudah mencapai batas toleransi"
                    exit
                end if

                ! Update nilai x dari iterasi ke-i
                x_old = x
            end do
        end subroutine bisection

        subroutine header(metode)
            implicit none
            integer, intent(in) :: metode

            write (*,*) "----------------------------------------------------------------------------------------------------------"
            if (metode == 1) then
                write (*,*) "|      Metode : Bisection"
            end if
            write (*,*) "|          Author : Bayu Aditya"
            write (*,*) "|          Copyright (c) 2019"
            write (*,*) "----------------------------------------------------------------------------------------------------------"
        end subroutine


end module nonlinear_equation