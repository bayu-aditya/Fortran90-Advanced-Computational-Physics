! Author : Bayu Aditya
! Copyright (c) 2019

module nonlinear_equation

    implicit none
    real, parameter, private :: tolerance = 0.00001
    
    contains
        subroutine bisection(func, a, b, x)
            implicit none
            real :: func
            real, intent(in) :: a, b
            real, intent(out) :: x
            real :: tol_i, a_temp, b_temp, x_old
            integer :: i
            call header(1)

            ! Inisialisasi
            a_temp = a
            b_temp = b
            x_old = (b_temp + a_temp)/2.0
            
            i = 0
            write (*,20) "Iterasi", "Titik_Kiri(A)", "Titik_Kanan(B)", "Titik_tengah(x)", "Fungsi di x", "Toleransi"
            20 format (A7, 5(A20))
            write (*,10) i, a_temp, b_temp, x_old, func(x_old)
            10 format (I7, 4(F20.10))

            if (func(a_temp)*func(b_temp) > 0) then
                write (*,*) "tidak ada solusi atau lebih dari 1 solusi di daerah a hingga b"
            else if (func(a_temp)*func(b_temp) < 0) then
                do
                    i = i + 1
                    if (func(x_old) == 0.0) then
                        write (*,*) "solusi ", x_old
                        exit
                    end if

                    if (func(a_temp)*func(x_old) < 0) then
                        b_temp = x_old
                    else
                        a_temp = x_old
                    end if
                    x = (a_temp + b_temp)/2.0
                    

                    tol_i = abs((x_old - x)/x)
                    write (*,30) i, a_temp, b_temp, x, func(x), tol_i
                    30 format (I7, 5(F20.10))

                    if (tol_i < tolerance) then
                        write (*,*) "Sudah mencapai batas toleransi"
                        exit
                    end if

                    x_old = x
                end do
            end if
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