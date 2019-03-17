module header_module
    implicit none

    contains
        subroutine header(metode)
            implicit none
            integer, intent(in) :: metode

            write (*,*) "----------------------------------------------------------------------------------------------------------"
            if (metode == 1) then
                write (*,*) "|      Metode : Bisection"
            else if (metode == 2) then
                write (*,*) "|      Metode : Secant"
            end if
            write (*,*) "|          Author : Bayu Aditya"
            write (*,*) "|          Copyright (c) 2019"
            write (*,*) "----------------------------------------------------------------------------------------------------------"
        end subroutine
end module header_module