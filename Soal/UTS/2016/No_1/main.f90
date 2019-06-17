program nomer_1
    use fitting_curve
    implicit none
    integer, parameter :: DBL = 8
    real(8), allocatable, dimension(:) :: waktu, ketinggian

    ! Input data dari file
    call import_data("data.dat", waktu, ketinggian)

    ! Proses Plot Fitting kuadratik (Least Square Orde 2)
    call least_square_plot(waktu, ketinggian, 2, "Orde_2.dat")

    deallocate(waktu, ketinggian)
end program nomer_1