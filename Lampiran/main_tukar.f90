program main_tukar
    use sub_program
    implicit none

    real :: a, b

    write(*,*) "Input Nilai"
    read(*,*) a, b

    ! Pengecekan Nilai Input sebelum diproses
    write(*,*) "Nilai sebelum diproses "
    write(*,*) " Nilai a = ", a, " nilai b = ", b
    write(*,*) " "

    ! Proses
    call swap(a,b)

    ! Melihat output setelah diproses
    write(*,*) "Nilai setelah diproses "
    write(*,*) " Nilai a = ", a, " nilai b = ", b

end program main_tukar