program bisection
    implicit none
    integer,external :: pengurangan_func
    integer :: nilai1, nilai2, res1, res2, res3

    nilai1 = 1000
    nilai2 = 20

    call pengurangan(nilai1, nilai2, res1)
    res2 = pengurangan_func(nilai1 + nilai2)
    !call sub2(pengurangan_func, nilai1, res3)
    write(*,*) res1
    write(*,*) res2
    write(*,*) res3
end program bisection

integer function pengurangan_func(c)
    implicit none
    integer, intent(in) :: c
    integer :: reslt
    reslt = c + 2
    pengurangan_func = reslt
end function pengurangan_func

subroutine sub2(func, a, hasil)
    implicit none
    integer :: func
    integer, intent(in) :: a
    integer, intent(out) :: hasil
    hasil = func(a)
end subroutine sub2

subroutine pengurangan(a, b, hasil)
    implicit none
    integer, intent(in) :: a, b
    integer, intent(out) :: hasil
    hasil = a-b
end subroutine pengurangan