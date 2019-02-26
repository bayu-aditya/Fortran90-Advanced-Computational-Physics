module sub_program
!-----------------------------------------------------------------------
!       Author : Bayu Aditya                                           |
!       Copyright (c) 2019                                             |
!-----------------------------------------------------------------------
    contains
    subroutine swap(a,b)

        real, intent(inout) :: a, b
        temp = a
        a = b
        b = temp
    end subroutine swap
end module sub_program