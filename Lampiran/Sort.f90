program urut
!-----------------------------------------------------------------------
!       Author : Bayu Aditya                                           |
!       Copyright (c) 2019                                             |
!-----------------------------------------------------------------------
    implicit none
    real, dimension(10) :: test

    test = [0,5,6,2,3,0,5,6,2,10]

    call ascending(test)
    write(*,*) "Ascending", test

    call descending(test)
    write(*,*) "Descending", test

    write(*,*) shape(test)

end program urut


subroutine ascending(array)
    real, allocatable, intent(inout) :: array(:)
    integer :: flags, i, size

    size = shape(array)
    allocate(array(size))

    flags = 0
    do
        do i = 1, size-1
            if (array(i) > array(i+1)) then
                call swap(array(i), array(i+1))
                flags = flags + 1
            end if
        end do
        if (flags == 0) then
            exit
        end if
        flags = 0
    end do

    deallocate(array)
end subroutine ascending


subroutine descending(array)
    real, dimension(10), intent(inout) :: array
    integer, :: flags, i

    flags = 0
    do
        do i = 1, 9
            if (array(i) < array(i+1)) then
                call swap(array(i), array(i+1))
                flags = flags + 1
            end if
        end do
        if (flags == 0) then
            exit
        end if
        flags = 0
    end do
end subroutine descending


subroutine swap(a,b)
    implicit none
    real, intent(inout) :: a, b
    real :: temp

    temp = a
    a = b
    b = temp
end subroutine swap