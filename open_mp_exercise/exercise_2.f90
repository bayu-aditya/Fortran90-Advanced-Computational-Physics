program do_openmp
implicit none

integer :: i

!$OMP DO
    do i = 1, 100
        write(*,*) i
    enddo
!$OMP END DO

end program do_openmp