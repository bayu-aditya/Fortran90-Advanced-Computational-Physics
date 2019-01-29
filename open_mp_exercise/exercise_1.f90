program hello_world_openmp
implicit none

!$OMP PARALLEL
    write(*,*) "Hello"
    write(*,*) "Hi"
!$OMP END PARALLEL

end program hello_world_openmp