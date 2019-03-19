module system_of_linear_equation
    implicit none

    contains
    subroutine gauss_jordan(matriksA)
        implicit none
        real, dimension(:,:), intent(in) :: matriksA
        integer :: i, j

        do i = lbound(matriksA, 1), ubound(matriksA, 1)
            write(*,*) (matriksA(i,j), j = lbound(matriksA, 2), ubound(matriksA, 2))
        end do

    end subroutine gauss_jordan
end module system_of_linear_equation