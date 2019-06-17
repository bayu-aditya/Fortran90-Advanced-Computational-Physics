	real*8 function f(x) result(z)
	implicit none
	real*8, intent(in) :: x
	z=    0.02530322108866265*x**5-0.3893202359473113*x**4&
	&+0.5619212261183752*x**3+5.418158288020924*x**2+27.904938153661575*x+29.398284847029764
  
	return
	end function	
	
	program main
	implicit none
	integer:: N
	integer :: i
	real*8 :: xmin,xmax,hasil
	
	real*8, external :: f,simpson,trapezoid
	
	xmin=0.d0
	xmax=10.d0
	N=100
	
	hasil = simpson(f,xmin,xmax,N)

	
	!Tulis hasil integrasi
	write(*,*) "Panjang lintasan lurus =",hasil
	write(*,*) "Kecepatan rata-rata =",hasil/10

	hasil = trapezoid(f,xmin,xmax,N)

	
	!Tulis hasil integrasi
	write(*,*) "Panjang lintasan lurus =",hasil
	write(*,*) "Kecepatan rata-rata =",hasil/10
	
	stop
	
	end program