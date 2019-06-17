	subroutine rungekutta4_PD1(f,x0_,y0_,xmin_,xmax_,N_)
	implicit none
	integer, intent(in) :: N_
	real*8 :: x0_,y0_,xmin_,xmax_
	
	integer :: i,N
	real*8 :: x0, y0, x, y, xmin, xmax, h, f0, f1, f2, f3
	real*8, external :: f
	
	! Syarat awal
	x0 = x0_
	y0 = y0_	
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = xmin_
	xmax = xmax_
	! Jumlah titik data
	N = N_
	
	h = (xmax-xmin)/real(N,8) ! Interval
		
	open(unit=12, file="hasil_PD1_RK4.txt", status="replace", action="write")	

	do i=0,N
		x = xmin+real(i,8)*h
		if (i==0) then
			y=y0
		else
			f0 = f(x0,y0) 
			f1 = f(x0+0.5*h, y0+0.5*h*f0)
			f2 = f(x0+0.5*h, y0+0.5*h*f1)
			f3 = f(x0+h, y0+h*f2)
			
			y  = y0+h*(f0+2.d0*f1+2.d0*f2+f3)/6.d0
		end if
	
		write(12,*) x,"                       ",y
		x0 = x
		y0 = y
	end do
	
	close(12)
	
	write(*,*) "Perhitungan Selesai"
	write(*,*) "Silakan cek file : hasil_PD1_RK4.txt"

	return
	end subroutine  