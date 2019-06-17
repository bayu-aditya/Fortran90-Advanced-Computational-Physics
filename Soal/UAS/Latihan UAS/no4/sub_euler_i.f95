	subroutine euler_i(f,x0_,y0_,xmin_,xmax_,N_) ! or midpoint2 method
	implicit none
	integer, intent(in) :: N_
	real*8 :: x0_,y0_,xmin_,xmax_
	
	integer :: i,N
	real*8 :: x0, y0, x, y, xmin, xmax, h, f1,f2
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
		
	open(unit=12, file="hasil_euler_i.txt", status="replace", action="write")	

	do i=0,N
		x = xmin+real(i,8)*h
		if (i==0) then
			y=y0
		else	
			f1 = f(x0,y0)
			f2 = f(x0+h,y0+h*f1)			
			y  = y0+0.5*h*(f1+f2)
		end if
	
		write(12,*) x,"                       ",y
		x0 = x
		y0 = y
	end do
	
	close(12)
	
	write(*,*) "Perhitungan Selesai"
	write(*,*) "Silakan cek file : hasil_euler_i.txt"

	return
	end subroutine  