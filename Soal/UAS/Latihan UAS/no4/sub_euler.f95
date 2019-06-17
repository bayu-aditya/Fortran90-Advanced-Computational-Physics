	subroutine euler(f,x0_,y0_,xmin_,xmax_,N_)
	implicit none
	integer, intent(in) :: N_
	real*8 :: x0_,y0_,xmin_,xmax_
	
	integer :: i,N
	real*8 :: x0, y0, x, y, xmin, xmax, h, fxy
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
		
	open(unit=12, file="hasil_euler.txt", status="replace", action="write")	

	do i=0,N
		x = xmin+real(i,8)*h
		if (i==0) then
			y=y0
		else	
			fxy = f(x0,y0)
			y  = y0+h*fxy
		end if
	
		write(12,*) x,"                       ",y
		x0 = x
		y0 = y
	end do
	
	close(12)
	
	write(*,*) "Perhitungan Selesai"
	write(*,*) "Silakan cek file : hasil_euler.txt"

	return
	end subroutine  