	subroutine rungekutta4_PD2(f,x0_,y0_,u0_,xmin_,xmax_,N_)
	implicit none
	integer, intent(in) :: N_
	real*8 :: x0_,y0_,u0_,xmin_,xmax_
	
	integer :: i,N
	real*8 :: x0, y0,u0, x, y, u, xmin, xmax, h, f0, f1, f2, f3, u1, u2, u3
	real*8, external :: f
	
	! Syarat awal
	x0 = x0_
	y0 = y0_	
	u0 = u0_
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = xmin_
	xmax = xmax_
	! Jumlah titik data
	N = N_
	
	h = (xmax-xmin)/real(N,8) ! Interval
		
	open(unit=10, file="hasil_PD2_RK4_xy.txt", status="replace", action="write")	
	open(unit=20, file="hasil_PD2_RK4_xu.txt", status="replace", action="write")	

	do i=0,N
		x = xmin+real(i,8)*h
		if (i==0) then
			y=y0
			u=u0
		else
			f0 = f(x0,y0, u0)
			u1 = u0+0.5*h*f0
			
			f1 = f(x0+0.5*h, y0+0.5*h*u0,u1)
			u2 = u0+0.5*h*f1

			f2 = f(x0+0.5*h, y0+0.5*h*u1,u2)
			u3 = u0+h*f2
			
			f3 = f(x0+h, y0+h*u2, u3)
			
			u  = u0+h*(f0+2.d0*f1+2.d0*f2+f3)/6.d0
			y  = y0+h*(u0+2.d0*u1+2.d0*u2+u3)/6.d0
		end if
	
		write(10,*) x,"                       ",y
		write(20,*) x,"                       ",u

		x0 = x
		y0 = y
		u0 = u
		
	end do
	
	close(10)
	close(20)

	
	write(*,*) "Perhitungan Selesai"
	write(*,*) "Silakan cek file : hasil_PD2_RK4_xy.txt dan hasil_PD2_RK4_xu.txt"

	return
	end subroutine  