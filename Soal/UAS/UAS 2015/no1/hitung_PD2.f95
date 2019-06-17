	program hitung_PD2
	implicit none
	integer :: N
	real*8 :: x0, y0, u0, x, y, xmin, xmax, pi=2*asin(1.d0)
	real*8, external :: f
	
	! Syarat awal
	x0 = 0.d0
	y0 = 0.5d0
	u0 = 0.d0
	
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = 0.d0
	xmax = 8 *pi
	! Jumlah titik data
	N = 10000
	
	call rungekutta4_PD2(f,x0,y0,u0,xmin,xmax,N)
	!call euler_PD2(f,x0,y0,u0,xmin,xmax,N)

	
	stop
	end program 
	
	
	! Ganti fungsi disini
	real*8 function f(x,y,u)
	implicit none
	real*8 :: x,y,u,e
		
		e=0
	
		f = -e*(y**2-1)*u -y

	return
	end