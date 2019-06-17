	program hitung_PD2
	implicit none
	integer :: N
	real*8 :: x0, y0, u0, x, y, xmin, xmax
	real*8, external :: f
	
	! Syarat awal
	x0 = 0.d0
	y0 = 1.d0
	u0 = 0.d0
	
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = 0.d0
	xmax = 2.d0
	! Jumlah titik data
	N = 100
	
	call rungekutta4_PD2(f,x0,y0,u0,xmin,xmax,N)
	!call euler_PD2(f,x0,y0,u0,xmin,xmax,N)

	
	stop
	end program 
	
	
	! Ganti fungsi disini
	real*8 function f(x,y,u)
	implicit none
	real*8 :: x,y,u

	f = 5*x - 5*y - 4*x*u
	
	return
	end