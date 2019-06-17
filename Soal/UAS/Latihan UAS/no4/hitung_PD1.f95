	program hitung_PD1
	implicit none
	integer :: N
	real*8 :: x0, y0, x, y, xmin, xmax
	real*8, external :: f
	
	! Syarat awal
	x0 = 0.d0
	y0 = 1.d0
	
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = 0.d0
	xmax = 3.d0
	! Jumlah titik data
	N = 1000
	
	call rungekutta4_PD1(f,x0,y0,xmin,xmax,N)
	!call euler(f,x0,y0,xmin,xmax,N)
	!call euler_m(f,x0,y0,xmin,xmax,N)
	!call euler_i(f,x0,y0,xmin,xmax,N)

	
	stop
	end program 
	
		real*8 function f(x,y)
	implicit none
	real*8 :: x,y
	
	f = (5*x) - (4*x*y)
	
	return
	end
