	program hitung_PD2
	implicit none
	integer :: N
	real*8 :: x0, y0, u0, x, y, xmin, xmax
	real*8, external :: f
	
	! Syarat awal
	x0 = 0.d0
	y0 = 0.d0
	u0 = 0.d0
	
	! Rentang daerah x yang akan dicari y(x) nya
	xmin = 0.d0
	xmax = 10.d0
	! Jumlah titik data
	N = 1000
	
	call rungekutta4_PD2(f,x0,y0,u0,xmin,xmax,N)
	!call euler_PD2(f,x0,y0,u0,xmin,xmax,N)

	
	stop
	end program 
	
	
	! Ganti fungsi disini
	real*8 function f(x,y,u)
	implicit none
	real*8 :: x,y,u
	real*8 :: E,L,C
	
	E =sin(1.87*x)
	L = 1
	C = 0.25
	
	f = E/L - (y/(L*C))
	
	return
	end