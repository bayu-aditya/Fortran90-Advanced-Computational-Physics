	real*8 function simpson(f,xmin_,xmax_,N_) result(z)
	implicit none
	real*8 :: xmin_,xmax_
	integer :: N_
	integer :: i,N
	real*8 :: xmin,xmax,x(0:N_),w(0:N_),h,hasil
	real*8 ,external :: f
	N=N_
	xmin=xmin_
	xmax=xmax_
	h=(xmax-xmin)/real(N,8)
	
	!Tabulasi harga-harga x(i)
	x(0)=xmin
	x(N)=xmax
	do i=1,N-1
		x(i)=x(0)+real(i,8)*h
	end do
	
	!Tabulasi harga pemberat Simpson w(i)
	w(0)=h/3.d0
	w(N)=h/3.d0
	do i=1,N-1,2
		w(i)=4.d0*h/3.d0
	end do
	do i=2,N-2,2
		w(i)=2.d0*h/3.d0
	end do
	
	hasil=0.d0
	do i=0,N
		hasil=hasil+f(x(i))*w(i)
	end do
	
	z=hasil
	return
	
	end function