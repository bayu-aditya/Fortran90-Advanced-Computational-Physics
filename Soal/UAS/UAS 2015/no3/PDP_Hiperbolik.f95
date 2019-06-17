	program PDP_Hiperbolik
	implicit none
	real*8, parameter :: pi = 2.d0*asin(1.d0)
	real*8, external :: rho
	real*8, allocatable :: x(:), b(:), t(:), psi(:,:)
	real*8 :: hx, ht, koef_ddt,c,alpha,beta,gamma,lambda
	integer :: i, j, Nx, Nt

	! ingat : - (1/c2) d2/dt2
	! 		  - koef_ddt d2/dt2
	koef_ddt = 	0.3*0.1		!###### GANTI WEY ######!
	
	c = sqrt(1/koef_ddt)	!nilai c	!###### GANTI WEY ######!
	
	do
		write(*,*) "Masukkan nilai Nx : "
		read(*,*) Nx
		write(*,*) "Masukkan nilai Nt : "
		read(*,*) Nt
	
		! alokasi memori
		allocate(x(0:Nx))
		allocate(b(0:Nx))
		allocate(t(0:Nt))
		allocate(psi(0:Nx, 0:Nt))
		
		x(0)  = 0.d0		!xmin	!###### GANTI WEY ######!
		x(Nx) = 200.d0		!xmax	!###### GANTI WEY ######!
		t(0)  = 0.d0		!tmin	!###### GANTI WEY ######!
		t(Nt) = 5.d0		!tmax	!###### GANTI WEY ######!
		
		hx = (x(Nx) - x(0))/Nx
		ht = (t(Nt) - t(0))/Nt
		
		lambda = c*ht/hx
		write(*,*) "Lambda = ", lambda
		
		if (lambda<=1.d0) then
			write(*,*) "Syarat stabilitas telah terpenuhi. Sedang mencari solusi PDP hiperbolik"
			exit
		end if
		
		write(*,*) "Syarat stabilitas belum terpenuhi"

	deallocate(x)
	deallocate(b)
	deallocate(t)
	deallocate(psi)
		
	end do
	
	! Mengisi x dan t
	do i = 1, Nx-1
		x(i) = x(i-1)+hx
	end do
	do i = 1, Nt-1
		t(i) = t(i-1)+ht
	end do
	
	! Syarat batas
	psi(:,:) = 0.d0
	psi(0,:) = 0.d0		!nilai psi di batas 0	!###### GANTI WEY ######!
	psi(Nx,:) = 0.d0	!nilai psi di batas max	!###### GANTI WEY ######!
	do i=1,Nx-1
		psi(i,0) = 110*sin(pi*x(i)/200)	!nilai psi diantara batas 	!###### GANTI WEY ######!
	end do
	b(:) = 0.d0		!turunan terhadap t	!###### GANTI WEY ######!
	
	! Perhitungan
	do j=0, Nt-1
		do i=1,Nx-1
			if (j==0) then
				alpha    = 2.d0*(c**2)*(ht**2)*pi*rho(x(i), t(0))
				beta     = b(i)*ht
				gamma    = 0.5d0*(c*ht/hx)**2
				psi(i,1) = alpha + beta + psi(i,0) + gamma*(psi(i+1,0)-2*psi(i,0)+psi(i-1,0))
			
			else
				alpha    = 4.d0*(c**2)*(ht**2)*pi*rho(x(i), t(j))
				beta     = 2.d0*psi(i,j) - psi(i,j-1)
				gamma    = (c*ht/hx)**2
				psi(i,j+1) = alpha + beta + gamma*(psi(i+1,j)-2*psi(i,j)+psi(i-1,j))		
			end if
		end do
	end do
	
	open(unit=25, file = 'PDP_Hiperbolik.dat', status='unknown')
	do i=0,Nx
		do j=0,Nt
			write(25,*) x(i),"			",t(j),"			",psi(i,j)
		end do
		write(25,*)
	end do
	close(25)
	
	
	deallocate(x)
	deallocate(b)
	deallocate(t)
	deallocate(psi)
	
	end program

	! Definisi fungsi rho
	real*8 function rho(x,y) result(z)
	implicit none
	real*8, intent(in) :: x,y
	real*8 :: func
	real*8, parameter :: pi=2.d0*asin(1.d0)
		
		func =0		!###### GANTI WEY ######!
		
		z = -func/(4.d0*pi)
		
	end function
	