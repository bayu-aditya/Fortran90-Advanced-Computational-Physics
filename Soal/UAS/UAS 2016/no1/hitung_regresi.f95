	program hitung_regresi
	implicit none
	integer :: N,m,i
	real*8, allocatable :: xd(:), fd(:),a(:)
	
	character*50 :: inputfile

! Proses pembacaan data masukan
	inputfile = "data_regresi.txt"
	open(unit=10, file=inputfile, status="old", action="read")
	read(10,*) N	
	allocate(xd(N))
	allocate(fd(N))
		
	read(10,*)
	do i=1,N
		read(10,*) xd(i), fd(i)
	end do
	close(10)

! Menampilkan hasil pembacaan data masukan	
	write(*,*) "Hasil baca file input"
	do i=1,N
		write(*,*) xd(i)," ",fd(i)
	end do
	
! Menentukan orde polinomial 
	write(*,*)
	write(*,*) "Orde polinomial yang diinginkan : "
	read(*,*)	m
	allocate(a(m+1))

	call regresi(N,xd,fd,m,a)
	
	! Tampilkan isi a
	write(*,*)
	write(*,*) "Koefisien Polinomial:"
	do i=1,m+1
		write(*,*) "a(",i-1,")= ",a(i)
	end do

 
	deallocate(xd)
	deallocate(fd)
	stop
	
	end program 

	subroutine regresi(N_,xd_,fd_,m_,a_)
	implicit none
	integer, intent(in) :: N_,m_
	real*8 :: xd_(N_), fd_(N_),a_(m_+1)
	integer :: i,j,k,N,m,p,s
	real*8, allocatable :: xd(:),fd(:),C(:,:), B_(:), B(:),A(:,:),X_(:),At(:),Ab(:,:),Bb(:)
	real*8 :: Bt, x, y, dx, xmin, xmax, sum
	
	N=N_
	m=m_
	
	
! Mengalokasikan matriks
	allocate(xd(N))
	allocate(fd(N))
	allocate(C(0:m,0:m))
	allocate(B_(0:m))
	allocate(A(m+1,m+1))
	allocate(X_(m+1))
	allocate(At(m+1))
	allocate(Ab(m+1,m+1))
	allocate(Bb(m+1))
	allocate(B(m+1))

	xd = xd_
	fd = fd_
	
	
! Proses penghitungan matriks C A = B
	do k=0,m
		do j=0,m
			sum = 0
			do i=1,N
				sum = sum + (xd(i)**(j+k))
			end do
			C(k,j)=sum
		end do
		
		sum = 0
		do i=1,N
			sum = sum + (fd(i)*(xd(i)**k))
		end do
		B_(k) = sum
	end do

! Menampilkan hasil penghitungan matriks C dan B
	write(*,*) 
	write(*,*) "Matriks C :"
	do i=0,m
		write(*,*) (C(i,j), j = 0,m)
	end do
	write(*,*)
	write(*,*) "Matriks B :"
	do i=0,m
		write(*,*) B_(i)
	end do	

	
!======Eliminasi Gauss=============

	! Perlu penyesuaian indeks matriks hasil 
	! LeastSquare agar dapat masuk ke program
	! EliminasiGauss
	
	! Copy C ke A
	do i=0,m
		do j=0,m
		A(i+1,j+1) = C(i,j)
		end do
	end do
	! Copy B_ ke B
	do i=0,m
		B(i+1) = B_(i)
	end do
		
	N=m+1
	Ab=A
	Bb=B

! Proses triangulasi matrix A
	do k=1,N-1
		do i=k+1,N
			do j=k,N
				Ab(i,j)=A(i,j)-(A(i,k)/A(k,k))*A(k,j)
			end do
			Bb(i)=B(i)-(A(i,k)/A(k,k))*B(k)
		end do
		! Cek apakah A(k,k) berharga nol
		if (Ab(k+1,k+1)==0.d0) then
		! Tukar isi matrix A baris ke-k isi baris di bawahnya
			At(:)=Ab(k+1,:)
			Bt=Bb(k+1)
			Ab(k+1,:)=Ab(k+2,:)
			Ab(k+2,:)=At(:)
			Bb(k+1)=Bb(k+2)
			Bb(k+2)=Bt
		end if
		A=Ab
		B=Bb
	end do
			
	! Substitusi mundur	
	X_(N)=B(N)/A(N,N)
	do j=1,N-1
		sum=0.d0
		do k=N-j+1,N
			sum=sum+A(N-j,k)*X_(k)
		end do
		X_(N-j)=(B(N-j)-sum)/A(N-j,N-j)
	end do
	
	s=0
	write(*,*)
	write(*,*) "Lakukan estimasi titik data?"
	write(*,*) "[0 : Tidak, 1 : Ya]"
	read(*,*) s
	
	if (s==1) then
		open(unit=11, file="hasil_regresi.txt", status="replace", action="write")
		write(*,*)
		write(*,*)	"=======Estimasi Titik Data======="
		write(*,*) "Masukkan batas awal :"
		read(*,*) xmin
		write(*,*) "Masukkan batas akhir :"
		read(*,*) xmax
		write(*,*) "Masukkan jumlah titik :"
		read(*,*) p
		
		p=p-1
		dx=(xmax-xmin)/ real(p, 8)

		do k=0,p
			x = xmin + real(k, 8)*dx
			y = 0
			do i=0,m
				y = y + (X_(i+1))*(x**i)
			end do
			write(11,*) x,"			", y
		end do
		
		close(11)
		
		write(*,*)
		write(*,*) "~~Proses Selesai~~"
		write(*,*) "Silakan cek titik data pada : hasil_regresi.txt"
		a_ = X_
		return
	end if
	a_ = X_
	return
	
	deallocate(xd)
	deallocate(fd)
	deallocate(C)
	deallocate(B)
	deallocate(A)
	deallocate(X_)
	deallocate(At)
	deallocate(Ab)
	deallocate(Bb)
	deallocate(xd)
	deallocate(fd)
	return
	
	end subroutine