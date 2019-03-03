program tukar_baris_matriks
!-----------------------------------------------------------------------
!       Author : Bayu Aditya                                           |
!       Copyright (c) 2019                                             |
!-----------------------------------------------------------------------
    use matriks_module

    implicit none
    real, allocatable, dimension(:,:) :: matA   ! Matriks
    integer :: baris_asal, baris_tujuan         ! tukar baris matriks
    integer :: kolom_asal, kolom_tujuan         ! tukar kolom matriks
    character(len = 90) :: nama_file            ! nama file matriks
    integer :: baris, kolom, i, j               ! variabel untuk load matriks

    ! Pengisian matriks dari file
    write(*,*) "Masukkan nama file"
    read(*,*) nama_file
    write(*,*) "Ukuran matriks"
    read(*,*) baris, kolom
 
    allocate(matA(baris, kolom))

    open(unit = 3, file = nama_file, action = 'read')
    do i = 1, baris
        read(3, *) (matA(i,j), j = 1, kolom)
    end do

    ! Menampilkan informasi dan nilai matriks awal
    call informasi_matriks(matA)
    call print_matriks(matA)

    ! Menukarkan baris matriks
    baris_asal = 1
    baris_tujuan = 3
    call tukar_baris(matA, baris_asal, baris_tujuan)

    ! Menampilkan matriks setelah tukar baris
    call print_matriks(matA)

    ! Menukarkan kolom matriks
    kolom_asal = 1
    kolom_tujuan = 3
    call tukar_kolom(matA, kolom_asal, kolom_tujuan)

    ! Menampilkan matriks setelah tukar kolom
    call print_matriks(matA)

    deallocate(matA)
end program tukar_baris_matriks