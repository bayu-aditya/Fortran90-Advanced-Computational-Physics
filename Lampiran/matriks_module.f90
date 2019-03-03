module matriks_module
    !-----------------------------------------------------------------------
    !       Author : Bayu Aditya                                           |
    !       Copyright (c) 2019                                             |
    !-----------------------------------------------------------------------
    contains
        subroutine informasi_matriks(matriks)
            ! Program
            ! =======
            !   Untuk menampilkan informasi suatu matriks, berupa ukuran baris dan ukuran kolom
            !
            ! Parameter
            ! =========
            !   Input
            !       matriks         : real, 2 dimensi
            implicit none
            real, dimension(:,:) :: matriks

            write(*,"(A, I2)") "[INFO] Ukuran Baris : ", ubound(matriks, 1) - lbound(matriks, 1) + 1
            write(*,"(A, I2)") "[INFO] Ukuran Kolom : ", ubound(matriks, 2) - lbound(matriks, 2) + 1
        end subroutine informasi_matriks

        subroutine print_matriks(matriks)
            ! Program
            ! =======
            !   untuk menampilkan matriks 2 dimensi
            !
            ! Parameter
            ! =========
            !   Input
            !       matriks         : real, 2dimensi, matriks yang akan ditampilkan
            implicit none
            real, dimension(:,:), intent(in) :: matriks
            integer :: i, j
        
            do i = lbound(matriks, 1), ubound(matriks, 1)
                write(*,*) (matriks(i, j), j = lbound(matriks, 2), ubound(matriks, 2))
            end do
        end subroutine print_matriks

        subroutine tukar_baris(matriks, baris_asal, baris_tujuan)
            ! Program
            ! =======
            !   Untuk menukar baris matriks antara "baris_asal" dan "baris_tujuan".
            !   baris_asal dan baris_tujuan harus berada di dalam rentang ukuran matriks
            !
            ! Parameter
            ! =========
            !   Input / Output
            !       matriks         : real, 2dimensi, matriks yang akan ditukar barisnya
            !   Input
            !       baris_asal      : integer
            !       baris_tujuan    : integer
            implicit none
            integer, intent(in) :: baris_asal, baris_tujuan
            real, dimension(:,:), intent(inout) :: matriks
            integer :: baris_terkecil, baris_terbesar
            integer :: kolom_terkecil, kolom_terbesar 
            logical :: syarat_baris_asal
            logical :: syarat_baris_tujuan
            integer :: j
            
            ! Rentang ukuran matriks
            baris_terkecil = lbound(matriks, 1)
            baris_terbesar = ubound(matriks, 1)
            kolom_terkecil = lbound(matriks, 2)
            kolom_terbesar = ubound(matriks, 2)

            ! Pengecekan apakah baris dan tujuan berada di rentang ukuran matriks, hasil keluaran merupakan variabel "syarat"
            call area_antara(baris_terkecil, baris_asal, baris_terbesar, syarat_baris_asal)
            call area_antara(baris_terkecil, baris_tujuan, baris_terbesar, syarat_baris_tujuan)

            ! Pengecekan apakah baris dan tujuan berada di rentang ukuran matriks, jika tidak maka program dihentikan
            if ((syarat_baris_asal .eqv. .false.) .or. (syarat_baris_tujuan .eqv. .false.)) then
                write(*,"(A)") "[PERINGATAN] baris diluar jangkauan. Program dihentikan"
                stop
            else 
                ! Kondisi baris asal dan tujuan terdapat di rentang ukuran matriks
                ! Menukarkan baris matriks dari baris asal -> baris tujuan, dan sebaliknya
                do j = kolom_terkecil, kolom_terbesar
                    call swap(matriks(baris_asal, j), matriks(baris_tujuan, j))
                end do
                write(*,"(2(A, I3), A)") "[INFO] Proses Tukar Baris ", baris_asal, " dan ", baris_tujuan, " Berhasil"
            end if
        end subroutine tukar_baris

        subroutine tukar_kolom(matriks, kolom_asal, kolom_tujuan)
            ! Program
            ! =======
            !   Untuk menukar kolom matriks antara "kolom_asal" dan "kolom_tujuan".
            !   kolom_asal dan kolom_tujuan harus berada di dalam rentang ukuran matriks
            !
            ! Parameter
            ! =========
            !   Input / Output
            !       matriks         : real, 2dimensi, matriks yang akan ditukar kolomnya
            !   Input
            !       kolom_asal      : integer
            !       kolom_tujuan    : integer
            implicit none
            integer, intent(in) :: kolom_asal, kolom_tujuan
            real, dimension(:,:), intent(inout) :: matriks
            integer :: baris_terkecil, baris_terbesar
            integer :: kolom_terkecil, kolom_terbesar 
            logical :: syarat_kolom_asal
            logical :: syarat_kolom_tujuan
            integer :: i
            
            ! Rentang ukuran matriks
            baris_terkecil = lbound(matriks, 1)
            baris_terbesar = ubound(matriks, 1)
            kolom_terkecil = lbound(matriks, 2)
            kolom_terbesar = ubound(matriks, 2)

            ! Pengecekan apakah kolom dan tujuan berada di rentang ukuran matriks, hasil keluaran merupakan variabel "syarat"
            call area_antara(kolom_terkecil, kolom_asal, kolom_terbesar, syarat_kolom_asal)
            call area_antara(kolom_terkecil, kolom_tujuan, kolom_terbesar, syarat_kolom_tujuan)

            ! Pengecekan apakah kolom dan tujuan berada di rentang ukuran matriks, jika tidak maka program dihentikan
            if ((syarat_kolom_asal .eqv. .false.) .or. (syarat_kolom_tujuan .eqv. .false.)) then
                write(*,"(A)") "[PERINGATAN] kolom diluar jangkauan. Program dihentikan"
                stop
            else 
                ! Kondisi kolom asal dan tujuan terdapat di rentang ukuran matriks
                ! Menukarkan kolom matriks dari kolom asal -> kolom tujuan, dan sebaliknya
                do i = baris_terkecil, baris_terbesar
                    call swap(matriks(i, kolom_asal), matriks(i, kolom_tujuan))
                end do
                write(*,"(2(A, I3), A)") "[INFO] Proses Tukar Kolom ", kolom_asal, " dan ", kolom_tujuan, " Berhasil"
            end if
        end subroutine tukar_kolom

        subroutine area_antara(batas_bawah, nilai, batas_atas, logika)
            ! Program
            ! =======
            !   Mengecek apakah : 
            !       batas_bawah <= nilai <= batas_atas
            !
            ! Parameter
            ! =========
            !   Input 
            !       batas_bawah     : integer, batas bawah dari area
            !       nilai           : integer, nilai yang akan diuji
            !       batas_atas      : integer, batas bawah dari area
            !   Output
            !       logika          : integer, jika nilai berada di antara batas_bawah dan batas_atas, maka output bernilai .true.
            implicit none
            integer, intent(in) :: batas_bawah, nilai, batas_atas
            logical, intent(out) :: logika

            if ((batas_bawah <= nilai) .and. (nilai <= batas_atas)) then
                logika = .true.
            else 
                logika = .false.
            end if
        end subroutine area_antara

        subroutine swap(a, b)
            ! Program
            ! =======
            !   menukarkan nilai a dan b
            !
            ! Parameter
            ! =========
            !   Input / Output
            !       a           : real
            !       b           : real
            implicit none
            real, intent(inout) :: a, b
            real :: temp
        
            temp = a
            a = b
            b = temp
        end subroutine
end module matriks_module