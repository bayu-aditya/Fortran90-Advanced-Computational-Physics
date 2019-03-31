# Sistem Persamaan Linear

## Gauss Jordan
```
call gauss_jordan(matriksA, matriksB ,x)
```
**Args :**
- ```matriksA``` : **(intent : inout), real, 2 dimensi.** Koefisien setiap variabel.
- ```matriksB``` : **(intent : inout), real, 1 dimensi.** Konstanta persamaan.
- ```x``` : **(intent : out), real, 1 dimensi.** Hasil dari perhitungan gauss jordan.

## LU Decomposition
```
call LU_decomposition(matriksA, matriksL, matriksU)
```
**Args :**
- ```matriksA``` : **intent(in), real, 2 dimensi.** Koefisien setiap variabel.
- ```matriksL``` : **intent(out), real, 2 dimensi.** Matriks segitiga bawah.
- ```matriksU``` : **intent(out), real, 2 dimensi.** Matriks segitiga atas.

```
call substitusi_LU_decomp(matriksL, matriksU, matriksB, matriksX)
```
**Args :**
- ```matriksL``` : **intent(in), real, 2 dimensi.** Matriks segitiga bawah.
- ```matriksU``` : **intent(in), real, 2 dimensi.** Matriks segitiga atas.
- ```matriksB``` : **intent(in), real, 1 dimensi.** Konstanta persamaan.
- ```matriksX``` : **intent(out), real, 1 dimensi.** Hasil penyelesaian dari sistem persamaan.

## Iterasi Jacobi
```
call iterasi_jacobi(matriksA, matriksB, x)
```
**Args :**
- ```matriksA``` : **intent(in), real, 2 dimensi.** Koefisien setiap variabel.
- ```matriksB``` : **intent(in), real, 1 dimensi.** Konstanta persamaan.
- ```x``` : **intent(out), real, 1 dimensi.** Hasil penyelesaian dari sistem persamaan.