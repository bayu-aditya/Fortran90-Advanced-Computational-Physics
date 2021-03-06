# Fitting Data

## Interpolasi Lagrange
```
call lagrange_plot(dataX, dataY, name_output)
```
**Args :**
- ``dataX`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu X
- ``dataY`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu Y
- ``name_output`` : **intent(in), character.** Nama file untuk output data plot

```
lagrange(x, dataX, dataY)
```
**Args :**
- ``x`` : **intent(in), double precision.** Titik di sumbu X
- ``dataX`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu X
- ``dataY`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu Y

**Return :**
- Hasil fungsi dari interpolasi lagrange di titik X , atau f(X)

## Least Square
```
call least_square_plot(dataX, dataY, orde, name_output)
```
**args :**
- ``dataX`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu X
- ``dataY`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu Y
- ``orde`` : **intent(in), integer.** Orde dari Least Square
- ``name_output`` : **intent(in), character.** Nama file untuk output data plot
```
call koefisien_least_square(x, dataX, dataY, orde, koef_polinomial)
```
**Args :**
- ``x`` : **intent(in), double precision.** Titik di sumbu X
- ``dataX`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu X
- ``dataY`` : **intent(in), double precision, 1 dimensi.** Data untuk sumbu Y
- ``orde`` : **intent(in), integer.** Orde dari Least Square
- ``koef_polinomial`` : **intent(out), double precision, 1 dimensi.** Koefisien Polinomal dari data yang diinput
```
least_square(x, dataX, dataY, orde)
```
**Args :**
- ``x`` : **intent(in), double precision.** Titik di sumbu X
- ``koef_polinomial`` : **intent(in), double precision, 1 dimensi.** Koefisien dari least square
## Lampiran
