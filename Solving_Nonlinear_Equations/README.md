# Akar Persamaan Non-linear
*Author : Bayu Aditya (2019)*

## Bisection
```
bisection(func, batas_bawah, batas_atas, x)
```
1. Argument
    - *func* : 
        - **real function**
        - Fungsi yang akan dikerjakan
    - *batas_bawah* : 
        - **real (in)**
        - batas nilai bawah 
    - *batas_atas* : 
        - **real (in)**
        - batas nilai atas
    - *x* : 
        - **real (out)**
        - hasil dari akar fungsi

2. Algoritma
    - Menentukan batas bawah ($a$) dan batas atas ($b$)
    - Menentukan nilai hasil ($x$), yaitu nilai antara $a$ dan $b$ dengan cara :
        $$x = \frac{a + b}{2}$$
    - Kondisi saat $f(a) * f(b) > 0$ :
        - Memasukkan nilai $a$ dan $b$ yang baru hingga $f(a) * f(b) < 0$
    - Melakukan looping dengan indeks-i :
        - Apabila $f(x) = 0$:
            - Maka solusinya adalah $x$
        - Apabila $f(a) * f(b) < 0$:
            - Memindahkan nilai $x$ menjadi batas atas ($b$)
        - Apabila $f(a) * f(b) > 0$:
            - Memindahkan nilai $x$ menjadi batas atas ($a$)
        - Update nilai tengah berdasarkan batas yang baru menjadi nilai $x$ dengan cara :
<img src="https://latex.codecogs.com/gif.latex?x&space;=&space;\frac{a&space;&plus;&space;b}{2}" title="x = \frac{a + b}{2}" />        - Apabila kesalahan relatif semu < toleransi yang diinginkan, maka hentikan looping.
            $$kesalahan relatif semu = \left| \frac{x_{i} - x_{i+1}}{x_{i+1}} \right|$$
