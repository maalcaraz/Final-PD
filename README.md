# SUDOKU MASTER

----
## Descripción

> Sudoku Master es una aplicación para resolver sudokus 4x4, 9x9, 16x16. Posee una versión que obtiene la solución única de un tablero válido dado, y otra que
es capaz de obtener hasta 10 de las posibles.

----
## Cómo los resuelve?

> Sudoku Master incluye los siguientes módulos de resolución:

    Sudoku4x4.hs
    Sudoku9x9.hs
    Sudoku16x16.hs
    Sudoku4x4V2.hs
    
En las primeras dos versiones se incluyen las funciones necesarias para resolver sudokus mediante una técnica de *backtracking*.
*  

La última mencionada es una versión alternativa, que implementa la forma "intuitiva" de resolver sudokus. Utiliza la técnica de *posición única*:

* Analiza el valor que falta en cada cuadrante
* Verifica en las filas y columnas de ese cuadrante si el valor que falta puede ser ingresado.  


----
## Instrucciones de uso
1. 
2. 


# headers

*emphasis*

**strong**

* list


[links](http://wikipedia.org)

----
## changelog
* 17-Feb-2013 re-design

----
## Agradecimientos
* [markdown-js](https://github.com/evilstreak/markdown-js)
