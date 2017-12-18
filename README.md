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
    
En las primeras tres versiones se incluyen las funciones necesarias para resolver sudokus mediante una técnica de *backtracking*:

*  Mediante una función detecta las ubicaciones vacías, y va analizando en el tablero qué valores puede tomar cada una. Para los x valores que pueda tomar crea un tablero duplicado y vuelva a llamar de manera recursiva a dicha función.


La última mencionada es una versión alternativa, que implementa la forma "intuitiva" de resolver sudokus. Utiliza la técnica de *posición única*:

* Analiza el valor que falta en cada cuadrante
* Verifica en las filas y columnas de ese cuadrante si el valor que falta puede ser ingresado.  


----
## Instrucciones de uso

> 1. Clonar el repositorio de la aplicación usando la url [Final-PD](https://github.com/maalcaraz/Final-PD.git)

2. Instalar la herramienta **stack**. Puede hacerlo siguiendo los pasos de este
[link](https://docs.haskellstack.org/en/stable/README/).

3. Una vez instalada, por consola dirigirse al path raíz del repositorio descargado       
    *cd ../Final-PD/sudoku*

4. Ejecutar el comando
     *stack ghci*
5. Escribir *main*
