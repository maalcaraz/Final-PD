
-- Funcion que debe ser mejorada, para traer los valores que puede tomar un casillero.
valoresPosibles :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int]) -> [Int]
valoresPosibles cuadrante fila columna = valorFaltante (snd cuadrante)  `intersect` valorFaltante (snd fila)  `intersect` valorFaltante (snd columna)

-- Manda a cada valor de la lista de cuadrantes | filas | columnas a ser analizado para colocar o no un valor.
preIteracion :: [(Int, [Int])] -> Bool
preIteracion (cuad:cs) = map (iterarEnCuadrante) 

-- Funcion que por cuadrante | fila | columna va rellenando con los valores que faltan.
iterarEnCuadrante :: (Int, [Int]) -> (Int, [Int])
iterarEnCuadrante (i, l) 
					| null  vf 			= (i,l)
					| length vf == 1 	= (i, (modificarValor l (getIndexUnico l) (head vf)))
					| otherwise 		= (i,l) --iterarEnCuadrante (i+1, l)
					where vf = valorFaltante l
