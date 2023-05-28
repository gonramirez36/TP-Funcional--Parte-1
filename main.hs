data Planta = Planta
  { vida :: Int,
    cantSoles :: Int,
    poder :: Int
  }
  deriving (Show, Eq)

data Zombie = Zombie
  { nombre :: [Char],
    accesorios :: [[Char]],
    poderMordida :: Int
  }
  deriving (Show, Eq)

vidaZombie = length . nombre

data LineaDeDefensa = LineaDeDefensa
  { plantas :: [Planta],
    zombies :: [Zombie]
  }
  deriving (Show, Eq)

-- Plantas
peashooter = Planta 5 0 2

repeater = Planta 5 0 4

sunflower = Planta 7 1 0

nut = Planta 30 0 0

cactus = Planta 9 0 0

rose = Planta 2 45 7

-- Zombies
zombieBase = Zombie "Base" [] 1

zombieBaloon = Zombie "Balloon Zombie" ["Globo"] 1

zombieNewspaper = Zombie "Newspaper Zombie" ["Diario"] 2

gargantuar = Zombie "Gargantuar Hulk Smash Puny God" ["Poste de luz", "zombie enano"] 30

-- Lineas
linea1 = LineaDeDefensa {plantas = [peashooter, cactus, repeater], zombies = []}

linea2 = LineaDeDefensa {plantas = [], zombies = []}

linea3 = LineaDeDefensa {plantas = [], zombies = []}

-- funciones utils (genericas para varias soluciones)
listaEstaVacia [] = True
listaEstaVacia _ = False

obtenerDanioTotal [] _ = 0
obtenerDanioTotal (elemento : cola) obtenerCampoPoder = (obtenerCampoPoder elemento) + obtenerDanioTotal cola obtenerCampoPoder

modificarPosicionDeLista _ _ [] = []
modificarPosicionDeLista index nuevoValor (x : xs)
  | index == 0 = nuevoValor : xs
  | otherwise = x : modificarPosicionDeLista (index - 1) nuevoValor xs

-- 2) a.
especialidad (Planta vida cantSoles poder)
  | cantSoles > 0 = "Proveedora"
  | poder > vida = "Atacante"
  | otherwise = "Defensiva"

-- 2) b.
tieneVidaMayorQueDiez = (> 10) . vidaZombie

tieneAccesorios = (>= 1) . length . accesorios

esPeligroso zombie = tieneAccesorios zombie || tieneVidaMayorQueDiez zombie

-- 3) a.

agregarElementoALinea elemento linea campoDeLinea = campoDeLinea linea ++ elemento

-- 3) b.
zombiesDeLineaSonPeligrosos [] = True
zombiesDeLineaSonPeligrosos (zombie : cola) = esPeligroso zombie && zombiesDeLineaSonPeligrosos cola

poderDePlantasEsMenorQuePoderDeZombies linea = obtenerDanioTotal (plantas linea) poder < obtenerDanioTotal (zombies linea) poderMordida

todosLosZombiesSonPeligrosos linea = (not . listaEstaVacia . zombies) linea && (zombiesDeLineaSonPeligrosos . zombies) linea

estaEnPeligro linea = poderDePlantasEsMenorQuePoderDeZombies linea || todosLosZombiesSonPeligrosos linea

-- 3) c.
plantasSonProveedoras [] = True
plantasSonProveedoras (planta : cola) = especialidad planta == "Proveedora" && plantasSonProveedoras cola

lineaNecesitaDefensa linea = (listaEstaVacia) listaDePlantas || plantasSonProveedoras listaDePlantas
  where
    listaDePlantas = plantas linea

-- 4)
plantasConsecutivasTieneDistintaEspecialidad [] _ = True
plantasConsecutivasTieneDistintaEspecialidad (planta : cola) [] = plantasConsecutivasTieneDistintaEspecialidad cola (especialidad planta)
plantasConsecutivasTieneDistintaEspecialidad (planta : cola) especialidadAnterior = especialidadAnterior /= (especialidad planta) && plantasConsecutivasTieneDistintaEspecialidad cola (especialidad planta)

-- sin usar length ni alguna funcion simil propia de haskell,
-- contar la cantidad de elementos de una lista
cantidadElementos [] = 0
cantidadElementos (elemento : cola) = 1 + cantidadElementos cola

esMixta linea = (cantidadElementos . plantas) linea >= 2 && (plantasConsecutivasTieneDistintaEspecialidad . plantas) linea []

-- 5)
restarVidaAObjeto objeto cantidadARestar registroVida funcionRestarVidaObjeto atacante = funcionRestarVidaObjeto objeto cantidadFinalARestar atacante
  where
    vidaNuevaDelObjeto = registroVida objeto - cantidadARestar
    cantidadFinalARestar = if vidaNuevaDelObjeto <= 0 then registroVida objeto else cantidadARestar

-- a.
-- filtrar accesorios recibe el elemento y devuelve la condicion de filtrado

condicionFiltradoPorPlanta (Planta 9 0 0) = (/= "Globo")
condicionFiltradoPorPlanta Planta {} = (/= "")

esCactus (Planta 9 0 0) = True
esCactus Planta {} = False

restarVidaAZombie zombie cantidadARestar plantaAtacante =
  zombie
    { nombre = drop cantidadARestar (nombre zombie),
      accesorios =
        [ accesorio | accesorio <- accesorios zombie, condicionFiltradoPorPlanta plantaAtacante accesorio
        ]
    }

plantaMataZombie zombie planta = restarVidaAObjeto zombie (poder planta) vidaZombie restarVidaAZombie planta

-- b.
restarVidaAPlanta planta cantidadARestar _ =
  planta
    { vida = vida planta - cantidadARestar
    }

zombieMataPlanta planta zombie = restarVidaAObjeto planta (poderMordida zombie) vida restarVidaAPlanta zombie

-- ### parte 2 ###

-- 1. I) evaluamos usando la estrategia call-by-name y sharing (como evalua haskell)
-- estaEnPeligro linea = poderDePlantasEsMenorQuePoderDeZombies linea || todosLosZombiesSonPeligrosos linea
--
-- estaEnPeligro LineaDeDefensa [] [zombieBase, ...]
--
-- aplicamos estaEnPeligro
-- poderDePlantasEsMenorQuePoderDeZombies linea || todosLosZombiesSonPeligrosos linea
--
-- como || es estricta, aplicamos poderDePlantasEsMenorQuePoderDeZombies
-- poderDePlantasEsMenorQuePoderDeZombies linea = obtenerDanioTotal (plantas linea) poder < obtenerDanioTotal (zombies linea) poderMordida
--
-- luego como < también es estricta, aplicamos obtenerDanioTotal
-- obtenerDanioTotal [] poder
--
-- 0
--
-- queda asi:
-- 0 < obtenerDanioTotal [zombieBase, ...] poderMordida
--
-- aplicamos lo de la derecha ("obtenerDanioTotal")
-- obtenerDanioTotal (elemento : cola) obtenerCampoPoder = (obtenerCampoPoder elemento) + obtenerDanioTotal cola obtenerCampoPoder
--
-- 1 + obtenerDanioTotal [zombieBase, ...] poderMordida
--
-- 1 +  (1 + obtenerDanioTotal [zombieBase, ...] poderMordida)

-- 1 +  (1 +  (1 + obtenerDanioTotal [zombieBase, ...] poderMordida))
--
-- ...
--
-- como podemos notar, no termina nunca.

-- II) a) evaluamos de igual manera que antes
--
-- lineaNecesitaDefensa linea = (listaEstaVacia . plantas) linea || (plantasSonProveedoras . plantas) linea
--
-- lineaNecesitaDefensa (Linea [peashooter, ...] [])
--
-- ignoramos el record sintax y pasamos a la evaluacion de las funciones internas
-- listaEstaVacia [peashooter, ...]
--
-- gracias al pattern matching, nos devuelve:
-- False
--
-- luego queda asi:
-- False || plantasSonProveedoras [peashooter, ...]
--
-- luego como || es estricta, evaluamos la funcion de la derecha:
-- plantasSonProveedoras (planta : cola) = especialidad planta == "Proveedora" && plantasSonProveedoras cola
--
-- plantasSonProveedoras (peashooter:[peashooter,...])
--
-- evaluamos la especialidad de la planta:
-- especialidad planta == "Proveedora"
--
-- "Defensiva" == "Proveedora"
--
-- False
--
-- False && plantasSonProveedoras cola
--
-- Como tenemos False del lado izquierdo y estamos evaluando la conjuncion, haskell termina el proceso y no continua evaluando el lado derecho.
--
--
--
-- II) b)
-- lineaNecesitaDefensa linea = (not. listaEstaVacia . plantas) linea || (plantasSonProveedoras . plantas) linea
--
-- lineaNecesitaDefensa (Linea [sunflower, ...] [])
--
-- ignoramos el record sintax y pasamos a la evaluacion de las funciones internas
-- listaEstaVacia [sunflower, ...]
--
-- gracias al pattern matching, nos devuelve:
-- False
--
-- luego se le aplica la funcion not y queda asi:
-- False || plantasSonProveedoras [sunflower, ...]
--
-- luego como || es estricta, evaluamos la funcion de la derecha:
-- plantasSonProveedoras (planta : cola) = especialidad planta == "Proveedora" && plantasSonProveedoras cola
--
-- plantasSonProveedoras (sunflower:[sunflower,...])
--
-- evaluamos la especialidad de la planta:
-- especialidad planta == "Proveedora"
--
-- "Proveedora" == "Proveedora"
--
-- True
--
-- True && plantasSonProveedoras [sunflower, ...]
--
-- luego evaluamos la parte derecha
--
-- plantasSonProveedoras [sunflower, ...]
--
-- especialidad planta == "Proveedora"
--
-- "Proveedora" == "Proveedora"
--
-- True && (True && plantasSonProveedoras [sunflower, ...])
--
-- y asi sucesivamente, por lo que el procesamiento no termina.

septimoRegimiento :: [(Zombie, Int)]
septimoRegimiento = [(zombieNewspaper, 2)]

region :: [(Zombie, Int)]
region = [(gargantuar, 1), (zombieBase, 1), (zombieBase, 2), (zombieBase, 2), (zombieBase, 3), (gargantuar, 3)]

jardin1 = [linea1, linea2, linea3]

-- III)
agregarHorda :: [LineaDeDefensa] -> [(Zombie, Int)] -> [LineaDeDefensa]
agregarHorda = foldl agregarZombieAJardin

-- agregarZombieAJardin [linea1, linea2, linea3] (zombie, 1) = linea1 {zombies=agregarElementoALinea [zombie] linea1 zombies}:[linea2,linea3]

-- agregarZombieAJardin [linea1, linea2, linea3] (zombie, 2) = [linea1] ++ [linea2 {zombies=agregarElementoALinea [zombie] linea2 zombies}] ++ [linea3]

-- agregarZombieAJardin [linea1, linea2, linea3] (zombie, 3) = [linea1,linea2] ++ [linea3 {zombies=agregarElementoALinea [zombie] linea3 zombies}]

agregarZombieAJardin :: [LineaDeDefensa] -> (Zombie, Int) -> [LineaDeDefensa]
agregarZombieAJardin jardin (zombie, nroLinea) =
  modificarPosicionDeLista posicionDeLinea nuevaLinea jardin
  where
    posicionDeLinea = nroLinea - 1
    nuevaLinea = agregarZombieALinea zombie (jardin !! posicionDeLinea)

agregarZombieALinea zombie linea = linea {zombies = agregarElementoALinea [zombie] linea zombies}

-- IV)
-- Siempre la planta dispara primero. Si la planta mata al zombie, el zombie no llega a morder nunca
rondaDeAtaque :: Planta -> Zombie -> Int -> (Planta, Zombie)
rondaDeAtaque planta zombie cantidadDeMordidas 
    | zombieMuere zombieLuegoDelAtaque = (planta, zombie{nombre=""})
    | otherwise = (zombieMataPlanta planta nuevoZombie, zombieLuegoDelAtaque)
  where
    zombieLuegoDelAtaque = plantaMataZombie zombie planta
    nuevoZombie = zombieLuegoDelAtaque {poderMordida = poderMordida zombieLuegoDelAtaque * cantidadDeMordidas}

-- V)
plantaMuere = (== 0) . vida
zombieMuere = (== 0) . vidaZombie

-- VI)
--sin lista por comprensión:
--ataqueSistematico zombie (planta : colaPlanta) = zombieMataPlanta planta zombie : ataqueSistematico zombie colaPlanta

--con lista por comprensión
ataqueSistematico :: Zombie -> [Planta] -> [Planta]
ataqueSistematico zombie (planta:cola) = [zombieMataPlanta planta zombie | planta  <- planta : cola]

-- VII)
resultadoDeAtaque :: LineaDeDefensa -> [(Zombie, Int)] -> LineaDeDefensa
resultadoDeAtaque lineaDeDefensa horda
  | null (plantas lineaDeDefensa) = incluirHordaEnLineaDeDefensa lineaDeDefensa horda
  | otherwise = procesarAtaquesEnLinea (incluirHordaEnLineaDeDefensa lineaDeDefensa horda)

incluirHordaEnLineaDeDefensa :: LineaDeDefensa -> [(Zombie, Int)] -> LineaDeDefensa
incluirHordaEnLineaDeDefensa lineaDeDefensa horda =
  lineaDeDefensa { zombies = zombies lineaDeDefensa ++ nuevosZombies }
  where
    nuevosZombies = [zombie | (zombie, _) <- horda, (not.esPeligroso) zombie]

procesarAtaquesEnLinea (LineaDeDefensa [] zombies) = LineaDeDefensa [] zombies
procesarAtaquesEnLinea (LineaDeDefensa plantas []) = LineaDeDefensa plantas []
procesarAtaquesEnLinea linea = procesarAtaquesEnLinea (actualizarLinea linea (ejecutarAtaque linea))

ejecutarAtaque linea = rondaDeAtaque ((head.plantas)linea) ((last.zombies)linea) 1

actualizarLinea linea (planta, zombie) 
          | plantaMuere planta = linea { plantas = (tail.plantas) linea }
          | zombieMuere zombie = linea { zombies = (init.zombies) linea }
          | otherwise = linea { 
                plantas = ((planta:).tail.plantas) linea,
                zombies = (init.zombies) linea ++ [zombie]
            }

-- VIII)



-- IX)
tieneMenosLetras zombie linea =
  not
    ( any
        ( \zombieEnLinea ->
            (length . nombre) zombieEnLinea <= (length . nombre) zombie
        )
        (zombies linea)
    )

-- X) Explicar qué hace la función y dar los tipos de la función:

-- f h m p lista
-- | elem h lista = head (filter (m h) lista)
-- | otherwise = fst p
-- a. Indicar todos los conceptos que se aplican acá.
-- b. ¿Se puede mejorar la función? En caso afirmativo realizarlo.
-- c. ¿Qué pasaría si la lista es infinita?
-- 
-- 0. La función primero evalúa si h está dentro de la lista y en caso afirmativo filtra los valores de la lista por la función m recibiendo a h y
-- devuelve la cabeza de la lista que devuelve filter, en caso negativo pasa al otro caso que devuelve la primera posición de la lista p. 
-- Entonces el tipo sería a -> (a -> Bool) -> (b, c) -> [a]  -> b

-- a. Aqui se aplica composición de funciones, guardas, listas, funciones de orden superior como filter que recibe una función y una lista y evalua 
-- cada posición de la lista con esa función. 
-- Currificacion, en la funcion que recibe filter como parametro

-- b.
filtrarSiExiste elementoAEncontrar condicionDeFiltrado exepcion lista = head (filter (\item -> elementoAEncontrar `elem` lista && condicionDeFiltrado elementoAEncontrar item) lista ++ [fst exepcion])

-- c. Si la lista es infinita y nunca encuentra al elemento h, la función elem va a estár buscando infinitas veces y nunca va a pasar a la segunda
-- guarda. En el caso de que lo encuentre haría la segunda parte con normalidad.

-- XI)
nivelSupervivencia :: LineaDeDefensa -> Int
nivelSupervivencia lineaDeDefensa =
  (\sumaVidaPlantas sumaMuerteZombies -> sumaVidaPlantas - sumaMuerteZombies)
    (sum (map vida (plantas lineaDeDefensa)))
    (sum (map vidaZombie (zombies lineaDeDefensa)))
