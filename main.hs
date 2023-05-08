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
cactus = Planta 8 20 3
rose = Planta 2 45 7

-- Zombies
zombieBase = Zombie "Base" [] 1 
zombieBaloon = Zombie "Balloon Zombie" ["Globo"] 1 
zombieNewspaper = Zombie "Newspaper Zombie" ["Diario"] 2 
gargantuar = Zombie "Gargantuar Hulk Smash Puny God" ["Poste de luz", "zombie enano"] 30

-- funciones utils (genericas para varias soluciones)
listaNoEstaVacia = (> 0) . length

obtenerDanioTotal [] _ = 0
obtenerDanioTotal (elemento : cola) obtenerCampoPoder = (obtenerCampoPoder elemento) + obtenerDanioTotal cola obtenerCampoPoder


-- 2) a.
especialidad (Planta vida cantSoles poder)
  | cantSoles > 0 = "Proveedora"
  | poder > vida = "Atacante"
  | otherwise = "Defensiva"


-- 2) b.
tieneVidaMayorQueDiez = (> 10) . vidaZombie
tieneAccesorios =  (>= 1) . length . accesorios
esPeligroso zombie = tieneAccesorios zombie || tieneVidaMayorQueDiez zombie

-- 3) a.
agregarElementoALinea elemento linea campoDeLinea = campoDeLinea linea : elemento

-- 3) b.
zombiesDeLineaSonPeligrosos [] = True
zombiesDeLineaSonPeligrosos (zombie : cola) = esPeligroso zombie && zombiesDeLineaSonPeligrosos cola

poderDePlantasEsMenorQuePoderDeZombies linea = obtenerDanioTotal (plantas linea) poder < obtenerDanioTotal (zombies linea) poderMordida

todosLosZombiesSonPeligrosos linea = (listaNoEstaVacia . zombies) linea && (zombiesDeLineaSonPeligrosos . zombies) linea

estaEnPeligro linea = poderDePlantasEsMenorQuePoderDeZombies linea || todosLosZombiesSonPeligrosos linea


-- 3) c.
plantasSonProveedoras [] = True
plantasSonProveedoras (planta : cola) = especialidad planta == "Proveedora" && plantasSonProveedoras cola

lineaNecesitaDefensa linea = (listaNoEstaVacia . plantas) linea || (plantasSonProveedoras . plantas) linea


-- 4)
plantasConsecutivasTieneDistintaEspecialidad [] _ = True
plantasConsecutivasTieneDistintaEspecialidad (planta : cola) [] = plantasConsecutivasTieneDistintaEspecialidad cola (especialidad planta)
plantasConsecutivasTieneDistintaEspecialidad (planta : cola) especialidadAnterior = especialidadAnterior /= (especialidad planta) && plantasConsecutivasTieneDistintaEspecialidad cola (especialidad planta)

-- sin usar length ni alguna funcion simil propia de haskell, 
-- contar la cantidad de elementos de una lista
cantidadElementos [] = 0
cantidadElementos (elemento: cola) = 1 + cantidadElementos cola

esMixta linea = (cantidadElementos . plantas) linea >= 2 && (plantasConsecutivasTieneDistintaEspecialidad . plantas) linea []

-- 5) a.
plantaMataZombie planta zombie = poder planta > vidaZombie zombie
-- 5) b.
zombieMataPlanta planta zombie = poderMordida zombie > vida planta

-- 5) a y b forma generica.
-- atacarObjeto receptor atacante vidaReceptor poderAtacante = vidaReceptor receptor - poderAtacante atacante
-- atacarObjeto peashooter zombieBase  vida poderMordida -- result: 4
-- atacarObjeto zombieBase peashooter vidaZombie poder -- result: 2