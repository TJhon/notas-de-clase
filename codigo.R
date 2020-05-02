# Sistema de calefaccion y refrigeracion

lima <- readRDS('dr/muestralima.rds')

lima <- mlogit.data(lima, varying = c(2:8, 10:16), choice = "depvar", shape = "wide")

sitema_doble <- index(lima)$alt %in% c('gcc', 'ecc', 'erc', 'hpc') # los dos sistemas

calefaccion <- index(lima)$alt %in% c('erc', 'er') # solo calefaccion

# costos de operacion de instalacion y costos de opracion solo son relevantes
# para el sistema doble


lima$icca[!sitema_doble] <- 0 #instalacion 
lima$occa[!sitema_doble] <- 0 #operacion

# create ingreso variables for two sets cooling and rooms

lima$ing_doble <- lima$simple <- 0
lima$ing_doble[sitema_doble] <- lima$ingreso[sitema_doble]
lima$simple[calefaccion] <- lima$ingreso[calefaccion]

# create an intercet for cooling modes

lima$int_doble <- as.numeric(sitema_doble)
# estimate the model with only one nest elasticity

nl <- mlogit(depvar ~ ich + och +icca + occa + simple + ing_doble + int_doble | 0, lima,
             nests = list(cooling = c('gcc','ecc','erc','hpc'), 
                          other = c('gc', 'ec', 'er')), un.nest.el = TRUE)
summary(nl)
