library(tidyverse); library(stargazer)

# Sistema de calefaccion y refrigeracion 

lima <- readRDS('dr/muestralima.rds')

lima <- mlogit.data(lima, varying = c(2:8, 10:16), choice = "depvar", shape = "wide")

sitema_doble <- index(lima)$alt %in% c('gcc', 'ecc', 'erc', 'hpc') # los dos sistemas

calefaccion <- index(lima)$alt %in% c('erc', 'er') # solo calefaccion

# costos de operacion de instalacion y costos de opracion solo son relevantes
# para el sistema doble
 ## ich: costos de instalacion
 ## och: costos operativos


lima$icca[!sitema_doble] <- 0 # instalacion 
lima$occa[!sitema_doble] <- 0 # operacion

# create ingreso variables for two sets cooling and rooms

lima$ing_doble <- lima$simple <- 0
lima$ing_doble[sitema_doble] <- lima$ingreso[sitema_doble]
lima$simple[calefaccion] <- lima$ingreso[calefaccion]

# interepto para la eleccion

lima$int_doble <- as.numeric(sitema_doble)

# modelo con un solo nudo

nl <- mlogit(depvar ~ ich + och +icca + occa + simple + ing_doble + int_doble | 0, lima,
             nests = list(cooling = c('gcc','ecc','erc','hpc'), 
                          other = c('gc', 'ec', 'er')), un.nest.el = TRUE)
summary(nl)

nl2 <- update(nl, nests = list(central = c('ec', 'ecc', 'gc', 'gcc', 'hpc'), 
                               room = c('er', 'erc')))
summary(nl2)

nl3 <- update(nl, nests=list(n1 = c('gcc', 'ecc', 'erc'), n2 = c('hpc'),
                             n3 = c('gc', 'ec', 'er')))
summary(nl3)

str_t <- function(x, y){
    stargazer(x, type = ifelse(y == "t", "text", "latex"), header = FALSE)
}

mo <- c(nl, nl2, nl3)

stargazer(nl, nl2, nl3, type = 'text', header = FALSE, dep.var.labels = "Nudos", single.row=TRUE)
