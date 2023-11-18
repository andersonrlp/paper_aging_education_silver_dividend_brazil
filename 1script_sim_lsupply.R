###########################################
#### Script 1 - Simulation Labor Force ####
###########################################
rm(list = ls(all = T))
options(scipen = 999)
graphics.off()
cat("\014")

#### Packages ####
library(readstata13); library(foreign); library(lattice)
library(readxl); library(reshape2); library(tidyr); library(dplyr)
library(xtable); library(stargazer)

#### Functions ####
## Function for linear interpolation
lin.interp <- function(x, y, ano.i = 1995, ano.f = 2050, freq = 5){
  n <- seq(ano.i, ano.f, freq)
  X1 <- matrix(,nrow = length(x), ncol = length(n))
  colnames(X1) <- n
  X1[,1] <- x; X1[,ncol(X1)] <- y
  
  w <- NULL # pesos
  for(i in 1:ncol(X1)){
    w[i] <- (ano.f - n[i])/(ano.f - ano.i)
  }
  
  ## Interpola??o
  for(j in 2:(ncol(X1)-1)){
    X1[,j] <- (1-w[j])*y + w[j]*x
  }
  
  return(as.data.frame(X1))
}

#### Directory and data ####
### Data - population, simulation
#setwd('')
load('data_sim.RData')

#### Interpolation ####
sim_labor <- apply(probs[,-c(1)], 2, function(i)
  lin.interp(x = probs$A, y = i, ano.i = 2010, ano.f = 2060, freq = 1))

for(i in seq_along(sim_labor)){
  rownames(sim_labor[[i]]) <- probs$Age
}
#View(sim_labor[[2]])

## Checking
plot(sim_labor[[2]]$`2010`, t = 'l', ylim = c(0,1), ylab = '')
lines(sim_labor[[2]]$`2030`, col = 2)
lines(sim_labor[[2]]$`2050`, col = 3)
lines(sim_labor[[2]]$`2060`, col = 4)

### Wide to long
sim_labor2 <- list()
for(i in seq_along(sim_labor)){
  sim_labor2[[i]] <- gather(sim_labor[[i]], year, value)
  sim_labor2[[i]]$age <- as.numeric(rownames(sim_labor[[i]]))
  sim_labor2[[i]]$scenario <- paste('Scenario', i)
}
#View(sim_labor2[[2]])
sim_labor3 <- do.call(rbind, sim_labor2)

#### Graph - labor force participation rate ####
x = c('2030','2060') # 2030 and 2060
sim_labor4 <- sim_labor3[which(sim_labor3$year %in% x),]

gcolors = gray.colors(6, .1, .65, rev = F)
mylty = 1:6
mylwd = 3
xcex <- 1.2

(gsim_labor2 <- 
xyplot(value ~ age | year, data = sim_labor4, groups = scenario, type = c('l','g'),
       ylim = c(0,1), col = gcolors, lwd = mylwd, lty = mylty, scales = list(alternating = F, y = 'free'),
       index.cond = list(c(1,2)), xlab = 'Age', ylab = '',
       par.strip.text = list(fontfamily = 'serif', cex = xcex),
       par.settings = list(axis.text = list(fontfamily = 'serif', cex = xcex),
                           axis.line = list(col = 0),
                           par.xlab.text = list(fontfamily = 'serif',cex = xcex),
                           par.ylab.text = list(fontfamily = 'serif',cex = xcex),
                           par.main.text = list(fontfamily = 'serif',cex = xcex),
                           par.sub.text = list(fontfamily = 'serif',cex = xcex),
                           strip.background = list(col = 'white'),
                           strip.border = list(col = "white"),
                           superpose.line = list(col = gcolors,
                                                 lwd = 4, lty = mylty)),
       auto.key = list(space = 'bottom', corner = c(1,.9), lines = T,
                       points = F, cex = xcex, columns = 3,
                       fontfamily = 'serif', text = c('Scenario 1','Scenario 2','Scenario 3',
                                                      'Scenario 4','Scenario 5','Scenario 6')),
       panel = function(...){
         lims <- current.panel.limits()
         panel.xyplot(...)
         panel.abline(h = 0, v = lims$xlim[1])
       })
)

#### Labor force 45+ ####
id1 <- which(as.numeric(rownames(pop)) >= 45 & as.numeric(rownames(pop)) <= 80)
id2 <- which(as.numeric(colnames(pop)) >= 2010 & as.numeric(colnames(pop)) <= 2060)
pop2 <- pop[id1, id2]; colnames(pop2); rownames(pop2)
rm(list = c('id1','id2'))
dim(pop2); dim(sim_labor[[1]]) # equal dimensions

### Number of workers by age (45+)
lfpage <- list() 
for(i in 1:length(sim_labor)){
  lfpage[[i]] <- pop2*sim_labor[[i]]
}
names(lfpage) <- names(sim_labor)

lfpage2 <- lapply(lfpage, colSums) # total number of workers by year
lfpage2 <- do.call(cbind, lfpage2)
lfpage2 <- ts(lfpage2, start = 2010); time(lfpage2)
#max(lfpage2[,1])

lfpage2[51,3]/lfpage2[51,1] # 1.178162 
lfpage2[51,6]/lfpage2[51,1] # 1.182414 

#### Projection of LFP ####
n <- seq(2010,2060,1)
lfpproj <- list()
for(i in 1:length(sim_labor)){
  lfpproj[[i]] <- rbind(lfp2010_44, sim_labor[[i]])
  colnames(lfpproj[[i]]) <- n
  rownames(lfpproj[[i]]) <- seq(0,80)
}
names(lfpproj) <- names(sim_labor) 
rm(list = c('x','n','lfp2010_44'))
#View(lfpproj[[2]])

dim(pop); dim(lfpproj[[1]])

id1 <- which(as.numeric(rownames(pop)) <= 80)
id2 <- which(as.numeric(colnames(pop)) >= 2010 & as.numeric(colnames(pop)) <= 2060)
pop3 <- pop[id1,id2]
dim(pop3); dim(lfpproj[[1]])

length(lfpproj)
labforce <- list()
for(i in 1:length(lfpproj)){
  labforce[[i]] <- pop3*lfpproj[[i]]
}
names(labforce) <- names(lfpproj)

labforce2 <- lapply(labforce, colSums) # total number of workers by year
labforce2 <- do.call(cbind, labforce2)
labforce2 <- ts(labforce2, start = 2010); time(labforce)

years <- seq(2010,2060)

lfpage3 <- data.frame(lfpage2/1000000)
lfpage3[,'year'] <- years
lfpage3[,'pop'] <- '45-80 years'
colnames(lfpage3)

labforce3 <- data.frame(labforce2/1000000)
labforce3[,'year'] <- years
labforce3[,'pop'] <- '15-80 years'
colnames(labforce3)

labforce4 <- rbind(lfpage3,labforce3)

gcolors <- gray.colors(6, .2, .6, rev = T)
mylty = 1:6
mylwd = 3
xcex <- 1.2

(glabforce3 <- 
xyplot(A + B + C + D + E + F ~ year | pop, data = labforce4, type = c('l','g'),
       xlab = 'Year', ylab = '(Millions)', col = gcolors, layout = c(1,2),
       scales = list(alternating = F, y = 'free'), index.cond = list(c(1,2)), 
       par.strip.text = list(fontfamily = 'serif', cex = xcex),
       par.settings = list(axis.text = list(fontfamily = 'serif', cex = xcex),
                           axis.line = list(col = 0),
                           par.xlab.text = list(fontfamily = 'serif',cex = xcex),
                           par.ylab.text = list(fontfamily = 'serif',cex = xcex),
                           par.main.text = list(fontfamily = 'serif',cex = xcex),
                           par.sub.text = list(fontfamily = 'serif',cex = xcex),
                           strip.background = list(col = 'white'),
                           strip.border = list(col = "white"),
                           superpose.line = list(col = gcolors,
                                                 lwd = 4, lty = mylty)),
       auto.key = list(space = 'bottom', corner = c(1,.9), lines = T,
                       points = F, cex = xcex, columns = 3,
                       fontfamily = 'serif', text = c('Scenario 1','Scenario 2','Scenario 3',
                                                      'Scenario 4','Scenario 5','Scenario 6')),
       panel = function(...){
         lims <- current.panel.limits()
         panel.xyplot(...)
         panel.abline(h = lims$ylim[1], v = lims$xlim[1])
       })
)

#### Years Worked 45+ ####
tabyearsworked <- data.frame(s1 = colSums(sim_labor[[1]]),s2 = colSums(sim_labor[[2]]),
                             s3 = colSums(sim_labor[[3]]),s4 = colSums(sim_labor[[4]]),
                             s5 = colSums(sim_labor[[5]]),s6 = colSums(sim_labor[[6]]))
class(rownames(tabyearsworked))

x <- seq(2010,2060,10); x <- as.character(x)
tabyearsworked <- tabyearsworked[rownames(tabyearsworked) %in% x,]
rm(x)

#### Saving results ####
### Figures
#setwd('')
pdf('fig1.pdf', width = 9, height = 6); gsim_labor2; dev.off()
pdf('fig3.pdf', width = 9, height = 14); glabforce3; dev.off()

### Table
#setwd('')
write.table(tabyearsworked, 'tabyearsworked.csv', sep = ';', dec = '.')

### Data
#setwd('')
save(list = c('lfpproj','sim_labor','pop','lin.interp'),
     file = 'proj_labor_others.RData')

rm(list = ls(all = T))
graphics.off()
cat("\014")
