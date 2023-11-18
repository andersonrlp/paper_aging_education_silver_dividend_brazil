############################################
#### Script 2 - Simulation Labor Income ####
############################################
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
  n <- seq(ano.i, ano.f, freq) # per?odo
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
### Simulation (stata) results
#setwd('')
load('data_sim2.RData')

#### Standardization ####
xid <- which(tab_sim$Idade >= 30 & tab_sim$Idade <= 49)
ylm <- apply(tab_sim[xid,-1], 2, mean) # mean 30-49 
rm(xid)

tabsim <- data.frame(idade = tab_sim$Idade,
                     Obs = tab_sim[,'Obs']/ylm['Obs'],
                     A = tab_sim[,'A']/ylm['A'],
                     B = tab_sim[,'B']/ylm['B'],
                     C = tab_sim[,'C']/ylm['C'],
                     D = tab_sim[,'D']/ylm['D'],
                     E = tab_sim[,'E']/ylm['E'],
                     F = tab_sim[,'F']/ylm['F'])
rownames(tabsim) <- tabsim$idade; tabsim <- tabsim[,-1]

#### Reescaling ####
rfactors <- matrix(,nrow = nrow(tabsim), ncol = ncol(tabsim))
for(j in 1:ncol(tabsim)){
  rfactors[,j] <- tabsim[,j]/tabsim[,'Obs']
}
rfactors <- data.frame(rfactors); colnames(rfactors) <- colnames(tabsim)
xfactor <- rowMeans(rfactors[,-(1:2)])

xtabsim <- data.frame(obs = tabsim[,'Obs']/rfactors[,'F'],
                      a = tabsim[,'A']/xfactor,
                      b = tabsim[,'B']/xfactor,
                      c = tabsim[,'C']/xfactor,
                      d = tabsim[,'D']/xfactor,
                      e = tabsim[,'E']/xfactor,
                      f = tabsim[,'F']/xfactor)

#### Smoothing ####
incsim <- data.frame(obs = lowess(xtabsim[,'obs'], f = .15)$y,
                     a = lowess(xtabsim[,'a'], f = .15)$y,
                     b = lowess(xtabsim[,'b'], f = .15)$y,
                     c = lowess(xtabsim[,'c'], f = .15)$y,
                     d = lowess(xtabsim[,'d'], f = .15)$y,
                     e = lowess(xtabsim[,'e'], f = .15)$y,
                     f = lowess(xtabsim[,'f'], f = .15)$y)
incsim[incsim < 0] <- 0

## checking
xyplot(a ~ as.numeric(rownames(tabsim)), data = incsim, type = c('l','g'))

incsim$age <- as.numeric(rownames(tabsim)) # ages
dim(incsim)
id45 <- which(incsim$age >= 45)

#### Interpolation ####
siminc <- apply(incsim[,2:7], 2, function(i)
  lin.interp(x = incsim$a, y = i, ano.i = 2010, ano.f = 2150, freq = 1))
for(i in seq_along(siminc)){
  rownames(siminc[[i]]) <- incsim$age
  siminc[[i]] <- siminc[[i]][,which(as.numeric(colnames(siminc[[i]])) <= 2060)]
}

# checking
plot(siminc[[2]]$`2010`, type = 'l', ylim = c(0,1.3))
lines(siminc[[3]]$`2030`, col = 2)
lines(siminc[[4]]$`2050`, col = 3)
lines(siminc[[6]]$`2060`, col = 4)

siminc2 <- list()
for(i in seq_along(siminc)){
  siminc2[[i]] <- gather(siminc[[i]], year, income)
  siminc2[[i]]$age <- incsim$age
  siminc2[[i]]$scen <- paste('Scenario', i)
}
siminc3 <- do.call(rbind, siminc2)

#### Graph ####
x <- c('2030','2060') # 2030 and 2060
siminc4 <- siminc3[which(siminc3$year %in% x),]
siminc4 <- siminc4[which(siminc4$age >= 45),]

gcolors = gray.colors(6, .1, .65, rev = F)
mylty = 1:6
mylwd = 3
xcex <- 1.2

(gsiminc2 <- 
xyplot(income ~ age | year, data = siminc4, groups = scen, type = c('l','g'),
       col = gcolors, lwd = mylwd, lty = mylty, scales = list(alternating = F, y = 'free'),
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
                                                 lwd = mylwd, lty = mylty)),
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

#### Average labor income 45+ ####
x <- which(siminc3$age >= 45)

siminc3[x,] %>% 
  group_by(year, scen) %>% summarise(m = mean(income))

tabavinc <- siminc3[x,] %>% 
  group_by(year, scen) %>% summarise(mean = mean(income))
tabavinc <- data.frame(tabavinc)
tabavinc$year <- as.numeric(tabavinc$year)

tabavinc <- reshape(tabavinc, direction = 'wide', idvar = 'year', timevar = 'scen')
str(tabavinc)

tabavinc2 <- ts(tabavinc[,-1], start = 2010); time(tabavinc2)

x <- seq(2010,2060,10)
tabavinc <- tabavinc[tabavinc$year %in% x,]
rownames(tabavinc) <- tabavinc$year
tabavinc <- tabavinc %>% select(,-('year'))

colnames(tabavinc) <- gsub('mean.', '', x = colnames(tabavinc), fixed = T)
rm(list = c('x','tabavinc2'))

#### Saving ####
### Figures
#setwd('')
pdf('fig2.pdf', width = 9, height = 6); gsiminc2; dev.off()

### Table
#setwd('')
write.table(tabavinc, 'tabavinc.csv', sep = ';', dec = '.')

### Data
#setwd('')
save(list = 'siminc', file = 'proj_income.RData')

rm(list = ls(all = T))
graphics.off()
cat("\014")
