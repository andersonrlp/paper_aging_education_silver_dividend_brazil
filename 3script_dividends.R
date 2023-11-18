#########################################################
#### Script 3 - Economic Support Ratios and Dividend ####
#########################################################
rm(list = ls(all = T))
options(scipen = 999)
graphics.off()
cat("\014")

#### Packages ####
library(readstata13); library(foreign); library(lattice)
library(readxl); library(reshape2); library(tidyr); library(dplyr)
library(xtable); library(stargazer)

#### Directory and data ####
#setwd('')
load('proj_labor_others.RData')
load('proj_income.RData')

#### Reescaling pop size
pop1 <- pop/1000000 # million
plot(pop1$`1950`, ylim = c(0,5)); lines(pop1$`2010`, col = 2); lines(pop1$`2050`, col = 3)

xid <- which(as.numeric(rownames(pop1)) <= 80)
yid <- which(colnames(pop1) >= 2010 & colnames(pop1) <= 2060)

dim(pop1[xid,yid]); dim(lfpproj[[1]])

#### Workforce ####
lfp <- list()
for(i in 1:length(lfpproj)){
  lfp[[i]] <- pop1[xid,yid]*lfpproj[[i]]
}
names(lfp) <- names(sim_labor)

#### Out of workforce ####
nlfp <- list()
for(i in 1:length(lfpproj)){
  nlfp[[i]] <- pop1[xid,yid]*(1 - lfpproj[[i]])
}
names(nlfp) <- names(sim_labor)

#### Economic Support Ratio (ESR) ####
lfp2 <- lapply(lfp, colSums)

reslfp <- lapply(lfp2,function(i)
  i/colSums(pop1[xid,yid]))

reslfp <- do.call(cbind, reslfp)
reslfp2 <- ts(reslfp, start = 2010); time(reslfp2)
#cbind(reslfp[[1]],reslfp2[,1]) # checking

### Variation in ESR
greslfp <- matrix(,nrow = nrow(reslfp2)-1, ncol = ncol(reslfp2))

for(j in 1:dim(reslfp2)[2]){
  greslfp[,j] <- (reslfp2[(2:dim(reslfp2)[1]),j]/reslfp2[(1:(dim(reslfp2)[1]-1)),j]-1)*100
}
greslfp <- ts(greslfp, start = 2011); time(greslfp)

t1 <- lowess(greslfp[,1], f = .08)$y
t2 <- lowess(greslfp[,2], f = .08)$y
t3 <- lowess(greslfp[,3], f = .08)$y
t4 <- lowess(greslfp[,4], f = .08)$y
t5 <- lowess(greslfp[,5], f = .08)$y
t6 <- lowess(greslfp[,6], f = .08)$y

greslfp2 <- ts(cbind(t1,t2,t3,t4,t5,t6), start = 2011); time(greslfp)
rm(list = c('t1','t2','t3','t4','t5','t6'))

#### Identity Y/N ####
pop1 <- pop1[,which(as.numeric(colnames(pop1)) >= 2010 & as.numeric(colnames(pop1)) <= 2060)]
yid1 <- which(as.numeric(rownames(pop1)) >= 15 & as.numeric(rownames(pop1)) <= 80)
yid2 <- which(as.numeric(rownames(lfp[[1]])) >= 15 & as.numeric(rownames(lfp[[1]])) <= 64)
dim(siminc[[1]]); dim(pop1[yid1,]); dim(lfp[[1]][yid1,]) # checking

### Income per capita
x <- siminc[[1]][,'2010']; n1 <- seq(2010,2060)
siminc[['c2010']] <- as.data.frame(matrix(rep(x, each = length(n1)), # renda constante 2010
                                          ncol = length(n1), byrow = T))
colnames(siminc[['c2010']]) <- n1
rm(list = c('x','n1'))

yn <- list() # Y/N
for(i in 1:length(siminc)){
  yn[[i]] <- colSums(siminc[[i]])/colSums(pop1)
}
names(yn) <- names(siminc)

### Working-age population
piapop <- colSums(pop1[yid2,])/colSums(pop1) # PIA/POP

### workforce by working-age population
x <- lfp[[1]][,'2010']; n1 <- seq(2010,2060)
lfp[['c2010']] <- as.data.frame(matrix(rep(x, each = length(n1)), # renda constante 2010
                                       ncol = length(n1), byrow = T))
colnames(lfp[['c2010']]) <- n1
rm(list = c('x','n1'))

lfppia <- list() # LFP/Working-age Population
for(i in 1:length(lfp)){
  lfppia[[i]] <- colSums(lfp[[i]])/colSums(pop1[yid2,])
}
names(lfppia) <- names(lfp)

### Income per worker
ylfp <- list() # Y/LFP
for(i in 1:length(lfp)){
  ylfp[[i]] <- colSums(siminc[[i]])/colSums(lfp[[i]])
}
names(ylfp) <- names(siminc)

## checking
cbind(yn[[1]], piapop*lfppia[[1]]*ylfp[[1]])
cbind(yn[[2]], piapop*lfppia[[2]]*ylfp[[2]])
cbind(yn[[3]], piapop*lfppia[[3]]*ylfp[[3]])
cbind(yn[[4]], piapop*lfppia[[4]]*ylfp[[4]])
cbind(yn[[5]], piapop*lfppia[[5]]*ylfp[[5]])
cbind(yn[[6]], piapop*lfppia[[6]]*ylfp[[6]])
cbind(yn[[7]], piapop*lfppia[[7]]*ylfp[[7]])

#### Variation ####
### Income per capita g(Y/N)
gynlfp <- lapply(yn, function(i)
  log(i[2:length(i)]/i[1:(length(i)-1)])*100)
plot(gynlfp[[6]], type = 'l')
gynlfp <- do.call(cbind, gynlfp)
colnames(gynlfp) <- names(siminc)

### g(Working-age population/POP)
gpiapop <- log(piapop[2:length(piapop)]/piapop[1:(length(piapop)-1)])*100

### g(LFP/Working-age population)
glfppia <- lapply(lfppia, function(i)
  log(i[2:length(i)]/i[1:(length(i)-1)])*100)
#plot(glfppia[[6]], type = 'l')
glfppia <- do.call(cbind, glfppia)
colnames(glfppia) <- names(siminc)

### g(Y/LFP)
gylfp <- lapply(ylfp, function(i)
  log(i[2:length(i)]/i[1:(length(i)-1)])*100)
#plot(gylfp[[6]], type = 'l')
gylfp <- do.call(cbind, gylfp)
colnames(gylfp) <- names(siminc)
#plot(gylfp[,6], type = 'l')

### Smoothing
## Y/N
t1 <- lowess(gynlfp[,1], f = .0015)$y
t2 <- lowess(gynlfp[,2], f = .0015)$y
t3 <- lowess(gynlfp[,3], f = .0015)$y
t4 <- lowess(gynlfp[,4], f = .0015)$y
t5 <- lowess(gynlfp[,5], f = .0015)$y
t6 <- lowess(gynlfp[,6], f = .0015)$y
t7 <- lowess(gynlfp[,7], f = .0015)$y
gynlfp2 <- data.frame(cbind(t1,t2,t3,t4,t5,t6,t7)); colnames(gynlfp2) <- colnames(gynlfp)
rm(list = c('t1','t2','t3','t4','t5','t6','t7'))

## Working-age population/POP
gpiapop2 <- lowess(gpiapop, f = .2)$y

### LFP/Working-age population
t1 <- lowess(glfppia[,1], f = .0015)$y
t2 <- lowess(glfppia[,2], f = .0015)$y
t3 <- lowess(glfppia[,3], f = .0015)$y
t4 <- lowess(glfppia[,4], f = .0015)$y
t5 <- lowess(glfppia[,5], f = .0015)$y
t6 <- lowess(glfppia[,6], f = .0015)$y
t7 <- lowess(glfppia[,7], f = .0015)$y
glfppia2 <- data.frame(cbind(t1,t2,t3,t4,t5,t6,t7)); colnames(glfppia2) <- colnames(glfppia)
rm(list = c('t1','t2','t3','t4','t5','t6','t7'))

### Y/LFP
t1 <- lowess(gylfp[,1], f = .0015)$y
t2 <- lowess(gylfp[,2], f = .0015)$y
t3 <- lowess(gylfp[,3], f = .0015)$y
t4 <- lowess(gylfp[,4], f = .0015)$y
t5 <- lowess(gylfp[,5], f = .0015)$y
t6 <- lowess(gylfp[,6], f = .0015)$y
t7 <- lowess(gylfp[,7], f = .0015)$y
gylfp2 <- data.frame(cbind(t1,t2,t3,t4,t5,t6,t7)); colnames(gylfp2) <- colnames(gylfp)
rm(list = c('t1','t2','t3','t4','t5','t6','t7'))

#### Organizing the data with the projected simulations ####
year <- seq(2011,2060)
df1 <- data.frame(year = year, gyn = gynlfp2$a, gpiapop = gpiapop2,
                  glfppia = glfppia2$a, gylfp = gylfp2$a, scen = 'Scenario 1')
df2 <- data.frame(year = year, gyn = gynlfp2$b, gpiapop = gpiapop2,
                  glfppia = glfppia2$b, gylfp = gylfp2$b, scen = 'Scenario 2')
df3 <- data.frame(year = year, gyn = gynlfp2$c, gpiapop = gpiapop2,
                  glfppia = glfppia2$c, gylfp = gylfp2$c, scen = 'Scenario 3')
df4 <- data.frame(year = year, gyn = gynlfp2$d, gpiapop = gpiapop2,
                  glfppia = glfppia2$d, gylfp = gylfp2$d, scen = 'Scenario 4')
df5 <- data.frame(year = year, gyn = gynlfp2$e, gpiapop = gpiapop2,
                  glfppia = glfppia2$e, gylfp = gylfp2$e, scen = 'Scenario 5')
df6 <- data.frame(year = year, gyn = gynlfp2$f, gpiapop = gpiapop2,
                  glfppia = glfppia2$f, gylfp = gylfp2$f, scen = 'Scenario 6')
df <- rbind(df1,df2,df3,df4,df5,df6)

### Graphs
gcolors <- gray.colors(6,.2,.5, rev = T)
xcex <- 1.2
(gynlfp <-
    xyplot(gyn ~ year, data = df, groups = scen, type = c('l','g'), ylim = c(-1,.7),
           scales = 'free', #index.cond = list(c(4,5,6,1,2,3)),
           xlab = 'Year', ylab = '%', col = gcolors, lwd = 3, lty = 1:6, #pch = 1:6,
           par.strip.text = list(fontfamily = 'serif', cex = xcex),
           par.settings = list(axis.text = list(fontfamily = 'serif',cex = xcex),
                               axis.line = list(col = 0),
                               par.xlab.text = list(fontfamily = 'serif',cex = xcex),
                               par.ylab.text = list(fontfamily = 'serif',cex = xcex),
                               par.main.text = list(fontfamily = 'serif',cex = xcex),
                               par.sub.text = list(fontfamily = 'serif',cex = xcex),
                               strip.background = list(col = 'white'),
                               superpose.line = list(col = gcolors, lwd = 3, lty = 1:6)),
           #auto.key = list(space = 'inside', corner = c(.95,.05),lines = T,
            #               points = F, cex = xcex, columns = 2,
             #              text = c('Scenario 1','Scenario 2','Scenario 3',
              #                      'Scenario 4','Scenario 5','Scenario 6'),
               #            fontfamily = 'serif'),
           panel = function(...){
             lims <- current.panel.limits()
             panel.xyplot(...)
             panel.abline(h = 0, lty = 4)
             panel.abline(h = lims$ylim[1], v = lims$xlim[1])
           }))

######
df2 <- gather(df[,-2], component, value, -c(year, scen))
unique(df2$component)

df2$component2 <- ifelse(df2$component == 'gpiapop', 'g(SR)',
                         ifelse(df2$component == 'glfppia', 'g(LM)', 'g(l)'))
unique(df2$component2)

greslfp3 <- ts(cbind(df2[which(df2$component2 == "g(SR)" & df2$scen == 'Scenario 1'),"value"],
                     greslfp2), start = 2011); time(greslfp3)
colnames(greslfp3) <- c('t0','t1','t2','t3','t4','t5','t6')

gcolors <- c(2,gray.colors(6,.2,.6, rev = T))
xcex <- 1.2
(g_vsr_lfp <-
    xyplot(greslfp3, superpose = T, type = c('l','g'), scales = 'free', ylim = c(-1,.7),
           xlab = 'Year', ylab = '%', col = gcolors, lwd = 3, lty = c(2,1:6), #pch = 1:6,
           par.settings = list(axis.text = list(fontfamily = 'serif',cex = xcex),
                               axis.line = list(col = 0),
                               par.xlab.text = list(fontfamily = 'serif',cex = xcex),
                               par.ylab.text = list(fontfamily = 'serif',cex = xcex),
                               par.main.text = list(fontfamily = 'serif',cex = xcex),
                               par.sub.text = list(fontfamily = 'serif',cex = xcex),
                               strip.background = list(col = 'white'),
                               superpose.line = list(col = gcolors, lwd = 4, lty = c(2,1:6))),
           auto.key = list(space = 'inside', corner = c(.95,.95),lines = T,
                           points = F, cex = 2, columns = 2,
                           text = c('DSR','Scenario 1','Scenario 2','Scenario 3',
                                    'Scenario 4','Scenario 5','Scenario 6'),
                           fontfamily = 'serif'),
           panel = function(...){
             lims <- current.panel.limits()
             panel.xyplot(...)
             panel.abline(h = 0, lty = 2)
             panel.abline(h = lims$ylim[1], v = lims$xlim[1])
           }))

gcolors <- gray.colors(3,.2,.5, rev = F)
xcex <- 1.2
(g_lm <-
    xyplot(value ~ year, data = df2[which(df2$component2 == "g(LM)"),], groups = scen, type = c('l','g'),
           scales = 'free', xlab = 'Year', ylab = '%', col = gcolors, lwd = 3, lty = 1:6, ylim = c(-.5, .5),
           par.settings = list(axis.text = list(fontfamily = 'serif', cex = xcex),
                               axis.line = list(col = 0),
                               par.xlab.text = list(fontfamily = 'serif', cex = xcex),
                               par.ylab.text = list(fontfamily = 'serif', cex = xcex),
                               par.main.text = list(fontfamily = 'serif', cex = xcex),
                               par.sub.text = list(fontfamily = 'serif', cex = xcex),
                               strip.background = list(col = 'white'),
                               superpose.line = list(col = gcolors, lwd = 3, lty = 1:6)),
           #auto.key = list(space = 'inside', corner = c(.95,.95), lines = T,
            #               points = F, cex = xcex, columns = 2,
             #              text = c('Scenario 1','Scenario 2','Scenario 3',
              #                      'Scenario 4','Scenario 5','Scenario 6'),
               #            fontfamily = 'serif'),
           panel = function(...){
             lims <- current.panel.limits()
             panel.xyplot(...)
             panel.abline(h = 0, lty = 4)
             panel.abline(h = lims$ylim[1], v = lims$xlim[1])
           }))

gcolors <- gray.colors(3,.2,.5, rev = F)
xcex <- 1.2
(g_gl <-
    xyplot(value ~ year, data = df2[which(df2$component2 == "g(l)"),], groups = scen, type = c('l','g'),
           scales = 'free', xlab = 'Year', ylab = '%', col = gcolors, lwd = 3, lty = 1:6,
           par.settings = list(axis.text = list(fontfamily = 'serif', cex = xcex),
                               axis.line = list(col = 0),
                               par.xlab.text = list(fontfamily = 'serif', cex = xcex),
                               par.ylab.text = list(fontfamily = 'serif', cex = xcex),
                               par.main.text = list(fontfamily = 'serif', cex = xcex),
                               par.sub.text = list(fontfamily = 'serif', cex = xcex),
                               strip.background = list(col = 'white'),
                               superpose.line = list(col = gcolors, lwd = 3, lty = 1:6)),
           #auto.key = list(space = 'inside', corner = c(.95,.05), lines = T,
            #               points = F, cex = xcex, columns = 2,
             #              text = c('Scenario 1','Scenario 2','Scenario 3',
              #                      'Scenario 4','Scenario 5','Scenario 6'),
               #            fontfamily = 'serif'),
           panel = function(...){
             lims <- current.panel.limits()
             panel.xyplot(...)
             panel.abline(h = 0, lty = 4)
             panel.abline(h = lims$ylim[1], v = lims$xlim[1])
           }))

rm(list = c('df3','df4','df5','df6'))

#### Saving ####
### Figures
#setwd('')

pdf('g_ynlfp.pdf', width = 9, height = 6); gynlfp; dev.off()
pdf('g_vsr.pdf', width = 9, height = 6); g_vsr_lfp; dev.off()
pdf('g_lm.pdf', width = 9, height = 6); g_lm; dev.off()
pdf('g_gl.pdf', width = 9, height = 6); g_gl; dev.off()
