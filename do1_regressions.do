
clear
set dp comma
set more off
cls

*log using "C:\Users\Anderson Rocha\Desktop\Anderson Rocha\1 UFMG\7 Estudos para publicação\2021\4_tese_sim\mods2010.log", text replace

cd "C:\Users\Anderson Rocha\Desktop\Anderson Rocha\1 UFMG\7 Estudos para publicação\1_em_desenvolvimento\1art_simulation\3versao_comp\3dados"

use "data2010.dta", clear

*use "data2015.dta", clear
cls
describe

keep if age >= 45
sum age

tab raca, m
tab raca2, m

drop raca
rename raca2 raca
tab raca, m

*logit trab age i.sex i.edattain [pw=perwt], or vce(robust)
*estimates store supply0

logit trab age i.sex i.raca i.relate i.urban [pw=perwt], or vce(robust)
estimates store supply1

logit trab age i.sex i.raca i.relate i.urban i.edattain [pw=perwt], or vce(robust)
estimates store supply2

logit trab age i.sex i.raca i.relate i.urban i.edattain i.retired [pw=perwt], or vce(robust)
estimates store supply3

** saving table
*cd ""

*outreg2 [supply1 supply2 supply3 supply4] using m_labor_supply, replace /*
**/ eform cti(odds ratio) stat(coef se) dec(4) noaster excel

*reg lnrend age i.sex i.edattain [pw=perwt], vce(robust)
*estimates store income0

reg lnrend age i.sex i.raca i.relate i.urban [pw=perwt], vce(robust)
estimates store income1

reg lnrend age i.sex i.raca i.relate i.urban i.edattain [pw=perwt], vce(robust)
estimates store income2

reg lnrend age i.sex i.raca i.relate i.urban i.edattain /*
*/ i.classwrk i.statusocc i.socsec i.retired [pw=perwt], vce(robust)
estimates store income3

** saving table
*cd ""

*outreg2 [income1 income2 income3 income4] using m_labor_income, replace /*
**/ stat(coef se) dec(4) noaster excel
