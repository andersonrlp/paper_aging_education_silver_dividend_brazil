
clear
set dp comma
set more off

* A - baseline
* B - secondary and tertiary education
* C - tertiary education and formally employed
* D - tertiary education, formally employed and high status activities
* E - secondary and tertiary education and minimum retirement ages (62 and 65).
* F - tertiary education, formally employed, high status activities and minimum retirement ages

//// Observed income by age ////
cd "C:\Users\Anderson Rocha\Desktop\Anderson Rocha\1 UFMG\7 Estudos\1_em_desenvolvimento\1art_simulation\3versao_comp\3dados"
use "data2010.dta", clear
describe
*keep if age >= 45
sum age
collapse (count) renda [w=perwt], by(age)

save "C:\Users\Anderson Rocha\Desktop\Anderson Rocha\1 UFMG\7 Estudos\1_em_desenvolvimento\1art_simulation\3versao_comp\3dados\sim_stata\labor_income\obs_age2010.dta", replace

//// No simulation ////
use "data2010.dta", clear
drop trab regnbr age2 empstat coorte agegrp formal setor grossinc grsincot
*keep if age >= 45

collapse (mean) renda [w=perwt], by(age)
*twoway (line renda age, sort)
lowess renda age, bwidth(.15) gen(renda_suav)

save "C:\Users\Anderson Rocha\Desktop\Anderson Rocha\1 UFMG\7 Estudos\1_em_desenvolvimento\1art_simulation\3versao_comp\3dados\sim_stata\labor_income\nosim2010.dta", replace

//// SIMULATION SCENARIO 1 (group A - baseline) ////
use "data2010.dta", clear
drop trab regnbr empstat coorte agegrp formal setor grossinc grsincot year lnrend
set seed 123
sample 50
compress

sum age

reg renda age age2 sex raca relate urban [pw=perwt]
matrix c = e(b) // matrix of betas
svmat c, names(b_a) // saving matrix of betas in the dataset
forvalues k=1/7{ // betas for each observation
	egen beta_a`k'=max(b_a`k')
}
drop b_a*
predict pred_a // predicted probability
sum pred_a

collapse (mean) pred_a [w=perwt], by(age)
lowess pred_a age, bwidth(.15) gen(prod_a_suav)

*save "sim2010_a.dta", replace

//// SIMULATION SCENARIO 2 (group B) ////
use "data2010.dta", clear
drop trab regnbr empstat coorte agegrp formal setor grossinc grsincot year lnrend
set seed 123
sample 50
compress


reg renda age age2 sex raca relate urban if edattain==2|edattain==3
matrix d = e(b)
svmat d, names(b_b)
forvalues k = 1/7{
	egen beta_b`k'=max(b_b`k')
}
drop b_b*

#delimit;
gen pred_sim_b = beta_b7 + beta_b1*age + beta_b2*age2 + beta_b3*sex +
				 beta_b4*raca + beta_b5*relate + beta_b6*urban
;

collapse (mean) pred_sim_b [w=perwt], by(age)
lowess pred_sim_b age, bwidth(.15) gen(pred_sim_b_suav)

*save "sim2010_b.dta", replace

//// SIMULATION SCENARIO 3 (group C) ////
use "data2010.dta", clear
drop trab regnbr empstat coorte agegrp formal setor grossinc grsincot year lnrend
set seed 123
sample 50
compress

reg renda age age2 sex raca relate urban [pw=perwt] if edattain==3 & socsec==1
matrix d = e(b)
svmat d, names(b_c)
forvalues k = 1/7{
	egen beta_c`k'=max(b_c`k')
}
drop b_c*
#delimit;
gen pred_sim_c = beta_c7 + beta_c1*age + beta_c2*age2 + beta_c3*sex +
				 beta_c4*raca + beta_c5*relate + beta_c6*urban
;

collapse (mean) pred_sim_c [w=perwt], by(age)
lowess pred_sim_c age, bwidth(.15) gen(pred_sim_c_suav)

*save "sim2010_c.dta", replace

//// SIMULATION SCENARIO 4 (group D) ////
use "data2010.dta", clear
drop trab regnbr empstat coorte agegrp formal setor grossinc grsincot year lnrend
set seed 123
sample 50
compress

reg renda age age2 sex raca relate urban [pw=perwt] if edattain==3 & socsec==1 & statusocc==1|statusocc==2
matrix d = e(b)
svmat d, names(b_d)
forvalues k = 1/7{
	egen beta_d`k'=max(b_d`k')
}
drop b_d*
#delimit;
gen pred_sim_d = beta_d7 + beta_d1*age + beta_d2*age2 + beta_d3*sex +
				 beta_d4*raca + beta_d5*relate + beta_d6*urban
;

collapse (mean) pred_sim_d [w=perwt], by(age)
lowess pred_sim_d age, bwidth(.15) gen(pred_sim_d_suav)

*save "sim2010_d.dta", replace

//// SIMULATION SCENARIO 5 (group E) ////
use "data2010.dta", clear
drop trab regnbr empstat coorte agegrp formal setor grossinc grsincot year lnrend
gen retired2=1
replace retired2=0 if (age<=65 & sex==0) | (age<=62 & sex==1)
set seed 123
sample 50
compress

reg renda age age2 sex raca relate urban retired2 [pw=perwt] if edattain==2|edattain==3 & socsec==1 & statusocc==1|statusocc==2
matrix d = e(b)
svmat d, names(b_e)
forvalues k = 1/8{
	egen beta_e`k'=max(b_e`k')
}
drop b_e*
#delimit;
gen pred_sim_e = beta_e8 + beta_e1*age + beta_e2*age2 + beta_e3*sex + 
				 beta_e4*raca + beta_e5*relate + beta_e6*urban + beta_e7*retired2
;

collapse (mean) pred_sim_e [w=perwt], by(age)
lowess pred_sim_e age, bwidth(.15) gen(pred_sim_e_suav)

*save "sim2010_e.dta", replace

//// SIMULATION SCENARIO 6 (group F) ////
use "data2010.dta", clear
drop trab regnbr empstat coorte agegrp formal setor grossinc grsincot year lnrend
gen retired2=1
replace retired2=0 if (age<=65 & sex==0) | (age<=62 & sex==1)
set seed 123
sample 50
compress

reg renda age age2 sex raca relate urban retired2 [pw=perwt] if edattain==3 & socsec==1 & statusocc==1|statusocc==2
matrix d = e(b)
svmat d, names(b_f)
forvalues k = 1/8{
	egen beta_f`k'=max(b_f`k')
}
drop b_f*
#delimit;
gen pred_sim_f = beta_f8 + beta_f1*age + beta_f2*age2 + beta_f3*sex + 
				 beta_f4*raca + beta_f5*relate + beta_f6*urban + beta_f7*retired2
;

collapse (mean) pred_sim_f [w=perwt], by(age)
lowess pred_sim_f age, bwidth(.15) gen(pred_sim_f_suav)

*save "sim2010_f.dta", replace

