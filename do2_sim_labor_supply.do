
clear
set dp comma
set more off

* A - baseline
* B - secondary and tertiary education
* C - tertiary education and formally employed
* D - tertiary education, formally employed and high status activities
* E - secondary and tertiary education and minimum retirement ages (62 and 65).
* F - tertiary education, formally employed, high status activities and minimum retirement ages

cd "C:\Users\Anderson Rocha\Desktop\Anderson Rocha\1 UFMG\7 Estudos\1_em_desenvolvimento\1art_simulation\3versao_comp\3dados"
use "data2010.dta", clear

cls
describe
cls

drop regnbr age2 empstat coorte agegrp formal setor grossinc grsincot

keep if age >= 45
sum age

compress

//// Simulation of Returns ////
*** Group A: all individuals - baseline
logit trab age sex raca relate urban [pw=perwt]
matrix c = e(b) // matrix of betas
svmat c, names(b_a) // saving matrix in the dataset
forvalues k=1/6{ // repeat betas for each observation
	egen beta_a`k'=max(b_a`k')
}
predict prob_a if e(sample) // predicted probability
sum prob_a

** Saving simulation data
collapse (mean) prob_a [w=perwt], by(age)
twoway (line prob_a age, sort)
*save "\sim2010_a.dta", replace

*** Group B: individuals in secondary and tertiary education
use "data2010.dta", clear
drop regnbr age2 empstat coorte agegrp formal setor grossinc grsincot

keep if age >= 45
sum age
compress

logit trab age sex raca relate urban [pw=perwt] if edattain==2|edattain==3
matrix d = e(b) // matrix of betas
svmat d, names(b_b) // saving matrix in the dataset
forvalues k = 1/6{ // betas for each observation
	egen beta_b`k'=max(b_b`k')
}
/* predict probability */
#delimit;
gen prob_sim_b = (exp(beta_b6 + beta_b1*age + beta_b2*sex + beta_b3*raca + 
					  beta_b4* relate + beta_b5*urban)/
			     (1 + exp(beta_b6 + beta_b1*age + beta_b2*sex + beta_b3*raca + 
						  beta_b4*relate + beta_b5*urban)))
;

** saving
collapse (mean) prob_sim_b [w=perwt], by(age)
twoway (line prob_sim_b age, sort)
*save "sim2010_b.dta", replace

*** Group C: individuals with tertiary education and formally employed
use "data2010.dta", clear
drop regnbr age2 empstat coorte agegrp formal setor grossinc grsincot

keep if age >= 45
sum age
compress

/* Multinomial logistic regression */
gen trab3 = trab
replace trab3 = 2 if socsec==1

mlogit trab3 age sex raca relate urban [pw=perwt] if edattain==3, base(0)
matrix d = e(b)
svmat d, names(b_c)
forvalues k = 13/18{
	egen beta_c`k'=max(b_c`k')
}

#delimit;
gen prob_sim_c = (exp(beta_c18 + beta_c13*age + beta_c14*sex + beta_c15*raca + 
					  beta_c16*relate + beta_c17*urban)/
			     (1 + exp(beta_c18 + beta_c13*age + beta_c14*sex + beta_c15*raca + 
					  beta_c16*relate + beta_c17*urban)))
;

collapse (mean) prob_sim_c [w=perwt], by(age)
twoway (line prob_sim_c age, sort)
*save "sim2010_c.dta", replace

*** Group D: tertiary education, formally employed and high status activities
use "data2010.dta", clear
drop regnbr age2 empstat coorte agegrp formal setor grossinc grsincot

keep if age >= 45
sum age
compress

/* Multinomial logistic regression */
gen trab4 = trab
replace trab4 = 2 if socsec==1 & statusocc==1|statusocc==2

mlogit trab4 age sex raca relate urban [pw=perwt] if edattain==3, base(0)
matrix d = e(b)
svmat d, names(b_d)
forvalues k = 13/18{
	egen beta_d`k'=max(b_d`k')
}

#delimit;
gen prob_sim_d = (exp(beta_d18 + beta_d13*age + beta_d14*sex + beta_d15*raca + 
					  beta_d16*relate + beta_d17*urban)/
			     (1 + exp(beta_d18 + beta_d13*age + beta_d14*sex + beta_d15*raca + 
					  beta_d16*relate + beta_d17*urban)))
;

collapse (mean) prob_sim_d [w=perwt], by(age)
twoway (line prob_sim_d age, sort)
*save "sim2010_d.dta", replace

*** Group E: individuals with secondary and tertiary education and retirement minimum age at 62 and 65
use "data2010.dta", clear
drop regnbr age2 empstat coorte agegrp formal setor grossinc grsincot

keep if age >= 45
sum age

gen retired2=1
replace retired2=0 if (age<=65 & sex==0) | (age<=62 & sex==1)
tab retired
tab retired2

compress

logit trab age sex raca relate urban retired2 [pw=perwt] if edattain==2|edattain==3
matrix d = e(b)
svmat d, names(b_e)
forvalues k = 1/7{
	egen beta_e`k'=max(b_e`k')
}

#delimit;
gen prob_sim_e = (exp(beta_e7 + beta_e1*age + beta_e2*sex + beta_e3*raca + 
					  beta_e4*relate + beta_e5*urban + beta_e6*retired2)/
			     (1 + exp(beta_e7 + beta_e1*age + beta_e2*sex + beta_e3*raca + 
						  beta_e4*relate + beta_e5*urban + beta_e6*retired2)))
;

collapse (mean) prob_sim_e [w=perwt], by(age)
twoway (line prob_sim_e age, sort)
*save "sim2010_e.dta", replace

*** Group F: individuals with tertiary education, formaly employed, high status activities, 
*** and minimum retirement age at 62 and 65
use "data2010.dta", clear
drop regnbr age2 empstat coorte agegrp formal setor grossinc grsincot

keep if age >= 45
sum age

gen retired2=1
replace retired2=0 if (age<=65 & sex==0) | (age<=62 & sex==1)
tab retired
tab retired2

gen trab4 = trab
replace trab4 = 2 if socsec==1 & statusocc==1|statusocc==2

compress

mlogit trab4 age sex raca relate urban retired2 [pw=perwt] if edattain==3, base(0)
matrix d = e(b)
svmat d, names(b_f)
forvalues k = 15/21{
	egen beta_f`k'=max(b_f`k')
}

#delimit;
gen prob_sim_f = (exp(beta_f21 + beta_f15*age + beta_f16*sex + beta_f17*raca + 
					  beta_f18*relate + beta_f19*urban + beta_f20*retired2)/
			     (1 + exp(beta_f21 + beta_f15*age + beta_f16*sex + beta_f17*raca + 
						  beta_f18*relate + beta_f19*urban + beta_f20*retired2)))
;

collapse (mean) prob_sim_f [w=perwt], by(age)
twoway (line prob_sim_f age, sort)
save "C:\Users\Anderson Rocha\Desktop\Anderson Rocha\1 UFMG\7 Estudos\1_em_desenvolvimento\1art_simulation\3versao_comp\3dados\sim_stata\labor_supply\sim2010_f.dta", replace
