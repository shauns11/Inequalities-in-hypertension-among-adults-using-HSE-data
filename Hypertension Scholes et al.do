
******************************************************************************************
*Income-based inequalities in hypertension and in undiagnosed hypertension: analysis of 
*Health Survey for England data.
*Journal of Hypertension 2020:38:912-924
*******************************************************************************************


clear
version 17
use "C:\Learning\Papers\BiProbit\Analysis_dataset.dta"
renvars, lower
generate cons=1

*Had to trim the nurse_wt for biprobit to work.
*With and without data from the nurse visit.
*33,286 and 16,531 respectively). 

gen flag=0
replace flag=1 if (wt_nurse>1.773 & wt_nurse!=.)
replace wt_nurse=1.74 if (wt_nurse>1.77 & wt_nurse!=.)
drop flag
di (2056/33286)         /* about 6%  */

svyset [pweight=wt_nurse],psu(point1) strata(gor1)

* Demographics.
gen male=0
gen female=0
replace male=1 if sex==1
replace female=1 if sex==2

*Lowest income as last category.
*Missing values as 99.
generate income=0
replace income=1 if eqv3==3
replace income=2 if eqv3==2
replace income=3 if eqv3==1
replace income=99 if eqv3<0
label define incomelbl 1 "Highest" 2 "Middle" 3 "Lowest" 99 "Missing"
label values income incomelbl

recode cigsta3 (-9/-1 =99)

generate BMIvg3=99
replace BMIvg3=0 if bmivg5==1
replace BMIvg3=1 if bmivg5==2
replace BMIvg3=2 if bmivg5==3
replace BMIvg3=3 if bmivg5==4
replace BMIvg3=3 if bmivg5==5
label define BMIlbl 0 "Underweight" 1 "Normal" 2 "Overweight" 3 "Obese" 99 "Missing"
label values BMIvg3 BMIlbl

recode diabete2 (-8 =99)
*recode as No/Yes.
recode diabete2 (2=0)
label define albl 0 "No diabetes" 1 "Diabetes"
label values diabete2 albl

recode origin2 (-9/-1 =99)
tab1 marital

*Ever had high bp.
recode everbp (-9/-8 =99)
recode topqual5 (-2=99)
recode urban14b (-1=99)

********************************************************
* Response to the nurse visit; Unweighted.
* Table S1.
*********************************************************

svyset, clear
svyset [pweight=cons],psu(point1) strata(gor1)

*Continuous age.
summ age if nurse==0
summ age if nurse==1
svy:mean age,over(nurse)   	
lincom _b[c.age@0bn.nurse] - _b[c.age@1.nurse]

*Demographics (excluding the missing).
foreach var of varlist ag16g10 sex income cigsta3 BMIvg3 everbp diabete2 origin2 marital topqual5 urban14b {
tab `var' nurse
tab `var' nurse, col nofreq
svy:tab `var' nurse if `var'<99 , per col format(%4.1f)  
}

*****************************************************.
*N=28019: nurse visit and BP data.
*N=17 without self-reported hypertension: N = 28002.
*****************************************************.

keep if nurse==1
keep if inrange(hy140om2,1,4)
keep if bp1==1|bp1==2  /* N = 28002   */

*N=4748 missing income date.
*N=23254 with income data.
di (19+4729)
di (28002-4748)

*****************************************************.
*Analysis of missing Income; unweighted.
*Table S2.
*****************************************************.

generate HAS_Income=1
replace HAS_Income=2 if inrange(eqv3,1,3)
svyset, clear
svyset [pweight=cons],psu(point1) strata(gor1)

* Continuous age.
summ age if HAS_Income==1
summ age if HAS_Income==2
svy:mean age,over(HAS_Income)  
lincom _b[c.age@1.HAS_Income] - _b[c.age@2bn.HAS_Income]

foreach var of varlist ag16g10 sex cigsta3 BMIvg3 everbp diabete2 origin2 marital topqual5 urban14b {
tab `var' HAS_Income
tab `var' HAS_Income, col nofreq
svy:tab `var' HAS_Income if `var'<99 , per col format(%4.1f)  
}

*****************************************
* Descriptives by income: Table1.
*****************************************

keep if nurse==1
keep if inrange(hy140om2,1,4)
keep if bp1==1|bp1==2                              /* N = 28002   */
count
mvdecode income,mv(99)

bysort income: summ age               /* age */
svy:regress age i.income
test 2.income 3.income

foreach var of varlist ag16g10 cigsta3 BMIvg3 everbp diabete2 origin2 marital topqual5 urban14b {
tab `var' income 
tab `var' income , col nofreq
svy:tab `var' income if `var'<99, per col format(%4.1f) 
}

*N=4748 missing income date. *N=23254 with income data.
* di (19+4729) 
* di (28002-4748)

*******************************************
*Hypertension definitions.
*Table 2 (sex); Table 3 (Income).
*******************************************

generate htn=0
replace htn=1 if inrange(hy140om2,2,4)  /* Survey-defined HTN */
tab hy140om2 htn

generate evertold=0
replace evertold=1 if bp1==1 	        /* Diagnosed HTN */

generate totalBP=0
replace totalBP=1 if (htn==1|bp1==1)    /* Total HTN */
tab1 totalBP                            /* 10,912 with hypertension (total definition) */

* Use pooled data as (standard).
* Different by sex

generate agegroup5=0
replace agegroup5=1 if (ag16g10==1|ag16g10==2|ag16g10==3)
replace agegroup5=2 if (ag16g10==4)
replace agegroup5=3 if (ag16g10==5)
replace agegroup5=4 if (ag16g10==6)
replace agegroup5=5 if (ag16g10==7)
label define agegroup5lbl 1 "16-44" 2 "45-54" 3 "55-64" 4 "65-74" 5 "75+"
label values agegroup5 agegroup5lbl

svyset, clear
svyset [pweight=wt_nurse],psu(point1) strata(gor1)
svy:tab agegroup5 sex, col format(%8.4f)    
*Table 1 (base = 28,002).
generate std_weight=0
replace std_weight=0.4459 if agegroup5==1 & sex==1
replace std_weight=0.1818 if agegroup5==2 & sex==1
replace std_weight=0.1554 if agegroup5==3 & sex==1
replace std_weight=0.1242 if agegroup5==4 & sex==1
replace std_weight=0.0928 if agegroup5==5 & sex==1

replace std_weight=0.4405 if agegroup5==1 & sex==2
replace std_weight=0.1754 if agegroup5==2 & sex==2
replace std_weight=0.1483 if agegroup5==3 & sex==2
replace std_weight=0.1233 if agegroup5==4 & sex==2
replace std_weight=0.1124 if agegroup5==5 & sex==2

*********************************************
*Total hypertension (totalBP).
*********************************************

tab1 sex 
tab sex income

svy:mean totalBP,stdize(agegroup5) stdweight(std_weight) over(sex) /*Table 2*/
estat size
svy:mean totalBP,stdize(agegroup5) stdweight(std_weight) over(sex income)  /*Table 3 */
estat size
lincom _b[c.totalBP@1bn.sex#2.income] - _b[c.totalBP@1bn.sex#1bn.income]
lincom _b[c.totalBP@1bn.sex#3.income] - _b[c.totalBP@1bn.sex#1bn.income]
lincom _b[c.totalBP@2.sex#2.income]-_b[c.totalBP@2.sex#1bn.income]
lincom _b[c.totalBP@2.sex#3.income]-_b[c.totalBP@2.sex#1bn.income]

*********************************************
* Survey-defined hypertension (HTN).
*********************************************

svy:mean htn,stdize(agegroup5) stdweight(std_weight) over(sex)                      /*Table 2 */
svy:mean htn,stdize(agegroup5) stdweight(std_weight) over(sex income) 				/*Table 3 */
lincom _b[c.htn@1bn.sex#2.income] - _b[c.htn@1bn.sex#1bn.income]
lincom _b[c.htn@1bn.sex#3.income] - _b[c.htn@1bn.sex#1bn.income]
lincom _b[c.htn@2.sex#2.income]-_b[c.htn@2.sex#1bn.income]
lincom _b[c.htn@2.sex#3.income]-_b[c.htn@2.sex#1bn.income]


*********************************************
* Diagnosed hypertension.
*********************************************

svy:mean evertold,stdize(agegroup5) stdweight(std_weight) over(sex)                     /*Table 2 */
svy:mean evertold,stdize(agegroup5) stdweight(std_weight) over(sex income) 				/*Table 3 */
lincom _b[c.evertold@1bn.sex#2.income] - _b[c.evertold@1bn.sex#1bn.income]
lincom _b[c.evertold@1bn.sex#3.income] - _b[c.evertold@1bn.sex#1bn.income]
lincom _b[c.evertold@2.sex#2.income]-_b[c.evertold@2.sex#1bn.income]
lincom _b[c.evertold@2.sex#3.income]-_b[c.evertold@2.sex#1bn.income]


*****************************************
*Estimates among the hypertensive.
*****************************************
tab totalBP sex

generate treated=0
replace treated=1 if (hy140om2==2|hy140om2==3)
generate control=0
replace control=1 if (omsysval<140 & omdiaval<90)
drop std_weight
svy:tab agegroup5 sex if totalBP==1, col format(%8.4f)     

generate std_weight=0
replace std_weight=0.1958 if agegroup5==1 & sex==1
replace std_weight=0.1848 if agegroup5==2 & sex==1
replace std_weight=0.2278 if agegroup5==3 & sex==1
replace std_weight=0.2160 if agegroup5==4 & sex==1
replace std_weight=0.1757 if agegroup5==5 & sex==1
replace std_weight=0.1271 if agegroup5==1 & sex==2
replace std_weight=0.1482 if agegroup5==2 & sex==2
replace std_weight=0.2047 if agegroup5==3 & sex==2
replace std_weight=0.2418 if agegroup5==4 & sex==2
replace std_weight=0.2783 if agegroup5==5 & sex==2

* Management (Table 1).
svy,subpop(totalBP):mean evertold,stdize(agegroup5) stdweight(std_weight)  over(sex)    /*Diagnosed among totalBP*/
estat size
svy,subpop(totalBP):mean treated,stdize(agegroup5) stdweight(std_weight)  over(sex)     /*Tx*/
estat size
svy,subpop(totalBP):mean control,stdize(agegroup5) stdweight(std_weight)  over(sex)     /*Control*/
estat size
svy,subpop(treated):mean control,stdize(agegroup5) stdweight(std_weight)  over(sex)     /*Control among Tx*/
estat size

* By income.
svy,subpop(totalBP):mean evertold,stdize(agegroup5) stdweight(std_weight)  over(sex income)                     
estat size
lincom _b[c.evertold@1bn.sex#2.income] - _b[c.evertold@1bn.sex#1bn.income]
lincom _b[c.evertold@1bn.sex#3.income] - _b[c.evertold@1bn.sex#1bn.income]
lincom _b[c.evertold@2.sex#2.income]-_b[c.evertold@2.sex#1bn.income]
lincom _b[c.evertold@2.sex#3.income]-_b[c.evertold@2.sex#1bn.income]

svy,subpop(totalBP):mean treated,stdize(agegroup5) stdweight(std_weight)  over(sex income)   
estat size
lincom _b[c.treated@1bn.sex#2.income] - _b[c.treated@1bn.sex#1bn.income]
lincom _b[c.treated@1bn.sex#3.income] - _b[c.treated@1bn.sex#1bn.income]
lincom _b[c.treated@2.sex#2.income]-_b[c.treated@2.sex#1bn.income]
lincom _b[c.treated@2.sex#3.income]-_b[c.treated@2.sex#1bn.income]

svy,subpop(totalBP):mean control,stdize(agegroup5) stdweight(std_weight)  over(sex income)                 
estat size
lincom _b[c.control@1bn.sex#2.income] - _b[c.control@1bn.sex#1bn.income]
lincom _b[c.control@1bn.sex#3.income] - _b[c.control@1bn.sex#1bn.income]
lincom _b[c.control@2.sex#2.income]-_b[c.control@2.sex#1bn.income]
lincom _b[c.control@2.sex#3.income]-_b[c.control@2.sex#1bn.income]

* Control among Tx.
svy,subpop(treated):mean control,stdize(agegroup5) stdweight(std_weight)  over(sex income)            
estat size
lincom _b[c.control@1bn.sex#2.income] - _b[c.control@1bn.sex#1bn.income]
lincom _b[c.control@1bn.sex#3.income] - _b[c.control@1bn.sex#1bn.income]
lincom _b[c.control@2.sex#2.income]-_b[c.control@2.sex#1bn.income]
lincom _b[c.control@2.sex#3.income]-_b[c.control@2.sex#1bn.income]

*Biprobit model.
*Men: N=9403; Women: N=11,590.
 
generate base=0
replace base=1 if inrange(eqv3,1,3) & inrange(cigsta3,1,3) & inrange(BMIvg3,1,3) ///
& inrange(diabete2,0,1) & inrange(origin2,1,6) & inrange(marital,1,3) 
tab base sex

preserve
tab sex if income==.
keep if inrange(income,1,3)
generate base2=0
replace base2=1 if inrange(cigsta3,1,3) & inrange(BMIvg3,1,3) & ///
inrange(diabete2,0,1) & inrange(origin2,1,6) & inrange(marital,1,3) 
tab base2 sex
restore

************************
**save dataset for MI.
************************

save "C:\Learning\Papers\BiProbit\Dataset_forMI.dta", replace


*complete cases for main analysis
keep if base==1                     
tab1 sex

* Hypertension (total and diagnosed).

summ age
generate agesq = age*age

************************************************************************
*Male.
*REF: Never smokers; normal-weight; not diabetic; married; highest-income.
************************************************************************

svy,subpop(male):biprobit (totalBP=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(evertold=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)
*AME (difference in probability).
*total HTN
margins if male==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1) predict(pmarg1)               
margins if male==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)              
margins if male==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                
margins if male==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg1)                 
margins if male==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg1)                
margins if male==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg1)           

*diagnosed (NOT conditional)
margins if male==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1) predict(pmarg2)                 
margins if male==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                
margins if male==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                 
margins if male==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg2)                 
margins if male==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg2)                
margins if male==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg2)             

**********************************************************
* Undiagnosed hypertension (conditional on total=1).
*********************************************************8

generate undiagnosed=evertold
recode undiagnosed (1=0)(0=1)
svy:mean undiagnosed if totalBP==1

svy,subpop(male):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(undiagnosed=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)
margins if male==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1) predict(pcond2)                 
margins if male==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
margins if male==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
margins if male==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pcond2)                 
margins if male==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pcond2)                 
margins if male==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pcond2)                 

*Check with the marginal and the joint probabilities.
*Joint Probabilities.
*margins if male==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p11)
*margins if male==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p10)
*margins if male==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p01)
*margins if male==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p00)

*********************.
* Women.
*********************.

*Hypertension (total and diagnosed).

svy,subpop(female):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(evertold=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)

margins if female==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1) predict(pmarg1)               
margins if female==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)               
margins if female==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                 
margins if female==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg1)               
margins if female==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg1)                 
margins if female==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg1)               

margins if female==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1) predict(pmarg2)                
margins if female==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                
margins if female==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                
margins if female==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg2)                 
margins if female==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg2)                 
margins if female==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg2)               

svy,subpop(female):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(undiagnosed=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)
margins if female==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1) predict(pcond2)                 
margins if female==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
margins if female==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
margins if female==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pcond2)                 
margins if female==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pcond2)                 
margins if female==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pcond2)                 

*Check with the marginal and the joint probabilities.
*Joint Probabilities.
*margins if female==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p11)
*margins if female==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p10)
*margins if female==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p01)
*margins if female==1, at((mean) age agesq income=(1 2 3) cigsta3=3 BMIvg3=1 marital=2 diabete2=0) predict(p00)


*************************************************************
*Sensitivity analysis.
*Standard Probit Model (Table S3).
*Outcome: is diagnosed hypertension.
*************************************************************

generate f1=0
replace f1=1 if (male==1 & totalBP==1)

svy,subpop(f1): probit evertold cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital
margins if f1==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1)                
margins if f1==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2)                
margins if f1==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2)                  
margins if f1==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2)                
margins if f1==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2)               
margins if f1==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0)              

generate g1=0
replace g1=1 if (female==1 & totalBP==1)

svy,subpop(g1): probit evertold cons c.age c.agesq i.income 
svy,subpop(g1): probit evertold cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital
margins if g1==1, dydx(age) at((mean) agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2 income=1)                
margins if g1==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2)                
margins if g1==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2)                  
margins if g1==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2)                
margins if g1==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2)               
margins if g1==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) 

*****************************
*Add urban/rural: Table S4.
******************************

tab urban14b sex
keep if inrange(urban14b,1,2)

svy,subpop(male):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b) ///
(evertold=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b)
margins if male==1, dydx(income) at((mean) age agesq urban14b=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                 
margins if male==1, dydx(income) at((mean) age agesq urban14b=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                 

svy,subpop(male):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b) ///
(undiagnosed=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b)
margins if male==1, dydx(income) at((mean) age agesq urban14b=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 

svy,subpop(female):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b) ///
(evertold=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b)
margins if female==1, dydx(income) at((mean) age agesq urban14b=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)               
margins if female==1, dydx(income) at((mean) age agesq urban14b=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)               

svy,subpop(female):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b) ///
(undiagnosed=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.urban14b)
margins if female==1, dydx(income) at((mean) age agesq urban14b=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
*****************************
* Add Educational status.
******************************

tab topqual5 sex 
keep if inrange(topqual5,1,5)

svy,subpop(male):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5) ///
(evertold=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5)
margins if male==1, dydx(income) at((mean) age agesq topqual5=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                 
margins if male==1, dydx(income) at((mean) age agesq topqual5=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                 

svy,subpop(male):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5) ///
(undiagnosed=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5)
margins if male==1, dydx(income) at((mean) age agesq topqual5=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 

svy,subpop(female):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5) ///
(evertold=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5)
margins if female==1, dydx(income) at((mean) age agesq topqual5=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)               
margins if female==1, dydx(income) at((mean) age agesq topqual5=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)               

svy,subpop(female):biprobit (totalBP=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5) ///
(undiagnosed=cons c.age c.agesq i.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital ib1.topqual5)
margins if female==1, dydx(income) at((mean) age agesq topqual5=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)              


************************
**Multiple Iimputation.
************************

use "C:\Learning\Papers\BiProbit\Dataset_forMI.dta", clear

*Impute: smoke (70); BMI (2580).

tab marital
replace marital=2 if marital==9
replace diabete2=0 if diabete2==99
replace urban14b=1 if urban14b==99
replace topqual5=1 if topqual5==99
replace genhelf=2 if genhelf==-8
mvdecode cigsta3 BMIvg3,mv(99)
drop if BMIvg3==0

preserve
tab1 income gor1 evertold cigsta3 BMIvg3 marital diabete2 urban14b topqual5 qimd genhelf, missing
restore

generate agesq = age*age
generate undiagnosed=evertold
recode undiagnosed (1=0)(0=1)

set seed 748
mi set mlong
mi register imputed income cigsta3 BMIvg3 
mi svyset [pw=wt_nurse],psu(point1) strata(gor1)
mi impute chain (mlogit) income cigsta3 BMIvg3 = c.age i.sex i.gor1 i.totalBP i.evertold ///
c.wt_int c.wt_nurse i.diabete2 i.urban14b i.topqual5 i.marital i.qimd i.genhelf,add(10) augment

* Male.

mi estimate, cmdok: svy,subpop(male): biprobit (totalBP=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(evertold=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)
mimrgns if male==1, dydx(age) at((mean) agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)               
mimrgns if male==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                 
mimrgns if male==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                 
mimrgns if male==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg1)                 
mimrgns if male==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg1)                 
mimrgns if male==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg1)               

mimrgns if male==1, dydx(age) at((mean) agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)               
mimrgns if male==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                 
mimrgns if male==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                 
mimrgns if male==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg2)                 
mimrgns if male==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg2)                 
mimrgns if male==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg2)                 

mi estimate, cmdok: svy,subpop(male): biprobit (totalBP=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(undiagnosed=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)
mimrgns if male==1, dydx(age) at((mean) agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)               
mimrgns if male==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
mimrgns if male==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
mimrgns if male==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pcond2)                 
mimrgns if male==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pcond2)                 
mimrgns if male==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pcond2)                 

* Female.
mi estimate, cmdok: svy,subpop(female): biprobit (totalBP=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(evertold=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)
mimrgns if female==1, dydx(age) at((mean) agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)               
mimrgns if female==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                 
mimrgns if female==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg1)                 
mimrgns if female==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg1)                 
mimrgns if female==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg1)                 
mimrgns if female==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg1)                 

mimrgns if female==1, dydx(age) at((mean) agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)               
mimrgns if female==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                 
mimrgns if female==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pmarg2)                 
mimrgns if female==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pmarg2)                 
mimrgns if female==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pmarg2)                 
mimrgns if female==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pmarg2)                 

mi estimate, cmdok: svy,subpop(female): biprobit (totalBP=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital) ///
(undiagnosed=cons c.age c.agesq ib1.income ib3.cigsta3 i.BMIvg3 i.diabete2 ib2.marital)
mimrgns if female==1, dydx(age) at((mean) agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)               
mimrgns if female==1, dydx(income) at((mean) age agesq cigsta3=3 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
mimrgns if female==1, dydx(cigsta3) at((mean) age agesq income=1 BMIvg3=1 diabete2=0 marital=2) predict(pcond2)                 
mimrgns if female==1, dydx(BMIvg3) at((mean) age agesq income=1 cigsta3=3 diabete2=0 marital=2) predict(pcond2)                 
mimrgns if female==1, dydx(diabete2) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 marital=2) predict(pcond2)                 
mimrgns if female==1, dydx(marital) at((mean) age agesq income=1 cigsta3=3 BMIvg3=1 diabete2=0) predict(pcond2)                 
















