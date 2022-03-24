* Encoding: UTF-8.
*SPSS dataset for Inequalities in hypertension.


dataset close all.
GET FILE="S:\FPHS_EPH_HSE_Shared\7 Research Papers\Shaun\Archive datasets\hse2011ai.sav"
/keep = everbp qimd bp1 eqv3 bp1 hserial pserial age sex wt_int cluster
gor1 ag16g10 eqv5 wt_nurse omsysval omdiaval psu  cigsta3 qimd pregbp eqv3 origin bmival diabete2 
genhelf everbp longill convdoc lastyr ageinfbp medcinbp
ageinfbp medcinbp stillbp fintabc1 fintabc2 fintabc3 fintabc4 fintabc5 fintabc6 advicebp
ADBPC1 ADBPC2 ADBPC3 ADBPC4 ADBPC5 ADBPC6
ADBPC7 bprespc bpmedc bpmedd omsyst omdiast topqual3 origin hy140om2
medbi01 medbi02 medbi03 medbi04 medbi05 medbi06 medbi07 medbi08 medbi09 medbi10
medbi11 medbi12 medbi13 medbi14 medbi15 medbi16 medbi17 medbi18 medbi19 medbi20 medbi21 medbi22
ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 medcnjd nuroutc bsoutc gor1 topqual3 lastdoc Urban
maritalb marstatc.

rename variables (ageinfbp = agebp).
exe.

*bpmedc (drugs affecting BP).
*bpmedd (drugs specifically prescribed BP).
* need bpmedd2 to derive hy140om2.

DO REPEAT xxdrug2=diur2 beta2 aceinh2 calciumb2 obpdrug2 lipid2 iron2 bpmedc2 bpmedd2.
COMPUTE xxdrug2=0.
RECODE medcnjd(-9 thru -1=COPY) INTO xxdrug2.
END REPEAT.
DO REPEAT xxcode2=medbi01 medbi02 medbi03 medbi04 medbi05 medbi06 medbi07 medbi08 medbi09 medbi10
medbi11 medbi12 medbi13 medbi14 medbi15 medbi16 medbi17 medbi18 medbi19 medbi20 medbi21 medbi22.
IF xxcode2=0 diur2=-9.
IF xxcode2=0 beta2=-9.
IF xxcode2=0 aceinh2 =-9.
IF xxcode2=0 calciumb2 =-9.
IF xxcode2=0 iron2 =-9.
IF xxcode2=0 lipid2 =-9.
IF xxcode2=0 obpdrug2 =-9.
IF xxcode2=0 bpmedc2=-9.
IF xxcode2=0 bpmedd2=-9.
END REPEAT.
DO REPEAT xxcode2=medbi01 medbi02 medbi03 medbi04 medbi05 medbi06 medbi07 medbi08 medbi09 medbi10
medbi11 medbi12 medbi13 medbi14 medbi15 medbi16 medbi17 medbi18 medbi19 medbi20 medbi21 medbi22.
IF RANGE(xxcode2,20201,20208) diur2=1.
IF xxcode2=20400 beta2=1.
IF RANGE(xxcode2, 020551, 020553) aceinh2=1.
IF xxcode2=20602 calciumb2=1.
IF ANY(xxcode2,20501,20502,20503,20504,20506) obpdrug2=1.
IF ANY(xxcode2,21200, 21201, 21202) lipid2=1.
IF xxcode2=90101 iron2=1.
END REPEAT.
IF ANY(1,diur2,beta2,aceinh2,calciumb2,obpdrug2) bpmedc2=1.
COUNT xbpdrug2=ytake012 ytake022 ytake032 ytake042 ytake052 ytake062 ytake072 ytake082
ytake092 ytake102 ytake112 ytake122 ytake132 ytake142 ytake152 ytake162 ytake172
ytake182 ytake192 ytake202 ytake212 ytake222 (1).
IF ANY(1,diur2,beta2,aceinh2,calciumb2,obpdrug2) & xbpdrug2>0 bpmedd2=1.
exe.

VARIABLE LABELS diur2 "(D) Diuretics (Blood pressure) {revised}".
VARIABLE LABELS beta2 "(D) Beta blockers (Blood pressure/Fibrinogen) {revised}".
VARIABLE LABELS aceinh2 "(D) Ace inhibitors (Blood pressure) {revised}".
VARIABLE LABELS calciumb2 "(D) Calcium blockers (Blood pressure) {revised}".
VARIABLE LABELS obpdrug2 "(D) Other drugs affecting BP {revised}" .
VARIABLE LABELS lipid2 "(D) Lipid lowering (Cholesterol/Fibrinogen) - prescribed {revised}" .
VARIABLE LABELS iron2 "(D) Iron deficiency (Haemoglobin/Ferritin) {revised}" .
VARIABLE LABELS bpmedc2 "(D) Whether taking drugs affecting blood pressure {revised}".
VARIABLE LABELS bpmedd2 "(D) Whether taking drugs prescribed for blood pressure {revised}".
VALUE LABELS diur2 beta2 aceinh2 calciumb2 obpdrug2 lipid2 iron2 bpmedc2 bpmedd2
0 'Not taking drug'
1 'Taking drug'.
EXECUTE.
fre bpmedd2 bpmedd.

cro/tables =medcnjd by bpmedd.
cro/tables =medcnjd by bpmedd2.

*bpmedd2 is better (no for medcnjd =0).

sel if age>=16.
exe.
fre sex.
fre nuroutc.

* c = affecting BP.
* d = prescribed for blood pressure (reason for taking).

missing values all ().
exe.
recode origin (1 thru 4 = 1) (14 thru 16=2) (5 thru 8=4) (9 thru 13=3) (17 thru 18=5) (lo thru -1 = copy) into origin2.
variable labels origin2 "(D) grouped ethnic categories".
val labels origin2 1 "White" 2 "Black" 3 "Asian" 4 "Mixed" 5 "Any other".
cro/tables = origin by origin2.

* based on taking drugs prescribed.

recode bprespc (2 thru 5,-1=-1) (-6,-2 = copy) (6=-7) into hy140om2x.
Do if bprespc=1.
if any(bpmedd2,0,-1) & range(omsysval,0,139.999) & range(omdiaval,0,89.9999)  hy140om2x=1. 
if bpmedd2=1 & range(omsysval,0,139.999) & range(omdiaval,0,89.9999)  hy140om2x=2.
if bpmedd2=1 & ((omsysval>=140) | (omdiaval>=90))  hy140om2x=3.
if any(bpmedd2,0,-1) &  (omsysval>=140 |omdiaval>=90) hy140om2x=4.
end if.
IF (bpmedd2 = -9) hy140om2x = -9.
exe.
cro/tables = hy140om2x by hy140om2.

missing values bmival ().
exe.

RECODE bmival (0 thru 18.5=1)(18.5 thru 25=2)(25 thru 30=3) (30 thru 40=4) (40 thru hi=5) (lo thru -1=9) INTO bmivg52.
If age<16 bmivg52=-1.
VARIABLE LABELS bmivg52 "(D) Valid BMI (grouped:<18.5,18.5-25,25-30,30-40 40+) estimated weight if>200kg".
VALUE LABELS bmivg52
1 "Under 18.5"
2 "18.5 and below 25"
3 "25 and below 30"
4 "30 and below 40"
5 "Over 40"
9 "Missing".
exe.

missing values marstatc ().
exe.

compute marital=0.
if  marstatc=1 marital=1.
if any(marstatc,2,3,7) marital=2.
if any(marstatc,4,5,6) marital=3.
if marstatc<0 marital=9.
val labels marital
1 "single"
2 "married"
3 "separated/divorced/widowed"
9 "missing".
exe.

compute strata = cluster.
exe.

select if age>=16.
exe.

*select if (omsysval>0 & omdiaval>0) & (bp1=1|bp1=2).
*exe.

compute highBP_survey=0.
if (hy140om2=2|hy140om2=3|hy140om2=4) highBP_survey=1.
variable label highBP_survey "survey-defined hypertension".
exe.

*numeric to string.

FORMAT psu (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2011".
exe.
fre year1.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
exe.


compute nurse=0.
if nuroutc=81 nurse=1.
exe.
compute blood=0.
if bsoutc=1 blood=1.
exe.

compute urban14b=0.
if urban=1 urban14b=1.
if any(urban,2,3) urban14b=2.
value labels urban14b 
1 "Urban"
2 "Town and Fringe/Hamlet".
exe.

compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
save outfile = "N:\Temp\File1.sav".

* 2012.

dataset close all.
GET FILE="S:\FPHS_EPH_HSE_Shared\7 Research Papers\Shaun\Archive datasets\hse2012ai.sav"
/keep pserial age sex wt_int cluster psu
gor1 ag16g10 eqv5 wt_nurse
omsysval omdiaval hy140om2 cigsta3 everbp docbp qimd docbp pregbp othbp eqv3 origin bmival diabete2 genhelf
everbp docbp pregbp medbp evermed othadv 
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06 whttrt95 convdoc lastyr limlast ill12m agebp
AgeBP MedBP BPStill EverMed
stpmed01 stpmed02 stpmed03 stpmed04 stpmed05 stpmed95 OthAdv
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06
whttrt95 topqual3 origin bpmedd2 bprespc omsyst omdiast nuroutc bsoutc gor1 topqual3 urban marstatc maritalb.

select if age>=16.
exe.

missing values all ().
exe.
recode origin (1 thru 4 = 1) (14 thru 16=2) (5 thru 8=4) (9 thru 13=3) (17 thru 18=5) (lo thru -1 = copy) into origin2.
variable labels origin2 "(D) grouped ethnic categories".
val labels origin2 1 "White" 2 "Black" 3 "Asian" 4 "Mixed" 5 "Any other".
cro/tables = origin by origin2.

recode bprespc (2 thru 5,-1=-1) (-6,-2 = copy) (6=-7) into hy140om2X.
Do if bprespc=1.
if any(bpmedd2,0,-1) & range(omsyst,0,139.999) & range(omdiast,0,89.9999)  hy140om2X=1. 
if bpmedd2=1 & range(omsyst,0,139.999) & range(omdiast,0,89.9999)  hy140om2X=2.
if bpmedd2=1 & ((omsyst>=140) | (omdiast>=90))  hy140om2X=3.
if any(bpmedd2,0,-1) &  ((omsyst>=140) | (omdiast>=90)) hy140om2X=4.
end if.
IF (bpmedd2 = -9) hy140om2x = -9.
fre hy140om2 hy140om2x.

missing values bmival ().
exe.

RECODE bmival (0 thru 18.5=1)(18.5 thru 25=2)(25 thru 30=3) (30 thru 40=4) (40 thru hi=5) (lo thru -1=9) INTO bmivg52.
If age<16 bmivg52=-1.
VARIABLE LABELS bmivg52 "(D) Valid BMI (grouped:<18.5,18.5-25,25-30,30-40 40+) estimated weight if>200kg".
VALUE LABELS bmivg52
1 "Under 18.5"
2 "18.5 and below 25"
3 "25 and below 30"
4 "30 and below 40"
5 "Over 40"
9 "Missing".
exe.
missing values marstatc ().
exe.
compute marital=0.
if  marstatc=1 marital=1.
if any(marstatc,2,3,7) marital=2.
if any(marstatc,4,5,6) marital=3.
if marstatc<0 marital=9.
val labels marital
1 "single"
2 "married"
3 "separated/divorced/widowed"
9 "missing".
exe.

missing values all ().
exe.

RECODE docbp (-9 thru -2=COPY) (1=1) (2=2) (-1=2) INTO bp1.
IF (sex=2 & othbp=2) bp1=2.
IF (ANY(-9,docbp,pregbp,othbp)) bp1=-9.
IF (ANY(-8,docbp,pregbp,othbp)) bp1=-8.
VARIABLE LABEL bp1 "(D) Doctor diagnosed high blood pressure (excluding pregnant)".
VALUE LABELS bp1
1 "Yes"
2 "No".
exe.
select if age>=16.
exe.

compute highBP_survey=0.
if (hy140om2=2|hy140om2=3|hy140om2=4) highBP_survey=1.
variable label highBP_survey "survey-defined hypertension".
exe.

* numeric to string.

FORMAT psu (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2012".
exe.

STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
exe.

compute nurse=0.
if nuroutc=81 nurse=1.
exe.
compute blood=0.
if bsoutc=1 blood=1.
exe.

compute urban14b=0.
if urban=1 urban14b=1.
if any(urban,2,3) urban14b=2.
value labels urban14b 
1 "Urban"
2 "Town and Fringe/Hamlet".
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
fre topqual5.
save outfile = "N:\Temp\File2.sav".

* 2013.
* no marstatc; no maritalb.

dataset close all.
GET FILE="S:\FPHS_EPH_HSE_Shared\7 Research Papers\Shaun\Archive datasets\HSE2013ai.sav"
/keep pserial age sex wt_int psu 
age85 gor1 ag16g10 wt_nurse
omsysval omdiaval hy140om2 cigsta3 everbp docbp qimd eqv3 origin bmival diabete2 genhelf
everbp docbp pregbp medbp evermed othadv 
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06 whttrt95 ndoctalk
ndctk12 pnur Outpat01 OutPat02 OutPat03 OutPat95 convdoc lastyr limlast ill12m agebp
AgeBP MedBP BPStill EverMed stpmed01 stpmed02 stpmed03 stpmed04 stpmed05 stpmed95 OthAdv
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06
whttrt95 bprespc bpmedc2 bpmedd2 omsyst omdiast topqual3 origin nuroutc bsoutc gor1 urban othbp maritald marstatd marstatq.

missing values all ().
exe.

RECODE docbp (-9 thru -2=COPY) (1=1) (2=2) (-1=2) INTO bp1.
IF (sex=2 & othbp=2) bp1=2.
IF (ANY(-9,docbp,pregbp,othbp)) bp1=-9.
IF (ANY(-8,docbp,pregbp,othbp)) bp1=-8.
VARIABLE LABEL bp1 "(D) Doctor diagnosed high blood pressure (excluding pregnant)".
VALUE LABELS bp1
1 "Yes"
2 "No".
exe.

select if age>=16.
exe.

missing values all ().
exe.
recode origin (1 thru 4 = 1) (14 thru 16=2) (5 thru 8=4) (9 thru 13=3) (17 thru 18=5) (lo thru -1 = copy) into origin2.
variable labels origin2 "(D) grouped ethnic categories".
val labels origin2 1 "White" 2 "Black" 3 "Asian" 4 "Mixed" 5 "Any other".

recode bprespc (2 thru 5,-1=-1) (-6,-2 = copy) (6=-7) into hy140om2X.
Do if bprespc=1.
if any(bpmedd2,0,-1) & range(omsyst,0,139.999) & range(omdiast,0,89.9999)  hy140om2X=1. 
if bpmedd2=1 & range(omsyst,0,139.999) & range(omdiast,0,89.9999)  hy140om2X=2.
if bpmedd2=1 & ((omsyst>=140) | (omdiast>=90))  hy140om2X=3.
if any(bpmedd2,0,-1) &  ((omsyst>=140) | (omdiast>=90)) hy140om2X=4.
end if.
IF (bpmedd2 = -9) hy140om2x = -9.
exe.

missing values bmival ().
exe.
RECODE bmival (0 thru 18.5=1)(18.5 thru 25=2)(25 thru 30=3) (30 thru 40=4) (40 thru hi=5) (lo thru -1=9) INTO bmivg52.
If age<16 bmivg52=-1.
VARIABLE LABELS bmivg52 "(D) Valid BMI (grouped:<18.5,18.5-25,25-30,30-40 40+) estimated weight if>200kg".
VALUE LABELS bmivg52
1 "Under 18.5"
2 "18.5 and below 25"
3 "25 and below 30"
4 "30 and below 40"
5 "Over 40"
9 "Missing".
exe.
missing values marstatq ().
exe.
compute marital=0.
if  marstatq=1 marital=1.
if any(marstatq,2,3,7) marital=2.
if any(marstatq,4,5,6) marital=3.
if marstatq<0 marital=9.
val labels marital
1 "single"
2 "married"
3 "separated/divorced/widowed"
9 "missing".
exe.

select if age>=16.
exe.

compute highBP_survey=0.
if (hy140om2=2|hy140om2=3|hy140om2=4) highBP_survey=1.
exe.
variable label highBP_survey "survey-defined hypertension".
exe.

* numeric to string.

FORMAT psu (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2013".
exe.

STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
exe.
compute nurse=0.
if nuroutc=81 nurse=1.
exe.
compute blood=0.
if bsoutc=1 blood=1.
exe.
compute urban14b=0.
if urban=1 urban14b=1.
if any(urban,2,3) urban14b=2.
value labels urban14b 
1 "Urban"
2 "Town and Fringe/Hamlet".
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
save outfile = "N:\Temp\File3.sav".

* 2014.

dataset close all.
GET FILE="S:\FPHS_EPH_HSE_Shared\7 Research Papers\Shaun\Archive datasets\HSE2014ai.sav"
/keep pserial sex wt_int psu 
age85 gor1 ag16g10 wt_nurse
omsysval omdiaval qimd hy140om2 cigsta3 everbp docbp qimd bp1 eqv3  bmival diabete2 genhelf
everbp docbp pregbp medbp evermed othadv 
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06 whttrt95 ndoctalk
ndctk12 pnur Outpat01 OutPat02 OutPat03 OutPat95 convdoc lastyr limlast ill12m agebp
AgeBP MedBP BPStill EverMed stpmed01 stpmed02 stpmed03 stpmed04 stpmed05 stpmed95 OthAdv
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06 whttrt95 topqual3 nuroutc bprespc bsoutc gor1 urban origin3 origin2 marstatd marstatq age90.
select if ag16g10>=1.
exe.
compute age = age90.
exe.
missing values bmival ().
exe.
RECODE bmival (0 thru 18.5=1)(18.5 thru 25=2)(25 thru 30=3) (30 thru 40=4) (40 thru hi=5) (lo thru -1=9) INTO bmivg52.
VARIABLE LABELS bmivg52 "(D) Valid BMI (grouped:<18.5,18.5-25,25-30,30-40 40+) estimated weight if>200kg".
VALUE LABELS bmivg52
1 "Under 18.5"
2 "18.5 and below 25"
3 "25 and below 30"
4 "30 and below 40"
5 "Over 40"
9 "Missing".
exe.
select if ag16g10>=1.
exe.

compute highBP_survey=0.
if (hy140om2=2|hy140om2=3|hy140om2=4) highBP_survey=1.
exe.
variable label highBP_survey "survey-defined hypertension".
exe.

FORMAT psu (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2014".
exe.

STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
exe.
compute nurse=0.
if nuroutc=81 nurse=1.
exe.
compute blood=0.
if bsoutc=1 blood=1.
exe.
compute urban14b=0.
if urban=1 urban14b=1.
if any(urban,2,3) urban14b=2.
value labels urban14b 
1 "Urban"
2 "Town and Fringe/Hamlet".
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
missing values marstatq ().
exe.
compute marital=0.
if  marstatq=1 marital=1.
if any(marstatq,2,3,7) marital=2.
if any(marstatq,4,5,6) marital=3.
if marstatq<0 marital=9.
val labels marital
1 "single"
2 "married"
3 "separated/divorced/widowed"
9 "missing".
exe.
save outfile = "N:\Temp\File4.sav".

* 2015.

dataset close all.
GET FILE="S:\FPHS_EPH_HSE_Shared\7 Research Papers\Shaun\Archive datasets\HSE2015ai.sav"
/keep seriala sex wt_int psu 
gor1 ag16g10 wt_nurse
omsysval omdiaval qimd hy140om2 cigsta3 everbp docbp qimd bp1 eqv3 bmival diabete2 genhelf
everbp docbp pregbp medbp evermed othadv 
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06 whttrt95 ndoctalk 
ndctk12 pnur Outpat01 OutPat02 OutPat03 OutPat95 convdoc lastyr limlast  ill12m agebp
AgeBP MedBP BPStill EverMed
stpmed01 stpmed02 stpmed03 stpmed04 stpmed05 stpmed95 OthAdv
whttrt01 whttrt02 whttrt03 whttrt04 whttrt05 whttrt06
whttrt95 topqual3 bprespc nuroutc bsoutc origin2 gor1 urban14b marstatd Age16g5.
select if ag16g10>=1.
exe.
compute age=0.
exe.
if age16g5=1 age=16.5.
if age16g5=2 age=18.5.
if age16g5=3 age=22.
if age16g5=4 age=27.
if age16g5=5 age=32.
if age16g5=6 age=37.
if age16g5=7 age=42.
if age16g5=8 age=47.
if age16g5=9 age=52.
if age16g5=10 age=57.
if age16g5=11 age=62.
if age16g5=12 age=67.
if age16g5=13 age=72.
if age16g5=14 age=77.
if age16g5=15 age=82.
if age16g5=16 age=87.
if age16g5=17 age=90.
EXECUTE.
missing values bmival ().
exe.
RECODE bmival (0 thru 18.5=1)(18.5 thru 25=2)(25 thru 30=3) (30 thru 40=4) (40 thru hi=5) (lo thru -1=9) INTO bmivg52.
VARIABLE LABELS bmivg52 "(D) Valid BMI (grouped:<18.5,18.5-25,25-30,30-40 40+) estimated weight if>200kg".
VALUE LABELS bmivg52
1 "Under 18.5"
2 "18.5 and below 25"
3 "25 and below 30"
4 "30 and below 40"
5 "Over 40"
9 "Missing".
exe.
select if ag16g10>=1.
exe.

compute highBP_survey=0.
if (hy140om2=2|hy140om2=3|hy140om2=4) highBP_survey=1.
exe.
variable label highBP_survey "survey-defined hypertension".
exe.
FORMAT psu (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2015".
exe.

STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute nurse=0.
if nuroutc=81 nurse=1.
exe.
compute blood=0.
if bsoutc=1 blood=1.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
missing values marstatd ().
exe.
compute marital=0.
if  marstatd=1 marital=1.
if any(marstatd,2,6) marital=2.
if any(marstatd,3,4,5) marital=3.
if marstatd<0 marital=9.
val labels marital
1 "single"
2 "married"
3 "separated/divorced/widowed"
9 "missing".
exe.
save outfile = "N:\Temp\File5.sav".

* 2016.
* No urban.
* no lastyr.

dataset close all.
GET FILE="S:\FPHS_EPH_HSE_Shared\7 Research Papers\Shaun\Archive datasets\hse2016_eul.sav"
/keep seriala sex wt_int psu gor1 ag16g10 wt_nurse omsysval omdiaval qimd hy140om2 cigsta3 everbp docbp qimd bp1 eqv3 bmival 
diabete2 genhelf everbp docbp pregbp limlast  ill12m agebp
AgeBP topqual3 bprespc nuroutc bsoutc urban14b origin2 marstatd age16g5.
select if ag16g10>=1.
exe.
compute age=0.
exe.
if age16g5=1 age=16.5.
if age16g5=2 age=18.5.
if age16g5=3 age=22.
if age16g5=4 age=27.
if age16g5=5 age=32.
if age16g5=6 age=37.
if age16g5=7 age=42.
if age16g5=8 age=47.
if age16g5=9 age=52.
if age16g5=10 age=57.
if age16g5=11 age=62.
if age16g5=12 age=67.
if age16g5=13 age=72.
if age16g5=14 age=77.
if age16g5=15 age=82.
if age16g5=16 age=87.
if age16g5=17 age=90.
EXECUTE.
missing values bmival ().
exe.

RECODE bmival (0 thru 18.5=1)(18.5 thru 25=2)(25 thru 30=3) (30 thru 40=4) (40 thru hi=5) (lo thru -1=9) INTO bmivg52.
VARIABLE LABELS bmivg52 "(D) Valid BMI (grouped:<18.5,18.5-25,25-30,30-40 40+) estimated weight if>200kg".
VALUE LABELS bmivg52
1 "Under 18.5"
2 "18.5 and below 25"
3 "25 and below 30"
4 "30 and below 40"
5 "Over 40"
9 "Missing".
exe.
select if ag16g10>=1.
exe.

compute highBP_survey=0.
if (hy140om2=2|hy140om2=3|hy140om2=4) highBP_survey=1.
exe.
variable label highBP_survey "survey-defined hypertension".
exe.
FORMAT psu (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2016".
exe.

STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
EXECUTE.
compute nurse=0.
if nuroutc=81 nurse=1.
exe.
compute blood=0.
if bsoutc=1 blood=1.
exe.
compute topqual5=-2.
if topqual3=1 topqual5=1.
if any(topqual3,2,3) topqual5=2.
if any(topqual3,4,5) topqual5=3.
if topqual3=7 topqual5=4.
if any(topqual3,6) topqual5=5.
value labels topqual5
1 "Degree or equivalent"
2 "Below degree"
3 "O level"
4 "No qualifications"
5 "Other".
missing values topqual5 (-2).
missing values marstatd ().
exe.
compute marital=0.
if  marstatd=1 marital=1.
if any(marstatd,2,6) marital=2.
if any(marstatd,3,4,5) marital=3.
if marstatd<0 marital=9.
val labels marital
1 "single"
2 "married"
3 "separated/divorced/widowed"
9 "missing".
exe.
save outfile = "N:\Temp\File6.sav".

* Put the datasets together.
* N=49,818.
* N=49,817: explain that (1 person with data withdrawn).

dataset close all.
get file = "N:\Temp\File1.sav".
add files/file=*/file="N:\Temp\File2.sav".
add files/file=*/file="N:\Temp\File3.sav".
add files/file=*/file="N:\Temp\File4.sav".
add files/file=*/file="N:\Temp\File5.sav".
add files/file=*/file="N:\Temp\File6.sav".
exe.

SAVE TRANSLATE OUTFILE= "N:\ESRA\Analysis Dataset\Analysis_dataset.dta"
  /TYPE=STATA
  /VERSION=8
  /EDITION=SE
  /MAP
  /REPLACE.





























