*******"Addpop"

cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
clear all


*** 1. Prepare 2006-2019 population estimates for Canada from Statistics Canada
import delimited using pop_canada_raw.csv, rowrange(11:626) colrange(2:4) varnames(nonames) clear
gen sex = _n
recode sex (1/308=1) (else=2)
label define sex 1 "male" 2 "female"
label values sex sex

#delimit;
label define age1
1 "All ages"
2 "0 to 4 years"
3 "5 to 9 years"
4 "10 to 14 years"
5 "15 to 19 years"
6 "20 to 24 years"
7 "25 to 29 years"
8 "30 to 34 years"
9 "35 to 39 years"
10 "40 to 44 years"
11 "45 to 49 years"
12 "50 to 54 years"
13 "55 to 59 years"
14 "60 to 64 years"
15 "65 to 69 years"
16 "70 to 74 years"
17 "75 to 79 years"
18 "80 to 84 years"
19 "85 to 89 years"
20 "90 to 94 years"
21 "95 to 99 years" 
22 "100 years and over" ;
#delimit cr

encode v1, gen(age) label(age1)
replace age=age[_n-1] if age==.
drop v1
label drop age1
drop if age==1
recode age (2=0) (21/22=21) //CAN WE DOUBLE CHECK THIS LINE

#delimit;
label define age2
0 "0-4"
3 "5-9"
4 "10-14"
5 "15-19"
6 "20-24"
7 "25-29"
8 "30-34"
9 "35-39"
10 "40-44"
11 "45-49"
12 "50-54"
13 "55-59"
14 "60-64"
15 "65-69"
16 "70-74"
17 "75-79"
18 "80-84"
19 "85-89/85+"
20 "90-94"
21 "95+" 
99 "unspecified" ;
#delimit cr



label values age age2

rename v2 year
replace v3 = subinstr(v3,",","",.)
destring v3, replace

collapse (sum) pop=v3, by(year sex age)

gen country = 2090
order country year sex age pop
sort country year sex age

save NEW_pop_canada.dta, replace


*** 2. Prepare 2008-2017 population estimates for the USA from US Census Bureau//where is data for 2018-2019 ?
insheet using "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/asrh/nc-est2020-agesex-res.csv", clear

keep if sex!=0 & age!=999

recode age (0=1 "0") (1/4=2 "1-4") (5/9=3 "5-9") (10/14=4 "10-14") (15/19=5 "15-19") ///
(20/24=6 "20-24") (25/29=7 "25-29") (30/34=8 "30-34") (35/39=9 "35-39") (40/44=10 "40-44") ///
(45/49=11 "45-49") (50/54=12 "50-54") (55/59=13 "55-59") (60/64=14 "60-64") (65/69=15 "65-69") ///
(70/74=16 "70-74") (75/79=17 "75-79") (80/84=18 "80-84") (85/89=19 "85-89/85+") (90/94=20 "90-94") ///
(95/100=21 "95+"), gen(agec)

/*collapse (sum) popestimate2010-popestimate2017, by(sex agec)
*/

collapse (sum) popestimate2010-popestimate2019, by(sex agec)
rename agec age

reshape long popestimate, i(sex age) j(year)
rename popestimate pop
save temp.dta, replace

insheet using"https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/national/us-est00int-alldata.csv", clear

keep if (year==2008 | year==2009) & age!=999
recode age (0=1 "0") (1/4=2 "1-4") (5/9=3 "5-9") (10/14=4 "10-14") (15/19=5 "15-19") ///
(20/24=6 "20-24") (25/29=7 "25-29") (30/34=8 "30-34") (35/39=9 "35-39") (40/44=10 "40-44") ///
(45/49=11 "45-49") (50/54=12 "50-54") (55/59=13 "55-59") (60/64=14 "60-64") (65/69=15 "65-69") ///
(70/74=16 "70-74") (75/79=17 "75-79") (80/84=18 "80-84") (85/89=19 "85-89/85+") (90/94=20 "90-94") ///
(95/100=21 "95+"), gen(agec)
collapse (sum) pop1=tot_male pop2=tot_female, by(year agec)
rename agec age

reshape long pop, i(year age) j(sex)
append using temp.dta

label values sex sex
gen country = 2450
order country year sex age pop
sort country year sex age

erase temp.dta
save NEW_pop_us.dta, replace


*** 3. Prepare 2015-2016 population estimates for France from National Institute of Statistics and Economic Studies
import excel "https://www.insee.fr/en/statistiques/pyramide/3312960/xls/pyramides-des-ages_bilan-demo_2019.xls", ///
sheet("France") cellrange(B11:AB215) clear
drop in 102/104
keep B AA AB

gen n = _n
recode n (1/101=1 "male") (else=2 "female"), gen(sex)

replace B=substr(B,1,3) 
destring B, replace
recode B (0=1 "0") (1/4=2 "1-4") (5/9=3 "5-9") (10/14=4 "10-14") (15/19=5 "15-19") ///
(20/24=6 "20-24") (25/29=7 "25-29") (30/34=8 "30-34") (35/39=9 "35-39") (40/44=10 "40-44") ///
(45/49=11 "45-49") (50/54=12 "50-54") (55/59=13 "55-59") (60/64=14 "60-64") (65/69=15 "65-69") ///
(70/74=16 "70-74") (75/79=17 "75-79") (80/84=18 "80-84") (85/89=19 "85-89/85+") (90/94=20 "90-94") ///
(95/100=21 "95+"), gen(age)

collapse (sum) AA AB, by(sex age)

rename AA pop2015
rename AB pop2016
gen n = _n
reshape long pop, i(n) j(year)
drop n

gen country = 4080
order country year sex age pop
sort country year sex age

save NEW_pop_france.dta, replace
