******"DataCleaningPop"

cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
clear all


*** 1. Import the datafile
insheet using NEWpop.csv, clear


*** 2. Keep the country-years of interest
gen keep = inlist(country, 2090, 2450, 3160, 4010, 4020, 4030, 4045, 4050, 4070, 4080, 4085, 4150, 4180, 4210, 4220, 4230, 4240, 4270, 4274, 4276, 4280, 4290, 4300, 4308, 5020)
keep if keep==1 & year>=1990
drop admin1 subdiv frmat pop1 lb keep


*** 3. Reshape pop counts from wide to long
isid country year sex
egen pop27 = rowtotal(pop3-pop6)
drop pop3-pop6
rename pop27 pop3
reshape long pop, i(country year sex) j(age)


*** 4. Clean age & sex
tab sex
label define sex 1 "male" 2 "female" 9 "unspecified"
label values sex sex

replace age = age-4
recode age (-2=1) (-1=2) (22=99)

#delimit;
label define age
0 "0-4"
1 "0"
2 "1-4"
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
label values age age

#delimit;
label define country
2090 "Canada"
2450 "United States of America"
3160 "Japan"
4010 "Austria"
4020 "Belgium"
4030 "Bulgaria"
4045 "Czech Republic"
4050 "Denmark"
4070 "Finland"
4080 "France"
4085 "Germany"
4150 "Hungary"
4180 "Italy"
4210 "Netherlands"
4220 "Norway"
4230 "Poland"
4240 "Portugal"
4270 "Romania"
4274 "Slovakia"
4276 "Slovenia"
4280 "Spain"
4290 "Sweden"
4300 "Switzerland"
4308 "United Kingdom"
5020 "Australia" ;
#delimit cr
label values country country


*** 5. Merge in additional pop data from external sources (i.e., national statistical offices)
tab country year if pop==. & age!=20 & age!=21
gen source = 1

foreach add in NEW_pop_canada NEW_pop_france NEW_pop_us {
append using `add'.dta
}
recode source (.=2)
label define source 1 "WHO" 2 "NSO"
label values source source
sort country year sex age


*** 6. Save the harmonized data
cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
save NEW_pop.dta, replace


*******"MergeAll"

cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
clear all


*** Merge in cleaned population counts. 
***The icd10y.dta file includes information on the year when each country switched from ICD9 to ICD10. I used this to create figures and show that the two coding schemes are compatible/comparable for the causes of death under study. I created the icd10y.dta file based on the WHO availability excel file. The icd10y.dta file is available in GitHub repository. 
foreach var in icd910 icd10 {
use NEW_death_`var', clear
merge m:1 country year sex age using NEW_pop.dta
sort country year sex age cause
drop if _merge==2
drop _merge
merge m:1 country using icd10y.dta, nogen
save NEW_dat_`var', replace
}


