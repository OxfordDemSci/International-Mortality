cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
clear all


*** 1. Import the datafile from https://www.who.int/data/data-collection-tools/who-mortality-database
insheet using Morticd9.csv, clear


*** 2. Keep the country-years of interest
gen keep = inlist(country, 2090, 2450, 3160, 4010, 4020, 4030, 4045, 4050, 4070, 4080, 4085, 4150, 4180, 4210, 4220, 4230, 4240, 4270, 4274, 4276, 4280, 4290, 4300, 4308, 5020)
keep if keep==1 & year>=1990
drop admin1 subdiv im_* deaths1 keep


*** 3. Reshape death counts from wide to long
isid country year cause sex

* sum up deaths at ages 1-4
tab frmat
misstable summarize deaths3-deaths6
egen deaths27 = rowtotal(deaths3-deaths6) // sum up deaths at 1-4; rowtotal treats missing as 0
drop deaths3-deaths6
rename deaths27 deaths3

* reshape
reshape long deaths, i(country year cause sex frmat) j(age)
drop if (age==24 | age==25) & frmat!=0 // Age groups 90-94 & 95+ only used when frmat==0. frmat means age-group format. Please refer to the WHO documentation for detailed info.


*** 4. Clean age & sex
tab sex
label define sex 1 "male" 2 "female" 9 "unspecified"
label values sex sex

replace age = age-4 //comment needed what this line does 
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


*** 5. Classification of causes of death
misstable summarize deaths if age!=20 & age!=21 // no missing values on deaths 

gen cause_new = .

#delimit;
label define cause
1 "Infectious and parasitic" /* B01, B02, B03, B04, B05, B06, B07 */
2 "HIV/AIDS" /* B184-B185 */
3 "Respiratory" /* B31, B32 */
4 "Trachea/bronchus, lung cancers" /* B101 */
5 "All other cancers" /* B08, B09, B100, B109, B11, B12, B13, B14, B15, B16, B17 */
6 "Nervous system" /* B22 */
7 "Metabolic" /* B180, B181, B182-B183, B189, B19 *///what was removed here?
8 "Cardiovascular disease" /* B25-B30 */
9 "Suicide" /* B54 */
10 "Homicide" /* B55 */
11 "Transport accidents" /* B47 */
12 "Other external causes of death" /* B480-B482, B49, B50, B51, B52, B53, B56 */
13 "All other causes" 
99 "All causes combined" ;
#delimit cr
label values cause_new cause                

replace cause_new = 1 if regexm(cause, "^B0[1-7]$")
replace cause_new = 2 if regexm(cause, "B18[45]$")
replace cause_new = 3 if regexm(cause, "B3[12]$")  
replace cause_new = 4 if cause=="B101"
replace cause_new = 5 if regexm(cause, "^B08$") | regexm(cause, "^B09$") | regexm(cause, "^B10[09]") | regexm(cause, "^B1[1-7]$")
replace cause_new = 6 if cause=="B22"
replace cause_new = 7 if regexm(cause, "^B18[01239]$") | cause=="B19"
replace cause_new = 8 if regexm(cause, "B2[5-9]$") | cause=="B30"
replace cause_new = 9 if cause=="B54"
replace cause_new = 10 if cause=="B55"
replace cause_new = 11 if cause=="B47"
replace cause_new = 12 if regexm(cause, "^B48[012]$") | cause=="B49" | regexm(cause, "^B5[0-36]$")
replace cause_new = 99 if cause=="B00" //B00: all causes of death

* aggregate deaths according to the new causes of death
collapse (sum) deaths, by(country year sex age cause_new)
bysort country year sex age: gen cumsum = sum(deaths) // to be used in line 141.

* calculate death counts for "All other causes"
recode cause_new (.=13)
bysort country year sex age: replace deaths = deaths[_N-1] - cumsum[_N-2] if cause_new==13 // subtract all-cause deaths by deaths from the above defined causes
// deaths[_N-1]: all-cause deaths
// cumsum[_N-2]: deaths to all causes expect for the "All other causes" category
sort country year sex age cause_new
drop cumsum
drop if deaths==.


*** 6. Save the harmonized ICD9-coded data (the ICD9-coded section of the ICD9-ICD10-combined dataset)
misstable summarize deaths
rename cause_new cause
save NEW_death_icd9_sec.dta, replace

