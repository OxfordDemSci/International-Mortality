cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
clear all


*** 1. Import and append all ICD10-coded datafiles
local datafiles: dir . files "NEWMorticd10_part*.csv"
foreach file of local datafiles {
preserve
insheet using `file', clear
save temp,replace
restore
append using temp, force
}
erase temp.dta


*** 2. Keep the country-years of interest
gen keep = inlist(country, 2090, 2450, 3160, 4010, 4020, 4030, 4045, 4050, 4070, 4080, 4085, 4150, 4180, 4210, 4220, 4230, 4240, 4270, 4274, 4276, 4280, 4290, 4300, 4308, 5020)
keep if keep==1 & year>=1990
drop admin1 subdiv im_* deaths1 keep


*** 3. Reshape death counts from wide to long
isid country year cause sex

* sum up deaths at ages 1-4
tab frmat
drop if frmat==9 // drop mortality data not disaggregated by age
misstable summarize deaths3-deaths6 
egen deaths27 = rowtotal(deaths3-deaths6)
drop deaths3-deaths6
rename deaths27 deaths3

* reshape
reshape long deaths, i(country year cause sex frmat) j(age)
drop if (age==24 | age==25) & frmat!=0


*** 4. Clean age, sex, & country
tab sex
label define sex 1 "male" 2 "female" 9 "unspecified"
label values sex sex

replace age = age-4
recode age (-2=1) (-1=2) (22=99)


/*this part may not be necessary
*** 5. Append USA 2018 and 2019 deaths 
append using USA_deaths_2018_2019.dta
*/


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


*** 6. Classification of causes of death
misstable summarize deaths if age!=20 & age!=21 // have missing values on deaths
// Portugal 2004-2005 because they used ICD10 special list (drop these country-years later)

* for the ICD10-coded dataset
gen cause_new = .

#delimit; 
label define cause
1 "Infectious and parasitic" /* A00-A99, B00-B19, B25-B99 */
2 "HIV/AIDS" /* B20-B24 */
3 "Respiratory" /* J00-J98 */
4 "Trachea/bronchus, lung cancers" /* C33-C34 */
5 "All other cancers" /* C00-D48, excluding C33-C34 */
6 "Nervous system" /* G00-G98 */
7 "Metabolic" /* E00-E88 */
8 "Cardiovascular disease" /* I00-I09, I11, I16-I19, I20-I99 */
9 "Suicide" /* X60–X84, Y87.0 */
10 "Drug poisoning" /* F11-16, F18-19, X40–X44, X85, Y10–Y14 */
11 "Alcohol-induced" /* K70, K73-74, F10, X45, Y15 */
12 "Homicide" /* X86-X99, Y00-Y09, Y87.1 */
13 "Transport accidents" /* V01–V99, Y85 */
14 "Other external causes of death" /* W00–W99, X00–X39, X46–X59, Y16–Y36, Y40–Y84, Y86, Y87.2, Y88, Y89, Y90-Y98 */
15 "All other causes" 
99 "All causes combined" ;  
#delimit cr
label values cause_new cause    

replace cause_new = 1 if regexm(cause, "^[AB][0-9]") // B20-B24 will be automatically removed when we run the next line
replace cause_new = 2 if regexm(cause, "^B2[0-4]")
replace cause_new = 3 if regexm(cause, "^J[0-9]") 
replace cause_new = 4 if regexm(cause, "^C3[34]")
replace cause_new = 5 if cause_new==. & (regexm(cause, "^C[0-9]") | regexm(cause, "^D[0-4]")) // liver cancer C22 included 
replace cause_new = 6 if regexm(cause, "^G[0-9]")
replace cause_new = 7 if regexm(cause, "^E")
replace cause_new = 8 if regexm(cause, "^I0") | regexm(cause, "^I1[16-9]") | regexm(cause, "^I[2-9]") 
replace cause_new = 9 if regexm(cause, "^X[67]") | regexm(cause, "^X8[0-4]") | cause=="Y870"
replace cause_new = 10 if regexm(cause, "^F1[1-689]") | regexm(cause, "^X4[0-4]") | regexm(cause, "^X85") | regexm(cause, "^Y1[0-4]")
replace cause_new = 11 if regexm(cause, "^K7[034]") | regexm(cause, "^F10") | regexm(cause, "^X45") | regexm(cause, "^Y15")
replace cause_new = 12 if regexm(cause, "^X8[6-9]") | regexm(cause, "^X9") | regexm(cause, "^Y0") | cause=="Y871"
replace cause_new = 13 if regexm(cause, "^V[0-9]") | regexm(cause,"^Y85") 
replace cause_new = 14 if regexm(cause, "^[WXY]") & cause_new==.
replace cause_new = 15 if cause_new==. & cause!="AAA" 
replace cause_new = 99 if cause=="AAA"

* for the ICD9-ICD10-combined dataset
gen cause_new2 = .

#delimit;
label define cause2 
1 "Infectious and parasitic" /* A00-A99, B00-B19, B25-B99 */
2 "HIV/AIDS" /* B20-B24 */
3 "Respiratory" /* J00-J98 */
4 "Trachea/bronchus, lung cancers" /* C33-C34 */
5 "All other cancers" /* C00-D48, excluding C33-C34 */
6 "Nervous system" /* G00-G98 */
7 "Metabolic" /* E00-E88 */
8 "Cardiovascular disease" /* I00-I09, I11, I16-I19, I20-I99 */
9 "Suicide" /* X60–X84, Y87.0 */
10 "Homicide" /* X86-X99, Y00-Y09, Y87.1 */
11 "Transport accidents" /* V01–V99, Y85 */
12 "Other external causes of death" //this category now includes drugs and alcohol --not sure if Jenn wants that
13 "All other causes" 
99 "All causes combined" ;  
#delimit cr
label values cause_new2 cause2

replace cause_new2 = cause_new if cause_new<=9 // the first 9 causes (up to suicide) are the same
replace cause_new2 = 10 if cause_new==12 // homicide
replace cause_new2 = 11 if cause_new==13 // transport accidents
replace cause_new2 = 12 if inlist(cause_new, 14) 
***replace cause_new2 = 12 if inlist(cause_new, 10, 11, 14) // drug poisoning + alcohol-induced + external causes except suicide, homicide & transport accidents
/* @ Kasia: the current code includes alcohol and drug related deaths to "other 
external causes of death". Just wondering, do you think they're better placed as
"all other causes" or "other external causes of death" (provided we decide not
 to exclude them)? */
replace cause_new2 = 13 if cause_new==15 // all other causes
replace cause_new2 = 99 if cause_new==99 // all causes combined


*** 7. Save the harmonized data
** save the ICD10-coded section of the ICD9-ICD10-combined dataset
preserve
collapse (sum) deaths, by(country year sex age cause_new2)
misstable summarize deaths 
rename cause_new2 cause
save NEW_death_icd10_sec.dta, replace
restore

** save the harmonized ICD10-coded data
preserve
collapse (sum) deaths, by(country year sex age cause_new)
misstable summarize deaths 
rename cause_new cause
cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
save NEW_death_icd10.dta, replace
restore
