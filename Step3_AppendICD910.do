*****"AppendICD910"


cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
clear all


*** 1. Append the ICD9 and ICD10 coded subsets
use NEW_death_icd9_sec, clear
append using NEW_death_icd10_sec
sort country year sex age cause

preserve
keep if country==2090 & year>=2006
recode age(1/2=0)
collapse (sum) deaths, by(country year sex age cause)
save temp.dta, replace
restore

drop if country==2090 & year>=2006
append using temp.dta
sort country year sex age cause
erase temp.dta


*** 2. Save the created ICD9-ICD10-combined dataset
cd "/Users/katarzyna/Desktop/STATA Code/Raw data"
save NEW_death_icd910, replace

