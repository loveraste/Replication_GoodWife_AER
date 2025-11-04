/******************************************************************************
Date Created: September 2024
	 by: Nina Buchmann (ncbuchmann@gmail.com )
Paper: The Good Wife? Reputation Dynamics and Financial Decision-Making Inside the Household
Section: Clean experiment 3 data
******************************************************************************/


** PREPARE **
********************************************************************************

** Clear **
set more off
eststo clear

** IMPORT DATA **
********************************************************************************

** Import data ** 
u "$rdata/experiment3", clear

** CREATE TREATMENT VARIABLES **
********************************************************************************
g any_sticker=free==1 | info==1

** CREATE TYPE VARIABLES **
********************************************************************************
g low_m_h_w=m_h_w<5 if m_h_w!=.

** CREATE TREATMENT INTERACTIONS **
********************************************************************************
* Interactions with free and information
g free_info=free*info
replace free=0 if free_info==1
replace info=0 if free_info==1

* Interactions with math_h_w and ability and both		
foreach var of varlist free info any_sticker free_info {
    g `var'_lr=low_m_h_w*`var' if low_m_h_w!=. & `var'!=.
}

** CREATE HETEROGENEITY VARIABLES **
********************************************************************************
g long_married=years_married>=7 if years_married!=.
sum transfers, det
g high_transfers=transfers>`r(p50)' if transfers!=.

** LABEL VARIABLES **
********************************************************************************
la var any_sticker "Any Sticker"
la var low_m_h_w "Non-Expert"
la var free_info "'Donated'\&'Effectiveness' Stickers"
la var free_lr "'Donated'*Non-Expert"
la var info_lr "'Effectiveness'*Non-Expert"
la var any_sticker_lr"Any Sticker*Non-Expert"
la var free_info "'Donated'\&'Effectiveness' Stickers"
la var free_info_lr "('Donated'\&'Effectiveness')*Non-Expert"
la var long_married "Married for >=7 years"
la var high_transfers "High Transfers"

** EXPORT DATA **
********************************************************************************

** Export data ** 
save "$cdata/experiment3_cleaned", replace
