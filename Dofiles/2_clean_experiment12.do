/******************************************************************************
Date Created: September 2024
	 by: Nina Buchmann (ncbuchmann@gmail.com )
Paper: The Good Wife? Reputation Dynamics and Financial Decision-Making Inside the Household
Section: Clean experiment 1 and 2 data
******************************************************************************/

** PREPARE **
********************************************************************************

** Clear **
set more off
eststo clear

** IMPORT DATA **
********************************************************************************

** Import data ** 
u "$rdata/experiment12", clear

** CREATE TYPE VARIABLES **
********************************************************************************

** MER - total score across four questions **
foreach var in hus_bad_purchase hus_tempted hus_wife_m_ability high_rel_ab {
	recode `var' (0=1) (1=0), gen(i_`var')
}
egen MER=rowtotal(i_hus_bad_purchase i_hus_tempted hus_wife_m_ability high_rel_ab), missing
g low_MER=MER<=2 if MER!=.

** GAR, woman's and husbands - Kling Mean Effects of raven and math scores **
local GAR "hus_r_w hus_m_w"
local hus_GAR "hus_r_h hus_m_h"
local groups "GAR hus_GAR"
foreach list in `groups' {
	foreach var in ``list''{
		sum `var'
		g `var'_std=(`var'-`r(mean)')/`r(sd)'
	}
	egen `list'=rowmean(*_std)	
	drop *_std 
	sum `list', det
	g low_`list'=`list'<`r(p50)' if `list'!=.
}

** Weighted average believed score **
g qw=0
forval i=1/6{
	replace qw=qw+`i'*bin`i'
}
replace qw=qw/10 
g low_qw=qw<5 if qw!=.

** Quality score **
sum q, det
g low_q=q<r(p50)

** 2nd order beliefs **
sum q_h_w, det
g low_qhw=q_h_w<r(p50) if q_h_w!=.

** Self-esteem **
g low_selfesteem=qw<q if qw!=. & q!=. 

** Education **
sum education, det
g low_edu=education<r(p50)
sum hus_education, det
g low_hus_edu=hus_education<r(p50)

** Math beliefs **
foreach var of varlist m m_w m_h_w{
	sum `var', det
	g low_`var'=`var'<r(p50)
}

** Accuracy: Wrong beliefs about score **
g q_mistake=abs(qw-q)
sum q_mistake, det
g low_accuracy=q_mistake>r(p50)
drop q_mistake

** Wife believes her husband will update negatively upon reviewing her score **
g update_neg=100*(q_h_w>q_w) if q_h_w!=. & q_w!=. 

** CREATE TREATMENT INTERACTIONS **
********************************************************************************

local hus "hus_"
local wife ""
local spouses `hus' `wife'
token `spouses'
forval spouse=1/2{
	sum ``spouse''transfers, det
	g high_``spouse''transfers=``spouse''transfers>=`r(p50)' if ``spouse''transfers!=.
}
g long_married=years_married>=7 if years_married!=.
g few_children=hh_children<=2 if hh_children!=. 
g no_income=income_yn==0 if income_yn!=. 

** CREATE TYPE INTERACTIONS **
********************************************************************************

* Reputation and Ability Interactions
foreach var of varlist low_GAR low_MER low_hus_GAR few_children no_income{
	g s_`var'=salience*`var' if salience!=. & `var'!=.
}
foreach cost in ch c1 c2 c3{
	foreach type in low_qw hv low_qhw low_q low_accuracy low_edu low_selfesteem low_hus_edu{
		g `type'_`cost'=`type'*`cost' if `type'!=. & `cost'!=.
	}
}

** LABEL VARIABLES **
********************************************************************************
la var i_hus_bad_purchase "High reputation"
la var i_hus_tempted "High reputation"	
la var i_hus_wife_m_ability "Low reputation"
la var i_high_rel_ab "Low reputation"	
la var GAR "GAR"
la var low_GAR "Low GAR"
la var s_low_GAR "Low GAR*Salience"	
la var few_children "Few Children"
la var s_few_children "Few Children*Salience"
la var no_income "No Wife Income"
la var s_no_income "No Wife Income*Salience"
la var MER "MER"
la var low_MER "Low MER"
la var low_qw "Non-Expert"
la var update_neg "W believes H will update negatively (/%)"
la var high_hus_transfers "High Transfers"
la var hus_GAR "Husband's GAR"
la var low_hus_GAR "Low Husband GAR"
la var qw "Perceived Score"
la var low_qhw "Low Perceived Reputation"
la var low_q "Non-Expert"
la var low_accuracy "Inaccurate Beliefs"
la var low_edu "Low Education"
la var low_selfesteem "Low Self-Esteem"
la var low_hus_edu "Low Husband Education"
foreach type in low_qw hv low_qhw low_q low_accuracy low_edu low_hus_edu low_selfesteem{
	local lab: variable label `type'
	la var `type'_ch "`lab'*High Cost"
	la var `type'_c1 "`lab'*High Cost"
	la var `type'_c2 "`lab'*Intermediate Cost"
	la var `type'_c3 "`lab'*High Cost"
}
la var s_low_MER "Low MER*Salience"
la var s_low_hus_GAR "Low Husband GAR*Salience"
la var low_m "Low math score"
la var low_m_w "Low perceived math score"
la var low_m_h_w "Low 2nd-order beliefs"
la var high_transfers "High Transfers"
la var long_married "Couple married for>=7 years"
la var q_h_w "W about H about W"

** EXPORT DATA **
********************************************************************************

** Export data ** 
save "$cdata/experiment12_cleaned", replace
