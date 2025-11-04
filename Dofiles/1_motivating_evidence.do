/******************************************************************************
Date Created: September 2024
	 by: Pascaline Dupas (pascaline.dupas@gmail.com )
Paper: The Good Wife? Reputation Dynamics and Financial Decision-Making Inside the Household
Section: Motivating Evidence
******************************************************************************/

** PREPARE **
********************************************************************************

** Clear **
set more off
eststo clear

** DUPAS 2009 BEDNETS **
********************************************************************************

** IMPORT DATA **
********************************************************************************
u "$rdata/bednets", clear

** PREPARE DATA **
********************************************************************************

* drop those who got net for free

drop if t_price==0

* group based on who was targeted

gen group_A=1 if t_treatF==1
replace group_A=2 if t_treatM==1
replace group_A=3 if t_treatBoth==1

* List of controls
local controls "b_hhsize b_children b_childbelow5 b_educyears_male b_educyears_female b_wealthus b_elec b_bank_account b_anynet b_know_shop b_distfromshop"

* Create globals for the different control specifications
local basic ""
local sparse ""	
local full "`controls'"

* Impute missing controls
foreach var in `controls' {
	g `var'_miss=`var'==.
	replace `var'=0 if `var'==.
}	

** CREATE MEAN GRAPH **
********************************************************************************
	
* COLLAPSE TO PLOT MEAN + 95% CI  
collapse (mean) m_o_purchasednet=o_purchasednet  m_o_hanging=o_hanging (sd) sd_o_purchasednet=o_purchasednet  sd_o_hanging=o_hanging (count) n_o_purchasednet=o_purchasednet  n_o_hanging=o_hanging, by(group_A) 
	
* 95% CI  
foreach var in o_purchasednet  o_hanging   {
	g hi_`var' = m_`var' + invttail(n_`var'-1,0.025)*(sd_`var' / sqrt(n_`var'))
	g low_`var' =  m_`var' - invttail(n_`var'-1,0.025)*(sd_`var' / sqrt(n_`var'))
}
		
tempfile graph_prepare
save `graph_prepare'

* Label variables for graph
lab var m_o_purchasednet "Purchased Net"
lab var m_o_hanging "Using Net"
	
* Graph -- FIGURE 1
	
sort group_A
	
graph twoway (bar m_o_purchasednet group_A if group_A == 1, barw(0.80) bcolor(ltkhaki)) (bar m_o_purchasednet group_A if group_A == 2, bcolor(maroon%90) barw(0.80)) (bar m_o_purchasednet group_A if group_A == 3, barw(0.80) bcolor(ebg)) (rcap hi_o_purchasednet low_o_purchasednet group_A, lcolor(gs5)), legend(off)  xtitle(" ") xscale(lstyle(none)) xlab(1 "Husband alone" 2 "Wife alone" 3 "Both together", labsize(medium) notick  nogrid) title("") name(investment, replace) graphregion(color(white))
	 
graph export "$graphs/00_Main/Figure_1_kenya_investment.png", replace


** ASHRAF ET AL 2010 CLORIN **
********************************************************************************

** IMPORT DATA **
********************************************************************************
use "$rdata/clorin.dta", clear

** PREPARE DATA **
********************************************************************************

* focus on people who have no prior experience with chlorine
keep if base_use==0

* can only do this with people who were willing to pay to start, and were randomized into free vs. not free
keep if bought==1  
gen notfree=second>0	 
gen notfree_married=notfree*married

local controls "first nchild5 base_have"

* Impute missing controls
foreach var in `controls' {
	g `var'_miss=`var'==.
	replace `var'=0 if `var'==.
}	

** GRAPH **
********************************************************************************

* Define the specifications to be run
local regress1 "notfree married notfree_married `controls' *_miss"

replace follow_use=follow_use*100
sum follow_use if notfree==0 & married==0
scalar control_mean=r(mean)

* Regression
eststo reg: reg follow_use notfree married notfree_married `controls' *_miss, r
estadd scalar control_mean
	
matrix values=r(table)
g T00=control_mean
qui: local N=e(N)

* Define names -- 1st digit is whether the woman is notfree, 2nd digit whether she is married 
local name1 "10"
local name2 "01"
local name3 "11"

forval i=1/3{
	* Extract the coefficients
	g T`name`i''=T00+values[1,`i']
	local T`name`i''_se=values[2,`i']
	if `i'==3{
		* Create the confidence interval for the interaction
		lincom married+notfree+notfree_married
		replace T`name`i''=T00+`r(estimate)'
		local T`name`i''_se=`r(se)'
	}
}

test notfree=0
local p01=r(p)
test notfree_married=0
local p11=r(p)
local p10=1

forval i=1/3{
	* Save the confidence intervals
	g Th`name`i''=T`name`i''+invttail(`N'-1,0.025)*`T`name`i''_se'
	g Tl`name`i''=T`name`i''-invttail(`N'-1,0.025)*`T`name`i''_se'
}

* Output the figure
preserve
keep T0* T1* Th* Tl*
* Store the pvalues
forval i=1/3{
	qui: g p`name`i''=""
	if `p`name`i'''<0.1{
		qui: replace p`name`i''="*"
	}
	if `p`name`i'''<0.05{
		qui: replace p`name`i''="**"
	}
	if `p`name`i'''<0.01{
		qui: replace p`name`i''="***"
	}
}
* Reshape the data
keep T0* T1* Th* Tl* p*
duplicates drop
g n=1
reshape long T0 Th0 Tl0 T1 Th1 Tl1 p1 p0, i(n) j(married)
drop n
reshape long T Th Tl p, i(married) j(notfree)
drop if T==.
* Save the lables
g stype=notfree if married==0
replace stype = notfree+2.5 if married==1
g type=notfree

sort stype
format T %5.0f
g p_y=T+0.015
g p_x=stype+0.26

* Output the graph -- FIGURE 2
twoway (bar T stype if type==0, bcolor(ltkhaki)) ///
	(bar T stype if type==1, bcolor(maroon%90)) ///
	(scatter T stype, msym(none) mlab(T) mlabpos(1) mlabcolor(black)) ///
	(scatter p_y p_x , msym(none) mlab(p) mlabpos(0) mlabcolor(black) mlabsize(small)), ///
	legend(row(1) order(1 "Free" 2 "Not Free")) xscale(r(0(1)4)) ///
	xlabel(0.5 "Single" 3 "Married", noticks)  graphregion(color(white))  ///
	xtitle("") ytitle("Usage") ylabel(0.3(0.1)0.6) name(zambia, replace)
	graph export "$graphs/00_Main/Figure_2_zambia_usage2.png", as(png) replace
		
