/******************************************************************************
Date Created: September 2024
	 by: Nina Buchmann (ncbuchmann@gmail.com )
Paper: The Good Wife? Reputation Dynamics and Financial Decision-Making Inside the Household
Section: Experiment 1 Results
******************************************************************************/

** PREPARE **
********************************************************************************

** Clear **
set more off
eststo clear

** GOODS EXAMPLE -- TABLE C.1 **
********************************************************************************
u "$cdata/experiment12_cleaned.dta", clear

* Randomly select examples for experiment 1
foreach var in tempted bad_purchase{
	preserve
	keep if hus_`var'_ex!=""
	set seed 070589
	generate u1=runiform()
	sort u1
	g n=_n
	keep if n<=50
	keep hus_`var'_ex
	tempfile
	*export excel using "$tables/Experiment 1/`var'_examples.xls", sheetreplace firstrow(variables) 
	restore
}	

** REGRESSION RESULTS -- TABLE A.2 **
********************************************************************************

* Define the controls
local controls "age_reported hus_age_reported education hus_education hus_w_income_total hus_h_income_total risk hus_risk years_married hh_children hh_member same hus_same m r hus_m hus_r"

local sparse "i.hus_enumeratorID i.compensation i.hus_version"	
local full "`sparse' `controls' *_miss"

* Impute missing controls
foreach var in `controls' {
	g `var'_miss=`var'==.
	replace `var'=0 if `var'==.
}	

* Define the specifications to be run
local regress1 "low_MER salience s_low_MER `sparse'"
local regress2 "low_MER salience s_low_MER `full'"
local regress3 "low_MER salience s_low_MER low_GAR s_low_GAR `sparse'"
local regress4 "low_MER salience s_low_MER low_hus_GAR s_low_hus_GAR `sparse'" 
local regress5 "low_MER salience s_low_MER no_income s_no_income `sparse'" 
local regress6 "low_MER salience s_low_MER few_children s_few_children `sparse'" 

sum hus_income, det
g high_income=hus_income>=r(p50)
replace high_income=. if hus_income==. 
forval r=1/6{
	
	if `r'<=6{
		local outcome "dg_transfer"
	}
	* Extract control means
	if `r'<=2{
	    sum dg_transfer if salience==0 & low_MER==0
	}
	if `r'==3{
	    sum dg_transfer if salience==0 & low_MER==0 & low_GAR==0
	}
	if `r'==4{
	    sum dg_transfer if salience==0 & low_MER==0 & low_hus_GAR==0
	}
	if `r'==5{
	    sum dg_transfer if salience==0 & low_MER==0 & no_income==0
	}
	if `r'==6{
	    sum dg_transfer if salience==0 & low_MER==0 & few_children==0
	}

	scalar control_mean=r(mean)
	* Regression
    eststo reg`r': reg `outcome' `regress`r'', r
	estadd scalar control_mean
	* In the second regression, store data for figure 
	if `r'==1{
		matrix values=r(table)
		g T00=control_mean
		qui: local N=e(N)
		* Define names -- 1st digit is whether the woman is low_MER=1, 2nd digit whether she is salience=1 
		local name1 "10"
		local name2 "01"
		local name3 "11"
		forval i=1/3{
			* Extract the coefficients
			g T`name`i''=T00+values[1,`i']
			local T`name`i''_se=values[2,`i']
			if `i'==3{
			    * Create the confidence interval for the interaction
				lincom low_MER+salience+s_low_MER
			    replace T`name`i''=T00+`r(estimate)'
				local T`name`i''_se=`r(se)'
			}
		}
	}
	test low_MER=0
	if `r'==1{
		local p10=r(p)
	}
	estadd scalar p_val1=r(p)
	test low_MER+s_low_MER=0
	if `r'==1{
		local p11=r(p)
		local p01=1
	}
	estadd scalar p_val2=r(p)

	if `r'==1{
		forval i=1/3{
			* Save the confidence intervals
			g Th`name`i''=T`name`i''+invttail(`N'-1,0.025)*`T`name`i''_se'
			g Tl`name`i''=T`name`i''-invttail(`N'-1,0.025)*`T`name`i''_se'
		}
	}
}

* Output the table	
estout reg* ///
using "$tables/01_Appendix/Table_A2_robustness.tex", label replace cells(b(fmt(3)) se(par fmt(3))) style(tex) ///
mlabels(, none) collabels(, none) ///
eqlabels(, none) ///
stats(control_mean p_val1 p_val2 N, fmt(3 3 3 0) ///
labels("Control Mean" "P-value (Expert vs. Non-Expert, Control)" "P-value (Expert vs. Non-Expert, Salience)" "Observations")) /// 
keep(low_MER salience s_low_MER low_GAR s_low_GAR low_hus_GAR s_low_hus_GAR no_income s_no_income few_children s_few_children) noomitted 
eststo clear

** REGRESSION RESULTS -- FIGURE 4 **
********************************************************************************

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
reshape long T0 Th0 Tl0 T1 Th1 Tl1 p1 p0, i(n) j(salience)
drop n
reshape long T Th Tl p, i(salience) j(type)
drop if T==.
* Save the lables
g stype=type if salience==0
replace stype = type+2.5 if salience==1

sort stype
format T %5.0f
g p_y=T+0.9
g p_x=stype+0.25

* Output the graphs
twoway (bar T stype if type==0) ///
(bar T stype if type==1) ///
(rcap Th Tl stype, color(black))  ///
(scatter T stype, msym(none) mlab(T) mlabpos(1) mlabcolor(black)) ///
(scatter p_y p_x , msym(none) mlab(p) mlabpos(0) mlabcolor(black) mlabsize(small)), ///
legend(row(1) order(1 "High MER" 2 "Low MER")) xscale(r(0(1)4)) ///
xlabel(0.5 "Control" 3 "Salience", noticks)  graphregion(color(white))  ///
xtitle("") ytitle("Transfer (%)") ylabel(50(5)80) name(experiment1_`i', replace)
graph export "$graphs/00_Main/Figure_4_main.png", as(png) replace
restore

** REGRESSION RESULTS -- TABLE A.1 **
********************************************************************************

* Correlations between husband transfers and outcomes

local outcomes "hus_transfers hus_transfers hus_transfers hus_transfers hus_wife_cash"

* Define the regressions
local regress1 "MER2 MER3 MER4 `full'"
local regress2 "low_MER `full'"
local regress3 "MER2 MER3 MER4 low_GAR `full'" 
local regress4 "low_MER low_GAR `full'" 
local regress5 "low_MER low_GAR `full'"

forval i=1/4{
	g MER`i'=MER==`i'
	la var MER`i' "MER=`i'"
}	

forval r=1/5{
    if `r'!=5{
	    local outcome "hus_transfers"
	}
	if `r'==5{
	    local outcome "hus_wife_cash"
	}
	
	eststo reg`r': reg `outcome' `regress`r'', r
	if `r'==1 | `r'==4{
	    sum `outcome' if MER4==1 
	}
	if `r'!=1 & `r'!=4{
	    sum `outcome' if low_MER==0
	}
	estadd scalar control_mean=r(mean)
	}
estout reg* ///
using "$tables/01_Appendix/Table_A1_correlations.tex", label replace cells(b(fmt(3)) se(par fmt(3))) style(tex) ///
mlabels(, none) collabels(, none) ///
eqlabels(, none) ///
stats(control_mean N, fmt(2 0) ///
labels("Control Mean" "Observations")) /// 
keep(MER2 MER3 MER4 low_MER low_GAR) noomitted 
eststo clear

** REGRESSION RESULTS BY INGREDIENT -- TABLE C.3 **
********************************************************************************
local r=0
foreach m_h_w in low_MER hus_bad_purchase hus_tempted i_high_rel_ab i_hus_wife_m_ability low_MER low_MER low_MER{
    
	preserve
	
	local r=`r'+1
	replace low_MER=`m_h_w'
	replace s_low_MER=low_MER*salience
	
	if `r'<=5{
		eststo reg`r': reg dg_transfer salience low_MER s_low_MER `sparse', r
		sum dg_transfer if salience==0 & low_MER==0
	}
	if `r'>5{
	    if `r'==6{
		    keep if MER<=2
		}
		if `r'>=7{
		    local j=`r'-4
			keep if MER==`j'
		}
		eststo reg`r': reg dg_transfer salience `sparse', r
		sum dg_transfer if salience==0
	}	
	estadd scalar control_mean=r(mean)
	
	restore
}
estout reg* ///
using "$tables/02_Online_Appendix/Table_C3_byscore.tex", label replace cells(b(fmt(3)) se(par fmt(3))) style(tex) ///
mlabels(, none) collabels(, none) varlabels(_cons Constant) ///
eqlabels(, none) ///
stats(control_mean N, fmt(3 0) ///
labels("Mean (Control)" "Observations")) /// 
keep(salience low_MER s_low_MER) order(salience low_MER s_low_MER) noomitted
eststo clear
