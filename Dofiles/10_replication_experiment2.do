
** PREPARE **
********************************************************************************

** Clear **
set more off
eststo clear

** EXPERIMENT 2 SCORES BY VERSION (EASY VS HARD) -- TABLE C.4 **
********************************************************************************
u "$cdata/experiment12_cleaned.dta", clear

**# Check 1

drop low_qw 
g low_qw = qw <= 5 if qw !=.

local scores "q1 q2 q3 q4 q5 q6 q hus_q q_w q_h_w update_neg"
local counter = 0
foreach var in `scores'{
	local counter = `counter' + 1
	loc row = "Row_`counter'_"
    forval i=0/1{
		estpost sum `var' if hv==`i'
		matrix sd`i'=e(sd)
		matrix list sd`i'
		}
	estpost ttest `var' if play!=., by(hv) // ttest by signal
	est store score`var'
	estadd matrix sd0 
	estadd matrix sd1
	estout score`var' /// export one latex table row per variable
	using "$tables/02_Online_Appendix/Table_C4/`row't_`var'.tex", label replace ///
	cells("mu_1(fmt(3)) sd0(fmt(3)) mu_2(fmt(3)) sd1(fmt(3)) b(fmt(3))") style(tex) ///
	mlabels(, none) collabels(, none)  ///
	eqlabels(, none) 
}
eststo clear

** PLAYING BY RISK PREFERENCES -- FIGURE C.2 ** 
********************************************************************************
binscatter play risk, xtitle("Reported Risk Preference") ytitle("Participate (%)") 
graph export "$graphs/02_Online_Appendix/Figure_C2_play_risk.png", as(png) replace

** FOREGONE EARNINGS BY PERCEIVED SCORE AND HIDING COST -- FIGURE C.3 ** 
********************************************************************************
g qw_round=round(qw)
binscatter foregone qw_round, by(ch) discrete xlabel(0(1)6) ylabel(0(50)250) xtitle("Mean Prior Score") ytitle("Total Foregone Earnings (MWK)") legend(label(1 "Low Hiding Cost") label(2 "High Hiding Cost")) graphregion(color(white)) m(circle diamond) 
graph export "$graphs/02_Online_Appendix/Figure_C3_foregone_score.png", as(png) replace

** REGRESSION RESULTS -- TABLE A.3, TABLE C.5, TABLE C.6, AND TABLE C.10**
********************************************************************************

local controls "age_reported hus_age_reported education hus_education w_income_total h_income_total same hus_same risk hus_risk m r hus_m hus_r years_married hh_children hh_member MER"

local outcomes "q play foregone_un q errors_corrected foregone_errors q_corrected foregone"

local sparse "i.enumeratorID i.compensation"	
local full "`sparse' `controls' *_miss"

* Impute missing controls
foreach var in `controls' {
	g `var'_miss=`var'==.
	replace `var'=0 if `var'==.
}	
	
* Define the specifications to be run
local regress1 "low_qw c2 c3 low_qw_c2 low_qw_c3"
local regress2 "hv c2 c3 hv_c2 hv_c3"
local regress3 "low_qw c2 c3 low_qw_c2 low_qw_c3"
local regress4 "hv c2 c3 hv_c2 hv_c3"
local regress5 "low_edu c2 c3 low_edu_c2 low_edu_c3"
local regress6 "low_selfesteem c2 c3 low_selfesteem_c2 low_selfesteem_c3"
local regress7 "low_q c2 c3 low_q_c2 low_q_c3"
local regress8 "low_accuracy c2 c3 low_accuracy_c2 low_accuracy_c3"

forval r=1/8{
	* Extract the regressors in each specification: r=1: market expert vs non-expert, r=2: hard vs easy, r=3: general ability expert vs. non-expert
	local type: word 1 of `regress`r''
	local interaction: word 4 of `regress`r''
	local interaction2: word 5 of `regress`r''
		
	* Define table names 
	if `r'==1 | `r'==3{
		local table "perceived_score"
		local panel "Panel_A"
	}
	if `r'==2 | `r'==4{
		local table "hard_version"
		local panel "Panel_B"
	}
	if `r'==5{
		local table "education"
		local panel "Panel_A"
	}
	if `r'==6{
		local table "selfesteem"
		local panel "Panel_B"
	}
	if `r'==7{
		local table "score"
		local panel "Panel_A"
	}
	if `r'==8{
		local table "accuracy"
		local panel "Panel_B"
	}
	
	* Define table labels
	if `r'==1 | `r'==3 | `r'==7 {
		local control_label "Low Cost \& Expert"
		local p_label "Expert vs. Non-Expert, Intermediate Cost"
		local p_label2 "Expert vs. Non-Expert, High Cost"
	}
	if `r'==2 | `r'==4 {
		local control_label "Low Cost \& Easier Version"
		local p_label "Easier vs. Harder Version, Intermediate Cost"
		local p_label2 "Easier vs. Harder Version, High Cost"
	}
	if `r'==5 {
		local control_label "Low Cost \& High Education"
		local p_label "High vs. Low Education, Intermediate Cost"
		local p_label2 "High vs. Low Education, High Cost"
	}
	if `r'==6 {
		local control_label "Low Cost \& High Self-Esteem"
		local p_label "High vs. Low Self-Esteem, Intermediate Cost"
		local p_label2 "High vs. Low Self-Esteem, High Cost"
	}
	if `r'==8 {
		local control_label "Low Cost \& Accurate Beliefs"
		local p_label "High vs. Low Accuracy, Intermediate Cost"
		local p_label2 "High vs. Low Accuracy, High Cost"
	}
	
	* Define the controls: 
	if `r'!=3 & `r'!=4{
		local control "sparse"
	}
	if `r'==3 | `r'==4{
		local control "full"
	}
	if `r'<3{
		local tabnum "01_Appendix/Table_A3/"
	}
	if `r'>=3 & `r'<5{
		local tabnum "02_Online_Appendix/Table_C5/"
	}
	if `r'>=5 & `r'<7{
		local tabnum "02_Online_Appendix/Table_C6/"
	}
	if `r'>=7{
		local tabnum "02_Online_Appendix/Table_C10/"
	}
	
	local l=0	
	foreach outcome in `outcomes'{

		preserve

		local l=`l'+1
		
		* In the sample of participating women, replace the score with missing for those who did not participate
		if "`outcome'"=="q" & `l'!=1 {
			replace q=. if play==0	
		}
	
		* Run the regression
		eststo reg`l': reg `outcome' `regress`r'' ``control'', r
		* Extract the regression results
		matrix values=r(table)
		* Test whether results are sig. different between NE and E women when the cost of hiding is high 
		test `type'=0
		local pval1=r(p)
		test `type'+`interaction'=0
		local pval2=r(p)
		estadd scalar p_val=r(p)
		test `type'+`interaction2'=0
		estadd scalar p_val2=r(p)
		local pval3=r(p)
		
		* Extract the control mean
		sum `outcome' if `type'==0 & c1==1
		estadd scalar control_mean=r(mean)
		local T00`outcome'`r'=r(mean)
		qui: local N=e(N)
		
		* Define names -- 1st digit is whether the woman is Non-Expert=1, 2nd digit is whether she is Medium-cost=1, High-cost=2
		local name1 "10"
		local name2 "01"
		local name3 "02"
		local name4 "11"
		local name5 "12"
		
		* Store outcomes for figures 
		* Extract and store the standard error and the p-value (Only do for non-expert vs expert for the non-play outcomes)
		forval i=1/5{
			if (`r'==1 & ("`outcome'"=="play" | "`outcome'"=="errors_corrected")) | ("`outcome'"=="foregone") {
					
				* Simple coefficients	
				if `i'<=3{				
					local T`name`i''`outcome'`r'=`T00`outcome'`r''+values[1,`i']
					local T`name`i''`outcome'`r'_se=values[2,`i']
				}
		
				* Create the standard errors for the interaction term
				if `i'>=4{
					if `i'==4 {
						lincom `type'+c2+`interaction'
					}
					if `i'==5 {
						lincom `type'+c3+`interaction2'
					}
					local T`name`i''`outcome'`r'=`T00`outcome'`r''+`r(estimate)'
					local T`name`i''`outcome'`r'_se=`r(se)'
				}

				* Save the confidence intervals
				local Th`name`i''`outcome'`r'=`T`name`i''`outcome'`r''+invttail(`N'-1,0.025)*`T`name`i''`outcome'`r'_se'
				local Tl`name`i''`outcome'`r'=`T`name`i''`outcome'`r''-invttail(`N'-1,0.025)*`T`name`i''`outcome'`r'_se'
			}
		}
		
		local p10`outcome'`r'=`pval1'
		local p01`outcome'`r'=1
		local p02`outcome'`r'=1
		local p11`outcome'`r'=`pval2'
		local p12`outcome'`r'=`pval3'

		restore
	}
	
	* Output the tables
	estout reg* ///
		using "$tables/`tabnum'`panel'_`table'_`control'.tex", label replace cells(b(fmt(3)) se(par fmt(3))) style(tex) ///
		mlabels(, none) collabels(, none) ///
		eqlabels(, none) ///
		stats(control_mean p_val p_val2, fmt(3 3 3) ///
		labels("Mean (`control_label')" "P-value (`p_label')"  "P-value (`p_label2')")) /// 
		keep(`type' c2 c3 `interaction' `interaction2') noomitted 
		eststo clear
}

** REGRESSION RESULTS -- FIGURE 5, FIGURE 6, AND FIGURE 7 **
********************************************************************************

* Output the figure
local outcome1 "play"
local outcome2 "errors_corrected"
local outcome3 "foregone"

forval l=1/3{
	local fignum = `l'+4
	
	forval r=1/2{
		cap drop T00`outcome`l''`r'
		g T00`outcome`l''`r'=`T00`outcome`l''`r''
		forval i=1/5{
			* Store control mean, coefficient, confidence intervals and p-values
			if (`r'==1 & `l'<=2) | `l'==3 {
				foreach stat in T Th Tl{
					cap drop `stat'`name`i''`outcome`l''`r'
					cap g `stat'`name`i''`outcome`l''`r'=``stat'`name`i''`outcome`l''`r''
				}
				cap drop p`name`i''`outcome`l''`r'
				cap g p`name`i''`outcome`l''`r'=""
				if `p`name`i''`outcome`l''`r''<0.1{
					cap replace p`name`i''`outcome`l''`r'="*  "
				}
				if `p`name`i''`outcome`l''`r''<0.05{
					cap replace p`name`i''`outcome`l''`r'="** "
				}
				if `p`name`i''`outcome`l''`r''<0.01{
					cap replace p`name`i''`outcome`l''`r'="***"
				}
			}
		}
	}

	* Reshape the data	
	preserve
	if `l'<=3{
		keep *`outcome`l''1
		ren *`outcome`l''1* **
	}
	duplicates drop
	g n=1
	if `l'<=3{
		reshape long T0 T1 Th0 Tl0 Th1 Tl1 p1 p0, i(n) j(cost)
		drop n
		reshape long T Th Tl p, i(cost) j(type)
	}
	drop if T==.
	* Define the figure labels
	if `l'<=3{
		g stype=type if cost==0
		replace stype=type+2.5 if cost==1
		replace stype=type+5 if cost==2
		sort stype
		local xlabel `"0.5 "Low" 3 "Intermediate" 5.5 "High" "'
		local max=7
		local rows=1
		local type1 "Expert" 
		local type2 "Non-Expert"
		local xline ""
		if `l'==1{
			g p_x=stype+0.36
			g p_y=T+1.3
			local ylabel "Play (%)"
			local scale "50(10)90"
			format T %5.0f
		}
		if `l'==2{
			g p_x=stype+0.42
			g p_y=T+0.032
			local ylabel "Errors Corrected"
			local scale "0(0.2)1"
			format T %5.1f
		}
			if `l'==3{
			g p_y=T+5.6
			g p_x=stype+0.35
			local ylabel "Total Foregone (MWK)"
			local scale "0(20)160"
			format T %5.0f
			local xline "xline(8.625, lc(black))" 
		}
	}


	* Export the graphs
	twoway (bar T stype if type==0) ///
		(bar T stype if type==1) ///
		(rcap Th Tl stype, color(black))  ///
		(scatter T stype, msym(none) mlab(T) mlabpos(1) mlabcolor(black)) ///
		(scatter p_y p_x , msym(none) mlab(p) mlabpos(0) mlabcolor(black) mlabsize(small)), ///
		legend(row(1) order(1 "`type1'" 2 "`type2'")) ///
		xlabel(`xlabel', noticks) `xline' graphregion(color(white))  ///
		xtitle("Hiding Cost") ytitle("`ylabel'") ylabel(`scale') name(experiment2_`l', replace)
		graph export "$graphs/03_Replication/Figure_`fignum'_`outcome`l''.png", as(png) replace
		if `l'==3{
		twoway (bar T stype if type==0) ///
			(bar T stype if type==1) ///
			(rcap Th Tl stype, color(black))  ///
			(scatter T stype, msym(none) mlab(T) mlabpos(11) mlabcolor(black)) ///
			(scatter p_y p_x , msym(none) mlab(p) mlabpos(0) mlabcolor(black) mlabsize(small)), ///
			legend(row(1) order(1 "`type1'" 2 "`type2'")) ///
			xlabel(`xlabel', noticks) `xline' graphregion(color(white))  ///
			xtitle("Hiding Cost") ytitle("`ylabel'") ylabel(`scale') name(experiment2_`l', replace)
			graph export "$graphs/03_Replication/Figure_`fignum'_`outcome`l''.png", as(png) replace
		}
	restore
}

