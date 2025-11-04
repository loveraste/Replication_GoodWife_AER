/******************************************************************************
Date Created: September 2024
	 by: Nina Buchmann (ncbuchmann@gmail.com )
Paper: The Good Wife? Reputation Dynamics and Financial Decision-Making Inside the Household
Section: Summary statistics and Balance tables
******************************************************************************/

** PREPARE **
********************************************************************************

** Clear **
set more off
eststo clear

* Summary Statistics -- TABLES 1 AND C.9
********************************************************************************
local vars12 "years_married hh_children hus_age_reported hus_education hus_h_income_total h_income_total age_reported education w_income_total hus_w_income_total transfers hus_transfers"
local vars3 "years_married hh_children age_reported education income hus_income transfers"
local caption12 "Couple characteristics, transfer and signaling experiments"
local caption3 "Couple characteristics, market experiment"
local tabnum12 "00_Main/Table_1"
local tabnum3 "02_Online_Appendix/Table_C9"

foreach data in 12 3{
    u "$cdata/experiment`data'_cleaned.dta", clear
	cd "$tables"
	sutex2 `vars`data'', digits(2) varlab caption(`caption`data'') tablabel(characteristics`data') saving(`tabnum`data''_respondent_characteristics_ex`data') placement(H) replace
	cd "$dof"
}

* Balance Tables -- TABLES C.2, C.7, C.8 AND C.11
********************************************************************************
local vars1 "age_reported hus_age_reported education hus_education q hus_q r hus_r m hus_m hus_w_income_total hus_h_income_total risk hus_risk years_married hh_children hh_member hus_transfers"
local stats1 "salience low_MER  s_low_MER"	
local controls1 "i.hus_enumeratorID i.compensation i.hus_version"
local save1 "02_Online_Appendix/Table_C2/"

local vars2 "age_reported hus_age_reported education hus_education q hus_q r hus_r m hus_m w_income_total h_income_total risk hus_risk years_married hh_children hh_member transfers MER"
local stats2 "low_qw c2 c3 low_qw_c2 low_qw_c3"
local controls2 "i.enumeratorID i.compensation"
local save2 "02_Online_Appendix/Table_C7/"

local vars3 "age_reported education m income hus_income risk years_married hh_children hh_member transfers"
local stats3 "low_m_h_w free free_lr info info_lr free_info free_info_lr"
local controls3 "i.enumeratorID"
local save3 "02_Online_Appendix/Table_C11/"

local vars4 "age_reported hus_age_reported education hus_education q hus_q r hus_r m hus_m w_income_total h_income_total risk hus_risk years_married hh_children hh_member transfers MER"
local stats4 "hv c2 c3 hv_c2 hv_c3"
local controls4 "i.enumeratorID i.compensation"
local save4 "02_Online_Appendix/Table_C8/"

forval ex=1/4{

    if `ex'!=3{
		u "$cdata/experiment12_cleaned.dta", clear
	}
	if `ex'==3{
		u "$cdata/experiment3_cleaned.dta", clear
	}
	
	local loop=0
		
	* Unadjusted regressions
	foreach var in `vars`ex''{
		if `loop'==0{
			local action "replace"
		}
		if `loop'!=0{
			local action "append"
		}
		local loop=`loop'+1

		* Saving the output
		local colcounter = 0
		foreach stat in means `stats`ex''{
		local colcounter = `colcounter' + 1
			if "`stat'"=="means"{
				local stat1 "mean"
				local stat2 "sd"
			}
			if "`stat'"!="means"{
				local stat1 "b"
				local stat2 "se"
			}
			estpost sum `var'
			est store `stat'
			
			* Storing mean and standard deviations
			foreach statistic in `stat1' `stat2'{
				if "`stat'"=="means"{
					sum `var'
					scalar `var'_`statistic'=r(`statistic')
					local col = "Col1"
				}
				if "`stat'"!="means"{
					reg `var' `stats`ex'' `controls`ex'', r
					scalar `var'_`statistic'=_`statistic'[`stat']
					local col = "Col`colcounter'"
				}
				est restore `stat'
				estadd scalar `var'_`statistic'
			}
			estout `stat' ///
				using "$tables/`save`ex''`col'_exp`ex'_balance_`stat'.tex", `action' style(tex)  ///
				label cells(none) mlabels(, none) collabels(, none) eqlabels(, none) ///
				stats(`var'_`stat1' `var'_`stat2',labels(, none) layout(@ (@)) fmt(%9.2fc))
		}
	}	
	
	*** Writing var names:
	file open f using "$tables/`save`ex''exp`ex'_balance_vars.tex", wr replace
	foreach var in `vars`ex''{
		local varlab: var label `var'
		file write f "`varlab' \\"  _n
		file write f "\\"_n
	}
	file close f
	
	
	
	* Multiple hypotheses adjustments, calculate q-values
	preserve	
	foreach var in `vars`ex''{
		qui: reg `var' `stats`ex'' `controls`ex'', r 
		foreach stat in `stats`ex''{
			qui: local t = _b[`stat']/_se[`stat']
			qui: g pval`var'_`stat'=2*ttail(e(df_r),abs(`t'))
		}
	}

	qui: keep pval*	
	qui: keep in 1
	qui: xpose, clear
	qui: ren v1 pval
	* Collect the total number of p-values tested
	qui: sum pval
	qui: local totalpvals = r(N)

	* Sort the p-values in ascending order and generate a variable that codes each p-value's rank
	qui: gen int original_sorting_order = _n
	qui: sort pval
	qui: gen int rank = _n if pval~=.

	* Set the initial counter to 1 
	local qval = 1

	* Generate the variable that will contain the BKY (2006) sharpened q-values
	qui: gen bky06_qval = 1 if pval~=.

	* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.
	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		qui: gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		qui: gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		qui: gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		qui: egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		qui: gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		qui: gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		qui: gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		qui: egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		qui: replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		qui: drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}
		
	qui: sort original_sorting_order
	restore
}
