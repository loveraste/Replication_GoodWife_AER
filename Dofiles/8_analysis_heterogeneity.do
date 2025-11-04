/******************************************************************************
Date Created: September 2024
	 by: Nina Buchmann (ncbuchmann@gmail.com )
Paper: The Good Wife? Reputation Dynamics and Financial Decision-Making Inside the Household
Section: Heterogeneity Results
******************************************************************************/

** PREPARE **
********************************************************************************

** Clear **
set more off
eststo clear

** HETEROGENEITY PLOTS -- FIGURES 11, C.7, C.8 
********************************************************************************

* Using math score (a=0), or random forest measure of expertise (a=1) 

forval a=0/1{
	
	* Define the names
	if `a'==0{
		local measure "math"
		local hete "high_hus_transfers many_children"

	}
	if `a'==1{
		local measure "forest"
		local topfold "02_Online_Appendix/"
		local hete "high_hus_transfers many_children"
	}

	** For each hetoergeneity measure
	foreach het in `hete' { 

		if "`het'"=="high_hus_transfers" {
			local label1 "Low Transfers"
			local label2 "High Transfers"
			if `a' == 1{
			local fign "Figure_C7_"
			}
			if `a' ==0{
			local fign "Figure_11_"
			local topfold "00_Main/"
			}
		}
		if "`het'"=="many_children"{
			local label1 "<=2 Children"
			local label2 ">2 Children"
			if `a' == 1{
			local fign "Figure_C8_B_"
			}
			if `a'==0{
			local fign "Figure_C8_A_"
			local topfold "02_Online_Appendix/"
			}
		}
		forval t=0/1{
				
			u "$cdata/experiment12_cleaned.dta", clear
							
			reg dg_transfer low_MER salience s_low_MER, r
			estimates store e_main
				
			forval ex=1/4{

			* Specify the regressors and heterogeneity variables
				if `ex'==1{
					local data "12"
					local controls "age_reported hus_age_reported education hus_education same hus_same risk hus_risk m r hus_m hus_r years_married  hh_member hh_children hus_w_income_total hus_h_income_total "

					local full "i.hus_enumeratorID i.compensation i.hus_version `controls' *_miss"
					local outcome "dg_transfer"
					local reg "low_MER salience s_low_MER"
					local control "if low_MER==0 & salience==0"
					local main_reg "s_low_MER"
				}
				
				if `ex'==2 | `ex'==3{
					local data "12"
					local controls "age_reported hus_age_reported education hus_education same hus_same risk hus_risk m r hus_m hus_r years_married  MER hh_member hh_children hus_w_income_total hus_h_income_total "
					local full "i.enumeratorID i.compensation `controls' *_miss"
					if `ex'==2{
						local outcome "play"
						local reg "low_qw ch low_qw_ch"
						local control "if low_qw==0 & ch==0"
						local main_reg "low_qw_ch"
					}
					if `ex'==3{
						local outcome "errors_corrected"
						local reg "var_low"
						local control "if var_low==0"
						local main_reg "var_low"
					}
				}
				
				if `ex'==4{
					local data "3"
					local controls "age_reported education m  risk years_married  hh_member hh_children hus_income income  "
					local full "i.enumeratorID i.tc `controls' *_miss"	
					local outcome "wtp"
					local reg "low_m_h_w any_sticker any_sticker_lr"
					local control "if low_m_h_w==0 & any_sticker==0"
					local main_reg "any_sticker_lr"
				}

				* Load data
				if `ex'==4{
					
					gl controls "age_reported education hh_children hh_member m1 m2 m3 m4 m5 m6 m_effort m_w m_h m_h_w m_h_h risk years_married "

					if `a'==1{
					
						** Load training data (experiment 2)
						u "$cdata/experiment12_cleaned.dta", clear

						** Rename variables to their variable names in experiment 3
						ren *_dec_* *_*
						ren purchase_* purchases_*
						ren health_* health_care_*
						ren children_* children_health_care_*
						ren schoolfees_* children_school_fees_*
						ren education_* children_education_*
						ren food_* children_food_*
						ren clothing_* children_clothing_*
						ren household_* household_expenses_*
						ren relatives_* relatives_support_*
						ren other_* other_basics_*

						** Save training data
						tempfile training
						save `training', replace

						** Hyper parameter optimization: Loop over possible combinations of iterations and number of variables to minimize oob error rate
						local controls_count: word count $controls

						** Load optimal hyper parameters (found above)
						u "$cdata/RF_parameters", clear
								
						** Store optimal parameters
						foreach out in oob_error optim_iteration optim_number_vars{
							sum `out'
							local `out'=`r(mean)'
						}
								
						** Load training data
						u `training', clear

						** Train the random forest algorithm with the oob-error minimizing hyper parameters
						rforest qw $controls if qw!=., type(reg) iter(`optim_iteration') numvars(`optim_number_vars') 

						** List important variables
						ereturn list
						matrix vars=e(importance)
						matrix list vars
					}
						
					** Load prediction data
					u "$cdata/experiment3_cleaned.dta", clear
							
					if `a'==1{	
						
						** Prediction
						predict qw

						** Create types to match the distribution in experiment 2
						egen cutoff=pctile(qw), p(50)
						g low_qw=qw<cutoff if qw!=.
						
						** Replace type variables
						replace low_m_h_w=low_qw
						replace free_lr=free*low_m_h_w
						replace info_lr=info*low_m_h_w
						replace free_info_lr=free_info*low_m_h_w
						replace any_sticker_lr=any_sticker*low_m_h_w	
					}
					
					** Rename heterogeneity transfers variable
					if "`het'"=="high_hus_transfers"{
						ren high_transfers high_hus_transfers
					}
				}
				
				** Load data
				if `ex'<=3{
					u "$cdata/experiment`data'_cleaned.dta", clear
				}
				
				* Rename variables for output
				if `ex'==3{
					ren low_qw var_low
				}

				* Create additional heterogeneity variables
				sum hh_children, det
				g many_children=hh_children>=3 if hh_children!=. 
				
				* Create dummies for missingness so we don't lose observations
				foreach var in `controls' {
					sum `var'
					g `var'_miss=`var'==.
					replace `var'=0 if `var'==.
				}	
				
				* Save variables as percentage deviations from the control mean
				sum `outcome' `control' & `het'==`t'
				local mean=`r(mean)'
				reg `outcome' `reg' `full' if `het'==`t', r
				nlcom `main_reg':100*_b[`main_reg']/`mean', post
				scalar e`ex'_`t'=_b[`main_reg']
				est store e`ex'_`t'
			}
		}

		* Output results
		coefplot (e1_0 e2_0 e3_0 e4_0, label(`label1') ciopts(color(dkorange)) mcolor(dkorange) mfc(dkorange) m(diamond) lcolor(dkorange)) (e1_1 e2_1 e3_1 e4_1, label(`label2')  ciopts(color(dkgreen)) mcolor(dkgreen) mfc(dkgreen)), keep(s_low_MER low_qw_ch var_low any_sticker_lr) coeflabels(s_low_MER = "Low MER*Salience (Transfer, %)" low_qw_ch = "Non-Expert*High Cost (Play, %)" var_low = "Non-Expert (Errors Corrected, #)" any_sticker_lr = "Non-Expert*Any Sticker (WTP, MWK)", labsize(medlarge)) xline(0, lcolor(black)) yline(1.5 3.5, lcolor(black)) text(0.55 -51 "Experiment 1" 1.6 -51 "Experiment 2" 3.6 -51 "Experiment 3", place(e) color(black)) hor graphregion(color(white)) legend(size(large))  bgcol(white) byopts(xrescale) xsize(8) name(het`het'_`measure', replace)
		graph export "$graphs/`topfold'`fign'`het'_`measure'.png", as(png) replace width(5000)
	}
}

eststo clear	

