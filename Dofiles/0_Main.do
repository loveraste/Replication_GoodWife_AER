** PREPARATION **
*******************************************************************************

** Set folder location **
gl loc "C:\Users\Stefany\OneDrive - Universidad EAFIT\2025-2\Replication games\paper\209322-V1\Replication_package\" // PUT THE LOCATION OF THE REPLICATION PACKAGE HERE

** Install commands **
local packages "sutex2 binscatter scheme-burd rforest estout coefplot"
foreach package in `packages' {
	cap which `package'
	if _rc==111 ssc install `package'				
}

** Clear all
clear all
set more off
set mem 1g
version 16

** Define colors **
global lagunita	"0 124 146"
global stanford "177 4 14"
global sky 		"0 152 219"
global poppy 	"233 131 0"
global sun 		"234 171 1"
global mint 	"0 155 118"

** Set theme **
set scheme burd, permanently
graph set window fontface "Century Schoolbook" 							

** Creating Global File Paths **
gl dof "$loc/Dofiles"
gl rdata "$loc/Data/Raw"
gl cdata "$loc/Data/Clean"
gl tables "$loc/Output/Tables"
gl graphs "$loc/Output/Graphs"

** ANALYSIS **
*******************************************************************************
cd "$dof"

do 1_motivating_evidence // Motivating evidence graphs

do 2_clean_experiment12 // Creating variables for experiment 1 and 2

do 3_clean_experiment3 // Creating variables for experiment 3

do 4_summary_statistics // Summary statistics and balance tables

do 5_analysis_experiment1 // Experiment 1 Analyses

do 6_analysis_experiment2 // Experiment 2 Analyses

do 7_analysis_experiment3 // Experiment 3 Analyses

do 8_analysis_heterogeneity // Heterogeneity analysis
