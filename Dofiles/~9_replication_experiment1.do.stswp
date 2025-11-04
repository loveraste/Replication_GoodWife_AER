** CONFIGURACIÓN INICIAL **
********************************************************************************
u "$cdata/experiment12_cleaned.dta", clear
set more off
eststo clear

**# CHECK 1 

* Distribución del MER
histogram MER, discrete freq ///
xlabel(0(1)5) xtitle("Marital Expertise Rating") ytitle("Frequency")
graph export "$graphs/03_Replication/mer_distribution.png", replace

* Define los controles "sparse" como en el script original
local sparse "i.hus_enumeratorID i.compensation i.hus_version"

** COLUMNA 1: MODELO ORIGINAL (Asumiendo low_MER = MER <= 2) **
********************************************************************************
* Se asume que "low_MER" y "s_low_MER" ya existen en el dta
* y que low_MER fue definida como (MER <= 2)

* Calcular media del grupo de control (High MER, No Salience)
sum dg_transfer if salience==0 & low_MER==0
scalar control_mean_orig = r(mean)

* Correr regresión original
eststo R1: reg dg_transfer low_MER salience s_low_MER `sparse', r
estadd scalar control_mean = control_mean_orig

** COLUMNA 2: ROBUSTEZ CON MER <= 1 **
********************************************************************************
preserve
    gen low_MER_1 = (MER <= 1)
    replace low_MER_1 = . if MER == .
    gen s_low_MER_1 = low_MER_1 * salience
    
    * Media del control (MER > 1, No Salience)
    sum dg_transfer if salience==0 & low_MER_1==0
    scalar control_mean_1 = r(mean)
    
    eststo R2: reg dg_transfer low_MER_1 salience s_low_MER_1 `sparse', r
    estadd scalar control_mean = control_mean_1
restore

** COLUMNA 3: ROBUSTEZ CON MER <= 3 **
********************************************************************************
preserve
    gen low_MER_3 = (MER <= 3)
    replace low_MER_3 = . if MER == .
    gen s_low_MER_3 = low_MER_3 * salience
    
    * Media del control (MER > 3, o sea MER=4, No Salience)
    sum dg_transfer if salience==0 & low_MER_3==0
    scalar control_mean_3 = r(mean)
    
    eststo R3: reg dg_transfer low_MER_3 salience s_low_MER_3 `sparse', r
    estadd scalar control_mean = control_mean_3
restore

** COLUMNA 4: MODELO CATEGÓRICO (Base MER = 4) **
********************************************************************************
* Media del control (MER=4, No Salience)
sum dg_transfer if salience==0 & MER==4
scalar control_mean_cat = r(mean)

* Correr regresión con interacciones categóricas
* ib4.MER indica que 4 es la categoría base de MER
* i.MER##i.salience incluye MER, salience, y la interacción
eststo R4: reg dg_transfer ib4.MER##i.salience `sparse', r
estadd scalar control_mean = control_mean_cat

** EXPORTAR LA TABLA DE ROBUSTEZ **
********************************************************************************
* Esta tabla mostrará las 4 regresiones
* Las variables no alineadas (ej. low_MER y 1.MER) aparecerán en filas separadas

estout R1 R2 R3 R4 ///
    using "$tables/03_Replication/Tabla_Robustez_MER.tex", replace label style(tex) ///
    cells(b(fmt(3)) se(par fmt(3))) ///
    mlabels("Original (MER<=2)" "Robustez (MER<=1)" "Robustez (MER<=3)" "Categórico (Base=4)", none) ///
    collabels(, none) eqlabels(, none) ///
    stats(control_mean N, fmt(3 0) labels("Media del Control" "Observaciones")) ///
    keep(low_MER salience s_low_MER low_MER_1 s_low_MER_1 low_MER_3 s_low_MER_3 ///
         **0.MER** 1.MER 2.MER 3.MER 1.salience **0.MER#1.salience** 1.MER#1.salience 2.MER#1.salience 3.MER#1.salience _cons) ///
    order(low_MER low_MER_1 low_MER_3 **0.MER** 1.MER 2.MER 3.MER salience 1.salience ///
          s_low_MER s_low_MER_1 s_low_MER_3 **0.MER#1.salience** 1.MER#1.salience 2.MER#1.salience 3.MER#1.salience _cons) ///
    varlabels(_cons Constant low_MER "Low MER (<=2)" low_MER_1 "Low MER (<=1)" low_MER_3 "Low MER (<=3)" ///
              **0.MER "MER = 0"** 1.MER "MER = 1" 2.MER "MER = 2" 3.MER "MER = 3" salience "Salience" 1.salience "Salience" ///
              s_low_MER "Salience x Low MER (<=2)" s_low_MER_1 "Salience x Low MER (<=1)" s_low_MER_3 "Salience x Low MER (<=3)" ///
              **0.MER#1.salience "MER=0 x Salience"** 1.MER#1.salience "MER=1 x Salience" 2.MER#1.salience "MER=2 x Salience" 3.MER#1.salience "MER=3 x Salience") ///
    noomitted

** CORRER LA REGRESIÓN CON INTERACCIÓN CATEGÓRICA **
********************************************************************************
* Usamos i.MER##i.salience para obtener el efecto de salience 
* en CADA nivel de MER. 'r' es para errores estándar robustos.
reg dg_transfer i.MER##i.salience `sparse', r
eststo model_int

** CALCULAR LOS EFECTOS MARGINALES **
********************************************************************************
* Tal como lo pediste: margins i.MER, dydx(salience)
* Esto calcula el efecto marginal (derivada) de 'salience' 
* en cada uno de los 4 niveles de MER.
margins i.MER, dydx(salience)

** GENERAR EL GRÁFICO **
********************************************************************************
* marginsplot usa los resultados del comando 'margins' anterior.
* Añadimos yline(0) para ver fácilmente qué efectos son distintos de cero.
marginsplot, ///
    yline(0, lpattern(dash) lcolor(black)) ///
    title("") ///
    ytitle("Effect on the percentage transferred") ///
    xtitle("MER level") ///
    name(ME_Plot_MER_Salience, replace)

* Guardar el gráfico
graph export "$graphs/03_Replication/Figure_ME_MER_Salience.png", as(png) replace


**# CHECK 2

** PREPARAR **
********************************************************************************
set more off
eststo clear

* Cargar los datos 
u "$cdata/experiment12_cleaned.dta", clear

* Definir los controles y sparse (basado en el do-file original) 
local controls "age_reported hus_age_reported education hus_education hus_w_income_total hus_h_income_total risk hus_risk years_married hh_children hh_member same hus_same m r hus_m hus_r"
local sparse "i.hus_enumeratorID i.compensation i.hus_version"	

* Variables clave de tu solicitud
local vars_con_missing "age_reported years_married same hus_same"

* CREAR INDICADORES DE MISSING (ANTES DE IMPUTAR)
******************************************************************************
foreach var in `controls' {
	g `var'_miss = (`var' == .)
}	

* TABLA DE DISTRIBUCIÓN DE MISSINGS
table low_MER salience, ///
    statistic(frequency) ///
    statistic(sum age_reported_miss) ///
    statistic(sum years_married_miss) ///
    statistic(sum same_miss) ///
    statistic(sum hus_same_miss)

* IMPUTAR CONTROLES (DESPUÉS DE CREAR INDICADORES)
* Este es el mismo código del do-file original 
foreach var in `controls' {
	replace `var'=0 if `var'==.
}	

* Definir 'full' (ahora incluye los controles imputados y las dummies _miss) 
local full "`sparse' `controls' *_miss"

* Definir variables principales de la regresión
local outcome "dg_transfer"
local base_regs "low_MER salience s_low_MER"


* TABLA DE REGRESIÓN CON 7 ESPECIFICACIONES

* 1. Solo "sparse" (sin controles)
eststo reg1: reg `outcome' `base_regs' `sparse', r

* 2. "Full" con imputaciones (especificación original de reg2)
eststo reg2: reg `outcome' `base_regs' `full', r

* 3. Regresión sin imputación en age_reported (usando el indicador _miss)
eststo reg3: reg `outcome' `base_regs' `full' if age_reported_miss == 0, r

* 4. Regresión sin imputación en years_married
eststo reg4: reg `outcome' `base_regs' `full' if years_married_miss == 0, r

* 5. Regresión sin imputación en hus_same
eststo reg5: reg `outcome' `base_regs' `full' if hus_same_miss == 0, r

* 6. Regresión sin imputación en same
eststo reg6: reg `outcome' `base_regs' `full' if same_miss == 0, r

* 7. Regresión sin NINGUNA imputación (solo obs completas en esas 4 variables)
eststo reg7: reg `outcome' `base_regs' `full' if age_reported_miss==0 & ///
	years_married_miss==0 & hus_same_miss==0 & same_miss==0, r


* EXPORTAR TABLA DE REGRESIÓN
******************************************************************************
estout reg* using "$tables/03_Replication/robustez_imputacion.tex", ///
	replace style(tex) label ///
	cells(b(star fmt(3)) se(par fmt(3))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///  <-- OPCIÓN AÑADIDA AQUÍ
	keep(`base_regs' _cons) ///
	mlabels("1. Sparse" "2. Full (Imputado)" "3. Sin imputar age_r" ///
	"4. Sin imputar years_m" "5. Sin imputar hus_same" ///
	"6. Sin imputar same" "7. Sin imputaciones (n=4)") ///
	collabels(, none) eqlabels(, none) varlabels(_cons Constant) ///
	stats(N, fmt(0) labels("Observaciones")) ///
	noomitted
