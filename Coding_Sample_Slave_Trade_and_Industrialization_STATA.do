* This is a do-file conducting country-level OLS and IV estimations of the slave trade's impact on contemporary industrialization.

global data_dir = "/Users/changjay/Dropbox/Economic History Seminar/Research/data/"
global result_dir = "/Users/changjay/Dropbox/Economic History Seminar/Research/results/"

use "$data_dir/Nathan_slavetrade.dta", clear
merge 1:1 country using "$data_dir/world_bank_indicators.dta"

/*
Potential dependent variables to try: 
Employment in industry in 2000 (%): indemp2000
Employment in agriculture in 2000 (%): agremp2000
Proportion of population with access to electricity in 2000 (%): electricity
Newly infected HIV cases in 2000: 
*/

global depvar = "indemp2000"





***** Summary Statistics
estpost sum $depvar ln_export_area abs_latitude longitude rain_min humid_max low_temp ln_coastline_area atlantic_distance_minimum indian_distance_minimum saharan_distance_minimum red_sea_distance_minimum if indemp2000 != . , detail
eststo sumstat

esttab sumstat using "$result_dir/sum_stat_country.tex", cell("count(fmt(0)) mean(fmt(4)) sd(fmt(4)) p10(fmt(4)) p50(fmt(4)) p90(fmt(4))") replace





***** Graphical correlatioins
*** country-level slave exports and industrialization
twoway (scatter $depvar ln_export_area, mlabel(isocode) msymbol(i)) (lfit indemp2000 ln_export_area), legend(off) xtitle("Log(exports/area)") ytitle("Share of Employment in Industrial Sectors in 2000") graphregion(margin(large) fcolor(white) lcolor(white))
graph save "$result_dir/${depvar}_slave_export", replace
graph export "$result_dir/${depvar}_slave_export.jpg", replace



*** country-level slave trade distance and slave exports (first-stage)
twoway (scatter ln_export_area atlantic_distance_minimum, mlabel(isocode) msymbol(i)) (lfit ln_export_area atlantic_distance_minimum), legend(off) xtitle("Distances to the Destinations of the Trans-Atlantic Slave Trade") ytitle("Log(exports/area)") graphregion(margin(large) fcolor(white) lcolor(white))
graph save "$result_dir/slave_export_Trans-atlanticDistance", replace
graph export "$result_dir/slave_export_Trans-atlanticDistance.jpg", replace

twoway (scatter ln_export_area indian_distance_minimum, mlabel(isocode) msymbol(i)) (lfit ln_export_area indian_distance_minimum), legend(off) xtitle("Distances to the Destinations of the Indian Ocean Slave Trade") ytitle("Log(exports/area)") graphregion(margin(large) fcolor(white) lcolor(white))
graph save "$result_dir/slave_export_IndianOceanDistance", replace
graph export "$result_dir/slave_export_IndianOceanDistance.jpg", replace

twoway (scatter ln_export_area red_sea_distance_minimum, mlabel(isocode) msymbol(i)) (lfit ln_export_area red_sea_distance_minimum), legend(off) xtitle("Distances to the Destinations of the Red Sea Slave Trade") ytitle("Log(exports/area)") graphregion(margin(large) fcolor(white) lcolor(white))
graph save "$result_dir/slave_export_RedSeaDistance", replace
graph export "$result_dir/slave_export_RedSeaDistance.jpg", replace

twoway (scatter ln_export_area saharan_distance_minimum, mlabel(isocode) msymbol(i)) (lfit ln_export_area saharan_distance_minimum), legend(off) xtitle("Distances to the Destinations of the Trans-Saharan Slave Trade") ytitle("Log(exports/area)") graphregion(margin(large) fcolor(white) lcolor(white))
graph save "$result_dir/slave_export_Trans-SaharanDistance", replace
graph export "$result_dir/slave_export_Trans-SaharanDistance.jpg", replace





***** Industrial employment on slave trades
*** sd, 25, and 75 percentile for the calculation of "implied effects"
* full sample
scalar sd = r(sd) // 3.852567
scalar percentile_25 = r(p25) // -0.9844123
scalar percentile_75 = r(p75) // 6.659775

* drop North Africa and island
sum ln_export_area if country != "Algeria" & country != "Cape Verde Islands" & country != "Comoros" & country != "Egypt" & country != "Libya" & country != "Mauritius" & country != "Morocco" & country != "Sao Tome & Principe" & country != "Seychelles" & country != "Tunisia", detail
scalar sd_r = r(sd) // 3.852567
scalar percentile_25_r = r(p25) // -0.9844123
scalar percentile_75_r = r(p75) // 6.659775



*** OLS
est clear
* column (1)
reg $depvar ln_export_area, robust
eststo column1
estadd scalar sd_increase = _b[ln_export_area] * sd
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75 - percentile_25)

* column (2)
reg $depvar ln_export_area colony*, robust
eststo column2
estadd scalar sd_increase = _b[ln_export_area] * sd
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75 - percentile_25)

* column (3)
reg $depvar ln_export_area colony* abs_latitude longitude rain_min humid_max low_temp ln_coastline_area, robust
eststo column3
estadd scalar sd_increase = _b[ln_export_area] * sd
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75 - percentile_25)

* column (4)
reg $depvar ln_export_area colony* abs_latitude longitude rain_min humid_max low_temp ln_coastline_area /// 
if country != "Algeria" & country != "Cape Verde Islands" & country != "Comoros" & country != "Egypt" & country != "Libya" & country != "Mauritius" & country != "Morocco" & country != "Sao Tome & Principe" & country != "Seychelles" & country != "Tunisia" /// // drop North Africa and island
, robust
eststo column4
estadd scalar sd_increase = _b[ln_export_area] * sd_r
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75_r - percentile_25_r)

esttab column1 column2 column3 column4 using "$result_dir/${depvar}_OLS.tex", drop(_cons colony*) starlevels(* 0.10 ** 0.05 *** 0.010) stat(sd_increase percentile_2575_increase r2 N) b(4) se(4) replace





***** IV with distance
est clear
* column (1)
ivreg2 $depvar (ln_export_area = atlantic_distance_minimum indian_distance_minimum saharan_distance_minimum red_sea_distance_minimum), first savefirst savefprefix(first1)
eststo c1
estadd scalar F_second = e(F)
estadd scalar sd_increase = _b[ln_export_area] * sd
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75 - percentile_25)

* column (2)
ivreg2 $depvar colony* (ln_export_area = atlantic_distance_minimum indian_distance_minimum saharan_distance_minimum red_sea_distance_minimum), first savefirst savefprefix(first2)
eststo c2
estadd scalar F_second = e(F)
estadd scalar sd_increase = _b[ln_export_area] * sd
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75 - percentile_25)

* column (3)
ivreg2 $depvar colony* abs_latitude longitude rain_min humid_max low_temp ln_coastline_area (ln_export_area = atlantic_distance_minimum indian_distance_minimum saharan_distance_minimum red_sea_distance_minimum), first savefirst savefprefix(first3)
eststo c3
estadd scalar F_second = e(F)
estadd scalar sd_increase = _b[ln_export_area] * sd
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75 - percentile_25)

* column (4)
ivreg2 $depvar colony* abs_latitude longitude rain_min humid_max low_temp ln_coastline_area /// 
(ln_export_area = atlantic_distance_minimum indian_distance_minimum saharan_distance_minimum red_sea_distance_minimum) /// IV
if country != "Algeria" & country != "Cape Verde Islands" & country != "Comoros" & country != "Egypt" & country != "Libya" & country != "Mauritius" & country != "Morocco" & country != "Sao Tome & Principe" & country != "Seychelles" & country != "Tunisia" /// // drop North Africa and island
, first savefirst savefprefix(first4)
eststo c4
estadd scalar F_second = e(F)
estadd scalar sd_increase = _b[ln_export_area] * sd_r
estadd scalar percentile_2575_increase = _b[ln_export_area] * (percentile_75_r - percentile_25_r)

* second stage results
esttab c1 c2 c3 c4 using "$result_dir/${depvar}_IV_second.tex", drop(_cons colony* abs_latitude longitude rain_min humid_max low_temp ln_coastline_area) starlevels(* 0.10 ** 0.05 *** 0.010) stats(sd_increase percentile_2575_increase F_second r2 N) b(4) se(4) replace

* first stage results
esttab first1ln_export_area first2ln_export_area first3ln_export_area first4ln_export_area using "$result_dir/${depvar}_IV_first.tex", drop(_cons colony* abs_latitude longitude rain_min humid_max low_temp ln_coastline_area) starlevels(* 0.10 ** 0.05 *** 0.010) b(4) se(4) replace











