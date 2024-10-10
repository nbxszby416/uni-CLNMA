
*********************************************************************************************
* 1. IOP dataset
**********************************************************************************************
* We use the new drug code (tt1, tt2):
* 1		Placebo/Vehicle/No treatment
* 2		Apraclonidine
* 3		Brimonidine
* 4		Betaxolol
* 5		Carteolol
* 6		Levobunolol
* 7		Timolol
* 8 	Levobetaxolol
* 9		Brinzolamide
* 10	Dorzolamide
* 11	Bimatoprost
* 12	Latanoprost
* 13	Travoprost
* 14	Tafluprost	
* 15	Unoprostone

*Install packages
*net install StataNMA, from("http://www.mtm.uoi.gr/images/Vienna2015") replace
*net from http://www.mtm.uoi.gr
*net install network_graphs, replace


use "IOP_data.dta",clear

networkplot tt1 tt2, nodew(n) asp(1) title (IOP data network) ///
lab(Placebo Apraclonidine Brimonidine Betaxolol Carteolol Levobunolol Timolol Levobetaxolol Brinzolamide Dorzolamide Bimatoprost Latanoprost Travoprost Tafluprost Unoprostone) ///
nodescale(1) edgescale(1)
graph export "IOP-network graph.png", as(png) replace



*********************************************************************************************
* 2. CP-CPPS dataset
**********************************************************************************************
* 1 Placebo
* 2 Alpha-blockers
* 3 Antibiotics
* 4 Alpha-blockers + Antibiotics
* 5 Anti-inflammatory


use "CP_CPPS_data.dta",clear
gen n = n1 + n2

networkplot treat1 treat2, nodew(n) asp(1) title (CP/CPPS data network) ///
lab(Placebo Alpha-blockers Antibiotics Alpha-blockers+Antibiotics  Anti-inflammatory) ///
nodescale(1.1) edgescale(0.3) 
graph export "CP_CPPS-network graph.png", as(png) replace
