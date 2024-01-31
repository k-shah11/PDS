clear

use "PDS.dta", clear

drop if state_name != "AP"

set cformat %9.2f
set pformat %4.2f
set sformat %3.2f

*********Generationg dummies******************

gen land_ownership = whetherownsland
replace land_ownership = 0 if land_ownership == 2

tab socialgrp, gen(caste_dummy)
rename caste_dummy1 ST
rename caste_dummy2 SC
rename caste_dummy3 OBC
rename caste_dummy4 Other_Castes

tab religion, gen(religion_dummy)
rename religion_dummy1 Hindu
rename religion_dummy2 Muslim
rename religion_dummy3 Christian
rename religion_dummy4 Sikh
rename religion_dummy5 Jain
rename religion_dummy6 Other_Religions

///ssc install labutil2
labvars Hindu Muslim Christian Sikh Jain Other_Religions "Hindu" "Muslim" "Christian" "Sikh" "Jain" "Other_Religions"
labvars ST SC OBC Other_Castes "ST" "SC" "OBC" "Other_Castes"
labvars ST SC OBC Other_Castes "ST" "SC" "OBC" "Other_Castes"

tab regularsal_earner, gen(regular_sal_dummy)
rename regular_sal_dummy1 regular_sal_earner
drop regular_sal_dummy2

tab possessrationcard, gen(ration_card)
rename ration_card1 ration_card
drop ration_card2

codebook lightingcode
gen electricity = .
replace electricity = 1 if lightingcode == 5
replace electricity = 0 if lightingcode != 5

///gen count_assets2 = count_assets^2

************ Balance Checks **************************

local vars MPCE_MRP count_assets land_ownership ckg_coalwooddung electricity hhsize ///
 regular_sal_earner edu_hhh SC ST OBC Other_Castes Hindu Muslim Christian Sikh Jain Other_Religions
estpost ttest `vars', by(PDS_RWS) 
esttab ., cells("mu_1 mu_2 p") nonumber title (Table 2)

************ OLS **************************

///ssc install estout, replace

eststo clear
eststo: quietly reg cal_pc_pd PDS_RWS
eststo: quietly reg cal_pc_pd PDS_RWS MPCE_MRP count_assets land_ownership ckg_coalwooddung electricity regular_sal_earner hhsize edu_hhh
eststo: quietly reg cal_pc_pd PDS_RWS MPCE_MRP count_assets land_ownership ckg_coalwooddung electricity regular_sal_earner hhsize edu_hhh SC ST OBC 
esttab, title(Table 1)

************Propensity Score Matching************************

///ssc install psmatch2, replace


///outcome var = is cal_pc_pd

teffects psmatch (cal_pc_pd) (PDS_RWS MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize ///
regular_sal_earner edu_hhh SC ST OBC), atet vce(iid) caliper(0.03)

tebalance summarize ///balance test

psmatch2 PDS_RWS MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
SC ST OBC, out(cal_pc_pd) common logit qui cal(0.03)

pstest, both ///balance test with LR chi-squared

psgraph ///shows enough common support 

///outcome var = calpcpd_cercst

teffects psmatch (calpcpd_cercst) (PDS_RWS MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize ///
regular_sal_earner edu_hhh SC ST OBC), atet vce(iid) caliper(0.03)


///outcome var = prot_pc_pd

teffects psmatch (prot_pc_pd) (PDS_RWS MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize ///
regular_sal_earner edu_hhh SC ST OBC), atet vce(iid) caliper(0.03)


///ssc install asdoc

************Instrumental Variable Estimation************************

///ssc install ivreg2
reg cal_pc_pd PDS_RWS ration_card MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
SC ST OBC ///suggestive evidence of exclusivity restriction -> 
///after controlling for wealth and social status, ownership of ration card has no direct effect on calorie consumption except through PDS usage

reg PDS_RWS ration_card MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
SC ST OBC ///relevance condition 

eststo clear
eststo: quietly ivreg2 cal_pc_pd (PDS_RWS = ration_card) MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
        SC ST OBC, partial (count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
        SC ST OBC) robust
eststo: quietly ivreg2 calpcpd_cercst (PDS_RWS = ration_card) MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
        SC ST OBC, partial (count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
        SC ST OBC) robust
eststo: quietly ivreg2 prot_pc_pd (PDS_RWS = ration_card) MPCE_MRP count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
        SC ST OBC, partial (count_assets land_ownership ckg_coalwooddung hhsize regular_sal_earner edu_hhh ///
        SC ST OBC) robust

esttab, title(Table 10)





		
		

