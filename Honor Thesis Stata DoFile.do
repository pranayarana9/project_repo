clear
use "H:\Private\ECON420-Honor Thesis\post april 21 data\HonorThesisPrimData.dta"
xtset ID year

gen capita_contract = (contractvalue1*10000) / population
//gen capita_contract = contractvalue_new / population
gen log_contract_capita = log(capita_contract)

gen fdi_capita = fdi1 / population
//gen fdi_capita = fdi_new / population
gen log_fdi_capita = log(fdi_capita)

//gen log_co2N = log(percapitaco2emissions)
//gen log_co2N = log(co2_wbank)
//gen log_co2N = log(percapitaco2emissions)
//xtreg log_co2N year, fe
//xtreg log_co2copy year,fe
//xtreg log_co2copy year, fe

//xtreg log_co2N year,fe
xtreg log_co2_unreliable year,fe
predict rlog_co2, e

xtreg log_fdi_capita year, fe
predict rlog_fdi_capita, e

xtreg log_contract_capita year, fe
//xtreg log_contractvalue year, fe
predict rlog_contract_capita, e

//xtreg log_gdp_new year, fe 
xtreg log_gdp year, fe
predict rlog_gdp, e

rename rlog_contract_capita CEC 
rename rlog_co2 CO2
rename rlog_gdp GDPC
rename rlog_fdi_capita FDI
rename log_political GOV


pvar CEC CO2, gmmstyle exog(GOV) instl(1/4) 
pvar CEC CO2 if income_level == 0, gmmstyle exog(GOV) instl(1/4) 
pvar CEC CO2 if income_level == 1, gmmstyle exog(GOV) instl(1/4) 
xtunitroot fisher FDI, dfuller lags(1)

xtunitroot ht FDI

pvarsoc CEC FDI CO2 GDPC, maxlag(3) pvaropts(instl(1/4)) 
pvar FDI CEC CO2 GDPC, gmmstyle exog(GOV) instl(1/4) 
//estimates store m1, title(Model 1)
pvargranger
pvarstable, graph
pvarirf , mc(200) oirf byopt(yrescale) porder(FDI CEC GDPC CO2)
asdoc pvarfevd, mc(200) 
//pvarfevd, mc(200)

pvar FDI CEC CO2 GDPC if income_level == 0, gmmstyle exog(GOV) instl(1/4) 
pvar FDI CEC CO2 GDPC if income_level == 0, gmmstyle instl(1/4) 
//estimates store m2, title(Model 2)
pvargranger
pvarstable, graph
//pvarfevd, mc(200)
pvarirf , mc(200) oirf byopt(yrescale) porder(FDI CEC GDPC CO2)
asdoc pvarfevd, mc(200) 

pvar FDI CEC CO2 GDPC if income_level == 1 , gmmstyle exog(GOV) instl(1/4) nest
//estimates store m3, title(Model 3)
pvargranger
pvarstable, graph
//pvarfevd, mc(200)
pvarirf , mc(200) oirf byopt(yrescale) porder(FDI CEC GDPC CO2)
asdoc pvarfevd, mc(200) 

estout m1 m2 m3, cells(b(star fmt(3)) se(par fmt(2))) legend

//___________
xtreg FDI L.FDI L.CEC L.CO2 L.GDPC, fe

pvar FDI CEC CO2 GDPC, gmmstyle instl(1/4) exog(log_political)
pvar FDI CEC CO2 GDPC if income_level == 0 , gmmstyle instl(1/4) 
pvar CEC CO2 GDPC if income_level == 0 , gmmstyle instl(1/4) 
pvarirf , mc(200) oirf byopt(yrescale) porder(CEC GDPC CO2) 
pvarstable, graph
pvarfevd, mc(200)
pvarirf , mc(200) oirf byopt(yrescale) porder(CEC FDI GDPC CO2) 

//gen inter_ =  income_level*CEC
//xtreg CO2 L.CEC L.FDI L.GDPC inter_, fe





//pvar CEC FDI CO2 GDPC, lags(2) instl(1/2) gmmstyle

//gen CEC_inc = CEC * income_level
//gen FDI_inc = FDI * income_level 

//pvar CEC CO2 GDPC, instl(1/4) exog(CEC_inc FDI_inc) gmmstyle

gen ratio = (contractvalue * 10000) / fdi if fdi > 1 & contractvalue != 0


//new scatters
twoway scatter CO2 CEC if income_level == 0 || lfit CO2 CEC if income_level == 0, ytitle("CO2 emissions per capita") xtitle("Chinese engineering contracts per capita")

twoway scatter CO2 CEC if income_level == 1 & CO2 > -1.5 & CO2 <1 & CEC >-6 & CEC < 5 || lfit CO2 CEC if income_level == 1 & CO2 > -1.5 & CO2 <1 & CEC >-6 & CEC < 5, ytitle("CO2 emissions per capita") xtitle("Chinese engineering contracts per capita")

//Importance of CEC
//low income_level
twoway (scatter capita_contract fdi_capita  if income_level == 0 & fdi_capita<1500 ) (function y=x if income_level == 0, range(capita_contract) ytitle("Chinese engineering contract per capita") xtitle("FDI per capita"))

//or
twoway (scatter capita_contract fdi_capita  if income_level == 0 & fdi_capita<1000 & capita_contract <1000) (function y=x if income_level == 0 & fdi_capita<1000 & capita_contract <1000, range(capita_contract) ytitle("Chinese engineering contract per capita") xtitle("FDI per capita"))

//high income
twoway (scatter capita_contract fdi_capita  if income_level == 1 & fdi_capita<3000 & capita_contract <3000) (function y=x if income_level == 1 & fdi_capita<3000 & capita_contract <3000, range(capita_contract) ytitle("Chinese engineering contract per capita") xtitle("FDI per capita"))

//Robustness
//robustness
//resid
xtreg CO2 CEC GDPC 
predict resid_CO2, e
ivreg CEC GDPC (CO2=resid_CO2) if income_level == 0
ivreg CEC GDPC (CO2=resid_CO2) if income_level == 1

xtreg CEC CO2 GDPC
predict resid_CEC, e
ivreg CO2 GDPC (CEC=resid_CEC) if income_level == 0
ivreg CO2 GDPC (CEC=resid_CEC) if income_level == 1