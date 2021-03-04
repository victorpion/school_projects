use "", clear

* Nettoyage des donnees 

destring esp_vie dep_sante  replace dpcomma 
replace esp_vie = round(esp_vie,0.001)
replace dep_sante = round(dep_sante ,0.001)

replace alcool ="." if alcool==".."
destring alcool, replace dpcomma 
replace alcool = round(alcool,0.001)

replace Chom_OIT ="." if Chom_OIT==".."
destring Chom_OIT, replace dpcomma 
replace Chom_OIT = round(Chom_OIT,0.001)

replace school ="." if school==".."
destring school, replace dpcomma 
replace school = round(school,0.001)

replace pollution ="." if pollution==".."
destring pollution, replace dpcomma 
replace pollution= round(pollution,0.001)

/* Descriptive statistic, same methodology for all variables */ 
summarize chomoit if year==2000
summarize chomoit if year==2015

tabstat chomoit if year==2000, by(country)
tabstat chomoit if year==2008, by(country)
tabstat chomoit if year==2013, by(country)
tabstat chomoit if year==2018, by(country) 

/* Graph for life expectancy and unemployment, same methodology*/ 
bysort year : egen m_esp = mean(esp_vie)
bysort year : egen min_esp = min(esp_vie)
bysort year : egen max_esp = max(esp_vie)

twoway (connected m_esp year)(connected min_esp year)(connected max_esp year), ///
ytitle("Life expectancy at birth") xtitle("Year(2000-2018)") ///
title("Evolution of life expectancy over time")  legend(label( 1 " Mean") label(2 "Minimum") label(3 "Maximum")) 

/* GRAPH alcohol consumption */ 
twoway (connected alcool year if country =="IRL")(connected alcool year if country =="FIN") /// 
(connected alcool year if country =="MEX")(connected alcool year if country =="CAN") /// 
(connected alcool year if country =="PRT")(connected alcool year if country =="FRA"), ///  
ytitle("Alcohol consumption ") xtitle("Year(2000-2018)") ///
title("Different trends over time, countries subset") legend(label( 1 "IRL") label(2 "FIN") label(3 "MEX") label(4 "CAN") label(5 "PRT") label(6 "FRA")) 

/* Graph for pollution */ 
bysort year : egen m_pol = mean(pollution)
twoway (connected m_pol year), ///
ytitle("Population exposed to fine particles (PM2.5) in %") xtitle("Year") ///
 legend(label( 1 " Mean")) 

/* Graph for dep_sante */ 
gen sante18 = dep_sante if year==2018
gen sante00 = dep_sante if year==2000
gen sante = sante18/sante00
graph bar (mean) sante, over(country)

/* Correlation */ 
corr esp_vie dep_sante school alcool pollution chomoit 

/* Regression */ 
sort  year

*Generate delayed variables 
by country: gen dep_sante5 = dep_sante[_n-5] 
gen logdep_sante5 = log(dep_sante5)
by country: gen alcool5 = alcool[_n-5] 
by country: gen pollution5 = pollution[_n-5] 
by country: gen school5 = school[_n-5]
by country: gen chomoit5 = chomoit[_n-5] 

tsset country_nb year
xtreg  esp_vie logdep_sante5 alcool5 school5 pollution5 chomoit5 i.year , fe 





