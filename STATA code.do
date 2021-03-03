clear

// cd "/Users/wyattoh/Library/Mobile Documents/com~apple~CloudDocs/NUS Y5S1/EC4304 Economic and Financial Forecasting/Group Project"

use usd_jpy_vol

***********************
***  Cleaning Data  ***
***********************

** create date variable
gen date = date(datetime, "YMD##")
format date %td

** create time variable
gen time = clock(datetime, "###hm")
format time %tcHH:MM

drop datetime bopen bhigh blow

** combine date and time variable
gen double datetime = date*24*60*60*1000 + time
format datetime %tcNN/DD/CCYY_HH:MM:SS
duplicates report datetime

tsset datetime, delta(1 minute)
tsfill

format datetime %tc

** replace missing dates with date
replace date = dofc(datetime) if date ==.

** replace missing time with time
gen hour = hh(datetime)
gen minute = mm(datetime)
gen seconds = ss(datetime)

gen temp = hms(hour, minute, seconds)
format temp %tcHH:MM
replace time = temp if time == .

drop temp

** interpolate missing data
ipolate bclose datetime, gen(bclose1)

** generate day of week to remove observations when markets are closed
gen dofw = dow(date)
drop if dofw == 6
drop if dofw == 0 & hour < 17	/* drop sunday before 5pm est */
drop if dofw == 5 & hour >= 17	/* drop friday after 5pm est */

** generate day variable month year
gen day = day(date)
gen year = year(date)
gen month = month(date)

** drop data before 4th jan
drop if day < 4 & month == 1 & year == 2015

** drop christmas (eve) & new years (eve)
drop if day == 24 & month == 12 & year == 2015 & hour >=17
drop if day == 25 & month == 12 & year == 2015
drop if day == 31 & month == 12 & year == 2015 & hour >= 17 

drop if day == 1 & month == 1 & year == 2016

drop if day == 24 & month == 12 & year == 2017
drop if day == 25 & month == 12 & year == 2017 & hour < 17
drop if day == 31 & month == 12 & year == 2017

drop if day == 1 & month == 1 & year == 2018 & hour < 17
drop if day == 24 & month == 12 & year == 2018 & hour >= 17
drop if day == 25 & month == 12 & year == 2018 & hour < 17
drop if day == 31 & month == 12 & year == 2018 & hour >= 17

drop if day == 1 & month == 1 & year == 2019 & hour < 17
drop if day == 24 & month == 12 & year == 2019 & hour >= 17
drop if day == 25 & month == 12 & year == 2019 & hour < 17
drop if day == 31 & month == 12 & year == 2019 & hour >= 17

drop if day == 1 & month == 1 & year == 2020 & hour < 17

egen u = seq(), b(1440)

** keep price data of 5 min intervals
keep if inlist(minute, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)

** generate 5-min ln returns
gen ret5min = ln(bclose1/bclose1[_n-1])

egen m = seq(), b(289)

** generating daily returns and realised volatility
egen retdaily = sum(ret5min), by(m) // sum to get daily returns since ln returns can be summed up over time
egen retvar = sum((ret5min)^2), by(m) // proxy for realized variance

collapse date retdaily retvar, by(m)

***********************
*** Plot Data ***
***********************

rename m t

tsset t

** eyeball the plots of daily returns 
tsline retdaily
** graph seems stochastic

** generating standard deviation of realised volatility
gen retvol = sqrt(retvar)

** examine ACF and PACF plots of daily returns 
ac retdaily if t <= 1387
pac retdaily if t <= 1387

** generating squared daily returns and examine their persistence
gen sqretdaily = retdaily^2
tsline sqretdaily
ac sqretdaily if t <= 1387          

** seems that there is autocorrelations up till lag 16,
** try regressing on 16th lag
reg sqretdaily L(1/16).sqretdaily if t <= 1387, r 

** Ljung-Box test on lags 1 to 16
testparm L(1/16).sqretdaily 		
** F-statistic=0.0002 
** can reject null that lags 1 through 16 are jointly insignificant

pac sqretdaily if t <= 1387 

*****************************************
*						 		  		*	
* 	        In-Sample Fit ARCH  		*
*						 				*
*****************************************

***********************
***      ARCH       ***
***********************

** Engle's LM test for ARCH effects, using 8 lags based on PACF of squared daily returns
** estimate the model for the mean: mean+noise
reg retdaily if t <= 1387
** RMSE=0.00552

** run joint test that all 8 AR coefficients are zero for squared returns
archlm, lag(8)
** p-value=0.0000 --> reject null that there are no ARCH effects

** Estimate quietly ARCH(5) through ARCH(11) and save AIC/BIC for model selection
forvalues p = 5/11 {
	qui arch retdaily if t <= 1387, arch(1/`p')	
	estimates store arch`p'
}

estimates stats arch5 arch6 arch7 arch8 arch9 arch10 arch11
** AIC chooses ARCH(10) and BIC chooses ARCH(8)

** look at the estimates of ARCH(10)
arch retdaily if t <= 1387, arch(1/10)
estat ic

** predict residuals and conditional variances
predict e, resid
predict arvar, variance

gen arvol =sqrt(arvar)
tsline retvar arvar if t>1387, lpattern(solid dash) title("Actual vs Forecast for ARCH(10) 1-Step Ahead Pseudo Out-of-Sample")

** generate squared standardized residuals and examine if any ARCH effects remain
gen se=(e^2)/arvar 
ac se
drop se
*************************************
*						 		  	*	
* 	PSUEDO OOS FORECASTING ARCH 	*
*						 			*
*************************************

******** Pseudo 1-step Rolling Window for ARCH(10)
gen rollarvar10_1step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(10)

	predict var_1step, variance dynamic(`a'+1)
	replace rollarvar10_1step = var_1step if t == (`a'+1)

	drop var_1step 
}

gen rollarvol10_1step = sqrt(rollarvar10_1step)

tsline retvar rollarvar10_1step if t>1387, lpattern(solid dash) title("Actual vs Forecast for ARCH(10) 1-Step Ahead Pseudo Out-of-Sample")

******** Pseudo 5-step Rolling Window for ARCH(10)
gen rollarvar10_5step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(10)

	predict var_5step, variance dynamic(`a'+1)
	replace rollarvar10_5step = var_5step if t == (`a'+5)

	drop var_5step 
}

gen rollarvol10_5step = sqrt(rollarvar10_1step)

tsline retvar rollarvar10_5step if t>1387, lpattern(solid dash) title("Actual vs Forecast for ARCH(10) 1-Step Ahead Pseudo Out-of-Sample")

******** Pseudo 22-step Rolling Window for ARCH(10)
gen rollarvar10_22step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(10)

	predict var_22step, variance dynamic(`a'+1)
	replace rollarvar10_22step = var_22step if t == (`a'+22)

	drop var_22step 
}

gen rollarvol10_22step = sqrt(rollarvar10_22step)

tsline retvar rollarvar10_22step if t>1387, lpattern(solid dash) title("Actual vs Forecast for ARCH(10) 1-Step Ahead Pseudo Out-of-Sample")


*****************************************
*						 		  		*	
* 	        FINDING GARCH (P,Q)  		*
*						 				*
*****************************************


***********************
*** GARCH ***
***********************

** estimate GARCH(1,1) model
arch retdaily if t <= 1387, arch(1) garch(1)
estimates store garch11

** predict residuals and conditional variances for GARCH(1,1)
predict eg, resid
predict garvar, variance

** generate squared standardized residuals and examine if any ARCH effects remain
gen seg=(eg^2)/garvar
ac seg

******** 
** run GARCH(1,2), GARCH(2,1), GARCH(2,2) and save AIC/BIC
qui arch retdaily if t <= 1387, arch(1) garch(1/2)	
estimates store garch12

qui arch retdaily if t <= 1387, arch(1/2) garch(1)
estimates store garch21

qui arch retdaily if t <= 1387, arch(1/2) garch(1/2) 
estimates store garch22

estimates stats garch11 garch12 garch21 garch22
** AIC chooses GARCH(1,2) and BIC chooses GARCH(1,1)


*****************************************
*						 		  		*	
* 	PSEUDO OOS FORECASTING GARCH (1,1)	*
*						 				*
*****************************************

***********************
*** GARCH(1,1) ***
***********************

******** Pseudo 1-step Rolling Window for GARCH(1,1)
gen rollvar11_1step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(1) garch(1)

	predict var_1step, variance dynamic(`a'+1)
	replace rollvar11_1step = var_1step if t == (`a'+1)

	drop var_1step 
}

gen rollvol11_1step = sqrt(rollvar11_1step)

tsline retvar rollvar11_1step if t>1387, lpattern(solid dash) title("Actual vs Forecast for GARCH(1,1) 1-Step Ahead Pseudo Out-of-Sample")


******** Pseudo 5-step Rolling Window for GARCH(1,1)
gen rollvar11_5step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(1) garch(1)

	predict var_5step, variance dynamic(`a'+1)
	replace rollvar11_5step = var_5step if t == (`a'+5)

	drop var_5step
}

gen rollvol11_5step = sqrt(rollvar11_5step)

tsline retvar rollvar11_5step if t>1387, lpattern(solid dash) title("Actual vs Forecast for GARCH(1,1) 5-Step Ahead Pseudo Out-of-Sample")


******** Pseudo 22-step Rolling Window for GARCH(1,1)
gen rollvar11_22step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(1) garch(1)

	predict var_22step, variance dynamic(`a'+1)
	replace rollvar11_22step = var_22step if t == (`a'+22)

	drop var_22step
}

gen rollvol11_22step = sqrt(rollvar11_22step)

tsline retvar rollvar11_22step if t>1387, lpattern(solid dash) title("Actual vs Forecast for GARCH(1,1) 22-Step Ahead Pseudo Out-of-Sample")


*****************************************
*						 		  		*	
* 	In-Sample Fit GARCH(1,2) 			*
*						 				*
*****************************************

***********************
*** GARCH(1,2) ***
***********************

** estimate GARCH(1,2) model
arch retdaily if t <= 1387, arch(1) garch(1/2)	//Z: changed from t<1387

predict eg2, resid
predict garvar2, variance 

gen seg2 = (eg2^2)/garvar2
ac seg2

** plot predicted in-sample conditional standard deviations for GARCH(1,2)
gen garvol = sqrt(garvar2)
tsline garvol

** use rolling window because GARCH(1,1) and GARCH(2,1) are nested models

*****************************************
*						 		  		*	
* 	PSEUDO OOS FORECASTING GARCH (1,2) 	*
*						 				*
*****************************************

******** Pseudo 1-step Rolling Window for GARCH(1,2)
gen rollvar12_1step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(1) garch(1/2)

	predict var_1step, variance dynamic(`a'+1)
	replace rollvar12_1step = var_1step if t == (`a'+1)

	drop var_1step
}

gen rollvol12_1step = sqrt(rollvar12_1step)

tsline retvar rollvar12_1step if t>1387, lpattern(solid dash) title("Actual vs Forecast for GARCH(1,2) 1-Step Ahead Pseudo Out-of-Sample")


******** Pseudo 5-step Rolling Window for GARCH(1,2)
gen rollvar12_5step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(1) garch(1/2)

	predict var_5step, variance dynamic(`a'+1)
	replace rollvar12_5step = var_5step if t == (`a'+5)

	drop var_5step
}

gen rollvol12_5step = sqrt(rollvar12_5step)

tsline retvar rollvar12_5step if t>1387, lpattern(solid dash) title("Actual vs Forecast for GARCH(1,2) 5-Step Ahead Pseudo Out-of-Sample")


******** Pseudo 22-step Rolling Window for GARCH(1,2)

gen rollvar12_22step = .

forvalues p = 1/100 {
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day
	
	qui arch retdaily if t >= `z' & t <= `a', arch(1) garch(1/2)

	predict var_22step, variance dynamic(`a'+1)
	replace rollvar12_22step = var_22step if t == (`a'+22)

	drop var_22step
}

gen rollvol12_22step = sqrt(rollvar12_22step)

tsline retvar rollvar12_22step if t>1387, lpattern(solid dash) title("Actual vs Forecast for GARCH(1,2) 22-Step Ahead Pseudo Out-of-Sample")

keep t date retdaily retvar rollvar11_1step rollvar11_5step rollvar11_22step rollvar12_1step rollvar12_5step rollvar12_22step rollarvar10_1step  rollarvar10_5step rollarvar10_22step

*****************************************
*						 		  		*	
* 		HAR-RV(3)						*
*						 				*
*****************************************

gen act = retvar if t > 1387		//training sample ends at t == 1386
label variable act "Actual daily RV"

**generate rolling means**
gen rv5 = .
gen rv22 = .

//5-day means
forvalues p = 1/1483 {
qui gen temp = retvar if t >= `p' & t <= `p'+5-1
egen temp2 = mean(temp)
qui replace rv5 = temp2 if t == `p'+5-1
drop temp*
}

//22-day means
forvalues p = 1/1466 {
qui gen temp = retvar if t >= `p' & t <= `p'+22-1
egen temp2 = mean(temp)
qui replace rv22 = temp2 if t == `p'+22-1
drop temp*
}
 
*****************************************
*						 		  		*	
* 	In-Sample Fit HAR-RV(3)				*
*						 				*
*****************************************


//in-sample fit (non robust)

reg retvar L.retvar L.rv5 L.rv22 if t < 1388
estimates store nr_har

predict nr_harrvfit
predict nr_harrvse, stdf

//Build 68% forecast intervals using normal rule:
gen nr_harrvint1 = nr_harrvfit - 1.645*nr_harrvse
gen nr_harrvint2 = nr_harrvfit + 1.645*nr_harrvse

tsline retvar nr_harrvfit nr_harrvint1 nr_harrvint2, lpattern(solid dash shortdash shortdash)
graph export nr_harrv.tif, replace


//in-sample fit (robust)

rreg retvar L.retvar L.rv5 L.rv22 if t < 1388
estimates store r_har

predict r_harrvfit
predict r_harrvse, stdp

//Build 68% forecast intervals using normal rule:
gen r_harrvint1 = r_harrvfit - 1.645*r_harrvse
gen r_harrvint2 = r_harrvfit + 1.645*r_harrvse

tsline retvar r_harrvfit r_harrvint1 r_harrvint2, lpattern(solid dash shortdash shortdash)
graph export r_harrv.tif, replace

// dmariano retvar r_harrvfit nr_harrvfit, crit(mse) kernel(bartlett) 

*****************************************
*						 		  		*	
* 	PSUEDO OOS FORECASTING 	HAR-RV(3)	*
*						 				*
*****************************************


//////////////////////
// 	reg vs rreg 	//	
//////////////////////

//	Non-robust-HAR-RV	//		//for periods t > 1387

reg retvar L.retvar L.rv5 L.rv22 if t < 1388
predict harrvfit if t > 1387
predict harrvse if t > 1387, stdf

//Produce 1-step forecast RMSE:
egen harrve = mean((act-harrvfit)^2)
gen harrvesq = sqrt(harrve)

label variable harrvfit "HAR-RV Forecast"

//Build 68% forecast intervals using normal rule:
gen harrvint1 = harrvfit - 1.645*harrvse
gen harrvint2 = harrvfit + 1.645*harrvse


label variable harrvint1 "90 FI (lower)"
label variable harrvint2 "90 FI (upper)"

tsline act harrvfit harrvint1 harrvint2 if t > 1387, lpattern(solid dash shortdash shortdash)
graph export fwnr_poos.tif, replace

//keep date t retdaily retvar rv5 rv22 act harrvfit 


//	Robust-HAR-RV	//			//for periods t > 1387

rreg retvar L.retvar L.rv5 L.rv22 if t < 1388
predict rharrvfit if t > 1387
predict rharrvse if t > 1387, stdp

//Produce 1-step forecast RMSE:
egen rharrve = mean((act-rharrvfit)^2)
gen rharrvesq = sqrt(rharrve)

label variable rharrvfit "HAR-RV Forecast (Robust)"

//Build 68% forecast intervals using normal rule:
gen rharrvint1 = rharrvfit - 1.645*rharrvse
gen rharrvint2 = rharrvfit + 1.645*rharrvse


label variable rharrvint1 "90 FI (lower)"
label variable rharrvint2 "90% FI (upper)"

tsline act rharrvfit rharrvint1 rharrvint2 if t > 1387, lpattern(solid dash shortdash shortdash)
graph export fwr_poos.tif, replace

//keep date t retdaily retvar rv5 rv22 act harrvfit rharrvfit


*joint comparison*

dmariano act harrvfit rharrvfit, crit(mse) kernel(bartlett) maxlag(4)  //rharvvfit is better

tsline act harrvfit rharrvfit if t > 1387, lpattern(solid dash shortdash)
graph export fw_poos.tif, replace

//keep date t retdaily retvar rv5 rv22 act


//////////////////////////
//	Expansive-window:	//			//for periods t > 1387
//////////////////////////

gen ew_yhat = .
gen ew_se = .

forvalues p = 1387/1486 {									 
	
	local ql = `p'-4
	local rl = `p'-21
	
	gen rv5e = .
	gen rv22e = .
	
	forvalues q = 1/`ql' {			//recalculate rv5 for every additional observation
		
		qui gen temp = retvar if t >= `q' & t <= `q'+5-1
		egen temp2 = mean(temp)
		qui replace rv5e = temp2 if t == `q'+5-1
		drop temp*
	}

	forvalues r = 1/`rl' {			//recalculate rv22 for every additional observation
		
		qui gen temp = retvar if t >= `r' & t <= `r'+22-1
		egen temp2 = mean(temp)
		qui replace rv22e = temp2 if t == `r'+22-1
		drop temp*
	}

	qui rreg retvar L.retvar L.rv5e L.rv22e if t <= `p'			//run HAR-RV for each additional observation

	predict yhat if t > `p'			//RV point forecast
	predict se, stdp		//RV forecast SE
	
	//egen ewharrvfite = mean((act-yhat)^2)			//gen MSE
	//gen se = sqrt(ewharrvfite)		//gen RMSE

	replace ew_yhat = yhat if t==(`p'+1)		//assigning point forecast
	replace ew_se = se if t==(`p'+1)		//assigning point RMSFE

	drop yhat se rv5e rv22e 
	//drop yhat se rv5e rv22e ewharrvfite 			//clean up for next iteration
}

//Build 68% forecast intervals using normal rule:
gen ewint1 = ew_yhat - 1.645*ew_se
gen ewint2 = ew_yhat + 1.645*ew_se

label variable ew_yhat "Expansive Window HAR-RV"
label variable ewint1 "90% FI (lower)"
label variable ewint2 "90% FI (upper)"

list t ew_yhat ewint1 ewint2 if t > 1387
tsline act ew_yhat ewint1 ewint2 if t > 1377, lpattern(solid dash shortdash shortdash)
graph export ew_poos_robust.tif, replace

//keep date t retdaily retvar rv5 rv22 act


//////////////////////////
// 	Rolling-window: 	//			//60 mth (60 * 22days) window 
//////////////////////////								

//1-step ahead

gen rw_yhat = .
gen rw_se = .
	
forvalues p = 1/100 {

	local ql = 1387 + `p' - 5
	local rl = 1387 + `p' - 22
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day 
	
	gen rv5r = .
	gen rv22r = .
	
	forvalues q = 1/`ql' {			//recalculate rv5 for every additional observation
		
		qui gen temp = retvar if t >= `q' & t <= `q'+5-1
		egen temp2 = mean(temp)
		qui replace rv5r = temp2 if t == `q'+5-1
		drop temp*
	}

	forvalues r = 1/`rl' {			//recalculate rv22 for every additional observation
		
		qui gen temp = retvar if t >= `r' & t <= `r'+22-1
		egen temp2 = mean(temp)
		qui replace rv22r = temp2 if t == `r'+22-1
		drop temp*
	}
	
	qui rreg retvar L.retvar L.rv5r L.rv22r if t >= `z' & t <= `a'

	predict yhat if t > `a'
	predict se, stdp
	
	//egen rwharrvfite = mean((act-yhat)^2)			//gen MSE
	//gen se = sqrt(rwharrvfite)		//gen RMSE

	replace rw_yhat = yhat if t == (`a'+1)
	replace rw_se = se if t == (`a'+1)

	//drop yhat se rv5r rv22r rwharrvfite 
	drop yhat se rv5r rv22r
}

//Build 68% forecast intervals using normal rule:
gen rwint1 = rw_yhat - 1.645*rw_se
gen rwint2 = rw_yhat + 1.645*rw_se

label variable rw_yhat "Rolling Window HAR-RV"
label variable rwint1 "90% FI (lower)"
label variable rwint2 "90% FI (upper)"

list t rw_yhat rwint1 rwint2 if t > 1387
tsline act rw_yhat rwint1 rwint2 if t > 1377, lpattern(solid dash shortdash shortdash) title("1 step ahead forecast")
graph export rw_poos_robust.tif, replace

//keep date t retdaily retvar rv5 rv22 act


//5-step ahead

gen rw_yhat5 = .
gen rw_se5 = .
	
forvalues p = 1/100 {

	local ql = 1387 + `p' - 5
	local rl = 1387 + `p' - 22
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day 
	
	gen rv5r = .
	gen rv22r = .
	
	forvalues q = 1/`ql' {			//recalculate rv5 for every additional observation
		
		qui gen temp = retvar if t >= `q' & t <= `q'+5-1
		egen temp2 = mean(temp)
		qui replace rv5r = temp2 if t == `q'+5-1
		drop temp*
	}

	forvalues r = 1/`rl' {			//recalculate rv22 for every additional observation
		
		qui gen temp = retvar if t >= `r' & t <= `r'+22-1
		egen temp2 = mean(temp)
		qui replace rv22r = temp2 if t == `r'+22-1
		drop temp*
	}
	
	qui rreg retvar L(5).retvar L(5).rv5r L(5).rv22r if t >= `z' & t <= `a'

	predict yhat if t > `a'
	predict se, stdp
	
	//egen rwharrvfite = mean((act-yhat)^2)			//gen MSE
	//gen se = sqrt(rwharrvfite)		//gen RMSE

	replace rw_yhat5 = yhat if t == (`a'+1)
	replace rw_se5 = se if t == (`a'+1)

	//drop yhat se rv5r rv22r rwharrvfite 
	drop yhat se rv5r rv22r
}

//Build 68% forecast intervals using normal rule:
gen rwint15 = rw_yhat5 - 1.645*rw_se5
gen rwint25 = rw_yhat5 + 1.645*rw_se5

label variable rw_yhat5 "Rolling Window HAR-RV"
label variable rwint15 "90% FI (lower)"
label variable rwint25 "90% FI (upper)"

list t rw_yhat5 rwint15 rwint25 if t > 1387
tsline act rw_yhat5 rwint15 rwint25 if t > 1377, lpattern(solid dash shortdash shortdash) title("5 step ahead forecast")
graph export rw_poos_robust5.tif, replace


//22-step ahead

gen rw_yhat22 = .
gen rw_se22 = .
	
forvalues p = 1/100 {

	local ql = 1387 + `p' - 5
	local rl = 1387 + `p' - 22
	
	local z = `p' + 67			//lower bound = 1389 - 60 * 22days - 1day
	local a = 1386 + `p'		//upper bound = 1387 - 1day 
	
	gen rv5r = .
	gen rv22r = .
	
	forvalues q = 1/`ql' {			//recalculate rv5 for every additional observation
		
		qui gen temp = retvar if t >= `q' & t <= `q'+5-1
		egen temp2 = mean(temp)
		qui replace rv5r = temp2 if t == `q'+5-1
		drop temp*
	}

	forvalues r = 1/`rl' {			//recalculate rv22 for every additional observation
		
		qui gen temp = retvar if t >= `r' & t <= `r'+22-1
		egen temp2 = mean(temp)
		qui replace rv22r = temp2 if t == `r'+22-1
		drop temp*
	}
	
	qui rreg retvar L(22).retvar L(22).rv5r L(22).rv22r if t >= `z' & t <= `a'

	predict yhat if t > `a'
	predict se, stdp
	
	//egen rwharrvfite = mean((act-yhat)^2)			//gen MSE
	//gen se = sqrt(rwharrvfite)		//gen RMSE

	replace rw_yhat22 = yhat if t == (`a'+1)
	replace rw_se22 = se if t == (`a'+1)

	//drop yhat se rv5r rv22r rwharrvfite 
	drop yhat se rv5r rv22r
}

//Build 68% forecast intervals using normal rule:
gen rwint122 = rw_yhat22 - 1.645*rw_se22
gen rwint222 = rw_yhat22 + 1.645*rw_se22

label variable rw_yhat22 "Rolling Window HAR-RV"
label variable rwint122 "90% FI (lower)"
label variable rwint222 "90% FI (upper)"

list t rw_yhat22 rwint122 rwint222 if t > 1387
tsline act rw_yhat22 rwint122 rwint222 if t > 1377, lpattern(solid dash shortdash shortdash) title("22 step ahead forecast")
graph export rw_poos_robust22.tif, replace

** Compare Mode Parsimonity using AIC

estimates stats arch10 garch11 garch12

*****************************************************************
*						 		  								*	
* 	Comparing Pseudo Forecast Performance using DM test			*
*						 										*
*****************************************************************

******** 1-step

** GARCH(1,1) vs GARCH(1,2)
dmariano retvar rollvar11_1step rollvar12_1step, crit(mse) kernel(bartlett) maxlag(4)
** chose GARCH(1,2)

** GARCH(1,1) vs HAR-RV
dmariano retvar rollvar11_1step rw_yhat, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,2) vs HAR-RV
dmariano retvar rollvar12_1step rw_yhat, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,1) vs ARCH(10)
dmariano retvar rollvar11_1step rollarvar10_1step, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,2) vs ARCH(10)
dmariano retvar rollvar12_1step rollarvar10_1step, crit(mse) kernel(bartlett) maxlag(4)



******** 5-step

replace rw_yhat = . if t < 1392 //balance obs

** GARCH(1,1) vs GARCH(1,2)
dmariano retvar rollvar11_5step rollvar12_5step, crit(mse) kernel(bartlett) maxlag(4)
** chose GARCH(1,2)

** GARCH(1,1) vs HAR-RV
dmariano retvar rollvar11_5step rw_yhat5, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,2) vs HAR-RV
dmariano retvar rollvar12_5step rw_yhat5, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,1) vs ARCH(10)
dmariano retvar rollvar11_5step rollarvar10_5step, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,2) vs ARCH(10)
dmariano retvar rollvar12_5step rollarvar10_5step, crit(mse) kernel(bartlett) maxlag(4)


******** 22-step

replace rw_yhat = . if t < 1409 //balance obs

** GARCH(1,1) vs GARCH(1,2)
dmariano retvar rollvar11_22step rollvar12_22step, crit(mse) kernel(bartlett) maxlag(4)
** chose GARCH(1,2)

** GARCH(1,1) vs HAR-RV
dmariano retvar rollvar11_22step rw_yhat22, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,2) vs HAR-RV
dmariano retvar rollvar12_22step rw_yhat22, crit(mse) kernel(bartlett) maxlag(4)
** chose GARCH(1,2)

** GARCH(1,1) vs ARCH(10)
dmariano retvar rollvar11_22step rollarvar10_22step, crit(mse) kernel(bartlett) maxlag(4)

** GARCH(1,2) vs ARCH(10)
dmariano retvar rollvar12_22step rollarvar10_22step, crit(mse) kernel(bartlett) maxlag(4)



*****************************************************************
*						 		  								*	
* 	Comparing Pseudo Forecast Performance using DM test (QLIKE)	*
*						 										*
*****************************************************************


******** 1-step

gen lrollvar11_1step = (retvar-rollvar11_1step)-log(retvar/rollvar11_1step)-1
gen lrollvar12_1step = (retvar-rollvar12_1step)-log(retvar/rollvar12_1step)-1
gen lrw_yhat = (retvar-rw_yhat)-log(retvar/rw_yhat)-1
gen lrollarvar10_1step = (retvar-rollarvar10_1step)-log(retvar/rollarvar10_1step)-1

** GARCH(1,1) vs GARCH(1,2)
sum lrollvar11_1step lrollvar12_1step
gen d12_1 = lrollvar11_1step - lrollvar12_1step
newey d12_1, lag(4)

** GARCH(1,1) vs HAR-RV 
sum lrollvar11_1step lrw_yhat
gen d1h_1 = lrollvar11_1step - lrw_yhat
newey d1h_1, lag(4)

** GARCH(1,2) vs HAR-RV 
sum lrollvar12_1step lrw_yhat
gen d2h_1 = lrollvar12_1step - lrw_yhat
newey d2h_1, lag(4)

** GARCH(1,1) vs ARCH(10) 
sum lrollvar11_1step lrollarvar10_1step
gen d110_1 = lrollvar11_1step - lrollarvar10_1step
newey d110_1, lag(4)

** GARCH(1,2) vs ARCH(10) 
sum lrollvar12_1step lrollarvar10_1step
gen d210_1 = lrollvar12_1step - lrollarvar10_1step
newey d210_1, lag(4)


******** 5-step

gen lrollvar11_5step = (retvar-rollvar11_5step)-log(retvar/rollvar11_5step)-1
gen lrollvar12_5step = (retvar-rollvar12_5step)-log(retvar/rollvar12_5step)-1
gen lrw_yhat5 = (retvar-rw_yhat5)-log(retvar/rw_yhat5)-1 if t > 1391 //for balanced obs
gen lrollarvar10_5step = (retvar-rollarvar10_5step)-log(retvar/rollarvar10_5step)-1

** GARCH(1,1) vs GARCH(1,2) 
sum lrollvar11_5step lrollvar12_5step
gen d12_5 = lrollvar11_5step - lrollvar12_5step
newey d12_5, lag(4)

** GARCH(1,1) vs HAR-RV 
sum lrollvar11_5step lrw_yhat5
gen d1h_5 = lrollvar11_5step - lrw_yhat5
newey d1h_5, lag(4)

** GARCH(1,2) vs HAR-RV 
sum lrollvar12_5step lrw_yhat5
gen d2h_5 = lrollvar12_5step - lrw_yhat5
newey d2h_5, lag(4)

** GARCH(1,1) vs ARCH(10) 
sum lrollvar11_5step lrollarvar10_5step
gen d110_5 = lrollvar11_5step - lrollarvar10_5step
newey d110_5, lag(4)

** GARCH(1,2) vs ARCH(10)  
sum lrollvar12_5step lrollarvar10_5step
gen d210_5 = lrollvar12_5step - lrollarvar10_5step
newey d210_5, lag(4)


******** 22-step

gen lrollvar11_22step = (retvar-rollvar11_22step)-log(retvar/rollvar11_22step)-1
gen lrollvar12_22step = (retvar-rollvar12_22step)-log(retvar/rollvar12_22step)-1
gen lrw_yhat22 = (retvar-rw_yhat22)-log(retvar/rw_yhat22)-1 if t > 1408 // for balanced obs
gen lrollarvar10_22step = (retvar-rollarvar10_22step)-log(retvar/rollarvar10_22step)-1

** GARCH(1,1) vs GARCH(1,2) 
sum lrollvar11_22step lrollvar12_22step
gen d12_22 = lrollvar11_22step - lrollvar12_22step
newey d12_22, lag(4)

** GARCH(1,1) vs HAR-RV 
sum lrollvar11_22step lrw_yhat22
gen d1h_22 = lrollvar11_22step - lrw_yhat22
newey d1h_22, lag(4)

** GARCH(1,2) vs HAR-RV 
sum lrollvar12_22step lrw_yhat22
gen d2h_22 = lrollvar12_22step - lrw_yhat22
newey d2h_22, lag(4)

** GARCH(1,1) vs ARCH(10) 
sum lrollvar11_22step lrollarvar10_22step
gen d110_22 = lrollvar11_22step - lrollarvar10_22step
newey d110_22, lag(4)

** GARCH(1,2) vs ARCH(10) 
sum lrollvar12_22step lrollarvar10_22step
gen d210_22 = lrollvar12_22step - lrollarvar10_22step
newey d210_22, lag(4)
