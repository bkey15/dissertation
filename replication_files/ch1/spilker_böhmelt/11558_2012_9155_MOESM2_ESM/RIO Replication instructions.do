************************************************************************************
* The impact of preferential trade agreements on governmental repression revisited *
* Address correspondence to: tobias.boehmelt@ir.gess.ethz.ch                       *
* This version: August 27, 2012                                                    *
* Note 1: Stata 12.1 has been used for analysis if not indicated otherwise         *
* Note 2: All data files must be moved into user's Stata working folder first      *
************************************************************************************

***********
* Table 1 *
***********

use "01_RIO shapefile.dta", clear

tab repression_final

***********
* Table 2 *
***********

sum repression_final hard_lag hras_lag density_lag durable_lag polity2_lag gdp_lag trade_lag_log

***********
* Table 3 *
***********

generate repression_lag2=l.repression_final
tab repression_lag2, gen(temp_cont)

ologit repression_final temp_cont1 temp_cont2 temp_cont3 temp_cont4 polity2_lag durable_lag density_lag trade_lag_log fdi_lag gdp_lag hras_lag soft_lag hard_lag if year<2003, cluster (ccode)
ologit repression_final temp_cont1 temp_cont2 temp_cont3 temp_cont4 polity2_lag durable_lag density_lag trade_lag_log fdi_lag gdp_lag hras_lag soft_lag hard_lag, cluster (ccode)
ologit repression_final polity2_lag durable_lag density_lag trade_lag_log gdp_lag hras_lag hard_lag, cluster (ccode)

*****************
* Matching in R *
*****************

library("foreign")
library("Matching")
mydata<-read.dta("01_RIO shapefile.dta")
attach(mydata)

Des<-cbind(ccode, year)

X<-cbind(trade_lag_log, polity2_lag, hras_lag)
BalanceMatrix<- cbind(trade_lag_log, gdp_lag, polity2_lag, durable_lag, density_lag, hras_lag) 

gen1<-GenMatch(Tr=hard_lag, X=X, BalanceMatrix=BalanceMatrix, pop.size=1000)
mgen1<- Match(Y=repression_final, Tr=hard_lag, X=X, Weight.matrix=gen1)

balancetest<-MatchBalance(hard_lag~trade_lag_log+gdp_lag+polity2_lag+durable_lag+density_lag+hras_lag, match.out=mgen1, nboots= 1000)
attach(mgen1)

U<-cbind(mgen1$mdata$Y,  mgen1$mdata$Tr,  mgen1$mdata$X, mgen1$index.treated, mgen1$index.control)
V<-rbind(BalanceMatrix[index.treated,],BalanceMatrix[index.control,])
X<-rbind(Des[index.treated,],Des[index.control,])
GenMatch1data<-cbind(U, V, X)
GenMatch1dataset<-data.frame( GenMatch1data)
summary(mgen1, full=TRUE)
write.dta(GenMatch1dataset, file="02_Matched data hard PTA")

***********
* Table 4 *
***********

use "02_Matched data hard PTA", clear

ologit V1 V2, cluster (ccode)
ologit V1 V2 trade_lag_log gdp_lag polity2_lag durable_lag density_lag hras_lag, cluster (ccode)
ologit V1 V2 trade_lag_log gdp_lag polity2_lag durable_lag density_lag, cluster (ccode)

generate control=V1
recode control (1=0) (2=0) (3=1) (4=1) (5=1)
btscs control year ccode, g(tortureyrs) nspline(3)

ologit V1 V2 trade_lag_log gdp_lag polity2_lag durable_lag density_lag hras_lag tortureyrs _spline1 _spline2 _spline3, cluster (ccode)

**********************************************
* Figure 1 - Based upon matching output in R *
**********************************************

use "03_Matching summary.dta", clear

twoway (scatter covariate std_mean_diff_before if law_id==2 & var7==1, mcolor(black) msymbol(square)) (scatter covariate std_mean_diff_after if law_id==2 & var7==1, mcolor(bluishgray) msymbol(square)), name(size1, replace) xline(0.25, lpattern(dash)) xline(-0.25, lpattern(dash)) scheme(s1mono)
twoway (scatter covariate t_test_before if law_id==2 & var7==1, mcolor(black) msymbol(square)) (scatter covariate t_test_after if law_id==2 & var7==1, mcolor(bluishgray) msymbol(square)), name(size2, replace) xline(0.10, lpattern(dash)) scheme(s1mono)
graph combine size1 size2, ycommon

************
* Figure 2 *
************

use "02_Matched data hard PTA", clear

estsimp ologit V1 V2, cluster (ccode)
setx V2 0
simqi, fd(prval(1)) changex(V2  0 1) level(90)
simqi, fd(prval(2)) changex(V2  0 1) level(90)
simqi, fd(prval(3)) changex(V2  0 1) level(90)
simqi, fd(prval(4)) changex(V2  0 1) level(90)
simqi, fd(prval(5)) changex(V2  0 1) level(90)
drop b1-b5

estsimp ologit V1 V2 trade_lag_log gdp_lag polity2_lag durable_lag density_lag hras_lag, cluster (ccode)
setx V2 0 trade_lag_log 4.270868  gdp_lag 7.628517 polity2_lag  4.032156 durable_lag 24.17497 density_lag 122.8226 hras_lag 1.371375 
simqi, fd(prval(1)) changex(V2  0 1) level(90)
simqi, fd(prval(2)) changex(V2  0 1) level(90)
simqi, fd(prval(3)) changex(V2  0 1) level(90)
simqi, fd(prval(4)) changex(V2  0 1) level(90)
simqi, fd(prval(5)) changex(V2  0 1) level(90)
drop b1-b11

estsimp ologit V1 V2 trade_lag_log gdp_lag polity2_lag durable_lag density_lag, cluster (ccode)
setx V2 0 trade_lag_log 4.270868  gdp_lag 7.628517 polity2_lag  4.032156 durable_lag 24.17497 density_lag 122.8226
simqi, fd(prval(1)) changex(V2  0 1) level(90)
simqi, fd(prval(2)) changex(V2  0 1) level(90)
simqi, fd(prval(3)) changex(V2  0 1) level(90)
simqi, fd(prval(4)) changex(V2  0 1) level(90)
simqi, fd(prval(5)) changex(V2  0 1) level(90)
drop b1-b10

generate control=V1
recode control (1=0) (2=0) (3=1) (4=1) (5=1)
btscs control year ccode, g(tortureyrs) nspline(3)

estsimp ologit V1 V2 trade_lag_log gdp_lag polity2_lag durable_lag density_lag hras_lag tortureyrs _spline1 _spline3, cluster (ccode)
setx V2 0 trade_lag_log 4.270868  gdp_lag 7.628517 polity2_lag  4.032156 durable_lag 24.17497 density_lag 122.8226 hras_lag 1.371375 tortureyrs 4.975309 _spline1 -1727.744 _spline3 -832.9216 
simqi, fd(prval(1)) changex(V2  0 1) level(90)
simqi, fd(prval(2)) changex(V2  0 1) level(90)
simqi, fd(prval(3)) changex(V2  0 1) level(90)
simqi, fd(prval(4)) changex(V2  0 1) level(90)
simqi, fd(prval(5)) changex(V2  0 1) level(90)
drop b1-b14

use "04_First differences.dta", clear

twoway (rcapsym low_bound upp_bound repression_value if id_law==1, lcolor(black) lpattern(dash) horizontal mcolor(black) msize(vsmall) msymbol(diamond_hollow)) (scatter repression_value first_diff if id_law==1, mcolor(black) msize(medlarge) msymbol(x)), ytitle(Political Repression Worldwide) xtitle(First Difference) xline(0, lpattern(solid) lcolor(gs9)) legend(off) scheme(s1mono) name(size1, replace)
twoway (rcapsym low_bound upp_bound repression_value if id_law==2, lcolor(black) lpattern(dash) horizontal mcolor(black) msize(vsmall) msymbol(diamond_hollow)) (scatter repression_value first_diff if id_law==2, mcolor(black) msize(medlarge) msymbol(x)), ytitle(Political Repression Worldwide) xtitle(First Difference) xline(0, lpattern(solid) lcolor(gs9)) legend(off) scheme(s1mono) name(size2, replace)
twoway (rcapsym low_bound upp_bound repression_value if id_law==3, lcolor(black) lpattern(dash) horizontal mcolor(black) msize(vsmall) msymbol(diamond_hollow)) (scatter repression_value first_diff if id_law==3, mcolor(black) msize(medlarge) msymbol(x)), ytitle(Political Repression Worldwide) xtitle(First Difference) xline(0, lpattern(solid) lcolor(gs9)) legend(off) scheme(s1mono) name(size3, replace)
twoway (rcapsym low_bound upp_bound repression_value if id_law==4, lcolor(black) lpattern(dash) horizontal mcolor(black) msize(vsmall) msymbol(diamond_hollow)) (scatter repression_value first_diff if id_law==4, mcolor(black) msize(medlarge) msymbol(x)), ytitle(Political Repression Worldwide) xtitle(First Difference) xline(0, lpattern(solid) lcolor(gs9)) legend(off) scheme(s1mono) name(size4, replace)
graph combine size1 size2 size3 size4, xcommon scheme(s1mono)
