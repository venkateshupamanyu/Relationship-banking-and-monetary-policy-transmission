cd "...\Data"


insheet using policyrates_alternate2.csv,c names clear
rename yearend year_n
save policyrates, replace
use policyrates,clear
sort year_n
tsset year_n, yearly
gen dcrr = crr - l.crr
gen davgrepo = avgrepo - l.avgrepo
gen avgrepoup = davgrepo > 0
gen crrup = dcrr > 0
gen davgcrr = avgcrr - l.avgcrr
gen avgcrrup = davgcrr > 0
gen crrdown = dcrr < 0f
gen crrch = crrup == 1
replace crrch = -1 if crrdown == 1
replace excrate = 1/excrate
gen ln_exc = log(excrate)
gen avgbaserate = avgrepo if year_n == 2001
replace avgbaserate = avgbaserate[_n-1] if avgbaserate >=.
/*filling up base rate to 2001 value*/
replace avgbaserate = 8.52  if avgbaserate >=.
gen basexrate = ln_exc if year_n == 2001
replace basexrate = basexrate[_n-1] if basexrate >=.
/*filling up base xchange rate to 2001 value*/
replace basexrate = .-3.8537581 if basexrate >=.
gen mci = (0.75)*(avgrepo - avgbaserate) + (0.25)*(ln_exc - basexrate)
egen tight = xtile(mp), n(2)
replace tight = tight - 1
gen tight2 = mci - mci[_n-1] > 0
gen lag_crr = crr[_n-1]
gen lag_repo = repo[_n-1]
gen lag_tight2 = tight2[_n-1]

*** venky lag_avgrepo insertion ***
sort companyname year_n
by companyname year_n: gen l1_avgrepo = l1.avgrepo
tsset year_n
gen l1_avgrepo = l1.avgrepo


gen l1_avgcrr = l1.avgcrr
gen l2_avgcrr = l2.avgcrr
gen l3_avgcrr = l3.avgcrr
gen f1_avgcrr = f1.avgcrr
gen f2_avgcrr = f2.avgcrr
gen f3_avgcrr = f3.avgcrr

label variable crr "CRR"
label variable repo "Repo Rate"
label variable revrepo "Rev Repo Rate"
label variable bankrate "Bank Rate"
label variable slr "SLR"

keep if year>=1998
keep if year<=2014

save "...\dta files\policyrates.dta", replace


cd "...\Data"

insheet using borrowings.csv,c names clear
gen year_n = int(slotdate/10000)
duplicates drop
foreach x of varlist borrowingfrombanks - sundrycreditors {
replace `x' = 0 if `x' ==.
}
save "...\dta files\borrowings.dta",replace

*IMORTING CLASSIFICATIONS
cd "D:\Abhishek\Creditor_rels"
import excel using vigclassification2, firstrow clear
keep companyname ind1 indcode
save "...\dta files\ind_class.dta", replace

*MERGING FIRM NAMES WITH BORROWINGS
use intermediate,clear
keep companyname slotdate borrowings
duplicates drop
merge m:1 companyname slotdate using "...\dta files\borrowings.dta"
keep if _m == 3
drop _m

replace borrowings = 0 if borrowings == .
gen borrowings2 = borrowingfrombanks+ borrowingfromfinancialinstitutio+ borrowingsfromcentralstategovt+ borrowingssyndicatedacrossbanksi+ debenturesandbonds+ foreigncurrencyborrowings+ loansfrompromotersdirectorsandsh+ intercorporateloans+deferredcredit+ interestaccruedanddue+ hirepurchaseloans+ fixeddeposits+ commercialpapers+ otherborrowings

save "...\dta files\borrowings.dta",replace
use "...\dta files\borrowings.dta", clear
gen bankborrow = borrowingfrombanks/ borrowings
gen bankborrow2 = borrowingfrombanks/ borrowings2
gen finborrow = borrowingfromfinancialinstitutio / borrowings2
gen govborrow = borrowingsfromcentralstategovt / borrowings2
gen cmborrow = debenturesandbonds / borrowings2
gen icborrow = intercorporateloans / borrowings2
gen forborrow = foreigncurrencyborrowings/borrowings2
gen prborrow = loansfrompromotersdirectorsandsh /borrowings2
gen othborrow = otherborrowings /borrowings2
gen cpborrow = commercialpapers / borrowings2
egen companyid = group(companyname)
xtset companyid year_n
sort companyid year_n
bysort companyid: gen tradecredit = (sundrycreditors - sundrycredit[_n-1])/borrowings
drop companyid
save "...\dta files\borrowings.dta",replace




cd "D:\Abhishek\Creditor_rels"
use intermediate,clear
gen scb = .
replace scb = 1 if bankername == "state bank of bikaner & jaipur"
replace scb = 1 if bankername == "state bank of hyderabad"
replace scb = 1 if bankername == "state bank of india"
replace scb = 1 if bankername == "state bank of mysore"
replace scb = 1 if bankername == "state bank of patiala"
replace scb = 1 if bankername == "state bank of travancore"
replace scb = 1 if bankername == "allahabad bank"
replace scb = 1 if bankername == "andhra bank"
replace scb = 1 if bankername == "bank of baroda"
replace scb = 1 if bankername == "bank of india"
replace scb = 1 if bankername == "bank of maharashtra"
replace scb = 1 if bankername == "canara bank"
replace scb = 1 if bankername == "central bank of india"
replace scb = 1 if bankername == "corporation bank"
replace scb = 1 if bankername == "dena bank"
replace scb = 1 if bankername == "indian bank"
replace scb = 1 if bankername == "indian overseas bank"
replace scb = 1 if bankername == "oriental bank of commerce"
replace scb = 1 if bankername == "punjab national bank"
replace scb = 1 if bankername == "syndicate bank"
replace scb = 1 if bankername == "uco bank"
replace scb = 1 if bankername == "union bank of india"
replace scb = 1 if bankername == "united bank of india"
replace scb = 1 if bankername == "vijaya bank"
replace scb = 1 if bankername == "axis bank ltd."
replace scb = 1 if bankername == "catholic syrian bank ltd."
replace scb = 1 if bankername == "city union bank ltd."
replace scb = 1 if bankername == "d c b bank ltd."
replace scb = 1 if bankername == "dhanlaxmi bank ltd."
replace scb = 1 if bankername == "federal bank ltd."
replace scb = 1 if bankername == "hdfc bank"
replace scb = 1 if bankername == "icici bank"
replace scb = 1 if bankername == "indusind bank ltd."
replace scb = 1 if bankername == "i n g vysya bank ltd."
replace scb = 1 if bankername == "jammu & kashmir bank ltd."
replace scb = 1 if bankername == "karnataka bank ltd."
replace scb = 1 if bankername == "karur vysya bank ltd."
replace scb = 1 if bankername == "kotak mahindra bank ltd."
replace scb = 1 if bankername == "lakshmi vilas bank ltd."
replace scb = 1 if bankername == "nainital bank ltd."
replace scb = 1 if bankername == "south indian bank ltd."
replace scb = 1 if bankername == "tamilnad mercantile bank ltd."
replace scb = 1 if bankername == "ratnakar bank ltd."
replace scb = 1 if bankername == "yes bank ltd."
*keep if foreign == 1
keep if scb == 1


replace cashandbankbalance = 0 if cashandbankbalance ==.
gen cashratio = cashandbankbalance/currentassets
gen tangibility = grossfixedassets / totalassets
foreach x of varlist shorttermbankborrowings longtermbankborrowings securedbankborrowings unsecuredbankborrowings {
replace `x' = 0 if `x' ==.
}
gen leverage = debt/totalassets 
replace totalinterestexpenses = interestpaid if totalinterestexpenses ==.
gen coverage = log(1+(pbdita /totalinterestexpenses))
gen currentratio = currentassets/ currentliabilities


keep companycode companyname bankername year_n is_bank public private foreign coop pfi is_rel totalassets profitaftertax ownershipgroup industrygroup shorttermbankborrowings longtermbankborrowings securedbankborrowings unsecuredbankborrowings ln_assets ln_sales ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio cashratio is_group is_pub scb
save "...\dta files\intermediate.dta",replace

cd "...\dta files"
use intermediate,clear
sort companyname year_n
merge m:1 companyname year_n using borrowings
drop if _m==2
drop _m

gen shbankborrow = shorttermbankborrowings / (shorttermbankborrowings + longtermbankborrowings)
gen lobankborrow = longtermbankborrowings / (shorttermbankborrowings + longtermbankborrowings)
gen secbankborrow = securedbankborrowings / (securedbankborrowings + unsecuredbankborrowings)
gen unsecbankborrow = unsecuredbankborrowings / (securedbankborrowings + unsecuredbankborrowings)

merge m:1 companyname using ind_class
drop if _m==2
drop _m

save "...\dta files\relationtemp.dta",replace


cd "...\dta files"
use relationtemp,clear
merge m:1 year_n using policyrates
drop _m

sort companyname year bankername
egen pair = group(companyname bankername)
bysort companyname year: gen nbank = _N
gen rel_1 = nbank == 1
gen rel_2 = (nbank==1 & pair==pair[_n-1])
gen rel_3 = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2])
gen rel_4 = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2] & pair==pair[_n-3])
gen rel_5 = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2] & pair==pair[_n-3] & pair==pair[_n-4])
gen rel_max = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2] & pair==pair[_n-3] & pair==pair[_n-4] & pair==pair[_n-5])

collapse (sum) is_bank public rel_1 rel_2 rel_3 rel_4 rel_5 rel_max, by (companyname industrygroup indcode year_n tight tight2 lag_tight2 is_group is_pub ln_assets ln_sales ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio mci bankrate repo repolag revrepolag msf slr avgbankrate avgrepo avgrepoup avgrevrepo avgcrr avgmsf avgslr mp crr crrch crrup avgcrrup crrdown lag_crr revrepo lag_repo l1_avgcrr l2_avgcrr l3_avgcrr f1_avgcrr f2_avgcrr f3_avgcrr bankborrow bankborrow2 shbankborrow lobankborrow secbankborrow unsecbankborrow finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow tradecredit borrowingfrombanks borrowings borrowings2)
sort companyname year_n

save relation,replace

use relation,clear

gen log_b = log(1+borrowings2)
label variable log_b "Log_Borrowings"
gen log_bb = log(1+borrowingfrombanks)
label variable log_bb "Log_BankBorrowings"
gen log_nbb = log(1+(borrowings2-borrowingfrombanks))
label variable log_nbb "Log_Non-BankBorrowings"
gen nonbankborrow2 = 1 - bankborrow2
label variable nonbankborrow2 "Non-bank borrowing as fraction of borrowing"

egen companycode = group(companyname)
tsset companycode year, yearly
sort companyname year_n
bysort companyname: gen lag_rel_1 = rel_1[_n-1]
bysort companyname: gen lag_ln_assets = ln_assets[_n-1]
bysort companyname: gen lag_ebit_assets = ebit_assets[_n-1]
bysort companyname: gen lag_tobin = tobin[_n-1]
bysort companyname: gen lag_leverage = leverage[_n-1]

/*CLEANING DATA*/
winsor2 bankborrow2 nonbankborrow2 log_b log_bb log_nbb ln_assets ebit_assets tangibility leverage currentratio tobin, replace cuts(1 99)

/*FILTER DATA FOR DATES*/
drop if year_n < 1997

/*LABELLING THE DATA*/
label variable crr "CRR"
label variable repo "Repo Rate"
label variable rel_1 "Exclusive Bank (1 yr)"
label variable rel_2 "Exclusive Bank (2 yr)"
label variable rel_3 "Exclusive Bank (3 yr)"
label variable rel_4 "Exclusive Bank (4 yr)"
label variable rel_5 "Exclusive Bank (5 yr)"
label variable rel_max "Exclusive Bank (>=6 yr)"
label variable bankborrow2 "Bank borr. ratio"
label variable nonbankborrow2 "Non-bank borr. ratio"
label variable finborrow "Financial institutions borr. ratio"
label variable govborrow "Government borr. ratio"
label variable cmborrow "Capital market borr. ratio"
label variable icborrow "Inter-corporate borr. ratio"
label variable forborrow "Foreign borr. ratio"
label variable prborrow "Promoter borr. ratio"
label variable othborrow "Other borr. ratio"
label variable cpborrow "CP borr. ratio"
label variable shbankborrow "Short-term bank borr. ratio"
label variable lobankborrow "Long-term bank borr. ratio"
label variable secbankborrow "Secured bank borr. ratio"
label variable unsecbankborrow "Unsecured bank borr. ratio"
label variable log_b "Log(Total borrowing amount)"
label variable log_bb "Log(Bank borrowing amount)"
label variable log_nbb "Log(Non-bank borrowing amount)"
label variable ln_assets "Size"
label variable ln_sales "Sales"
label variable ebit_assets "Profitability"
label variable leverage "Leverage"
label variable currentratio "Current Ratio"
label variable tobin "Tobin's Q"

save relation,replace

/*SUMMARY STATS AND UNIVARIATE*/
use relation, clear
estpost tabstat  rel_1 rel_2 rel_3 rel_4 rel_5 rel_max bankborrow2 nonbankborrow2 finborrow govborrow cmborrow icborrow forborrow prborrow cpborrow othborrow log_b log_bb log_nbb shbankborrow lobankborrow secbankborrow unsecbankborrow ln_assets ebit_assets leverage currentratio tobin, col(stat) statistics(count mean sd min p25 med p75 max) 
est store stat1
estout stat1 using "SummaryStat.csv", replace c("count(fmt(0)) mean(fmt(3)) p50(fmt(3)) p25(fmt(3)) p75(fmt(3)) sd (fmt(3)) min (fmt(3)) max (fmt(3))") prehead("Summary Statistics" "") delimiter(",")  postfoot ("") label

estpost tabstat bankborrow2 nonbankborrow2 finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow shbankborrow lobankborrow secbankborrow unsecbankborrow log_b log_bb log_nbb ln_assets ebit_assets leverage currentratio tobin if rel_1 == 1,  col(stat) statistics(count mean med) 
est store stat1
estout stat1 using "Summary Stat (Single).csv", replace c("count(fmt(0)) mean(fmt(3)) p50(fmt(3)) p25(fmt(3)) p75(fmt(3)) sd (fmt(3)) min (fmt(3)) max (fmt(3))") prehead("Summary Statistics" "") delimiter(",")  postfoot ("") label
estpost tabstat bankborrow2 nonbankborrow2 finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow shbankborrow lobankborrow secbankborrow unsecbankborrow log_b log_bb log_nbb ln_assets ebit_assets leverage currentratio tobin if rel_1 == 0,  col(stat) statistics(count mean med) 
est store stat2
estout stat2 using "Summary Stat (Multiple).csv", replace c("count(fmt(0)) mean(fmt(3)) p50(fmt(3)) p25(fmt(3)) p75(fmt(3)) sd (fmt(3)) min (fmt(3)) max (fmt(3))") prehead("Summary Statistics" "") delimiter(",")  postfoot ("") label
gen inv_rel_1 = rel_1 == 0
estpost ttest bankborrow2 nonbankborrow2 finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow shbankborrow lobankborrow secbankborrow unsecbankborrow log_b log_bb log_nbb ln_assets ebit_assets leverage currentratio tobin, by(inv_rel_1)
est store univ
estout univ using "Univariate.csv", replace c("mu_1(fmt(3)) mu_2(fmt(3)) b(star fmt(3)) t (fmt(2))") prehead("Univariate Results for single vs. multiple banking relationships" "") delimiter(",")  postfoot ("") label


/*GRAPH FOR RATES*/
twoway (line avgcrr year, sort lwidth(medthin)) (line avgrepo year, sort lwidth(medthin) lpattern(dash)) (line avgrevrepo year, sort lwidth(medthin) lpattern(dash_dot)) (line avgbankrate year, sort lwidth(medthin) lpattern(shortdash_dot_dot))
/*GRAPH FOR BORROWING RATIOS*/

u relation, clear
save temp1, replace
collapse bankborrow2 nonbankborrow2 finborrow govborrow cmborrow icborrow forborrow prborrow cpborrow othborrow, by(year_n)
twoway (line bankborrow2 year_n, sort lwidth(medthin)) (line finborrow year_n, sort lwidth(medthin) lpattern(dash)) (line govborrow year_n, sort lwidth(medthin) lpattern(dash_dot)) (line cmborrow year_n, sort lwidth(medthin) lpattern(shortdash_dot_dot)) (line icborrow year_n, sort lwidth(medthin)) (line forborrow year_n, sort lwidth(medthin) lpattern(dash)) (line prborrow year_n, sort lwidth(medthin) lpattern(dash_dot)) (line cpborrow year_n, sort lwidth(medthin) lpattern(shortdash_dot_dot)) (line othborrow year_n, sort lwidth(medthin) lpattern(shortdash_dot_dot))


/**** REGRESSIONS ****/

*USING THREE RATES SEPARATELY
use relation,clear
xi i.year_n
est clear
gen int1 = rel_1*avgcrr
gen int2 = rel_1*avgrepo
gen int3 = rel_1*avgslr
rename int1 intavgvrr

gen int1 = rel_1*avgcrr
gen int2 = ln_assets*avgcrr
gen int3 = ebit_assets*avgcrr
gen int4 = tobin*avgcrr

gen int5 = rel_1*crr
gen int6 = ln_assets*crr
gen int7 = ebit_assets*crr
gen int8 = tobin*crr
gen int9=rel_1*lead_crr

sort companyname year
bysort companyname: gen lead_crr = crr[_n+1]
bysort companyname: gen lead_avgcrr = avgcrr[_n+1]
bysort companyname: gen lead_repo = repo[_n+1]
bysort companyname: gen lead_avgrepo = avgrepo[_n+1]

label variable int1 "Relationship Bank * CRR"
label variable int2 "Size * CRR"
label variable int3 "Profitability * CRR"
label variable int4 "Tobin's Q * CRR"
label variable avgcrr "CRR"
label variable rel_1 "Relationship Bank"
label variable avgrepo "Repo Rate"
label variable ln_assets "Size"
label variable ebit_assets "Profitability"
label variable tobin "Tobin's Q"

reg bankborrow2 int1 avgcrr rel_1 avgrepo
est store c1
reg bankborrow2 int1 avgcrr rel_1 avgrepo ln_assets ebit_assets tobin
est store c2
reg bankborrow2 int1 int2 int3 int4 avgcrr rel_1 avgrepo ln_assets ebit_assets tobin 
est store c3
outreg2 [c1 c2 c3] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table - without FE.xls", label drop(_I* ) sortvar(int1 avgcrr int2 avgrepo int3 avgslr)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, No, Firm FE, No) 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table - without FE.txt"

************************************************************************************************
*** Table 4 ***
areg bankborrow2 int5 crr rel_1 repo,absorb(companyname)
est store a1
areg bankborrow2 int5 crr rel_1 repo ln_assets ebit_assets tobin,absorb(companyname)
est store a2
areg bankborrow2 int5 int6 int7 int8 crr rel_1 repo ln_assets ebit_assets tobin,absorb(companyname)
est store a3
reg bankborrow2 int5 crr rel_1 repo
est store a4
reg bankborrow2 int5 crr rel_1 repo ln_assets ebit_assets tobin
est store a5
reg bankborrow2 int5 int6 int7 int8 crr rel_1 repo ln_assets ebit_assets tobin
est store a6
outreg2 [a1 a2 a3 a4 a5 a6] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 v1.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(4) excel replace tstat
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 v1.txt"

*** Table 5 ***
areg bankborrow2 int9 lead_repo rel_1,absorb(companyname)
est store b1
areg bankborrow2 int9 lead_repo rel_1 ln_assets ebit_assets leverage currentratio tobin,absorb(companyname)
est store b2
areg bankborrow2 int9 int5 repo lead_repo rel_1 crr,absorb(companyname)
est store b3
areg bankborrow2 int9 int5 repo lead_repo rel_1 ln_assets ebit_assets leverage currentratio tobin crr,absorb(companyname)
est store b4
reg bankborrow2 int9 lead_repo rel_1
est store b5
reg bankborrow2 int9 lead_repo rel_1 ln_assets ebit_assets leverage currentratio tobin 
est store b6
reg bankborrow2 int9 int5 repo lead_repo rel_1 crr
est store b7
reg bankborrow2 int9 int5 repo lead_repo rel_1 ln_assets ebit_assets leverage currentratio tobin crr
est store b8
outreg2 [b1 b2 b3 b4 b5 b6 b7 b8] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 v1.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(4) excel replace tstat
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 v1.txt"

*** Prasanna details ***
gen  anti_ln_assets=exp(ln_assets)

*** Tables 4,5,6 as in Sept 27 draft version ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relation - Venky.dta"
br
*** Table 4 ***
gen int10=rel_1*avgrepo

*** without year FE ***
areg bankborrow2 int1 avgcrr rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 yes Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int10 avgrepo rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 yes Firm FE no Year FE.txt"
 
*** without firm and year FE ***
reg bankborrow2 int1 avgcrr rel_1,cluster(companycode)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin,cluster(companycode)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10 avgrepo rel_1,cluster(companycode)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin,cluster(companycode)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 2 no Firm FE no Year FE.txt"

areg bankborrow2 int1 avgcrr rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int10 avgrepo rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)

*** frozen ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky"
reg bankborrow2 int1 avgcrr rel_1 
est store a1
reg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin
est store a2
reg bankborrow2 int10 avgrepo rel_1
est store a3
reg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin
est store a4
outreg2 [a1 a2 a3 a4] using "Table 4 without FE.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Table 4 without FE.txt"
*** frozen ***

*** Table 5 ***
gen int11=rel_2*avgcrr
gen int12=rel_3*avgcrr
gen int13=rel_4*avgcrr
gen int14=rel_5*avgcrr

*** withour Year FE ***
areg bankborrow2 avgcrr avgrepo int1 rel_1 ln_assets ebit_assets tobin,absorb(companycode) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 yes Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 avgcrr avgrepo int11 rel_2 ln_assets ebit_assets tobin,absorb(companycode) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 avgcrr avgrepo int12 rel_3 ln_assets ebit_assets tobin,absorb(companycode) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 avgcrr avgrepo int13 rel_4 ln_assets ebit_assets tobin,absorb(companycode) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 avgcrr avgrepo int14 rel_5 ln_assets ebit_assets tobin,absorb(companycode) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 yes Firm FE no Year FE.txt"

*** without firm and year FE ***
reg bankborrow2 avgcrr avgrepo int1 rel_1 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int11 rel_2 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int12 rel_3 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int13 rel_4 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int14 rel_5 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 3 no Firm FE no Year FE.txt"


areg bankborrow2 avgcrr avgrepo int1 rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companycode) cluster(companyname)
areg bankborrow2 avgcrr avgrepo int11 rel_2 ln_assets ebit_assets tobin i.year_n,absorb(companycode) cluster(companyname)
areg bankborrow2 avgcrr avgrepo int12 rel_3 ln_assets ebit_assets tobin i.year_n,absorb(companycode) cluster(companyname)
areg bankborrow2 avgcrr avgrepo int13 rel_4 ln_assets ebit_assets tobin i.year_n,absorb(companycode) cluster(companyname)
areg bankborrow2 avgcrr avgrepo int14 rel_5 ln_assets ebit_assets tobin i.year_n,absorb(companycode) cluster(companyname)

*** frozen ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky"
reg bankborrow2 avgcrr avgrepo int1 rel_1 ln_assets ebit_assets tobin
est store b1
reg bankborrow2 avgcrr avgrepo int11 rel_2 ln_assets ebit_assets tobin
est store b2
reg bankborrow2 avgcrr avgrepo int12 rel_3 ln_assets ebit_assets tobin
est store b3
reg bankborrow2 avgcrr avgrepo int13 rel_4 ln_assets ebit_assets tobin
est store b4
reg bankborrow2 avgcrr avgrepo int14 rel_5 ln_assets ebit_assets tobin
est store b5
outreg2 [b1 b2 b3 b4 b5] using "Table 5 without FE.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Table 5 without FE.txt"
*** frozen ***

*** Table 6 ***
*** without year FE ***
areg log_b int1 avgcrr rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 yes Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg log_bb int1 avgcrr rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg log_nbb int1 avgcrr rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg log_b int10 avgrepo rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg log_bb int10 avgrepo rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg log_nbb int10 avgrepo rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 yes Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 yes Firm FE no Year FE.txt"
*** without firm and year FE ***

reg log_b int1 avgcrr rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg log_bb int1 avgcrr rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg log_nbb int1 avgcrr rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg log_b int10 avgrepo rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg log_bb int10 avgrepo rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg log_nbb int10 avgrepo rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 4 no Firm FE no Year FE.txt"

areg log_b int1 avgcrr rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg log_bb int1 avgcrr rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg log_nbb int1 avgcrr rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg log_b int10 avgrepo rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg log_bb int10 avgrepo rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg log_nbb int10 avgrepo rel_1 i.year_n,absorb(companyname) cluster(companyname)

*** frozen ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky"
reg log_b int1 avgcrr rel_1
est store c1
reg log_bb int1 avgcrr rel_1
est store c2
reg log_nbb int1 avgcrr rel_1
est store c3
reg log_b int10 avgrepo rel_1
est store c4
reg log_bb int10 avgrepo rel_1
est store c5
reg log_nbb int10 avgrepo rel_1
est store c6
outreg2 [c1 c2 c3 c4 c5 c6] using "Table 6 without FE.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Table 6 without FE.txt"
*** frozen ***

*** Table 7 ***
cd "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relationtemp - public banks.dta"
merge m:1 year_n using policyrates
drop _m

bysort companyname year_n: gen totalbanks=_N
bysort companyname year_n: egen totalpublic=sum(public)
keep if totalpublic==totalbanks

sort companyname year bankername
egen pair = group(companyname bankername)
bysort companyname year: gen nbank = _N
gen rel_1 = nbank == 1

collapse (sum) is_bank public rel_1, by (companyname industrygroup indcode year_n tight tight2 lag_tight2 is_group is_pub ln_assets ln_sales ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio mci bankrate repo repolag revrepolag msf slr avgbankrate avgrepo avgrepoup avgrevrepo avgcrr avgmsf avgslr mp crr crrch crrup avgcrrup crrdown lag_crr revrepo lag_repo bankborrow bankborrow2 shbankborrow lobankborrow secbankborrow unsecbankborrow finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow tradecredit borrowingfrombanks borrowings borrowings2)
sort companyname year_n

gen int1=rel_1*avgcrr
gen int10=rel_1*avgrepo

*** with firm FE and no Year FE ***
areg bankborrow2 int1 avgcrr rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 Yes Firm FE no Year FE - Public banks.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 Yes Firm FE no Year FE - Public banks.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int10 avgrepo rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 Yes Firm FE no Year FE - Public banks.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 Yes Firm FE no Year FE - Public banks.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 Yes Firm FE no Year FE - Public banks.txt"

*** with no firm and Year FE ***
reg bankborrow2 int1 avgcrr rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 no Firm FE no Year FE - Public banks.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 no Firm FE no Year FE - Public banks.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10 avgrepo rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 no Firm FE no Year FE - Public banks.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 no Firm FE no Year FE - Public banks.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 5 no Firm FE no Year FE - Public banks.txt"

areg bankborrow2 int1 avgcrr rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int10 avgrepo rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)

*** frozen ***
reg bankborrow2 int1 avgcrr rel_1
est store d1
reg bankborrow2 int1 avgcrr rel_1 ln_assets ebit_assets tobin
est store d2
reg bankborrow2 int10 avgrepo rel_1
est store d3
reg bankborrow2 int10 avgrepo rel_1 ln_assets ebit_assets tobin
est store d4
outreg2 [d1 d2 d3 d4] using "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 7 without FE.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Table 7 without FE.txt"
*** frozen ***

*** Table 8 ***
cd "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relation - Venky.dta"
br

xi i.year_n
est clear
gen int1 = rel_1*avgcrr
gen int10 = rel_1*avgrepo
gen int15 = avgcrr*tight2
gen int16 = rel_1*tight2
gen int17 = rel_1*avgcrr*tight2
gen int18 = avgrepo*tight2
gen int19 = rel_1*avgrepo*tight2

*** with firm FE and no Year FE ***
areg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 yes Firm FE no Year FE - tighten.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1 ln_assets ebit_assets tobin,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 yes Firm FE no Year FE - tighten.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 yes Firm FE no Year FE - tighten.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
areg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1 ln_assets ebit_assets tobin,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 yes Firm FE no Year FE - tighten.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 yes Firm FE no Year FE - tighten.txt"

*** with no firm and year FE ***
reg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 no Firm FE no Year FE - tighten.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 no Firm FE no Year FE - tighten.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 no Firm FE no Year FE - tighten.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1 ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 no Firm FE no Year FE - tighten.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 6 no Firm FE no Year FE - tighten.txt"


areg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1 i.year_n,absorb(companyname) cluster(companyname)
areg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)

*** frozen ***
reg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1
est store e1
reg bankborrow2 int17 int1 int15 avgcrr int16 tight2 rel_1 ln_assets ebit_assets tobin
est store e2
reg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1
est store e3
reg bankborrow2 int19 int10 int18 avgrepo int16 tight2 rel_1 ln_assets ebit_assets tobin
est store e4
outreg2 [e1 e2 e3 e4] using "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 8 without FE.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
*** frozen***

*** Table 7 - Industry X Year trends ***
xi i.year_n*i.indcode

reghdfe bankborrow2 int1 rel_1 avgcrr,absorb(indcode#year_n) vce(cluster companycode)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 7 Industry X Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Industry * Year FE,Yes) excel 
reghdfe bankborrow2 int1 rel_1 avgcrr lag_ln_assets lag_ebit_assets lag_tobin,absorb(indcode#year_n) vce(cluster companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 7 Industry X Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Industry * Year FE,Yes) excel 
reghdfe bankborrow2 int2 rel_1 avgrepo,absorb(indcode#year_n) vce(cluster companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 7 Industry X Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Industry * Year FE,Yes) excel 
reghdfe bankborrow2 int2 rel_1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin,absorb(indcode#year_n) vce(cluster companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 7 Industry X Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Industry * Year FE,Yes) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 7 Industry X Year FE.txt"

areg bankborrow2 int1 rel_1 avgcrr  _I*, absorb(companyname) cluster(companyname)
est store abb_crr1
areg bankborrow2 int1 rel_1 avgcrr lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crr2
areg bankborrow2 int2 rel_1 avgrepo  _I*, absorb(companyname) cluster(companyname)
est store abb_repo1
areg bankborrow2 int2 rel_1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)


*** Only small firms case ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relation - Venky.dta"
br

sort companyname year_n
bysort companyname: egen firm_asset_avg=mean(anti_ln_assets)
egen firm_tercile = xtile(firm_asset_avg),n(3)

gen small_firm_dum=0
replace small_firm_dum=1 if firm_tercile==1
gen int_smallfirm_crr=avgcrr*small_firm_dum
gen int_smallfirm_repo=avgrepo*small_firm_dum

*** wih firm and year FE ***
areg bankborrow2 int_smallfirm_crr i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 yes Firm FE yes Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int_smallfirm_crr ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int_smallfirm_repo i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int_smallfirm_repo ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 yes Firm FE yes Year FE.txt"

*** with no firm and year FE ***
reg bankborrow2 int_smallfirm_crr,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int_smallfirm_crr ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int_smallfirm_repo,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int_smallfirm_repo ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 11 no Firm FE no Year FE.txt"

*** Only small banks case ***
cd "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relationtemp - number of bankers.dta"
br
*** use the above data and copy-paste the bank name,bank assets,crr & repo data and put in a separate data ***
*** name the data as "Banks interaction case.dta" ***
*** finding small banks ***
sort sa_company_name year_n
bysort bankername: egen bank_asset_avg=mean(sa_total_assets)
egen bank_tercile = xtile(bank_asset_avg),n(3)

gen lnbankasset=ln(sa_total_assets)

gen small_bank_dum=0
replace small_bank_dum=1 if bank_tercile==1

gen int_smallbank_crr=avgcrr*small_bank_dum
gen int_smallbank_repo=avgrepo*small_bank_dum

*** with firm and year FE ***
areg lnbankasset int_smallbank_crr i.year_n,absorb(sa_company_name) cluster(sa_company_name)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 12 yes Bank FE yes Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Bank FE,Yes,Year FE,Yes) excel 
areg lnbankasset int_smallbank_repo i.year_n,absorb(sa_company_name) cluster(sa_company_name)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 12 yes Bank FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Bank FE,Yes,Year FE,Yes) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 12 yes Bank FE yes Year FE.txt"

reg lnbankasset int_smallbank_crr,cluster(sa_company_name)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 12 no Bank FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Bank FE,No,Year FE,No) excel 
reg lnbankasset int_smallbank_repo,cluster(sa_company_name)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 12 no Bank FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Bank FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 12 no Bank FE no Year FE.txt"

*** Only small firm & small bank case ***
cd "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relationtemp - number of bankers.dta"
br

*** this data has bank assets and policyrates ***
*** finding small firms ***
sort companyname year_n
bysort companyname: egen firm_asset_avg=mean(totalassets)
egen firm_tercile = xtile(firm_asset_avg),n(3)

*** finding small banks ***
sort bankername year_n
bysort bankername: egen bank_asset_avg=mean(sa_total_assets)
egen bank_tercile = xtile(bank_asset_avg),n(3)

*** taking only small firms & small banks for relationship dummy ***
sort companyname year_n bankername
egen pair1 = group(companyname bankername) if firm_tercile==1 & bank_tercile==1
gen rel_1_small = firm_tercile==1 & bank_tercile==1
gen rel_2_small = pair1==pair1[_n-1] & firm_tercile==1 & bank_tercile==1
gen rel_3_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & firm_tercile==1 & bank_tercile==1
gen rel_4_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & pair1==pair1[_n-3] & firm_tercile==1 & bank_tercile==1
gen rel_5_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & pair1==pair1[_n-3] & pair1==pair1[_n-4] & firm_tercile==1 & bank_tercile==1
gen rel_max_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & pair1==pair1[_n-3] & pair1==pair1[_n-4] & pair1==pair1[_n-5] & firm_tercile==1 & bank_tercile==1

*** taking only small banks for relationship banking ***
sort companyname year_n bankername
egen pair1 = group(companyname bankername) if bank_tercile==3
gen rel_1_small = bank_tercile==3
gen rel_2_small = pair1==pair1[_n-1] & bank_tercile==3
gen rel_3_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & bank_tercile==3
gen rel_4_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & pair1==pair1[_n-3] & bank_tercile==3
gen rel_5_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & pair1==pair1[_n-3] & pair1==pair1[_n-4] & bank_tercile==3
gen rel_max_small = pair1==pair1[_n-1] & pair1==pair1[_n-2] & pair1==pair1[_n-3] & pair1==pair1[_n-4] & pair1==pair1[_n-5] & bank_tercile==3

*** dropped many bank-level variables and changed relation variables & then dropped duplicates to arrive at the dataset ***
drop sa_company_name sa_total_assets avgasset avgassettercile lnbankasset lnnbank countrb totalrbfirmcount counttb totaltbfirmcount totalfirmcount rbtototalfirmcount sa_total_assets_billion sa_total_assets_trillion
drop bank_asset_avg bank_tercile pair1 is_bank public private foreign coop pfi is_rel pair bankcode cocode bankername

by companyname year_n: egen rel_1_small_1=max(rel_1_small)
by companyname year_n: egen rel_2_small_1=max(rel_2_small)
by companyname year_n: egen rel_3_small_1=max(rel_3_small)
by companyname year_n: egen rel_4_small_1=max(rel_4_small)
by companyname year_n: egen rel_5_small_1=max(rel_5_small)
by companyname year_n: egen rel_max_small_1=max(rel_max_small)
drop rel_1_small rel_2_small rel_3_small rel_4_small rel_5_small rel_max_small 
 
duplicates drop
*** save this new dataset as relationtemp - number of bankers - small firm bank case.dta ***
*** use this data to get table 13 and table 14 - two each ***
gen int1_small = rel_1_small_1*avgcrr
gen int10_small=rel_1_small_1*avgrepo

*** with FE ***
areg bankborrow2 int1_small avgcrr rel_1_small_1 i.year_n if inlist(firm_tercile,2,3),absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 25 yes Firm FE yes Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int1_small avgcrr rel_1_small_1 ln_assets ebit_assets tobin i.year_n if inlist(firm_tercile,2,3) ,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 25 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int10_small avgrepo rel_1_small_1 i.year_n if inlist(firm_tercile,2,3),absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 25 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int10_small avgrepo rel_1_small_1 ln_assets ebit_assets tobin i.year_n if inlist(firm_tercile,2,3),absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 25 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 25 yes Firm FE yes Year FE.txt"

*** without  FE ***
reg bankborrow2 int1_small avgcrr rel_1_small_1 if inlist(firm_tercile,2,3),cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 26 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int1_small avgcrr rel_1_small_1 ln_assets ebit_assets tobin if inlist(firm_tercile,2,3),cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 26 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10_small avgrepo rel_1_small_1 if inlist(firm_tercile,2,3),cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 26 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10_small avgrepo rel_1_small_1 ln_assets ebit_assets tobin if inlist(firm_tercile,2,3),cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 26 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 26 no Firm FE no Year FE.txt"


*** with FE ***
areg bankborrow2 int1_small avgcrr rel_1_small i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 yes Firm FE yes Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int1_small avgcrr rel_1_small ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int10_small avgrepo rel_1_small i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 int10_small avgrepo rel_1_small ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,Yes,Year FE,Yes) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 yes Firm FE yes Year FE.txt"

*** without  FE ***
reg bankborrow2 int1_small avgcrr rel_1_small,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int1_small avgcrr rel_1_small ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10_small avgrepo rel_1_small,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 int10_small avgrepo rel_1_small ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat  addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 13 no Firm FE no Year FE.txt"

gen int11_small=rel_2_small*avgcrr
gen int12_small=rel_3_small*avgcrr
gen int13_small=rel_4_small*avgcrr
gen int14_small=rel_5_small*avgcrr

***  with firm FE and year FE ***
areg bankborrow2 avgcrr avgrepo int1_small rel_1_small ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 yes Firm FE yes Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 avgcrr avgrepo int11_small rel_2_small ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 avgcrr avgrepo int12_small rel_3_small ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 avgcrr avgrepo int13_small rel_4_small ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 avgcrr avgrepo int14_small rel_5_small ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,Yes) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 yes Firm FE yes Year FE.txt"

*** without firm and year FE ***
reg bankborrow2 avgcrr avgrepo int1_small rel_1_small ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int11_small rel_2_small ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int12_small rel_3_small ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int13_small rel_4_small ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 avgcrr avgrepo int14_small rel_5_small ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 14 no Firm FE no Year FE.txt"



*** small firm X small bank case ***
cd "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relationtemp - number of bankers.dta"
br

*** this data has bank assets and policyrates ***
*** finding small firms ***
sort companyname year_n
bysort companyname: egen firm_asset_avg=mean(totalassets)
egen firm_tercile = xtile(firm_asset_avg),n(3)

*** finding small banks ***
sort bankername year_n
bysort bankername: egen bank_asset_avg=mean(sa_total_assets)
egen bank_tercile = xtile(bank_asset_avg),n(3)

gen small_firm=0
replace small_firm=1 if firm_tercile==1
gen small_bank=0
replace small_bank=1 if bank_tercile==1
sort companyname  year_n bankername

drop sa_company_name sa_total_assets avgasset avgassettercile lnbankasset lnnbank countrb totalrbfirmcount counttb totaltbfirmcount totalfirmcount rbtototalfirmcount sa_total_assets_billion sa_total_assets_trillion
drop bank_asset_avg bank_tercile is_bank public private foreign coop pfi is_rel pair bankcode cocode bankername
duplicates drop

gen intsmall_firm_bank_crr=small_firm * small_bank * avgcrr
gen intsmall_firm_bank=small_firm * small_bank
gen intsmall_firm_crr=small_firm * avgcrr
gen intsmall_bank_crr=small_bank * avgcrr

areg bankborrow2 intsmall_firm_bank intsmall_firm_crr intsmall_bank_crr small_firm small_bank avgcrr i.year_n,absorb(companyname) cluster(companyname)


areg bankborrow2 intsmall_firm_bank_crr intsmall_firm_bank intsmall_firm_crr intsmall_bank_crr small_firm small_bank avgcrr i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 27 yes Firm FE yes Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,Yes) excel 
areg bankborrow2 intsmall_firm_bank_crr intsmall_firm_bank intsmall_firm_crr intsmall_bank_crr small_firm small_bank avgcrr ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 27 yes Firm FE yes Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,Yes,Year FE,Yes) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 27 yes Firm FE yes Year FE.txt"

reg bankborrow2 intsmall_firm_bank_crr intsmall_firm_bank intsmall_firm_crr intsmall_bank_crr small_firm small_bank avgcrr ,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 27 no Firm FE no Year FE.xls", replace nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
reg bankborrow2 intsmall_firm_bank_crr intsmall_firm_bank intsmall_firm_crr intsmall_bank_crr small_firm small_bank avgcrr ln_assets ebit_assets tobin,cluster(companyname)
outreg2 using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 27 no Firm FE no Year FE.xls", append nocons addstat(Adj R-squared,e(r2_a)) dec(3) tstat addtext (Firm FE,No,Year FE,No) excel 
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Table 27 no Firm FE no Year FE.txt"

*** Regression on number of bankers ***
*** Take relationtemp - number of bankers.dta file which we use for number of bankers ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relationtemp - number of bankers.dta"
br
sort companyname year bankername
bysort companyname year: gen nbank = _N

keep companyname year_n nbank ln_assets ebit_assets tobin currentratio leverage companycode ln_sales tangibility 
save "relationtemp - number of bankers-working.dta"
use "relationtemp - number of bankers-working.dta"
duplicates drop
gen lnnbank=log(nbank)

*** frozen ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky"
reg nbank ln_assets ebit_assets tobin currentratio tangibility
est store d1
areg nbank ln_assets ebit_assets tobin currentratio tangibility i.year_n,absorb(companycode) cluster(companycode)
est store d2
reg lnnbank ln_assets ebit_assets tobin currentratio tangibility
est store d3
areg lnnbank ln_assets ebit_assets tobin currentratio tangibility i.year_n,absorb(companycode) cluster(companycode)
est store d4
outreg2 [d1 d2 d3 d4] using "Firm size and no. of bankers.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Firm size and no. of bankers.txt"
*** frozen ***

*** new frozen ***
areg nbank ln_assets ebit_assets tobin i.year_n,absorb(companycode) cluster(companycode)
est store y1
areg lnnbank ln_assets ebit_assets tobin i.year_n,absorb(companycode) cluster(companycode)
est store y2
outreg2 [y1 y2] using "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Firm size and no. of bankers 2.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, Yes, Year FE, Yes,)
erase "Firm size and no. of bankers 2.txt"
*** new frozen ***


*** Prof Subbu suggestion ***
merge m:1 year_n using "policyrates.dta"
merge m:1 year_n using "policyrates - Venky.dta"
sort companyname year_n
by companyname: gen lag_avgcrr=avgcrr[_n-1]
by companyname: gen lag_avgrepo=avgrepo[_n-1]

areg lnnbank lag_avgcrr lag_avgrepo ln_assets ebit_assets tobin currentratio,absorb(companyname)

*** Relation between firm characteristics and relationship banking ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relation - Venky.dta"
br

egen sizetercile = xtile(ln_assets), n(3)
egen sizepentile = xtile(ln_assets), n(5)
unique sizetercile
unique sizetercile if sizetercile!=.
unique companyname if sizetercile==1
unique companyname if sizetercile==2
unique companyname if sizetercile==3

unique companyname if sizepentile==1
unique companyname if sizepentile==2
unique companyname if sizepentile==3
unique companyname if sizepentile==4
unique companyname if sizepentile==5

keep if year>=1998
keep if year<2014

*** frozen ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky"
reg rel_1 ln_assets
est store e1
reg rel_1 ln_assets ebit_assets tobin
est store e2
areg rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
est store e3
outreg2 [e1 e2 e3] using "Firm characteristics and RB.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Firm characteristics and RB.txt"
*** frozen ***

*** new frozen ***
areg rel_1 ln_assets ebit_assets tobin i.year_n,absorb(companyname) cluster(companyname)
est store e4
outreg2 [e4] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Firm characteristics and RB extra.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Firm characteristics and RB extra.txt"
*** new frozen ***

*** Relationship between bank size and relationship banking ***
*** take relationtemp.dta and find relationship dummies ***
*** also find banker names, get their data from prowess and merge with relationtemp.dta ***
*** then regress rel_1 with bank size and other characteristics ***

cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relationtemp - number of bankers.dta"
egen bankcode=group(bankername)

merge m:1 bankcode year_n using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\allbankasset1998to2013.dta"
sort companyname year_n
drop if _merge==2

gen lnbankasset=log(sa_total_assets)
areg rel_1 lnbankasset,absorb(bankcode)
areg rel_1 lnbankasset i.year_n,absorb(bankcode) cluster(bankcode)

*** frozen ***
areg rel_1 sa_total_assets,absorb(bankcode)
est store f1
areg rel_1 sa_total_assets i.year_n,absorb(bankcode)
est store f2
outreg2 [f1 f2] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 1.txt"
*** frozen ***

gen sa_total_assets_billion=sa_total_assets/1000
gen sa_total_assets_trillion=sa_total_assets/1000000

reg rel_1 sa_total_assets_trillion
est store f11
areg rel_1 sa_total_assets_trillion,absorb(bankcode)
est store f12
areg rel_1 sa_total_assets_trillion i.year_n,absorb(bankcode)
est store f13
reg rel_1 lnbankasset
est store f14
areg rel_1 lnbankasset,absorb(bankcode)
est store f15
areg rel_1 lnbankasset i.year_n,absorb(bankcode)
est store f16
outreg2 [f11 f12 f13 f14 f15 f16] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 4.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 4.txt"

*** frozen ***
areg rel_1 sa_total_assets_trillion,absorb(bankcode)
est store f11
areg rel_1 sa_total_assets_trillion i.year_n,absorb(bankcode)
est store f12
outreg2 [f11 f12] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 4.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 4.txt"
*** frozen ***

*** frozen ***
areg rel_1 sa_total_assets_trillion,absorb(bankcode) cluster(bankcode)
areg rel_1 sa_total_assets_trillion i.year_n,absorb(bankcode) cluster(bankcode)
*** frozen ***

areg rel_1 sa_total_assets i.year_n,absorb(bankcode)
areg rel_1 lnbankasset i.year_n,absorb(bankcode)

*** create a variable for banks - every year - the proportion of no. RB firms to total firms ***
*** find no. of RB firms every year for all banks ***
sort bankcode year_n rel_1
by bankcode year_n rel_1: gen countrb=_N if rel_1==1
by bankcode year_n: egen totalrbfirmcount=mean(countrb)

sort bankcode year_n rel_1
by bankcode year_n rel_1: gen counttb=_N if rel_1==0
by bankcode year_n: egen totaltbfirmcount=mean(counttb)

gen totalfirmcount=totalrbfirmcount+totaltbfirmcount

gen rbtototalfirm=totalrbfirmcount/totalfirmcount
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky"
use "Bank size and relationship banking.dta"

ren sa_total_assets sa_total_assets_million
gen sa_total_assets_billion=sa_total_assets_million/1000
gen sa_total_assets_trillion=sa_total_assets_million/1000000

*** frozen ***
reg rbtototalfirmcount sa_total_assets
est store g1
areg rbtototalfirmcount sa_total_assets,absorb(bankcode)
est store g2
areg rbtototalfirmcount sa_total_assets i.year_n,absorb(bankcode)
est store g3
reg rbtototalfirmcount lnbankasset
est store g4
areg rbtototalfirmcount lnbankasset,absorb(bankcode)
est store g5
areg rbtototalfirmcount lnbankasset i.year_n,absorb(bankcode)
est store g6
outreg2 [g1 g2 g3 g4 g5 g6] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 2.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 2.txt"
*** frozen ***

areg rbtototalfirmcount sa_total_assets_million i.year_n,absorb(bankcode)

reg rbtototalfirmcount sa_total_assets_trillion

*** frozen ***
reg rbtototalfirmcount sa_total_assets_trillion
est store g11
areg rbtototalfirmcount sa_total_assets_trillion,absorb(bankcode)
est store g12
areg rbtototalfirmcount sa_total_assets_trillion i.year_n,absorb(bankcode)
est store g13
reg rbtototalfirmcount lnbankasset
est store g14
areg rbtototalfirmcount lnbankasset,absorb(bankcode)
est store g15
areg rbtototalfirmcount lnbankasset i.year_n,absorb(bankcode)
est store g16
outreg2 [g11 g12 g13 g14 g15 g16] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 3.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Bank size and RB 3.txt"
*** frozen ***

*** frozen ***
areg rbtototalfirmcount sa_total_assets_trillion,absorb(bankcode) cluster(bankcode)
areg rbtototalfirmcount sa_total_assets_trillion i.year_n,absorb(bankcode) cluster(bankcode)
*** frozen ***

areg rbtototalfirmcount lnbankasset i.year_n,absorb(bankcode)

*** Take non-listed firms and repeat table 4 ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relation - Venky.dta"
*** now collect data on identity of all required companies here and merge ***
sort companycode year_n
merge m:1 companycode using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\11109 companynames identity.dta"
rename _merge _mergeidentityv1
merge m:1 companycode using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\11109 companynames identity v2.dta"
rename _merge _mergeidentityv2
sort companyname year_n

egen nse_symbolcode=group(nse_symbol)
egen nse_symbolcodev2=group(nse_symbolv2)
egen bse_scrip_idcode=group(bse_scrip_id)

reg bankborrow2 int5 rel_1 crr if nse_symbolcodev2==. & bse_scrip_idcode==.
reg bankborrow2 int5 rel_1 crr ln_assets ebit_assets tobin if nse_symbolcodev2==. & bse_scrip_idcode==.

*** frozen ***
reg bankborrow2 int5 rel_1 crr if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.
reg bankborrow2 int5 rel_1 crr ln_assets ebit_assets tobin if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.
areg bankborrow2 int5 rel_1 crr if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.,absorb(companyname)
areg bankborrow2 int5 rel_1 crr ln_assets ebit_assets currentratio if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.,absorb(companyname)

reg bankborrow2 int1 rel_1 avgcrr if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.
est store x1
reg bankborrow2 int1 rel_1 avgcrr ln_assets ebit_assets if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.
est store x2
areg bankborrow2 int1 rel_1 avgcrr if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.,absorb(companyname)
est store x3
areg bankborrow2 int1 rel_1 avgcrr ln_assets ebit_assets if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.,absorb(companyname)
est store x4
reg bankborrow2 int10 rel_1 avgrepo if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.
est store x5
reg bankborrow2 int10 rel_1 avgrepo ln_assets ebit_assets if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.
est store x6
areg bankborrow2 int10 rel_1 avgrepo if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.,absorb(companyname)
est store x7
areg bankborrow2 int10 rel_1 avgrepo ln_assets ebit_assets if nse_symbolcodev2==. & bse_scrip_code==. & bse_code==. & bse_scrip_idcode==.,absorb(companyname)
est store x8
outreg2 [x1 x2 x3 x4 x5 x6 x7 x8] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Non-listed firms and RB 1.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Non-listed firms and RB 1.txt"
*** frozen ***


*** Regression on shift in relationship banking - growth in sales,profit,losses etc. ***
*** create a dummy which shows shift from rb-to-tb and tb-to-rb ***
cd "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\dta files"
use "relation - Venky.dta"
br

sort companyname year_n
gen rbtotbdum=0
by companyname: replace rbtotbdum=1 if rel_1==0 & rel_1[_n-1]==1

gen tbtorbdum=0
by companyname: replace tbtorbdum=1 if rel_1==1 & rel_1[_n-1]==0


unique companyname if year_n==2013
unique companyname if rbtotbdum==1 & year_n==2013
unique companyname if tbtorbdum==1 & year_n==2013

unique companyname if twowayshiftdum==1 & year_n==2012


reg tbtorbdum crr ln_assets
reg tbtorbdum crr ln_assets ebit_assets tobin

*** shift frozen ***
areg rbtotbdum lag_avgcrr lag_avgrepo,absorb(companyname)
est store k1
areg tbtorbdum lag_avgcrr lag_avgrepo,absorb(companyname)
est store k2
areg twowayshiftdum lag_avgcrr lag_avgrepo,absorb(companyname)
est store k3

areg rbtotbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin,absorb(companyname)
est store k4
areg tbtorbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin,absorb(companyname)
est store k5
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin,absorb(companyname)
est store k6

areg rbtotbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_log_b,absorb(companyname)
est store k7
areg tbtorbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_log_b,absorb(companyname)
est store k8
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_log_b,absorb(companyname)
est store k9

areg rbtotbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_log_bb,absorb(companyname)
est store k10
areg tbtorbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_log_bb,absorb(companyname)
est store k11
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_log_bb,absorb(companyname)
est store k12

areg rbtotbdum lag_avgcrr lag_avgrepo gr_ln_sales gr_ebit_asset gr_tobin,absorb(companyname)
est store k13
areg tbtorbdum lag_avgcrr lag_avgrepo gr_ln_sales gr_ebit_asset gr_tobin,absorb(companyname)
est store k14
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_sales gr_ebit_asset gr_tobin,absorb(companyname)
est store k15

areg rbtotbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_leverage,absorb(companyname)
est store k16
areg tbtorbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_leverage,absorb(companyname)
est store k17
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_leverage,absorb(companyname)
est store k18

areg rbtotbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_currentratio,absorb(companyname)
est store k22
areg tbtorbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_currentratio,absorb(companyname)
est store k23
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_asset gr_tobin gr_currentratio,absorb(companyname)
est store k24
outreg2 [k1- k24] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\TB-RB & RB-TB & Twoway.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, Yes, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Shift from TB to RB.txt"

areg rbtotbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ln_sales gr_ebit_asset gr_tobin gr_currentratio gr_log_b gr_log_bb gr_leverage,absorb(companyname)
est store k25
areg tbtorbdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ln_sales gr_ebit_asset gr_tobin gr_currentratio gr_log_b gr_log_bb gr_leverage,absorb(companyname)
est store k26
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ln_sales gr_ebit_asset gr_tobin gr_currentratio gr_log_b gr_log_bb gr_leverage,absorb(companyname)
est store k27
outreg2 [k25- k27] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\TB-RB & RB-TB & Twoway v1.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel  tstat addtext (Firm FE, Yes, Year FE, No,) append
erase  "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\TB-RB & RB-TB & Twoway v1.txt"


areg rbtotbdum lag_avgcrr lag_avgrepo ln_assets ln_sales ebit_asset tobin log_b log_bb currentratio,absorb(companyname)
areg tbtorbdum lag_avgcrr lag_avgrepo ln_assets ln_sales ebit_asset tobin log_b log_bb currentratio,absorb(companyname)
areg twowayshiftdum lag_avgcrr lag_avgrepo ln_assets ln_sales ebit_asset tobin log_b log_bb currentratio,absorb(companyname)

areg rbtotbdum lag_avgcrr lag_avgrepo ln_assets ln_sales ebit_asset tobin currentratio log_b log_bb,absorb(companyname)
areg tbtorbdum lag_avgcrr lag_avgrepo ln_assets ln_sales ebit_asset tobin currentratio log_b log_bb,absorb(companyname)
areg twowayshiftdum lag_avgcrr lag_avgrepo ln_assets ln_sales ebit_asset tobin currentratio log_b log_bb,absorb(companyname)
*** shift frozen ***

areg tbtorbdum crr ln_assets ebit_assets tobin,absorb(companyname)

*** frozen ***
areg tbtorbdum ln_assets ebit_assets tobin,absorb(companyname)
est store i1
areg tbtorbdum ln_assets ebit_assets tobin i.year_n,absorb(companyname)
est store i2
areg tbtorbdum ln_assets ebit_assets tobin tangibility leverage currentratio i.year_n,absorb(companyname)
est store i3
outreg2 [i1 i2 i3] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Shift from TB to RB.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Shift from TB to RB.txt"
*** frozen ***

*** Prof. Subbu suggestion - add crr,repo to RB-TB shifts ***

sort companyname year_n
by companyname: gen lag_avgcrr = avgcrr[_n-1]
by companyname: gen lag2_avgcrr = avgcrr[_n-2]
by companyname: gen lag_avgrepo = avgrepo[_n-1]
by companyname: gen lag2_avgrepo = avgrepo[_n-2]

gen twowayshiftdum=0
replace twowayshiftdum=1 if tbtorbdum==1 | rbtotbdum==1

logit tbtorbdum lag_avgcrr lag2_avgcrr lag_avgrepo lag2_avgrepo ln_assets ebit_assets tobin 
est store n1
logit rbtotbdum lag_avgcrr lag2_avgcrr lag_avgrepo lag2_avgrepo ln_assets ebit_assets tobin
est store n2
outreg2 [n1 n2] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Impact of MP variables - logit.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)

areg tbtorbdum lag_avgcrr lag_avgrepo ln_assets ebit_assets tobin,absorb(companyname)
est store l1
areg rbtotbdum lag_avgcrr lag_avgrepo ln_assets ebit_assets tobin,absorb(companyname)
est store l2
outreg2 [l1 l2] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Impact of MP variables - lag.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Impact of MP variables - lag.txt"

areg twowayshiftdum avgcrr ln_assets ebit_assets tobin,absorb(companyname) 
est store o1
areg twowayshiftdum avgrepo ln_assets ebit_assets tobin,absorb(companyname) 
est store o2
areg twowayshiftdum avgcrr avgrepo ln_assets ebit_assets tobin,absorb(companyname) 
est store o3
areg twowayshiftdum lag_avgcrr ln_assets ebit_assets tobin,absorb(companyname) 
est store o4
areg twowayshiftdum lag_avgrepo ln_assets ebit_assets tobin,absorb(companyname) 
est store o5
areg twowayshiftdum lag_avgcrr lag_avgrepo ln_assets ebit_assets tobin,absorb(companyname) 
est store o6
outreg2 [o1 o2 o3 o4 o5 o6] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Impact of MP variables - twowayshift.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "Impact of MP variables - twowayshift.txt"

sort companyname year_n
by companyname: gen gr_ln_assets=ln_assets-ln_assets[_n-1]
by companyname: gen gr_ebit_assets=ebit_assets-ebit_assets[_n-1]
by companyname: gen gr_tobin=tobin-tobin[_n-1]
by companyname: gen gr_ln_sales=ln_sales-ln_sales[_n-1]
by companyname: gen gr_leverage=leverage-leverage[_n-1]
by companyname: gen gr_log_b=log_b-log_b[_n-1]
by companyname: gen gr_log_bb=log_bb-log_bb[_n-1]
by companyname: gen gr_currentratio=currentratio-currentratio[_n-1]

areg twowayshiftdum l1_avgcrr,absorb(companyname)
areg twowayshiftdum l1_avgrepo,absorb(companyname)
areg twowayshiftdum lag_avgcrr lag_avgrepo,absorb(companyname)
areg twowayshiftdum lag_avgcrr lag_avgrepo gr_ln_assets gr_ebit_assets gr_tobin,absorb(companyname)

areg twowayshiftdum lag_avgrepo ln_assets ebit_assets tobin,absorb(companyname) cluster(companyname)
areg twowayshiftdum avgcrr ln_assets ebit_assets tobin,absorb(companyname)
areg twowayshiftdum avgcrr avgrepo ln_assets ebit_assets tobin,absorb(companyname)
areg twowayshiftdum lag_avgcrr ln_assets ebit_assets tobin,absorb(companyname) 
areg twowayshiftdum lag_avgrepo ln_assets ebit_assets tobin,absorb(companyname) 
areg twowayshiftdum avgcrr avgrepo bankborrow2 ln_assets ebit_assets tobin,absorb(companyname) 

areg  twowayshiftdum avgcrr lag_avgcrr 

areg lead_tbtorbdum avgcrr avgrepo ln_assets ebit_assets tobin,absorb(companyname)
areg lead_rbtotbdum avgcrr avgrepo ln_assets ebit_assets tobin,absorb(companyname)


areg tbtorbdum avgcrr,absorb(companyname)
areg rbtotbdum avgcrr  avgrepo ln_assets ebit_assets tobin,absorb(companyname)
areg rbtotbdum avgcrr ,absorb(companyname)

areg rbtotbdum avgcrr ln_assets ebit_assets tobin,absorb(companyname)

sort companyname year_n
by companyname: gen lead_twowayshiftdum=lead_twowayshiftdum[_n+1]
gen lead_tbtorbdum=tbtorbdum[_n+1]
gen lead_rbtotbdum=rbtotbdum[_n+1]
by year_n: gen lag_avgcrr=avgcrr[_n-1]
by year_n: gen lag_avgrepo=avgrepo[_n-1]

*** Prof. Subbu suggestion ***

reg rbtotbdum crr ln_assets
reg rbtotbdum crr ln_assets ebit_assets tobin
areg rbtotbdum crr ln_assets,absorb(companyname)

*** frozen ***
areg rbtotbdum ln_assets ebit_assets tobin,absorb(companyname)
est store j1
areg rbtotbdum ln_assets ebit_assets tobin i.year_n,absorb(companyname)
est store j2
areg rbtotbdum ln_assets ebit_assets tobin tangibility leverage currentratio i.year_n,absorb(companyname)
est store j3
outreg2 [j1 j2 j3] using "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Shift from RB to TB.xls", label addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Firm FE, No, Year FE, No,)
erase "D:\Google drive\Working\Professor KS\JFI work\Liquidity Creation\Venky\Shift from RB to TB.txt"
*** frozen ***

*** graphs work ***

*** graph 1 - sensitivity graph ***

*** frozen ***
use "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files\relation - Copy.dta",clear
br
egen companyid = group(companyname)
tsset companyid year_n, yearly

gen bbf1 = f1.bankborrow2
gen bbf2 = f2.bankborrow2
gen bbf3 = f3.bankborrow2
gen bbl0 = l0.bankborrow2
gen bbl1 = l1.bankborrow2
gen bbl2 = l2.bankborrow2
gen bbl3 = l3.bankborrow2

gen lag_ln_assets_f1 = f1.lag_ln_assets
gen lag_ln_assets_f2 = f2.lag_ln_assets
gen lag_ln_assets_f3 = f3.lag_ln_assets
gen lag_ln_assets_l0 = l0.lag_ln_assets
gen lag_ln_assets_l1 = l1.lag_ln_assets
gen lag_ln_assets_l2 = l2.lag_ln_assets
gen lag_ln_assets_l3 = l3.lag_ln_assets

gen lag_ebit_assets_f1 = f1.lag_ebit_assets
gen lag_ebit_assets_f2 = f2.lag_ebit_assets
gen lag_ebit_assets_f3 = f3.lag_ebit_assets
gen lag_ebit_assets_l0 = l0.lag_ebit_assets
gen lag_ebit_assets_l1 = l1.lag_ebit_assets
gen lag_ebit_assets_l2 = l2.lag_ebit_assets
gen lag_ebit_assets_l3 = l3.lag_ebit_assets

gen lag_tobin_f1 = f1.lag_tobin
gen lag_tobin_f2 = f2.lag_tobin
gen lag_tobin_f3 = f3.lag_tobin
gen lag_tobin_l0 = l0.lag_tobin
gen lag_tobin_l1 = l1.lag_tobin
gen lag_tobin_l2 = l2.lag_tobin
gen lag_tobin_l3 = l3.lag_tobin

gen int1 = rel_1*avgcrr
xi i.year_n
est clear

areg bbl3 int1 avgcrr rel_1 lag_ln_assets_l3 lag_ebit_assets_l3 lag_tobin_l3 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a_3
mat rtable = r(table)
mat rel_l3 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l3 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l3 = (-3,rel_l3,tra_l3)

areg bbl2 int1 avgcrr rel_1 lag_ln_assets_l2 lag_ebit_assets_l2 lag_tobin_l2 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a_2
mat rtable = r(table)
mat rel_l2 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l2 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l2 = (-2,rel_l2,tra_l2)

areg bbl1 int1 avgcrr rel_1 lag_ln_assets_l1 lag_ebit_assets_l1 lag_tobin_l1 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a_1
mat rtable = r(table)
mat rel_l1 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l1 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l1 = (-1,rel_l1,tra_l1)

areg bbl0 int1 avgcrr rel_1 lag_ln_assets_l0 lag_ebit_assets_l0 lag_tobin_l0 avgrepo _I*, absorb(companyname) cluster(companyname)
est store a_0
mat rtable = r(table)
mat rel_l0 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l0 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l0 = (0,rel_l0,tra_l0)

areg bbf1 int1 avgcrr rel_1 lag_ln_assets_f1 lag_ebit_assets_f1 lag_tobin_f1 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a1
mat rtable = r(table)
mat rel_f1 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_f1 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_f1 = (1,rel_f1,tra_f1)

areg bbf2 int1 avgcrr rel_1 lag_ln_assets_f2 lag_ebit_assets_f2 lag_tobin_f2 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a2
mat rtable = r(table)
mat rel_f2 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_f2 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_f2 = (2,rel_f2,tra_f2)

areg bbf3 int1 avgcrr rel_1 lag_ln_assets_f3 lag_ebit_assets_f3 lag_tobin_f3 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a3
mat rtable = r(table)
mat rel_f3 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_f3 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_f3 = (3,rel_f3,tra_f3)

outreg2 [a*] using "Table9", label drop(_I* ) sortvar(int1 avgcrr rel_1)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year * Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table9.txt"

*MAKING GRAPH FOR LEAD LAG TABLE
clear
mat a = (a_l2\ a_l1\ a_l0\ a_f1\ a_f2)
svmat a, names(reg)
ren reg1 time
ren reg2 betaRelationship
ren reg3 beta_lowRelationship
ren reg4 beta_highRelationship
ren reg5 betaTransactional
ren reg6 beta_lowTransactional
ren reg7 beta_highTransactional
reshape long beta beta_low beta_high, i(time) j(banking_category) string
gen zero = 0
graph twoway (connected beta time) (rcap beta_low beta_high time,  lwidth(medthin)) (line zero time, lpattern(shortdash)), by(banking_category)  ytitle(Beta Coefficient) xtitle(Time (w.r.t  year of CRR change)) by(, title(Sensitivity to CRR) subtitle(Correlation: Bank loan to Total loan (at time T=t) with Avg CRR (at time T=0))) 
*** frozen ***
*** graph 1 - sensitivity graph ***

*** graph 2 - Policy rates graph ***
use "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files\relation - Venky.dta",clear
br

keep if year>1997 & year<2014
preserve
twoway (line crr year_n, sort lwidth(medthin)) (line repo year_n, sort lwidth(medthin) lpattern(dash)) /// 
(line revrepo year_n, sort lwidth(medthin) lpattern(dash_dot)) ///
(line bankrate year_n, sort lwidth(medthin) lpattern(shortdash_dot_dot)), ytitle(Percentage) xtitle(Year) xlabel(1998 2001 2004 2007 2010 2013)   
restore
*** graph 2 - Policy rates graph ***

*** graph 3 - fraction of borrowing ***
use "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files\relation - Venky.dta",clear
br

save "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files\relation - Venky-copy.dta", replace
use "D:\Google Drive\Working\Professor KS\JFI work\Liquidity Creation\dta files\relation - Venky-copy.dta", clear
sort companyname year_n
bysort companyname: gen yearindex = _n

collapse (mean) bankborrow2 finborrow cmborrow icborrow forborrow prborrow othborrow, by(yearindex)
*** frozen ***
collapse (mean) bankborrow2 finborrow cmborrow icborrow forborrow prborrow othborrow, by(year_n)
label var bankborrow2 "Bank borrowing"
label var finborrow "Financial Institutions borrowing"
label var icborrow "Inter-corporate borrowing"
label var forborrow "Foreign borrowing"
twoway (line bankborrow2 year_n, sort) (line finborrow year_n, sort lpattern(dash)) (line icborrow year_n, sort  lpattern(dash_dot)) (line forborrow year_n, sort  lpattern(shortdash_dot_dot)), ytitle(Fraction of Borrowing) xtitle(Year) xlabel(1998 2001 2004 2007 2010 2013)
*** frozen ***

*** graph 3 - fraction of borrowing ***

*** graphs work ***

















************************************************************************************************

areg bankborrow2 int1 rel_1 avgcrr avgrepo _I*, absorb(companyname) cluster(companyname)
est store abb_crr1
areg bankborrow2 int1 rel_1 avgcrr lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crr2
areg bankborrow2 int2 rel_1 avgrepo  _I*, absorb(companyname) cluster(companyname)
est store abb_repo1
areg bankborrow2 int2 rel_1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_repo2

label variable int1 "Exclusive Bank * Avg CRR"
label variable int2 "Exclusive Bank * Avg Repo Rate"
label variable int3 "Exclusive Bank * Avg SLR"
label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"
label variable avgslr "Avg SLR"
outreg2 [a*] using "Table1 (Preliminary)", label drop(_I* ) sortvar(int1 avgcrr int2 avgrepo int3 avgslr)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table1 (Preliminary).txt"

*NEXT TABLE WITH BORROWING LEVELS

use relation,clear

xi i.year_n
est clear

gen int1 = rel_1*avgcrr
gen int2 = rel_1*avgrepo

areg log_b int1 rel_1 avgcrr  _I*, absorb(companyname) cluster(companyname)
est store aLog_Borrow0
areg log_bb int1 rel_1 avgcrr  _I*, absorb(companyname) cluster(companyname)
est store aLog_BankBorrow0
areg log_nbb int1 rel_1 avgcrr  _I*, absorb(companyname) cluster(companyname)
est store aLog_NonBankBorrow0
areg log_b int2 rel_1  avgrepo _I*, absorb(companyname) cluster(companyname)
est store aLog_Borrow2
areg log_bb int2 rel_1  avgrepo _I*, absorb(companyname) cluster(companyname)
est store aLog_BankBorrow3
areg log_nbb int2 rel_1  avgrepo _I*, absorb(companyname) cluster(companyname)
est store aLog_NonBankBorrow4

label variable int1 "Exclusive Bank * Avg CRR"
label variable int2 "Exclusive Bank * Avg Repo Rate"
label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"

outreg2 [a*] using "Table2 (Log values)", label drop(_I* ) sortvar(int1 avgcrr int2 avgrepo)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table2 (Log values).txt"

*PUBLIC BANKS ONLY

cd "...\dta files"
use relationtemp,clear
merge m:1 year_n using policyrates
drop _m

bysort companyname year_n: gen totalbanks=_N
bysort companyname year_n: egen totalpublic=sum(public)
keep if totalpublic==totalbanks

sort companyname year bankername
egen pair = group(companyname bankername)
bysort companyname year: gen nbank = _N
gen rel_1 = nbank == 1
gen rel_2 = (nbank==1 & pair==pair[_n-1])
gen rel_3 = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2])
gen rel_4 = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2] & pair==pair[_n-3])
gen rel_5 = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2] & pair==pair[_n-3] & pair==pair[_n-4])
gen rel_max = (nbank==1 & pair==pair[_n-1] & pair==pair[_n-2] & pair==pair[_n-3] & pair==pair[_n-4] & pair==pair[_n-5])

collapse (sum) is_bank public rel_1 rel_2 rel_3 rel_4 rel_5 rel_max, by (companyid companyname industrygroup indcode year_n tight tight2 lag_tight2 is_group is_pub ln_assets ln_sales ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio mci bankrate repo repolag revrepolag msf slr avgbankrate avgrepo avgrepoup avgrevrepo avgcrr avgmsf avgslr mp crr crrch crrup avgcrrup crrdown lag_crr revrepo lag_repo bankborrow bankborrow2 shbankborrow lobankborrow secbankborrow unsecbankborrow finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow tradecredit borrowingfrombanks borrowings borrowings2)
sort companyname year_n
collapse (sum) is_bank public rel_1, by (companyname industrygroup indcode year_n tight tight2 lag_tight2 is_group is_pub ln_assets ln_sales ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio mci bankrate repo repolag revrepolag msf slr avgbankrate avgrepo avgrepoup avgrevrepo avgcrr avgmsf avgslr mp crr crrch crrup avgcrrup crrdown lag_crr revrepo lag_repo bankborrow bankborrow2 shbankborrow lobankborrow secbankborrow unsecbankborrow finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow tradecredit borrowingfrombanks borrowings borrowings2)
sort companyname year_n

save relation,replace

use relation,clear

gen log_b = log(1+borrowings2)
label variable log_b "Log_Borrowings"
gen log_bb = log(1+borrowingfrombanks)
label variable log_bb "Log_BankBorrowings"
gen log_nbb = log(1+(borrowings2-borrowingfrombanks))
label variable log_nbb "Log_Non-BankBorrowings"
gen nonbankborrow2 = 1 - bankborrow2
label variable nonbankborrow2 "Non-bank borrowing as fraction of borrowing"

egen companycode = group(companyname)
tsset companycode year, yearly
sort companyname year_n
bysort companyname: gen lag_rel_1 = rel_1[_n-1]
bysort companyname: gen lag_ln_assets = ln_assets[_n-1]
bysort companyname: gen lag_ebit_assets = ebit_assets[_n-1]
bysort companyname: gen lag_tobin = tobin[_n-1]
bysort companyname: gen lag_leverage = leverage[_n-1]



*** Venky public banks ***
gen int10 = rel_1*crr 
gen int3 = rel_1*avgslr

*** Table 6 - original ones ***
areg log_b int10 crr rel_1 repo i.year_n, absorb(companyname) cluster(companyname)
areg log_bb int10 crr rel_1 repo i.year_n, absorb(companyname) cluster(companyname)
areg log_nbb int10 crr rel_1 repo i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int10 crr rel_1 repo ln_assets ebit_assets log_b currentratio tobin i.year_n, absorb(companyname) cluster(companyname)

areg bankborrow2 int10 crr rel_1 repo i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int10 crr rel_1 repo i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int10 crr rel_1 repo i.year_n, absorb(companyname) cluster(companyname)


*** Table 9 in excel results ***
areg bankborrow2 int1 avgcrr rel_1 i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int1 avgcrr rel_1 lag_ln_assets lag_ebit_assets lag_tobin i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int2 rel_1 avgrepo i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int2 rel_1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int3 avgslr rel_1 i.year_n, absorb(companyname) cluster(companyname)
areg bankborrow2 int3 avgslr rel_1 lag_ln_assets lag_ebit_assets lag_tobin i.year_n, absorb(companyname) cluster(companyname)






/*CLEANING DATA*/
winsor2 bankborrow2 nonbankborrow2 log_b log_bb log_nbb ln_assets ebit_assets leverage currentratio tobin, replace cuts(1 99)

/*FILTER DATA FOR DATES*/
drop if year_n < 1997

/*LABELLING THE DATA*/
label variable crr "CRR"
label variable repo "Repo Rate"
label variable rel_1 "Exclusive Bank (1 yr)"
label variable rel_2 "Exclusive Bank (2 yr)"
label variable rel_3 "Exclusive Bank (3 yr)"
label variable rel_4 "Exclusive Bank (4 yr)"
label variable rel_5 "Exclusive Bank (5 yr)"
label variable rel_max "Exclusive Bank (>=6 yr)"
label variable bankborrow2 "Bank borr. ratio"
label variable nonbankborrow2 "Non-bank borr. ratio"
label variable finborrow "Financial institutions borr. ratio"
label variable govborrow "Government borr. ratio"
label variable cmborrow "Capital market borr. ratio"
label variable icborrow "Inter-corporate borr. ratio"
label variable forborrow "Foreign borr. ratio"
label variable prborrow "Promoter borr. ratio"
label variable othborrow "Other borr. ratio"
label variable cpborrow "CP borr. ratio"
label variable shbankborrow "Short-term bank borr. ratio"
label variable lobankborrow "Long-term bank borr. ratio"
label variable secbankborrow "Secured bank borr. ratio"
label variable unsecbankborrow "Unsecured bank borr. ratio"
label variable log_b "Log(Total borrowing amount)"
label variable log_bb "Log(Bank borrowing amount)"
label variable log_nbb "Log(Non-bank borrowing amount)"
label variable ln_assets "Size"
label variable ln_sales "Sales"
label variable ebit_assets "Profitability"
label variable leverage "Leverage"
label variable currentratio "Current Ratio"
label variable tobin "Tobin's Q"

save relationtemp2,replace

use relationtemp2,clear
xi i.year_n
est clear
gen int1 = rel_1*avgcrr
gen int2 = rel_1*avgrepo
gen int3 = rel_1*avgslr
areg bankborrow2 int1 rel_1 avgcrr  _I*, absorb(companyname) cluster(companyname)
est store abb_crr1
areg bankborrow2 int1 rel_1 avgcrr lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crr2
areg bankborrow2 int2 rel_1 avgrepo  _I*, absorb(companyname) cluster(companyname)
est store abb_repo1
areg bankborrow2 int2 rel_1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_repo2
label variable int1 "Exclusive Bank * Avg CRR"
label variable int2 "Exclusive Bank * Avg Repo Rate"
label variable int3 "Exclusive Bank * Avg SLR"
label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"
label variable avgslr "Avg SLR"

outreg2 [a*] using "Table3(Publicbanks)", label drop(_I* ) sortvar(int1 avgcrr int2 avgrepo int3 avgslr)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table3(Publicbanks).txt"


/*TABLE 4 IS SUBSUMED IN TABLE 1 NOW*/


*SECONDARY TABLE WITH LOAN DURATION AND TERMS
use relation,clear
xi i.year_n
est clear
gen int1 = rel_1*avgcrr
gen int2 = lag_ln_assets*avgcrr
gen int3 = lag_ebit_assets*avgcrr
gen int4 = lag_tobin*avgcrr
areg lobankborrow avgcrr rel_1 int1 avgrepo _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratio1
areg lobankborrow avgcrr rel_1 int1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc
areg unsecbankborrow  avgcrr rel_1 int1 avgrepo _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratio145
areg unsecbankborrow  avgcrr rel_1 int1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc67
label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"
label variable int1 "Exclusive Bank * CRR"
label variable int2 "Size * CRR"
label variable int3 "Profitability * CRR"
label variable int4 "lag_tobin's Q * CRR"
outreg2 [a*] using "Table5 (Main2)", label drop(_I* ) sortvar(int1 rel_1 avgcrr avgrepo  rel_1 avgcrr lag_ln_assets lag_ebit_assets lag_tobin int2 int3 int4)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table5 (Main2).txt"

*DURING UP AND DOWN PERIODS
use relation,clear
xi i.year_n
est clear
gen int1 = rel_1*avgcrr
gen int2 = rel_1*avgrepo

gen int11 = avgcrr*tight2
gen int12 = tight2*rel_1
gen int111 = rel_1*avgcrr*tight2
gen int21 = avgrepo*tight2
gen int211 = rel_1*avgrepo*tight2


areg bankborrow2 int1 rel_1 avgcrr tight2 int11 int12 int111 _I*, absorb(companyname) cluster(companyname)
est store abb_crr1
areg bankborrow2 int1 rel_1 avgcrr tight2 int11 int12 int111  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crr2

areg bankborrow2 int2 rel_1 avgrepo tight2 int21 int12 int211 _I*, absorb(companyname) cluster(companyname)
est store abb_repo1
areg bankborrow2 int2 rel_1 avgrepo tight2 int21 int12 int211  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_repo2

label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"
label variable tight2 "Tighten Dummy"
label variable int1 "Exclusive Bank * Avg CRR"
label variable int2 "Exclusive Bank * Avg Repo Rate"
label variable int11 "Avg CRR * Tighten Dummy"
label variable int12 "Exclusive Bank * Tighten Dummy"
label variable int111 "Exclusive Bank * Avg CRR * Tighten Dummy"
label variable int21 "Avg Repo Rate * Tighten Dummy"
label variable int211 "Exclusive Bank * Avg Repo Rate * Tighten Dummy"

outreg2 [a*] using "Table6 (UpDown)", label drop(_I* ) sortvar(int111 int1 int11 avgcrr int211 int2 int21 avgrepo int12 tight2 rel_1 lag_ln_assets lag_ebit_assets lag_tobin)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table6 (UpDown).txt



*ROBUSTNESS - DIFFERENT RELATIONSHIP DURATIONS
use relation,clear
xi i.year_n
est clear
gen int1 = rel_1*avgcrr
gen int2 = rel_2*avgcrr
gen int3 = rel_3*avgcrr
gen int4 = rel_4*avgcrr
gen int5 = rel_5*avgcrr
gen intmax = rel_max*avgcrr
areg bankborrow2 avgcrr rel_1 int1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc1
areg bankborrow2 avgcrr rel_2 int2 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc2
areg bankborrow2 avgcrr rel_3 int3 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc3
areg bankborrow2 avgcrr rel_4 int4 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc4
areg bankborrow2 avgcrr rel_5 int5 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc5
areg bankborrow2 avgcrr rel_max intmax avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store aBankBorr_Ratiowc6
label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"
label variable int1 "Exclusive Bank (1 yr) * Avg CRR"
label variable int2 "Exclusive Bank (2 yr) * Avg CRR"
label variable int3 "Exclusive Bank (3 yr) * Avg CRR"
label variable int4 "Exclusive Bank (4 yr) * Avg CRR"
label variable int5 "Exclusive Bank (5 yr) * Avg CRR"
label variable intmax "Exclusive Bank (>=6 yr) * Avg CRR"
label variable rel_1 "Exclusive Bank (1 yr)"
label variable rel_2 "Exclusive Bank (2 yr)"
label variable rel_3 "Exclusive Bank (3 yr)"
label variable rel_4 "Exclusive Bank (4 yr)"
label variable rel_5 "Exclusive Bank (5 yr)"
label variable rel_max "Exclusive Bank (>=6 yr)"
outreg2 [a*] using "Table7 (Robust_rel)", label drop(_I* ) sortvar(avgcrr avgrepo int1 int2 int3 int4 int5 intmax rel_1 rel_2 rel_3 rel_4 rel_5 rel_max)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table7 (Robust_rel).txt"

/*ROBUSTNESS INDUSTRY TRENDS*/
use relation,clear
xi i.year_n*i.indcode
est clear
gen int1 = rel_1*avgcrr
gen int2 = rel_1*avgrepo
gen int3 = rel_1*avgslr

areg bankborrow2 int1 rel_1 avgcrr  _I*, absorb(companyname) cluster(companyname)
est store abb_crr1
areg bankborrow2 int1 rel_1 avgcrr lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crr2
areg bankborrow2 int2 rel_1 avgrepo  _I*, absorb(companyname) cluster(companyname)
est store abb_repo1
areg bankborrow2 int2 rel_1 avgrepo lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_repo2

label variable int1 "Exclusive Bank * Avg CRR"
label variable int2 "Exclusive Bank * Avg Repo Rate"
label variable int3 "Exclusive Bank * Avg SLR"
label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"
label variable avgslr "Avg SLR"

outreg2 [a*] using "Table8(Ind Trend FE)", label drop(_I* ) sortvar(int1 avgcrr int2 avgrepo rel_1 lag_ln_assets lag_ebit_assets lag_tobin )   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year * Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table8(Ind Trend FE).txt"









use relation,clear
xi i.year_n
est clear
gen intf1 = rel_1*f1_avgcrr
gen intf2 = rel_1*f2_avgcrr
gen intf3 = rel_1*f3_avgcrr
gen int1 = rel_1*avgcrr
gen intl1 = rel_1*l1_avgcrr
gen intl2 = rel_1*l2_avgcrr
gen intl3 = rel_1*l3_avgcrr

areg bankborrow2 intf3 rel_1 f3_avgcrr  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crrf3
areg bankborrow2 intf2 rel_1 f2_avgcrr  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crrf2
areg bankborrow2 intf1 rel_1 f1_avgcrr  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crrf1
areg bankborrow2 int1 rel_1 avgcrr  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crrl0
areg bankborrow2 intl1 rel_1 l1_avgcrr  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crrl1
areg bankborrow2 intl2 rel_1 l2_avgcrr  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crrl2
areg bankborrow2 intl3 rel_1 l3_avgcrr  lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crrl3
outreg2 [a*] using "Table9", label drop(_I* ) sortvar(intf3 intf2 intf1 int1 intl1 intl2 intl3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year * Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table9.txt"

/*MAKING LEAD LAG TABLE ADN GRAPH*/
use relation,clear
egen companyid = group(companyname)
tsset companyid year_n, yearly
gen bbf1 = f1.bankborrow2
gen bbf2 = f2.bankborrow2
gen bbf3 = f3.bankborrow2
gen bbl0 = l0.bankborrow2
gen bbl1 = l1.bankborrow2
gen bbl2 = l2.bankborrow2
gen bbl3 = l3.bankborrow2

gen lag_ln_assets_f1 = f1.lag_ln_assets
gen lag_ln_assets_f2 = f2.lag_ln_assets
gen lag_ln_assets_f3 = f3.lag_ln_assets
gen lag_ln_assets_l0 = l0.lag_ln_assets
gen lag_ln_assets_l1 = l1.lag_ln_assets
gen lag_ln_assets_l2 = l2.lag_ln_assets
gen lag_ln_assets_l3 = l3.lag_ln_assets

gen lag_ebit_assets_f1 = f1.lag_ebit_assets
gen lag_ebit_assets_f2 = f2.lag_ebit_assets
gen lag_ebit_assets_f3 = f3.lag_ebit_assets
gen lag_ebit_assets_l0 = l0.lag_ebit_assets
gen lag_ebit_assets_l1 = l1.lag_ebit_assets
gen lag_ebit_assets_l2 = l2.lag_ebit_assets
gen lag_ebit_assets_l3 = l3.lag_ebit_assets

gen lag_tobin_f1 = f1.lag_tobin
gen lag_tobin_f2 = f2.lag_tobin
gen lag_tobin_f3 = f3.lag_tobin
gen lag_tobin_l0 = l0.lag_tobin
gen lag_tobin_l1 = l1.lag_tobin
gen lag_tobin_l2 = l2.lag_tobin
gen lag_tobin_l3 = l3.lag_tobin

gen int1 = rel_1*avgcrr
xi i.year_n
est clear

areg bbl3 int1 avgcrr rel_1 lag_ln_assets_l3 lag_ebit_assets_l3 lag_tobin_l3 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a_3
mat rtable = r(table)
mat rel_l3 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l3 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l3 = (-3,rel_l3,tra_l3)

areg bbl2 int1 avgcrr rel_1 lag_ln_assets_l2 lag_ebit_assets_l2 lag_tobin_l2 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a_2
mat rtable = r(table)
mat rel_l2 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l2 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l2 = (-2,rel_l2,tra_l2)

areg bbl1 int1 avgcrr rel_1 lag_ln_assets_l1 lag_ebit_assets_l1 lag_tobin_l1 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a_1
mat rtable = r(table)
mat rel_l1 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l1 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l1 = (-1,rel_l1,tra_l1)

areg bbl0 int1 avgcrr rel_1 lag_ln_assets_l0 lag_ebit_assets_l0 lag_tobin_l0 avgrepo _I*, absorb(companyname) cluster(companyname)
est store a_0
mat rtable = r(table)
mat rel_l0 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_l0 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_l0 = (0,rel_l0,tra_l0)

areg bbf1 int1 avgcrr rel_1 lag_ln_assets_f1 lag_ebit_assets_f1 lag_tobin_f1 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a1
mat rtable = r(table)
mat rel_f1 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_f1 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_f1 = (1,rel_f1,tra_f1)


areg bbf2 int1 avgcrr rel_1 lag_ln_assets_f2 lag_ebit_assets_f2 lag_tobin_f2 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a2
mat rtable = r(table)
mat rel_f2 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_f2 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_f2 = (2,rel_f2,tra_f2)

areg bbf3 int1 avgcrr rel_1 lag_ln_assets_f3 lag_ebit_assets_f3 lag_tobin_f3 avgrepo bbl0 _I*, absorb(companyname) cluster(companyname)
est store a3
mat rtable = r(table)
mat rel_f3 = (rtable[1,2]+rtable[1,1], (rtable[1,2]+rtable[1,1])-(1.96*(rtable[2,1]+rtable[2,2])),(rtable[1,2]+rtable[1,1])+(1.96*(rtable[2,1]+rtable[2,2])))
mat tra_f3 = (rtable[1,2], (rtable[1,2])-(1.96*(rtable[2,2])),(rtable[1,2])+(1.96*(rtable[2,2])))
mat a_f3 = (3,rel_f3,tra_f3)

outreg2 [a*] using "Table9", label drop(_I* ) sortvar(int1 avgcrr rel_1)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year * Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table9.txt"

*MAKING GRAPH FOR LEAD LAG TABLE
clear
mat a = (a_l2\ a_l1\ a_l0\ a_f1\ a_f2)
svmat a, names(reg)
ren reg1 time
ren reg2 betaRelationship
ren reg3 beta_lowRelationship
ren reg4 beta_highRelationship
ren reg5 betaTransactional
ren reg6 beta_lowTransactional
ren reg7 beta_highTransactional
reshape long beta beta_low beta_high, i(time) j(banking_category) string
gen zero = 0
graph twoway (connected beta time) (rcap beta_low beta_high time,  lwidth(medthin)) (line zero time, lpattern(shortdash)), by(banking_category)  ytitle(Beta Coefficient) xtitle(Time (w.r.t  year of CRR change)) by(, title(Sensitivity to CRR) subtitle(Correlation: Bank Borr. Ratio (at time T=t) with Avg CRR (at time T=0))) 

*ONLY INTERACTIONS WITH NO CRR
use relation,clear
xi i.year_n
est clear
gen int1 = rel_1*avgcrr
gen int2 = rel_1*avgrepo
gen int3 = rel_1*avgslr

areg bankborrow2 int1 int2 rel_1 _I*, absorb(companyname) cluster(companyname)
est store abb_crr1
areg bankborrow2 int1 int2 rel_1 lag_ln_assets lag_ebit_assets lag_tobin _I*, absorb(companyname) cluster(companyname)
est store abb_crr2

label variable int1 "Exclusive Bank * Avg CRR"
label variable int2 "Exclusive Bank * Avg Repo Rate"
label variable int3 "Exclusive Bank * Avg SLR"
label variable avgcrr "Avg CRR"
label variable avgrepo "Avg Repo Rate"
label variable avgslr "Avg SLR"
outreg2 [a*] using "Table10", label drop(_I* ) sortvar(int1 avgcrr int2 avgrepo int3 avgslr)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table10.txt"

