
cd "...\Data"

insheet using policyrates.csv,c names clear
rename yearend year_n
save policyrates, replace
use policyrates,clear
sort year_n
tsset year_n, yearly
gen dcrr = crr - l.crr
gen drevrepo = revrepo - l.revrepo
gen crrup = dcrr > 0
gen crrdown = dcrr < 0
gen crrch = crrup == 1
replace crrch = -1 if crrdown == 1
gen revrepoup = drevrepo > 0
gen revrepodown = drevrepo < 0
gen revrepoch = revrepoup ==1
replace revrepoch = -1 if revrepodown == 1
keep if year>=1998
replace excrate = 1/excrate
gen ln_exc = log(excrate)
gen baserate = revrepo if year_n == 1998
replace baserate = baserate[_n-1] if baserate >=.
gen basexrate = ln_exc if year_n == 1998
replace basexrate = basexrate[_n-1] if basexrate >=.
gen mci = (0.75)*(revrepo - baserate) + (0.25)*(ln_exc - basexrate)
egen tight = xtile(mci), n(2)
replace tight = tight - 1
gen tight2 = mci - mci[_n-1] > 0
gen lag_crr = crr[_n-1]
gen lag_revrepo = revrepo[_n-1]
gen lag_tight2 = tight2[_n-1]
save "...\dta files\policyrates.dta", replace

insheet using pincodedata.csv,c names clear
ren pincode registeredofficepincode
tostring registeredofficepincode,replace
duplicates drop registeredofficepincode,force
save "...\dta files\pincodedata.dta", replace

insheet using rulingparty.csv,c names clear
rename dist_name district
rename year year_n
duplicates drop
replace district = "AHMEDABAD" if district == "AHMADABAD"
replace district = "AHMEDNAGAR" if district == "AHMADNAGAR"
replace district = "ALAPPUZHA" if district == "ALAPUZHA"
replace district = "BANASKANTHA" if district == "BANAS KANTHA"
replace district = "BANGALORE" if district == "BANGALORE URBAN"
replace district = "BARABANKI" if district == "BARA BANKI"
replace district = "BARDHAMAN" if district == "BARDDHAMAN"
replace district = "BHATINDA" if district == "BATHINDA"
replace district = "BIJNORE" if district == "BIJNOR"
replace district = "BADAUN" if district == "BUDAUN"
replace district = "CHIKAMAGLUR" if district == "CHIKMAGALUR"
replace district = "CHITTORGARH" if district == "CHITTAURGARH"
replace district = "DARJEELING" if district == "DARJILING"
replace district = "DHARWAR" if district == "DHARWAD"
replace district = "FEROZEPUR" if district == "FEROZPUR"
replace district = "GANDHI NAGAR" if district == "GANDHINAGAR"
replace district = "HISSAR" if district == "HISAR"
replace district = "IDUKI" if district == "IDUKKI"
replace district = "KUCHCHH" if district == "KACHCHH"
replace district = "KASARGODE" if district == "KASARAGOD"
replace district = "KENDUJHAR" if district == "KEONJHAR"
replace district = "KODURU" if district == "KODAGU"
replace district = "MAHBOOBNAGAR" if district == "MAHBUBNAGAR"
replace district = "MOHINDERGARH" if district == "MAHENDRAGARH"
replace district = "MALAPURAM" if district == "MALAPPURAM"
replace district = "MANDASUR" if district == "MANDSAUR"
replace district = "NASHIK" if district == "NASIK"
replace district = "THE NILGIRI" if district == "NILGIRIS"
replace district = "MEDINIPUR" if district == "PASCHIM MEDINIPUR"
replace district = "PUDUKOTTAI" if district == "PUDUKKOTTAI"
replace district = "SABARKANTHA" if district == "SABAR KANTHA"
replace district = "SHOLAPUR" if district == "SOLAPUR"
replace district = "SONEPAT" if district == "SONIPAT"
replace district = "TIRUVALLUR" if district == "THIRUVALLUR"
replace district = "TIRUCHIRAPPALLI" if district == "TIRUCHIRAPALLI"
replace district = "TIRUNELVELI" if district == "TIRUNELVALI"
replace district = "VILUPPURAM" if district == "VILLUPURAM"
replace district = "CALCUTTA" if district == "KOLKATA"
replace district = "MUMBAI" if district == "GREATER BOMBAY"
save "...\dta files\rulingparty.dta",replace


insheet using Branchdata.csv,c names clear
gen year_n = substr(dateofopen,8,4)
destring year_n, replace
sort district year_n
bysort district: gen bankdistrict = 1
bysort district : egen bankdistrict1998 = sum(bankdistrict) if year_n <1998
bysort district : egen bankdistrict1999 = sum(bankdistrict) if year_n <1999
bysort district : egen bankdistrict2000 = sum(bankdistrict) if year_n <2000
bysort district : egen bankdistrict2001 = sum(bankdistrict) if year_n <2001
bysort district : egen bankdistrict2002 = sum(bankdistrict) if year_n <2002
bysort district : egen bankdistrict2003 = sum(bankdistrict) if year_n <2003
bysort district : egen bankdistrict2004 = sum(bankdistrict) if year_n <2004
bysort district : egen bankdistrict2005 = sum(bankdistrict) if year_n <2005
bysort district : egen bankdistrict2006 = sum(bankdistrict) if year_n <2006
bysort district : egen bankdistrict2007 = sum(bankdistrict) if year_n <2007
bysort district : egen bankdistrict2008 = sum(bankdistrict) if year_n <2008
bysort district : egen bankdistrict2009 = sum(bankdistrict) if year_n <2009
bysort district : egen bankdistrict2010 = sum(bankdistrict) if year_n <2010
bysort district : egen bankdistrict2011 = sum(bankdistrict) if year_n <2011
bysort district : egen bankdistrict2012 = sum(bankdistrict) if year_n <2012
bysort district : egen bankdistrict2013 = sum(bankdistrict) if year_n <2013
keep district bankdistrict1998-bankdistrict2013
duplicates drop
duplicates drop district, force
reshape long bankdistrict, i(district) j(year_n)

replace district = "BUNDI" if district == "BUNDI"
replace district = "DADRA & NAGAR HAVELI" if district == "DADRA&NAGAR HAVELI"
replace district = "JANJGIR - CHAMPA" if district == "JANJGIR-CHAMPA"
replace district = "RATLAM" if district == "RATLAM"
replace district = "SINDHUDURG" if district == "SINDHUDURG"
replace district = "PURBI SINGBHUM" if district == "PURBI SINGHBHUM"
replace district = "TIRUCHIRAPPALLI" if district == "TIRUCHIRAPALLI"
replace district = "VISHAKHAPATNAM" if district == "VISAKHAPATNAM"
replace district = "PUDUKOTTAI" if district == "PUDUKKOTTAI"
replace district = "TIRUNELVELI" if district == "TIRUNELVALI"
replace district = "UTTAR KANNADA" if district == "UTTAR KANNAD"
replace district = "GANDHI NAGAR" if district == "GANDHINAGAR"
replace district = "BANASKANTHA" if district == "BANAS KANTHA"
replace district = "BARABANKI" if district == "BARA BANKI"
replace district = "DEHRADUN" if district == "DEHRA DUN"
replace district = "SABARKANTHA" if district == "SABAR KANTHA"
replace district = "AHMEDNAGAR" if district == "AHMADNAGAR"
replace district = "BARDHAMAN" if district == "BARDDHAMAN"
replace district = "HAZARIBAGH" if district == "HAZARIBAG"
replace district = "MALAPURAM" if district == "MALAPPURAM"
replace district = "ALAPPUZHA" if district == "ALAPUZHA"
replace district = "FEROZEPUR" if district == "FEROZPUR"
replace district = "MANDASUR" if district == "MANDSAUR"
replace district = "SHOLAPUR" if district == "SOLAPUR"
replace district = "DHARWAR" if district == "DHARWAD"
replace district = "KUCHCHH" if district == "KACHCHH"
replace district = "PALAMU" if district == "PALAMAU"
replace district = "SIRMOUR" if district == "SIRMAUR"
replace district = "SONEPAT" if district == "SONIPAT"
replace district = "BIJNORE" if district == "BIJNOR"
replace district = "CHIKAMAGLUR" if district == "CHIKMAGALUR"
replace district = "BADAUN" if district == "BUDAUN"
replace district = "HISSAR" if district == "HISAR"
replace district = "MAHBOOBNAGAR" if district == "MAHBUBNAGAR"
replace district = "NASHIK" if district == "NASIK"
replace district = "SHIMLA" if district == "SIMLA"
replace district = "CHITTORGARH" if district == "CHITTAURGARH"
replace district = "IDUKI" if district == "IDUKKI"
replace district = "KANPUR" if district == "KANPUR NAGAR"
replace district = "DARJEELING" if district == "DARJILING"
replace district = "KULLU" if district == "KULU"
replace district = "VILUPPURAM" if district == "VILLUPURAM"
replace district = "KASARGODE" if district == "KASARAGOD"
replace district = "BANGALORE" if district == "BANGALORE RURAL"
replace district = "MEDINIPUR" if district == "PASCHIM MEDINIPUR"
replace district = "BHATINDA" if district == "BATHINDA"
replace district = "MIDNAPUR" if district == "MIRZAPUR"
replace district = "PONDICHERRY" if district == "PUDUCHERRY"
replace district = "THE NILGIRI" if district == "NILGIRIS"
replace district = "CHACHER" if district == "CACHAR"
replace district = "TIRUVALLUR" if district == "THIRUVARUR"
replace district = "KENDUJHAR" if district == "KEONJHAR"
replace district = "KODURU" if district == "KODAGU"
replace district = "MOHINDERGARH" if district == "MAHENDRAGARH"
replace district = "WEST DINAJPUR" if district == "DIMAPUR"
replace district = "A&N ISLANDS" if district == "NORTH AND MIDDLE ANDAMAN"
replace district = "A&N ISLANDS" if district == "SOUTH ANDAMAN"
replace district = "BEED" if district == "BID"
replace district = "CALCUTTA" if district == "KOLKATA"
replace district = "DAK. KANNADA" if district == "DAKSHIN KANNAD"
replace district = "HOOGHLY" if district == "KOLKATA"
replace district = "HOWRAH" if district == "KOLKATA"
replace district = "PANCHMAHAL" if district == "PANCH MAHALS"
replace district = "RAIBAREILLY" if district == "RAI BARELI"
replace district = "RANGA REDDY" if district == "RANGAREDDI"
replace district = "SWAIMADHOPUR" if district == "SAWAI MADHOPUR"
replace district = "TRIVANDRUM" if district == "THIRUVANANTHAPURAM"
replace district = "YEOTAMAL" if district == "YAVATMAL"
collapse (sum) bankdistrict, by(district year_n)
save "...\dta files\Branchdata.dta", replace


cd "...\Data"
insheet using interestincidence.csv,c names clear
sort companyname year_n
egen companyid = group(companyname)
xtset companyid year_n, yearly
gen dinterestincidence = interestincidence - l.interestincidence
gen interestup = dinterestincidence > 0
gen interestdown = dinterestincidence < 0
gen interestch = interestup == 1
replace interestch = -1 if interestdown == 1
save "...\dta files\interestincidence.dta",replace


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
gen tangibility = netfixedassets / totalassets
foreach x of varlist shorttermbankborrowings longtermbankborrowings securedbankborrowings unsecuredbankborrowings {
replace `x' = 0 if `x' ==.
}
gen leverage = debt/totalassets 
replace totalinterestexpenses = interestpaid if totalinterestexpenses ==.
gen coverage = log(1+(pbdita /totalinterestexpenses))
gen currentratio = currentassets/ currentliabilities


gen listed = isincode != ""
merge m:1 registeredofficepincode using "...\dta files\pincodedata.dta"
drop if _m ==2 
drop _m
replace district = strupper(district)
merge m:1 district year_n using "...\dta files\Branchdata.dta"
replace bankdistrict = 0 if bankdistrict ==.
drop if _m==2


keep companycode companyname bankername year_n district bankdistrict is_bank public private foreign coop pfi is_rel totalassets profitaftertax ownershipgroup industrygroup shorttermbankborrowings longtermbankborrowings securedbankborrowings unsecuredbankborrowings ln_assets ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio cashratio is_group is_pub listed scb
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


merge m:1 companyname year_n using interestincidence
keep if _m==3
drop _m
drop if companycode == .
sort companyname year_n

keep if is_bank==1
bysort companyname year_n: gen count=_N
gen single = count==1

keep if year >=1998
save "...\dta files\relation.dta",replace


cd "...\dta files"
use relation,clear
merge m:1 year_n using policyrates
drop _m
bysort companyname: egen avgsize = mean(ln_assets)
egen small = xtile (avgsize), n(2)
replace small = small == 1
bysort companyname: egen avgcash = mean(cashratio)
egen cashrich = xtile (avgcash), n(2)
replace cashrich = cashrich - 1
bysort companyname: egen avgtang = mean(tangibility)
egen hightang = xtile (avgtang), n(2)
replace hightang = hightang - 1
save relation,replace


keep if count==1
save relationsingle,replace

use relation,clear
merge m:1 year_n using policyrates
drop _m




/*********************************************************************************************************************************************************************************************************/


/*********************************************************************************************************************************************************************************************************/
save relationtemp,replace

use relationtemp,clear

collapse public private foreign coop pfi is_rel, by (companyid companyname industrygroup indcode year_n district bankdistrict tight tight2 lag_tight2 small is_group is_pub listed cashrich hightang ln_assets ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio mci crr crrch crrup crrdown lag_crr revrepo revrepoch revrepoup revrepodown lag_revrepo single bankborrow bankborrow2 shbankborrow lobankborrow secbankborrow unsecbankborrow finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow tradecredit borrowingfrombanks borrowings borrowings2)
egen bankpublic = xtile(public), n(2)
replace bankpublic = bankpublic - 1
replace bankdistrict = log(1+bankdistrict)
sort companyname year_n
bysort companyname: gen bankborrowch = bankborrow - bankborrow[_n-1] > 0

/*FOR SINGLE2YEAR, GENERATE A DUMMY FOR FIRST YEAR ENTRY*/

bysort companyname: gen single2 = 1 if (single==1 & single[_n-1]==1)
replace single2 = 0 if single2 ==.
bysort companyname: gen drop1styear = 1 if _n==1

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

/*CLEANING DATA*/
winsor2 single single2 bankborrow2 nonbankborrow2 log_b log_bb log_nbb ln_assets ebit_assets leverage currentratio tobin, replace cuts(1 99)

/*LABELLING THE DATA*/
label variable single "Exclusive relationship dummy"
label variable single2 "Exclusive relationship dummy (2 yrs)"
label variable bankborrow2 "Bank borrowing"
label variable nonbankborrow2 "Non-bank borrowing"
label variable finborrow "Financial institutions borrowing"
label variable govborrow "Government borrowing"
label variable cmborrow "Capital market borrowing"
label variable icborrow "Inter-corporate borrowing"
label variable forborrow "Foreign borrowing"
label variable prborrow "Promoter borrowing"
label variable othborrow "Other borrowing"
label variable cpborrow "CP borrowing"
label variable shbankborrow "Short-term bank borrowing"
label variable lobankborrow "Long-term bank borrowing"
label variable secbankborrow "Secured bank borrowing"
label variable unsecbankborrow "Unsecured bank borrowing"
label variable log_b "Log(Total borrowing amount)"
label variable log_bb "Log(Bank borrowing amount)"
label variable log_nbb "Log(Non-bank borrowing amount)"
label variable ln_assets "Size"
label variable ebit_assets "Profitability"
label variable leverage "Leverage"
label variable currentratio "Current Ratio"
label variable tobin "Tobin's Q"

save relation,replace

/**** REGRESSIONS ****/

use relation,clear

xi i.year_n
est clear
gen int1 = single*crr
gen int2 = single*revrepo

areg log_b crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a1
areg log_bb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a2
areg log_nbb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a3

areg log_b revrepo int2 single _I* if year_n >=2001, absorb(companyname) cluster(companyname)
est store a4
areg log_bb revrepo int2 single _I* if year_n >=2001 , absorb(companyname) cluster(companyname)
est store a5
areg log_nbb revrepo int2 single _I* if year_n >=2001 , absorb(companyname) cluster(companyname)
est store a6
outreg2 [a*] using "Table1", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table1.txt"

use relation,clear
xi i.year_n
est clear
gen int1 = single * crr
gen int2 = single * revrepo
areg bankborrow2 crr single int1 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
areg bankborrow2 revrepo single int2 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2
label variable int1 "singlebank_firm*crr"
label variable int2 "singlebank_firm*revrepo"
outreg2 [a*] using "Table2", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table2.txt"



/* Loosening and tightening */
use relation,clear
xi i.year_n
est clear
gen int1 = single * crr
gen int2 = single * revrepo

gen int11 = single * tight2
gen int111 = crr * tight2
gen int1111 = single * crr * tight2

gen int22 = single * tight2
gen int222 = revrepo * tight2
gen int2222 = single * revrepo * tight2

areg bankborrow2 crr single int1 ln_assets ebit_assets leverage currentratio tobin _I* if crrup==0, absorb(companyname) cluster(companyname)
est store a1
areg bankborrow2 crr single int1 ln_assets ebit_assets leverage currentratio tobin _I* if crrup == 1, absorb(companyname) cluster(companyname)
est store a2
areg bankborrow2 revrepo single int2 ln_assets ebit_assets leverage currentratio tobin _I* if revrepoup == 0, absorb(companyname) cluster(companyname)
est store a3
areg bankborrow2 revrepo single int2 ln_assets ebit_assets leverage currentratio tobin _I* if revrepoup == 1, absorb(companyname) cluster(companyname)
est store a4
label variable int1 "singlebank_firm*crr"
label variable int2 "singlebank_firm*revrepo"
outreg2 [a*] using "Table3", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table3.txt"



use relation,clear
xi i.year_n
est clear
gen int1 = single * crr
gen int2 = single * revrepo

gen int11 = single * tight
gen int111 = crr * tight
gen int1111 = single * crr * tight

gen int22 = single * tight
gen int222 = revrepo * tight
gen int2222 = single * revrepo * tight

est clear
areg bankborrow2 crr single tight int1 int11 int111 int1111 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
areg bankborrow2 revrepo single tight int2 int22 int222 int2222 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2

label variable int1111 "singlebank_firm*crr*tight_monetary_condn"
label variable int2222 "singlebank_firm*revrepo*tight_monetary_condn"
outreg2 [a*] using "Table4", label drop(_I* ) sortvar(int1111 int2222 int3333)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table4.txt"




/* Robustness: Using Lagged Rates */
use relation,clear
xi i.year_n
est clear
gen int1 = single * lag_crr
gen int2 = single * lag_revrepo

areg bankborrow2 lag_crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
areg bankborrow2 lag_revrepo single int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2
label variable int1 "singlebank_firm*lag_crr"
label variable int2 "singlebank_firm*lag_revrepo"
outreg2 [a*] using "Table5(Lagged Rates)", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table5(Lagged Rates).txt"

/* Robustness: with overall rate index MCI */

use relation,clear
xi i.year_n
est clear

gen int1 = single * mci
areg bankborrow2 mci single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
label variable int1 "singlebank_firm*mci"

outreg2 [a*] using "Table6(MCI)", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table6(MCI).txt"


/* Robustness: Other values of single bank */

/* Single bank this year and last year */
use relation,clear
xi i.year_n
est clear

bysort companyname: gen single2 = 1 if (single==1 & single[_n-1]==1)
replace single2 = 0 if single2 ==.
bysort companyname: gen drop1styear = 1 if _n==1

gen int1 = single2 * crr
gen int2 = single2 * revrepo
est clear

areg bankborrow2 crr single2 int1 ln_assets ebit_assets leverage currentratio tobin _I* if drop1styear !=1, absorb(companyname) cluster(companyname)
est store a1
areg bankborrow2 revrepo single2 int2 ln_assets ebit_assets leverage currentratio tobin _I* if drop1styear !=1, absorb(companyname) cluster(companyname)
est store a2
label variable int1 "singlebank_firm2year*crr"
label variable int2 "singlebank_firm2year*revrepo"

outreg2 [a*] using "Table7a(Single2yr)", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table7a(Single2yr).txt"



/* Additional: short/long secured/unsecured term bank borrowings */

use relation,clear
xi i.year_n
est clear
gen int1 = single * crr
gen int2 = single * revrepo

areg shbankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
areg lobankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2
areg secbankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a3
areg unsecbankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a4

areg shbankborrow revrepo single int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a5
areg lobankborrow revrepo single int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a6
areg secbankborrow revrepo single int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a7
areg unsecbankborrow revrepo single int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a8

label variable int1 "singlebank_firm*crr"
label variable int2 "singlebank_firm*revrepo"
outreg2 [a*] using "Table8(Breakup)", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table8(Breakup).txt"



use relation,clear

xi i.year_n*i.indcode
est clear
gen int1 = single*crr
gen int2 = single*revrepo

areg bankborrow2 crr int1 single ln_assets ebit_assets log_b currentratio tobin _I* , absorb(companyname) cluster(companyname)
est store a1
areg log_b crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a2
areg log_bb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a3

areg bankborrow2 revrepo int2 single ln_assets ebit_assets log_b currentratio tobin _I* , absorb(companyname) cluster(companyname)
est store a4
areg log_b revrepo int2 single _I* , absorb(companyname) cluster(companyname)
est store a5
areg log_bb revrepo int2 single _I* , absorb(companyname) cluster(companyname)
est store a6
outreg2 [a*] using "Table9(Ind*YearFE)", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table9(Ind*YearFE).txt"


/**** ALL PUBLIC BANKS NON STATE BANK GROUP ****/

use relationtemp,clear
bysort companyname year_n: gen totalbanks=_N
bysort companyname year_n: egen totalpublic=sum(public)
keep if totalpublic==totalbanks

drop if bankername == "state bank of bikaner & jaipur"
drop if bankername == "state bank of hyderabad"
drop if bankername == "state bank of india"
drop if bankername == "state bank of mysore"
drop if bankername == "state bank of patiala"
drop if bankername == "state bank of travancore"


collapse public private foreign coop pfi is_rel, by (companyid companyname industrygroup indcode year_n district bankdistrict tight tight2 lag_tight2 small is_group is_pub listed cashrich hightang ln_assets ebit_assets leverage currentratio tobin tangibility leverage coverage currentratio mci crr crrch crrup crrdown lag_crr revrepo revrepoch revrepoup revrepodown lag_revrepo single bankborrow bankborrow2 shbankborrow lobankborrow secbankborrow unsecbankborrow finborrow govborrow cmborrow icborrow forborrow prborrow othborrow cpborrow tradecredit borrowingfrombanks borrowings borrowings2)
egen bankpublic = xtile(public), n(2)
replace bankpublic = bankpublic - 1
replace bankdistrict = log(1+bankdistrict)
sort companyname year_n
bysort companyname: gen bankborrowch = bankborrow - bankborrow[_n-1] > 0
save relationtemp2,replace



use relationtemp2,clear

xi i.year_n
est clear
gen int1 = single*crr
gen int2 = single*revrepo

areg log_b crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a1
areg log_bb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a2
areg log_nbb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a3

areg log_b revrepo int2 single ln_assets ebit_assets log_b currentratio tobin _I* , absorb(companyname) cluster(companyname)
est store a4
areg log_bb revrepo int2 single _I* , absorb(companyname) cluster(companyname)
est store a5
areg log_nbb revrepo int2 single _I* , absorb(companyname) cluster(companyname)
est store a6

areg bankborrow2 crr single int1 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a7
areg bankborrow2 revrepo single int2 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a8
label variable int1 "singlebank_firm*crr"
label variable int2 "singlebank_firm*revrepo"

outreg2 [a*] using "Table10(Publicbanks)", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table10(Publicbanks).txt"


/****ALL FOREIGN BANKS ONLY (CHANGE CODE TO KEEP ONLY FOREIGN BANKS)****/

use relation,clear

xi i.year_n
est clear
gen int1 = single*crr
gen int2 = single*revrepo
label variable int1 "singlebank_firm*crr"
label variable int2 "singlebank_firm*revrepo"

areg log_b crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a1
areg log_bb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a2
areg log_nbb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a3
areg log_b revrepo int2 single _I* , absorb(companyname) cluster(companyname)
est store a4
areg log_bb revrepo int2 single _I* , absorb(companyname) cluster(companyname)
est store a5
areg log_nbb revrepo int2 single _I* , absorb(companyname) cluster(companyname)
est store a6

areg bankborrow2 crr single int1 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a7
areg bankborrow2 revrepo single int2 ln_assets ebit_assets log_b currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a8
label variable int1 "singlebank_firm*crr"
label variable int2 "singlebank_firm*revrepo"

outreg2 [a*] using "Table11 (Foreign)", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table11 (Foreign).txt"




*CODE FINISHED



/*IV using ruling party dummy NOT SUCCESSFUL*/

use relation,clear
keep if year <=2009
keep if year >=1999
merge m:1 district year_n using rulingparty
keep if _m==3
drop _m
sort companyname year_n

xi i.year_n
est clear
gen int1 = single*crr
gen int2 = single*revrepo
gen log_b = log(1+borrowings2)
label variable log_b "Log_Borrowings"
gen log_bb = log(1+borrowingfrombanks)
label variable log_bb "Log_BankBorrowings"
gen log_nbb = log(1+(borrowings2-borrowingfrombanks))
label variable log_nbb "Log_Non-BankBorrowings"

drop if borrowings >=.
xtset companyid year_n,yearly

gen ivint1 = ruling_party_dummy * crr
gen ivint2 = ruling_party_dummy * revrepo

xtivreg2 log_b crr (single int1 = ruling_party_dummy ivint1), fe cluster(companyid) first small
xtivreg2 log_bb crr (single int1 = ruling_party_dummy ivint1), fe cluster(companyid) first small
xtivreg2 log_nbb crr (single int1 = ruling_party_dummy ivint1), fe cluster(companyid) first small
xtivreg2 bankborrow2 crr (single int1 = ruling_party_dummy ivint1), fe cluster(companyid) first small









areg log_b crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a1
areg log_bb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a2
areg log_nbb crr int1 single _I* , absorb(companyname) cluster(companyname)
est store a3
outreg2 [a*] using "Table1", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "Table1.txt"




/* IV Regression */

use relation,clear
xi i.year_n
est clear
gen int1 = single * crr
gen int2 = single * revrepo
gen log_b = log(1+borrowings2)
label variable log_b "Log_Borrowings"






use relation,clear
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = single * crr
areg bankborrow crr single int1 ln_assets ebit_assets log_b currentratio tobin _I* if lag_tight2 == 0, absorb(companyname) cluster(companyname)
est store a1
areg bankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I* if lag_tight2 == 1, absorb(companyname) cluster(companyname)
est store a2
gen int2 = is_rel * crr
areg bankborrow crr is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I* if lag_tight2 == 0, absorb(companyname) cluster(companyname)
est store a3
areg bankborrow crr is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I* if lag_tight2 == 1, absorb(companyname) cluster(companyname)
est store a4
gen int3 = is_group * crr
areg bankborrow crr is_group int3 ln_assets ebit_assets leverage currentratio tobin _I* if lag_tight2 == 0, absorb(companyname) cluster(companyname)
est store a5
areg bankborrow crr is_group int3 ln_assets ebit_assets leverage currentratio tobin _I* if lag_tight2 == 1, absorb(companyname) cluster(companyname)
est store a6

label variable int1 "singlebank_firm*crr"
label variable int2 "relationship_firm*crr"
label variable int3 "group_firm*crr"
outreg2 [a*] using "DiD2_lag", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiD2_lag.txt"


/* Additional: short/long secured/unsecured term bank borrowings */

use relation,clear
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = single * crr
areg shbankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
areg lobankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2
areg secbankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a3
areg unsecbankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a4
gen int2 = is_rel * crr
areg shbankborrow crr is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a5
areg lobankborrow crr is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a6
areg secbankborrow crr is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a7
areg unsecbankborrow crr is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a8
gen int3 = is_group * crr
areg shbankborrow crr is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a9
areg lobankborrow crr is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a10
areg secbankborrow crr is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a11
areg unsecbankborrow crr is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a12

label variable int1 "singlebank_firm*crr"
label variable int2 "relationship_firm*crr"
label variable int3 "group_firm*crr"
outreg2 [a*] using "DiDadd", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDadd.txt"

use relation,clear
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = single * revrepo
areg shbankborrow revrepo single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
areg lobankborrow revrepo single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2
areg secbankborrow revrepo single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a3
areg unsecbankborrow revrepo single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a4
gen int2 = is_rel * revrepo
areg shbankborrow revrepo is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a5
areg lobankborrow revrepo is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a6
areg secbankborrow revrepo is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a7
areg unsecbankborrow revrepo is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a8
gen int3 = is_group * revrepo
areg shbankborrow revrepo is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a9
areg lobankborrow revrepo is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a10
areg secbankborrow revrepo is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a11
areg unsecbankborrow revrepo is_group int3 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a12

label variable int1 "singlebank_firm*revrepo"
label variable int2 "relationship_firm*revrepo"
label variable int3 "group_firm*revrepo"
outreg2 [a*] using "DiDrateadd", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDrateadd.txt"


/* Robustness: With listed firms */ /**************************** Subsample shows up but triple diff does not ************************************/
use relation,clear
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = is_pub * crr
areg bankborrow crr is_pub int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
gen int2 = listed * crr
areg bankborrow crr listed int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2

areg bankborrow crr ln_assets ebit_assets leverage currentratio tobin _I* if listed == 0, absorb(companyname) cluster(companyname)
est store a3
areg bankborrow crr ln_assets ebit_assets leverage currentratio tobin _I* if listed == 1, absorb(companyname) cluster(companyname)
est store a4

label variable int1 "publicfirm*crr"
label variable int2 "listedfirm*crr"
outreg2 [a*] using "DiDadd2", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDadd2.txt"

use relation,clear
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = is_pub * revrepo
areg bankborrow revrepo is_pub int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
gen int2 = listed * revrepo
areg bankborrow revrepo listed int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2

areg bankborrow revrepo ln_assets ebit_assets leverage currentratio tobin _I* if listed == 0, absorb(companyname) cluster(companyname)
est store a3
areg bankborrow revrepo ln_assets ebit_assets leverage currentratio tobin _I* if listed == 1, absorb(companyname) cluster(companyname)
est store a4

label variable int1 "publicfirm*revrepo"
label variable int2 "listedfirm*revrepo"
outreg2 [a*] using "DiDrateadd2", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDrateadd2.txt"

/* IV Regression */

use relation,clear
egen indyear = group(indcode year_n)
drop if companyid ==.
drop if district == ""
xtset companyid year_n
xi i.year_n i.bankpublic
est clear
gen post = year_n > 2008
gen int1 = single * crr
gen int2 = bankdistrict * crr
xtivreg2 bankborrow crr ln_assets ebit_assets leverage currentratio tobin _I* (single int1 = bankdistrict int2 ln_assets ebit_assets tobin),cluster(industrygroup) fe first

esttab using myfile.xls, replace /* 
*/ sca("widstat Weak Ident." "sargan Sargan")
!myfile.xls


use relation,clear
egen indyear = group(indcode year_n)
drop if companyid ==.
xtset companyid year_n
xi i.year_n i.bankpublic
est clear
gen post = year_n > 2008
gen int1 = single * revrepo
gen int2 = bankdistrict * revrepo
xtivreg2 bankborrow revrepo ln_assets ebit_assets leverage currentratio tobin _I* (single int1 = bankdistrict int2 ln_assets ebit_assets tobin),cluster(industrygroup) fe first

esttab using myfile.xls, replace /* 
*/ sca("widstat Weak Ident." "sargan Sargan")
!myfile.xls








use relation,clear
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
gen post = year_n > 2008
est clear
areg single post ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
predict single1, xb
gen int11 = single1 * crr
areg bankborrow single1 crr int11 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)


/* Robustness: No arm twisting for listed 'single' firms SAME RESULTS HOLD :( */

use relation,clear
keep if listed == 1
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = single * crr
areg bankborrow crr single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
gen int2 = is_rel * crr
areg bankborrow crr is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2
gen int3 = is_group * crr
areg bankborrow crr is_group int3 ln_assets ebit_assets leverage currentratio tobin _I* if is_group != -1, absorb(companyname) cluster(companyname)
est store a3

label variable int1 "singlebank_firm*crr"
label variable int2 "relationship_firm*crr"
label variable int3 "group_firm*crr"
outreg2 [a*] using "DiDlisted", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDlisted.txt"

use relation,clear
keep if listed == 1
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = single * revrepo
areg bankborrow revrepo single int1 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
gen int2 = is_rel * revrepo
areg bankborrow revrepo is_rel int2 ln_assets ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a2
gen int3 = is_group * revrepo
areg bankborrow revrepo is_group int3 ln_assets ebit_assets leverage currentratio tobin _I* if is_group != -1, absorb(companyname) cluster(companyname)
est store a3

label variable int1 "singlebank_firm*revrepo"
label variable int2 "relationship_firm*revrepo"
label variable int3 "group_firm*revrepo"
outreg2 [a*] using "DiDratelisted", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDratelisted.txt"




/* Robustness: Informationally opaque borrowers face even lesser sensitivity to Monetary policy */

/* Small Firms */

use relation,clear
egen indyear = group(indcode year_n)

xi i.year_n i.bankpublic
est clear
gen int1 = small * crr
areg bankborrow crr small int1  ebit_assets leverage currentratio tobin _I*, absorb(companyname) cluster(companyname)
est store a1
gen int2 = cashrich * crr
areg bankborrow crr cashrich int2 ln_assets ebit_assets leverage  tobin _I*, absorb(companyname) cluster(companyname)
est store a2
gen int3 = hightang * crr
areg bankborrow crr hightang int3 ln_assets ebit_assets leverage currentratio tobin _I* if is_group != -1, absorb(companyname) cluster(companyname)
est store a3

label variable int1 "small*crr"
label variable int2 "cashrich*crr"
label variable int3 "hightang*crr"
outreg2 [a*] using "DiDotherdep", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDotherdep.txt"

/* Small firms with single and multiple banks */

use relation,clear
egen indyear = group(indcode year_n)
xi i.year_n i.bankpublic
est clear
gen int1 = single * crr
areg bankborrow crr single int1 ln_assets ebit_assets leverage currentratio  _I* if small == 1, absorb(companyname) cluster(companyname)
est store a1
areg bankborrow crr single int1 ln_assets ebit_assets leverage currentratio  _I* if small == 0, absorb(companyname) cluster(companyname)
est store a2
areg bankborrow crr single int1 ln_assets ebit_assets leverage currentratio  _I* if listed == 1, absorb(companyname) cluster(companyname)
est store a3
areg bankborrow crr single int1 ln_assets ebit_assets leverage currentratio  _I* if listed == 0, absorb(companyname) cluster(companyname)
est store a4
label variable int1 "single*crr"

outreg2 [a*] using "DiDsmallandbig", label drop(_I* ) sortvar(int1 int2 int3)   addstat(Adj R-squared,e(r2_a)) nocons dec(3) excel replace  tstat addtext (Bank FE, Yes, Year FE, Yes, Firm FE, Yes,  Clustering, Firm-Level) 
erase "DiDsmallandbig.txt"


/* Robustness: Above a borrower quality, the channel will start becoming significant */

/* Robustness: For bad quality borrower, relationship loans are supply side phenomena */

/* Robustness: With average value of annual CRR and Rev Repo */
