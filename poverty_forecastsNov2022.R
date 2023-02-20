required.packages <- c("WDI","data.table", "readxl","varhandle")
lapply(required.packages, require, character.only=T)

setwd("D:/git/poverty_predictions")
load("project_data/pip17Sept2022PIPallslim.RData")
recent_years=subset(smy_results3, year==2019)
load("output/post2019pipscrapeOct2022.RData")
##Reading in table B3B from "Mahler, Daniel Gerszon; Yonzan, Nishant; Lakner, Christoph. 2022. The Impact of COVID-19 on Global Inequality and Poverty. Policy Research Working Papers;10198. World Bank, Washington, DC. © World Bank. https://openknowledge.worldbank.org/handle/10986/38114  
##This table incorporates the World Banks' 2020 estimates of extreme poverty based on their model which primarily draws on non economic pulse surveys conducted via phone in 2020
nowcast2020=fread("Mahleretal2022TableB3B.csv") 
nowcast2020=subset(nowcast2020, Mahler_headcount>.001)

wb_un.regions <- fread("project_data/WB_UN regions.csv")
names(wb_un.regions)[names(wb_un.regions) == "Povcal_Region"] <- "region"

wb_un.regions[ISO3 == "KSV"]$ISO3 <- "XKX"
wb_un.regions[ISO3 == "WBG"]$ISO3 <- "PSE"
# WEoraw=fread("https://www.imf.org/imf/weodatabase/downloadreport?c=512,914,612,614,311,213,911,314,193,122,912,313,419,513,316,913,124,339,638,514,218,963,616,223,516,918,748,618,624,522,622,156,626,628,228,924,233,6365,636,634,238,662,960,423,935,128,611,3651,243,248,469,253,642,643,939,734,644,819,172,1365,646,648,915,134,652,174,3658,258,656,654,336,263,268,5365,944,176,534,536,429,433,178,436,136,343,158,439,916,664,826,542,967,443,917,544,941,446,666,668,672,946,137,546,674,676,548,6856,678,181,867,682,684,273,868,921,948,943,686,688,518,728,836,6858,138,196,278,692,694,962,142,449,564,565,283,853,288,293,566,964,182,359,453,968,922,714,862,135,716,456,722,942,718,724,576,936,961,813,726,199,733,184,524,361,362,364,7365,366,144,146,463,528,923,738,578,537,742,866,369,744,186,925,869,746,926,466,112,111,298,927,846,299,582,487,474,754,698,&s=NGDP_RPCH,&sy=2018&ey=2025&ssm=0&scsm=1&scc=1&ssd=1&ssc=0&sic=0&sort=country&ds=.&br=1&wsid=114f286b-e3fc-476b-8736-3f2cb3016edb")
WEOraw=fread("project_data/WEOOct2022all.csv")
WEO <- WEOraw[`WEO Subject Code` %in% c("NGDPRPPPPCPCH", "NGDPRPPPPC")]

year.cols <- as.character(seq(1980,2027))
year.sd.cols <- tail(year.cols, -1)
WEO[, (year.cols) := lapply(.SD, function(x) gsub(",", "", x)), .SDcols=(year.cols)]
WEO[`WEO Subject Code` == "NGDPRPPPPC", (year.sd.cols) := as.data.table(t(apply(.SD, 1, function(x) as.character(diff(as.numeric(x))/as.numeric(x))))), .SDcols=(year.cols), by=Country]

WEO[WEO=="--"] <- 0

{WEO$Country[which(WEO$Country=="Democratic Republic of the Congo")]="Congo, Dem. Rep."
  WEO$Country[which(WEO$Country=="Republic of Congo")]="Congo, Rep."
  WEO$Country[which(WEO$Country=="Côte d'Ivoire")]="Cote d'Ivoire"
  WEO$Country[which(WEO$Country=="Egypt")]="Egypt, Arab Rep."
  WEO$Country[which(WEO$Country=="The Gambia")]="Gambia, The"
  WEO$Country[which(WEO$Country=="Islamic Republic of Iran")]="Iran, Islamic Rep."
  WEO$Country[which(WEO$Country=="Russia")]="Russian Federation"
  WEO$Country[which(WEO$Country=="Syria")]="Syrian Arab Republic"
  WEO$Country[which(WEO$Country=="Venezuela")]="Venezuela, RB"
  WEO$Country[which(WEO$Country=="Yemen")]="Yemen, Rep."
  WEO$Country[which(WEO$Country=="Lao P.D.R.")]="Lao PDR"
  WEO$Country[which(WEO$Country=="FYR Macedonia")]="Macedonia, former Yugoslav Republic of"
  WEO$Country[which(WEO$Country=="Micronesia")]="Micronesia, Fed. Sts."
  WEO$Country[which(WEO$Country=="Taiwan Province of China")]="Taiwan, China"
  WEO$Country[which(WEO$Country=="São Tomé and Príncipe")]="Sao Tome and Principe"
  WEO$Country[which(WEO$Country=="Turkey")]="Turkiye"
  WEO$Country[which(WEO$Country=="Türkiye")]="Turkiye"
  WEO$Country[which(WEO$Country=="Korea")]="Korea, Rep."}

names(WEO)[which(names(WEO)=="Country")]<- "country_name"
WEO$country_name=as.character(WEO$country_name)
recent_years$country_name=as.character(recent_years$country_name)

WEO$WEO2019=as.numeric(WEO$`2019`)
WEO$WEO2020=as.numeric(WEO$`2020`)
WEO$WEO2021=as.numeric(WEO$`2021`)
WEO$WEO2022=as.numeric(WEO$`2022`)
WEO$WEO2023=as.numeric(WEO$`2023`)
WEO$WEO2024=as.numeric(WEO$`2024`)
WEO$WEO2025=as.numeric(WEO$`2025`)
WEO$WEO2026=as.numeric(WEO$`2026`)
WEO$WEO2027=as.numeric(WEO$`2027`)



WEO3=WEO[,c("country_name",
          "WEO2019",
          "WEO2020",
          "WEO2021",
          "WEO2022",
          "WEO2023",
          "WEO2024",
          "WEO2025",
          "WEO2026",
          "WEO2027")]
consumption=subset(recent_years, welfare_type==c("consumption"))
consumption=unique(consumption$country_name)
income=subset(recent_years, welfare_type==c("income"))
income=unique(income$country_name)


WEO2=subset(WEO, country_name %in% consumption)
WEO3=subset(WEO, country_name %in% income)
##The World Bank Poverty and Shared Prosperity 2022 reports that only 70% of consumption growth "passes through" to households while 100% of income growth passes to households
WEO2$WEO2019=WEO2$WEO2019*.7
WEO2$WEO2020=WEO2$WEO2020*.7
WEO2$WEO2021=WEO2$WEO2021*.7
WEO2$WEO2022=WEO2$WEO2022*.7
WEO2$WEO2023=WEO2$WEO2023*.7
WEO2$WEO2024=WEO2$WEO2024*.7
WEO2$WEO2025=WEO2$WEO2025*.7
WEO2$WEO2026=WEO2$WEO2026*.7
WEO2$WEO2027=WEO2$WEO2027*.7

WEO=rbind(WEO2, WEO3)

#THe Following Countries have data in the PIP that goes past the last "lined up" year
post2019countries=unique(smy_resultspost2019$country_name)
recent_yearspost=subset(recent_years, country_name %in% post2019countries)
recent_years$postdata=0
recent_years$postdata[which(recent_years$country_name %in% post2019countries)]=1
recent_years=subset(recent_years, postdata=0)
recent_years=recent_years[,c("country_code" ,"year"        
                             , "region_code" , "country_name"
                             , "poverty_line", "headcount"   
                             , "pop"        ,  "welfare_type"
                              )]
smy_resultspost2019$year=2020
smy_resultspost2019$year[which(smy_resultspost2019$country_name=="Indonesia")]=2021
recent_years2=rbind(recent_years,smy_resultspost2019)
recent_years3=merge(recent_years2,recent_yearspost, by=c("country_name","poverty_line"))
recent_years4=merge(recent_years2,nowcast2020, by=c("country_name"))
recent_years4$nowcastdiff=abs(recent_years4$headcount-recent_years4$Mahler_headcount)
nowcastgrowth=data.table(recent_years4)[,.SD[which.min(nowcastdiff)],by=.(country_name)]
#Because the World Bank doesn't report the growth rates estimated or the poverty headcounts estimated for their list of "nowcasted" countries, we assume that the growth rate for those at the international poverty line are representative of the rest of the distribution. This may be a strong assumption but is the best possible without further data from Mahler et al. (2022).
nowcastgrowth$nowcastgrowthrt=(2.15-nowcastgrowth$poverty_line)/2.15
WEO4=merge(WEO, nowcastgrowth, by="country_name", all=T)
#The growth rates are not estimated based on Mahler data when Mahler et al. (2022) reports no extreme poverty.
WEO4$WEO2020[which(WEO4$Mahler_headcount>=.0001)]=WEO4$nowcastgrowthrt[which(WEO4$Mahler_headcount>=.0001)]
WEO4=WEO4[,c("country_name","WEO2020","WEO2021","WEO2022","WEO2023","WEO2024","WEO2025","WEO2026","WEO2027")]

dat <- merge(recent_years, WEO4, by="country_name", all=T)
#GDP growth data from Syria not available in WEO but are available for 2019-2022 in the World Bank's June 2022 Global Economic Prospects with average growth moved forward for the years that would be covered in WEO
# dat$WEO2019[which(dat$country_name=="Syrian Arab Republic")]=.037*.7
dat$WEO2020[which(dat$country_name=="Syrian Arab Republic")]=.013*.7
dat$WEO2021[which(dat$country_name=="Syrian Arab Republic")]=-.021*.7
dat$WEO2022[which(dat$country_name=="Syrian Arab Republic")]=-.026*.7
dat$WEO2023[which(dat$country_name=="Syrian Arab Republic")]=.003*.7
dat$WEO2024[which(dat$country_name=="Syrian Arab Republic")]=.003*.7
dat$WEO2025[which(dat$country_name=="Syrian Arab Republic")]=.003*.7
dat$WEO2026[which(dat$country_name=="Syrian Arab Republic")]=.003*.7
dat$WEO2027[which(dat$country_name=="Syrian Arab Republic")]=.003*.7

dat$WEO2020PL=dat$poverty_line*(1+dat$WEO2020)
#replacing projected poverty lines for actual data when available
dat$WEO2020PL[which(dat$year==2020)]=dat$poverty_line
dat$WEO2021PL=dat$WEO2020PL*(1+dat$WEO2021)
#replacing projected poverty lines for actual data when available
dat$WEO2021PL[which(dat$year==2021)]=dat$poverty_line

dat$WEO2022PL=dat$WEO2021PL*(1+dat$WEO2022)
dat$WEO2023PL=dat$WEO2022PL*(1+dat$WEO2023)
dat$WEO2024PL=dat$WEO2023PL*(1+dat$WEO2024)
dat$WEO2025PL=dat$WEO2024PL*(1+dat$WEO2025)
dat$WEO2026PL=dat$WEO2025PL*(1+dat$WEO2026)
dat$WEO2027PL=dat$WEO2026PL*(1+dat$WEO2027)
dat$WEOaveragegrowth=((dat$WEO2023+dat$WEO2024+dat$WEO2025+dat$WEO2026+dat$WEO2027)/5)
dat$WEO2028PL=dat$WEO2027PL*(1+dat$WEOaveragegrowth)
dat$WEO2029PL=dat$WEO2028PL*(1+dat$WEOaveragegrowth)
dat$WEO2030PL=dat$WEO2029PL*(1+dat$WEOaveragegrowth)



dat$pl19abs=abs(dat$poverty_line-2.15)
dat$pl20abs=abs(dat$WEO2020PL-2.15)
dat$pl21abs=abs(dat$WEO2021PL-2.15)
dat$pl22abs=abs(dat$WEO2022PL-2.15)
dat$pl23abs=abs(dat$WEO2023PL-2.15)
dat$pl24abs=abs(dat$WEO2024PL-2.15)
dat$pl25abs=abs(dat$WEO2025PL-2.15)
dat$pl26abs=abs(dat$WEO2026PL-2.15)
dat$pl27abs=abs(dat$WEO2027PL-2.15)
dat$pl28abs=abs(dat$WEO2028PL-2.15)
dat$pl29abs=abs(dat$WEO2029PL-2.15)
dat$pl30abs=abs(dat$WEO2030PL-2.15)


dat$pl19abs365=abs(dat$poverty_line-3.65)
dat$pl20abs365=abs(dat$WEO2020PL-3.65)
dat$pl21abs365=abs(dat$WEO2021PL-3.65)
dat$pl22abs365=abs(dat$WEO2022PL-3.65)
dat$pl23abs365=abs(dat$WEO2023PL-3.65)
dat$pl24abs365=abs(dat$WEO2024PL-3.65)
dat$pl25abs365=abs(dat$WEO2025PL-3.65)
dat$pl26abs365=abs(dat$WEO2026PL-3.65)
dat$pl27abs365=abs(dat$WEO2027PL-3.65)
dat$pl28abs365=abs(dat$WEO2028PL-3.65)
dat$pl29abs365=abs(dat$WEO2029PL-3.65)
dat$pl30abs365=abs(dat$WEO2030PL-3.65)



dat$pl19abs685=abs(dat$poverty_line-6.85)
dat$pl20abs685=abs(dat$WEO2020PL-6.85)
dat$pl21abs685=abs(dat$WEO2021PL-6.85)
dat$pl22abs685=abs(dat$WEO2022PL-6.85)
dat$pl23abs685=abs(dat$WEO2023PL-6.85)
dat$pl24abs685=abs(dat$WEO2024PL-6.85)
dat$pl25abs685=abs(dat$WEO2025PL-6.85)
dat$pl26abs685=abs(dat$WEO2026PL-6.85)
dat$pl27abs685=abs(dat$WEO2027PL-6.85)
dat$pl28abs685=abs(dat$WEO2028PL-6.85)
dat$pl29abs685=abs(dat$WEO2029PL-6.85)
dat$pl30abs685=abs(dat$WEO2030PL-6.85)

baseabs=data.table(dat)[,.SD[which.min(pl19abs)],by=.(country_name)]
baseabs=baseabs[,c("country_name","headcount","pop","region_code")]
baseabsw=baseabs
names(baseabs)=c("country_name","HC2019","pop2019","region_code")
base20=data.table(dat)[,.SD[which.min(pl20abs)],by=.(country_name)]
base20w=base20[,c("country_name","headcount","pop","region_code")]
names(base20w)=c("headcount","HC2020","pop2020","region_code")
base21=data.table(dat)[,.SD[which.min(pl21abs)],by=.(country_name)]
base21w=base21[,c("country_name","headcount","pop","region_code")]
names(base21w)=c("headcount","HC2021","pop2021","region_code")
base22=data.table(dat)[,.SD[which.min(pl22abs)],by=.(country_name)]
base22w=base22[,c("country_name","headcount","pop","region_code")]
names(base22w)=c("headcount","HC2022","pop2022","region_code")
base23=data.table(dat)[,.SD[which.min(pl23abs)],by=.(country_name)]
base23w=base23[,c("country_name","headcount")]
setnames(base23,"headcount","HC2023")
base24=data.table(dat)[,.SD[which.min(pl24abs)],by=.(country_name)]
base24=base24[,c("country_name","headcount")]
setnames(base24,"headcount","HC2024")
base25=data.table(dat)[,.SD[which.min(pl25abs)],by=.(country_name)]
base25=base25[,c("country_name","headcount")]
setnames(base25,"headcount","HC2025")
base26=data.table(dat)[,.SD[which.min(pl26abs)],by=.(country_name)]
base26=base26[,c("country_name","headcount")]
setnames(base26,"headcount","HC2026")
base27=data.table(dat)[,.SD[which.min(pl27abs)],by=.(country_name)]
base27=base27[,c("country_name","headcount")]
setnames(base27,"headcount","HC2027")
base30=data.table(dat)[,.SD[which.min(pl30abs)],by=.(country_name)]
base30=base30[,c("country_name","headcount")]
setnames(base30,"headcount","HC2030")

base20365=data.table(dat)[,.SD[which.min(pl20abs365)],by=.(country_name)]
base20365=base20365[,c("country_name","headcount")]
base20365$year=2020
base21365=data.table(dat)[,.SD[which.min(pl21abs365)],by=.(country_name)]
base21365=base21365[,c("country_name","headcount")]
base21365$year=2021
base21365w=rbind(base21365,base20365)
setnames(base21365,"headcount","HC2021365")
base22365=data.table(dat)[,.SD[which.min(pl22abs365)],by=.(country_name)]
base22365=base22365[,c("country_name","headcount")]
base22365$year=2022
base22365w=rbind(base22365,base21365w)
setnames(base22365,"headcount","HC2022365")
base23365=data.table(dat)[,.SD[which.min(pl23abs365)],by=.(country_name)]
base23365=base23365[,c("country_name","headcount")]
base23365$year=2023
base23365w=rbind(base23365,base22365w)
base24365=data.table(dat)[,.SD[which.min(pl24abs365)],by=.(country_name)]
base24365=base24365[,c("country_name","headcount")]
base24365$year=2024
base24365w=rbind(base24365,base23365w)
base25365=data.table(dat)[,.SD[which.min(pl25abs365)],by=.(country_name)]
base25365=base25365[,c("country_name","headcount")]
base25365$year=2025
base25365w=rbind(base25365,base24365w)
base26365=data.table(dat)[,.SD[which.min(pl26abs365)],by=.(country_name)]
base26365=base26365[,c("country_name","headcount")]
base26365$year=2026
base26365w=rbind(base26365,base25365w)
base27365=data.table(dat)[,.SD[which.min(pl27abs365)],by=.(country_name)]
base27365=base27365[,c("country_name","headcount")]
base27365$year=2027
base27365w=rbind(base27365,base26365w)
base28365=data.table(dat)[,.SD[which.min(pl28abs365)],by=.(country_name)]
base28365=base28365[,c("country_name","headcount")]
base28365$year=2028
base28365w=rbind(base28365,base27365w)
base29365=data.table(dat)[,.SD[which.min(pl29abs365)],by=.(country_name)]
base29365=base29365[,c("country_name","headcount")]
base29365$year=2029
base29365w=rbind(base29365,base28365w)
base30365=data.table(dat)[,.SD[which.min(pl30abs365)],by=.(country_name)]
base30365=base30365[,c("country_name","headcount")]
base30365$year=2030
base30365w=rbind(base30365,base29365w)
setnames(base30365,"headcount","HC2030365")


base20685=data.table(dat)[,.SD[which.min(pl20abs685)],by=.(country_name)]
base20685=base20685[,c("country_name","headcount")]
base20685$year=2020
base21685=data.table(dat)[,.SD[which.min(pl21abs685)],by=.(country_name)]
base21685=base21685[,c("country_name","headcount")]
base21685$year=2021
base21685w=rbind(base21685,base20685)
setnames(base21685,"headcount","HC2021685")
base22685=data.table(dat)[,.SD[which.min(pl22abs685)],by=.(country_name)]
base22685=base22685[,c("country_name","headcount")]
base22685$year=2022
base22685w=rbind(base22685,base21685w)
setnames(base22685,"headcount","HC2022685")
base23685=data.table(dat)[,.SD[which.min(pl23abs685)],by=.(country_name)]
base23685=base23685[,c("country_name","headcount")]
base23685$year=2023
base23685w=rbind(base23685,base22685w)
base24685=data.table(dat)[,.SD[which.min(pl24abs685)],by=.(country_name)]
base24685=base24685[,c("country_name","headcount")]
base24685$year=2024
base24685w=rbind(base24685,base23685w)
base26855=data.table(dat)[,.SD[which.min(pl25abs685)],by=.(country_name)]
base26855=base26855[,c("country_name","headcount")]
base26855$year=2025
base26855w=rbind(base26855,base24685w)
base26685=data.table(dat)[,.SD[which.min(pl26abs685)],by=.(country_name)]
base26685=base26685[,c("country_name","headcount")]
base26685$year=2026
base26685w=rbind(base26685,base26855w)
base27685=data.table(dat)[,.SD[which.min(pl27abs685)],by=.(country_name)]
base27685=base27685[,c("country_name","headcount")]
base27685$year=2027
base27685w=rbind(base27685,base26685w)
base28685=data.table(dat)[,.SD[which.min(pl28abs685)],by=.(country_name)]
base28685=base28685[,c("country_name","headcount")]
base28685$year=2028
base28685w=rbind(base28685,base27685w)
base29685=data.table(dat)[,.SD[which.min(pl29abs685)],by=.(country_name)]
base29685=base29685[,c("country_name","headcount")]
base29685$year=2029
base29685w=rbind(base29685,base28685w)
base30685=data.table(dat)[,.SD[which.min(pl30abs685)],by=.(country_name)]
base30685=base30685[,c("country_name","headcount")]
base30685$year=2030
base30685w=rbind(base30685,base29685w)
setnames(base30685,"headcount","HC2030685")


base201=data.table(dat)[,.SD[which.min(pl20abs1)],by=.(country_name)]
base201=base201[,c("country_name","headcount")]
base201$year=2020
base211=data.table(dat)[,.SD[which.min(pl21abs1)],by=.(country_name)]
base211=base211[,c("country_name","headcount")]
base211$year=2021
base211w=rbind(base211,base201)
setnames(base211,"headcount","HC20211")
base221=data.table(dat)[,.SD[which.min(pl22abs1)],by=.(country_name)]
base221=base221[,c("country_name","headcount")]
base221$year=2022
base221w=rbind(base221,base211w)
setnames(base221,"headcount","HC20221")
base231=data.table(dat)[,.SD[which.min(pl23abs1)],by=.(country_name)]
base231=base231[,c("country_name","headcount")]
base231$year=2023
base231w=rbind(base231,base221w)
base241=data.table(dat)[,.SD[which.min(pl24abs1)],by=.(country_name)]
base241=base241[,c("country_name","headcount")]
base241$year=2024
base241w=rbind(base241,base231w)
base251=data.table(dat)[,.SD[which.min(pl25abs1)],by=.(country_name)]
base251=base251[,c("country_name","headcount")]
base251$year=2025
base251w=rbind(base251,base241w)
base261=data.table(dat)[,.SD[which.min(pl26abs1)],by=.(country_name)]
base261=base261[,c("country_name","headcount")]
base261$year=2026
base261w=rbind(base261,base251w)
base271=data.table(dat)[,.SD[which.min(pl27abs1)],by=.(country_name)]
base271=base271[,c("country_name","headcount")]
base271$year=2027
base271w=rbind(base271,base261w)
base281=data.table(dat)[,.SD[which.min(pl28abs1)],by=.(country_name)]
base281=base281[,c("country_name","headcount")]
base281$year=2028
base281w=rbind(base281,base271w)
base291=data.table(dat)[,.SD[which.min(pl29abs1)],by=.(country_name)]
base291=base291[,c("country_name","headcount")]
base291$year=2029
base291w=rbind(base291,base281w)
base301=data.table(dat)[,.SD[which.min(pl30abs1)],by=.(country_name)]
base301=base301[,c("country_name","headcount")]
base301$year=2030
base301w=rbind(base301,base291w)
setnames(base301,"headcount","HC20301")



base20=data.table(dat)[,.SD[which.min(pl20abs)],by=.(country_name)]
base20=base20[,c("country_name","headcount")]
base20w=base20
base20w$year=2020

setnames(base20,"headcount","HC2020")
base201=data.table(dat)[,.SD[which.min(pl20abs1)],by=.(country_name)]
base201=base201[,c("country_name","headcount")]
setnames(base201,"headcount","HC20201")
base21=data.table(dat)[,.SD[which.min(pl21abs)],by=.(country_name)]
base21w=base21[,c("country_name","headcount")]
base21w$year=2021
base21w=rbind(base20w,base21w)
setnames(base21,"headcount","HC2021")
base211=data.table(dat)[,.SD[which.min(pl21abs1)],by=.(country_name)]
setnames(base211,"headcount","HC20211")
base22=data.table(dat)[,.SD[which.min(pl22abs)],by=.(country_name)]
base22w=base22[,c("country_name","headcount")]
base22w$year=2022
base22w=rbind(base21w,base22w)
setnames(base22,"headcount","HC2022")
# base22=base22[,c("headcount","HC2022")]
base23=data.table(dat)[,.SD[which.min(pl23abs)],by=.(country_name)]
base23w=base23[,c("country_name","headcount")]
base23w$year=2023
base23w=rbind(base22w,base23w)
base24=data.table(dat)[,.SD[which.min(pl24abs)],by=.(country_name)]
base24w=base24[,c("country_name","headcount")]
base24w$year=2024
base24w=rbind(base23w,base24w)
base25=data.table(dat)[,.SD[which.min(pl25abs)],by=.(country_name)]
base25w=base25[,c("country_name","headcount")]
base25w$year=2025
base25w=rbind(base25w,base24w)
base26=data.table(dat)[,.SD[which.min(pl26abs)],by=.(country_name)]
base26w=base26[,c("country_name","headcount")]
base26w$year=2026
base26w=rbind(base25w,base26w)
base27=data.table(dat)[,.SD[which.min(pl27abs)],by=.(country_name)]
base27w=base27[,c("country_name","headcount")]
base27w$year=2027
base27w=rbind(base27w,base26w)
base28=data.table(dat)[,.SD[which.min(pl28abs)],by=.(country_name)]
base28w=base28[,c("country_name","headcount")]
base28w$year=2028
base28w=rbind(base27w,base28w)
base29=data.table(dat)[,.SD[which.min(pl29abs)],by=.(country_name)]
base29w=base29[,c("country_name","headcount")]
base29w$year=2029
base29w=rbind(base29w,base28w)
base30=data.table(dat)[,.SD[which.min(pl30abs)],by=.(country_name)]
base30w=base30[,c("country_name","headcount")]
base30w$year=2030
base30w=rbind(base29w,base30w)

dat1920=merge(baseabs,base20,by=c("country_name"))
dat192021=merge(base21,dat1920,by=c("country_name"))
WEOpov=merge(dat192021,base22,by=c("country_name"))
WEOpov=merge(WEOpov,base25,by=c("country_name"))
WEOpov=WEOpov[,c("country_name","HC2019","HC2020","HC2021","HC2022","region_code.x")]
WEOpov=rbind(WEOpov)
WEOpov$hcdiff1920=WEOpov$HC2020-WEOpov$HC2019
WEOpov$hcdiff2021=WEOpov$HC2021-WEOpov$HC2020


expov=fread("expovSept2022.csv")
expov=subset(expov, reporting_level %in% c("national"))
expovrecent=subset(expov, year>1989 & year<2020)
expovrecent=expovrecent[,c("country_name","year","headcount","pop")]


pop=read.csv("D:/git/poverty_predictions/pops.csv")
pop$PopTotal=pop$PopTotal*1000



pop2020=subset(pop, Time==2020 & Variant=="Medium")
pop2021=subset(pop, Time==2021 & Variant=="Medium")
pop2022=subset(pop, Time==2022 & Variant=="Medium")
names(pop2020)[which(names(pop2020)=="PopTotal")]="PopTotal2020"
names(pop2021)[which(names(pop2021)=="PopTotal")]="PopTotal2021"
names(pop2022)[which(names(pop2022)=="PopTotal")]="PopTotal2022"
pop2020=pop2020[,c("country_name","PopTotal2020")]
pop2021=pop2021[,c("country_name","PopTotal2021")]
pop2022=pop2022[,c("country_name","PopTotal2022")]



pop=subset(pop, Variant=="Medium")

names(pop)[which(names(pop)=="Time")]="year"
long=merge(base30w, pop, by=c("year","country_name"))
long=long[,c("year","headcount","country_name","PopTotal")]    
setnames(long, "PopTotal","pop") 

long=rbind(long, expovrecent)  
long$poorpop=long$headcount*long$pop
regions=subset(WEOpov, year=1990)[,c("country_name","region_code.x")]    

long=merge(long,regions,by=c("country_name"))
globalexpov=data.table(long)[,.(poorpop=sum(poorpop)
                                ,totalpop=sum(pop))
                            , by=c("year")]
globalexpov$expovHC=globalexpov$poorpop/globalexpov$totalpop

pop=pop[,c("year","country_name","PopTotal","PIP_Region")]

moderate=merge(base30365w, pop, by=c("country_name","year"))
moderate$poorpop=moderate$headcount*moderate$PopTotal
moderatetab=data.table(moderate)[,.(poorpop=sum(poorpop)
                                    ,totalpop=sum(PopTotal)),
                                 by=c("year","PIP_Region")]
moderatetab$HC=moderatetab$poorpop/moderatetab$totalpop
moderatetab$poverty_line=3.65
higherpovforecast=merge(base30685w, pop, by=c("country_name","year"))
higherpovforecast$poorpop=higherpovforecast$headcount*higherpovforecast$PopTotal
highertab=data.table(higherpovforecast)[,.(poorpop=sum(poorpop)
                                    ,totalpop=sum(PopTotal)),
                                 by=c("year","PIP_Region")]

highertab$HC=highertab$poorpop/highertab$totalpop
highertab$poverty_line=6.85
modhigherforecasts=rbind(highertab,moderatetab)

#Find most recent year for poverty data
dat=get_stats(country="all")
latest=dat %>% 
  group_by(country_code) %>% 
  slice(which.max(year))
latest=latest[,c("country_code","year","country_name")]

fwrite(higherpovforecast,"output/higherforecast.csv")
fwrite(moderate,"output/moderateforecast.csv")
fwrite(long, "output/poorpoptrends.csv")
