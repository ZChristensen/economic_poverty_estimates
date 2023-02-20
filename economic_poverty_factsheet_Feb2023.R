required.packages <- c("data.table","jsonlite","Hmisc","bit64" ,"readxl","varhandle","ggplot2")
lapply(required.packages, require, character.only=T)

setwd("D:/git/poverty_predictions")
#IPL is the result of our country level forecasts combined with historical PIP data
IPL=fread("output/poorpoptrends.csv")
moderate=fread("output/moderateforecast.csv")
higher=fread("output/higherforecast.csv")
threelineshist=load("D:/git/poverty_predictions/project_data/pipthreelinesSept2022.RData")
highermoderatehist=subset(threelines, poverty_line!=2.15)
#agg is historical regional aggregates from the PIP
agg=fread("aggregates25Sept2022threelines.csv")
#pop is taken from the latest UN population forecasts
pop=fread("pops.csv")
IPL$poverty_line=2.15
moderate$poverty_line=3.65
higher$poverty_line=6.85
all=rbind(higher,moderate)
setnames(all, "PopTotal", "pop")
setnames(IPL, "region_code.x", "PIP_Region")
setnames(highermoderatehist,"region_code", "PIP_Region")
highermoderatehist$poorpop=highermoderatehist$headcount*highermoderatehist$pop
highermoderatehist=highermoderatehist[,c("country_name", "year"      ,   "headcount" ,  
                                          "pop"       ,   "PIP_Region"  , "poorpop"   ,  
                                          "poverty_line")]
all=rbind(all,IPL)
all=rbind(all, highermoderatehist)


geojsoniso <- fromJSON("D:/git/poverty_predictions/project_data/world_map.geo.json")
geojsonnames=geojsoniso$features$properties
geojsonnames$ISO_A3[which(geojsonnames$ISO_A3=="-99")]=geojsonnames$WB_A3[which(geojsonnames$ISO_A3=="-99")]

all$country_name2=all$country_name
all$country_name2[which(all$country_name=="Cabo Verde")]="Cape Verde"
all$country_name2[which(all$country_name=="China")]="People's Republic of China"
all$country_name2[which(all$country_name=="Congo, Dem. Rep.")]="Democratic Republic of the Congo"
all$country_name2[which(all$country_name=="Congo, Rep.")]="Republic of the Congo"
all$country_name2[which(all$country_name=="Cote d'Ivoire")]="Ivory Coast"
all$country_name2[which(all$country_name=="Egypt, Arab Rep.")]="Egypt"
all$country_name2[which(all$country_name=="Eswatini")]="eSwatini"
all$country_name2[which(all$country_name=="Gambia, The")]="The Gambia"
all$country_name2[which(all$country_name=="Iran, Islamic Rep.")]="Iran"
all$country_name2[which(all$country_name=="Korea, Rep.")]="South Korea"
all$country_name2[which(all$country_name=="Kyrgyz Republic")]="Kyrgyzstan"
all$country_name2[which(all$country_name=="Lao PDR")]="Laos"
all$country_name2[which(all$country_name=="Micronesia, Fed. Sts.")]="Federated States of Micronesia"
all$country_name2[which(all$country_name=="North Macedonia")]=  "Republic of Macedonia" 
all$country_name2[which(all$country_name=="Russian Federation")]= "Russia" 
all$country_name2[which(all$country_name=="Sao Tome and Principe")]="São Tomé and Príncipe"
all$country_name2[which(all$country_name=="Slovak Republic")]="Slovakia"    
all$country_name2[which(all$country_name=="St. Lucia" )]= "Saint Lucia"          
all$country_name2[which(all$country_name=="Syrian Arab Republic")]="Syria"
all$country_name2[which(all$country_name=="Turkiye")]="Turkey"              
all$country_name2[which(all$country_name=="United States")]="United States of America"         
all$country_name2[which(all$country_name=="West Bank and Gaza")]="Palestine" 
all$country_name2[which(all$country_name=="Yemen, Rep.")]="Yemen" 
all$country_name2[which(all$country_name=="Timor-Leste")]="East Timor" 
geojsonnames$country_name2=geojsonnames$NAME_EN
geojsonnames=geojsonnames[,c("country_name2","ISO_A3")]
all=merge(all,geojsonnames, by=c("country_name2"))


pop=subset(pop, Variant=="Medium")
#subsetting UN population forecasts for the "medium" projections
setnames(pop,"Time","year")
regionalpops=data.table(pop)[,.(totalpop=sum(PopTotal)),
                                 by=c("year","PIP_Region")]
regionalpops$totalpop=regionalpops$totalpop*1000
#totalpop numbers are reported in the thousands so they are adjusted here

forecastedaggregates=data.table(all)[,.(poorpop=sum(poorpop)
                                        ,pop=sum(pop)),
                                     by=c("year","PIP_Region","poverty_line")]
forecastedaggregates$HC=forecastedaggregates$poorpop/forecastedaggregates$pop
forecastedaggregates=merge(forecastedaggregates,regionalpops, by=c("year","PIP_Region"))

# moderatehigherforecasts=moderatehigher[which(moderatehigher$year>2019),]
# moderatehigherhistory=moderatehigher[which(moderatehigher$year<2020),]
# 
# moderatehigherforecasts=merge(moderatehigherforecasts,globalpops, by=c("year"))
# moderatehigherforecasts$poorpop2=moderatehigherforecasts$HC*moderatehigherforecasts$totalpop.y
#following the World Bank we adjust for the countries missing from the PIP by applying the regional HC to that population
forecastedaggregates$adjustedpoorpop=forecastedaggregates$totalpop*forecastedaggregates$HC
forecastedaggregates=forecastedaggregates[which(forecastedaggregates$year>2019| forecastedaggregates$poverty_line==2.15),]
modhighhistory=agg[,c("year","poverty_line","pop_in_poverty","pop","headcount","region_name")]
names(modhighhistory)=c("year" , "poverty_line" ,"poorpop","totalpop","HC","region_name")
modhighhistory$poorpop=modhighhistory$HC*modhighhistory$totalpop

modhighhistoryreg=modhighhistory[which(modhighhistory$poverty_line!=2.15 & modhighhistory$region_name!="World"),]
modhighhistory=modhighhistory[which(modhighhistory$poverty_line!=2.15 & modhighhistory$region_name=="World"),]
modhighhistory=modhighhistory[,c("year"      ,   "poverty_line", "poorpop"     
                                 , "totalpop"  ,   "HC" )]

#2022 global estimates
global=data.table(forecastedaggregates)[,.(poorpop=sum(adjustedpoorpop),
                                           totalpop=sum(totalpop)),
                                        by=c("year", "poverty_line")]
global$HC=global$poorpop/global$totalpop
global=rbind(global,modhighhistory)

exglobal=global[which(global$poverty_line==2.15),]

exglobal$HC[which(exglobal$year==2020)]/.093
exglobal$HC[which(exglobal$year==2021)]/.088
exglobal$HC[which(exglobal$year==2022)]/.084
exglobal$HC[which(exglobal$year==2030)]/.068
##The World Bank's poverty and shared prosperity report (page 57) estimates a 2020 Headcount of 9.3%, 8.8% for 2021, 8.4% for 2022 and 6.8% for 2030. Our estimates for those time periods are within 2% of that level. This is probably due to differences in the growth estimates being used or different assumptions about the post 2020 wealth distributions where the World Bank has unpublished data at its disposal.  

exglobal$HC[which(exglobal$year==2022)]
#In 2022, 8.5% of global pop is in extreme poverty or 681 million. 
exglobal$poorpop[which(exglobal$year==2022)]
exglobal$poorpop[which(exglobal$year==2022)]-exglobal$poorpop[which(exglobal$year==2019)]
#In 2022, we estimate that 23% of the global population live below $3.65 and 47% lived below $6.85 a day, effectively matching the 2019 numbers.
global$HC[which(global$poverty_line==3.65 & global$year==2022)]
global$HC[which(global$poverty_line==6.85 & global$year==2022)]
(global$HC[which(global$poverty_line==3.65 & global$year==2022)])-(global$HC[which(global$poverty_line==3.65 & global$year==2019)])
(global$HC[which(global$poverty_line==6.85 & global$year==2022)])-(global$HC[which(global$poverty_line==6.85 & global$year==2019)])

#In 2022, we estimate that 1.85 billion people lived below the $3.65 line while 3.71 billion lived below the $6.85 line.
global$poorpop[which(global$poverty_line==3.65 & global$year==2022)]
global$poorpop[which(global$poverty_line==6.85 & global$year==2022)]
(global$poorpop[which(global$poverty_line==3.65 & global$year==2022)])-(global$poorpop[which(global$poverty_line==3.65 & global$year==2019)])
(global$poorpop[which(global$poverty_line==6.85 & global$year==2022)])-(global$poorpop[which(global$poverty_line==6.85 & global$year==2019)])

#In 1990, 14% of people who were extremely poor lived in the region, in 2022 this share is  estimated to be 62%.
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$year==1990 & forecastedaggregates$poverty_line==2.15)]/exglobal$poorpop[which(exglobal$year==1990)]
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$year==2022 & forecastedaggregates$poverty_line==2.15)]/exglobal$poorpop[which(exglobal$year==2022)]
#Countries in the East Asia and Pacific region made up 53% of the extremely poor population in 1990, while in 2022 they represent just 4%.
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$year==1990 & forecastedaggregates$poverty_line==2.15)]/exglobal$poorpop[which(exglobal$year==1990)]
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$year==2022 & forecastedaggregates$poverty_line==2.15)]/exglobal$poorpop[which(exglobal$year==2022)]
#China and India have experienced the are responsible for the greatest national reductions in people living in extreme poverty. More than 1.1 billion people across those two countries moved out of extreme poverty between 1990 and 2022.
(IPL$poorpop[which(IPL$country_name=="China" & IPL$year==1990 & IPL$poverty_line==2.15)]+IPL$poorpop[which(IPL$country_name=="India" & IPL$year==1990 & IPL$poverty_line==2.15)])-(IPL$poorpop[which(IPL$country_name=="China" & IPL$year==2022 & IPL$poverty_line==2.15)]+IPL$poorpop[which(IPL$country_name=="India" & IPL$year==2022 & IPL$poverty_line==2.15)])
(IPL$poorpop[which(IPL$country_name=="China" & IPL$year==2019 & IPL$poverty_line==2.15)])-(IPL$poorpop[which(IPL$country_name=="China" & IPL$year==2021 & IPL$poverty_line==2.15)])
IPL$poorpop[which(IPL$country_name=="India" & IPL$year==2019 & IPL$poverty_line==2.15)]-IPL$poorpop[which(IPL$country_name=="India" & IPL$year==2022 & IPL$poverty_line==2.15)]

#Extreme poverty has increased in most countries in sub-Saharan Africa. The largest increases have occurred in Democratic Republic of the Congo (DRC), Madagascar, Yemen and Nigeria.
ninetyIPL=IPL[which(IPL$year==1990 & IPL$poverty_line==2.15),]
twentytwoIPL=IPL[which(IPL$year==2022 & IPL$poverty_line==2.15),]
countryIPLtrends=merge(ninetyIPL,twentytwoIPL, by=c("country_name","PIP_Region"))
countryIPLtrends$progresspoorpop=countryIPLtrends$poorpop.x-countryIPLtrends$poorpop.y
#Using our model which assumes that the shocks to GDP were equally distributed since COVID, we estimate that Nigeria has 7.5 million more people in poverty than in 2019, followed by the Philippines with 6.4 million, the Democratic Republic of the Congo with 6.0 million. 
nineteenIPL=IPL[which(IPL$year==2019 & IPL$poverty_line==2.15),]
covidIPLtrends=merge(nineteenIPL,twentytwoIPL, by=c("country_name","PIP_Region"))
covidIPLtrends$progresspoorpop=covidIPLtrends$poorpop.x-covidIPLtrends$poorpop.y


#In 1990, it was estimated that 2.0 billion people were living below the extreme poverty line – 38% of the world population at the time.
exglobal$poorpop[which(exglobal$year==1990)]
exglobal$HC[which(exglobal$year==1990)]
#the goal (to reduce extreme poverty by half) was achieved in 2011, four years ahead of schedule.
exglobal$poorpop[which(exglobal$year==2011)]/exglobal$poorpop[which(exglobal$year==1990)]
#In 2019 (the most recent year for which global estimates are available), 660 million people were living in extreme poverty – 8.5% of the world population. In 2020, that number increased and is estimated to be at 733 million and in 2022 it is estimated at 682 million . 
exglobal$poorpop[which(exglobal$year==2019)]
exglobal$HC[which(exglobal$year==2019)]
exglobal$poorpop[which(exglobal$year==2020)]
exglobal$poorpop[which(exglobal$year==2022)]

figure1=exglobal[which(exglobal$year<2023),]
figure1$poorpop=figure1$poorpop/1000000000
figure1=figure1[,c("year","poorpop","HC")]
ggplot(exglobal, aes(x=year, y=poorpop))+ geom_line()
ggplot(figure1,aes(x=year))+ geom_line(aes(y=poorpop, color='poorpop'))+geom_line(aes(y=HC), color='green')+
  scale_color_manual('Measure', values=c('red', 'green')) +
  labs(title = 'progress in extreme poverty 1990-2022', x = 'Year', y = 'Population in extreme poverty (billions)')+
  theme_minimal()
names(figure1)=c("year","Number of People in poverty (billions)","Proportion of people in poverty")
# fwrite(figure1, "output/factsheetfigure1.csv")
#The number of people living in poverty as measured by the higher international poverty lines of $3.65 increased between 1990 and 1999, but fell between 2000 and 2019. For poverty at $6.85, poverty increased between 1990 and 2003 before falling until 2020. 
ggplot(global[which(global$poverty_line==3.65),], aes(x=year, y=poorpop))+geom_line()
ggplot(global[which(global$poverty_line==6.85),], aes(x=year, y=poorpop))+geom_line()
# as of 2022, 23% of the global population (X billion people) were living below $3.65 a day. 
global$HC[which(global$poverty_line==3.65 & global$year==2022)]
global$poorpop[which(global$poverty_line==3.65 & global$year==2022)]
#The number of people living below $3.65 increased between 1990 and 1999 (mostly attributable to rapid population growth in low- and lower-middle income countries) but fell until 2020 and is estimated to have increased by about 160 million between 2019 and 2020 and is estimated to have 46 million more people in 2022 than 2019. 
global$poorpop[which(global$poverty_line==3.65 & global$year==2020)]-global$poorpop[which(global$poverty_line==3.65 & global$year==2019)]
global$poorpop[which(global$poverty_line==3.65 & global$year==2022)]-global$poorpop[which(global$poverty_line==3.65 & global$year==2019)]

#Between 1990 and 1999, the number of people living on less than $6.85 a day increased, rising from 3.6 billion to a peak of almost 4.2 billion people.
global$poorpop[which(global$poverty_line==6.85 & global$year==1990)]
global$poorpop[which(global$poverty_line==6.85 & global$year==2003)]
#the pandemic increased the number of people below the line by 179 million. In 2022, there are still 124 million more people below that line than in 2019.
global$poorpop[which(global$poverty_line==6.85 & global$year==2020)]-global$poorpop[which(global$poverty_line==6.85 & global$year==2019)]
global$poorpop[which(global$poverty_line==6.85 & global$year==2022)]-global$poorpop[which(global$poverty_line==6.85 & global$year==2019)]
#In 2022, about two fifths of the world’s population (47%) lived on less than $6.85 a day. 
global$HC[which(global$poverty_line==6.85 & global$year==2022)]

figure2=global[which(global$poverty_line!=2.15 & global$year>1989 & global$year< 2023),]
figure2$poorpop=figure2$poorpop/1000000000
ggplot(figure2[which(figure2$poverty_line==3.65),],aes(x=year))+ geom_line(aes(y=poorpop, color='poorpop'))+geom_line(aes(y=HC), color='green')+
  scale_color_manual('Measure', values=c('red', 'green')) +
  labs(title = 'progress in $3.65 poverty 1990-2022', x = 'Year', y = 'Population in extreme poverty (billions)')+
  theme_minimal()
ggplot(figure2[which(figure2$poverty_line==6.85),],aes(x=year))+ geom_line(aes(y=poorpop, color='poorpop'))+geom_line(aes(y=HC), color='green')+
  scale_color_manual('Measure', values=c('red', 'green')) +
  labs(title = 'progress in $6.85 poverty 1990-2022', x = 'Year', y = 'Population in extreme poverty (billions)')+
  theme_minimal()

figure3=subset(figure2, poverty_line==6.85)
figure2=subset(figure2, poverty_line==3.65)
figure2=figure2[,c("year","poorpop","HC")]
figure3=figure3[,c("year","poorpop","HC")]
# fwrite(figure2,"output/factsheetfigure2.csv")
# fwrite(figure3,"output/factsheetfigure3.csv")

regname=agg[which(agg$year==1990 & poverty_line==2.15),]
regname=regname[,c("region_code","region_name")]
setnames(regname, "region_code","PIP_Region")



figure4=forecastedaggregates[which(forecastedaggregates$year<2023),]
figure4$poorpop=figure4$adjustedpoorpop/1000000000
figure4=merge(figure4,regname,by="PIP_Region")


ggplot(figure4[which(figure4$poverty_line==2.15),], aes(x=year, y=poorpop, fill=region_name))+geom_col()


figure4=figure4[,c("year","poorpop","poverty_line","region_name","HC")]
modhighhistoryreg=modhighhistoryreg[,c("year","poorpop","poverty_line","region_name","HC")]
modhighhistoryreg$poorpop=modhighhistoryreg$poorpop/1000000000
figure4=rbind(figure4,modhighhistoryreg)
figure4$year[which(figure4$region_name=="South Asia")]
#Note that South Asia is missing from 1997-2001 from the aggregates table
figure4=figure4[which(figure4$year>2001),]
ggplot(figure4[which(figure4$poverty_line==2.15),], aes(x=year, y=poorpop, fill=region_name))+geom_col()
ggplot(figure4[which(figure4$poverty_line==3.65),], aes(x=year, y=poorpop, fill=region_name))+geom_col()
ggplot(figure4[which(figure4$poverty_line==6.85),], aes(x=year, y=poorpop, fill=region_name))+geom_col()

names(figure4)=c("year","Population in poverty (billions)","poverty line (2017 PPP)","Region name","Proportion of the population in poverty")
figure4=figure4[,c("year","Region name","Population in poverty (billions)","Proportion of the population in poverty","poverty line (2017 PPP)")]
  # fwrite(figure4, "output/factsheetfigure4.csv")

#In 1990, the number of people living in extreme poverty was: 81 million in Latin America and Caribbean, 15 million in Europe and Central Asia, 14 million in Middle East and North America, 275 million in Sub-Saharan Africa, 567 in South Asia, 1.06 billion in East Asia and Pacific, and 4 million in other high income countries.
forecastedaggregates[which(forecastedaggregates$year==1990 & forecastedaggregates$poverty_line==2.15),]
#In 2022, the number of people living in extreme poverty was: 30 million in Latin America and Caribbean, 11 million in Europe and Central Asia, 41 million in Middle East and North America, 426 million in Sub-Saharan Africa, 137 in South Asia, 29 million in East Asia and Pacific, and 7 million in other high income countries
forecastedaggregates[which(forecastedaggregates$year==2022 & forecastedaggregates$poverty_line==2.15),]

exglobal2=exglobal[,c("year","poorpop")]
setnames(exglobal2,"poorpop","globalpoorpop")
forecastedaggregates=merge(forecastedaggregates,exglobal2, by=c("year"))
forecastedaggregates$shareofpoor=forecastedaggregates$adjustedpoorpop/forecastedaggregates$globalpoorpop
#In 1990, 53% of the world’s extremely poor people lived in countries in the East Asia and Pacific region; in 2021 they represent 4%
forecastedaggregates$shareofpoor[which(forecastedaggregates$year==1990 & forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$poverty_line==2.15)]
forecastedaggregates$shareofpoor[which(forecastedaggregates$year==2022 & forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$poverty_line==2.15)]
# East Asia and the Pacific has demonstrated the most significant change: in 1990, the region was home to 53% of the world’s extremely poor people (over 1 billion), but by 2022 this had dropped to 4% (29 million people). 
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$year==1990 & forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$poverty_line==2.15)]
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$year==2022 & forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$poverty_line==2.15)]
#This rate of poverty reduction is unmatched anywhere else in the world, with the extreme poverty rate within that region dropping from 65% to 1% in this period.
forecastedaggregates$HC[which(forecastedaggregates$year==1990 & forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$poverty_line==2.15)]
forecastedaggregates$HC[which(forecastedaggregates$year==2022 & forecastedaggregates$PIP_Region=="EAP" & forecastedaggregates$poverty_line==2.15)]

#The rapid poverty reductions in the East Asia and Pacific region contrast with changes in sub-Saharan Africa, where the number of people living in extreme poverty has significantly increased since 1990 – without major drops since 2003.
ggplot(forecastedaggregates[which(forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$poverty_line==2.15 & year<2023),], aes(x=year, y=adjustedpoorpop))+geom_line()
# In 1990, 275 million people in sub-Saharan Africa were living below the extreme poverty line; by 2022, the region is home to more than 426 million extremely poor people, a global share of over 62%.
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$year==1990 & forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$poverty_line==2.15)]
forecastedaggregates$adjustedpoorpop[which(forecastedaggregates$year==2022 & forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$poverty_line==2.15)]
forecastedaggregates$shareofpoor[which(forecastedaggregates$year==2022 & forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$poverty_line==2.15)]
#The change in poverty rate has also been the slowest of any region, with a fall from 53% in 1990 to 35% in 2022.
forecastedaggregates$HC[which(forecastedaggregates$year==1990 & forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$poverty_line==2.15)]
forecastedaggregates$HC[which(forecastedaggregates$year==2022 & forecastedaggregates$PIP_Region=="SSA" & forecastedaggregates$poverty_line==2.15)]
#Although most countries worldwide have seen a decrease in the number of people living  in extreme poverty since 1990, 36% have experienced an increase in the number of people living in extreme poverty.
countryIPLtrends$progressHC=countryIPLtrends$headcount.x-countryIPLtrends$headcount.y
nrow(countryIPLtrends[which(countryIPLtrends$progresspoorpop<0),])/(nrow(countryIPLtrends))
#The proportion of extremely poor people decreased in 122 countries since 1990, however the number of people in extreme poverty has increased in 33 countries
nrow(countryIPLtrends[which(countryIPLtrends$progressHC>0),])
nrow(countryIPLtrends[which(countryIPLtrends$progressHC<0),])


ninetyIPL=IPL[which(IPL$year==1990 & IPL$poverty_line==3.65),]
twentytwoIPL=IPL[which(IPL$year==2022 & IPL$poverty_line==3.65),]
countryIPLtrends=merge(ninetyIPL,twentytwoIPL, by=c("country_name","PIP_Region"))
countryIPLtrends$progresspoorpop=countryIPLtrends$poorpop.x-countryIPLtrends$poorpop.y

ninety3lines=all[which(all$year==1990),]
twentytwo3lines=all[which(all$year==2022),]
countrytrends=merge(ninety3lines,twentytwo3lines, by=c("country_name", "poverty_line","PIP_Region","ISO_A3"))
countrytrends$progresspoorpop=countrytrends$poorpop.x-countrytrends$poorpop.y
countrytrends$progressHC=countrytrends$headcount.x-countrytrends$headcount.y

figure5=countrytrends[,c("country_name","poverty_line","PIP_Region","progresspoorpop","progressHC","ISO_A3")]
#find quartiles for heatmap display
quartpop215pos=figure5$progresspoorpop[which(figure5$progresspoorpop<0 & figure5$poverty_line==2.15)]
quantile(quartpop215pos)
quartpop215neg=figure5$progresspoorpop[which(figure5$progresspoorpop>0 & figure5$poverty_line==2.15)]
quantile(quartpop215neg)
quartpop365pos=figure5$progresspoorpop[which(figure5$progresspoorpop<0 & figure5$poverty_line==3.65)]
quantile(quartpop365pos)
quartpop365neg=figure5$progresspoorpop[which(figure5$progresspoorpop>0 & figure5$poverty_line==3.65)]
quantile(quartpop365neg)
quartpop685pos=figure5$progresspoorpop[which(figure5$progresspoorpop<0 & figure5$poverty_line==6.85)]
quantile(quartpop685pos)
quartpop685neg=figure5$progresspoorpop[which(figure5$progresspoorpop>0 & figure5$poverty_line==6.85)]
quantile(quartpop685neg)



# fwrite(figure5,"output/factsheetfigure5.csv")

#In 1990, China and India were home to just under 1.2 billion people living in extreme poverty – over 61% of the world’s poorest people at that time. 
all$poorpop[which(all$country_name=="China" & all$year==1990 & all$poverty_line==2.15)] + all$poorpop[which(all$country_name=="India" & all$year==1990 & all$poverty_line==2.15)] 
(all$poorpop[which(all$country_name=="China" & all$year==1990 & all$poverty_line==2.15)] + all$poorpop[which(all$country_name=="India" & all$year==1990 & all$poverty_line==2.15)] )/(exglobal$poorpop[which(exglobal$year==1990)])
# By 2022, this figure is estimated to have reduced to less than 117 million. 
all$poorpop[which(all$country_name=="China" & all$year==2022 & all$poverty_line==2.15)] + all$poorpop[which(all$country_name=="India" & all$year==2022 & all$poverty_line==2.15)] 
(all$poorpop[which(all$country_name=="China" & all$year==2022 & all$poverty_line==2.15)] + all$poorpop[which(all$country_name=="India" & all$year==2022 & all$poverty_line==2.15)] )/(exglobal$poorpop[which(exglobal$year==2022)])

# However, while China had all but ended extreme poverty in 2017 (with rates at 0.4%), 8% of India’s population remains below the extreme poverty line.
all$headcount[which(all$country_name=="China" & all$year==2022 & all$poverty_line==2.15)] 
all$headcount[which(all$country_name=="India" & all$year==2022 & all$poverty_line==2.15)] 

#Of the 44 countries in sub-Saharan Africa with available poverty trend data, 18 have seen an increase in the number of people living in extreme poverty since 1990.
nrow(countrytrends[which(countrytrends$PIP_Region=="SSA" & countrytrends$poverty_line==2.15),])
nrow(countrytrends[which(countrytrends$progresspoorpop>=0 & countrytrends$PIP_Region=="SSA" & countrytrends$poverty_line==2.15),])
#Rises in the extremely poor population were most acute in DRC, Madagascar, Yemen and Nigeria (although the DRC and Nigeria saw a reduction in the proportion of its population below the extreme poverty line over the same period).
countryIPLtrends=countrytrends[which(countrytrends$poverty_line==2.15),]
describe(countryIPLtrends$progresspoorpop)
countryIPLtrends$country_name[which(countryIPLtrends$progresspoorpop< -16000000)]
countryIPLtrends$progressHC[which(countryIPLtrends$country_name %in% c("Congo, Dem. Rep.", "Madagascar" ,     
                                                           "Nigeria" ,         "Yemen, Rep." ))]
#The United States stands out among high-income countries for its extreme poverty rate which as been about 1% of the population since 2003 and is forecasted to remain at that level. Due to population growth, this places the United States among the countries expected to see the biggest increases in the number of people in extreme poverty. Between 1990 and 2022, the number of people in extreme poverty in the United States is estimated to have grown by 2.1 million.
USAexpov=all[which(all$country_name=="United States"  & all$poverty_line==2.15),] 
countryIPLtrends[which(countryIPLtrends$country_name=="United States")]
