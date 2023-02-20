required.packages <- c("ggplot2","Hmisc","data.table","plyr")
lapply(required.packages, require, character.only=T)

setwd("D:/git/poverty_predictions")

load("D:/git/poverty_predictions/pipAggScrapeOct2022.RData")


agg_results$HC=round(agg_results$headcount,2)
agg_results=subset(agg_results, region_name %in% c("Sub-Saharan Africa"         
                                                  ,"Europe & Central Asia"      
                                                   ,"Other High Income Countries"
                                                  ,"Latin America & Caribbean"  
                                                   ,"South Asia"                 
                                                   ,"Middle East & North Africa" 
                                                   ,"East Asia & Pacific"        
                                                   ,"World" ) )
percentiles=data.table(agg_results)[,.(pl=min(poverty_line)
                                               ,pop=mean(pop)
                                               ,pop_in_poverty=mean(pop_in_poverty)
                                       
)
,by=c("region_name","year","HC")
]
percentiles1990=subset(percentiles, year==1990)
names(percentiles1990)=c("region_name","year","HC","pl1990","pop1990","pop_in_poverty1990")
percentiles2019=subset(percentiles, year==2019)
names(percentiles2019)=c("region_name","year","HC","pl2019","pop2019","pop_in_poverty2019")
percentiles9019=join(percentiles1990,percentiles2019, by=c("region_name","HC"))  
percentiles9019$growth=(percentiles9019$pl2019-percentiles9019$pl1990)/percentiles9019$pl1990
worldpercentiles=subset(percentiles9019, region_name=="World")
plot(worldpercentiles$HC,worldpercentiles$growth)
fwrite(worldpercentiles,"worldpercentilegrowth19902019.csv")
