required.packages <- c("data.table", "readxl","varhandle")
lapply(required.packages, require, character.only=T)

setwd("D:/git/poverty_predictions")

# WorldPopulationProspects
# pop=read.csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.csv")
pop=read.csv("WPP2022_TotalPopulationBySex.csv")

names(pop)[which(names(pop)=="Location")]="country_name"


pop$country_name[which(pop$country_name=="Bolivia (Plurinational State of)")]="Bolivia"
pop$country_name[which(pop$country_name=="Democratic Republic of the Congo")]="Congo, Democratic Republic of"
pop$country_name[which(pop$country_name=="Congo")]="Congo, Republic of"
pop$country_name[which(pop$country_name=="CÃ´te d'Ivoire")]="Cote d'Ivoire"
pop$country_name[which(pop$country_name=="Côte d'Ivoire")]="Cote d'Ivoire"
pop$country_name[which(pop$country_name=="Czechia")]="Czech Republic"
pop$country_name[which(pop$country_name=="Egypt")]="Egypt, Arab Republic of"
pop$country_name[which(pop$country_name=="Gambia")]="Gambia, The"
pop$country_name[which(pop$country_name=="Iran (Islamic Republic of)")]="Iran, Islamic Republic of"
pop$country_name[which(pop$country_name=="Republic of Korea")]="Korea, Republic of"
pop$country_name[which(pop$country_name=="Kyrgyzstan")]="Kyrgyz Republic"
pop$country_name[which(pop$country_name=="TFYR Macedonia")]="Macedonia, former Yugoslav Republic of"
pop$country_name[which(pop$country_name=="Micronesia (Fed. States of)")]="Micronesia, Fed. Sts."
pop$country_name[which(pop$country_name=="Republic of Moldova")]="Moldova"
pop$country_name[which(pop$country_name=="Slovakia")]="Slovak Republic"
pop$country_name[which(pop$country_name=="Saint Lucia")]="St. Lucia"
pop$country_name[which(pop$country_name=="United Republic of Tanzania")]="Tanzania"
pop$country_name[which(pop$country_name=="United States of America")]="United States"
pop$country_name[which(pop$country_name=="Venezuela (Bolivarian Republic of)")]="Venezuela, RB"
pop$country_name[which(pop$country_name=="Viet Nam")]="Vietnam"
pop$country_name[which(pop$country_name=="Yemen")]="Yemen, Republic of"
pop$country_name[which(pop$country_name=="State of Palestine")]="West Bank and Gaza"
# pop$country_name[which(pop$country_name=="Lao People's Democratic Republic")]="Lao PDR"
pop$country_name[which(pop$country_name=="Türkiye")]="Turkiye"
# pop$country_name[which(pop$country_name=="Kosovo (under UNSC res. 1244)")]="Kosovo"
# pop$country_name[which(pop$country_name=="China, Taiwan Province of China")]="Taiwan, China"
pop$country_name[which(pop$country_name=="China, Macao SAR")]="Macao SAR, China"
pop$country_name[which(pop$country_name=="China, Hong Kong SAR")]="Hong Kong SAR, China"
pop$country_name[which(pop$country_name=="Curaçao")]="Curacao"
pop$country_name[which(pop$country_name=="Saint Martin (French part)")]="St. Martin (French part)"
pop$country_name[which(pop$country_name=="Saint Kitts and Nevis")]="St. Kitts and Nevis"
pop$country_name[which(pop$country_name=="Saint Vincent and the Grenadines")]="St. Vincent and the Grenadines"
pop$country_name[which(pop$country_name=="United States Virgin Islands")]="St. Vincent and the Grenadines"
pop$country_name[which(pop$country_name=="Venezuela, RB")]="Venezuela, Republica Bolivariana de"
pop$country_name[which(pop$country_name=="Micronesia, Fed. Sts.")]="Micronesia"



pop=subset(pop, LocTypeName %in% "Country/Area")

regions=read.csv("project_data/WB_UN regions.csv", fileEncoding = "UTF-8")

setdiff(unique(pop$country_name), unique(regions$country_name))

pop=merge(pop, regions, by=c("country_name"))

pop$country_name[which(pop$country_name=="China, Taiwan Province of China")]="Taiwan, China"
pop$country_name[which(pop$country_name=="Congo, Democratic Republic of")]="Congo, Dem. Rep."
pop$country_name[which(pop$country_name=="Congo, Republic of")]="Congo, Rep."
pop$country_name[which(pop$country_name=="Egypt, Arab Republic of")]="Egypt, Arab Rep."
pop$country_name[which(pop$country_name=="Micronesia")]="Micronesia, Fed. Sts."
pop$country_name[which(pop$country_name=="Iran, Islamic Republic of")]="Iran, Islamic Rep."
pop$country_name[which(pop$country_name=="Korea, Republic of")]="Korea, Rep."
pop$country_name[which(pop$country_name=="Lao People's Democratic Republic")]="Lao PDR"
pop$country_name[which(pop$country_name=="Venezuela, Republica Bolivariana de")]="Venezuela, RB"
pop$country_name[which(pop$country_name=="Kosovo (under UNSC res. 1244)")]="Kosovo"
pop$country_name[which(pop$country_name=="Yemen, Republic of")]="Yemen, Rep."


write.csv(pop,"D:/git/poverty_predictions/pops.csv",row.names=F,na="")