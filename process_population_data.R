library(tidycensus)
library(tidyverse)
library(sf)
library(sfheaders)


census_api_key("XXX", install = TRUE)
readRenviron("~/.Renviron")

v17 <- load_variables(2009, "acs5", cache = TRUE)
View(v17[v17$geography=="block group" & !is.na(v17$geography),])

#variables: population (B01003_001), median income (B19013_001), educational attainment (B15003_001)
vars_toget<-c("B01003_001","B19013_001")#,"B15003_017","B15003_021","B15003_022","B15003_023","B15003_024","B15003_025")
vars_names<-c("Population","Median_Income")#,"HS_Grad","AS_Grad","BA_Grad","MA_Grad","PRO_Grad","PHD_Grad")


rBOUND_data<-NULL
for(y in 2009:2023){
  getGeom<-T
  for(i in 1:length(vars_toget)){
    ca_pop_data <- get_acs(
      geography = "tract",
      state="CA",
      variables = vars_toget[i],
      year = y,
      survey = "acs5",
      geometry = getGeom,
      cb=F
    )
    
    colnames(ca_pop_data)[4]<-vars_names[i]
    colnames(ca_pop_data)[5]<-paste0("moe_",vars_names[i])
    ca_pop_data$NAME<-NULL
    ca_pop_data$variable<-NULL
    
    if(i==1){
      ca_pop_data$year<-y
      alldata<-ca_pop_data
      getGeom<-F
    } else{
      alldata<-merge(alldata,ca_pop_data,by="GEOID",all.x=T)
    }
  }
  rBOUND_data<-rbind(alldata,rBOUND_data)
}


v17 <- load_variables(2000, "sf3", cache = TRUE)
vars_toget<-c("P001001","HCT012001")#,"B15003_017","B15003_021","B15003_022","B15003_023","B15003_024","B15003_025")
vars_names<-c("Population","Median_Income")#,"HS_Grad","AS_Grad","BA_Grad","MA_Grad","PRO_Grad","PHD_Grad")
sumfile<-c("sf1","sf3")


getGeom<-T
for(i in 1:length(vars_toget)){
  ca_pop_data <- get_decennial(
    geography = "tract",
    state="CA",
    variables = vars_toget[i],
    year = 2000,
    geometry = getGeom,
    cb=F
  )
  
  colnames(ca_pop_data)[4]<-vars_names[i]
  ca_pop_data[[paste0("moe_",vars_names[i])]]<-NA
  ca_pop_data$NAME<-NULL
  ca_pop_data$variable<-NULL
  
  if(i==1){
    ca_pop_data$year<-2000
    ca_pop_data<-ca_pop_data[,c(1,2,4,5,3)]
    alldata<-ca_pop_data
    getGeom<-F
  } else{
    alldata<-merge(alldata,ca_pop_data,by="GEOID",all.x=T)
  }
}


rBOUND_data<-rbind(alldata,rBOUND_data)

st_write(rBOUND_data, "C:/Users/joeja/Desktop/research_postdoc/digital_twin_data/population/pop_income_data.shz", delete_dsn = TRUE,driver = "ESRI Shapefile")


toKEEP<-rep(F,nrow(ca_pop_data))

for(i in 1:nrow(ca_pop_data)){
  what<-sf_to_df(ca_pop_data[i,])
  
  # what$lon<= -110 & what$lon>= -125 & what$lat>= 30 & what$lat<= 45
  if(max(what$x)>= -125 & min(what$x)< -110 & max(what$y)>= 30 & min(what$x)< 45) toKEEP[i]<-T
  if(i %% 1000 ==0) print(i)
}

ca_pop_data<-ca_pop_data[toKEEP,]






vars_acs <- c(
  total_pop = "B01003_001",        # Total population
  med_hh_income = "B19013_001",    # Median household income
  # Education: % with BA or higher
  edu_bachelors = "B15003_022", 
  edu_masters   = "B15003_023",
  edu_professional = "B15003_024",
  edu_doctorate    = "B15003_025"
)

# a helper to compute percent with BA+
calc_edu_ba <- function(df) {
  df %>%
    mutate(
      total_ba_plus = edu_bachelors + edu_masters + edu_professional + edu_doctorate,
      pct_ba_plus = 100 * total_ba_plus / estimate_total_pop
    )
}


get_yearly_tract_data <- function(year) {
  
  # determine source
  source <- ifelse(year %in% c(1980, 1990, 2000, 2010, 2020), "dec" , "acs5")
  
  message("Fetching: ", year, " | source = ", source)
  
  tidycensus::get_acs(
    geography = "tract",
    variables = vars_acs,
    year = year,
    survey = source,
    state = "CA",
    geometry = T
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = estimate
    ) %>%
    # rename what comes back
    rename(
      total_pop = B01003_001,
      med_hh_income = B19013_001,
      edu_bachelors = B15003_022,
      edu_masters   = B15003_023,
      edu_professional = B15003_024,
      edu_doctorate    = B15003_025
    ) %>%
    mutate(year = year) %>%
    # compute education percent
    mutate(
      total_ba_plus = edu_bachelors + edu_masters + edu_professional + edu_doctorate,
      pct_ba_plus = 100 * total_ba_plus / total_pop
    )
}

years <- c(1980, 1990, 2000, 2010, seq(2011, 2024))

# fetch all
all_tracts <- map_dfr(years, get_yearly_tract_data)



