library(httr)
library(Quandl)
library(readr)
library(dplyr)
library(rvest)
library(stringr)
library(xlsx)
library(noncensus)
library(rvest)
library(tidyverse)
library(sqldf)


Quandl.api_key('fCJstrGkz2LkRxzp9AxX')


data = Quandl("ZILLOW/CO599_MSPAH")
data2 = data[1,]

china = Quandl.datatable("DY/MIA")

pricepersqft = read_csv("pricepersqft.csv")

county_count = pricepersqft %>% dplyr::select(County,State) %>% 
  group_by(County,State) %>% summarize(count = length(County))

price_2017 = pricepersqft %>% 
  dplyr::select(County,State, `January 2017`) %>%
  group_by(County,State) %>% 
  summarize(price = median(`January 2017`))


#first wiki

url <- "https://en.wikipedia.org/wiki/List_of_United_States_counties_by_per_capita_income"

df = read_html(url) %>%
  html_nodes("table.wikitable.sortable") %>%
  .[[1]] %>%
  html_table() %>% 
  filter(.$State != "")


county_data <- data.frame(
  County = df$`County-equivalent` %>% as.character(.),
  State = df$State %>% as.character(.),
  Income_PerCapita = df$`Per capitaincome` %>% 
    str_replace_all(",", "") %>%
    substr(2, 10000) %>% 
    as.numeric(as.character(.)),
  Median_Household_Income = df$Medianhouseholdincome %>% 
    str_replace_all(",", "") %>%
    substr(2, 10000) %>% 
    as.numeric(as.character(.)),
  Median_Family_Income = df$Medianfamilyincome %>% 
    str_replace_all(",", "") %>%
    substr(2, 10000) %>% 
    as.numeric(as.character(.)),
  Population = df$Population %>% 
    str_replace_all(",", "") %>% 
    as.numeric(as.character(.)),
  Num_Household = df$`Number ofhouseholds` %>% 
    str_replace_all(",", "") %>% 
    as.numeric(as.character(.))
)

county_data$State = state.abb[match(county_data$State,state.name)]

#price_2017_2 = price_2017_2[price_2017_2$State.x==price_2017_2$State.y,]

'%ni%' <- Negate('%in%')

difficult_county = price_2017[price_2017$County %ni% county_data$County,]
price_2017[55,1] = 'Baltimore County'
price_2017[305,1] = 'Colorado County'
price_2017[374,1] = 'DeKalb'
price_2017[375,1] = 'DeSoto'
price_2017[376,1] = 'DeWitt'
price_2017[380,1] = 'DeKalb'
for(i in 382:386){
  price_2017[i,1] = 'Delaware County'
}
price_2017[295,1] = 'Washington County'
price_2017[405,1] = 'Doña Ana'
price_2017[419,1] = 'DuPage'
price_2017[462,1] = 'Fairfax County'
price_2017[641,1] = 'Hawaii County'
price_2017[705,1] = 'Iowa County'
price_2017[727,1] = 'James City County'
price_2017[815,1] = 'LaPorte'
price_2017[821,1] = 'LaGrange'
price_2017[845,1] = 'LaRue'
price_2017[866,1] = 'Le Flore'
price_2017[976,1] = 'Matanuska-Susitna'
price_2017[1100,1] = 'Nevada County'
price_2017[1133,1] = 'Ohio County'
price_2017[1136,1] = 'Oklahoma County'
price_2017[1248,1] = "Prince George's"
price_2017[1249,1] = "Prince George's"
price_2017[1262,1] = "Queen Anne's"
price_2017[1298,1] = "Roanoke County"
for (i in 1327:1344){
  price_2017[i,1] = paste0('St.',substr(price_2017[i,1],6,100))}
price_2017[1531,1] = "Utah County"
for(i in 1572:1588){
  price_2017[i,1] = "Washington County"
}
price_2017[1661,1] = "Wyoming County"

difficult_county_check = price_2017[price_2017$County %ni% county_data$County,]
price_2017[1338,1] = "St. Louis County"
price_2017[1339,1] = "St. Louis County"
price_2017[1343,1] = "St. Mary's"

price_2017_2 = merge(price_2017,county_data,by = c('County','State'))
#write.xlsx(price_2017_2, "/Users/ricky/Downloads/price_2017.xlsx")

## hilton data


url = "https://www3.hilton.com/en/hotel-locations/us/index.html"
baseURL =  "https://www3.hilton.com"
webpage = read_html(url)
links = html_nodes(webpage,'ul.directory_locations_list li a')
states = html_text(links)
urls = html_attr(links, "href")
urls = paste0(baseURL, urls)
zips = c()
for(url in urls){
  webpage = read_html(url)
  links = html_nodes(webpage,'ul.directory_locations_list li a')
  hotels = html_text(links)
  locationURLs = html_attr(links, "href")
  locationURLs = paste0(baseURL, locationURLs)
  for(locationURL in locationURLs){
    webpage2 = read_html(locationURL)
    links = html_nodes(webpage2,'ul.directory_hotels_list li a')
    hotel = html_text(links)
    # print(hotel)
    lis = html_nodes(webpage2,'ul.directory_hotels_list li')
    locations = html_text(lis)
    locations = gsub("\t","", locations)
    locations = gsub("\r\n","", locations)
    tokens = strsplit(locations, " ")[[1]]
    target = tokens[length(tokens)]
    zipcode = strsplit(target, "USA")[[1]][1]
    zipcode = substr(zipcode,1,5)
    zips = c(zips, zipcode)
  }
}
hilton_zips = as.data.frame(zips)

zip_county_lookup = read_csv("ZIP-COUNTY-FIPS_2018-03.csv")
hilton_county = merge(hilton_zips,zip_county_lookup,by.x ='zips',by.y = 'ZIP')
hilton_county$COUNTYNAME = strsplit(hilton_county$COUNTYNAME, " ") 
for (i in 1:189){
  hilton_county$COUNTYNAME[[i]] = hilton_county$COUNTYNAME[[i]][-length(hilton_county$COUNTYNAME[[i]])]
  if (length(hilton_county$COUNTYNAME[[i]]) >= 2){
    hilton_county$COUNTYNAME[[i]] = paste(hilton_county$COUNTYNAME[[i]][1],hilton_county$COUNTYNAME[[i]][2])
  }}
hilton_county$COUNTYNAME = unlist(hilton_county$COUNTYNAME)


distinct_count = hilton_county%>%
  group_by(COUNTYNAME) %>%
  summarize(count = length(COUNTYNAME))


colnames(distinct_count)[2] = 'hilton_count'
hilton_county_final = merge(hilton_county,distinct_count,by='COUNTYNAME') %>%
  .[!duplicated(.$COUNTYNAME),] %>% dplyr::select(COUNTYNAME,hilton_count)


price_2017_3 = left_join(price_2017_2, hilton_county_final, by = c("County"="COUNTYNAME")) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

price_2017_4 = price_2017_3
price_2017_4$County = strsplit(price_2017_4$County, " ")
for (i in 1:1669){
  if ((length(price_2017_4$County[[i]]) > 1 & (price_2017_4$County[[i]][2] == 'County')) | (length(price_2017_4$County[[i]]) > 2 & price_2017_4$County[[i]][3] == 'County')){
    price_2017_4$County[[i]] = price_2017_4$County[[i]][-length(price_2017_4$County[[i]])]
  }}

for (i in 1:1669){
  if (length(price_2017_4$County[[i]]) == 2) {
    price_2017_4$County[[i]] = paste(price_2017_4$County[[i]][1],price_2017_4$County[[i]][2])}
  if (length(price_2017_4$County[[i]]) == 3) {
    price_2017_4$County[[i]] = paste(price_2017_4$County[[i]][1],price_2017_4$County[[i]][2],price_2017_4$County[[i]][3])}
  if (length(price_2017_4$County[[i]]) == 4) {
    price_2017_4$County[[i]] = paste(price_2017_4$County[[i]][1],price_2017_4$County[[i]][2],price_2017_4$County[[i]][3],price_2017_4$County[[i]][4])}
}

price_2017_4$County = unlist(price_2017_4$County)


## crime rate
crime_data = read_csv("crime_data_w_population_and_crime_rate.csv")
crime_data = crime_data %>%
  select(county_name,crime_rate_per_100000,IDNO,CPOPCRIM,CPOPARST,COVIND,MURDER,RAPE,ROBBERY,AGASSLT,BURGLRY,LARCENY,MVTHEFT,ARSON)
crime_data$county_name = strsplit(crime_data$county_name, " ") 
for (i in 1:3136){
  crime_data$State[i] = crime_data$county_name[[i]][length(crime_data$county_name[[i]])]
  crime_data$county_name[[i]] = crime_data$county_name[[i]][-length(crime_data$county_name[[i]])]
  if (crime_data$county_name[[i]][length(crime_data$county_name[[i]])] == 'County,' | crime_data$county_name[[i]][length(crime_data$county_name[[i]])] == 'Borough,' |crime_data$county_name[[i]][length(crime_data$county_name[[i]])] == 'Parish,' ){
    crime_data$county_name[[i]] = crime_data$county_name[[i]][-length(crime_data$county_name[[i]])]}
  if (crime_data$county_name[[i]][length(crime_data$county_name[[i]])] == 'city,' | crime_data$county_name[[i]][length(crime_data$county_name[[i]])] == 'City,'){
    crime_data$county_name[[i]][length(crime_data$county_name[[i]])] = 'City'
  }
}

for (i in 1:3136){
  if (length(crime_data$county_name[[i]]) == 2) {
    crime_data$county_name[[i]] = paste(crime_data$county_name[[i]][1],crime_data$county_name[[i]][2])}
  if (length(crime_data$county_name[[i]]) == 3) {
    crime_data$county_name[[i]] = paste(crime_data$county_name[[i]][1],crime_data$county_name[[i]][2],crime_data$county_name[[i]][3])}
  if (length(crime_data$county_name[[i]]) == 4) {
    crime_data$county_name[[i]] = paste(crime_data$county_name[[i]][1],crime_data$county_name[[i]][2],crime_data$county_name[[i]][3],crime_data$county_name[[i]][4])}
}
crime_data$county_name = unlist(crime_data$county_name)
colnames(crime_data)[1] = 'County'

price_2017_5 = merge(price_2017_4,crime_data,by=c('County','State'))

crime_lost = price_2017_4[price_2017_4$County %ni% price_2017_5$County,]

crime_data$County[60] = crime_lost$County[1]
crime_data$County[1588] = crime_lost$County[3]

price_2017_5 = left_join(price_2017_4,crime_data,by=c('County','State'))


##tax rate data
setwd("TAXRATES_ZIP5")
filenames = list.files(pattern = "*.csv")
tax_rate_data = read.csv(filenames[1])
for(i in 2:length(filenames)){
  temp = read.csv(filenames[i])
  tax_rate_data = rbind(tax_rate_data, temp)
}

zip_county_lookup$COUNTYNAME= strsplit(zip_county_lookup$COUNTYNAME, " ") 
for (i in 1:53962){
  if (length(zip_county_lookup$COUNTYNAME[[i]])>1){
    if (zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'County' | zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'Borough' |zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'Parish' |zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'Municipality'|zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'Municipio'|zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'Island'){
      zip_county_lookup$COUNTYNAME[[i]] = zip_county_lookup$COUNTYNAME[[i]][-length(zip_county_lookup$COUNTYNAME[[i]])]}
    if (zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'city' | zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] == 'City'){
      zip_county_lookup$COUNTYNAME[[i]][length(zip_county_lookup$COUNTYNAME[[i]])] = 'City'
    }}
}

for (i in 1:53962){
  if (length(zip_county_lookup$COUNTYNAME[[i]]) == 2) {
    zip_county_lookup$COUNTYNAME[[i]] = paste(zip_county_lookup$COUNTYNAME[[i]][1],zip_county_lookup$COUNTYNAME[[i]][2])}
  if (length(zip_county_lookup$COUNTYNAME[[i]]) == 3) {
    zip_county_lookup$COUNTYNAME[[i]] = paste(zip_county_lookup$COUNTYNAME[[i]][1],zip_county_lookup$COUNTYNAME[[i]][2],zip_county_lookup$COUNTYNAME[[i]][3])}
  if (length(zip_county_lookup$COUNTYNAME[[i]]) == 4) {
    zip_county_lookup$COUNTYNAME[[i]] = paste(zip_county_lookup$COUNTYNAME[[i]][1],zip_county_lookup$COUNTYNAME[[i]][2],zip_county_lookup$COUNTYNAME[[i]][3],zip_county_lookup$COUNTYNAMEe[[i]][4])}
  if (length(zip_county_lookup$COUNTYNAME[[i]]) == 5) {
    zip_county_lookup$COUNTYNAME[[i]] = paste(zip_county_lookup$COUNTYNAME[[i]][1],zip_county_lookup$COUNTYNAME[[i]][2],zip_county_lookup$COUNTYNAME[[i]][3],zip_county_lookup$COUNTYNAMEe[[i]][4],zip_county_lookup$COUNTYNAMEe[[i]][5])}
}
zip_county_lookup$COUNTYNAME = unlist(zip_county_lookup$COUNTYNAME) 
zip_county_lookup = zip_county_lookup %>%
  dplyr::select(ZIP,STATE,COUNTYNAME,CITY)

tax_rate_data_final = left_join(tax_rate_data,zip_county_lookup,by=c("ZipCode"="ZIP"))
colnames(tax_rate_data_final)[11] = 'County'
tax_rate_data_final2 = tax_rate_data_final[!duplicated(tax_rate_data_final[,10:11]), ]
price_2017_6 = left_join(price_2017_5,tax_rate_data_final2,by=c('County','State'))


## gasoline price data

url = "https://gasprices.aaa.com/state-gas-price-averages/"
webpage = read_html(url)
states = html_text(html_nodes(webpage,'td:nth-child(1)'))
states = trimws(states)
regular = trimws(html_text(html_nodes(webpage, "td:nth-child(2)")))
midgrades = trimws(html_text(html_nodes(webpage, "td:nth-child(3)")))
premium = trimws(html_text(html_nodes(webpage, "td:nth-child(4)")))
diesel = trimws(html_text(html_nodes(webpage, "td:nth-child(5)")))

gasoline_data = data.frame(State=states, Regular=regular, MidGrade=midgrades, Premium=premium, Diesel=diesel)

gasoline_data$State = state.abb[match(gasoline_data$State,state.name)]
price_2017_7 = left_join(price_2017_6,gasoline_data,by='State')




## Number of colleges

filename = "IPEDS Data Center.html"
setwd("/Users/ricky/Desktop/Final-Project/")
schools = read_html(filename) %>%
    html_node("table.idc_gridview") %>%
    html_table() %>%
    select(-X1) %>% .[-1,]

colnames(schools) <- c("school", "city", "state")

## reference: https://nces.ed.gov/ipeds/datacenter/InstitutionProfile.aspx

cities <- read.csv("uscitiesv1.4.csv", 
                   stringsAsFactors=FALSE) %>%
    select(city, county_name, state_id)

colleges <- sqldf(
    "SELECT s.school, 
    s.city, 
    c.county_name AS County,
    s.state
    FROM schools AS s
    LEFT JOIN cities AS c
    ON s.city = c.city 
    AND s.state = c.state_id"
) %>% 
    group_by(County,state) %>%
    summarise(college_num = n())

colnames(colleges)[2] = 'State'
price_2017_8 = left_join(price_2017_7, colleges, by=c('County','State')) %>%
    mutate(college_num = ifelse(is.na(college_num), 0, college_num))

## read traffic, unemployment rate. etc data
epidata <- read_csv("EQIDATA_ALL_DOMAINS_2014MARCH11.CSV") %>% 
  select(county_name,state,hwyprop,ryprop,
         pct_pub_transport_log,fatal_rate_log,
         pct_pers_lt_pov,pct_unemp) %>% 
  rename(County=county_name,State=state)%>% 
  mutate(County=str_replace_all(County,"County","") %>% 
           str_trim(side = "both"))

##merge 
price_2017_9 =left_join(price_2017_8, epidata,by=c('County','State'))%>%
  mutate_all(funs(replace(., is.na(.), 0)))


#shopping mall
zip_to_city_lookup = read_csv("ZIP-COUNTY-FIPS_2018-03.csv")


#alabama 
url_al<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Alabama"

alabama = read_html(url_al) %>%
  html_nodes("table.wikitable.sortable") %>%
  html_table() 

al_data<-data_frame(
  name = alabama[[1]][[1]] %>% as.character(.),
  location=alabama[[1]][[2]] %>% as.character(.)
) %>% mutate('STATE' = 'AL')


#california

url_ca<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_California"
pageca<-read_html(url_ca)

ca_data<-data_frame(
  name=html_nodes(pageca,"li") %>% html_text() 
)
ca_data<-ca_data[1:181,]

ca_data <-ca_data %>% separate(name, c("name", "location"), "-",remove=TRUE)%>%
  separate(location, c("location", "state"), ",",remove=TRUE, extra = "merge") %>% 
  select(name, location) %>%
  separate(location, c("location", "other"), "[(]",remove=TRUE, extra = "merge") %>% 
  select(name, location)  %>%
  separate(name, c("name", "number"), "[.)]",remove=TRUE, extra = "merge") %>% 
  mutate('STATE' = 'CA') %>% select(name,location,STATE)


#maryland

url_md<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Maryland"
pagemd<-read_html(url_md)

md_data<-data_frame(
  name=html_nodes(pagemd,"li") %>% html_text() 
)
md_data<-md_data[1:34,]

md_data <-md_data %>% 
  separate(name, c("name", "location"), "-",remove=TRUE) %>% 
  mutate('STATE' = 'MD')



#michigan
url_mi<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Michigan"

michigan = read_html(url_mi) %>%
  html_nodes("table.wikitable.sortable") %>% 
  html_table() %>% 
  flatten()


mi_data=data.frame(
  name=c(michigan[[1]] %>% as.character(.),michigan[[8]] %>% as.character(.),
         michigan[[15]] %>% as.character(.),michigan[[22]] %>% as.character(.),
         michigan[[29]] %>% as.character(.),michigan[[36]] %>% as.character(.)
  ),
  location=c(michigan[[2]] %>% as.character(.),michigan[[9]] %>% as.character(.),
             michigan[[16]] %>% as.character(.),michigan[[23]] %>% as.character(.),
             michigan[[30]] %>% as.character(.),michigan[[37]] %>% as.character(.)
  ))

mi_data <-mi_data %>% separate(location, c("location", "latitude"), "4",remove=TRUE, extra = "merge") %>% 
  select(name, location) %>% 
  separate(location, c("location", "ownership"), "-",remove=TRUE, extra = "merge")%>%
  mutate(location = coalesce(location,ownership)) %>%
  select(name, location) %>% 
  mutate('STATE' = 'MI')


#new jersey
url_nj<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_New_Jersey"

newjersey = read_html(url_nj) %>%
  html_nodes("table.wikitable.sortable") %>%
  html_table() 

nj_data<-data_frame(
  name = newjersey[[1]][[1]] %>% as.character(.),
  location=newjersey[[1]][[2]] %>% as.character(.)
)

nj_data <-nj_data %>% separate(location, c("location", "other"), "[(]",remove=TRUE, extra = "merge") %>% 
  select(name, location) %>% 
  separate(name, c("name", "other"), "[(]",remove=TRUE, extra = "merge")%>%
  select(name, location) %>%
  separate(location, c("location", "ownership"), "-",remove=TRUE, extra = "merge")%>%
  select(name, location) %>% 
  mutate('STATE' = 'NJ')


#oregon
url_or<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Oregon"

oregon = read_html(url_or) %>%
  html_nodes("table.wikitable.sortable") %>% 
  html_table() %>% 
  flatten()


or_data=data.frame(
  name=c(oregon[[1]] %>% as.character(.),oregon[[7]] %>% as.character(.),
         oregon[[12]] %>% as.character(.),oregon[[17]] %>% as.character(.)
  ),
  location=c(oregon[[2]] %>% as.character(.),oregon[[8]] %>% as.character(.),
             oregon[[13]] %>% as.character(.),oregon[[18]] %>% as.character(.)
  )) %>% 
  mutate('STATE' = 'OR')

#pennsylania
url_pa1<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Pennsylvania"

pennsylania1 = read_html(url_pa1) %>%
  html_nodes("table.wikitable.sortable") %>%
  html_table() 

pa1_data<-data_frame(
  name = pennsylania1[[1]][[1]] %>% as.character(.),
  location=pennsylania1[[1]][[2]] %>% as.character(.)
)

pa1_data <-pa1_data %>% separate(location, c("location", "other"), "[.]",remove=TRUE, extra = "merge") %>% 
  select(name, location) %>% 
  separate(location, c("location", "ownership"), "-",remove=TRUE, extra = "merge")%>%
  mutate(location = coalesce(location,ownership)) %>%
  select(name, location) %>% 
  mutate('STATE' = 'PA')

url_pa2<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_the_Lehigh_Valley"

pennsylania2 = read_html(url_pa2) %>%
  html_nodes("table.wikitable") %>%
  html_table() %>%
  .[[1]]

pennsylania2<-pennsylania2[,1:2]
colnames(pennsylania2)<-c("name","location")
pa2_data<- pennsylania2 %>% separate(location, c("location", "other"), ",",remove=TRUE, extra = "merge") %>% 
  select(name, location) %>% 
  mutate('STATE' = 'PA')
pa_data<-rbind(pa1_data,pa2_data)


#texas
url_tx<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Texas"

dallas = read_html(url_tx) %>%
  html_nodes("table.wikitable.sortable") %>%
  .[[1]] %>% 
  html_table(fill=TRUE) 
dallas<-dallas[1:22,1:2]
colnames(dallas)<-c("name","location")

dallas<- dallas %>% separate(location, c("location", "other"), ",",remove=TRUE, extra = "merge") %>% 
  select(name, location) %>% 
  separate(name, c("name", "other"), "[(]",remove=TRUE, extra = "merge") %>% 
  select(name, location) 


houston = read_html(url_tx) %>%
  html_nodes("table.wikitable.sortable") %>%
  .[[2]] %>% 
  html_table(fill=TRUE) 
houston<-houston[,1:2]
colnames(houston)<-c("name","location")
houston<- houston %>% separate(location, c("location", "other"), ",",remove=TRUE, extra = "merge") %>% 
  select(name, location) %>% 
  separate(name, c("name", "other"), "[(]",remove=TRUE, extra = "merge") %>% 
  select(name, location)

san = read_html(url_tx) %>%
  html_nodes("table.wikitable.sortable") %>%
  .[[3]] %>% 
  html_table(fill=TRUE) 
san<-san[,1:2]
colnames(san)<-c("name","location")

pagetx<-read_html(url_tx)

texas_data<-data_frame(
  name=html_nodes(pagetx,"li") %>% html_text() 
)

texas_data<-texas_data[15:50,]

texas_data <-texas_data %>% separate(name, c("name", "location"), " - ",remove=TRUE)%>%
  separate(location, c("location", "other"), "[(]",remove=TRUE, extra = "merge") %>% 
  select(name, location)

tx_data<-rbind(dallas,houston,san,texas_data) %>% 
  mutate('STATE' = 'TX')

#other states 

url<-"https://en.wikipedia.org/wiki/List_of_shopping_malls_in_the_United_States"
page<-read_html(url)

st_data<-data_frame(
  name=html_nodes(page,"li") %>% html_text() 
)
st_data<-st_data[59:879,]

st_data <-st_data %>% separate(name, c("name", "location1"), " . ",remove=TRUE)%>%
  separate(name, c("name", "location2"), "–",remove=TRUE, extra = "merge") %>% 
  mutate(location = coalesce(location1,location2)) %>%
  select(name, location)%>%
  separate(name, c("name", "other"), "[(]",remove=TRUE)%>%
  select(name, location)%>%
  separate(location, c("location", "other"), "[(]",remove=TRUE)%>%
  select(name, location)

shooping_mall_num = read.xlsx("shooping_mall_num.xlsx",1)
shopping = shooping_mall_num[rep(row.names(shooping_mall_num), shooping_mall_num$Number.of.Malls), 1] %>%
  as.data.frame() %>% .[1:821,]
st_data = st_data %>%
  mutate(STATE = shopping)

shopping_mall_data = bind_rows(al_data,ca_data,md_data,mi_data,nj_data,or_data,pa_data,tx_data,st_data)

shopping_mall_data$location = tolower(shopping_mall_data$location)
colnames(shopping_mall_data)[2] = 'CITY'
shopping_mall_data$CITY = str_trim(shopping_mall_data$CITY,"left")
zip_county_lookup$CITY = tolower(zip_county_lookup$CITY)
shopping_mall_data_final = merge(shopping_mall_data,zip_county_lookup,by=c("CITY","STATE")) %>%
  select(name,CITY,STATE,COUNTYNAME) %>% unique()

shopping_mall_data_final = shopping_mall_data_final %>%
  group_by(COUNTYNAME,STATE) %>%
  summarise(Count = length(COUNTYNAME))
colnames(shopping_mall_data_final)=c("County","State","shopping_mall_count")

price_2017_10 = left_join(price_2017_9,shopping_mall_data_final,by=c("County","State"))
