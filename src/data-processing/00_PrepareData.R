#Prepare Integrated Data

#Set-Up
source("src/lib/functions.R")

#Load & Tidy Comparative Manifesto Database 
cmp <- read_csv("data/raw/MPDataset_MPDS2019b.csv")

cmp <- cmp %>%
  filter(country == 42 | country == 21 | country == 41 | 
           country == 53 | country == 22 | country == 12 | country == 14 |
           country == 11 | country == 13 | country == 15 ) %>% #Select Countries under Study
  mutate( #Calculate Issue Positions
    issue1 = ((per401 + per402 + per407 + per414 + per702) - 
                (per404 + per405 + per406 + per409 + per412 + per413 + per415 + per701 + per403)),#pos economy
    issue2 = (per504 + per506) - (per505 + per507),#pos welfare
    issue3 = per108 - per110,#pos welfare
    issue4 = per607 - per608,#pos multiculturalism
    issue5 = ((per103 + per105 + per106 + per107) -
                (per104 + per109)),#pos international affairs
    issue6 = per101 - per102,#pos specialrelations
    issue7 = per203 - per204,#pos constitution
    issue8 = per301 - per302,#pos decentralization
    issue9 = (per601 + per603) - (per602 + per604),#pos traditional
    issue10 = per201 + per202,#sal democracy
    issue11 = per503 + per705 + per706,#sal groups
    issue12 = per303 + per304 + per305,#sal governance
    issue13 = per408 + per411 + per416,#sal economicgrowth
    issue14 = per502,#sal culture
    issue15 = per605,#sal law_order
    issue16 = per606,#sal harmony
    issue17 = per703,#sal farmers
    issue18 = per704,#sal middleclass
    issue19 = per416 + per501,#sal environment
    electionid = interaction(country, edate, drop=TRUE)
  ) %>%
  select(party, country, edate, electionid, rile, issue1, issue2, issue3, issue4, issue5, issue6, issue7, issue8, 
         issue9, issue10, issue11, issue12, issue13, issue14, issue15,
         issue16, issue17, issue18, issue19) #Select all relevant variables

cmp <- calculate_sum_difs(cmp)# Create overlap for each pair of parties 

cmp <- cmp %>%
  mutate(country = ifelse(substr(electionid_ext,1,2)=="11","Sweden",#Recode country variable
                   ifelse(substr(electionid_ext,1,2)=="12","Norway",
                   ifelse(substr(electionid_ext,1,2)=="13","Denmark",
                   ifelse(substr(electionid_ext,1,2)=="14","Finland",
                   ifelse(substr(electionid_ext,1,2)=="22","Netherlands",
                   ifelse(substr(electionid_ext,1,2)=="21","Belgium",
                   ifelse(substr(electionid_ext,1,2)=="41","Germany",
                   ifelse(substr(electionid_ext,1,2)=="42","Austria",
                   ifelse(substr(electionid_ext,1,2)=="53","Ireland",NA))))))))),
         electiondate = as.Date(str_sub(electionid_ext,4,13),format="%d/%m/%Y"),
         id2 = paste(electionid_ext, party1, party2, sep="."),
         party1=recode(party1,#Recode merger parties
                       `21914` = 21917,
                       `21221` = 21321,
                       `23111` = 23113,
                       `23112` = 23113,
                       `41112` = 41111,
                       `41113` = 41111,
                       `41223` = 41221,
                       `41222` = 41221,
                       `21221` = 21321),
         party2=recode(party2,#Recode merger parties
                       `21914` = 21917,
                       `21221` = 21321,
                       `23111` = 23113,
                       `23112` = 23113,
                       `41112` = 41111,
                       `41113` = 41111,
                       `41223` = 41221,
                       `41222` = 41221,
                       `21221` = 21321),
         id = paste(country,substr(electiondate,1,4),party1, sep="-")) %>%
  select(country, electiondate, electionid_ext, id, id2, party1, party2, sum_difs, rile_difs) %>%
  drop_na(sum_difs)

#Load & Tidy poll data from Van Der Velden (2015), Jennings and Wlezien (2014) and Askham-Christensen (2012) 
load("data/raw/polldata_combined.RData")

polls <- data %>%
  pivot_longer(cols = `party 1`:`party 42`,
               names_to = "party",
               values_to = "polls") %>%
  filter(country == "Austria" | country == "Belgium" | country == "Denmark" |
           country == "Finland" | country == "Germany" | country == "Ireland" |
           country == "Netherlands" | country == "Norway" | country == "Sweden") %>%
  mutate(year = as.character(year),
         month = as.character(month),
         month = ifelse(nchar(month)==1, paste0("0",month), month),
         date = paste("01", month, year, sep="/"),
         date = as.Date(date, format = "%d/%m/%Y"))

#add cmp code names to party
party <- row.names(cmpcode)
for(i in 1:length(party)){
  for(j in 1:length(unique(polls$country))){
    select <- which(polls$party==row.names(cmpcode)[i] & polls$country==unique(polls$country)[j])
    polls$party[select] <- cmpcode[row.names(cmpcode)[i],j] 
  }
} 

polls <- polls %>%
  mutate(party = as.numeric(party),
         party = recode(party,#Recode merger parties
                       `21914` = 21917,
                       `21221` = 21321,
                       `23111` = 23113,
                       `23112` = 23113,
                       `41112` = 41111,
                       `41113` = 41111,
                       `41223` = 41221,
                       `41222` = 41221,
                       `21221` = 21321),
         country_code = substr(party,1,2),
         id = paste(country, date, sep=".")) %>%
  add_column(event = NA) %>%
  drop_na(party) %>% 
  arrange(country_code)

#add election dates
id <- read_csv(file = "data/raw/election_id.csv")
for(i in 1:dim(id)[1]){
    if(i==1){
      start <- i
      }
    else{
      start <- max(which(polls$id==id$id[(i-1)]))+1
    }
    end <- max(which(polls$id==id$id[i]))
    polls$event[start:end] <- str_split(id$id[i], "\\.")[[1]][2]
}

polls <- polls %>%  
  drop_na(event) %>%
  mutate(event = as.Date(event),
         month_teller = abs(event-date),#add teller to select 6 months before election
         month_teller = round(as.numeric(month_teller)/365 *12,0),
         event = paste(country, event, sep = ".")) %>%
  add_column(mean_polls = 0) %>%
  drop_na(polls)

#Average polls of 6 months prior to election  
for(i in 1: length(unique(polls$event))){
  tmp <- unique(polls$event)
  party <- unique(polls$party[which(polls$event==tmp[i])])
  for(j in 1: length(party)){
    tmpp <- polls[which(polls$event==tmp[i] & polls$month_teller < 7 & polls$month_teller > 0 & polls$party==party[j]), "polls"]
    if(dim(tmpp)[1]>0){
    polls$mean_polls[which(polls$event==tmp[i])] <- round(sum(tmpp, na.rm=T)/(6 - length(which(is.na(tmpp)))),2)}
  }
}

polls <- polls %>%
  filter(id == event) %>%
  mutate(id = paste(country,year,party, sep="-")) %>%
  select(id, mean_polls)

#Load & tidy Parlgov Data and with Coalition Data Set of Bergmann et al.
parlgov <- read_csv(file = "data/raw/view_cabinet.csv")  %>%
  filter(country_name == "Austria" | country_name == "Belgium" | country_name == "Denmark" |
         country_name == "Finland" | country_name == "Germany" | country_name == "Ireland" |
         country_name == "Netherlands" | country_name == "Norway" | country_name == "Sweden") %>%
  select(country_name_short, election_date, start_date, cabinet_name, caretaker, cabinet_party, prime_minister, 
         seats, election_seats_total, party_name_short, party_name_english, party = party_id) %>%
  mutate(party = recode(party,#Recode merger parties
                        `1113` = 1029,
                        `1487` = 1029,
                        `255` = 772),
         id3 = paste(party_name_short,country_name_short, sep=".")) %>%
  add_column(cmp_id = 0)

ids <- read_csv(file = "data/raw/cmp_code_parlgov.csv") %>%
  drop_na(cmp_id)
for(i in 1:dim(ids)[1]){
  select <- which(parlgov$id3==ids$parlgov_id[i])
  parlgov$cmp_id[select] <- ids$cmp_id[i]
}

coalitiondata <- read_csv(file = "data/raw/Bergmann_Muller_Strom_Cabinets-Dataset.csv")  %>%
  filter(v001x==1 | v001x==2 | v001x==3 | v001x==4| v001x==6 | v001x==9 | 
         v001x==12|v001x==13 |v001x==16) %>%
  select(cabinet_name = v003x, termination_cause = v217y, no_cabinetparties = v051y, govtype = v058y2) %>%
    mutate(cabinet_name = as.character(cabinet_name),
           cabinet_name = recode(cabinet_name,
                               "Hansson" = "Hansson I",
                               "F\xe4lldin I" = "Falldin I",
                               "F\xe4lldin II" = "Falldin II",
                               "F\xe4lldin III" = "Falldin III",
                               "Bondevik" = "Bondevik I", 
                               "de Valera VI" = "Valera VI",
                               "de Valera VII" = "Valera VII", 
                               "de Valera VIII" = "Valera VIII",  
                               "FitzGerald II"="Fitzgerald II",                     
                               "Schroeder" = "Schroeder I",
                               "Kohl V"="Kohl IV",
                               "Kohl VI"="Kohl V",
                               "Juncker" = "Juncker I",
                               "Lipponen" = "Lipponen I",
                               "von Fieandt" = "Fieandt",
                               "T\xf6rngren" = "Torngren",
                               "Karjalainen Iia" = "Karjalainen II",
                               "Karjalainen Iib" = "Karjalainen III",
                               "Sukselainen Ib"= "Sukselainen II",   
                               "Sukselainen Ic"= "Sukselainen I",              
                               "Sukselainen IV"=  "Miettunen I",                 
                               "Holkeri a"="Holkeri I", 
                               "Holkeri b"="Holkeri II",            
                               "Aho a"="Aho I",                 
                               "Aho b"="Aho II",
                               "Paasikivi III"="Paasikivi II",
                               "Rasmussen IV" = "Rasmussen N IV",
                               "Rasmussen I" = "Rasmussen N I",
                               "Rasmussen III" = "Rasmussen N III",         
                               "Rasmussen II" = "Rasmussen N II",    
                               "Schl\xfcter I" = "Schluter I",
                               "Schl\xfcter II" = "Schluter II",
                               "Schl\xfcter III" = "Schluter III",
                               "Schl\xfcter IV" = "Schluter IV",
                               "Schl\xfcter V" = "Schluter V",
                               "Hansen" = "Hansen I",
                               "Spaak" = "Spaak I",
                               "Dehaenen I"="Dehaene I",
                               "Van den Boeynants I"="Vanden Boeynants I",                
                               "Van den Boeynants II"="Vanden Boeynants II",     
                               "Tindemans"="Tindemans I",             
                               "Eyskens" = "Eyskens G I", 
                               "Eyskens II" = "Eyskens G II",
                               "Eyskens III"="Eyskens G III",          
                               "Eyskens V"="Eyskens G V",            
                               "Eyskens IV"="Eyskens G IV", 
                               "M Eyskens" = "Eyskens M",
                               "Van Houtte" = "Houtte", 
                               "Klima" = "Klima I",   
                               "Vranizky V"="Vranitzky V"))

cabinet_data <- left_join(x = parlgov, y = coalitiondata, by="cabinet_name") %>%
  mutate(start_date = as.Date(start_date),
         election_date = as.Date(election_date)) %>%
  select(country_name_short:election_seats_total, party, cmp_id:govtype)

#Add termination causes
#source("imputing_termination_cause.R") 
#termination_cause only goes to 1996, rest own coding, see csv "imputing_termination_cause" for coding decisions
erd <- read_csv(file = "data/raw/imputing_termination_cause.csv")
cabinet_names <- erd$cabinet_name
for(i in 1:length(cabinet_names)){
  cabinet_data$termination_cause[which(cabinet_data$cabinet_name==cabinet_names[i])] <- rep(erd$termination_cause[which(erd$cabinet_name==cabinet_names[i])], length(which(cabinet_data$cabinet_name==cabinet_names[i])))
}

#Integrate Data Sets 
cmp <- left_join(x = cmp, y = polls, by = "id")
cmp <- cmp %>%
  select(country, electiondate, electionid_ext, id2, party1, party2, sum_difs, rile_difs,
         polls_party1 = mean_polls) %>%
  mutate(id = paste(country,substr(electiondate,1,4),party2, sep="-"))
cmp <- left_join(x = cmp, y = polls, by = "id") 
  
cmp <- cmp %>%
  select(country, electiondate, electionid_ext, id2, party1, party2, sum_difs, rile_difs,
         polls_party1, polls_party2 = mean_polls)

