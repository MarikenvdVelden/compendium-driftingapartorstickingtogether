Prepare Integrated Data
================

- DESCRIPTION: Clean and construct measures according to manuscript (pp. XXX)
- CREATES: [Cleaned Data for Analysis](../../data/intermediate/cleaned_dyadic_data.csv)
- DEPENDS: [Raw Manifesto Project Data](../../data/raw/MPDataset_MPDS2019b.csv), [Raw ParlGov Cabinet Data](../../data/raw/view_cabinet.csv), [Raw European Representative Democracy Data](../../data/raw/Bergmann_Muller_Strom_Cabinets-Dataset.csv), [Raw Opinion Poll Data](../../data/raw/polldata_combined.RData)

Content
======

-   [Setup](#setup)
-   [Data](#data)
    -   [Manifesto Project Data](#Manifesto-Project-Data)
    -   [Opinion Poll Data](#Opinion-Poll-Data)
    -   [Cabinet Data](#Cabinet-Data)
    -   [Integrate Datasets](#Integrate-Datasets)
- [Tidy Data](#Tidy-Data)
    -   [Dependent Variable](#Dependent-Variable)
    -   [Independent Variables](#Independent-Variables)
    -   [Moderator](#Moderator)
    -   [Control Variables](#Control-Variables)
- [Missing Data](#Missing-Data)

Setup
=====

Load the required packages and source the auxiliary functions from `src/lib/functions.R`:

``` r
source("src/lib/functions.R")
```
Data
====
All raw data files are publicly accessible.


Manifesto Project Data
------------
```r
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
  select(country, electiondate, electionid_ext, id, id2, party1, party2, sum_difs,
         rile_party1, rile_party2, rile_difs) %>%
  drop_na(sum_difs)
```

Opinion Poll Data
------------
``` r
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
    tmp <- as.character(unique(polls$country)[j])
    polls$party[select] <- cmpcode[row.names(cmpcode)[i], tmp]
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
```

Cabinet Data
------------
Integrate [Raw ParlGov Cabinet Data](../../data/raw/view_cabinet.csv), [Raw European Representative Democracy Data](../../data/raw/Bergmann_Muller_Strom_Cabinets-Dataset.csv), and [Termination Cause Data](../../data/raw/imputing_termination_cause.csv)  

```r
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
         id3 = paste(party_name_short,country_name_short, sep="."))

#add cmp code to parlgov
ids <- read_csv(file = "data/raw/view_party.csv") %>%
  mutate(id3 = paste(party_name_short, country_name_short, sep="."))%>%
  select(id3, cmp_id = cmp) %>%
  drop_na(cmp_id)

parlgov <- left_join(parlgov, ids, by="id3") %>%
  drop_na(cmp_id)

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
         election_date = as.Date(election_date),
         end_date = paste(substr(start_date,1,4), substr(start_date,6,7), substr(start_date,9,10), sep="-"),
         id3 = paste(id3, cabinet_name, sep="."))

#Add termination causes
#source("imputing_termination_cause.R")
#termination_cause only goes to 1996, rest own coding, see csv "imputing_termination_cause" for coding decisions
erd <- read_csv(file = "data/raw/imputing_termination_cause.csv")

cabinet_names <- erd$cabinet_name
for(i in 1:length(cabinet_names)){
  cabinet_data$termination_cause[which(cabinet_data$cabinet_name==cabinet_names[i])] <- rep(erd$termination_cause[which(erd$cabinet_name==cabinet_names[i])], length(which(cabinet_data$cabinet_name==cabinet_names[i])))
}

cabinet_data <- slide(data = cabinet_data, Var = "cabinet_name", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "cabinet_name1")
cabinet_data <- slide(data = cabinet_data, Var = "caretaker", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "caretaker1")
cabinet_data <- slide(data = cabinet_data, Var = "cabinet_party", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "cabinet_party1")
cabinet_data <- slide(data = cabinet_data, Var = "prime_minister", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "prime_minister1")
cabinet_data <- slide(data = cabinet_data, Var = "seats", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "seats1")
cabinet_data <- slide(data = cabinet_data, Var = "election_seats_total", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "election_seats_total1")
cabinet_data <- slide(data = cabinet_data, Var = "termination_cause", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "termination_cause1")
cabinet_data <- slide(data = cabinet_data, Var = "no_cabinetparties", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "no_cabinetparties1")
cabinet_data <- slide(data = cabinet_data, Var = "govtype", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "govtype1")
cabinet_data <- slide(data = cabinet_data, Var = "end_date", TimeVar = "start_date", GroupVar = "cmp_id", NewVar = "end_date1", slideBy = 1)

cabinet_data <- cabinet_data %>%
  mutate(match_id = paste(election_date, cmp_id, sep="."),
         end_date = as.Date(end_date1),
         duration_months = round(as.numeric((end_date - start_date))/365*12,0)) %>%
  select(match_id, country_name = country_name_short, election_date, start_date, end_date, duration_months, cmp_id, party,
         cabinet_party=cabinet_party1, prime_minister=prime_minister1, cabinet_name=cabinet_name1,
         no_cabinetparties=no_cabinetparties1, caretaker=caretaker1, govtype=govtype1,
         termination_cause=termination_cause1, seats=seats1, tot_seats = election_seats_total1)
```

Integrate Datasets
------------
``` r
cmp <- left_join(x = cmp, y = polls, by = "id")
cmp <- cmp %>%
  select(country:electionid_ext, id2, party1:rile_difs, polls_party1 = mean_polls) %>%
  mutate(id = paste(country,substr(electiondate,1,4),party2, sep="-"))
cmp <- left_join(x = cmp, y = polls, by = "id")

cmp <- cmp %>%
  mutate(pcombi1 = paste0(party1, party2),
         pcombi2 = paste0(party2, party1),
         unique_pcombi = as.numeric(pcombi1==pcombi2)) %>%
  filter(unique_pcombi==0) %>%
  select(country:party2, pcombi=pcombi1, sum_difs:polls_party1, polls_party2 = mean_polls) %>%
  mutate(match_id = paste(electiondate, party1, sep="."))

df <- left_join(cmp, cabinet_data, by = "match_id") %>%
  mutate(match_id = paste(electiondate, party2, sep=".")) %>%
  select(match_id, country:electiondate, start_date,
         electionid_ext, party = party1, partner = party2, pcombi, sum_difs,
         rile_party = rile_party1, rile_partner=rile_party2, rile_difs, polls_party = polls_party1,
         polls_partner = polls_party2, cabinet_party, pm_party = prime_minister, seats_party = seats)

df <- left_join(df, cabinet_data, by = "match_id") %>%
  select(country:electiondate, start_date = start_date.x, end_date, electionid_ext,
         party = party.x, partner:polls_partner, cabinet_party = cabinet_party.x,
         cabinet_partner = cabinet_party.y, pm_party, pm_partner = prime_minister, seats_party,
         seats_partner = seats, tot_seats, cabinet_name:termination_cause, duration_months) %>%
  mutate(first_election = ifelse(electionid_ext=="11.17/09/1944",1,
                          ifelse(electionid_ext=="12.08/10/1945",1,
                          ifelse(electionid_ext=="13.30/10/1945",1,
                          ifelse(electionid_ext=="14.18/03/1945",1,
                          ifelse(electionid_ext=="21.17/02/1946",1,
                          ifelse(electionid_ext=="22.17/05/1946",1,
                          ifelse(electionid_ext=="41.14/08/1949",1,
                          ifelse(df$electionid_ext=="42.09/10/1949",1,
                          ifelse(electionid_ext=="53.04/02/1948",1,0))))))))),
         termination_cause = replace_na(termination_cause, 0),
         unique_id = paste(electionid_ext, pcombi, sep=".")) %>%
  filter(!duplicated(unique_id)) %>%
  select(-unique_id) %>%
  filter(first_election==0) %>%
  select(-first_election)
```


Tidy Data
====

``` r
# Mutate data
df <- slide(data = df, Var = "sum_difs", TimeVar = "electiondate", GroupVar = "pcombi", NewVar = "l_sum_difs")
df <- df %>%
  mutate(pcombi = as.numeric(pcombi),
         year = substr(electiondate, 1,4),
         year = as.numeric(year),
         d_issue_distance = sum_difs-l_sum_difs,
         cabinet_pair = ifelse(cabinet_party + cabinet_partner==2,1,0),
         termination_cause2 = ifelse(cabinet_pair==1 & termination_cause==0, 1, #Recode termination cause so that no coalition dyads do not have a termination cause
                              ifelse(cabinet_pair==1 & termination_cause==1, 2,
                              ifelse(cabinet_pair==1 & termination_cause==2, 3, 0))),
         termination_cause3 = recode(termination_cause2,
                                     `2` = 0,# 0 = no coalition dyad, 1= end of term, 2 = conflict, 3= voluntary early elections
                                     `0` = 3,
                                     `3` = 2,
                                     `1` = 1),
         conflict = ifelse(termination_cause3==2, 1, #where conflict is ref. category
                    ifelse(termination_cause3==3, 2, termination_cause3)),#and voluntary early & regular end of term are 1 category
         popularity = log((polls_party + polls_partner)/(seats_party + seats_partner)),
         rile = ifelse(rile_party<0 & rile_partner<0,0,
                ifelse(rile_party>0 & rile_partner>0,0,1))) %>%#rile:  being on other side = 1, same side = 0
  add_column(rile_mean = 0,
             rile_median = 0)

#calculate mean and median per election cycle
cutoffs_rile <- df %>%
  group_by(electionid_ext) %>%
  summarise(rile_mean = mean(rile_party + rile_partner, na.rm = T)/2,
            rile_median = median(rile_party + rile_partner, na.rm = T)/2)

for(i in 1:dim(cutoffs_rile)[1]){
  df$rile_mean[df$electionid_ext==cutoffs_rile$electionid_ext[i]] <- cutoffs_rile$rile_mean[i]
  df$rile_median[df$electionid_ext==cutoffs_rile$electionid_ext[i]] <- cutoffs_rile$rile_median[i]
}

df <- df %>%
  mutate(rile_mean = ifelse(rile_party<rile_mean & rile_partner<rile_mean,0,
                     ifelse(rile_party>rile_mean & rile_partner>rile_mean,0,1)),
         rile_median = ifelse(rile_party<rile_median & rile_partner<rile_median,0,
                       ifelse(rile_party>rile_median & rile_partner>rile_median,0,1)))

#calculate experience
tmp <- tibble(pairs=unique(df$pcombi),
              select = 0)
for(i in 1:length(pairs)){
  tmp$select[i] <- ifelse(length(which(df$pcombi==tmp$pairs[i] & df$cabinet_pair==1))>0,1,0)
}
select <- tmp$pairs[which(tmp$select==1)]
tmp <- df[,c("duration_months", "pcombi", "cabinet_name", "start_date")]
tmp <- slide(tmp, Var = "duration_months", TimeVar = "start_date", GroupVar = "pcombi", NewVar = "l_duration")
tmp <- tmp %>%
  mutate(l_duration = replace_na(l_duration,0),
    experience = duration_months + l_duration,
    match_id = paste(cabinet_name, pcombi, sep=".")) %>%
  select(match_id, experience)

df <- df%>%
  mutate(match_id = paste(cabinet_name, pcombi, sep=".")) %>%
  left_join(y=tmp, by="match_id") %>%
  select(country:rile_median,
         experience) %>%
  mutate(experience = replace_na(experience, 0))

#add controls
controls <- read_csv(file = "data/raw/controls.csv") %>%
  mutate(id = paste(counrty, year, sep = "."),
         type_conflict = replace_na(type_conflict,0)) %>%
  select(enps:id)

df <- df %>%
  mutate(id = paste(country, year, sep=".")) %>%
  left_join(y = controls, by = "id") %>%
  mutate(year = ifelse(electionid_ext=="13.22/09/1953", 1954,# For tscs reasons, double election years can't be dealed with
                ifelse(electionid_ext=="53.24/11/1982", 1983,# (Denemarken 1953, iceland 1969 en Ierland 1982)
                ifelse(electionid_ext=="15.25/10/1959", 1960, year))),# therefore 2nd election is coded as year later
         pec_bergman = ifelse(country=="Norway" & year == 1965, 1,
                       ifelse(country=="Norway" & year == 1981, 1,
                       ifelse(country=="Norway" & year == 1989, 1,
                       ifelse(country=="Norway" & year == 2001, 1,
                       ifelse(country=="Sweden" & year == 2006, 1,
                       ifelse(country=="Ireland" & year == 2007, 1, 0)))))))#Control for pre-coalition agreements Bergman

#Control for pre-coalition agreements Golder's BJPS 2006
pec <- read_csv(file = "data/raw/PEC_Golder2006.csv") %>%
  mutate(id = paste(country, year, sep=".")) %>%
  select(pec_golder = pec, id)

df <- df %>%
  left_join(y = pec, by = "id") %>%
  select(country:electionid_ext, year, party:pcombi, sum_difs, l_sum_difs,
         d_issue_distance, seats_party, seats_partner,tot_seats, polls_party,
         polls_partner, popularity, cabinet_name:termination_cause,
         termination_cause2, termination_cause3, conflict, cabinet_pair, duration_months,experience,
         rile_party:rile_difs, rile:rile_median, enps:pec_golder)

#Check Correlations
# as a default this function outputs a correlation matrix plot
df%>%
  select(mobnews, tradnews, hs, surv, esc, pt,
         ent, social, nfm, corona, trust_online, trust_trad,
         polsoph, polpart) %>%
  ggstatsplot::ggcorrmat(
    type = "robust", # correlation method
    sig.level = 0.05, # threshold of significance
    p.adjust.method = "holm", # p-value adjustment method for multiple comparisons
    cor.vars = c(mobnews:polpart), # a range of variables can be selected
    cor.vars.names = c(
      "Mobile News Usage", # variable names
      "Traditional News Usage", # variable names
      "Habit Strength",
      "Surveillance",
      "Escapism",
      "Entertainment",
      "Passing Time",
      "Social",
      "News Finds Me Perception",
      "Corona Knowledge",
      "Trust Online Media",
      "Trust Traditional Media",
      "Political Sophistication",
      "Political Participation"
    ),
    matrix.type = "upper", # type of visualization matrix
    colors = c("#B2182B", "white", "darkgreen"),
    title = "Correlalogram for Variables under Study",
    lab_size = 2
  )

```
![Figure](../../report/figures/Corrplot_Pollfish.png)


Dependent Variables
 -------
* News Finds Me Perception (**H1**)
* Political Knowledge (**H2**)
* Political Interest (**H2**)
* Electoral Participation (**H3**)

 ``` r
##  Dependent Variables
rbind(tibble(freq = round(table(df$nfm)/dim(df)[1],2),
             values = 1:7,
             id = "News Finds Me Perception \n Mean: 3.72, Standard Deviation: 1.27"),
      tibble(freq = round(table(df$polknow)/dim(df)[1],2),
             values = c(0,0.33, 0.67,1),
             id = "Political Knowledge \n Mean: 0.30, Standard Deviation: 0.32"),
      tibble(freq = round(table(df$polint)/dim(df)[1],2),
             values = 1:7,
             id = "Political Interest \n Mean: 3.99, Standard Deviation: 1.23"),
      tibble(freq = round(table(df$polpart)/dim(df)[1],2),
             values = 1:5,
             id = "Political Participation \n Mean: 3.86, Standard Deviation: 1.23")) %>%
  ggplot(aes(x = values, y = freq)) +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ id, ncol = 3, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y="", title = "Dependent Variables")
```

![Figure](../../report/figures/Distributions_DV_PollFish.png)

Independent Variables
-------
- Gratifications of the News (**RQ1**)
	- Entertainment
	- Escapisme
	- Habit Strength
	- Passing Time
	- Surveillance
- Mobile News Consumption (**H1**)
- Knowledge about Corona (**RQ2**)
- Political Sophistication (**H3**)

 ``` r
##  Independent Variables
rbind(tibble(freq = round(table(df$hs)/dim(df)[1],2),
             values = 1:7,
             id = "Habit Strength \n Mean: 3.98, Standard Deviation: 1.60"),
      tibble(freq = round(table(df$surv)/dim(df)[1],2),
             values = 1:7,
             id = "Surveillance \n Mean: 4.62, Standard Deviation: 1.45"),
      tibble(freq = round(table(df$esc)/dim(df)[1],2),
             values = 1:7,
             id = "Escapism \n Mean: 3.24, Standard Deviation: 1.63"),
      tibble(freq = round(table(df$pt)/dim(df)[1],2),
             values = 1:7,
             id = "Passing Time \n Mean: 3.51, Standard Deviation: 1.63"),
      tibble(freq = round(table(df$ent)/dim(df)[1],2),
             values = 1:7,
             id = "Entertainment \n Mean: 3.59, Standard Deviation: 1.73")) %>%
  ggplot(aes(x = values, y = freq)) +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ id, ncol = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "", y="", title = "Independent Variable: Gratifications of the News")

  rbind(tibble(freq = round(table(df$mobnews)/dim(df)[1],2),
             values = 0:7,
             id = "Mobile News Usage \n Mean: 3.29, Standard Deviation: 1.63"),
      tibble(freq = round(table(df$corona)/dim(df)[1],2),
             values = seq(from = 0, to = 0.9, by = 0.1),
             id = "Knowledge about Corona \n Mean: 0.36, Standard Deviation: 0.15"),
      tibble(freq = round(table(df$polsoph)/dim(df)[1],2),
             values = 1:5,
             id = "Political Sophistication \n Mean: 2.56, Standard Deviation: 0.78")) %>%
  ggplot(aes(x = values, y = freq)) +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ id, ncol = 3, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y="", title = "Independent Variables")

```

![Figure](../../report/figures/Distributions_IVugt_PollFish.png)
![Figure](../../report/figures/Distributions_IV_PollFish.png)

Control Variables
-------
- Trust in Media
- News Usage
- Political Efficacy
- Gender
- Age

 ``` r
# Controls
rbind(tibble(freq = round(table(df$trust_online)/dim(df)[1],2),
             values = 1:6,
             id = "Trust in Online Media \n Mean: 3.55, Standard Deviation: 0.99"),
      tibble(freq = round(table(df$tradnews)/dim(df)[1],2),
             values = 0:7,
             id = "Traditional News Usage \n Mean: 3.45, Standard Deviation: 1.69"),
      tibble(freq = round(table(df$trust_trad)/dim(df)[1],2),
             values = 1:7,
             id = "Trust in Traditional Media \n Mean: 4.40, Standard Deviation: 1.22"),
      tibble(freq = round(table(df$gender)/dim(df)[1],2),
             values = levels(df$gender),
             id = "Gender \n Median: Male"),
      tibble(freq = round(table(df$age_group)/dim(df)[1],2),
             values = c("20's", "30's", "40's", "50's", "60's", "70's"),
             id = "Age \n Mean: 31.86, Standard Deviation: 12.30"),
      tibble(freq = round(table(df$eo)/dim(df)[1],2),
             values = -2:7,
             id = "Epistemic Overconfidence \n Mean: 2.50, Standard Deviation: 1.91")) %>%
  ggplot(aes(x = values, y = freq)) +
  facet_wrap(~ id, ncol = 3, scales = "free") +
  geom_col(fill = "gray85", colour = "black") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y="", title = "Control Variables")
```
![Figure](../../report/figures/Distributions_Controls_PollFish.png)

Missing Data
====
We employ the following criteria:

- If 10\% or less of the values on the dimension are missing, then we re-code the missing values to the overall mean.
- If 11\% or more of the values on the dimension are missing, then we re-code the missing values to a constant (for instance 0) and include a dummy variable indicating whether the response on the covariate was missing or not.

```r
#Check Missing Values
#Check Missing Values
tibble(Covariate = c("UGT: Entertainment","UGT: Escapism", "UGT: Habit Strength", "UGT: Passing Time",
                     "UGT: Surveillance","UGT: Social", "News Finds Me Perception",
                     "Knowledge about Corona","Mobile News Usage", "Traditional News Usage",
                     "Political Sophistication", "Political Knowledge","Political Interest",
                     "Political Participation", "Voting Behavior", "Trust in Online Media",
                     "Trust in Traditional Media", "Epistemic Overconfidence",
                     "Age","Gender"),
       Percentage =c(round(sum(is.na(df$ent))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$esc))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$hs))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$pt))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$surv))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$social))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$nfm))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$corona))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$mobnews))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$tradnews))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$polsoph))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$polknow))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$polint))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$polpart))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$vote))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$trust_online))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$trust_trad))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$eo))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$age))/prod(dim(df)[1]),2),
                     round(sum(is.na(df$gender))/prod(dim(df)[1]),2)))
```

| Covariate 					| Percentage	|
|------------------------------------------------|---------------------|
| UGT: Entertainment            		| 0.01		|
| UGT: Escapism                  		| 0.01		|
| UGT: Habit Strength            		| 0.01		|
| UGT: Passing Time              		| 0.01  		|
| UGT: Surveillance              		| 0.01   		|
| UGT: Social					| 0.01		|
| News Finds Me Perception		| 0			|
| Knowledge about Corona		| 0			|
| Mobile News Usage			| 0			|
| Traditional News Usage			| 0 			|
| Political Sophistication			| 0			|
| Political Knowledge				| 0			|
| Political Interest				| 0			|
| Political Participation			| 0			|
| Voting Behavior				| 0.35		|
| Trust in Online Media			| 0			|
| Trust in Traditional Media		| 0			|
| Epistemic Overconfidence       	| 0.17		|
| Age                            			| 0			|
| Gender                         			| 0.02		|

 We recode the missing values of the variables `UGT: Entertainment`, `UGT: Escapism`, `UGT: Habit Strength`, `UGT: Passing Time`, `UGT: Surveillance`, `UGT: Social`, and `Gender`,  to the mean value of the respective variables.

 ```r
df <- df %>%
  select(-age_group) %>%
  mutate(ent = tidyr::replace_na(ent, round(mean(df$ent, na.rm=T),0)),
         esc = tidyr::replace_na(esc, round(mean(df$esc, na.rm=T),0)),
         hs = tidyr::replace_na(hs, round(mean(df$hs, na.rm=T),0)),
         pt = tidyr::replace_na(pt, round(mean(df$pt, na.rm=T),0)),
         surv = tidyr::replace_na(surv, round(mean(df$surv, na.rm=T),0)),
         social = tidyr::replace_na(social, round(mean(df$social, na.rm=T),0)),
         gender = tidyr::replace_na(gender, "Male"))
```
 We recode missing values of variable `Epistemic Overconfidence` to 0 and include the variable `Missing_eo` to the data, indicating whether the response on the covariate was missing (value of 1) or not (value of 0). For variable `Voting Behavior`, we recode missing values to "not mentioned" and include the variable `Missing_vote` to the data, indicating whether the response on the covariate was missing (value of 1) or not (value of 0).

```r
# Change missing values in variables where missings are >10%
df <- df %>%
  mutate(missing_eo = ifelse(is.na(eo), 1, 0),
         eo = tidyr::replace_na(eo, 0),
         missing_vote = ifelse(is.na(vote), 1, 0),
         vote = tidyr::replace_na(vote, "not mentioned"))
```

 Save data to `data/intermediate` and make public.

 ```r
 #save data
 write_csv(df, "../../data/intermediate/cleaned_PollFish.csv")
 ```
