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
-   [Descriptive Information](#Descriptive-Information)  
    -   [Tidy Data](#Tidy-Data)
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
  select(country, electiondate, electionid_ext, id, id2, party1, party2, sum_difs, rile_difs) %>%
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
```


Descriptive Information
====

Tidy Data
-------

``` r
# Mutate data
df <-  df %>%
  mutate(mobnews = round((mediagebruik_4 + mediagebruik_5 + mediagebruik_6 +
                           mediagebruik_7 + mediagebruik_8 + mediagebruik_9)/6, digits = 0),
         tradnews = round((mediagebruik_1 + mediagebruik_2 + mediagebruik_3 +
                           mediagebruik_9)/4, digits = 0),
         hs = round((ugt_1 + ugt_2 + ugt_3 + ugt_4)/4, digits = 0),
         surv = round((ugt_5 + ugt_6 + ugt_7 + ugt_8  + ugt_9  + ugt_10 + ugt_11)/7,
                      digits = 0),
         esc = round((ugt_12 + ugt_13 + ugt_14 + ugt_15 + ugt_16)/5, digits = 0),
         pt = round((ugt_17 + ugt_18 + ugt_19 + ugt_20 + ugt_21)/5, digits = 0),
         ent = round((ugt_22 + ugt_23)/2, digits = 0),
         social = round((ugt_24 + ugt_28 + ugt_29 + ugt_30)/3, digits = 0),
         nfm = round((nfm_1 + nfm_2 + nfm_3 + nfm_4 + nfm_5 + nfm_6)/6, digits = 0),
         corona = round((KC1 + KC2 + KC3 + KC4)/4, digits = 1),
         trust_online = round((trust_media_4 + trust_media_5 + trust_media_6 + trust_media_7 +
                                     trust_media_8 + trust_media_9)/7,digits = 0),
         trust_trad = round((trust_media_1 + trust_media_2  + trust_media_3 +
                                    trust_media_9)/4, digits = 0),
         polknow = round((pk1 + pol_ken2 + pol_ken3)/3, digits = 2),
         polint = round((pol_efficacy_1 + pol_efficacy_2 + pol_efficacy_5)/3, digits = 0),
         polsoph = round((pk1 + pol_ken2 + pol_ken3 + pol_efficacy_1 + pol_efficacy_2 + pol_efficacy_5)/5,
                       digits = 0),
         polpart = round((elect_part1 + elect_part2)/2, digits = 0),
         vote = recode(elect_part3,
                       `80` = "CDA",
                       `81` = "CU",
                       `82` = "D66",
                       `83` = "GL",
                       `84` = "PvdA",
                       `85` = "PvdD",
                       `86` = "PVV",
                       `87` = "SGP",
                       `88` = "SP",
                       `89` = "VVD",
                       `90` = "DENK",
                       `91` = "Vrijzinnige Partij",
                       `92` = "Nieuwe Wegen",
                       `93` = "FvD",
                       `94` = "missing", `95` = "missing", `96` = "missing", `97` = "missing"),
         vote = na_if(vote, "missing"),
         DK1 = str_to_lower(DK1, locale = "nl"),
         DK1 = ifelse(DK1 == "2", 1,
               ifelse(DK1 == "2e", 1,
               ifelse(DK1 == "2de", 1,
               ifelse(DK1 == "2de plaats", 1,
               ifelse(DK1 == "2e plaats",1,
               ifelse(DK1 == "de tweede", 1,
               ifelse(DK1 == "de tweede plaats", 1,
               ifelse(DK1 == "in de tweede plaats", 1,
               ifelse(DK1 == "op de tweede plaats", 1,
               ifelse(DK1 == "plek 2", 1,
               ifelse(DK1 == "tweede", 1, 0))))))))))),
         DK2 = ifelse(DK2 == "8", 1,
               ifelse(DK2 == "8 schapen", 1,
               ifelse(DK2 == "8 schapwn", 1,
               ifelse(DK2 == "Acht", 1, 0)))),
         DK3 = str_to_lower(DK3),
         DK3 = ifelse(DK3 == "emily", 1,
               ifelse(DK3 == "emilu", 1,
               ifelse(DK3 == "emily's", 1,
               ifelse(DK3 == "emily s", 1, 0)))),
         DK4 = str_to_lower(DK4),
         DK4 = ifelse(DK4 == "0", 1,
               ifelse(DK4 == "0 als het een gat is waar geen vuil in zit", 1,
               ifelse(DK4 == "een gat is leeg", 1,
               ifelse(DK4 == "gat is leeg, dus geen vuil", 1,
               ifelse(DK4 == "geen vuil, want het is een gat", 1,
               ifelse(DK4 == "geen, want het is een gat", 1,
               ifelse(DK4 == "nee", 1,
               ifelse(DK4 == "niets", 1,
               ifelse(DK4 == "nul", 1, 0))))))))),
         DK5 = ifelse(DK5 == ".05", 1,
               ifelse(DK5 == "£0,05", 1,
               ifelse(DK5 == "€0,05", 1,
               ifelse(DK5 == "€0.05", 1,
               ifelse(DK5 == "0,05", 1,
               ifelse(DK5 == " 0,05 aangezien ik de koers van euro 1,10 naar dollar niet weet kan ik dit niet met zekerheid zeggen", 1,
               ifelse(DK5 == "0,05 dollar", 1,
               ifelse(DK5 == "0,05 eurocent", 1,
               ifelse(DK5 == "0.05", 1,
               ifelse(DK5 == "5 cent", 1,
               ifelse(DK5 == "5 dollar cent", 1,
               ifelse(DK5 == "Ik ken de wisselkoers van Euro naar Dollar en vice versa op het moment niet. Maar er vanuit gaande dat er maar met een soort valuta mag worden gerekend dan kost de bal: 5 cent.",1,
               0)))))))))))),
         DK6 = str_to_lower(DK6),
         DK6 = ifelse(DK6 == "5 min", 1,
               ifelse(DK6 == "5 min.", 1,
               ifelse(DK6 == "5 minuten", 1,
               ifelse(DK6=="5min", 1,
               ifelse(DK6=="5minuten", 1,
               ifelse(DK6=="vijf", 1, 0)))))),
         DK7 = str_to_lower(DK7),
         DK7 = str_replace(df$DK7, "47", "1"),
         DK7 = ifelse(DK7 == "1", 1, 0),
         correct = (DK1 + DK2 + DK3 + DK4 + DK5 + DK6 + DK7),
         eo = Overconfidence - correct,
         gender = recode(Gender, `0` = "Male", `1` = "Female", `99` = "missing"),
         gender = na_if(gender, "missing"),
         gender = factor(gender, levels = c("Female", "Male")),
         age = Age,
         age = na_if(age, "812"),
         age = na_if(age, "a"),
         age = na_if(age, "asd"),
         age = na_if(age, "Dbn"),
         age = na_if(age, "eg"),
         age = na_if(age, "Gcc"),
         age = na_if(age, "i"),
         age = na_if(age, "Ja"),
         age = na_if(age, "nee"),
         age = na_if(age, "yes"),
         age = na_if(age, "Zek"),
         age = as.numeric(age),
         ResponseId = V1
  ) %>%
  filter(part == 1) %>%
  select(ResponseId, mobnews, tradnews, hs, surv, esc, pt,
         ent, social, nfm, corona, trust_online, trust_trad,
         polknow, polint, polsoph, polpart, vote, eo, gender,
         age) %>%
  filter(age >17)

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
