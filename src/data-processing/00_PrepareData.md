Prepare Integrated Data
================

- DESCRIPTION: Clean and construct measures according to manuscript (pp. XXX)
- CREATES: [Cleaned Data for Analysis](../../data/intermediate/cleaned_dyadic_data.csv)
-  DEPENDS:
  - [Raw Data from Manifesto Project Database](../../data/raw/MPDataset_MPDS2019b.csv)
  - [Raw Data from ParlGov](../../data/raw/view_cabinet.csv)
  - [Raw Data from European Representative Democracy Data Archive](../../data/raw/Bergmann_Muller_Strom_Cabinets Dataset.csv)
  - [Raw Opinion Poll Data](../../data/raw/polldata_combined.RData)

Content
======

-   [Setup](#setup)
-   [Data](#data)
    -   [Scalability](#Scalability)
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
-  Requires access to the csv file prepared by PollFish.
-  [Cleaned data](../../data/intermediate/cleaned_NL.csv) and [codings](00_PrepareData_NL.R) are saved to public folders.


``` r
df <- read_sav("../../data/raw-private-encrypted/POLLFISH.csv")
```

Scalability
------------
* The gratifications of the news is constructed as an additive scale using a Principal Components Factor Analysis using varimax rotation, similar to Diddi and LaRose (2006), PAP p.X.
* NFM according to GdZ
* News Usage
*

```r
#Scalability of UGT (RQ1)
hs <- df %>%
  select(ugt_1:ugt_4,)
cols <- c(1:4)
hs[,cols] = apply(hs[,cols], 2, function(x) as.numeric(as.character(x)));
hs <- hs[complete.cases(hs), ]
fac_hs <- psych::fa(hs, rotate="varimax", fm="pa", scores="Bartlett")

surv <- df %>%
  select(ugt_5:ugt_11)
cols <- c(1:7)
surv[,cols] = apply(surv[,cols], 2, function(x) as.numeric(as.character(x)));
surv <- surv[complete.cases(surv), ]
fac_surv <- psych::fa(surv, rotate="varimax", fm="pa", scores="Bartlett")

esc <- df %>%
  select(ugt_12:ugt_16)
cols <- c(1:5)
esc[,cols] = apply(esc[,cols], 2, function(x) as.numeric(as.character(x)));
esc <- esc[complete.cases(esc), ]
fac_esc <- psych::fa(esc, rotate="varimax", fm="pa", scores="Bartlett")

pt <- df %>%
  select(ugt_17:ugt_21)
cols <- c(1:5)
pt[,cols] = apply(pt[,cols], 2, function(x) as.numeric(as.character(x)));
pt <- pt[complete.cases(pt), ]
fac_pt <- psych::fa(pt, rotate="varimax", fm="pa", scores="Bartlett")

ent <- df %>%
  select(ugt_22:ugt_23)
cols <- c(1:2)
ent[,cols] = apply(ent[,cols], 2, function(x) as.numeric(as.character(x)));
ent <- ent[complete.cases(ent), ]
fac_ent <- psych::fa(ent, rotate="varimax", fm="pa", scores="Bartlett")

social <- df %>%
  select(ugt_24, ugt_28:ugt_30)
cols <- c(1:4)
social[,cols] = apply(social[,cols], 2, function(x) as.numeric(as.character(x)));
social <- social[complete.cases(social), ]
fac_social <- psych::fa(social, rotate="varimax", fm="pa", scores="Bartlett")

#Scalability of NFM (DV H1, RQ2, DV H2, RQ3)
nfm <- df %>%
  select(nfm_1:nfm_6)
cols <- c(1:6)
nfm[,cols] = apply(nfm[,cols], 2, function(x) as.numeric(as.character(x)));
nfm <- nfm[complete.cases(nfm), ]
fac_nfm <- psych::fa(nfm, rotate="varimax", fm="pa", scores="Bartlett")

#Scalability of Mobile News Usage (IV H1)
mob_usage <- df %>%
  select(mediagebruik_4:mediagebruik_9)
cols <- c(1:6)
mob_usage[,cols] = apply(mob_usage[,cols], 2, function(x) as.numeric(as.character(x)));
mob_usage <- mob_usage[complete.cases(mob_usage), ]
fac_mob_usage <- psych::fa(mob_usage, rotate="varimax", fm="pa", scores="Bartlett")

# Knowledge about Corona (RQ 2)
df <- df %>%
  mutate(corona1=str_to_lower(corona1, locale = "nl"),
         KC1 = as.numeric(str_detect(df$corona1, "covid")),
         corona2_1 = replace_na(corona2_1, 0),
         corona2_2 = replace_na(corona2_2, 0),
         corona2_5 = replace_na(corona2_5, 0),
         corona2_8 = replace_na(corona2_8, 0),
         KC2 = (corona2_1 + corona2_2 + corona2_5 + corona2_8)/4,
         corona3=str_to_lower(corona3, locale = "nl"),
         KC3 = as.numeric(str_detect(df$corona3, "loon")),
         corona4_1 = ifelse(df$corona4_1 == 2, 1, 0),
         corona4_2 = ifelse(df$corona4_2 == 1, 1, 0),
         corona4_3 = ifelse(df$corona4_3 == 2, 1, 0),
         corona4_4 = ifelse(df$corona4_4 == 2, 1, 0),
         KC4 = (corona4_1 + corona4_2 + corona4_3 + corona4_4)/4)

KC <- df %>%
  select(KC1:KC4)
cols <- c(1:4)
KC[,cols] = apply(KC[,cols], 2, function(x) as.numeric(as.character(x)));
KC <- KC[complete.cases(KC), ]
fac_KC <- psych::fa(KC, rotate="varimax", fm="pa", scores="Bartlett")

#Political Knowledge (IV H2, DV H3, RQ3: together with Political Efficacy)
df <- df %>%
  mutate(pol_ken1_1 =  replace_na(pol_ken1_1, 0),
         pol_ken1_2 =  replace_na(pol_ken1_2, 0),
         pol_ken1_3 =  replace_na(pol_ken1_3, 0),
         pol_ken1_10 =  replace_na(pol_ken1_10, 0),
         pk1 = round((pol_ken1_1 + pol_ken1_2 + pol_ken1_3 + pol_ken1_10)/4, 0),
         pol_ken2 = recode(pol_ken2, `99` = 0),
         pol_ken3 = recode(pol_ken3, `99` = 0))

PK <- df %>%
  select(pk1, pol_ken2, pol_ken3)
cols <- c(1:3)
PK[,cols] = apply(PK[,cols], 2, function(x) as.numeric(as.character(x)));
PK <- PK[complete.cases(PK), ]
fac_PK <- psych::fa(PK, rotate="varimax", fm="pa", scores="Bartlett")

#Political Efficacy (IV H2, DV H3, RQ3: together with Political Knowledge)
PE <- df %>%
  select(pol_efficacy_1:pol_efficacy_5)
cols <- c(1:3)
PE[,cols] = apply(PE[,cols], 2, function(x) as.numeric(as.character(x)));
PE <- PE[complete.cases(PE), ]
fac_PE <- psych::fa(PE, rotate="varimax", fm="pa", scores="Bartlett")

PS <- df%>%
  select(pk1, pol_ken2, pol_ken3, pol_efficacy_1:pol_efficacy_5)
cols <- c(1:6)
PS[,cols] = apply(PS[,cols], 2, function(x) as.numeric(as.character(x)));
PS <- PS[complete.cases(PS), ]
fac_PS <- psych::fa(PS, rotate="varimax", fm="pa", scores="Bartlett")

#Political Participation (IV H3, RQ3)
PP <- df %>%
  select(elect_part1:elect_part2)
cols <- c(1:2)
PP[,cols] = apply(PP[,cols], 2, function(x) as.numeric(as.character(x)));
PP <- PP[complete.cases(PP), ]
fac_PP <- psych::fa(PP, rotate="varimax", fm="pa", scores="Bartlett")

tibble(Scale = c("Habit Strengt", "Surveillance", "Escapism","Passing Time", "Entertainment", "Social Motivation",
                 "News Finds Me (NFM) Perception", "Mobile News Usage", "Knowledge about COVID-19",
                 "Political Knowledge", "Political Interest", "Political Sophistication",
                 "Political Participation"),
       `Chi Square` = c(fac_hs[3]$chi, fac_surv[3]$chi, fac_esc[3]$chi, fac_pt[3]$chi, fac_ent[3]$chi,
                        fac_social[3]$chi, fac_nfm[3]$chi, fac_mob_usage[3]$chi, fac_KC[3]$chi,
                        fac_PK[3]$chi, fac_PE[3]$chi, fac_PS[3]$chi, fac_PP[3]$chi),
       Fit = c(fac_hs[10]$fit, fac_surv[10]$fit, fac_esc[10]$fit, fac_pt[10]$fit, fac_ent[10]$fit,
               fac_social[10]$fit, fac_nfm[10]$fit, fac_mob_usage[10]$fit, fac_KC[10]$fit,
              fac_PK[10]$fit, fac_PE[10]$fit, fac_PS[10]$fit, fac_PP[10]$fit),
       PA = c(fac_hs[29]$R2, fac_surv[29]$R2, fac_esc[29]$R2, fac_pt[29]$R2, fac_ent[27]$R2,
              fac_social[29]$R2, fac_nfm[29]$R2, fac_mob_usage[29]$R2, fac_KC[29]$R2,
              fac_PK[26]$R2, fac_PE[26]$R2, fac_PS[29]$R2, fac_PP[27]$R2))
```

|  Scale		 				| Chi Square |	Fit 		  | PA		|
|-----------------------------------------------  | :-------------: | :--------------:  | :----------------: |
| Habit Strength					|  24.2	     | 0.911 		  | 0.876		|
| Surveillance					|  37.7	     | 0.951 	  | 0.920 		|
| Escapism					|  12.0	     | 0.941 	  | 0.915		|
| Passing Time					|  13.6 	     | 0.938 	  | 0.908		|
| Entertainment					|  00.0	     | 0.961 	  | 0.859		|
| Social Motivation				|  23.9	     | 0.892 	  | 0.846		|
| News Finds Me (NFM) Perception	|  51.6	     | 0.787 	  | 0.809		|
| Mobile News Usage			|150.0	     | 0.717	 	  | 0.783		|
| Knowledge about COVID-19		|  70.8	     | 0.331 	  | 0.530		|
| Political Knowledge				|  00.0	     | 0.543 	  | 0.641		|
| Political Interest				|  00.0	     | 0.673 	  | 0.733		|
| Political Sophistication			|227.0	     | 0.370 	  | 0.897		|
| Political Participation			|  00.0	     | 0.966 	  | 0.869		|

- Variables created with an additive scale:
	- Trust in Traditional News
	- Trust in Online News
	- Traditional News Usage

```r
#Scalability of Controls
trust_tradmedia <- df %>%
  select(trust_media_1:trust_media_3)
trust_tradmedia <- trust_tradmedia[complete.cases(trust_tradmedia), ]
trust_tradmedia <- psy::cronbach(trust_tradmedia)

trust_mobmedia <- df %>%
  select(trust_media_4:trust_media_8)
trust_mobmedia <- trust_mobmedia[complete.cases(trust_mobmedia), ]
trust_mobmedia <- psy::cronbach(trust_mobmedia)

trad_newsusage <- df %>%
  select(mediagebruik_1:mediagebruik_3, mediagebruik_9)
trad_newsusage <- trad_newsusage[complete.cases(trad_newsusage), ]
trad_newsusage <- psy::cronbach(trad_newsusage)

tibble(Scale = c("Trust in Traditional Media", "Trust in Mobile Media", "Traditional News Usage"),
       `Cronbach's Alpha` = c(trust_tradmedia[3]$alpha, trust_mobmedia[3]$alpha, trad_newsusage[3]$alpha))
```

| Scale					| Cronbach's Alpha	|
| -----------------------------------	| ------------------------- |
| Trust in Traditional Media	| 0.689			|
| Trust in Online Media		| 0.745			|
| Traditional News Usage		| 0.692			|

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
