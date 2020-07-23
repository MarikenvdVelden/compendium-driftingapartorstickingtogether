library(foreign)
library(tidyverse)
library(lubridate)

calculate_sum_difs <- function(cmp){
  unique_elections <- unique(cmp$electionid)
  for(i in 1:length(unique_elections)){
    tmp <- expand_grid(x = cmp$party[cmp$electionid==unique_elections[i]],
                       y = cmp$party[cmp$electionid==unique_elections[i]])
    rile <- expand_grid(x = cmp[cmp$electionid==unique_elections[i],"rile"],
                        y = cmp[cmp$electionid==unique_elections[i],"rile"])
    difference <- tibble(party1 = tmp$x,
                         party2 = tmp$y,
                         rile_difs = abs(rile[,1] - rile[,2])) 
    difference$rile_difs <- unlist(difference$rile_difs)
    for(j in 1:(dim(cmp)[2] - 5)){ 
      tmp <- expand_grid(x = cmp[cmp$electionid==unique_elections[i],paste0("issue",j)],
                         y = cmp[cmp$electionid==unique_elections[i],paste0("issue",j)])
      difference <- add_column(difference, !!(paste0("issue",j)) := abs(tmp[,1]-tmp[,2]))
      difference[,(j+3)] <- unlist(difference[,(j+3)])
    }
    if(i==1){
      tot <- mutate(difference, 
                    sum_difs = apply(difference[3:((dim(difference)[2]))], 1, sum),
                    electionid_ext = rep(unique_elections[i], dim(difference)[1]))
    }
    else{
      difference <- mutate(difference, 
                           sum_difs = apply(difference[3:((dim(difference)[2]))], 1, sum),
                           electionid_ext = rep(unique_elections[i], dim(difference)[1]))
      tot <- bind_rows(tot, difference)
      }
    }
  return(tot)
}

