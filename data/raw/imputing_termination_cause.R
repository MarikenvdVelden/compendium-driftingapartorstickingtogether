###
erd <- read.csv("/Users/velden/Dropbox/Data/Data ERD_2014.csv")
colnames(erd)[1] <- "v001e"
erd <- subset(erd, v001e==1|v001e==2|v001e==3|v001e==4|v001e==6|v001e==8|v001e==9|v001e==11|v001e==12|v001e==13|v001e==16, 
              select=c(v001e, v003e, v004e, v005e,  v008e, v300e, v603e))
colnames(erd) <- c("country", "cabinet_name", "start", "end","election_date", "start_new_gov", "relative_duration") 
erd$year <- substr(erd$election_date,1,4)
erd <- erd[which(erd$year>1996),]

erd$country <- ifelse(erd$country==1, "Austria",
               ifelse(erd$country==2, "Belgium",
               ifelse(erd$country==3, "Denmark",
               ifelse(erd$country==4, "Finland",
               ifelse(erd$country==6, "Germany",
               ifelse(erd$country==8, "Iceland",
               ifelse(erd$country==9, "Ireland",
               ifelse(erd$country==11, "Luxembourg",
               ifelse(erd$country==12, "Netherlands",
               ifelse(erd$country==13, "Norway",
               ifelse(erd$country==16, "Sweden", erd$country)))))))))))

erd$early.elections <- ifelse(erd$relative_duration<1,1,0)
length(which(erd$early.elections==1))# 22 cases we need to look at - see csv "imputing_termination_cause" for coding decisions
erd <- erd[which(erd$early.elections==1),]
write.csv(erd, "imputing_termination_cause.csv")
