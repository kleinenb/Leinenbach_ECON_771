#Empirical Exercise

#Load Data
setwd("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data")
data <- read.delim("HCRIS_Data.txt", header = TRUE, sep = "\t", dec = ".")

setwd("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data/provider-of-services")
pos <- read.csv("pos.csv",header = TRUE)
pos_last <- read.csv("pos_lastyear.csv", header = TRUE)

setwd("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data")
medicaid <- read.csv("KFF_medicaid_expansion_2019.csv",header = TRUE)

#Clean Data before Merge
names(pos)[names(pos) == "pn"] <- "provider_number" #change column name
names(pos_last)[names(pos_last) == "pn"] <- "provider_number" #change column name
medicaid$state <- state.abb[match(medicaid$State,state.name)]
pos$provider_number <- sub("^0+", "", pos$provider_number) 
data <- data[(data$year >"2002" &  data$year < "2020"),]


#Merge Data
data <- merge(data,pos,by=c("provider_number", "year"), all.x = TRUE)
data_nomatch <- data[is.na(data$name),] #remove values that do not have a match in pos
data_nomatch <- data_nomatch[,1:36] #remove merged columns
data_nomatch <- merge(data_nomatch, pos_last, by = "provider_number") #use last year data for those without year provider matches
names(data_nomatch)[names(data_nomatch) == "city"] <- "city.y" #change column name
names(data_nomatch)[names(data_nomatch) == "state"] <- "state.y" #change column name
names(data_nomatch)[names(data_nomatch) == "zip"] <- "zip.y" #change column name
data_nomatch <- data_nomatch[,1:65] #remove last year column
data <- data[!is.na(data$name),] #remove those that did not merge properly with pos
data <- rbind(data,data_nomatch) #add the no match data with matches from last year to the matched data
names(data)[names(data) == "state.y"] <- "state" #change column name
data <- merge(data, medicaid, by = "state") #merge in medicaid data by state

data$adoption_year <- sub('.*\\/', '', data$Description)
data$adoption_year[data$adoption_year == "2015\n"] <- "2015" 
data$adoption_year[data$adoption_year == "2014 \n"] <- "2014" 
data$adoption_year[data$adoption_year == "2014 "] <- "2014" 
data$adoption_year[data$adoption_year == "2014\n"] <- "2014" 
data$adoption_year[data$adoption_year == "2016\n"] <- "2016" 
data$adoption_year[data$adoption_year == "2018)\n"] <- "2019" 
data$adoption_year[data$Expansion.Status == "Not Adopted"] <- "" 
data$adoption_year[data$Expansion.Status == "Adopted but Not Implemented"] <- "" 


#Question 1
#Summary Statistics for Uncompensated Care
library(psych)
uncom_stat <- describeBy(data$uncomp_care, data$year, mat=TRUE)
names(uncom_stat)[names(uncom_stat) == "group1"] <- "Year" #change column name
rownames(uncom_stat) <- uncom_stat$Year
uncom_stat <- uncom_stat[,c("mean","sd","min","max")]
colnames(uncom_stat) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")

setwd("/Users/katieleinenbach/Desktop")
write.csv(as.data.frame(uncom_stat), file =  "question_1_1.csv")

#Summary Statistics for Total Revenue
rev_stat <- describe.by(data$tot_pat_rev, data$year, mat=TRUE)
names(rev_stat)[names(rev_stat) == "group1"] <- "Year" #change column name
rownames(rev_stat) <- rev_stat$Year
rev_stat <- rev_stat[,c("mean","sd","min","max")]
colnames(rev_stat) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")
write.csv(as.data.frame(rev_stat), file =  "question_1_2.csv")

#Question 2
data$type <- ifelse(data$nonprofit == 1, "Non-profit", "For profit")

graph <- data%>%                                     
  group_by(type, year) %>%
  dplyr::summarise(uncom_care = mean(uncomp_care, na.rm = TRUE))
library(ggplot2)

ggplot(graph, aes(x=year, y=uncom_care, group=type)) +
  geom_line(aes(linetype=type))+
  geom_point()+
  scale_linetype_manual(values=c("twodash", "dotted"))

#Question 3
library(fixest)
data$treat <- ifelse(data$year >= data$adoption_year, 1, 0)
twfe_tot <- feols(uncomp_care ~ treat | state + year, data = data)
twfe_2014 <- feols(uncomp_care ~ treat | state + year, data = data[data$adoption_year == 2014 | data$adoption_year == "",])
twfe_2015 <- feols(uncomp_care ~ treat | state + year, data = data[data$adoption_year == 2015 | data$adoption_year == "",])
twfe_2016 <- feols(uncomp_care ~ treat | state + year, data = data[data$adoption_year == 2016 | data$adoption_year == "",])

twfe_table <- etable(twfe_tot, twfe_2014, twfe_2015, twfe_2016, order = "f", drop = "Int")
colnames(twfe_table) <- c("Full Sample", "2014 Treatment", "2015 Treatment", "2016 Treatment")
write.csv(as.data.frame(twfe_table), file =  "question_3.csv")


#Question 4
data$treat_interaction <- ifelse(data$adoption_year == "","",as.numeric(data$year) - as.numeric(data$adoption_year))
twfe_tot4 <- feols(uncomp_care ~ treat_interaction | state + year, data = data)
twfe_2014_4 <- feols(uncomp_care ~ treat_interaction | state + year, data = data[data$adoption_year == 2014 | data$adoption_year == "",])

question_4_table <- etable(twfe_tot4, twfe_2014_4, order = "f", drop = "Int")
colnames(question_4_table) <- c("Full Sample", "2014 Treatment")
names(question_4_table)[names(question_4_table) == "treat_interaction-1"] <- "Year -1"

write.csv(as.data.frame(question_4_table), file =  "question_4.csv")

#Question 5
sun_ab_2014 <- feols(uncomp_care ~ sunab(adoption_year, year) | state + year,
                cluster=~state,
                data=data[data$adoption_year == 2014 | data$adoption_year == "",])
sun_ab_2015 <- feols(uncomp_care ~ sunab(adoption_year, year) | state + year,
                     cluster=~state,
                     data=data[data$adoption_year == 2015 | data$adoption_year == "",])
sun_ab_2016 <- feols(uncomp_care ~ sunab(adoption_year, year) | state + year,
                     cluster=~state,
                     data=data[data$adoption_year == 2016 | data$adoption_year == "",])
question_5_table <- etable(sun_ab_2014, sun_ab_2015, sun_ab_2016, order = "f", drop = "Int")
colnames(question_5_table) <- c("2014 Treatment", "2015 Treatment", "2016 Treatment")
write.csv(as.data.frame(question_5_table), file =  "question_5.csv")


#Question 6

iplot(sun_ab_2014, xlab = "Time to Treatment", main = "2014 SA Event Study")
iplot(sun_ab_2015, xlab = "Time to Treatment", main = "2015 SA Event Study")
iplot(sun_ab_2016, xlab = "Time to Treatment", main = "2016 SA Event Study")



#Question 7
library(did)
library(DRDID)
data$adoption_year <- as.numeric(data$adoption_year)
data$adoption_year[is.na(data$adoption_year)] <- 0 

mod.cs <- att_gt(yname="uncomp_care", tname="year", idname="provider_number",
                 gname="adoption_year", clustervars = "state",
                 data=data, panel=TRUE, est_method="dr",
                 allow_unbalanced_panel=TRUE)
mod.cs.event <- aggte(mod.cs, type="dynamic")
ggdid(mod.cs.event, legend = FALSE, cex.axis = 3)

#Question 8
install.packages("remotes")
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
remotes::install_github("asheshrambachan/HonestDiD")

library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)
library(HonestDiD)

honest_did <- function(es, ...) {
  UseMethod("honest_did", es)
}

honest_did.AGGTEobj <- function(es,
                                e=0,
                                type=c("smoothness", "relative_magnitude"),
                                method=NULL,
                                bound="deviation from parallel trends",
                                Mvec=NULL,
                                Mbarvec=NULL,
                                monotonicityDirection=NULL,
                                biasDirection=NULL,
                                alpha=0.05,
                                parallel=FALSE,
                                gridPoints=10^3,
                                grid.ub=NA,
                                grid.lb=NA,
                                ...) {
  
  
  type <- type[1]
  
  # make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    warning("it is recommended to use a universal base period for honest_did")
  }
  
  # recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / (n*n) 
  
  
  nperiods <- nrow(V)
  npre <- sum(1*(es$egt < 0))
  npost <- nperiods - npre
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  
  orig_ci <- constructOriginalCS(betahat = es$att.egt,
                                 sigma = V, numPrePeriods = npre,
                                 numPostPeriods = npost,
                                 l_vec = baseVec1)
  
  if (type=="relative_magnitude") {
    if (is.null(method)) method <- "C-LF"
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat = es$att.egt, sigma = V, 
                                                             numPrePeriods = npre, 
                                                             numPostPeriods = npost,
                                                             bound=bound,
                                                             method=method,
                                                             l_vec = baseVec1,
                                                             Mbarvec = Mbarvec,
                                                             monotonicityDirection=monotonicityDirection,
                                                             biasDirection=biasDirection,
                                                             alpha=alpha,
                                                             gridPoints=100,
                                                             grid.lb=-1,
                                                             grid.ub=1,
                                                             parallel=parallel)
    
  } else if (type=="smoothness") {
    robust_ci <- createSensitivityResults(betahat = es$att.egt,
                                          sigma = V, 
                                          numPrePeriods = npre, 
                                          numPostPeriods = npost,
                                          method=method,
                                          l_vec = baseVec1,
                                          Mvec=Mvec,
                                          monotonicityDirection=monotonicityDirection,
                                          biasDirection=biasDirection,
                                          alpha=alpha,
                                          parallel=parallel)
  }
  
  list(robust_ci=robust_ci, orig_ci=orig_ci, type=type)
}

hd_cs_smooth_never <- honest_did(mod.cs.event,
                                 type="smoothness")

