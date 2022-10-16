#Assignment 2

#Libraries
library(vroom)
library(dplyr)
library(here)

#Load Data thanks to Nixon's Code
list.files(path = paste0("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data"),
           recursive = TRUE,
           pattern = "\\.txt$|\\.csv$",
           full.names = TRUE) -> dir


for (i in 2013:2017) {
  message(paste0("Merging data sets year:",i))
  #Get the path for the specific year i.
  dir.mdppas = dir[(grepl(i, dir, ignore.case = TRUE) & 
                      grepl("PhysicianData", dir, ignore.case = TRUE))]
  dir.puf = dir[(grepl(i, dir, ignore.case = TRUE) & 
                   grepl("PUF", dir, ignore.case = TRUE))]
  
  #------
  #Read the MDPPAS data for the Year i. create int as in eq 1
  #------
  message("reading MDPPAS")
  a <- vroom(dir.mdppas)
  a$npi = as.character(a$npi) #Make sure npi has the same type 'character' on both data frames
  a <- a %>%
    select(npi, Year, pos_asc, pos_opd, pos_office, group1, group2) %>%
    group_by(Year, npi) %>%
    mutate(
      int = ifelse( pos_opd / (pos_opd + pos_office + pos_asc) >= 0.75,1,0) #Create int variable for Q2
    ) %>%
    select(Year,npi,int, group1, group2)
  
  #------
  #Read the PUF data for the Year i. Keep all MD observations. Collapse to Physician level; 1 observation per \{npi ~ Year\}
  #------
  
  message("Reading PUF")
  b <- vroom(dir.puf)
  names(b) = tolower(names(b))
  b <-   b %>%
    select(npi, nppes_credentials, average_medicare_allowed_amt, average_submitted_chrg_amt, 
           average_medicare_payment_amt,line_srvc_cnt, bene_unique_cnt) %>%
    filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE)) %>%
    mutate( #Create temporal variables just to speed the coding. (Check how to calculate the actual variables later)
      Total_Spending = average_medicare_allowed_amt*line_srvc_cnt, 
      Total_Claims = line_srvc_cnt, 
      Total_Patients = bene_unique_cnt
    ) %>%
    group_by(npi) %>%
    summarise(
      Total_Spending                = sum(Total_Spending, na.rm = TRUE),
      Total_Claims                  = sum(Total_Claims, na.rm = TRUE),
      Total_Patients                = sum(Total_Patients, na.rm = TRUE),
      average_submitted_chrg_amt    = sum(average_submitted_chrg_amt, na.rm = TRUE),
      average_medicare_payment_amt  = sum(average_medicare_payment_amt, na.rm = TRUE),
      average_medicare_allowed_amt  = sum(average_medicare_allowed_amt, na.rm = TRUE),
      line_srvc_cnt                 = sum(line_srvc_cnt, na.rm = TRUE),
      bene_unique_cnt               = sum(bene_unique_cnt, na.rm = TRUE)
    )  
  # Write the inner join of MDPPAS and PUF in Disk in rectangular form for Year i 
  # -> gives just the cases that we observe in both data sets
  dat <- inner_join(a,b, by="npi")
  vroom_write(dat, paste0(here("Desktop/OneDrive - Emory University/ECON 771/Empirical Assignment 2","dat_"),i,".csv"), delim = ",", col_names = TRUE)
}


#------
# Clean memory
#------ 

#Remove auxiliary objects from memory
rm(dir, dir.mdppas, dir.puf, i, a, b, dat)
#Clean memory
gc()

#------
# Append all the data.frames created in the loop in rectangular form
#------
list.files(path = paste0("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Empirical Assignment 2"),
           recursive = TRUE,
           pattern = "\\.csv$",
           full.names = TRUE) -> dir
data <- vroom(dir)
vroom_write(data, file = ("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Empirical Assignment 2/data.csv"), delim = ",")
rm(dir)


### Question 1
library(psych)

#Spending
spending <- describeBy(data$Total_Spending, data$Year, mat=TRUE)
names(spending)[names(spending) == "group1"] <- "Year" #change column name
rownames(spending) <- spending$Year
spending <- spending[,c("mean","sd","min","max")]
colnames(spending) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")

#Claims
claims <- describeBy(data$Total_Claims, data$Year, mat=TRUE)
names(claims)[names(claims) == "group1"] <- "Year" #change column name
rownames(claims) <- claims$Year
claims <- claims[,c("mean","sd","min","max")]
colnames(claims) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")

#Patients
patients <- describeBy(data$Total_Patients, data$Year, mat=TRUE)
names(patients)[names(patients) == "group1"] <- "Year" #change column name
rownames(patients) <- patients$Year
patients <- patients[,c("mean","sd","min","max")]
colnames(patients) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")

### Question 2
library(ggplot2)
claims_by_integration <- data  %>% 
  filter(!is.na(int)) %>%
  group_by(Year, int) %>%
  summarise(mean_claims_count = mean(Total_Claims, na.rm = TRUE))
  
### Question 3
library(fixest)
int_on_claims <- feols(log(Total_Claims) ~ int | npi + Year, data = data[!(data$Year == 2012 & data$int == 1),])
table3 <- etable(int_on_claims)

### Question 4
rho <- c(0, 0.5, 1, 1.5, 2)
rsquaredmax <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)

X1 <- feols(log(Total_Claims) ~ int | npi + Year, data = data)
D <- feols(log(Total_Claims) ~ int, data = data)
i <- 1
bounds <- as.data.frame(matrix(ncol = 4, nrow = sum(rho) * length(rsquaredmax)))
bounds[,3] <- X1$coefficients[[1]]
colnames(bounds) <- c("Rho","R Squared Max", "Left Bound", "Right Bound")

for (rho in rho){
  rsquaredmax <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
  for (rsquaredmax in rsquaredmax){
    bounds[i,1] <- rho
    bounds[i,2] <- rsquaredmax
    bounds[i,4] <- bounds[i,3] - rho*(D$coefficients[[1]] - bounds[i,3])*(rsquaredmax - X1$sq.cor)/(X1$sq.cor - D$sq.cor)
    i <- i + 1
    }
}


### Question 5
library(tidyr)

#Using Nixon's code to create the instrument 
#Create a character vector containing paths to all files.

list.files(path = paste0("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data"),
           recursive = TRUE,
           pattern = "\\.txt$|\\.csv$",
           full.names = TRUE) -> dir

message("Reading MDPPAS_2009 and PFS")
taxid.base <- vroom(("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data/PhysicianData_2009.csv")) %>% 
  select(npi, tax_id = group1) %>%
  mutate(npi = as.character(npi))

pfs <- vroom("/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Data/PFS_update_data.txt")

price.shock <- data.frame()

#------
# Create the instrument for int -> price.shock
#------


for (i in 2012:2017){
  message(paste0("creating instrument year ", i, "..."))
  if (i <=2013) {
    dir.puf = dir[(grepl(i, dir, ignore.case = TRUE) & 
                     grepl("PUF", dir, ignore.case = TRUE))]
    
    # Read the PUF data
    b <- vroom(dir.puf)
    names(b) = tolower(names(b))
    b <- b %>%
      select(npi, nppes_credentials, hcpcs_code, average_medicare_allowed_amt, average_submitted_chrg_amt, 
             average_medicare_payment_amt,line_srvc_cnt, bene_unique_cnt) %>%
      filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE)) 
    
    # Run Ian's snippet code tax_id is group1 from MDPPAS 2009
    price.shock.temp <- b %>% inner_join(taxid.base, by="npi") %>%
      inner_join(pfs %>% filter(year==i) %>%
                   select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007),
                 by=c("hcpcs_code"="hcpcs")) %>%
      mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
      mutate(price_shock = case_when(
        i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
        i>2013  ~ dprice_rel_2010
      ),
      denom = line_srvc_cnt*price_nonfac_orig_2010,
      numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) %>%
      group_by(npi) %>%
      summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(tax_id)) %>%
      ungroup() %>%
      mutate(phy_rev_change=phy_numer/phy_denom) %>%
      group_by(tax_id) %>%
      summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(Year = i)
  } else {
    
    price.shock.temp <- price.shock %>% 
      filter(Year==2013) %>% 
      ungroup() %>%
      mutate(Year = i)
  }
  
  price.shock <- rbind(price.shock, price.shock.temp)
  message("Instrument ",i," appended...")
  
}

# Remove Auxiliary object and clear memory
rm(i, dir, dir.puf, price.shock.temp, b, pfs, taxid.base)

# Write file
vroom_write(price.shock, path = "/Users/katieleinenbach/Desktop/OneDrive - Emory University/ECON 771/Empirical Assignment 2/instrument.csv")


#Merge Data
df <- left_join(data, price.shock, by=c("group1" = "tax_id", "Year" = "Year"))
est_2SLS <- feols(log(Total_Claims) ~ 1 | npi + Year | int ~ practice_rev_change, df)

question5_stages <- etable(summary(est_2SLS, stage = 1:2))
question5 <- etable(summary(est_2SLS))


### Question 6
df <- df[!is.na(df$practice_rev_change),]
df <- df[!is.na(df$int),]

df$vhat <- feols(int ~ practice_rev_change | npi + Year, df)$residuals
est_DWS <- feols(log(Total_Claims) ~ int + vhat | npi + Year, df)
question6 <- etable(summary(est_DWS))

feols(log(Total_Claims) ~ 1 | npi + year | integration ~ practice_rev_change, puf)

### Question 7
question7 <- feols(log(Total_Claims) ~ practice_rev_change | npi + Year, df)
table7 <- etable(summary(question7))


### Question 8
df$random_rev_change = rowMeans(replicate(100, sample(df$practice_rev_change)))
df$center_rev_change = df$practice_rev_change - df$random_rev_change
est_SS = feols(log(Total_Claims) ~ 1 | npi + Year | int ~ center_rev_change, df)
etable(est_SS)









