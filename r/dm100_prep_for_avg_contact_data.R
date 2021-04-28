## Average contacts overtime
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemr)
ggthemr('dust')

setwd("C:/Users/kw/documents/comix_stringency")

# Load participant data ---------------------------------------------------
# local <- qs::qread("data/clean/part_v3.qs")
# table(local[row_id==0]$part_gender, local[row_id==0]$sample_type, useNA = "always")
part <- qs::qread("C:/Users/kw/Filr/Net Folders/EPH Shared/Comix_survey/data/validated/part_valid.qs")
part <- part[sample_type == "adult"] #45191 adult-wave
table(part$survey_round, part$country)
table(part$wave, part$country)

# Get total contacts ------------------------------------------------------
cnts <- qs::qread("C:/Users/kw/Filr/Net Folders/EPH Shared/Comix_survey/data/validated/contacts_valid.qs")

# Map objects for labels --------------------------------------------------
cnt_main_vars <- c(
  "cnt_home", 
  "cnt_work",
  "cnt_school",
  "cnt_other",
  "cnt_phys"
)

cnt_other_vars <- c(
  "cnt_inside", 
  "cnt_outside", 
  "cnt_sport", 
  #"cnt_outside_other",
  "cnt_other_place", 
  "cnt_other_house",
  "cnt_worship",
  "cnt_public_transport", 
  "cnt_supermarket",
  "cnt_shop",
  "cnt_bar_rest",
  "cnt_health_facility", 
  "cnt_salon",
  "cnt_public_market"
)

cnt_vars <- c(cnt_main_vars, cnt_other_vars)
all_vars <- c(cnt_vars, "part_wave_uid")
cntd <- cnts[, ..all_vars]

cp_n_cnts <- cnts[, .(n_cnt = .N), by = part_wave_uid]

pt_cnt = merge(part[,.(part_wave_uid)], cp_n_cnts, by = c("part_wave_uid"),
               all.x = TRUE, all.y = TRUE)

var_list <- names(cp_n_cnts)
for (j in var_list){
  set(pt_cnt,which(is.na(pt_cnt[[j]])),j,0)
}

# Add on contacts ---------------------------------------------------------
dt <- base::merge(part, pt_cnt, by = "part_wave_uid", all.x = TRUE)
dt[is.na(n_cnt), n_cnt := 0]

## Remove round 6 and 7
dt <- dt[!(survey_round %in% 6:7 & country=="uk")]
dt <- dt[!(survey_round %in% 9 & country=="nl")]

# Create a weekday weekend weight -----------------------------------------
dt[, dayweight := fifelse(weekday %in% c("Sun", "Sat"), 2/7, 5/7)]

# # Restrict the contacts to 50 max ----------------------------------------
dt[, n_cnt := pmin(n_cnt, 50)]

## the idea here is that the data in the next week will also be present in 
## the previous week. Therefore there will be duplicates in the dataset.

## Include variables that we will want to subset by.
vars <- c(
  ## Identifiers
  "part_id",
  "survey_round",
  "date",
  "panel",
  "wave",
  "part_wave_uid",
  ## contact setting
  "n_cnt",

  ## Subsets
  "country",
  "sample_type",
  "part_age",
  "part_gender",
  ## Weighting
  "dayweight")


# Combine the data over two weeks for smoothing ---------------------------
## Original data
p1 <- dt[, ..vars] ## Will be used for one week version
p2 <-  dt[, ..vars]


## Move p2 back for one survey round
p2[, survey_round := survey_round -1]

## Remove round 0, round 7 for p2 will be single week, and round 5 for p1 would be a single week so remove. 
p2 <- p2[!(survey_round %in% c(0,7) & country=="uk")]
p12 <- p1[!(survey_round %in% c(5) & country=="uk")]

# Combine data double the amount for each round
pdt <- rbind(p12,p2)

# Create a new start, mid, and end date for the survey rounds
p1[, start_date := min(date), by = .(country, survey_round)]
p1[, end_date := max(date), by = .(country, survey_round)]
p1[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(country, survey_round)]

## Repeat for two week version
pdt[, start_date := min(date), by = .(country, survey_round)]
pdt[, end_date := max(date), by = .(country, survey_round)]
pdt[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(country, survey_round)]


#weight by age using the UNWPP population data
library(wpp2019)
data(popF); data(popM)
popF <- as.data.table(popF)
popM <- as.data.table(popM)
popvar <- c("name","age","2020")
popF <- popF[, ..popvar]
popF[, gender := "female"]
popM <- popM[, ..popvar]
popM[, gender := "male"]

pop <- rbind(popF, popM)
names(pop) <- c("country","age","pop2020","part_gender")
map_country <- c(
  "Austria" = "at",
  "Denmark" = "dk",
  "Spain" = "es",
  "France" = "fr",
  "Italy" = "it",
  "Poland" = "pl",
  "Portugal" = "pt",
  "United Kingdom" = "uk",
  "Netherlands" = "nl",
  "Norway" = "no",
  "Belgium" = "be"
)
pop[, country := map_country[country]]
pop <- pop[!is.na(country)]

#prepare the age variable for merging
pop[, c("age_min", "age_max") := tstrsplit(age, "-", fixed=TRUE)]
pop[, age_max := as.numeric(age_max)+1]
pop[, age := paste0("[", age_min, ",",age_max,")")]
pop[, c("age_min","age_max") := NULL]

#create a lookup for each sex-age(group)
#lookup should contain, for each sex-age(group) group, sample size and total population
#weight is calculated as pop/sample
#meaning, each participant who belongs in each sex-age(group) group should represent x=pop/sample in the population
weightlookup <- setDT(pdt)[, .(sample = .N), by = .(country, mid_date, part_gender, part_age)]
#remove NA age or NA gender for now since no. is low, might need to better deal with this later
weightlookup <- weightlookup[!is.na(part_age)]
weightlookup <- weightlookup[!is.na(part_gender)]

weightlookup[, age := cut(as.numeric(part_age), seq(0,100,5), include.lowest = TRUE, right = FALSE)]
weightlookup <- merge(weightlookup, pop, by = c("country", "part_gender", "age"), all.x = TRUE)

weightlookup <- setDT(weightlookup)[, .(sample = sum(sample), 
                                        pop = mean(pop2020)), by = .(country, mid_date, part_gender, age)]
weightlookup[, genderageweight := (pop/sample)]
#bound it by 0.3 and 3
# weightlookup[genderageweight > 3, genderageweight := 3]
# weightlookup[genderageweight <= 0.3, genderageweight := 0.3]
weightlookup[, c("sample", "pop") := NULL]

#merge back to dt
pdt[, age := cut(as.numeric(part_age), seq(0,100,5), include.lowest = TRUE, right = FALSE)]
pdt <- merge(pdt, weightlookup, by = c("country", "mid_date", "part_gender", "age"))


# Save data ---------------------------------------------------------------

qs::qsave(p1, "data/dt_1w.qs")
qs::qsave(pdt, "data/dt_2w.qs")

