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
weightlookup <- setDT(dt)[, .(sample = .N), by = .(country, part_gender, part_age)]
#remove NA age or NA gender for now since no. is low, might need to better deal with this later
weightlookup <- weightlookup[!is.na(part_age)]
weightlookup <- weightlookup[!is.na(part_gender)]

weightlookup[, age := cut(as.numeric(part_age), seq(0,100,5), include.lowest = TRUE, right = FALSE)]
weightlookup <- merge(weightlookup, pop, by = c("country", "part_gender", "age"), all.x = TRUE)

weightlookup <- setDT(weightlookup)[, .(sample = sum(sample), 
                                        pop = mean(pop2020)), by = .(country, part_gender, age)]
weightlookup[, genderageweight := pop/sample]
weightlookup[, c("sample", "pop") := NULL]

#merge back to dt
dt[, age := cut(as.numeric(part_age), seq(0,100,5), include.lowest = TRUE, right = FALSE)]
dt <- merge(dt, weightlookup, by = c("country", "part_gender", "age"))