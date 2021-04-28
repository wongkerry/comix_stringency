## Name: dm101_calc_avg_contact_data
## Description: Calculate and save the mean contacts over time.

## Input file: dt_1w and dt_2w
## Functions: bs_group
## Output file: 2021-01-24_bs_means_2w.qs

# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(ggpubr)
library(RColorBrewer)
library(tsibble)

# Source user written scripts ---------------------------------------------

## Only works for the subset in this script will need adapting to do more
source('r/functions/bs_group.R')

# Load participant data ---------------------------------------------------
pdt <- qs::qread('data/dt_2w.qs')

# Define boots ------------------------------------------------------------
boots <- 100
dt_boot <- data.table()

pdt <- pdt[!country %in% c("ch","fi","gr","lt","si")]

setnames(pdt, "country", "area")
setorder(pdt, area, survey_round, part_id)

# pdt[, mean := weighted.mean(n_cnt, w = dayweight * genderageweight), by = .(area, survey_round)]
# dts <- as.data.table(unique(pdt[, .(area, mid_date, survey_round, mean)]))

# Main analysis -----------------------------------------------------------
# Get country -------------------------------------------------------------
for(i in c(unique(pdt$area))){
#for(i in "uk"){
  print(i)
  dt1 <- bs_group(pdt,  boots, prop = 1.0, area_ = i)
  dt_boot <- rbind(dt_boot, dt1)
}

dt_boot[, n := round(median(N)), by = .(area, panel, start_date, mid_date, end_date)]

mea_vars <- c("weighted","unweighted")

l_dt <- melt(dt_boot, id.vars = c("area", "panel", "start_date", "mid_date", "end_date", "survey_round", "n"),
             measure.vars = mea_vars, variable.name = "setting", value  = "avg")

dts <- l_dt[, .(lci = quantile(avg, 0.025, na.rm = T), mean = mean(avg, na.rm = T),
                uci = quantile(avg, 0.975, na.rm = T), boots = .N),
            by = .(area, panel, start_date, mid_date, end_date, setting, n)]
setorder(dts, area, panel, mid_date)
 
qs::qsave(dts, "data/dts.qs")

