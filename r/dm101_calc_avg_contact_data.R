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


setwd("C:/Users/kw/documents/comix_stringency")

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
 
ggplot(data=dts[area=="uk"], aes(x=mid_date, mean)) + geom_line(aes(col=setting)) +
  scale_y_continuous(limits=c(2,5)) +
  facet_wrap(.~area)


qs::qsave(dts, "data/dts.qs")
dts <- qs::qread('data/dts.qs')



string <- as.data.table(read.csv("data/OxCGRT_latest.csv"))
string <- string[CountryName %in% c("Netherlands","Austria","Denmark",
                                    "Spain","France","Italy","Poland","Norway",
                                    "Portugal", "United Kingdom", "Belgium")]
string <- string[!(CountryName == "United Kingdom" & RegionName != "England")]
#string <- string[, .(CountryName, Date, StringencyIndex)]
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

string[, CountryName := map_country[CountryName]]
#string <- string[Date>20201219, ]
string <- string[, Date := as.character(Date)]
string$Date <- as.Date(string$Date, format = "%Y%m%d")
string <- string[Date>=as.Date(min(dts[area=="uk"]$mid_date))]
string <- string[Date<=as.Date(max(dts[area=="uk"]$mid_date))]
setnames(string, "CountryName", "area")

cols <- brewer.pal(12,"Paired")
cols <- cols[2:12]
  ggplot() +
  geom_line(data = dts[setting=="weighted"], 
            aes(x = mid_date, y = mean, group = interaction(toupper(area), panel), col = toupper(area)), 
            alpha=0.75, size=1.5) +
  geom_vline(xintercept=as.numeric(string$Date[yday(string$Date)==1]), colour="grey60") +
  scale_x_date(breaks = "month", labels = date_format("%b"), name = "") +
  scale_y_continuous(expand = c(0,0), name = "Mean contacts", limits = c(0,6), breaks = seq(0,6,1)) +
  annotate("text", x = as.Date("2020-04-03"), y = 5.8, size=10, label = "2020") +
  annotate("text", x = as.Date("2021-01-15"), y = 5.8, size=10, label = "2021") +
  scale_color_manual(values =cols, name = c("Country \n Code")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20), 
        axis.text.y.right = element_text(face="bold", color = 'black'),
        axis.title.y.right = element_text(face="bold", color = 'black'),
        axis.text.y.left = element_text(face="bold", color = 'black'),
        axis.title.y.left = element_text(face="bold", color = 'black'),
        legend.position = c(0.86,0.1), 
        legend.text = element_text(size=15), legend.title = element_text(size=15),
        legend.background = element_rect(fill=NA), legend.direction = "horizontal")

  ggplot() +
  geom_line(data = dts[setting=="weighted"], aes(x = mid_date, y = mean, 
                                                 group = interaction(toupper(area), panel), col = toupper(area)), size=1.5) +
  geom_vline(xintercept=as.numeric(string$Date[yday(string$Date)==1]), colour="grey60") +
  scale_x_date(limits = c(as.Date("2020-12-01"), as.Date("2021-04-15")), 
               breaks = "month", labels = date_format("%b"), name = "") +
  scale_y_continuous(expand = c(0,0), name = "Mean contacts", limits = c(0,6), breaks = seq(0,6,1)) +
  scale_color_manual(values =cols, name = c("Country \n Code")) +
  annotate("text", x = as.Date("2020-12-02"), y = 5.8, size=10, label = "2020") +
  annotate("text", x = as.Date("2021-01-06"), y = 5.8, size=10, label = "2021") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20), 
        axis.text.y.right = element_text(face="bold", color = 'black'),
        axis.title.y.right = element_text(face="bold", color = 'black'),
        axis.text.y.left = element_text(face="bold", color = 'black'),
        axis.title.y.left = element_text(face="bold", color = 'black'),
        axis.text.x = element_text(size=15),
        legend.position = c(0.86,0.1), 
        legend.text = element_text(size=15), legend.title = element_text(size=15),
        legend.background = element_rect(fill=NA), legend.direction = "horizontal")



ggplot() +
  geom_step(data=string, aes(x = Date, y = StringencyIndex/17), size=1, col="red") +
  geom_line(data = dts[setting=="weighted"], aes(x = mid_date, y = mean, group=panel), size=1, col="black") +
  geom_ribbon(data = dts[setting=="weighted"], aes(x = mid_date, ymin=uci, ymax = lci, group=panel), 
               fill="black", alpha=0.3) +
  geom_vline(xintercept=as.numeric(string$Date[yday(string$Date)==1]), colour="grey60") +
  facet_wrap(.~toupper(area), nrow=4) +
  scale_x_date(breaks = "month", labels = date_format("%b"), name = "") +
  scale_y_continuous(expand=c(0,0),name = "Mean contacts", limits = c(0,7), breaks = seq(0,7,1),
                     sec.axis = sec_axis(~.*17, name = "OxCGRT Stringency Index", breaks = seq(0,100,20))) +
  annotate("text", x = as.Date("2020-04-03"), y = 6.5, size=5, label = "2020") +
  annotate("text", x = as.Date("2021-01-25"), y = 6.5, size=5, label = "2021") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y.right = element_text(size=15, face="bold", color = 'red'),
        axis.title.y.right = element_text(size=15, face="bold", color = 'red'),
        axis.text.y.left = element_text(size=15, face="bold", color = 'black'),
        axis.title.y.left = element_text(size=15, face="bold", color = 'black'),
        axis.text.x = element_text(size=10),
        strip.text.x = element_text(size = 20)) 



study_dates <- as.Date(c(
  "2020-03-31",
  "2020-06-03",
  "2020-11-05",
  "2020-12-02",
  "2021-01-05",
  "2021-04-10", ## LD 3 ending may need updating
  "2020-07-30",
  "2020-09-02"
))


colors <- colorRampPalette(c("blue", "yellow", "red"))(13)


 ggplot() +
  geom_ribbon(data = dts[area=="uk" & setting=="weighted"], 
              aes(x = mid_date, ymin = uci, ymax = lci), alpha=0.1, fill="black") +
  geom_line(data = dts[area=="uk" & setting=="weighted"], aes(x = mid_date, y = mean), size=1.2,
            col = "black") +
  geom_vline(xintercept=as.numeric(string$Date[yday(string$Date)==1]), colour="grey60") +
  geom_step(data=string[area=="uk"], aes(x=Date, y=StringencyIndex/17), col="red", size=1.2) +
  scale_x_date(breaks = "month", labels = date_format("%b"), name = "") +
  scale_y_continuous(expand = c(0,0), name = "Mean contacts", limits = c(0,6), breaks = seq(0,6,1),
                     sec.axis = sec_axis(~.*17, name = "OxCGRT Stringency Index", breaks = seq(0,100,20))) +
  annotate("text", x = as.Date("2020-04-03"), y = 5.8, size=10, label = "2020") +
  annotate("text", x = as.Date("2021-01-15"), y = 5.8, size=10, label = "2021") +
  annotate("rect", xmin = study_dates[1], xmax = study_dates[2], ymin = 0, ymax = 6, alpha = .1) +
  annotate("rect", xmin = study_dates[3], xmax = study_dates[4], ymin = 0, ymax = 6, alpha = .1) +
  annotate("rect", xmin = study_dates[5], xmax = study_dates[6], ymin = 0, ymax = 6, alpha = .1) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size=20), 
        axis.text.y.right = element_text(face="bold", color = 'red'),
        axis.title.y.right = element_text(face="bold", color = 'red'),
        axis.text.y.left = element_text(face="bold", color = 'black'),
        axis.title.y.left = element_text(face="bold", color = 'black'),
        axis.text.x = element_text(size=15),
        legend.position = c(0.13,0.1),
        legend.text = element_text(size=15), legend.title = element_text(size=15),
        legend.background = element_rect(fill=NA)) 


string_merge <- as.data.table(merge(string, dts, by.x = c("area", "Date"),
                                    by.y = c("area", "mid_date")))
string_merge[, CountryName := toupper(area)]
string_merge[, month := yearmonth(Date)]


ggplot(data = string_merge[setting=="weighted"], aes(x=StringencyIndex, y=mean)) +
  geom_point(size=7.5, alpha=0.5, aes(col=CountryName)) +
  scale_color_manual(values =cols, name = c("Country \n Code")) +
  scale_y_continuous(expand=c(0,0), name = "Mean contacts", limits=c(0,6), breaks=seq(0,6,1)) +
  scale_x_continuous(expand=c(0,0), name = "OxCGRF Stringency Index", limits = c(0,100)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=20), 
        axis.text.y = element_text(face="bold", color = 'black'),
        axis.title.y = element_text(face="bold", color = 'black'),
        axis.text.x = element_text(face="bold", color = 'black'),
        axis.title.x = element_text(face="bold", color = 'black'),
        legend.position = c(0.86,0.1), 
        legend.text = element_text(size=15), legend.title = element_text(size=15),
        legend.background = element_rect(fill=NA), legend.direction = "horizontal")

ggplot(data = string_merge[setting=="weighted"], aes(x=StringencyIndex, y=mean)) +
  geom_point(size=7.5, alpha=0.5, aes(col=CountryName)) +
  scale_color_manual(values =cols) +
  facet_wrap(.~CountryName, nrow=4) +
  scale_y_continuous(expand=c(0,0), name = "Mean contacts", limits=c(0,6), breaks=seq(0,6,1)) +
  scale_x_continuous(expand=c(0,0), name = "OxCGRF Stringency Index", limits = c(0,100)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=20), 
        axis.text.y = element_text(face="bold", color = 'black'),
        axis.title.y = element_text(face="bold", color = 'black'),
        axis.text.x = element_text(face="bold", color = 'black'),
        axis.title.x = element_text(face="bold", color = 'black'),
        strip.text.x = element_text(size = 20),
        legend.position = "none")
   
library(tsibble)
colors <- colorRampPalette(c("blue", "yellow", "red"))(13)

ggplot(data = string_merge[CountryName=="UK" & setting == "weighted"], aes(x=StringencyIndex, y=mean)) +
  geom_point(size=7.5, alpha=0.3, aes(col=as.factor(month))) +
  scale_color_manual(values=setNames(colors, levels(as.factor((string_merge$month))))) +
  geom_text(aes(label = month), size=5) +
  scale_y_continuous(breaks=seq(0,6,1), name = "Mean contacts") +
  scale_x_continuous(name = "OxCGRF Stringency Index") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size=15), 
        axis.title = element_text(size=20))
 

