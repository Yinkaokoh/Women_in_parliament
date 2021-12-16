
# Women in Parliament - ASFI Group 5 Project
# 
# Script written by OKOH Olayinka Sunday
# 14th December, 2021

# sent to me by Dr Nwaru Bright


###Load the packages that will be needed for the project
library(maptools)
library(RColorBrewer)
library(maps)
library(mapdata)
library(readxl)
library(ggplot2)
library(qwraps2)
library(dplyr)
library(gridExtra)
library(ggcorrplot)
library(ggpubr)
library(gridExtra)
library(vcd)
library(tidyr)
library(maps)
library(mapdata)
library(scatterpie)
library(ggmap)
library(mapproj)
library(readr) #to read in tsv data
library(lubridate)
#library(read)
#raw_data <- read_tsv(file.choose()) #This could have been used interactively. But to avoid manual selection each time
#read in the data

# This is the main raw data
raw_data <- read_excel("C:/Users/Yinka Okoh/Downloads/Group 5 Project Data.xls", 
                       sheet = "Data", skip = 2)

# This is the meta data that contains further information
meta_data <- read_excel("C:/Users/Yinka Okoh/Downloads/Group 5 Project Data.xls", 
                        sheet = "Metadata - Countries", skip = 0)


#convert the  data to data frame
raw_data <- as.data.frame(raw_data)
meta_data <- as.data.frame(meta_data)

raw_data <- raw_data %>% select(`Country Name`, `Country Code`, "2000":"2020")

raw_data$IncomeGroup <- factor(raw_data$IncomeGroup, level = c("High income", "Upper middle income", "lower middle income", "Low income"))

curated_data <- raw_data <- left_join(meta_data, raw_data, by = "Country Code")

#remove the Table column because it is just a repetition
curated_data <- curated_data %>% select(-(TableName))

# Make the Region column a factor
curated_data$Region <- as.factor(curated_data$Region)
curated_data$IncomeGroup <- as.factor(curated_data$IncomeGroup)

# Convert to long data type for easy analysis with ggplot2
long_curated <- gather(curated_data, 
                       year, 
                       percentage, 
                       "2000":"2020", 
                       factor_key = TRUE)

# Convert the year column to date type data
long_curated$IncomeGroup <- factor(long_curated$IncomeGroup, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))

long_curated$year <- as.Date(long_curated$year, "%Y")
long_curated$year <- year(long_curated$year)

# Number of countries captured
number_countries <- curated_data %>% group_by(Region) %>% summarise(Countries = n())

## Bar chart of captured countries.
ggplot(number_countries, 
       aes(x = reorder(Region, Countries),
           y= Countries,
           fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Countries)), 
            position = position_dodge(width = 1),vjust = 0.5) +
  theme(axis.text.x = element_blank()) +
  xlab("Region")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/captured_countries_bar.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/captured_countries_bar.pdf", height = 120, width = 200, units = 'mm')

# line Graph of Regions
ggplot(long_curated, aes(x = year, 
                         y = percentage,
                         colour = Region
))+ 
  geom_line(size= 1.5)+
  xlab("Year") + ylab("Percentage")

ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_date_line.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_date_line.pdf", height = 120, width = 200, units = 'mm')


# Boxplot of Region vs percentage
ggplot(long_curated, aes(y = percentage,
                         x = Region,
                         fill = Region))+
  geom_boxplot(notch = TRUE) +
  theme(axis.text.x = element_blank()) +
  xlab("Region")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_percentage_box.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_percentage_box.pdf", height = 120, width = 200, units = 'mm')


# To get the values
B <- boxplot(long_curated$percentage ~ long_curated$Region)
B
# Violin plot
ggplot(long_curated, aes(y = percentage,
                         x = Region,
                         fill = Region
))+
  geom_violin() +
  theme(axis.text.x = element_blank()) +
  xlab("Region")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_percentage_violin.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_percentage_violin.pdf", height = 120, width = 200, units = 'mm')


#filter Africa data 
Sub_Africa <- long_curated %>% filter(Region == "Sub-Saharan Africa")
ME_NAfrica <- long_curated %>% filter(Region == "Middle East & North Africa")
Africa1


Sub_Africa2 <- curated_data %>% filter(Region == "Sub-Saharan Africa")
ME_NAfrica2 <- curated_data %>% filter(Region == "Middle East & North Africa")
Africa2 <- merge(Sub_Africa2, ME_NAfrica2)


quantile(long_curated$percentage, probs = c(0.1, 0.25, 0.75, 0.9), na.rm = TRUE)

#    10%         25%         75%         90% 
#  5.654493   10.433916  23.616978   32.258065 

Africa_bottom10perc <- Sub_Africa %>% filter(percentage <= 5.65 )
Africa_25perc <- Sub_Africa %>% filter(percentage <= 10.43 )
Africa_75perc <- Sub_Africa %>% filter(percentage <= 23.61 )
Africa_top10perc <- Sub_Africa %>% filter(percentage >= 32.258)

Africa_bottom <- Sub_Africa %>% filter(percentage <= 4)
Africa_top <- Sub_Africa %>% filter(percentage >= 40)


ggplot(Africa_bottom, aes(x = year, 
                          y = percentage,
                          colour = `Country Name`
)) +
  geom_line(size = 1.5)+
  xlab("Year") +
  ylab("Percentage (%)")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/Africa_bottom_line.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/Africa_bottom_line.pdf", height = 120, width = 200, units = 'mm')



ggplot(Africa_top, aes(x = year, 
                       y = percentage,
                       col = `Country Name`,
)) +
  geom_line(size = 1.5)+
  xlab("Year") +
  ylab("Percentage (%)")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/Africa_top_line.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/Africa_top_line.pdf", height = 120, width = 200, units = 'mm')




# Top Countries in the world
World_top <- long_curated %>% filter(percentage >= 50) 

ggplot(World_top, aes(x = `Country Name`, 
                      y = percentage,
                      fill = Region))+
  geom_bar(stat = "identity") +
  ylab(NULL)+
  xlab("Country")+
  coord_flip()
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_top_bar.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_top_bar.pdf", height = 120, width = 200, units = 'mm')


# Point graph of World top
ggplot(World_top, aes(x = year,
                      y = percentage,
                      shape = Region,
                      colour = Region,
                      label = `Country Name`))+
  geom_point()+
  xlab("Year") + ylab("Percentage")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_top_point.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_top_point.pdf", height = 120, width = 200, units = 'mm')

# geom_text(aes(label = `Country Code`))




# Bottom countries in the world
# Bar plot of World bottom
World_bottom <- long_curated %>% 
  filter(percentage <= 2)


ggplot(World_bottom, aes(x = `Country Name`, 
                         y = percentage,
                         fill = Region))+
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(x = "Country", y = "Percentage (%)")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_bottom_bar.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_bottom.bar.pdf", height = 120, width = 200, units = 'mm')



# Point graph of World bottom
ggplot(World_bottom, aes(x = year,
                         y = percentage,
                         shape = Region,
                         colour = Region))+
  geom_point()+
  labs(x = "Year", y = "Percentage (%)")
#geom_text(aes(label = 'Country Code'))
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_bottom_point.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/World_bottom_point.pdf", height = 120, width = 200, units = 'mm')


############### Statistical Analysis

# One way ANOVA
income_grp_anova <- aov(percentage ~ IncomeGroup, data = long_curated)
summary(income_grp_anova)
TukeyHSD(income_grp_anova)
plot(TukeyHSD(income_grp_anova))

# Output anova result
capture.output(summary(income_grp_anova), file = "images/IncomeGroup Anova.txt")


# Percentage dependent on region
percent_reg_aov <- aov(percentage ~ Region, data = long_curated)

summary(percent_reg_aov)
TukeyHSD(percent_reg_aov)
plot( TukeyHSD(percent_reg_aov))
capture.output(summary(percent_reg_aov), file = "images/Region Anova.txt")



# 2 Way ANOVA

IncomeGrp_region_anova <- aov(percentage ~ IncomeGroup + Region, data = long_curated)

summary(IncomeGrp_region_anova)
TukeyHSD(IncomeGrp_region_anova)
plot(TukeyHSD(IncomeGrp_region_anova))
capture.output(summary(IncomeGrp_region_anova), file = "images/IncomeRegion Anova.txt")



IncomeGrp_region_int_anova <- aov(percentage ~ IncomeGroup*Region, data = long_curated)

summary(IncomeGrp_region_int_anova)
TukeyHSD(IncomeGrp_region_int_anova)
plot(TukeyHSD(IncomeGrp_region_int_anova))
capture.output(summary(IncomeGrp_region_int_anova), file = "images/IncomeGroupinterRegion Anova.txt")



# Visualizing ANOVA
library(ggpubr)
ggboxplot(long_curated, 
          x = "Region",
          y = "percentage",
          fill = "Region",
          legend = "none",
          notch = TRUE) +
  rotate_x_text(angle = 45)+
  stat_compare_means(method = "anova", 
                     label.y = 72)+
  stat_compare_means(method = "t.test", 
                     label = "p.signif",
                     ref.group = ".all.",
                     label.y = 66)+
  labs(x = "Region", y = "Percentage (%)")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_percentANOVA_box.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/region_percentANOVA_box.pdf", height = 120, width = 200, units = 'mm')

long_curated$IncomeGroup <- factor(long_curated$IncomeGroup, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
ggboxplot(long_curated, 
          x = "IncomeGroup",
          y = "percentage",
          fill = "IncomeGroup",
          notch = TRUE,
          legend = "none") +
  rotate_x_text(angle = 45)+
  stat_compare_means(method = "anova", 
                     label.y = 72)+
  stat_compare_means(method = "t.test", 
                     label = "p.signif",
                     ref.group = ".all.",
                     label.y = 66)+
  labs(x= "Income Group", y = "Percentage (%)")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/IncomeGrp_percentANOVA_box.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/IncomeGrp_percentANOVA_box.pdf", height = 120, width = 200, units = 'mm')




ggboxplot(Sub_Africa, 
          x = "IncomeGroup",
          y = "percentage",
          fill = "IncomeGroup",
          legend = "none") +
  rotate_x_text(angle = 45)+
  stat_compare_means(method = "anova", 
                     label.y = 72)+
  stat_compare_means(method = "t.test", 
                     label = "p.signif",
                     ref.group = ".all.",
                     label.y = 66)+
  labs(x= "Income Group", y = "Percentage (%)")
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/Africa_IncomeGrp_percentANOVA_box.jpg", height = 120, width = 200, units = 'mm')
ggsave(file = "C:/Users/Yinka Okoh/Documents/R_Analysis/Women_Parliamentary/Women_Parliamentary/images/Africa_IncomeGrp_percentANOVA_box.pdf", height = 120, width = 200, units = 'mm')

