empty.DataFrame <- data.frame()
empty.DataFrame
(v1 <- 1:10)
v2 <- letters[1:10]
v2
(df <- data.frame(col.name.1 = v1, col.name.2 = v2))


### Reading CSV in 2 Ways ###
gpw3_2010_csv <- read.csv("/Users/chirag/Desktop/Spring21/DA/Week2/GPW3_GRUMP_SummaryInformation_2010.csv")
gpw3_2010_csv
View(gpw3_2010_csv)
head(gpw3_2010_csv)
gpw3_2010_csv[1:5,1:8]
gpw3_2010_csv1 <- file.choose()
gpw3_2010_csv1[1:5,1:8]


### Reading Excel With 'readxl' Library ###
install.packages("readxl")
library("readxl")
file_xls <- "/Users/chirag/Desktop/Spring21/DA/Week2/GPW3_GRUMP_SummaryInformation_2010.xls"
gpw3_2010_xls <- read_excel(file_xls, sheet = "Summary Information")
gpw3_2010_xls[1:5,1:8]


### Reading Excel With 'xlsx' Library ###
install.packages("xlsx")
library("xlsx")
gpw3_2010_xls <- read.xlsx(file, 2, header=TRUE)
gpw3_2010_xls[1:5,1:8]
boxplot(gpw3_2010_xls$Continent)
boxplot(gpw3_2010_xls$PopulationPerUnit)
hist(gpw3_2010_xls$Continent, prob = T, main = "Histogram of Number of Countries in Each Continent")
hist(gpw3_2010_xls$Continent, main = "Histogram of Number of Countries in Each Continent")
plot(gpw3_2010_xls$Continent, main = "Continent Plot")


### Exercise 1: Exploring the Distribution ###

### EPI Dataset Analysis ###
epi_data = read.csv('/Users/chirag/Desktop/Spring21/DA/Week1/EPI_data.csv', header = T)
epi_data[1:5,1:8]
summary(epi_data)
fivenum(epi_data)  ## Error due to binary operator
fivenum(epi_data$Population07, na.rm = T)
?stem(epi_data$Population07)
hist(epi_data$Landarea, breaks = 20, xlab = 'LandArea', main = 'Histogram of LandArea')
lines(density(epi_data$Landarea,na.rm=TRUE,bw=1))
lines(density(epi_data$Landarea,na.rm=TRUE,bw="SJ"))
rug(epi_data$Population07, col = "Red")


### Removing NA Values From EPI Column ###
fix(epi_data)
epi_epi <- epi_data$EPI
epi_epi
(tf <- is.na(epi_epi))
epi_epi_wo_na <- epi_epi[!is.na(epi_epi)]
epi_epi_wo_na


### Different Types of Plots ###
plot(ecdf(epi_epi), do.points = T, verticals = T)
plot(ecdf(epi_epi_wo_na), do.points = T, verticals = T)
plot(ecdf(epi_epi_wo_na), do.points = F, verticals = T)
par(pty="m")
qqnorm(epi_epi)
qqline(epi_epi, col = "Red")

(x<-seq(30,95,1))
ppoints(10)
qqplot(qt(ppoints(100), df = 8), x, xlab = "Q-Q plot for t dsn")
# In short qt() gives probability of t-value = ppoints(100) with df degrees of freedom
qqplot(qt(ppoints(500), df = 8), x, xlab = "Q-Q plot for t dsn")
qqplot(qt(ppoints(100), df = 50), x, xlab = "Q-Q plot for t dsn", col = 'Orange')
qqline(x, col = 'Blue')


### Removing NA Values From DALY, WATER_H Columns ###
names(epi_data)
epi_data[0:2,c("DALY","WATER_H")]
(epi_daly <- epi_data$DALY)
(tf <- is.na(epi_daly))
(epi_daly_wo_na <- epi_daly[!is.na(epi_daly)])

(epi_water_h <- epi_data$WATER_H)
(tf <- is.na(epi_water_h))
(epi_water_h_wo_na <- epi_water_h[!is.na(epi_water_h)])


### ECDF Plots ###
plot(ecdf(epi_daly), do.points = T, verticals = T)
plot(ecdf(epi_daly_wo_na), do.points = T, verticals = T)
plot(ecdf(epi_daly_wo_na), do.points = F, verticals = T)
par(pty="m")
qqnorm(epi_daly)
qqline(epi_daly, col = "Red")

plot(ecdf(epi_water_h), do.points = T, verticals = T)
plot(ecdf(epi_water_h_wo_na), do.points = T, verticals = T)
plot(ecdf(epi_water_h_wo_na), do.points = F, verticals = T)
par(pty="m")
qqnorm(epi_water_h)
qqline(epi_water_h, col = "Red")

### Box-Plot & QQ-Plot For Different Variables ###
boxplot(epi_epi, epi_daly, epi_water_h)
qqplot(epi_epi, epi_daly, epi_water_h)
qqplot(epi_epi, epi_daly)
qqplot(epi_water_h, epi_daly)
qqplot(epi_water_h, epi_epi)


### Exercise 2: Filtering (Populations) ###

### EPI Dataset Analysis ###
attach(epi_data)
(epi_land <- !epi_data[Landlock])
(epi_land_true <- epi_land[!is.na(epi_land)])
(epi_desert <- !epi_data[Desert])
(epi_desert_true <- epi_desert[!is.na(epi_desert)])

(epi_reg_south_asia <- epi_data[epi_data$EPI_regions %in% c('South Asia'), ])
(epi_sub_reg_caribbean <- epi_data[epi_data$GEO_subregion %in% c('Caribbean'), ])


### GPW Dataset Analysis ###

### Exercise 1: Exploring the Distribution ###
View(gpw3_2010_xls)
boxplot(gpw3_2010_xls$Continent,gpw3_2010_xls$PopulationPerUnit)
hist(gpw3_2010_xls$Continent, prob = T, main = "Histogram of Number of Countries in Each Continent")
hist(gpw3_2010_xls$Continent, main = "Histogram of Number of Countries in Each Continent")
plot(gpw3_2010_xls$Continent, main = "Continent Plot")
summary(gpw3_2010_xls)
fivenum(gpw3_2010_xls$PopulationPerUnit, na.rm = T)

hist(gpw3_2010_xls$PopulationPerUnit, breaks = 10, xlab = 'PopulationPerUnit', main = 'Histogram of PopulationPerUnit')
lines(density(gpw3_2010_xls$PopulationPerUnit,na.rm=TRUE,bw=1))
lines(density(gpw3_2010_xls$PopulationPerUnit,na.rm=TRUE,bw="SJ"))
rug(gpw3_2010_xls$PopulationPerUnit, col = "Red")


### Removing NA Values From DALY, WATER_H Columns ###
names(gpw3_2010_xls)
gpw3_2010_xls[0:2,c('Resolution')]
(gpw_reso <- gpw3_2010_xls$Resolution)
(tf <- is.na(gpw_reso))
(gpw_reso_wo_na <- gpw_reso[!is.na(gpw_reso)])


### ECDF Plots ###
plot(ecdf(gpw_reso), do.points = T, verticals = T)
plot(ecdf(gpw_reso_wo_na), do.points = T, verticals = T)
par(pty="m")
qqnorm(gpw_reso)
qqline(gpw_reso, col = "Red")


### Box-Plot & QQ-Plot For Different Variables ###
boxplot(gpw3_2010_xls$Continent,gpw3_2010_xls$PopulationPerUnit)
qqplot(gpw3_2010_xls$Continent,gpw3_2010_xls$PopulationPerUnit)
qqplot(gpw3_2010_xls$PopulationPerUnit,gpw3_2010_xls$Continent)
# interchanging the position of variables, reverses the axis of view


### Exercise 2: Filtering (Populations) ###
(gpw_res_1 <- gpw3_2010_xls[gpw3_2010_xls$Resolution > 100, ])
boxplot(gpw3_2010_xls$Resolution,gpw_res_1$Resolution)
(gpw_un_reg_caribbean <- gpw3_2010_xls[gpw3_2010_xls$UNRegion %in% c('Caribbean'), ])


### Water.csv Dataset Analysis ###
water_csv_file = '/Users/chirag/Desktop/Spring21/DA/Week2/water-treatment.csv'
water_csv <- read.csv(water_csv_file)

### Exercise 1: Exploring the Distribution ###
View(water_csv)
(water_csv_ssp <- water_csv[water_csv$SS.P, ])
(water_csv_sedp <- water_csv[water_csv$SED.P, ])
fivenum(water_csv$SS.P)
boxplot(water_csv_ssp$SS.P)
hist(water_csv_ssp$SS.P, prob = T, main = "Histogram of SS.P")


### Removing NA Values ###
water_csv[0:2,c('SS.P')]
(water_csv_ssp_1 <- water_csv$SS.P)
(tf <- is.na(water_csv_ssp_1))
(water_csv_ssp_wo_na <- water_csv_ssp_1[!is.na(water_csv_ssp_1)])


### Exercise 2: Filtering (Populations) and ECDF Plots ###
plot(ecdf(water_csv_ssp_1), do.points = T, verticals = T)
par(pty="m")
qqnorm(water_csv_ssp_1)
qqline(water_csv_ssp_1, col = "Red")

(water_csv_ssp_1 <- water_csv[water_csv$SS.P > 300, ])
boxplot(water_csv$SS.P,water_csv_ssp_1$SS.P)