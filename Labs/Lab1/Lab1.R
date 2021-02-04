empty.DataFrame <- data.frame()
empty.DataFrame
(v1 <- 1:10)
v2 <- letters[1:10]
v2
(df <- data.frame(col.name.1 = v1, col.name.2 = v2))

# Reading CSV in 2 ways
gpw3_2010_csv <- read.csv("/Users/chirag/Desktop/Spring21/DA/Week2/GPW3_GRUMP_SummaryInformation_2010.csv")
gpw3_2010_csv
View(gpw3_2010_csv)
head(gpw3_2010_csv)
gpw3_2010_csv[1:5,1:8]
gpw3_2010_csv1 <- file.choose()
gpw3_2010_csv1[1:5,1:8]

# Reading Excel with readxl Library
install.packages("readxl")
library("readxl")
file_xls <- "/Users/chirag/Desktop/Spring21/DA/Week2/GPW3_GRUMP_SummaryInformation_2010.xls"
gpw3_2010_xls <- read_excel(file_xls, sheet = "Summary Information")
gpw3_2010_xls[1:5,1:8]

# Reading Excel with xlsx Library
install.packages("xlsx")
library("xlsx")
gpw3_2010_xls <- read.xlsx(file, 2, header=TRUE)
gpw3_2010_xls[1:5,1:8]

boxplot(gpw3_2010_xls$Continent)
boxplot(gpw3_2010_xls$PopulationPerUnit)
hist(gpw3_2010_xls$Continent, prob = T)
hist(gpw3_2010_xls$Continent)
plot(gpw3_2010_xls$Continent)


#Exercise 1: exploring the distribution
# EPI Dataset Analysis
epi_data = read.csv('/Users/chirag/Desktop/Spring21/DA/Week1/EPI_data.csv', header = T)
epi_data[1:5,1:8]
summary(epi_data)
fivenum(epi_data)  ## Error due to binary operator
fivenum(epi_data$Population07)
?stem(epi_data$Population07)
hist(epi_data$Landarea, breaks = 20, prob=TRUE)
lines(density(epi_data$Landarea,na.rm=TRUE,bw=1))
lines(density(epi_data$Landarea,na.rm=TRUE,bw="SJ"))
rug(epi_data$Population07, col = "Red")

# Removing NA values from EPI column
fix(epi_data)
epi_col <- epi_data$EPI
epi_col
(tf <- is.na(epi_col))
epi_col_wo_na <- epi_col[!is.na(epi_col)]
epi_col_wo_na
