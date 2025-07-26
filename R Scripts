data <- read.csv("C:\\Users\\HP\\Documents\\urban_development_housing_500.csv")
head(data)
str(data)
**DESCRIPTIVE ANALYSIS**
**HOUSING PRICE TREND**
HP<-table(data$Housing_Price_Trend)
HP
PHP<-prop.table(HP)*100
PHP
pie(PHP)
**HOUSING AVAILABILTY**
HA<-table(data$Housing_Availability)
HA
PHA<-prop.table(HA)*100
PHA
barplot(PHA)
**FOR URBAN DEVELOPMENT LEVEL**
UD
UD<-table(data$Urban_Development_Level)
PUD<-prop.table(UD)*100
PUD
pie(PUD)

**REGGRESSION ANALYSIS**
data$Housing_Price_Numeric <- as.numeric(factor(data$Housing_Price_Trend))
data$Urban_Development_Level_Numeric<-as.numeric(factor(data$Urban_Development_Level))
regression_model <- lm(Housing_Price_Numeric ~ Urban_Development_Level_Numeric, data = data)
summary(regression_model)


**CHI SQUARE TEST**
contingency_table<-table(data$Urban_Development_Level, data$Housing_Availability)
chi_sq_test <- chisq.test(contingency_table)
print(chi_sq_test)

CAA<-table(data$Challenges_Affecting_Availability)
CAA
PCCA<-prop.table(CAA)*100
PCCA
pie(PCCA)

LIH<-table(data$Low_Income_Housing_Access)
LIH
PLIH<-prop.table(LIH)*100
PLIH
barplot(PLIH)

UDD<-table(data$Urban_Development_Displacement)
UDD
PUDD<-prop.table(UDD)*100
PUDD
barplot(PUDD)

PR<-table(data$Policy_Recommendation)
PR
PPR<-prop.table(PR)*100
PPR
pie(PPR)

