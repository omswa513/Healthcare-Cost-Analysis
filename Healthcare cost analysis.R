#Healthcare cost analysis

> library(readxl)
> X1555054100_hospitalcosts <- read_excel("D:/R Programming/Project/Healthcare cost analysis Project/1555054100_hospitalcosts.xlsx")
> View(X1555054100_hospitalcosts)

hosp_cost <- X1555054100_hospitalcosts
hosp_cost

#1) To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure

str(hosp_cost)
summary(hosp_cost)

cor(hosp_cost$AGE,hosp_cost$TOTCHG)

cor(hosp_cost$AGE,hosp_cost$APRDRG)

summary(hosp_cost$AGE)
table(hosp_cost$AGE)
hist(hosp_cost$AGE)

summary(as.factor(hosp_cost$AGE))
max(table(hosp_cost$AGE))
max(summary(as.factor(hosp_cost$AGE)))
which.max(table(hosp_cost$AGE))
age <- aggregate(TOTCHG ~ AGE, data = hosp_cost, sum)
max(age)

# Age group 0 to 1 are frequent visit in hospital and Maximum expenditure.

#2) In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.

t <- table(hosp_cost$APRDRG)
d <- as.data.frame(t)
names(d)[1] = 'Diagnosis Group'
d
which.max(table(hosp_cost$APRDRG))
which.max(t)
which.max(d)          
res <- aggregate(TOTCHG ~ APRDRG, data = hosp_cost, sum)
res
which.max(res$TOTCHG)
res[which.max(res$TOTCHG),]

# diagnosis related group 640 have the maximum expenditure.

#3) To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs

table(hosp_cost$RACE)
hosp_cost$RACE <- as.factor(hosp_cost$RACE)
fit <- lm(TOTCHG ~ RACE,data=hosp_cost)
fit
summary(fit)
fit1 <- aov(TOTCHG ~ RACE,data=hosp_cost)
summary(fit1)
hosp_cost <- na.omit(hosp_cost)
hosp_cost


#4) To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for proper allocation of resources.

table(hosp_cost$FEMALE)
a <- aov(TOTCHG ~ AGE+FEMALE,data=hosp_cost)
summary(a)
b <- lm(TOTCHG ~ AGE+FEMALE,data=hosp_cost)
summary(b)

#5) Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.

table(hosp_cost$LOS)
cat <- aov(LOS ~ AGE+FEMALE+RACE,data=hosp_cost)
summary(cat)
cat <- lm(LOS ~ AGE+FEMALE+RACE,data=hosp_cost)
summary(cat)

# Age,Female, Race are more than aplha value so not significant.

#6) To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
aov(TOTCHG ~.,data=hosp_cost)
hosp_mod <- lm(TOTCHG ~ .,data=hosp_cost)
summary(hosp_mod)

# Age, LOS and APRDRG are significant as P value is less than alpha and Female, Race are more than alpha value so not significant.





