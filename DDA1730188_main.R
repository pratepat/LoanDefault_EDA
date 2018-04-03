# Load relevant libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)

# Load dataset in R
loan <- read.csv("loan.csv")

##################################################################
# Data Cleaning
##################################################################

summary(loan)

## Primary key and row count
length(unique(loan$id))
length(unique(loan$member_id))
# Both Id and member id are unique and can act as Primary key
# 39717 Rows and 111 columns

## Check for relevant columns. Columns without any data can be removed before starting analysis

# Define ni as opposite of in (not in)
'%ni%' <- Negate('%in%')

##########################################################################################################################
# 1: Check for columns with all NA rows

na_count <-sapply(loan, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count <- add_rownames(na_count,"Columns")
# Filter columns with more than 90% NA rows
na_count <- na_count %>%
  filter(na_count >= 39717*0.9)

# Remove columns that are present in na_count
loan <- subset(loan,select = names(loan) %ni% na_count$Columns)

##########################################################################################################################
# 2: Check for columns with only zeroes or blanks. Irrelevant for analysis.

zero_count <- sapply(loan, function(y) length(which(y == 0 | as.character(y) == "")))
zero_count <- data.frame(zero_count)
zero_count <- add_rownames(zero_count,"Columns")
# Filter columns with more than 95% irrelevant data
zero_count <- zero_count %>%
  filter(zero_count >= 39717*0.95)

zero_count %>%
  arrange(desc(zero_count))

# Remove columns that only zeroes or blanks (Has more than 95% irrelevant rows)
loan <- subset(loan,select = names(loan) %ni% zero_count$Columns)

##########################################################################################################################
# 3: Check for columns with same data throughout.
unique_count <- sapply(loan, function(y) length(unique(y,na.rm = FALSE)))
unique_count <- data.frame(unique_count)
unique_count <- add_rownames(unique_count,"Columns")
# Filter columns that have same values for all data. 
unique_count <- unique_count %>%
  filter(unique_count == 1)

unique_count
# Remove columns that have same data for all rows
loan <- subset(loan,select = names(loan) %ni% unique_count$Columns)

##########################################################################################################################
# 4: Additional columns where we cannot analyse anything.

# url, desc, zip code are also irrelevant as we cannot draw any useful insight
# title is also redundant as column pupose as lesser and clear values
# emp_title is also redundant as it contains blank and irrelevant data

loan <- subset(loan,select = -c(emp_title,url,desc,title,zip_code))

sapply(loan, function(y) length(unique(y)))

ncol(loan)
# 39 Columns

##########################################################################################################################
## Derive relevant columns for analysis

# 1: Convert Interest Rate to number
loan$int_rate_num <- as.numeric(str_replace(loan$int_rate,"\\%", "")) 

# 2: Convert Revolving Utilization percent to number
loan$revol_util_num <- as.numeric(str_replace(loan$revol_util,"\\%", "")) 

# 3: Identification if defaulter or not
loan$defaulter <- sapply(loan$loan_status, function(y) ifelse(y=="Charged Off","Yes","No"))

# 4: Draw year out of Issued Date
loan$issue_d <- paste("01-",loan$issue_d,sep = "")
loan$issue_d <- as.POSIXct(loan$issue_d, format = "%d-%b-%y")
loan$issue_d_Year <- format(loan$issue_d, "%Y")

max(loan$issue_d_Year)

# 5: Draw year out of Earliest credit line
loan$earliest_cr_line <- paste("01-",loan$earliest_cr_line,sep = "")
loan$earliest_cr_line <- as.POSIXct(loan$earliest_cr_line, format = "%d-%b-%y")
loan$earliest_cr_line_Year <- format(loan$earliest_cr_line, "%Y")

max(loan$earliest_cr_line_Year)

# Handle Year before 68 converted to 2068. Convert them to 19**
loan$earliest_cr_line_Year <- sapply(loan$earliest_cr_line_Year, function(y) ifelse(y > 2018,as.numeric(y)-100,y))

max(loan$earliest_cr_line_Year)

# 6: New Column Loan to income ratio
# Creating this variable to see as to how much percent of annual income the employee is seeking loan for
loan$LoanToIncomeRatio <- round((loan$funded_amnt/loan$annual_inc),2)

# 7: Employee Length
# Convert this column to number.
unique(loan$emp_length)

loan %>%
  group_by(emp_length) %>%
  summarise(noOfEmp = n()) %>%
  select(emp_length,noOfEmp) %>%
  arrange(desc(noOfEmp))

# We are going to include n/a in 0. As when a new customer joins LC for loan and doesnt enter anything we assume they are not employed

EmployeeLength <- function(emp_length) {
  
  emp_length <- str_split_fixed(emp_length," ",2)[1]
  
  if(emp_length == "10+") {
    return(10)
  }
  else if(emp_length == "<" | emp_length == "n/a") {
    return(0)
  }
  else
    return(emp_length)
}

loan$EmployeeLength <- as.factor(sapply(loan$emp_length, FUN = EmployeeLength))

#Lets change the order of levels to correctly view them on graphs
loan$EmployeeLength <-
  factor(
    loan$EmployeeLength,
    levels = c(0,1,2,3,4,5,6,7,8,9,10)
  )

## Create a subset for analysing defaulters
defaulters <- subset(loan,loan_status == "Charged Off")

##########################################################################################################################

##################################################################
# Univariate Analysis
##################################################################

# Lets go through different categorical columns to analyse trends
## 1: Loan_Status
unique(loan$loan_status)

# We are interested in defaulters
ggplot(loan, aes(x = loan_status,fill = defaulter)) +
  geom_bar(stat = "count") +
  geom_text(stat = "count",
            aes(label = ..count..),
            vjust = -0.5)  +
  labs(title = "Defaulters", x = "Loan Status", y = "Number of Loans")
  
# Out of the given data 5627 people have defaulted on there loans  

##########################################################################################################################
## 2: Term
unique(loan$term)

term.defaulters <- loan %>%
  group_by(term,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

# Loan Term vs No Of Loans
ggplot(loan, aes(x = term,fill=defaulter)) +
  geom_bar(stat = "count", position = "dodge") +
  geom_text(stat = "count",
             aes(label = ..count..), position = position_dodge(width = 1),
             vjust = -0.2) + 
  labs(title = "Loan Term vs No Of Loans", x = "Term", y = "Number of Loans")

# We are more interested in the ratio of defaulters
# Loan Term vs Percent of defaulters
ggplot(term.defaulters, aes(x = term, y = Percent,fill=defaulter)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")), position = position_dodge(width = 1),
            vjust = -0.3) +  
  labs(title = "Loan Term vs Percent of defaulters", x = "Loan Term", y = "Percent of Loan", face="bold") +
  theme(axis.text = element_text(face="bold"))


# Loan Term spread in different grades
termGrade.defaulters <- loan %>%
  group_by(grade,term,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

ggplot(termGrade.defaulters, aes(x = grade, y = Percent,fill=defaulter)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")), position = position_dodge(width = 1),
            vjust = -0.3) +  
  labs(title = "Loan Term spread in different grades", x = "Grade", y = "Percent of Loan", face="bold") +
  theme(axis.text = element_text(face="bold")) + 
  facet_wrap(~term)

# Count of defaulters
defaulters %>%
  group_by(term) %>%
  summarise(Number_Of_Charged_off_loans = n())

# More loans are given for 36 months term hence more no of defaulters, 
# but a larger percent of people have defaulted in 60 months term

##########################################################################################################################
## 3: Grade and subgrades
unique(loan$grade)
unique(loan$sub_grade)

grade.defaulters <- loan %>%
  group_by(grade,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

# frequency of loans for each grade
ggplot(loan, aes(x = grade,fill=defaulter)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Loan Grade vs No of Loans", x = "Grade", y = "Number of Loans")

loan %>%
  group_by(grade) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

# Grades A, B, C have almost 75% loans

# frequency of defaulters for each grade
ggplot(defaulters, aes(x = grade)) +
  geom_bar(stat = "count",fill="dark green") +
  labs(title = "Loan Grade vs No of Loans", x = "Grade", y = "Number of Loans")

# frequency of defaulters for each sub-grade
ggplot(defaulters, aes(x = grade,fill=sub_grade)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Loan Grade vs No of Loans", x = "Grade", y = "Number of Loans")

# Percent of defaulters for each grade
ggplot(subset(grade.defaulters,defaulter == "Yes"), aes(x = grade, y = Percent)) +
  geom_bar(stat = "identity",fill="dark green") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")),
            vjust = -0.4) +  
  labs(title = "Loan Grade vs Percent of defaulters", x = "Loan Grade", y = "Percent of defaulters") +
  theme(axis.text = element_text(face="bold"))
  
# The number of defaulters decrease from A to G. But the proportion increases from A to G
# Most percent of defaulters lie in E,F and G grade. Hence making it very risky.

##########################################################################################################################
## 4: Employee Length
unique(loan$EmployeeLength)

emp_length.defaulters <- loan %>%
  group_by(EmployeeLength,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

# Frequency of loans for different employee lengths
ggplot(defaulters, aes(x = EmployeeLength)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",
            aes(label = ..count..),
            vjust = -0.5) +
  labs(title = "Employee Length vs No of Loans", x = "Employee Length", y = "Number of Defaulters") +
  theme(axis.text = element_text(face="bold"))

# Percent of defaulters for different employee lengths
ggplot(subset(emp_length.defaulters,defaulter == "Yes"), aes(x = EmployeeLength, y = Percent)) +
  geom_bar(stat = "identity",fill="dark green") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")),
            vjust = -0.4) +   
  labs(title = "Years in employment vs Percent of defaulters", x = "Years in employment", y = "Percent of defaulters") +
  theme(axis.text = element_text(face="bold"))

defaulters %>%
  filter(emp_length == "n/a") %>%
  summarise(Number_Of_Charged_off_loans = n())

# The above analysis does not yield much. The proportin of defaulters in all employee lengths is nearly same

##########################################################################################################################
## 5: Home Ownership
unique(loan$home_ownership)

home_ownership.defaulters <- loan %>%
  group_by(home_ownership,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

# Frequency of loans for different Home Ownership
ggplot(defaulters, aes(x = home_ownership)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",
            aes(label = ..count..),
            vjust = -0.5) +
  labs(title = "Home Ownership Type vs No of Loans", x = "Home Ownership Type", y = "Number of Loans") +
  theme(axis.text = element_text(face="bold"))

# Percent of defaulters for different Home Ownership
ggplot(subset(home_ownership.defaulters,defaulter == "Yes"), aes(x = home_ownership, y = Percent)) +
  geom_bar(stat = "identity",fill="blue") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")),
            vjust = -0.5) +   
  labs(title = "Home Ownership Type vs Percent of defaulters", x = "Home Ownership Type", y = "Percent of defaulters") +
  theme(axis.text = element_text(face="bold"))

defaulters %>%
  group_by(home_ownership) %>%
  summarise(Number_Of_Charged_off_loans = n()) %>%
  arrange(desc(Number_Of_Charged_off_loans))

# People whose home ownwership is OTHER or RENT are a risky bet.

##########################################################################################################################
## 6: Customer Verification_Status
unique(loan$verification_status)

verification_status.defaulters <- loan %>%
  group_by(verification_status,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

# Frequency of loans for type of verification status
ggplot(defaulters, aes(x = verification_status)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",
            aes(label = ..count..),
            vjust = -0.5) +
  labs(title = "Verification Status vs No of Loans", x = "Verification Status", y = "Number of Loans") +
  theme(axis.text = element_text(face="bold"))

# Percent of defaulters for different verification status
ggplot(subset(verification_status.defaulters,defaulter == "Yes"), aes(x = verification_status, y = Percent)) +
  geom_bar(stat = "identity",fill="dark green") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")),
            vjust = -0.5) +    
  labs(title = "Verification Status vs Percent of defaulters", x = "Verification Status", y = "Percent of defaulters") +
  theme(axis.text = element_text(face="bold"))

# It is inconclusive to say that unverified sources have a bigger tendency to default

##########################################################################################################################
## 7: Issued Date
unique(defaulters$issue_d_Year)

issue_d_Year.defaulters <- loan %>%
  group_by(issue_d_Year,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

loan %>%
  group_by(issue_d_Year) %>%
  summarise(NoOfLoans = n()) %>%
  arrange(desc(NoOfLoans))

# Number of loans given each year
ggplot(defaulters, aes(x = issue_d_Year)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",
            aes(label = ..count..),
            vjust = -0.5) +
  labs(title = "Issued Year vs No of Loans", x = "Issued Year", y = "Number of Loans") +
  theme(axis.text = element_text(face="bold"))

# Percent of defaulters each year
ggplot(subset(issue_d_Year.defaulters,defaulter == "Yes"), aes(x = issue_d_Year, y = Percent)) +
  geom_bar(stat = "identity",fill="dark green") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")),
            vjust = -0.5) +   
  labs(title = "Loan Issued Year vs Percent of defaulters", x = "Loan Issued Year", y = "Percent of defaulters") +
  theme(axis.text = element_text(face="bold"))

loan %>%
  group_by(issue_d_Year) %>%
  summarise(Number_Of_loans = n()) %>%
  arrange(desc(Number_Of_loans))

# Banks have started giving more loans each year and hence the number of defaulters constantly increase which is not true 
# for percent of defaulters. It decreased from 2007-2010 and has slightly increased in 2011. This is evident fro the
# fact that there was an economic slowdown during 2007 to 2009.

##########################################################################################################################
## 8: Loan Purpose
unique(defaulters$purpose)

purpose.defaulters <- loan %>%
  group_by(purpose,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

loan %>%
  group_by(purpose) %>%
  summarise(NoOfLoans = n()) %>%
  arrange(desc(NoOfLoans))

# Number of loans given for different purpose
ggplot(defaulters, aes(x = purpose)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",
            aes(label = ..count..),
            vjust = -0.5) +
  labs(title = "Issued Year vs No of Loans", x = "Issued Year", y = "Number of Loans") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,face="bold")) 

# Percent of deafaulters Vs various type of reason given for loans
ggplot(subset(purpose.defaulters,defaulter == "Yes"), aes(x = purpose, y = Percent)) +
  geom_bar(stat = "identity",fill="dark green") +
  geom_text(stat = "identity",
            aes(label = paste(round(Percent),"%")),
            vjust = -0.2) +   
  labs(title = "Reason for Loan vs Percent of defaulters", x = "Loan Purpose", y = "Percent of defaulters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,face="bold")) 

purpose.defaulters %>%
  filter(loan_status == "Charged Off") %>%
  group_by(purpose) %>%
  arrange(desc(Percent))

# The above data shows that people asking loan by quoting small business have a higher percent of defaulters

##########################################################################################################################
## 9: Loan issuing state
unique(defaulters$addr_state)

addr_state.defaulters <- loan %>%
  group_by(addr_state,defaulter) %>%
  summarise(NoOfLoans = n()) %>%
  mutate(Percent = NoOfLoans/sum(NoOfLoans)*100)

loan %>%
  group_by(addr_state) %>%
  summarise(NoOfLoans = n()) %>%
  arrange(desc(NoOfLoans))

# number of loans approved in various states
ggplot(defaulters,aes(x = addr_state)) +
  geom_bar(stat = "count",fill="dark green") +
  labs(title = "Issued Year vs No of Loans", x = "Issued Year", y = "Number of Loans") +
  theme(axis.text = element_text(face="bold"))

# Percent of deafaulters of various states
ggplot(subset(addr_state.defaulters,defaulter == "Yes"), aes(x = addr_state, y = Percent)) +
  geom_bar(stat = "identity",fill="dark green") +
  labs(title = "Loan issuing state vs Percent of defaulters", x = "States", y = "Percent of defaulters") +
  theme(axis.text = element_text(face="bold"))

# No of loans approved in Nebraska
addr_state.defaulters %>%
  filter(addr_state == 'NE')
# Nebraska has very less loans approved to conclude anything

# From above we can conclude that most of the loans are given in California, Florida, New york and Texas
# But it is inconclusive to say that any one state has a higher percent of defaulters.

##########################################################################################################################
# Lets go through different quantitative columns to analyse trends
##########################################################################################################################
# Lets look at them one by one
## 1: Interest Rate

# Spread of Interest rate for various loan status
quantile(loan$int_rate_num, c(0,0.25,0.5,0.75,0.9,0.99,1))

ggplot(loan, aes(x = int_rate_num)) +
  geom_histogram(aes(fill = defaulter)) +
  facet_wrap(~defaulter, ncol = 1) +
  labs(title = "Interest Rates Spread", x = "Interest Rates", y = "Number of loans") +
  theme(axis.text = element_text(face="bold"))

ggplot(loan, aes(x = defaulter, y = int_rate_num, fill=defaulter)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Interest Rates Spread", x = "Defaulter (Y/N)", y = "Interest Rate") +
  theme(axis.text = element_text(face="bold"))

# Interest rates for different grades
ggplot(loan, aes(x = grade, y = int_rate_num, fill=defaulter)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Interest Rates spread for different grades", x = "Grade", y = "Interest Rate") +
  theme(axis.text = element_text(face="bold"))

# Median interest rates for different loan reasons
ggplot(defaulters, aes(x=purpose,y=int_rate_num)) + 
  geom_boxplot( fill = "light blue")  +
  labs(title = "Median interest rates for different Loan Purpose", x = "Purpose", y = "Interest Rates")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(face="bold"))

# Interest rates for different loan terms
ggplot(loan, aes(x=term,y=int_rate_num,fill=defaulter)) + 
  geom_boxplot()  +
  labs(title = "Interest rates for different loan terms", x = "Term", y = "Interest Rates")  +
  theme(axis.text = element_text(face="bold"))

# Interest rates for different verification status
ggplot(loan, aes(x=verification_status,y=int_rate_num,fill=defaulter)) + 
  geom_boxplot() +
  labs(title = "Interest rates for different loan terms", x = "Verification Status", y = "Interest Rates")  +
  theme(axis.text = element_text(face="bold"))

# Summary: All plots conclude the same thing most of the people defaulting had a very high interest rates. Much higher 
# than the ones not defaulting in all segmentations

##########################################################################################################################
## 2: Annual Income

# Lets check for outliers in annual_inc
quantile(loan$annual_inc, c(0,0.25,0.5,0.75,0.9,0.99,1))
quantile(defaulters$annual_inc, c(0,0.25,0.5,0.75,0.9,0.99,1))

# Lets remove the outliers for our analysis. Lets keep 99 quantile as the max
MaxAnnualIncAnalysis <- quantile(loan$annual_inc, c(0.99))
MaxAnnualIncAnalysis
MedianAnnualInc <- quantile(loan$annual_inc, c(0.50))

# Annual income spread
ggplot(subset(loan,annual_inc < MaxAnnualIncAnalysis), aes(x=annual_inc)) +
  geom_histogram(binwidth = 4000,aes(fill=defaulter)) +
  labs(title = "Annual income spread", x = "Annual income", y = "Frequency")  +
  theme(axis.text = element_text(face="bold"))

ggplot(subset(loan,annual_inc < MaxAnnualIncAnalysis), aes(x = defaulter, y = annual_inc, fill=defaulter)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Annual income spread", x = "Defaulter (Y/N)", y = "Annual Income") +
  theme(axis.text = element_text(face="bold"))

# Annual Income  spread in various loan grades
ggplot(subset(loan,annual_inc < MaxAnnualIncAnalysis), aes(x=grade,y=as.numeric(annual_inc),fill=defaulter)) + 
  geom_boxplot(outlier.color = "red") +
  labs(title = "Annual income spread for all grades", x = "Grade", y = "Annual Income")  +
  theme(axis.text = element_text(face="bold"))

# Annual Income  spread in various loan purpose
ggplot(subset(defaulters,annual_inc < MaxAnnualIncAnalysis), aes(x=purpose,y=as.numeric(annual_inc))) + 
  geom_boxplot(outlier.color = "red",fill = "light blue") +
  labs(title = "Annual income spread for all purpose", x = "Purpose", y = "Annual Income")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(face="bold")) 

# Annual Income  Vs DTI for defaulters
ggplot(subset(defaulters,annual_inc < MaxAnnualIncAnalysis), aes(x=dti,y=annual_inc)) + 
  geom_point(color="red") +
  labs(title = "Annual income Vs DTI", x = "DTI", y = "Annual Income") +
  scale_x_continuous(breaks = seq(0,30,by = 5)) +
  theme(axis.text = element_text(face="bold")) 

# Annual Income  Vs Revolving utilization defaulters
ggplot(subset(defaulters,annual_inc < MaxAnnualIncAnalysis & is.na(revol_util_num) == F), aes(x=revol_util_num,y=annual_inc)) + 
  geom_point(color="green") +
  labs(title = "Annual income Vs Revolving Utilization", x = "Revolving Utilization", y = "Annual Income")  +
  scale_x_continuous(breaks = seq(0,100,by = 10)) +
  scale_y_continuous(breaks = seq(0,MaxAnnualIncAnalysis,by = 25000)) +
  theme(axis.text = element_text(face="bold")) 

# Summary: Less annual income and a high revolving utilization is a bad sign. 
# LC should avoid giving loans for employees with low income and high DTI or Revolving utilization.

##########################################################################################################################
## 3: Funded Amount

# Funded amount spread across the dataset
quantile(loan$funded_amnt, c(0,0.25,0.5,0.75,0.9,0.99,1))
quantile(defaulters$funded_amnt, c(0,0.25,0.5,0.75,0.9,0.99,1))

ggplot(loan, aes(x=defaulter,y=as.numeric(loan_amnt),fill=defaulter)) + 
  geom_boxplot(outlier.color = "red") +
  labs(title = "Funded Amount Spread", x = "Funded Amount", y = "Frequency")  +
  theme(axis.text = element_text(face="bold"))

# Funded amount spread for different grades
ggplot(defaulters, aes(x=grade,y=as.numeric(loan_amnt))) + 
  labs(title = "Funded amount spread for different grades", x = "Grade", y = "Funded Amount")  +
  geom_boxplot(fill="light blue") +
  theme(axis.text = element_text(face="bold"))

# Funded amount  spread in various loan purpose
ggplot(defaulters, aes(x=purpose,y=as.numeric(loan_amnt))) + 
  labs(title = "Funded amount spread for different purpose", x = "Purpose", y = "Funded Amount")  +
  geom_boxplot(fill="light blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(face="bold")) 

# Summary: Grades E,F and G have very high median of amount funded for defaulters
# Small Business, credit card and debt consolidations have a high amount of funding amounts.

##########################################################################################################################
## 4: Revolving Utilization

# Spread of frequency for revolving utilization percent for entire population
quantile(defaulters$revol_util_num, c(0,0.25,0.5,0.75,0.9,0.99,1),na.rm = TRUE)

ggplot(subset(loan,is.na(revol_util_num) == F), aes(x=defaulter,y= revol_util_num ,fill=defaulter)) + 
  geom_boxplot() +
  labs(title = "Revolving Utilization percent", x = "Defaulter (Y/N)", y = "Revolving Utilization percent")

  # Revolving Utilization percent spread for different grades
ggplot(subset(loan,is.na(revol_util_num) == F), aes(x=grade,y= revol_util_num ,fill=defaulter)) + 
  geom_boxplot(outlier.color = "red") +
  labs(title = "Revolving Utilization percent for different Grades", 
       x = "Grade", y = "Revolving Utilization percent")+
  theme(axis.text = element_text(face="bold"))

# Revolving Utilization percent spread for different loan request types
ggplot(subset(defaulters,is.na(revol_util_num) == F), aes(x=purpose,y= revol_util_num ,fill=defaulter)) + 
  geom_boxplot() +
  labs(title = "Revolving Utilization percent for different laon purpose", 
       x = "Purpose", y = "Revolving Utilization percent") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Revolving Utilization percent spread for different home ownerships
ggplot(subset(loan, home_ownership != "NONE" & is.na(revol_util_num) == F),
  aes(x = home_ownership, y = revol_util_num , fill = defaulter)) +
  geom_boxplot() +
  labs(title = "Revolving Utilization percent for different home ownership",
       x = "Home Ownership", y = "Revolving Utilization percent") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(face="bold"))

# Summary: Overall a high revolving utilization percent is an indicator for bad loans.
# Grades E, F , G have a high revoling utilization and hence are risky
# Employees with rented apartments are risky as they have a high revoling utilization

##########################################################################################################################
## 5: Debt to income ratio (dti)

AvgDti <- round(mean(loan$dti),0)

# Spread of frequency for debt to income ratio for entire population
quantile(loan$dti, c(0,0.25,0.5,0.75,0.9,0.99,1))

# Spread of frequency for debt to income ratio for defaulters
quantile(defaulters$dti, c(0,0.25,0.5,0.75,0.9,0.99,1))

ggplot(loan, aes(x=defaulter,y=as.numeric(dti))) + 
  geom_boxplot(aes(fill = defaulter)) +
  labs(title = "Debt to income ratio", x = "Defaulter (Y/N)", y = "DTI")

# The defaulters have a higher DTI

# DTI spread for different grades
ggplot(defaulters, aes(x=grade,y=as.numeric(dti))) + 
  geom_boxplot(fill="light blue") +
  geom_hline(aes(yintercept = AvgDti),
             size = 1,
             colour = "red") +
  geom_text(aes(0,AvgDti,label = AvgDti, vjust = -0.5,hjust=-0.5)) +
  labs(title = "Debt to income ratio for different grades", x = "Grade", y = "DTI") +
  theme(axis.text = element_text(face="bold"))

# As suspected above grades E,F and G have a high DTI hence making the risky

# DTI vs Reason for loan application
ggplot(defaulters, aes(x = purpose, y = as.numeric(dti))) +
  geom_boxplot(fill = "light blue") +
  geom_hline(aes(yintercept = AvgDti),
             size = 1,
             colour = "red") +
  labs(title = "Debt to income ratio for different loan request", x = "Reason for Loan", y = "DTI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(face = "bold"))

# Clearly people already in debt (Higher DTI) are requesting for more loan

# Summary: A higher DTI is an indicator for risky loans overall but especially in case of grades E,F,G and 
# purpose as debt consolidation or credit card

##########################################################################################################################

## 6: Loan to income ratio

# Spread of frequency for debt to income ratio for entire population
quantile(loan$LoanToIncomeRatio, c(0,0.25,0.5,0.75,0.9,0.99,1))

ggplot(loan, aes(x=defaulter,y=as.numeric(LoanToIncomeRatio))) + 
  geom_boxplot(aes(fill = defaulter)) +
  labs(title = "Loan Amount to income ratio", x = "Defaulter (Y/N)", y = "Loan to Income Ratio")

ggplot(loan, aes(x = purpose, y = LoanToIncomeRatio, fill=defaulter)) + 
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Loan Amount to income ratio for different loan request", x = "Reason for Loan", y = "Loan Amount to income ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Loan Amount to income ratio spread for different grades
ggplot(loan, aes(x=grade,y=LoanToIncomeRatio)) + 
  geom_boxplot(aes(fill = grade)) +
  facet_wrap(~defaulter) +
  labs(title = "Loan Amount to income ratio for different grades", x = "Grade", y = "Loan Amount to income ratio")

# Summary: People asking loan which is a high percent of there income especially applying for small busines as a purpose

##########################################################################################################################

##################################################################
# Bivariate Analysis
##################################################################

#Correlation matrix between all 

cor.loan <- loan %>%
  select(
    funded_amnt,
    installment,
    total_pymnt,
    int_rate_num,
    annual_inc,
    dti,
    open_acc,
    total_acc,
    revol_bal,
    revol_util_num
  ) %>%
  rename(
    Funded_Amount = funded_amnt,
    Installment = installment,
    Total_Payment = total_pymnt,
    Interest_Rate = int_rate_num,
    Annual_Income = annual_inc,
    DTI = dti,
    Open_Accounts = open_acc,
    Toatal_Accounts = total_acc,
    Revolving_Balance = revol_bal,
    Revolving_Utilization = revol_util_num
  )

sapply(cor.loan, function(y) sum(length(which(is.na(y)))))

cormat <- round(cor(cor.loan,use = "complete.obs"),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

melted_cormat %>%
  filter(value >= 0.8 & Var1 != Var2)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#looking at defaulters
cor.defaulters <- defaulters %>%
  select(
    funded_amnt,
    installment,
    total_pymnt,
    int_rate_num,
    annual_inc,
    dti,
    open_acc,
    total_acc,
    revol_bal,
    revol_util_num
  ) %>%
  rename(
    Funded_Amount = funded_amnt,
    Installment = installment,
    Total_Payment = total_pymnt,
    Interest_Rate = int_rate_num,
    Annual_Income = annual_inc,
    DTI = dti,
    Open_Accounts = open_acc,
    Toatal_Accounts = total_acc,
    Revolving_Balance = revol_bal,
    Revolving_Utilization = revol_util_num
  )

sapply(cor.defaulters, function(y) sum(length(which(is.na(y)))))

cormat_defaulters <- round(cor(cor.defaulters,use = "complete.obs"),2)
head(cormat_defaulters)

melted_cormat_defaulters <- melt(cormat_defaulters)
head(melted_cormat_defaulters)

melted_cormat_defaulters %>%
  filter(value >= 0.5 & Var1 != Var2)

melted_cormat_defaulters %>%
  filter(value < 0 & Var1 != Var2)

ggplot(data = melted_cormat_defaulters, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Summary: Funded amount has a high correlation with installment and total payment

