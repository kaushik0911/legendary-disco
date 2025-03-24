bank_data = read.csv("Bank Churn Data CMM703.csv")
colnames(bank_data)
dim(bank_data)

head(bank_data, 10)
# by looking at the dataset description its seems that HasCrCard, IsActiveMember, and Exited are categorical variables
# lest convert them into categorical variables using factors

bank_data$HasCrCard = factor(
  bank_data$HasCrCard,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

bank_data$IsActiveMember = factor(
  bank_data$IsActiveMember,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

bank_data$Exited = factor(
  bank_data$Exited,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# lets do a quick summary statistics
# i install vtable package because it seems more eye plesent
# install.packages('vtable')
library(vtable)
?sumtable
sumtable(bank_data)

head(bank_data)

exited_customers = subset(bank_data, Exited == "Yes")
boxplot(exited_customers$Age)
?boxplot
