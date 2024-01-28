library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(coefplot)
library(car)


my_data_sheet6 <- readxl::read_excel("C:/Users/SHRAVAN/Downloads/Updated Datasets.xlsx",sheet='Sheet6')
my_data_sheet6$UPIVolume <- my_data_sheet6$'UPI Volume (in Mn)'
my_data_sheet6$UPIValuepertransaction <- my_data_sheet6$'Value per transaction'
my_data_sheet6$UPIVal <- my_data_sheet6$'UPIValue'
my_data_sheet6$DepositPerAccount <- my_data_sheet6$'Deposit per account'
my_data_sheet6$TotalAccounts <- my_data_sheet6$'Total accounts (in Mn)'

colnames(my_data_sheet6)

model_sheet6_depositperaccount <- lm(DepositPerAccount ~ UPIVolume + UPIValuepertransaction, data=my_data_sheet6)
model_sheet6_totalaccounts <- lm(TotalAccounts ~ UPIVolume + UPIValuepertransaction, data=my_data_sheet6)

model_sheet6_depos <- lm(DepositPerAccount ~ UPIVolume + UPIVal, data=my_data_sheet6)

vif(model_sheet6_depos)

summary(model_sheet6_depositperaccount)
summary(model_sheet6_totalaccounts)
summary(model_sheet6_totalaccounts)


ggplot(model_sheet6_depositperaccount,aes(UPIVolume, DepositPerAccount)) +
  geom_point(colour='red', shape=16) +
  geom_smooth(method='lm')+
  labs(title="UPI Impact on Deposit per Account", x="UPI Transaction Volume", y="Deposit Per Account")+
  theme_minimal()

ggplot(model_sheet6_totalaccounts,aes(UPIVolume, TotalAccounts)) +
  geom_point(colour='red', shape=16) +
  geom_smooth(method='lm')+
  labs(title="UPI Impact on Total number of Accounts", x="UPI Transaction Volume", y="Total number of Accounts")+
  theme_minimal()

install.packages("carData")
library(car)
vif(model_sheet6_totalaccounts)

#Small Credit
my_data_sheet7 <- readxl::read_excel("C:/Users/SHRAVAN/Downloads/Updated Datasets.xlsx",sheet='Sheet7')

colnames(my_data_sheet7)

my_data_sheet7$CredAcc <- my_data_sheet7$'Total Credit accounts'
my_data_sheet7$CredRec <- my_data_sheet7$'Total Credit received (In Cr)'
my_data_sheet7$UPIVolume <- my_data_sheet7$'Trailing 3 month UPI transactions volume (In Mn)'
my_data_sheet7$UPIValue <- my_data_sheet7$'value per transaction'

View(my_data_sheet7)

model_sheet7_CredAcc <- lm(CredAcc ~ UPIVolume + UPIValue, data=my_data_sheet7)
model_sheet7_CredRec <- lm(CredRec ~ UPIVolume + UPIValue, data=my_data_sheet7)

summary(model_sheet7_CredAcc)
summary(model_sheet7_CredRec)

ggplot(model_sheet7_CredAcc,aes(UPIVolume, CredAcc)) +
  geom_point(colour='red', shape=16) +
  geom_smooth(method='lm')+
  labs(title="UPI Impact on Total Credit accounts", x="UPI Transaction Volume", y="Total Credit accounts")+
  theme_minimal()

ggplot(model_sheet7_CredRec,aes(UPIVolume, CredRec)) +
  geom_point(colour='red', shape=16) +
  geom_smooth(method='lm')+
  labs(title="UPI Impact on Total Credit Received", x="UPI Transaction Volume", y="Total Credit Received")+
  theme_minimal()



#AePS 
my_data_sheet8 <- readxl::read_excel("C:/Users/SHRAVAN/Downloads/Updated Datasets.xlsx",sheet='AePS data')

colnames(my_data_sheet8)

my_data_sheet8$janAcc <- my_data_sheet8$'Total accounts (in Mn)...30'
my_data_sheet8$jandpst <- my_data_sheet8$'Total deposits (in lac)'
my_data_sheet8$aepsVolume <- my_data_sheet8$'Total Approved Transaction(In Mn'


View(my_data_sheet7)

model_sheet8_janAcc <- lm(janAcc ~ aepsVolume, data=my_data_sheet8)
model_sheet8_jandpst <- lm(jandpst ~ aepsVolume, data=my_data_sheet8)

summary(model_sheet8_janAcc)
summary(model_sheet8_jandpst)

ggplot(model_sheet8_janAcc,aes(aepsVolume, janAcc)) +
  geom_point(colour='red', shape=16) +
  geom_smooth(method='lm')+
  labs(title="AePS Impact on Total Number of Accounts", x="Total AePS Approved Transaction(In Mn)", y="Total accounts (in Mn)")+
  theme_minimal()



