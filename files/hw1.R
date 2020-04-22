
install.packages("data.table")
library(data.table)
install.packages("xlsx")
install.packages("zoo")
install.packages("xts")
install.packages("ggpubr")



library(zoo)
library(xts)
library(readxl)
library(ggpubr)

data <- read_excel("C:/Users/LENOVO/Desktop/EVDS.xlsx")
View(data)
dates<- seq(from=as.Date("2016-10-01"),to=as.Date("2019-10-31"),by="months")
genelxts<-xts(x=data[1:37,],order.by=dates)

card_exp <- data[1:37,2]  
cardxts<- xts(x=card_exp,order.by=dates) #xts object for card usage
  storage.mode(cardxts) <- "numeric"
  str(cardxts)
  
loan_interest<- data[1:37,3]     
loanxts<- xts(x=loan_interest,order.by=dates) #xts object for interest rates of loans
storage.mode(loanxts)<- "numeric"


price_index <- data[1:37,4]       
pricexts <- xts(x=price_index,order.by=dates) #xts object for price indexes
storage.mode(pricexts)<- "numeric"

unemp_rate <-data[1:37,5] 
unempxts <- xts(x=unemp_rate,order.by=dates) #xts object for unemployement rates
storage.mode(unempxts)<- "numeric"

  
plot(cardxts)  #plot the 4 xts objects
plot(loanxts) 
plot(pricexts)
plot(unempxts)

merge(cardxts,loanxts)  #compare cardxts wit the other xts object 
merge(cardxts,pricexts)
merge(cardxts,unempxts)



qqplot(loanxts,cardxts)  #qqplots of cardxts against other xts objects
qqplot(pricexts,cardxts)
qqplot(unempxts,cardxts)



cor1  <- cor.test(cardxts, loanxts,   method = "pearson") #correlation measurments between cardxts and other xts objects
cor2  <- cor.test(cardxts, pricexts,   method = "pearson")
cor3  <- cor.test(cardxts, unempxts,   method = "pearson")


periodicity(cardxts) #periodicity calculations of xts objects (check if they are monthly)
periodicity(loanxts)
periodicity(pricexts)
periodicity(unempxts)


