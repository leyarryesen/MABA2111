#Rosario Rayel
#Assignment #2
#11/14/2018



#1.Define an R function that removes NA values from a vector.
remove_na<-function(sample_vec){
  set_list<-c()
  for(i in 1:length(sample_vec)){
    if(!is.na(sample_vec[i])){set_list<-c(set_list,sample_vec[i])
    }
  }
  return(set_list)
}

x<-c(NA,4,NA,6,NA,8,NA,10)
remove_na(x)


#2.Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
compinterest=function(principal,intrate=0.05,n_cpd_periods=1){return(principal*((1+intrate)**n_cpd_periods-1))}
compinterest(1000)




#3.Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.

Is.Prime<-function(num) {return (sum(num/1:num==num%/%1:num) == 2)}

x<-(3)
Is.Prime(x)


#4.Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.

date = as.POSIXct(as.Date("01/01/1970"),format = "%m/%d/%Y")

any_WDay <-function(date) {
  days<- c("Thurs","Fri","Sat","Sun","Mon","Tues","Wed")
  uncl<- ((unclass(date) %% 7) +7)
  return(days[uncl])
}

date <- as.Date("2018-11-14")
any_WDay(date)

#5. Create a function to compute for your net pay at work
net_pay =function(basic,tax_law = 0, tax_allowance = 0, lwop_days = 0, month_pay = 13, working_days = 22){
  annual = (basic + tax_law) * month_pay
  if(annual <=250000) {
    net = annual
  } else if (annual <= 400000){
    net = annual - (annual - 250000) * 0.2
  } else if (annual <= 800000) {
    net = annual - (annual - 400000) * 0.25 - 30000
  } else if (annual <= 2000000) {
    net = annual - (annual - 800000) * 0.30 - 130000
  }

  mnet = net * 1./month_pay
  net_final = mnet + tax_law - (mnet * 1./working_days) * lwop_days
  return (net_final)
}

net_pay(60000,2500,2500, 2, 13, 20)

