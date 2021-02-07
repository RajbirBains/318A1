# Cmpt 318 Assignment 1
# Rajbir Singh Bains
# Sukhwinder Singh
# Tej Singh Pooni


#datacollected = read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")

dcollectedg7 = read.table("Group_7_Dataset_A1.txt", header = TRUE, sep = ",") #Accessing data

data_cols = as.POSIXlt(dcollectedg7$Date, format = "%d/%m/%Y")


# Calculating the mean
meanA = mean(dcollectedg7$Global_active_power)
meanB = mean(dcollectedg7$Global_reactive_power)
meanC = mean(dcollectedg7$Voltage)


print("Arithmetic Mean")
print(meanA)
print(meanB)
print(meanC)


# Geometric mean calculations 

gmeanA = geoMean(dcollectedg7$Global_active_power)
gmeanB = geoMean(dcollectedg7$Global_reactive_power) #Value should be zero but is giving NA
gmeanC = geoMean(dcollectedg7$Voltage)


print("Geometric Mean")

print(gmeanA)
print(gmeanB) # Assume value is 0 but function gives NA
print(gmeanC)


# Calculating the standard deviation
sdA = sd(dcollectedg7$Global_active_power)
sdB = sd(dcollectedg7$Global_reactive_power)
sdC = sd(dcollectedg7$Voltage)

print("Standard Deviation")
print(sdA)
print(sdB)
print(sdC)




# TIME DEFINITION
#Day -> 6AM - 6PM
#Night -> 6PM - 6AM
#"12/2/2007" is Monday start

# 
# 05:59:00 #Night
# bool(IsDay?) -> FALSE
# 06:00:00 #Day
# bool -> TRUE
# 18:00:00 #Day
# bool -> FALSE
# 18:01:00 #Night




print("FOR A (Global_active_power)")

weekendmaxAday = 0
weekendmaxAnight = 0
daycounter = 7200
isDay = FALSE #Night time
for(x in dcollectedg7$Date){
  if(x == "17/2/2007" || x == "18/2/2007"){ # Checks if it is the weekend
    daycounter = daycounter + 1
    if(dcollectedg7$Time[daycounter] == "06:00:00"){ # Checks if it is day or night
      isDay = TRUE
    }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
      isDay = FALSE
    }
    if(isDay == FALSE){ # night time
      
      if(weekendmaxAnight < dcollectedg7$Global_active_power[daycounter]){
        weekendmaxAnight = max(weekendmaxAnight,dcollectedg7$Global_active_power[daycounter]) # Finds max for night
      }
      
    }else{
      
      if(weekendmaxAday < dcollectedg7$Global_active_power[daycounter]){
        weekendmaxAday = max(weekendmaxAday,dcollectedg7$Global_active_power[daycounter]) # Finds max for day
      }
    }

    
  }
}

# Printing
print("WEEKENDMAXA")
print(paste("Day ", weekendmaxAday))
print(paste("Night ",weekendmaxAnight))


weekendminAday = weekendmaxAday
weekendminAnight = weekendmaxAnight
daycounter = 7200
isDay = FALSE
for(x in dcollectedg7$Date){
  if(x == "17/2/2007" || x == "18/2/2007" ){
    daycounter = daycounter + 1
    
    if(dcollectedg7$Time[daycounter] == "06:00:00"){
      isDay = TRUE
    }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
      isDay = FALSE
    }
    if(isDay == FALSE){
      
      if(weekendminAnight > dcollectedg7$Global_active_power[daycounter]){
        weekendminAnight = min(weekendminAnight,dcollectedg7$Global_active_power[daycounter])
      }
      
    }else{
      
      if(weekendminAday > dcollectedg7$Global_active_power[daycounter]){
        weekendminAday = min(weekendminAday,dcollectedg7$Global_active_power[daycounter])
      }
    }
    
    
  }
}

print("WEEKENDMINA")
print(paste("Day ",weekendminAday))
print(paste("Night ",weekendminAnight))


# FOR DAYS OF WEEK

weekdaymaxAday = 0
weekdaymaxAnight = 0
daycounter = 0
for(x in dcollectedg7$Date){
  daycounter = daycounter + 1
  if(dcollectedg7$Time[daycounter] == "06:00:00"){
    isDay = TRUE
  }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
    isDay = FALSE
  }
  if(isDay == FALSE){
    if(weekdaymaxAnight < dcollectedg7$Global_active_power[daycounter]){
      weekdaymaxAnight = max(weekdaymaxAnight,dcollectedg7$Global_active_power[daycounter])
    }
  }
  if(weekdaymaxAday < dcollectedg7$Global_active_power[daycounter]){
    weekdaymaxAday = max(weekdaymaxAday,dcollectedg7$Global_active_power[daycounter])
  }
  
  if(x == "17/2/2007"){
    break
  }
}
print("WEEKDAYMAXA")
print(paste("Day ",weekdaymaxAday))
print(paste("Night ",weekdaymaxAnight))



weekdayminAday = weekdaymaxAday
weekdayminAnight = weekdaymaxAnight
daycounter = 0

for(x in dcollectedg7$Date){
  daycounter = daycounter + 1
  if(dcollectedg7$Time[daycounter] == "06:00:00"){
    isDay = TRUE
  }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
    isDay = FALSE
  }
  if(isDay == FALSE){
    if(weekdayminAnight > dcollectedg7$Global_active_power[daycounter]){
      weekdayminAnight = min(weekdayminAnight,dcollectedg7$Global_active_power[daycounter])
    }
  }else{
    if(weekdayminAday > dcollectedg7$Global_active_power[daycounter]){
      weekdayminAday = min(weekdayminAday,dcollectedg7$Global_active_power[daycounter])
    }
  }
  
  
  if(x == "17/2/2007"){
    break
  }
  
}
print("WEEKDAYMINA")
print(paste("Day ",weekdayminAday))
print(paste("Night ", weekdayminAnight))


#FOR B (Global_reactive_power)
print("FOR B (Global_reactive_power)")
weekendmaxBday = 0
weekendmaxBnight = 0
daycounter = 7200
isDay = FALSE
for(x in dcollectedg7$Date){
  if(x == "17/2/2007" || x == "18/2/2007" ){
    daycounter = daycounter + 1
    if(dcollectedg7$Time[daycounter] == "06:00:00"){
      isDay = TRUE
    }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
      isDay = FALSE
    }
    if(isDay == FALSE){
      
      if(weekendmaxBnight < dcollectedg7$Global_reactive_power[daycounter]){
        weekendmaxBnight = max(weekendmaxBnight,dcollectedg7$Global_reactive_power[daycounter])
      }
    }else{
      if(weekendmaxBday < dcollectedg7$Global_reactive_power[daycounter]){
        weekendmaxBday = max(weekendmaxBday,dcollectedg7$Global_reactive_power[daycounter])
      }
    }
    
  }
}
print("WEEKENDMAXB")
print(paste("Day ",weekendmaxBday))
print(paste("Night ", weekendmaxBnight))



weekendminBday = weekendmaxBday
weekendminBnight = weekendmaxBnight
daycounter = 7200
for(x in dcollectedg7$Date){
  if(x == "17/2/2007" || x == "18/2/2007"){
    daycounter = daycounter + 1
    if(dcollectedg7$Time[daycounter] == "06:00:00"){
      isDay = TRUE
    }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
      isDay = FALSE
    }
    if(isDay == FALSE){
      if(weekendminBnight > dcollectedg7$Global_reactive_power[daycounter]){
        weekendminBnight = min(weekendminBnight,dcollectedg7$Global_reactive_power[daycounter])
      }
    }else{
      if(weekendminBday > dcollectedg7$Global_reactive_power[daycounter]){
        weekendminBday = min(weekendminBday,dcollectedg7$Global_reactive_power[daycounter])
      }
    }
    
  }
}

print("WEEKENDMINB")
print(paste("Day ",weekendminBday))
print(paste("Night ", weekendminBnight))



weekdaymaxBday = 0
weekdaymaxBnight = 0
daycounter = 0
for(x in dcollectedg7$Date){
  daycounter = daycounter + 1
  if(dcollectedg7$Time[daycounter] == "06:00:00"){
    isDay = TRUE
  }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
    isDay = FALSE
  }
  if(isDay == FALSE){
    if(weekdaymaxBnight < dcollectedg7$Global_reactive_power[daycounter]){
      weekdaymaxBnight = max(weekdaymaxBnight,dcollectedg7$Global_reactive_power[daycounter])
    }
  }else{
    if(weekdaymaxBday < dcollectedg7$Global_reactive_power[daycounter]){
      weekdaymaxBday = max(weekdaymaxBday,dcollectedg7$Global_reactive_power[daycounter])
    }
  }
    if(x == "17/2/2007"){ # Reaches the weekend
      break 
    }
}

print("WEEKDAYMAXB")
print(paste("Day ", weekdaymaxBday))
print(paste("Night ", weekdaymaxBnight))


weekdayminBday = weekdaymaxBday
weekdayminBnight = weekdaymaxBnight
daycounter = 0
for(x in dcollectedg7$Date){
  daycounter = daycounter + 1
  if(dcollectedg7$Time[daycounter] == "06:00:00"){
    isDay = TRUE
  }else if(dcollectedg7$Time[daycounter] == "18:00:00"){
    isDay = FALSE
  }
  if(isDay == FALSE){
    if(weekdayminBnight > dcollectedg7$Global_reactive_power[daycounter]){
      weekdayminBnight = min(weekdayminBnight,dcollectedg7$Global_reactive_power[daycounter])
    }
  }else{
    if(weekdayminBday > dcollectedg7$Global_reactive_power[daycounter]){
      weekdayminBday = min(weekdayminBday,dcollectedg7$Global_reactive_power[daycounter])
    }
  }
    if(x == "17/2/2007"){
        break
    }
    
}
print("WEEKDAYMINB")
print(paste("Day ",weekdayminBday))
print(paste("Night ",weekdayminBnight))



#PART 2

print("Part 2 (Correlation)")

#correlation cor(var1, var2, method = "") 
#combinations starting w/ a 
a_corr_a = cor(dcollectedg7$Global_active_power,dcollectedg7$Global_active_power)
a_corr_b = cor(dcollectedg7$Global_reactive_power,dcollectedg7$Global_active_power)
print(a_corr_b)

a_corr_c = cor(dcollectedg7$Global_active_power,dcollectedg7$Voltage)
print(a_corr_c)

a_corr_d = cor(dcollectedg7$Global_active_power,dcollectedg7$Global_intensity)
print(a_corr_d)

a_corr_e = cor(dcollectedg7$Global_active_power,dcollectedg7$Sub_metering_1)
print(a_corr_e)

a_corr_f = cor(dcollectedg7$Global_active_power,dcollectedg7$Sub_metering_2)
print(a_corr_f)

a_corr_g = cor(dcollectedg7$Global_active_power,dcollectedg7$Sub_metering_3)
print(a_corr_g)

#combinations starting w/ b
b_corr_b = cor(dcollectedg7$Global_reactive_power,dcollectedg7$Global_reactive_power)
b_corr_c = cor(dcollectedg7$Global_reactive_power,dcollectedg7$Voltage)

b_corr_d = cor(dcollectedg7$Global_reactive_power,dcollectedg7$Global_intensity)

b_corr_e = cor(dcollectedg7$Global_reactive_power,dcollectedg7$Sub_metering_1)

b_corr_f = cor(dcollectedg7$Global_reactive_power,dcollectedg7$Sub_metering_2)

b_corr_g = cor(dcollectedg7$Global_reactive_power,dcollectedg7$Sub_metering_3)

#starting w/ c
c_corr_c = cor(dcollectedg7$Voltage,dcollectedg7$Voltage)
c_corr_d = cor(dcollectedg7$Voltage,dcollectedg7$Global_intensity)
c_corr_e = cor(dcollectedg7$Voltage,dcollectedg7$Sub_metering_1)
c_corr_f = cor(dcollectedg7$Voltage,dcollectedg7$Sub_metering_2)
c_corr_g = cor(dcollectedg7$Voltage,dcollectedg7$Sub_metering_3)

#starting w/ d
d_corr_d = cor(dcollectedg7$Global_intensity,dcollectedg7$Global_intensity)
d_corr_e = cor(dcollectedg7$Global_intensity,dcollectedg7$Sub_metering_1)
d_corr_f = cor(dcollectedg7$Global_intensity,dcollectedg7$Sub_metering_2)
d_corr_g = cor(dcollectedg7$Global_intensity,dcollectedg7$Sub_metering_3)

#starts w/ e
e_corr_e =cor(dcollectedg7$Sub_metering_1,dcollectedg7$Sub_metering_1)
e_corr_f =cor(dcollectedg7$Sub_metering_1,dcollectedg7$Sub_metering_2)
e_corr_g =cor(dcollectedg7$Sub_metering_1,dcollectedg7$Sub_metering_3)

#starts w/f
f_corr_f =cor(dcollectedg7$Sub_metering_2,dcollectedg7$Sub_metering_2)
f_corr_g =cor(dcollectedg7$Sub_metering_2,dcollectedg7$Sub_metering_3)

g_corr_g =cor(dcollectedg7$Sub_metering_3,dcollectedg7$Sub_metering_3)




# PART 3

date = dcollectedg7$Date
time = dcollectedg7$Time
intensity = dcollectedg7$Global_intensity

start = "07:30:00"
end =   "12:30:00"
vd1 = c()
vd2 = c()
vd3 = c()
vd4 = c()
vd5 = c()

vd6 = c()
vd7 = c()

k1=1
k2=1
k3=1
k4=1
k5=1

k6=1
k7=1

for(i in 1:length(date)){
  if(date[i] == "12/2/2007"){
    if((strptime(time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      vd1[k1] <- intensity[i]
      k1=k1+1
    }
  }
  if(date[i] == "13/2/2007"){
    if((strptime(time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      vd2[k2] <- intensity[i]
      k2=k2+1
    }
  }
  if(date[i] == "14/2/2007"){
    if((strptime(time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      vd3[k3] <- intensity[i]
      k3=k3+1
    }
  }
  if(date[i] == "15/2/2007"){
    if((strptime(time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      vd4[k4] <- intensity[i]
      k4=k4+1
    }
  }
  if(date[i] == "16/2/2007"){
    if((strptime(time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      vd5[k5] <- intensity[i]
      k5=k5+1
    }
  }
  if(date[i] == "17/2/2007"){
    if((strptime(time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      vd6[k6] <- intensity[i]
      k6=k6+1
    }
  }
  if(date[i] == "18/2/2007"){
    if((strptime(time[i],"%H:%M:%S") >= strptime(start,"%H:%M:%S")) && (strptime(time[i],"%H:%M:%S") <= strptime(end,"%H:%M:%S"))){
      vd7[k7] <- intensity[i]
      k7=k7+1
    }
  }
  
  
  
  
}
use1 = c()
use2 = c()
for(i in 1:length(vd1)){
  x<- c(vd1[i],vd2[i],vd3[i],vd4[i],vd5[i])
  y<- c(vd6[i],vd7[i])
  use1[i] = mean(x)
  use2[i] = mean(y)
  
}


k <- 1 : length(use1)
fit_linear <-lm(use1~k)  #weekdays
fit_linear2 <- lm(use2~k)  #weekends

fit_polynomial <- lm(use1 ~ poly(k,2,raw=TRUE)) #weekdays
fit_polynomial2 <- lm(use2 ~ poly(k,2,raw=TRUE))  #weekends

plot(use1~k, xlab="time",ylab="Global Intestiy", main = "WeekDays")
abline(fit_linear, xlab="time",ylab="Global Intestiy", main = "WeekDays")


plot(use2~k, xlab="time",ylab="Global Intestiy", main = "WeekEnds")
abline(fit_linear2, xlab="time",ylab="Global Intestiy", main = "WeekEnds")


plot(use1~k, xlab="time",ylab="Global Intestiy", main = "WeekDays")
abline(fit_polynomial, xlab="time",ylab="Global Intestiy", main = "WeekDays")


plot(use2~k, xlab="time",ylab="Global Intestiy", main = "WeekEnds")
abline(fit_polynomial2, xlab="time",ylab="Global Intestiy", main = "WeekEnds")



#plot(fit_linear, type = "p", main = "linear regression")



#fit_linear <- lm(y ~ x, use1)






# TESTING STUFF. SKIP OVER

# cor_list = list(a_corr_a,a_corr_b, a_corr_c,a_corr_d,a_corr_e,a_corr_f,a_corr_g,
#                 b_corr_b,b_corr_c, b_corr_d, b_corr_e,b_corr_f,b_corr_g,
#                 c_corr_c, c_corr_d,c_corr_e,c_corr_f,c_corr_g,
#                 d_corr_d,d_corr_e,d_corr_f,d_corr_g,
#                 e_corr_e,e_corr_f,e_corr_g,
#                 f_corr_f,f_corr_g,
#                 g_corr_g)



# # frow = c(a_corr_a,a_corr_b, a_corr_c,a_corr_d,a_corr_e,a_corr_f,a_corr_g)
# # srow = c(b_corr_b,b_corr_c, b_corr_d, b_corr_e,b_corr_f,b_corr_g)
# # trow = c(c_corr_c, c_corr_d,c_corr_e,c_corr_f,c_corr_g)
# # forow = c(d_corr_d,d_corr_e,d_corr_f,d_corr_g)
# # firow = c(e_corr_e,e_corr_f,e_corr_g)
# # sirow = c(f_corr_f,f_corr_g)
# # serow = c(g_corr_g)
# 
# test = as.matrix(cor_list)
# corrplot(test)
# # data(cor_list)
# # textm2 = matrix(cor_list, 7,7)
# # 
# # testvar = round(cor(textm2),1)
# # 
# # 
# # ggcorrplot(testvar,method = "square", "full", lab = TRUE)
# # 
# 

