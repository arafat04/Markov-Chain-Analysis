library(dplyr)
library(data.table)
df_weather = read.csv("C:\\Users\\USER\\Downloads\\STA301\\daily_weather_2020.csv")

df_weather$Country.Region
unique(df_weather$Country.Region)

df_Bangladesh = filter(df_weather,Country.Region=="Bangladesh")
df_Bangladesh = select(df_Bangladesh, Country.Region, time, summary , icon)

unique(
  df_Bangladesh$icon
)

#finding the probability: 1 step matrix
table(df_Bangladesh$icon)
clear_day = count(filter(df_Bangladesh,icon=="clear-day"))
clear_day

partly_Cloudy_day = count(filter(df_Bangladesh,icon=="partly-cloudy-day"))
partly_Cloudy_day

rainy_day = count(filter(df_Bangladesh,icon=="rain"))
rainy_day

count=0

weather_type = df_Bangladesh$icon
weather_type
#defining the transitions
rain_to_rain = 0
rain_to_clear = 0
rain_to_partly_cloudy = 0

clear_to_clear = 0
clear_to_rain = 0
clear_to_partly_cloudy = 0

partly_cloudy_to_partly_cloudy = 0
partly_cloudy_to_rain = 0
partly_cloudy_clear = 0
len = length(weather_type)-1
for (i in 1:len){
  if (weather_type[i] == "rain"){
    if(weather_type[i +1] == "rain"){
      rain_to_rain = rain_to_rain + 1
    }
    else if(weather_type[i +1] == "clear-day"){
      rain_to_clear= rain_to_clear + 1
    }
    else{
      rain_to_partly_cloudy = rain_to_partly_cloudy + 1
    }
  }
  
  else if (weather_type[i] == "clear-day"){
    if(weather_type[i +1] == "clear-day"){
      clear_to_clear = clear_to_clear + 1
    }
    else if(weather_type[i +1] == "rain"){
      clear_to_rain= clear_to_rain + 1
    }
    else{
      clear_to_partly_cloudy = clear_to_partly_cloudy + 1
    }
  }
  
  else{
    if(weather_type[i +1] == "partly-cloudy-day"){
      partly_cloudy_to_partly_cloudy = partly_cloudy_to_partly_cloudy + 1
    }
    else if(weather_type[i +1] == "rain"){
      partly_cloudy_to_rain= partly_cloudy_to_rain + 1
    }
    else{
      partly_cloudy_clear = partly_cloudy_clear + 1
    }
  }

 count= count+1
}
 
rain_to_rain
rain_to_clear
rain_to_partly_cloudy
rainy_trasition = rain_to_rain + rain_to_clear + rain_to_partly_cloudy


clear_to_clear
clear_to_rain
clear_to_partly_cloudy
clear_transition = clear_to_clear + clear_to_partly_cloudy + clear_to_rain
  
  
partly_cloudy_to_partly_cloudy
partly_cloudy_to_rain
partly_cloudy_clear
partly_transition = partly_cloudy_to_partly_cloudy + partly_cloudy_clear + partly_cloudy_to_rain

#now defining the probability for transition matrix
# transition from rain to other states
p_rain_to_rain = rain_to_rain/rainy_trasition
p_rain_to_clear = rain_to_clear/rainy_trasition
p_rain_to_partly_cloudy = rain_to_partly_cloudy/rainy_trasition
p_rain_to_clear + p_rain_to_rain + p_rain_to_partly_cloudy

#transition probability from clear day to other states
p_clear_to_clear = clear_to_clear/clear_transition
p_clear_to_rain = clear_to_rain/clear_transition
p_clear_to_partly_cloudy = clear_to_partly_cloudy/clear_transition

p_clear_to_clear + p_clear_to_rain + p_clear_to_partly_cloudy

#transition probability from partly-cloudy-day to other states
p_partly_cloudy_to_partly_cloudy = partly_cloudy_to_partly_cloudy/partly_transition
p_partly_cloudy_to_rain = partly_cloudy_to_rain/partly_transition
p_partly_cloudy_clear = partly_cloudy_clear/partly_transition
p_partly_cloudy_to_partly_cloudy + p_partly_cloudy_clear + p_partly_cloudy_to_rain


# now applying markov chain

library(markovchain)
library(diagram)

#for weather forecasting problem defining the transition matrix using the probabilities I found aboue manually
transition_matrix_weather <- round(matrix(c(p_clear_to_clear,p_clear_to_rain,p_clear_to_partly_cloudy,
                p_rain_to_clear,p_rain_to_rain, p_rain_to_partly_cloudy,
                p_partly_cloudy_clear, p_partly_cloudy_to_rain,
                p_partly_cloudy_to_partly_cloudy),nrow = 3, byrow = TRUE),3)

discrete_transition_matrix_weather <- new("markovchain",transitionMatrix=transition_matrix_weather, 
             states=c("Clear Day","Rainy","Partly Cloudy Day"),
             name="MarkovChain for Weather Prediction") 

discrete_transition_matrix_weather
plot(discrete_transition_matrix_weather)

dim(discrete_transition_matrix_weather)

# plot using diagram package

stateNames <- c("Clear Day","Rainy","Partly Cloudy Day")
row.names(transition_matrix_weather) <- stateNames; colnames(transition_matrix_weather) <- stateNames
plotmat(t(transition_matrix_weather),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Markov Chain")


# Forecasting of rain for next 7 days

#Defining Current State
#Let's take clear day as our current state , that is the probability that the
#weather goes from Clear day to Rainy day in the next 7 days

# I defined my state row matrix as this order : "Clear Day","Rainy","Partly Cloudy Day"
# so the vector for the vector for choosing a clear Day is: (1,0,0)
library(expm)
library(pracma)

initial_state <- matrix(c(1,0,0),nrow=1, byrow=TRUE)

# Forecasting Rain in the next day
days <- 1
after_one_day <- round(initial_state * discrete_transition_matrix_weather ^ days, 3)
after_one_day

after_one_day <- round(transition_matrix_weather%^%days,3)
after_one_day
#plot
plotmat(t(after_one_day),pos = c(2,1), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.12, 
        box.type = "circle", 
        box.prop = 0.7,
        box.col = "light blue",
        arr.length=.4,
        arr.width=.2,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .17,
        main = "Forecasting Rain on the Next Day")

# Forecasting Rain after 2 days 
days <- 2
after_two_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)
after_two_days

after_two_days <- round(transition_matrix_weather%^%days,3)
after_two_days

plotmat(t(after_two_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 2 Days")
# Forecasting Rain after 3 days 
days<-3
after_three_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)
after_three_days

after_three_days <- round(transition_matrix_weather%^%days,3)
after_three_days

plotmat(t(after_three_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 2 Days")

# Forecasting Rain after 4 days 
days <- 4
after_four_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)

after_four_days

after_four_days <- round(transition_matrix_weather%^%days,3)
after_four_days

plotmat(t(after_four_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 2 Days")

# Forecasting Rain after 5 days 
days <- 5
after_five_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)

after_five_days

after_five_days <- round(transition_matrix_weather%^%days,3)
after_five_days

plotmat(t(after_five_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 2 Days")

# Forecasting Rain after 6 days 
days <- 6
after_six_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)

after_six_days

after_six_days <- round(transition_matrix_weather%^%days,3)
after_six_days

plotmat(t(after_six_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 6 Days")
# Forecasting Rain after 7 days 
days <- 7
after_seven_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)

after_seven_days

after_seven_days <- round(transition_matrix_weather%^%days,3)
after_seven_days

plotmat(t(after_seven_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 7 Days")

