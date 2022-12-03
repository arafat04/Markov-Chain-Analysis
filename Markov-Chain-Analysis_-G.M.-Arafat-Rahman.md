Final Project - Markov Chain Analysis with R
================
G.M. Arafat Rahman
2022-08-18

# Markov Chain Analysis with Weather Dataset

> **Historical Daily Weather Data 2020**
>
> This dataset contains historical daily weather data for 163
> countries(with provincial data for some) from Jan 1, 2020 up to April
> 21, 2020. The countries and provinces were chosen based on the Johns
> Hopkins COVID-19 dataset. It contains various features such as
> temperature, pressure, humidity, ozone levels, visibility,
> precipitation, etc. This dataset was collected from Kaggle.

## Calling necessary libraries

> I have used the markovchain package for demonstrating markov chain.

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 4.2.1

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library(markovchain)
```

    ## Warning: package 'markovchain' was built under R version 4.2.1

    ## Package:  markovchain
    ## Version:  0.9.0
    ## Date:     2022-07-01
    ## BugReport: https://github.com/spedygiorgio/markovchain/issues

``` r
library(diagram)
```

    ## Loading required package: shape

``` r
library(expm)
```

    ## Warning: package 'expm' was built under R version 4.2.1

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'expm'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     expm

``` r
library(pracma)
```

    ## Warning: package 'pracma' was built under R version 4.2.1

    ## 
    ## Attaching package: 'pracma'

    ## The following objects are masked from 'package:expm':
    ## 
    ##     expm, logm, sqrtm

    ## The following objects are masked from 'package:Matrix':
    ## 
    ##     expm, lu, tril, triu

> Now loading the data-set.

``` r
df_weather = read.csv("C:\\Users\\USER\\Downloads\\STA301\\daily_weather_2020.csv")
```

> We can see the countries those are included in the data-set.

``` r
unique(df_weather$Country.Region)
```

    ##   [1] "Afghanistan"                      "Albania"                         
    ##   [3] "Algeria"                          "Andorra"                         
    ##   [5] "Antigua and Barbuda"              "Argentina"                       
    ##   [7] "Armenia"                          "Aruba"                           
    ##   [9] "Australia"                        "Austria"                         
    ##  [11] "Azerbaijan"                       "Bahrain"                         
    ##  [13] "Bangladesh"                       "Barbados"                        
    ##  [15] "Belarus"                          "Belgium"                         
    ##  [17] "Benin"                            "Bhutan"                          
    ##  [19] "Bolivia"                          "Bosnia and Herzegovina"          
    ##  [21] "Brazil"                           "Brunei"                          
    ##  [23] "Bulgaria"                         "Burkina Faso"                    
    ##  [25] "Cambodia"                         "Canada"                          
    ##  [27] "Central African Republic"         "Chile"                           
    ##  [29] "China"                            "Colombia"                        
    ##  [31] "Congo (Brazzaville)"              "Congo (Kinshasa)"                
    ##  [33] "Costa Rica"                       "Cote d'Ivoire"                   
    ##  [35] "Croatia"                          "Cruise Ship"                     
    ##  [37] "Cuba"                             "Cyprus"                          
    ##  [39] "Denmark"                          "Djibouti"                        
    ##  [41] "Dominican Republic"               "Ecuador"                         
    ##  [43] "Egypt"                            "Equatorial Guinea"               
    ##  [45] "Estonia"                          "Eswatini"                        
    ##  [47] "Ethiopia"                         "Finland"                         
    ##  [49] "France"                           "French Guiana"                   
    ##  [51] "Gabon"                            "Gambia, The"                     
    ##  [53] "Georgia"                          "Germany"                         
    ##  [55] "Ghana"                            "Greece"                          
    ##  [57] "Greenland"                        "Guadeloupe"                      
    ##  [59] "Guam"                             "Guatemala"                       
    ##  [61] "Guernsey"                         "Guinea"                          
    ##  [63] "Guyana"                           "Holy See"                        
    ##  [65] "Honduras"                         "Hungary"                         
    ##  [67] "Iceland"                          "India"                           
    ##  [69] "Indonesia"                        "Iran"                            
    ##  [71] "Iraq"                             "Ireland"                         
    ##  [73] "Israel"                           "Italy"                           
    ##  [75] "Jamaica"                          "Japan"                           
    ##  [77] "Jersey"                           "Jordan"                          
    ##  [79] "Kazakhstan"                       "Kenya"                           
    ##  [81] "Korea, South"                     "Kuwait"                          
    ##  [83] "Kyrgyzstan"                       "Latvia"                          
    ##  [85] "Lebanon"                          "Liberia"                         
    ##  [87] "Liechtenstein"                    "Lithuania"                       
    ##  [89] "Luxembourg"                       "Malaysia"                        
    ##  [91] "Maldives"                         "Malta"                           
    ##  [93] "Martinique"                       "Mauritania"                      
    ##  [95] "Mauritius"                        "Mayotte"                         
    ##  [97] "Mexico"                           "Moldova"                         
    ##  [99] "Monaco"                           "Mongolia"                        
    ## [101] "Montenegro"                       "Morocco"                         
    ## [103] "Namibia"                          "Nepal"                           
    ## [105] "Netherlands"                      "New Zealand"                     
    ## [107] "Nigeria"                          "North Macedonia"                 
    ## [109] "Norway"                           "Oman"                            
    ## [111] "Pakistan"                         "Panama"                          
    ## [113] "Paraguay"                         "Peru"                            
    ## [115] "Philippines"                      "Poland"                          
    ## [117] "Portugal"                         "Puerto Rico"                     
    ## [119] "Qatar"                            "Republic of the Congo"           
    ## [121] "Reunion"                          "Romania"                         
    ## [123] "Russia"                           "Rwanda"                          
    ## [125] "Saint Lucia"                      "Saint Vincent and the Grenadines"
    ## [127] "San Marino"                       "Saudi Arabia"                    
    ## [129] "Senegal"                          "Serbia"                          
    ## [131] "Seychelles"                       "Singapore"                       
    ## [133] "Slovakia"                         "Slovenia"                        
    ## [135] "Somalia"                          "South Africa"                    
    ## [137] "Spain"                            "Sri Lanka"                       
    ## [139] "Sudan"                            "Suriname"                        
    ## [141] "Sweden"                           "Switzerland"                     
    ## [143] "Taiwan*"                          "Tanzania"                        
    ## [145] "Thailand"                         "The Bahamas"                     
    ## [147] "The Gambia"                       "Togo"                            
    ## [149] "Trinidad and Tobago"              "Tunisia"                         
    ## [151] "Turkey"                           "US"                              
    ## [153] "Ukraine"                          "United Arab Emirates"            
    ## [155] "United Kingdom"                   "Uruguay"                         
    ## [157] "Uzbekistan"                       "Venezuela"                       
    ## [159] "Vietnam"                          "Zambia"

> I took Bangladesh’s weather data to apply Markov chain.

``` r
df_Bangladesh = filter(df_weather,Country.Region=="Bangladesh")
df_Bangladesh = select(df_Bangladesh, Country.Region, time, summary , icon)
```

> We can see the unique weather condition in Bangladesh so that we can
> build the states for markov chain.

``` r
unique(
  df_Bangladesh$icon
)
```

    ## [1] "partly-cloudy-day" "rain"              "clear-day"

# 1. Defining the problem:

> In the weather data-set of Bangladesh there are 112 observations. The
> observations are daily based. We have found that there are 3 states
> during this time period, they are: a day can be a “clear-day” or
> “partly-cloudy-day” and there could be “rain” on that day. So we can
> predict what will be weather forecast for next 7 days after a state
> being chosen.

### Finding the probability of the states from the data-set that we can build a transition matrix:

> Printing the total occurrences of each state.

``` r
table(df_Bangladesh$icon)
```

    ## 
    ##         clear-day partly-cloudy-day              rain 
    ##                70                23                19

``` r
clear_day = count(filter(df_Bangladesh,icon=="clear-day"))
clear_day
```

    ##    n
    ## 1 70

``` r
partly_Cloudy_day = count(filter(df_Bangladesh,icon=="partly-cloudy-day"))
partly_Cloudy_day
```

    ##    n
    ## 1 23

``` r
rainy_day = count(filter(df_Bangladesh,icon=="rain"))
rainy_day
```

    ##    n
    ## 1 19

## Finding the probability:

> We will choose the weather column in order to find the probability of
> each states.

``` r
weather_type = df_Bangladesh$icon
weather_type
```

    ##   [1] "partly-cloudy-day" "partly-cloudy-day" "rain"             
    ##   [4] "rain"              "rain"              "clear-day"        
    ##   [7] "clear-day"         "partly-cloudy-day" "rain"             
    ##  [10] "clear-day"         "clear-day"         "clear-day"        
    ##  [13] "clear-day"         "clear-day"         "clear-day"        
    ##  [16] "clear-day"         "clear-day"         "clear-day"        
    ##  [19] "clear-day"         "partly-cloudy-day" "partly-cloudy-day"
    ##  [22] "clear-day"         "partly-cloudy-day" "partly-cloudy-day"
    ##  [25] "clear-day"         "clear-day"         "clear-day"        
    ##  [28] "clear-day"         "clear-day"         "rain"             
    ##  [31] "clear-day"         "clear-day"         "clear-day"        
    ##  [34] "clear-day"         "clear-day"         "partly-cloudy-day"
    ##  [37] "partly-cloudy-day" "partly-cloudy-day" "partly-cloudy-day"
    ##  [40] "partly-cloudy-day" "clear-day"         "clear-day"        
    ##  [43] "clear-day"         "clear-day"         "clear-day"        
    ##  [46] "clear-day"         "clear-day"         "clear-day"        
    ##  [49] "clear-day"         "clear-day"         "partly-cloudy-day"
    ##  [52] "partly-cloudy-day" "partly-cloudy-day" "clear-day"        
    ##  [55] "partly-cloudy-day" "rain"              "rain"             
    ##  [58] "clear-day"         "clear-day"         "clear-day"        
    ##  [61] "clear-day"         "clear-day"         "rain"             
    ##  [64] "clear-day"         "rain"              "rain"             
    ##  [67] "rain"              "clear-day"         "clear-day"        
    ##  [70] "clear-day"         "clear-day"         "clear-day"        
    ##  [73] "clear-day"         "partly-cloudy-day" "partly-cloudy-day"
    ##  [76] "partly-cloudy-day" "clear-day"         "clear-day"        
    ##  [79] "clear-day"         "partly-cloudy-day" "partly-cloudy-day"
    ##  [82] "rain"              "clear-day"         "clear-day"        
    ##  [85] "clear-day"         "clear-day"         "clear-day"        
    ##  [88] "clear-day"         "clear-day"         "clear-day"        
    ##  [91] "clear-day"         "clear-day"         "clear-day"        
    ##  [94] "clear-day"         "clear-day"         "partly-cloudy-day"
    ##  [97] "clear-day"         "clear-day"         "partly-cloudy-day"
    ## [100] "clear-day"         "clear-day"         "clear-day"        
    ## [103] "clear-day"         "clear-day"         "clear-day"        
    ## [106] "rain"              "rain"              "rain"             
    ## [109] "rain"              "rain"              "rain"             
    ## [112] "rain"

## Defining the transitions:

> As there are 3 states, there will be states \* states =3\*3 = 9
> transition states and from each state there will be 3 states outgoing.

### If the weather is in state “Rain”

``` r
rain_to_rain = 0
rain_to_clear = 0
rain_to_partly_cloudy = 0
```

### If the weather is in state “clear-day”

``` r
clear_to_clear = 0
clear_to_rain = 0
clear_to_partly_cloudy = 0
```

### If the weather is in state “partly-cloudy-day”

``` r
partly_cloudy_to_partly_cloudy = 0
partly_cloudy_to_rain = 0
partly_cloudy_clear = 0
```

### Counting the outgoing occurrences from each state:

> This loop will find the count from each state to other states in order
> to calculate the probability. We will run the loop till length-1 time
> in order to avoid array index out of bound error and to make the count
> accurate.

``` r
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

}
```

### Printing the state transition counts from each state:

### If the weather is in state “Rain”:

``` r
rain_to_rain
```

    ## [1] 11

``` r
rain_to_clear
```

    ## [1] 7

``` r
rain_to_partly_cloudy
```

    ## [1] 0

### If the weather is in state “clear-day”:

``` r
clear_to_clear
```

    ## [1] 56

``` r
clear_to_rain
```

    ## [1] 4

``` r
clear_to_partly_cloudy
```

    ## [1] 10

### If the weather is in state “partly-cloudy-day”:

``` r
partly_cloudy_to_partly_cloudy
```

    ## [1] 12

``` r
partly_cloudy_to_rain
```

    ## [1] 4

``` r
partly_cloudy_clear
```

    ## [1] 7

### Correcting the total count:

> Earlier we have calculated how many observations are there for each
> state. But from one state there can be no outgoing state to more than
> one states. So we have to calculate to transition from each state.

``` r
rainy_transition = rain_to_rain + rain_to_clear + rain_to_partly_cloudy
clear_transition = clear_to_clear + clear_to_partly_cloudy + clear_to_rain
partly_transition = partly_cloudy_to_partly_cloudy + partly_cloudy_clear + partly_cloudy_to_rain
```

## Defining the probability for transition matrix:

### Transition probability from rain to other states:

``` r
p_rain_to_rain = rain_to_rain/rainy_transition
p_rain_to_clear = rain_to_clear/rainy_transition
p_rain_to_partly_cloudy = rain_to_partly_cloudy/rainy_transition
```

> We can see the transition probabilities:

``` r
p_rain_to_rain
```

    ## [1] 0.6111111

``` r
p_rain_to_clear
```

    ## [1] 0.3888889

``` r
p_rain_to_partly_cloudy
```

    ## [1] 0

> We can also see the total probability of each row is 1 or not

``` r
p_rain_to_clear + p_rain_to_rain + p_rain_to_partly_cloudy
```

    ## [1] 1

### Transition probability from clear day to other states:

``` r
p_clear_to_clear = clear_to_clear/clear_transition
p_clear_to_rain = clear_to_rain/clear_transition
p_clear_to_partly_cloudy = clear_to_partly_cloudy/clear_transition
```

> We can see the transition probabilities:

``` r
p_clear_to_clear
```

    ## [1] 0.8

``` r
p_clear_to_rain
```

    ## [1] 0.05714286

``` r
p_clear_to_partly_cloudy
```

    ## [1] 0.1428571

> We can also see the total probability of each row is 1 or not

``` r
p_clear_to_clear + p_clear_to_rain + p_clear_to_partly_cloudy
```

    ## [1] 1

### Transition probability from partly-cloudy-day to other states:

``` r
p_partly_cloudy_to_partly_cloudy = partly_cloudy_to_partly_cloudy/partly_transition
p_partly_cloudy_to_rain = partly_cloudy_to_rain/partly_transition
p_partly_cloudy_clear = partly_cloudy_clear/partly_transition
```

> We can see the transition probabilities:

``` r
p_partly_cloudy_to_partly_cloudy
```

    ## [1] 0.5217391

``` r
p_partly_cloudy_to_rain
```

    ## [1] 0.173913

``` r
p_partly_cloudy_clear
```

    ## [1] 0.3043478

> We can also see the total probability of each row is 1 or not

``` r
p_partly_cloudy_to_partly_cloudy + p_partly_cloudy_clear + p_partly_cloudy_to_rain
```

    ## [1] 1

# 2. Selecting the Markov Chain Model:

> I have imported the “markovchain” library to analyse the weather data.
>
> Now applying markov chain

### Building the transition matrix for markov chain model

> Transition matrix is essential to make a markov chain model. I
> calculated 1-step transition probabilities from each state manually.
> Now I will create a matrix using those probabilities. In this matrix
> byrow = True is enabled that will make the matrix created by row
> values.

``` r
transition_matrix_weather <- round(matrix(c(p_clear_to_clear,p_clear_to_rain,p_clear_to_partly_cloudy,
                p_rain_to_clear,p_rain_to_rain, p_rain_to_partly_cloudy,
                p_partly_cloudy_clear, p_partly_cloudy_to_rain,
                p_partly_cloudy_to_partly_cloudy),nrow = 3, byrow = TRUE),3)
```

> We can see the transition matrix now.

``` r
stateNames <- c("Clear Day","Rainy","Partly Cloudy Day")
row.names(transition_matrix_weather) <- stateNames; colnames(transition_matrix_weather) <- stateNames

transition_matrix_weather
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.800 0.057             0.143
    ## Rainy                 0.389 0.611             0.000
    ## Partly Cloudy Day     0.304 0.174             0.522

### Making the Discrete Markov chain model:

> Using a markovchain object, here I have built a discrete markov chain
> model.

``` r
discrete_transition_matrix_weather <- new("markovchain",transitionMatrix=transition_matrix_weather, 
             states=c("Clear Day","Rainy","Partly Cloudy Day"),
             name="MarkovChain for Weather Prediction") 
```

> We can see the markov chain model now.

``` r
discrete_transition_matrix_weather
```

    ## MarkovChain for Weather Prediction 
    ##  A  3 - dimensional discrete Markov Chain defined by the following states: 
    ##  Clear Day, Rainy, Partly Cloudy Day 
    ##  The transition matrix  (by rows)  is defined as follows: 
    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.800 0.057             0.143
    ## Rainy                 0.389 0.611             0.000
    ## Partly Cloudy Day     0.304 0.174             0.522

> We can also see the dimension of the matrix.

``` r
dim(discrete_transition_matrix_weather)
```

    ## [1] 3

### Plotting the transition matrix

``` r
plotmat(t(transition_matrix_weather),pos = c(2,1), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.12, 
        box.type = "circle", 
        box.prop = 0.7,
        box.col = "green",
        arr.length=.4,
        arr.width=.2,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .17,
        main = "Forecasting Rain on the Next Day")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

# Forecasting of rain for next 7 days

### Defining Current State

> Let’s take clear day as our current state , that is the probability
> that the weather goes from Clear day to Rainy day in the next 7 days I
> defined my state row matrix as this order
> **`: "Clear Day","Rainy","Partly Cloudy Day"`**, so the vector for the
> vector for choosing a clear Day is: **`(1,0,0)`**

``` r
initial_state <- matrix(c(1,0,0),nrow=1, byrow=TRUE)
```

### Forecasting Rain in the next day:

``` r
days <- 1
after_one_day <- round(initial_state * discrete_transition_matrix_weather ^ days, 3)
after_one_day
```

    ##      Clear Day Rainy Partly Cloudy Day
    ## [1,]       0.8 0.057             0.143

``` r
after_one_day <- round(transition_matrix_weather%^%days,3)
after_one_day
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.800 0.057             0.143
    ## Rainy                 0.389 0.611             0.000
    ## Partly Cloudy Day     0.304 0.174             0.522

``` r
#plot
plotmat(t(after_one_day),pos = c(2,1), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.12, 
        box.type = "circle", 
        box.prop = 0.7,
        box.col = "green",
        arr.length=.4,
        arr.width=.2,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .17,
        main = "Forecasting Rain on the Next Day")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

> Here we can see that there is only 5.7% chance of raining in the next
> day of a clear day.

### Forecasting Rain after 2 days:

``` r
days <- 2
after_two_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)
after_two_days
```

    ##      Clear Day Rainy Partly Cloudy Day
    ## [1,]     0.706 0.105             0.189

``` r
after_two_days <- round(transition_matrix_weather%^%days,3)
after_two_days
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.706 0.105             0.189
    ## Rainy                 0.549 0.395             0.056
    ## Partly Cloudy Day     0.470 0.214             0.316

``` r
plotmat(t(after_two_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 2 Days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

> Here we can see that there is only 10.5% chance of raining after 2
> days of a clear day.

### Forecasting Rain after 3 days

``` r
days<-3
after_three_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)
after_three_days
```

    ##      Clear Day Rainy Partly Cloudy Day
    ## [1,]     0.663 0.137               0.2

``` r
after_three_days <- round(transition_matrix_weather%^%days,3)
after_three_days
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.663 0.137             0.200
    ## Rainy                 0.610 0.283             0.108
    ## Partly Cloudy Day     0.555 0.213             0.232

``` r
plotmat(t(after_three_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 3 Days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

> Here we can see that there is only 13.7% chance of raining after 3
> days of a clear day.

### Forecasting Rain after 4 days

``` r
days <- 4
after_four_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)
after_four_days
```

    ##      Clear Day Rainy Partly Cloudy Day
    ## [1,]     0.645 0.157             0.199

``` r
after_four_days <- round(transition_matrix_weather%^%days,3)
after_four_days
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.645 0.157             0.199
    ## Rainy                 0.631 0.226             0.143
    ## Partly Cloudy Day     0.597 0.202             0.201

``` r
plotmat(t(after_four_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 4 Days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

> Here we can see that there is only 15.7% chance of raining after 4
> days of a clear day.

### Forecasting Rain after 5 days

``` r
days <- 5
after_five_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)

after_five_days
```

    ##      Clear Day Rainy Partly Cloudy Day
    ## [1,]     0.637 0.167             0.196

``` r
after_five_days <- round(transition_matrix_weather%^%days,3)
after_five_days
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.637 0.167             0.196
    ## Rainy                 0.636 0.199             0.165
    ## Partly Cloudy Day     0.618 0.192             0.190

``` r
plotmat(t(after_five_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 5 Days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

> Here we can see that there is only 16.7% chance of raining after 5
> days from a clear day.

### Forecasting Rain after 6 days

``` r
days <- 6
after_six_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)

after_six_days
```

    ##      Clear Day Rainy Partly Cloudy Day
    ## [1,]     0.634 0.172             0.193

``` r
after_six_days <- round(transition_matrix_weather%^%days,3)
after_six_days
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.634 0.172             0.193
    ## Rainy                 0.636 0.187             0.177
    ## Partly Cloudy Day     0.627 0.186             0.188

``` r
plotmat(t(after_six_days),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 6 Days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

> Here we can see that there is only 17.2% chance of raining after 6
> days from a clear day.

### Forecasting Rain after 7 days

``` r
days <- 7 
after_seven_days <- round(initial_state * discrete_transition_matrix_weather ^ days,3)

after_seven_days
```

    ##      Clear Day Rainy Partly Cloudy Day
    ## [1,]     0.633 0.175             0.192

``` r
after_seven_days <- round(transition_matrix_weather%^%days,3) 
after_seven_days
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day             0.633 0.175             0.192
    ## Rainy                 0.635 0.181             0.183
    ## Partly Cloudy Day     0.631 0.182             0.188

``` r
plotmat(t(after_seven_days),pos = c(1,2),
        lwd = 1, box.lwd = 2, cex.txt = 0.8,
        box.size = 0.1, box.type = "circle",
        box.prop = 0.5,
        box.col ="green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Forecasting Rain After 7 Days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

> Here we can see that there is only 17.5% chance of raining after 7
> days from a clear day.

## Simulation of Multi Nomial Markov Chain on Weather Data:

We will use transition matrix transition_matrix_weather to simulate.

``` r
round(transition_matrix_weather,2)
```

    ##                   Clear Day Rainy Partly Cloudy Day
    ## Clear Day              0.80  0.06              0.14
    ## Rainy                  0.39  0.61              0.00
    ## Partly Cloudy Day      0.30  0.17              0.52

``` r
run.mc.sim <- function( P, t) { 

  # number of possible states

  num.states <- nrow(P)
  print("num.states")
  print(num.states)
  # stores the states X_t through time

  states <- numeric(t)
  print('the value of state is:')
  print(states)
  # initialize variable for first state 

  states[1]    <- 1


  for(i in 2:t) { #i=2

    # probability vector to simulate next state X_{t+1}

    a<-states[i-1]
    print("the value of a is")
    print( a)
    pt  <- P[a, ]
    print("THe value of pt")
    print(pt)
    ## draw from multinomial and determine state

    states[i] <-  which(rmultinom(1, 1, pt) == 1)
    print("value of states after rmul")
    print(states[i])
  }

  return(states)

}
```

``` r
# setup transition matrix p, here we will use transition_matrix_weather

P <- transition_matrix_weather

#num.iterations 

t=100

MC<-run.mc.sim(P,t)
```

    ## [1] "num.states"
    ## [1] 3
    ## [1] "the value of state is:"
    ##   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [38] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 3
    ## [1] "the value of a is"
    ## [1] 3
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.304             0.174             0.522 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 3
    ## [1] "the value of a is"
    ## [1] 3
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.304             0.174             0.522 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 3
    ## [1] "the value of a is"
    ## [1] 3
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.304             0.174             0.522 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 3
    ## [1] "the value of a is"
    ## [1] 3
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.304             0.174             0.522 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 3
    ## [1] "the value of a is"
    ## [1] 3
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.304             0.174             0.522 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 3
    ## [1] "the value of a is"
    ## [1] 3
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.304             0.174             0.522 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 2
    ## [1] "the value of a is"
    ## [1] 2
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.389             0.611             0.000 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1
    ## [1] "the value of a is"
    ## [1] 1
    ## [1] "THe value of pt"
    ##         Clear Day             Rainy Partly Cloudy Day 
    ##             0.800             0.057             0.143 
    ## [1] "value of states after rmul"
    ## [1] 1

``` r
MC
```

    ##   [1] 1 1 1 1 1 1 1 1 1 1 3 1 2 2 2 1 1 1 1 3 1 1 1 3 2 1 1 1 1 1 1 1 3 1 1 1 1
    ##  [38] 3 2 2 2 2 2 2 2 1 1 1 1 1 1 1 3 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1
    ##  [75] 1 1 2 2 1 1 1 1 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1

``` r
typeof(MC)
```

    ## [1] "double"

``` r
length(MC)
```

    ## [1] 100

``` r
plot(MC,type="b",col='green')
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

# Problem 2: Markov chain on Stock price data

``` python
import pandas as pd
```

``` python
# this is a stock data set of dhaka stcok exchange from 2021
df_2 = pd.read_json("C:\\Users\\USER\\Downloads\\STA301\\prices_2021.json")
#will print unique company names in the dataset
df_2["trading_code"].unique() 
```

    ## array(['1JANATAMF', '1STPRIMFMF', 'AAMRANET', 'AAMRATECH', 'ABB1STMF',
    ##        'ABBANK', 'ACFL', 'ACI', 'ACIFORMULA', 'ACMELAB', 'ACMEPL',
    ##        'ACTIVEFINE', 'ADNTEL', 'ADVENT', 'AFCAGRO', 'AFTABAUTO',
    ##        'AGNISYSL', 'AGRANINS', 'AIBL1STIMF', 'AIBLPBOND', 'AIL',
    ##        'AL-HAJTEX', 'ALARABANK', 'ALIF', 'ALLTEX', 'AMANFEED', 'AMBEEPHA',
    ##        'AMCL(PRAN)', 'ANLIMAYARN', 'ANWARGALV', 'AOL', 'APEXFOODS',
    ##        'APEXFOOT', 'APEXSPINN', 'APEXTANRY', 'APOLOISPAT', 'APSCLBOND',
    ##        'ARAMIT', 'ARAMITCEM', 'ARGONDENIM', 'ASIAINS', 'ASIAPACINS',
    ##        'ATCSLGF', 'ATLASBANG', 'AZIZPIPES', 'BANGAS', 'BANKASIA',
    ##        'BARKAPOWER', 'BATASHOE', 'BATBC', 'BAYLEASING', 'BBS',
    ##        'BBSCABLES', 'BDAUTOCA', 'BDCOM', 'BDFINANCE', 'BDLAMPS',
    ##        'BDSERVICE', 'BDTHAI', 'BDWELDING', 'BEACHHATCH', 'BEACONPHAR',
    ##        'BENGALWTL', 'BERGERPBL', 'BEXIMCO', 'BGIC', 'BIFC', 'BNICL',
    ##        'BPML', 'BPPL', 'BRACBANK', 'BSC', 'BSCCL', 'BSRMLTD', 'BSRMSTEEL',
    ##        'BXPHARMA', 'BXSYNTH', 'CAPMBDBLMF', 'CAPMIBBLMF', 'CENTRALINS',
    ##        'CENTRALPHL', 'CITYBANK', 'CITYGENINS', 'CNATEX', 'CONFIDCEM',
    ##        'CONTININS', 'COPPERTECH', 'CRYSTALINS', 'CVOPRL', 'DACCADYE',
    ##        'DAFODILCOM', 'DBH', 'DBH1STMF', 'DEBARACEM', 'DEBBDLUGG',
    ##        'DEBBDWELD', 'DEBBDZIPP', 'DEBBXDENIM', 'DEBBXFISH', 'DEBBXKNI',
    ##        'DEBBXTEX', 'DELTALIFE', 'DELTASPINN', 'DESCO', 'DESHBANDHU',
    ##        'DGIC', 'DHAKABANK', 'DHAKAINS', 'DOMINAGE', 'DOREENPWR',
    ##        'DSHGARME', 'DSSL', 'DULAMIACOT', 'DUTCHBANGL', 'EASTERNINS',
    ##        'EASTLAND', 'EASTRNLUB', 'EBL', 'EBL1STMF', 'EBLNRBMF', 'ECABLES',
    ##        'EGEN', 'EHL', 'EIL', 'EMERALDOIL', 'ENVOYTEX', 'EPGL',
    ##        'ESQUIRENIT', 'ETL', 'EXIM1STMF', 'EXIMBANK', 'FAMILYTEX',
    ##        'FARCHEM', 'FAREASTFIN', 'FAREASTLIF', 'FASFIN', 'FBFIF',
    ##        'FEDERALINS', 'FEKDIL', 'FINEFOODS', 'FIRSTFIN', 'FIRSTSBANK',
    ##        'FORTUNE', 'FUWANGCER', 'FUWANGFOOD', 'GBBPOWER', 'GEMINISEA',
    ##        'GENEXIL', 'GENNEXT', 'GHAIL', 'GHCL', 'GLOBALINS', 'GOLDENSON',
    ##        'GP', 'GPHISPAT', 'GQBALLPEN', 'GRAMEENS2', 'GREENDELMF',
    ##        'GREENDELT', 'GSPFINANCE', 'HAKKANIPUL', 'HEIDELBCEM', 'HFL',
    ##        'HRTEX', 'HWAWELLTEX', 'IBBL2PBOND', 'IBBLPBOND', 'IBNSINA', 'IBP',
    ##        'ICB', 'ICB3RDNRB', 'ICBAGRANI1', 'ICBAMCL2ND', 'ICBEPMF1S1',
    ##        'ICBIBANK', 'ICBSONALI1', 'IDLC', 'IFADAUTOS', 'IFIC', 'IFIC1STMF',
    ##        'IFILISLMF1', 'ILFSL', 'IMAMBUTTON', 'INDEXAGRO', 'INTECH',
    ##        'INTRACO', 'IPDC', 'ISLAMIBANK', 'ISLAMICFIN', 'ISLAMIINS',
    ##        'ISNLTD', 'ITC', 'JAMUNABANK', 'JAMUNAOIL', 'JANATAINS', 'JMISMDL',
    ##        'JUTESPINN', 'KARNAPHULI', 'KAY&QUE', 'KBPPWBIL', 'KDSALTD',
    ##        'KEYACOSMET', 'KOHINOOR', 'KPCL', 'KPPL', 'KTL', 'LANKABAFIN',
    ##        'LEGACYFOOT', 'LHBL', 'LIBRAINFU', 'LINDEBD', 'LOVELLO', 'LRBDL',
    ##        'LRGLOBMF1', 'MAKSONSPIN', 'MALEKSPIN', 'MARICO', 'MATINSPINN',
    ##        'MBL1STMF', 'MEGCONMILK', 'MEGHNACEM', 'MEGHNALIFE', 'MEGHNAPET',
    ##        'MERCANBANK', 'MERCINS', 'METROSPIN', 'MHSML', 'MICEMENT',
    ##        'MIDASFIN', 'MIRACLEIND', 'MIRAKHTER', 'MITHUNKNIT', 'MJLBD',
    ##        'MLDYEING', 'MONNOAGML', 'MONNOCERA', 'MONNOFABR', 'MONOSPOOL',
    ##        'MPETROLEUM', 'MTB', 'NAHEEACP', 'NATLIFEINS', 'NAVANACNG', 'NBL',
    ##        'NCCBANK', 'NCCBLMF1', 'NEWLINE', 'NFML', 'NHFIL', 'NITOLINS',
    ##        'NLI1STMF', 'NORTHERN', 'NORTHRNINS', 'NPOLYMAR', 'NPOLYMER',
    ##        'NRBCBANK', 'NTC', 'NTLTUBES', 'NURANI', 'OAL', 'OIMEX', 'OLYMPIC',
    ##        'ONEBANKLTD', 'ORIONINFU', 'ORIONPHARM', 'PADMALIFE', 'PADMAOIL',
    ##        'PAPERPROC', 'PARAMOUNT', 'PDL', 'PENINSULA', 'PEOPLESINS',
    ##        'PF1STMF', 'PHARMAID', 'PHENIXINS', 'PHOENIXFIN', 'PHPMF1',
    ##        'PIONEERINS', 'PLFSL', 'POPULAR1MF', 'POPULARLIF', 'POWERGRID',
    ##        'PRAGATIINS', 'PRAGATILIF', 'PREMIERBAN', 'PREMIERCEM',
    ##        'PREMIERLEA', 'PRIME1ICBA', 'PRIMEBANK', 'PRIMEFIN', 'PRIMEINSUR',
    ##        'PRIMELIFE', 'PRIMETEX', 'PROGRESLIF', 'PROVATIINS', 'PTL',
    ##        'PUBALIBANK', 'PURABIGEN', 'QUASEMIND', 'QUEENSOUTH', 'RAHIMAFOOD',
    ##        'RAHIMTEXT', 'RAKCERAMIC', 'RANFOUNDRY', 'RDFOOD', 'RECKITTBEN',
    ##        'REGENTTEX', 'RELIANCE1', 'RELIANCINS', 'RENATA', 'RENWICKJA',
    ##        'REPUBLIC', 'RINGSHINE', 'RNSPIN', 'ROBI', 'RSRMSTEEL',
    ##        'RUNNERAUTO', 'RUPALIBANK', 'RUPALIINS', 'RUPALILIFE',
    ##        'SAFKOSPINN', 'SAIFPOWER', 'SAIHAMCOT', 'SAIHAMTEX', 'SALAMCRST',
    ##        'SALVOCHEM', 'SAMATALETH', 'SAMORITA', 'SANDHANINS', 'SAPORTL',
    ##        'SAVAREFR', 'SBACBANK', 'SEAPEARL', 'SEBL1STMF', 'SEMLFBSLGF',
    ##        'SEMLIBBLSF', 'SEMLLECMF', 'SHAHJABANK', 'SHASHADNIM', 'SHEPHERD',
    ##        'SHURWID', 'SHYAMPSUG', 'SIBL', 'SILCOPHL', 'SILVAPHL', 'SIMTEX',
    ##        'SINGERBD', 'SINOBANGLA', 'SJIBLPBOND', 'SKICL', 'SKTRIMS',
    ##        'SONALIANSH', 'SONALILIFE', 'SONALIPAPR', 'SONARBAINS',
    ##        'SONARGAON', 'SOUTHEASTB', 'SPCERAMICS', 'SPCL', 'SQUARETEXT',
    ##        'SQURPHARMA', 'SSSTEEL', 'STANCERAM', 'STANDARINS', 'STANDBANKL',
    ##        'STYLECRAFT', 'SUMITPOWER', 'SUNLIFEINS', 'T05Y0715', 'T05Y0815',
    ##        'T10Y0117', 'T10Y0118', 'T10Y0119', 'T10Y0121', 'T10Y0126',
    ##        'T10Y0214', 'T10Y0215', 'T10Y0216', 'T10Y0217', 'T10Y0218',
    ##        'T10Y0219', 'T10Y0220', 'T10Y0221', 'T10Y0317', 'T10Y0318',
    ##        'T10Y0319', 'T10Y0320', 'T10Y0321', 'T10Y0414', 'T10Y0415',
    ##        'T10Y0416', 'T10Y0418', 'T10Y0419', 'T10Y0420', 'T10Y0421',
    ##        'T10Y0517', 'T10Y0518', 'T10Y0519', 'T10Y0520', 'T10Y0521',
    ##        'T10Y0614', 'T10Y0615', 'T10Y0616', 'T10Y0617', 'T10Y0618',
    ##        'T10Y0619', 'T10Y0620', 'T10Y0621', 'T10Y0717', 'T10Y0718',
    ##        'T10Y0719', 'T10Y0720', 'T10Y0721', 'T10Y0814', 'T10Y0816',
    ##        'T10Y0817', 'T10Y0818', 'T10Y0819', 'T10Y0820', 'T10Y0821',
    ##        'T10Y0916', 'T10Y0917', 'T10Y0918', 'T10Y0919', 'T10Y0920',
    ##        'T10Y0921', 'T10Y1014', 'T10Y1016', 'T10Y1017', 'T10Y1018',
    ##        'T10Y1019', 'T10Y1116', 'T10Y1117', 'T10Y1118', 'T10Y1120',
    ##        'T10Y1213', 'T10Y1214', 'T10Y1215', 'T10Y1216', 'T10Y1217',
    ##        'T10Y1218', 'T10Y1219', 'T10Y1220', 'T15Y0123', 'T15Y0124',
    ##        'T15Y0125', 'T15Y0223', 'T15Y0224', 'T15Y0225', 'T15Y0226',
    ##        'T15Y0323', 'T15Y0324', 'T15Y0325', 'T15Y0326', 'T15Y0423',
    ##        'T15Y0424', 'T15Y0425', 'T15Y0426', 'T15Y0523', 'T15Y0524',
    ##        'T15Y0525', 'T15Y0526', 'T15Y0623', 'T15Y0624', 'T15Y0625',
    ##        'T15Y0626', 'T15Y0722', 'T15Y0723', 'T15Y0724', 'T15Y0725',
    ##        'T15Y0822', 'T15Y0823', 'T15Y0824', 'T15Y0825', 'T15Y0826',
    ##        'T15Y0922', 'T15Y0923', 'T15Y0924', 'T15Y0925', 'T15Y0926',
    ##        'T15Y1022', 'T15Y1023', 'T15Y1024', 'T15Y1025', 'T15Y1122',
    ##        'T15Y1123', 'T15Y1125', 'T15Y1222', 'T15Y1223', 'T15Y1224',
    ##        'T15Y1225', 'T20Y0128', 'T20Y0129', 'T20Y0131', 'T20Y0228',
    ##        'T20Y0229', 'T20Y0230', 'T20Y0231', 'T20Y0328', 'T20Y0329',
    ##        'T20Y0330', 'T20Y0428', 'T20Y0429', 'T20Y0430', 'T20Y0431',
    ##        'T20Y0528', 'T20Y0530', 'T20Y0531', 'T20Y0628', 'T20Y0629',
    ##        'T20Y0630', 'T20Y0631', 'T20Y0727', 'T20Y0728', 'T20Y0729',
    ##        'T20Y0730', 'T20Y0731', 'T20Y0827', 'T20Y0828', 'T20Y0829',
    ##        'T20Y0830', 'T20Y0831', 'T20Y0927', 'T20Y0928', 'T20Y0930',
    ##        'T20Y0931', 'T20Y1027', 'T20Y1028', 'T20Y1029', 'T20Y1030',
    ##        'T20Y1127', 'T20Y1128', 'T20Y1130', 'T20Y1227', 'T20Y1228',
    ##        'T20Y1229', 'T20Y1230', 'T5Y0112', 'T5Y0113', 'T5Y0114', 'T5Y0115',
    ##        'T5Y0116', 'T5Y0212', 'T5Y0213', 'T5Y0214', 'T5Y0215', 'T5Y0216',
    ##        'T5Y0313', 'T5Y0314', 'T5Y0315', 'T5Y0316', 'T5Y0412', 'T5Y0414',
    ##        'T5Y0415', 'T5Y0416', 'T5Y0514', 'T5Y0516', 'T5Y0613', 'T5Y0615',
    ##        'T5Y0616', 'T5Y0712', 'T5Y0713', 'T5Y0714', 'T5Y0716', 'T5Y0811',
    ##        'T5Y0812', 'T5Y0813', 'T5Y0814', 'T5Y0816', 'T5Y0911', 'T5Y0912',
    ##        'T5Y0913', 'T5Y0914', 'T5Y0915', 'T5Y0916', 'T5Y1011', 'T5Y1012',
    ##        'T5Y1013', 'T5Y1014', 'T5Y1015', 'T5Y1111', 'T5Y1112', 'T5Y1113',
    ##        'T5Y1114', 'T5Y1115', 'T5Y1211', 'T5Y1212', 'T5Y1213', 'T5Y1214',
    ##        'T5Y1215', 'TAKAFULINS', 'TALLUSPIN', 'TAMIJTEX', 'TAUFIKA',
    ##        'TITASGAS', 'TOSRIFA', 'TRUSTB1MF', 'TRUSTBANK', 'TUNGHAI', 'UCB',
    ##        'UNILEVERCL', 'UNIONCAP', 'UNIQUEHRL', 'UNITEDAIR', 'UNITEDFIN',
    ##        'UNITEDINS', 'UPGDCL', 'USMANIAGL', 'UTTARABANK', 'UTTARAFIN',
    ##        'VAMLBDMF1', 'VAMLRBBF', 'VFSTDL', 'WALTONHIL', 'WATACHEM',
    ##        'WMSHIPYARD', 'YPL', 'ZAHEENSPIN', 'ZAHINTEX', 'ZEALBANGLA'],
    ##       dtype=object)

``` python
df_2.head(10)#viewing fisrt 10 rows
```

    ##         date trading_code  last_traded_price  ...  trade  value_mn   volume
    ## 0 2021-12-30    1JANATAMF                6.2  ...    132     3.150   504433
    ## 1 2021-12-29    1JANATAMF                6.3  ...     82     1.197   190078
    ## 2 2021-12-28    1JANATAMF                6.3  ...    136     2.526   405009
    ## 3 2021-12-27    1JANATAMF                6.3  ...    115     1.463   235782
    ## 4 2021-12-26    1JANATAMF                6.2  ...    162     2.203   350769
    ## 5 2021-12-23    1JANATAMF                6.3  ...    124     1.807   283643
    ## 6 2021-12-22    1JANATAMF                6.4  ...    124     1.041   162579
    ## 7 2021-12-21    1JANATAMF                6.4  ...    115     2.497   389047
    ## 8 2021-12-20    1JANATAMF                6.5  ...    106     3.005   465218
    ## 9 2021-12-19    1JANATAMF                6.5  ...    311     6.571  1015043
    ## 
    ## [10 rows x 11 columns]

``` python
df_2.columns # will show the column/feature names of the stock dataset.
#with pd.option_context("display.max_columns=12", None):
   # display(df_2.head(10))
```

    ## Index(['date', 'trading_code', 'last_traded_price', 'high', 'low',
    ##        'opening_price', 'closing_price', 'yesterdays_closing_price', 'trade',
    ##        'value_mn', 'volume'],
    ##       dtype='object')

``` python
BATBC = df_2.loc[df_2['trading_code']=='BATBC']

BATBC['closing_price']
```

    ## 11333     635.6
    ## 11334     634.5
    ## 11335     637.2
    ## 11336     623.8
    ## 11337     627.2
    ##           ...  
    ## 11568    1207.9
    ## 11569    1198.5
    ## 11570    1213.3
    ## 11571    1252.2
    ## 11572    1249.4
    ## Name: closing_price, Length: 240, dtype: float64

``` python
BATBC = BATBC[['date','trading_code','closing_price']]
BATBC['date'] = pd.to_datetime(BATBC['date']).dt.date
BATBC.head(20)
```

    ##              date trading_code  closing_price
    ## 11333  2021-12-30        BATBC          635.6
    ## 11334  2021-12-29        BATBC          634.5
    ## 11335  2021-12-28        BATBC          637.2
    ## 11336  2021-12-27        BATBC          623.8
    ## 11337  2021-12-26        BATBC          627.2
    ## 11338  2021-12-23        BATBC          635.9
    ## 11339  2021-12-22        BATBC          639.7
    ## 11340  2021-12-21        BATBC          640.1
    ## 11341  2021-12-20        BATBC          639.0
    ## 11342  2021-12-19        BATBC          645.6
    ## 11343  2021-12-15        BATBC          657.4
    ## 11344  2021-12-14        BATBC          650.7
    ## 11345  2021-12-13        BATBC          655.4
    ## 11346  2021-12-12        BATBC          659.0
    ## 11347  2021-12-09        BATBC          650.2
    ## 11348  2021-12-08        BATBC          640.8
    ## 11349  2021-12-07        BATBC          646.1
    ## 11350  2021-12-06        BATBC          646.3
    ## 11351  2021-12-05        BATBC          639.0
    ## 11352  2021-12-02        BATBC          631.4

``` python
BATBC.to_csv('BATBC.csv', index = True)
```

# Analysis of Stock Market Trend Analysis of British American Tobacco Bangladesh

**Importing the data-set:**

``` r
df_batbc = read.csv("C:\\Users\\USER\\Downloads\\STA301\\BATBC.csv")
head(df_batbc,20)
```

    ##        X       date trading_code closing_price
    ## 1  11333 2021-12-30        BATBC         635.6
    ## 2  11334 2021-12-29        BATBC         634.5
    ## 3  11335 2021-12-28        BATBC         637.2
    ## 4  11336 2021-12-27        BATBC         623.8
    ## 5  11337 2021-12-26        BATBC         627.2
    ## 6  11338 2021-12-23        BATBC         635.9
    ## 7  11339 2021-12-22        BATBC         639.7
    ## 8  11340 2021-12-21        BATBC         640.1
    ## 9  11341 2021-12-20        BATBC         639.0
    ## 10 11342 2021-12-19        BATBC         645.6
    ## 11 11343 2021-12-15        BATBC         657.4
    ## 12 11344 2021-12-14        BATBC         650.7
    ## 13 11345 2021-12-13        BATBC         655.4
    ## 14 11346 2021-12-12        BATBC         659.0
    ## 15 11347 2021-12-09        BATBC         650.2
    ## 16 11348 2021-12-08        BATBC         640.8
    ## 17 11349 2021-12-07        BATBC         646.1
    ## 18 11350 2021-12-06        BATBC         646.3
    ## 19 11351 2021-12-05        BATBC         639.0
    ## 20 11352 2021-12-02        BATBC         631.4

``` r
unique(
  df_batbc$closing_price
)
```

    ##   [1]  635.6  634.5  637.2  623.8  627.2  635.9  639.7  640.1  639.0  645.6
    ##  [11]  657.4  650.7  655.4  659.0  650.2  640.8  646.1  646.3  631.4  626.1
    ##  [21]  620.4  626.7  627.1  628.0  629.3  634.2  634.8  647.6  635.3  618.5
    ##  [31]  619.2  609.0  619.1  623.5  618.6  623.4  638.6  616.4  606.4  632.6
    ##  [41]  649.3  640.7  654.8  672.4  697.7  688.7  712.9  717.3  725.0  732.7
    ##  [51]  746.9  719.8  712.7  699.7  699.6  696.4  684.0  678.9  681.9  685.6
    ##  [61]  669.6  651.0  652.2  652.6  653.3  651.8  649.9  648.0  648.8  656.2
    ##  [71]  657.0  652.5  658.2  662.2  658.8  660.2  649.0  605.4  594.9  585.1
    ##  [81]  586.7  582.5  569.9  569.6  569.4  568.1  567.7  562.5  563.6  566.2
    ##  [91]  567.6  568.6  572.3  572.9  571.3  569.1  567.5  568.9  570.5  567.2
    ## [101]  565.7  577.9  579.9  560.3  556.8  548.5  545.4  543.6  540.7  540.0
    ## [111]  539.4  540.4  546.9  539.1  534.3  533.1  531.7  539.2  538.0  542.1
    ## [121]  546.3  535.4  529.5  532.9  534.2  532.7  533.8  536.2  540.2  543.2
    ## [131]  544.1  544.6  547.2  543.4  549.1  553.2  560.2  566.0  557.1  557.8
    ## [141]  563.1  570.0  554.6  541.6  547.9  544.3  520.8  518.0  518.3  519.9
    ## [151]  519.3  518.2  518.4  519.7  520.9  524.4  523.6  525.6  523.0  522.9
    ## [161]  530.4  529.3  529.4  533.4  537.4  526.5  523.7  526.8  529.0  541.0
    ## [171]  542.2  546.8  549.8  560.5  527.5  532.4  539.9  552.8  557.7  570.4
    ## [181]  567.8  574.7  592.8  598.5 1554.0 1546.0 1544.9 1527.1 1522.7 1512.1
    ## [191] 1537.3 1592.3 1602.9 1621.5 1554.7 1599.0 1700.3 1509.7 1493.0 1496.9
    ## [201] 1596.4 1698.6 1598.7 1507.7 1488.5 1562.6 1602.5 1589.3 1506.1 1438.3
    ## [211] 1418.8 1424.4 1410.4 1418.3 1413.5 1446.6 1388.3 1337.5 1299.5 1253.0
    ## [221] 1227.6 1207.9 1198.5 1213.3 1252.2 1249.4

# 1. Abstract

Stock Market of Bangladesh is very much inconsistent in terms of closing
rates and returning rates. This paper aims to analyze the stock price
trend of British American Tobacco Bangladesh Company(BATBC). We will use
Markov Chain model to analyze this trend.

# 2. Data Collection:

This stock data set covers the period from January 2021 to December 2021
on the Dhaka Stock Exchange. Then, we limited our observations to those
made where BATBC is the Company and made a separate data-set containing
only the observations of BATBC.

# 3. Results:

The BATBC data set has 240 observations. The stock’s daily closing price
will be taken into account. Among these 240 observations, 221 stock
closing prices are distinct. We will investigate three different
scenarios in order to examine the trend: if the stock closing price is
higher or lower the day after a specific stock closing price, or if it
stays the same.

## 3.1 Determining the states:

There will be three states in total in the transition matrix. If the
stock closing price is higher than a specific day, we will denote
higher, if it is lower then lower, otherwise if it remains same then
constant.

## 3.2 Finding the transitional probability:

We encoded the closing_price column into those three states. We took
first observation as constant_closing_price and then if a stock closing
price was higher than current one, then we encoded as
higher_closing_price, and if it decreased, then it was encoded as
lower_closing_price.

``` r
print("hello world")
```

    ## [1] "hello world"

``` r
closing_price_encoded <- c("constant")
closing_price_encoded
```

    ## [1] "constant"

``` r
closing_price = df_batbc$closing_price
len_closing_price = length(df_batbc$closing_price)-1

len_closing_price
```

    ## [1] 239

``` r
for (i in 1:len_closing_price){
  if (closing_price[i] == closing_price[i+1]){
    closing_price_encoded <- c(closing_price_encoded,"constant")
  }
  
  else if (closing_price[i] < closing_price[i+1]){
    closing_price_encoded <- c(closing_price_encoded,"Higher")
  }
  
  
  else{
    closing_price_encoded <- c(closing_price_encoded,"Lower")
  }

}
closing_price_encoded
```

    ##   [1] "constant" "Lower"    "Higher"   "Lower"    "Higher"   "Higher"  
    ##   [7] "Higher"   "Higher"   "Lower"    "Higher"   "Higher"   "Lower"   
    ##  [13] "Higher"   "Higher"   "Lower"    "Lower"    "Higher"   "Higher"  
    ##  [19] "Lower"    "Lower"    "Lower"    "Lower"    "Higher"   "Higher"  
    ##  [25] "Higher"   "Higher"   "Higher"   "Higher"   "Higher"   "Lower"   
    ##  [31] "Lower"    "Higher"   "constant" "Lower"    "Higher"   "Higher"  
    ##  [37] "Lower"    "Higher"   "Higher"   "Lower"    "Lower"    "Higher"  
    ##  [43] "Higher"   "Lower"    "Higher"   "Higher"   "Higher"   "Lower"   
    ##  [49] "Higher"   "Higher"   "Higher"   "Higher"   "Higher"   "Lower"   
    ##  [55] "Lower"    "Lower"    "Lower"    "Lower"    "Lower"    "Lower"   
    ##  [61] "Higher"   "Higher"   "Lower"    "Lower"    "Higher"   "Higher"  
    ##  [67] "Higher"   "Lower"    "Higher"   "Lower"    "Higher"   "Lower"   
    ##  [73] "Higher"   "Higher"   "Higher"   "Lower"    "Higher"   "Higher"  
    ##  [79] "Lower"    "Higher"   "Lower"    "Lower"    "Lower"    "Lower"   
    ##  [85] "Higher"   "Lower"    "Lower"    "Lower"    "Lower"    "Lower"   
    ##  [91] "Lower"    "Lower"    "Higher"   "Higher"   "Higher"   "Higher"  
    ##  [97] "Higher"   "Higher"   "Lower"    "Lower"    "Lower"    "Higher"  
    ## [103] "Higher"   "Lower"    "Lower"    "Higher"   "Higher"   "Higher"  
    ## [109] "Lower"    "Lower"    "Lower"    "Lower"    "Lower"    "Lower"   
    ## [115] "Lower"    "Lower"    "Higher"   "Higher"   "Lower"    "Lower"   
    ## [121] "Lower"    "Lower"    "Higher"   "Lower"    "Higher"   "Higher"  
    ## [127] "Lower"    "Lower"    "Lower"    "constant" "Higher"   "Higher"  
    ## [133] "Lower"    "Higher"   "Higher"   "Higher"   "Higher"   "Higher"  
    ## [139] "Higher"   "Higher"   "Lower"    "Higher"   "Higher"   "Higher"  
    ## [145] "Higher"   "Lower"    "Higher"   "Higher"   "Higher"   "Lower"   
    ## [151] "Lower"    "Lower"    "Higher"   "Lower"    "Lower"    "Lower"   
    ## [157] "Higher"   "Higher"   "Lower"    "Lower"    "Higher"   "Higher"  
    ## [163] "Higher"   "Higher"   "Lower"    "Higher"   "Lower"    "Lower"   
    ## [169] "constant" "Higher"   "Higher"   "Lower"    "constant" "Higher"  
    ## [175] "Higher"   "Higher"   "Lower"    "Lower"    "Higher"   "Higher"  
    ## [181] "Higher"   "Higher"   "Higher"   "Higher"   "Higher"   "Higher"  
    ## [187] "Lower"    "Higher"   "Higher"   "Higher"   "Higher"   "Higher"  
    ## [193] "Lower"    "Higher"   "Higher"   "Higher"   "Lower"    "Higher"  
    ## [199] "constant" "Lower"    "Lower"    "Lower"    "Lower"    "Lower"   
    ## [205] "Higher"   "Higher"   "Higher"   "Higher"   "Lower"    "Higher"  
    ## [211] "Higher"   "Lower"    "Lower"    "Higher"   "Higher"   "Higher"  
    ## [217] "Lower"    "Lower"    "Lower"    "Higher"   "Higher"   "Lower"   
    ## [223] "Lower"    "Lower"    "Lower"    "Higher"   "Lower"    "Higher"  
    ## [229] "Lower"    "Higher"   "Lower"    "Lower"    "Lower"    "Lower"   
    ## [235] "Lower"    "Lower"    "Lower"    "Higher"   "Higher"   "Lower"

``` r
typeof(closing_price_encoded)
```

    ## [1] "character"

``` r
length(closing_price_encoded)
```

    ## [1] 240

Now replacing the closing_price column with encoded closing price array

``` r
df_batbc$closing_price <- closing_price_encoded
```

## **Defining the transitions:**

> As there are 3 states, there will be states \* states =3\*3 = 9
> transition states and from each state there will be 3 states outgoing.

**If the stock closing price is in state “Constant”:**

``` r
constant_to_constant = 0
constant_to_higher = 0
constant_to_lower = 0
```

**If the stock closing price is in state “Lower”:**

``` r
lower_to_constant = 0
lower_to_higher = 0
lower_to_lower = 0
```

**If the stock closing price is in state “Higher”:**

``` r
higher_to_constant = 0
higher_to_higher = 0
higher_to_lower = 0
```

### **Counting the outgoing occurrences from each state:**

> This loop will find the count from each state to other states in order
> to calculate the probability. We will run the loop till length-1 time
> in order to avoid array index out of bound error and to make the count
> accurate.

``` r
closing_price = df_batbc$closing_price
len_closing_price = length(df_batbc$closing_price)-1
for (i in 1:len_closing_price){
  if (closing_price[i] == "constant"){
    if(closing_price[i +1] == "constant"){
      constant_to_constant = constant_to_constant + 1
    }
    else if(closing_price[i +1] == "Lower"){
      constant_to_lower = constant_to_lower + 1
    }
    else{
      constant_to_higher = constant_to_higher + 1
    }
  }
  
  else if (closing_price[i] == "Lower"){
    if(closing_price[i +1] == "Lower"){
      lower_to_lower = lower_to_lower + 1
    }
    else if(closing_price[i +1] == "constant"){
      lower_to_constant = lower_to_constant + 1
    }
    else{
      lower_to_higher = lower_to_higher + 1
    }
  }
  
  else{
    if(closing_price[i +1] == "Higher"){
      higher_to_higher = higher_to_higher + 1
    }
    else if(closing_price[i +1] == "Lower"){
      higher_to_lower = higher_to_lower + 1
    }
    else{
      higher_to_constant = higher_to_constant + 1
    }
  }

}
```

``` r
constant_to_constant
```

    ## [1] 0

``` r
constant_to_higher 
```

    ## [1] 3

``` r
constant_to_lower
```

    ## [1] 3

``` r
lower_to_constant
```

    ## [1] 3

``` r
lower_to_higher
```

    ## [1] 45

``` r
lower_to_lower
```

    ## [1] 60

``` r
higher_to_constant
```

    ## [1] 2

``` r
higher_to_higher
```

    ## [1] 77

``` r
higher_to_lower
```

    ## [1] 46

### **Correcting the total count:**

> Earlier we have calculated how many observations are there for each
> state. But from one state there can be no outgoing state to more than
> one states. So we have to calculate to transition from each state.

``` r
constant_transition = constant_to_constant + constant_to_higher + constant_to_lower

lower_transition = lower_to_constant + lower_to_higher + lower_to_lower

higher_transition = higher_to_constant + higher_to_higher + higher_to_lower

constant_transition 
```

    ## [1] 6

``` r
lower_transition
```

    ## [1] 108

``` r
higher_transition
```

    ## [1] 125

**Defining the probability for transition matrix:**

### **Transition probability from constant to other states:**

``` r
p_constant_to_constant = constant_to_constant/ constant_transition
p_constant_to_higher = constant_to_higher/ constant_transition
p_constant_to_lower = constant_to_lower/ constant_transition

p_constant_to_constant
```

    ## [1] 0

``` r
p_constant_to_higher 
```

    ## [1] 0.5

``` r
p_constant_to_lower
```

    ## [1] 0.5

### **Transition probability from Lower to other states:**

``` r
p_lower_to_constant = lower_to_constant / lower_transition
p_lower_to_higher = lower_to_higher / lower_transition
p_lower_to_lower = lower_to_lower / lower_transition

p_lower_to_constant
```

    ## [1] 0.02777778

``` r
p_lower_to_higher
```

    ## [1] 0.4166667

``` r
p_lower_to_lower
```

    ## [1] 0.5555556

### **Transition probability from Higher to other states:**

``` r
p_higher_to_constant = higher_to_constant / higher_transition
p_higher_to_higher = higher_to_higher / higher_transition
p_higher_to_lower = higher_to_lower / higher_transition

p_higher_to_constant
```

    ## [1] 0.016

``` r
p_higher_to_higher
```

    ## [1] 0.616

``` r
p_higher_to_lower
```

    ## [1] 0.368

### 

# **2. Selecting the Markov Chain Model:**

> I have imported the “markovchain” library to analyse the weather data.
>
> Now applying markov chain

### **Building the transition matrix for markov chain model**

> Transition matrix is essential to make a markov chain model. I
> calculated 1-step transition probabilities from each state manually.
> Now I will create a matrix using those probabilities. In this matrix
> byrow = True is enabled that will make the matrix created by row
> values.

``` r
transition_matrix_stock <- matrix(c(p_higher_to_higher, p_higher_to_lower,p_higher_to_constant, p_lower_to_higher , p_lower_to_lower,p_lower_to_constant , p_constant_to_higher , p_constant_to_lower,p_constant_to_constant),nrow = 3, byrow = TRUE)
```

We can see the transition matrix now.

``` r
stateNames_stock <- c("Higher","Lower","Constant")
row.names(transition_matrix_stock) <- stateNames_stock; colnames(transition_matrix_stock) <- stateNames_stock

round(transition_matrix_stock,3)
```

    ##          Higher Lower Constant
    ## Higher    0.616 0.368    0.016
    ## Lower     0.417 0.556    0.028
    ## Constant  0.500 0.500    0.000

### **Making the Discrete Markov chain model:**

> Using a Markov chain object, here I have built a discrete markov chain
> model.

``` r
discrete_transition_matrix_stock <- new("markovchain",transitionMatrix=transition_matrix_stock, 
             states=c("Higher","Lower","Constant"),
             name="MarkovChain for Stock Price trend Analysis")
discrete_transition_matrix_stock
```

    ## MarkovChain for Stock Price trend Analysis 
    ##  A  3 - dimensional discrete Markov Chain defined by the following states: 
    ##  Higher, Lower, Constant 
    ##  The transition matrix  (by rows)  is defined as follows: 
    ##             Higher     Lower   Constant
    ## Higher   0.6160000 0.3680000 0.01600000
    ## Lower    0.4166667 0.5555556 0.02777778
    ## Constant 0.5000000 0.5000000 0.00000000

### **Plotting the transition matrix**

``` r
plotmat(t(round(transition_matrix_stock,2)),pos = c(2,1), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.12, 
        box.type = "circle", 
        box.prop = 0.7,
        box.col = "green",
        arr.length=.4,
        arr.width=.2,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .17,
        main = "One step transition probability")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

## The code chunk below is built in Markov chain transition matrix and model builder:

I have added this code to cross check my own calculated transition
matrix with the built in library’s result. Here we can see the results
are same.

``` r
sequence <- closing_price

mcFit <- markovchainFit(data=sequence)
mcFit
```

    ## $estimate
    ## MLE Fit 
    ##  A  3 - dimensional discrete Markov Chain defined by the following states: 
    ##  Higher, Lower, constant 
    ##  The transition matrix  (by rows)  is defined as follows: 
    ##             Higher     Lower   constant
    ## Higher   0.6160000 0.3680000 0.01600000
    ## Lower    0.4166667 0.5555556 0.02777778
    ## constant 0.5000000 0.5000000 0.00000000
    ## 
    ## 
    ## $standardError
    ##              Higher      Lower   constant
    ## Higher   0.07019972 0.05425864 0.01131371
    ## Lower    0.06211300 0.07172191 0.01603751
    ## constant 0.28867513 0.28867513 0.00000000
    ## 
    ## $confidenceLevel
    ## [1] 0.95
    ## 
    ## $lowerEndpointMatrix
    ##             Higher     Lower constant
    ## Higher   0.4784111 0.2616550        0
    ## Lower    0.2949274 0.4149832        0
    ## constant 0.0000000 0.0000000        0
    ## 
    ## $upperEndpointMatrix
    ##             Higher    Lower   constant
    ## Higher   0.7535889 0.474345 0.03817447
    ## Lower    0.5384059 0.696128 0.05921072
    ## constant 1.0000000 1.000000 0.00000000
    ## 
    ## $logLikelihood
    ## [1] -181.1351

``` r
initial_state <- matrix(c(1,0,0),nrow=1, byrow=TRUE)
```

# **Analysis of stock closing price trend after next 7 days**

### **Defining Current State**

> Let’s take clear day as our current state , that is the probability
> that the stock closing price goes from Lower to High after the next 7
> days. I defined my state row matrix as this order
> **`: "Higher","Lower","Constant",`**, so the vector for the vector for
> choosing a clear Day is: **`(0,1,0)`**

``` r
initial_state_stock <- matrix(c(0,1,0),nrow=1, byrow=TRUE)
```

## **Analysis of stock closing price trend after 2 days**

``` r
days <- 2
after_two_days_stock <- round(initial_state_stock * discrete_transition_matrix_stock ^ days,2)
after_two_days_stock
```

    ##      Higher Lower Constant
    ## [1,]    0.5  0.48     0.02

``` r
after_two_days_stock <- round(transition_matrix_stock%^%days,2)
after_two_days_stock
```

    ##          Higher Lower Constant
    ## Higher     0.54  0.44     0.02
    ## Lower      0.50  0.48     0.02
    ## Constant   0.52  0.46     0.02

``` r
plotmat(t(round(after_two_days_stock,2)),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Analysis of stock closing price trend after 2 days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

## **Analysis of stock closing price trend after 7 days**

``` r
days <- 7
after_seven_days_stock <- round(initial_state_stock * discrete_transition_matrix_stock ^ days,2)
after_seven_days_stock
```

    ##      Higher Lower Constant
    ## [1,]   0.52  0.46     0.02

``` r
after_seven_days_stock <- round(transition_matrix_stock%^%days,2)
after_seven_days_stock
```

    ##          Higher Lower Constant
    ## Higher     0.52  0.46     0.02
    ## Lower      0.52  0.46     0.02
    ## Constant   0.52  0.46     0.02

``` r
plotmat(t(round(after_seven_days_stock,2)),pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "green",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Analysis of stock closing price trend after 7 days")
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

## Simulation of Multi Nomial Markov Chain on BATBC Stock Price:

> We will use transition matrix `transition_matrix_stock` to simulate

``` r
round(transition_matrix_stock,2)
```

    ##          Higher Lower Constant
    ## Higher     0.62  0.37     0.02
    ## Lower      0.42  0.56     0.03
    ## Constant   0.50  0.50     0.00

``` r
simulation_for_stock <- function( P, t) { 

  # number of possible states

  num.states <- nrow(P)
  print("num.states")
  print(num.states)
  # stores the states X_t through time

  states <- numeric(t)
  print('the value of state is:')
  print(states)
  # initialize variable for first state 

  states[1]    <- 1


  for(i in 2:t) { #i=2

    # probability vector to simulate next state X_{t+1}

    a<-states[i-1]
   
    pt  <- P[a, ]
   
    ## draw from multinomial and determine state

    states[i] <-  which(rmultinom(1, 1, pt) == 1)
   
  }

  return(states)

}
```

``` r
# setup transition matrix p, here we will use transition_matrix_weather

P <- transition_matrix_stock

#num.iterations 

t=100

simulation_for_stock_states<-simulation_for_stock(P,t)
```

    ## [1] "num.states"
    ## [1] 3
    ## [1] "the value of state is:"
    ##   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [38] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

``` r
print("simulation_for_stock_states:")
```

    ## [1] "simulation_for_stock_states:"

``` r
simulation_for_stock_states
```

    ##   [1] 1 1 1 2 2 1 2 2 2 2 1 1 1 2 2 2 2 1 1 1 2 1 1 1 1 2 2 2 1 1 1 2 2 2 2 2 2
    ##  [38] 2 2 2 2 2 1 1 1 1 2 2 2 2 2 1 1 1 2 2 1 1 1 1 2 1 1 2 2 1 1 2 1 1 1 1 1 1
    ##  [75] 1 2 1 2 2 1 2 1 2 2 1 1 1 2 1 1 1 2 1 1 1 1 1 1 3 1

``` r
plot(simulation_for_stock_states,type="b",col='green')
```

![](Markov-Chain-Analysis_-G.M.-Arafat-Rahman_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->
