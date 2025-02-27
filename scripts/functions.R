#<function_name> <- function(arg1, arg2, .....) {
#  <code for function to run>
#  return(<something>)
#  }

interior_f <- c(186, 175, 182)
interior_c <- (interior_f-32) * 5 / 9

exterior_f <- c(75, 78, 80)
exterior_c <- (exterior_f - 32) * 5 / 9


surface_f <- c(103, 109, 98)
surface_c <- (surface_f - 32) * 5 / 9

convert_f_to_c <- function(fahr) {
 celcius <- (fahr - 32) * 5 / 9
 return(celcius) 
 }

convert_f_to_c(surface_f)

#define function to convert C to F
#F = C*9 / 5 +32


interior_f <- (interior_c+32) * 9 / 5
interior_c <- c(86, 75, 82)

convert_c_to_f <- function(celcius) {
  fahr <- (celcius + 32) * 9 / 5
  return(fahr) 
}

convert_c_to_f(interior_c)

##############

convert_temps <- function(temp, units = "F"){
  if(units %in% c("C", "F")){
    stop("Units must be C or F")  
}
  
  
  if(units == "F") {
    fahr <- temp
    celcius <- convert_f_to_c(fahr)
  } else {
    celcius <- temp
    fahr <- convert_c_to_f(celcius)
  }
  
  kelvin <- celcius + 273.15
  
  out_df <- data.frame(fahr, celcius, kelvin)
  
  return(out_df)
}

convert_temps(surface_f)

###########

library(dplyr)

data_frame(f = surface_f) %>% 
  mutate(c = convert_f_to_c(f))

#############

add_hot_or_cold <- function(df, thresh = 70) {
  if(!"fahr"%in% names(df)){
    stop("Data frame must have fahr column!")
  }
  out_df <- df %>% 
    mutate(hotcold = ifelse(fahr > thresh, "hot", "cold"))
  return(out_df)
  
}

data_frame(fahr = surface_f) %>% 
  mutate(c = convert_f_to_c(fahr)) %>% 
  add_hot_or_cold()

 ############
