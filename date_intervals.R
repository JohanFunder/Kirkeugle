# Load file
date <- read.csv(file=date, header= TRUE, sep = ',', dec = '.')
str(x)

# lav function for dato med intervaller på n mellemrum
nth_element <- function(vector, starting_position, n) { 
  vector[seq(starting_position, length(vector), n)] 
}

trim_date <- nth_element(date, 1, 14)
trim_date
