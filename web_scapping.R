##Web scrapping ----
rm(list = ls())
library(tidyverse)
library(rvest)

## Harvest data from TIOBE ----
web_page <- "https://www.tiobe.com/tiobe-index/" ##get url
data <- read_html(web_page) ##read the html webpage
nodes <- html_nodes(data, "table") ##extract table nodes - 
#also data %>% html_nodes("table")
first_tab <- nodes[[1]] %>% html_table()
second_tab <- nodes[[2]] %>% html_table()
third_tab <- nodes[[3]] %>% html_table()
fourth_tab <- nodes[[4]] %>% html_table()

##second exercise on web scrapping----
##import website
url <- "https://www.datatables.net/"
data_tables <- read_html(url)
##capture the nodes 
nodes2 <- html_nodes(data_tables, "table")
##extract table
final <- nodes2[[1]] %>% html_table(header = TRUE)
final
final <- final[-58, ]

##Third exercise ----
##Baseball payrolls ----
page <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
load_page <- read_html(page) #loading the web page
nodes_page <- html_nodes(load_page, "table") ##get all table nodes
nodes_page
#extract first table
payroll1 <- nodes_page[[1]] %>% html_table(header = TRUE)
payroll1
class(payroll1)

payroll2 <- nodes_page[[2]] %>% html_table(header = TRUE)
payroll2

payroll3 <- nodes_page[[3]] %>% html_table(header = TRUE)
payroll3

payroll4 <- nodes_page[[4]] %>% html_table(header = TRUE)
payroll4

tab_1 <- nodes_page[[10]] %>% html_table(header = TRUE)
str(tab_1)
tab1 <- tab_1 %>% select(Team, Payroll, Average)
tab_1

tab_2 <- nodes_page[[19]] %>% html_table(header = TRUE)
tab_2
str(tab_2)

tab_3 <- full_join(tab_1, tab_2, by = "Team")
nrow(tab_3)

##Brexit referendum ----
web_brexit <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
brexit <- read_html(web_brexit)
##get nodes 
brexit_nodes <- brexit %>% html_nodes("table")
brexit_nodes
length(brexit_nodes)

##Get tables 
brexit_1 <- brexit_nodes[[1]] %>% html_table(header = TRUE, fill = TRUE)
brexit_2 <- brexit_nodes[[2]] %>% html_table(header = TRUE, fill = TRUE)
brexit_3 <- brexit_nodes[[3]] %>% html_table(header = TRUE, fill = TRUE)
brexit_4 <- brexit_nodes[[4]] %>% html_table(header = TRUE, fill = TRUE)
brexit_5 <- brexit_nodes[[5]] %>% html_table(header = TRUE, fill = TRUE)
ncol(brexit_5)


##Comprehensive pipe for web scrapping ----
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

##US Homicide rates by state ----
homicide <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_homicide_rate") %>% 
  html_nodes("table") %>% html_table(header = TRUE, fill = TRUE) %>% .[[1]] %>% 
  pivot_longer(8:2, names_to = "Year")
homicide$Rate <- homicide$value
homicide <- homicide %>% select(State, Year, Rate)               

##US Gun murders by state ----
gun_murders <- read_html("https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167") %>% 
  html_nodes("table") %>% html_table(header = TRUE, fill = TRUE) %>% .[[1]] %>% 
  setNames(c("state", "population", "total", "murder_rate"))

commas <- function(x){any(str_detect(x, ","))}
gun_murders %>% summarize_all(funs(commas))
##replace all commas in pop 
gun_murders$population <- parse_number(gun_murders$population)

##A more complex data wrangling example ----
library(dslabs)
data("reported_heights")
head(reported_heights)
class(reported_heights$height)
View(reported_heights)
##Separate out the date and time 
reported_heights <- reported_heights %>% 
  separate(.,time_stamp, into = c("Date", "Time"), sep = " ", extra = "merge")
##See the problematics versions
x <- as.numeric(reported_heights$height)
sum(is.na(x))

##Keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>% 
  filter(is.na(new_height)) %>% head(n = 10)

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6 ##significance level
qnorm(1-alpha/2, 69.1, 2.9) ##metrics for tallest --- mean and sd
qnorm(alpha/2, 63.7, 2.7) ## metrics for shortest --- mean and sd

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

not_inches(reported_heights$height)
sum(not_inches(reported_heights$height))

# number of problematic entries
problems <- reported_heights %>% filter(not_inches(height)) %>% 
  .$height

length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat