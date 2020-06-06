#Data wrangling ----
getwd()
setwd("C:/Users/John Karuitha/Documents/my_projects/data_wr")

#Load datasets ----
library(tidyverse)
library(dslabs)
library(readxl)

##Path to data files ----
path <- system.file("extdata", package = "dslabs")
list.files(path)

#Generate a full path to a file ----
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath
#Copy file to working directory ----
file.copy(fullpath, getwd())
file.exists("murder.csv")

##Read first few lones of a file ----
read_lines("murders.csv", n_max = 3)

##Evaluation 
filename <- "murders.csv"
path <- system.file("extdata", package = "dslabs")
file.copy(file.path(path, "murders.csv"), getwd())

## Read data----
aaa <- read_csv("http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", 
         col_names = F)


## Import data in wide format ----
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)
wide_data <- wide_data %>% gather(year, fertility, -country)
head(wide_data)

####Exercises - seperate a dataset ----
head(wide_data)
wide_data <- wide_data %>% separate(fertility, into = c("full", "fraction"))
wide_data <- wide_data %>% separate(year, into = c("century", "year"), sep = 2)
wide_data <- wide_data %>% unite(new, century, year, sep = "")
wide_data <- wide_data %>% unite(fertility, full, fraction, sep = ".")

##Exercising unite and separate commands on admisiions data
data("admissions")
head(admissions)
admissions <- admissions %>% separate(admitted, into = c("tens", "ones"), sep = 1)
admissions <- admissions %>% unite(admitted, tens, ones, sep = "")
admissions <- admissions %>% separate(applicants, into = c("hundreds", "tens", "ones"), sep = c(1, 1))
admissions <- admissions %>% unite(applicants, hundreds, tens, ones, sep = "")

##Separating a bit complex file ----
spread <- read_table(file.choose())
dat <- spread
dat_tidy <- dat %>% spread(key = var, value = people)

##Times data----
d <- read.csv("time.csv")
##cant work
tidy_data <- d %>% 
  gather(key = “key”, value = “value”, -age_group) %>% 
  separate(col = key, into = c(“year”, “variable_name”), sep = “.”) %>% 
  spread(key = variable_name, value = value)
##
tidy_data <- d %>%
  gather(key = “key”, value = “value”, -age_group) %>%
  separate(col = key, into = c(“year”, “variable_name”), sep = “_”) %>% 
  spread(key = variable_name, value = value)
##
dev <- stats
stats <- read_csv("stats.csv")
head(stats)
stats <- stats %>% separate(key, into = c("player", "variable_name"), 
      sep = "_", extra = "merge") %>% spread(key = player, value = value)

##CO2 Data ----
data(co2)
head(CO2, 20)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>% mutate(year = as.character(1959:1997))

co2_long <- co2_wide %>% gather(wide, )


###Applicants ----
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)
dat
spread(dat, gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(gender, key))
tmp2

##Evaluation - joining tables
library(tidyverse)
tab1 <- read.table(file.choose(), sep = ",", header = TRUE)
tab2 <- read.table(file.choose(), sep = ",", header = TRUE)
left_join(tab1, tab2, by = "state")
right_join(tab1, tab2, by = "state")
full_join(tab1, tab2, by = "state") 
inner_join(tab1, tab2, by = "state") 
semi_join(tab1, tab2, by = "state") 
