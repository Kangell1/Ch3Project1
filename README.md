# Ch3Project1
Springboard Chapter 3 Project 1
library(tidyr)
library(readr)
library(dplyr)
library(utils)
refine_original <- read_csv("Documents/Course_Work /Springboard/Data/refine_original.csv")


###fix names and create 2 columns 

refine_clean<- refine_original %>% 
  mutate(company=ifelse(grepl("^f", company, ignore.case = TRUE), "philips", company)) %>%
  mutate(company=ifelse(grepl("^ph", company, ignore.case = TRUE), "philips", company)) %>%
  mutate(company=ifelse(grepl("^a", company, ignore.case = TRUE), "akzo", company)) %>%
  mutate(company=ifelse(grepl("^u", company, ignore.case = TRUE), "unilever", company)) %>%
  mutate(company=ifelse(grepl("^v", company, ignore.case = TRUE), "van_houten", company)) 


refine_clean
###create product categories out of product code
refine_clean <- refine_clean %>% 
  separate('Product code / number', into=c("product_code", "product_number"), sep="-")


###create product categories out of product code
refine_clean <- refine_clean %>% 
  mutate(product_category = case_when(product_code == "p" ~ "smartphone",
                                      product_code == "v" ~ "TV",
                                      product_code == "x" ~ "laptop",
                                      product_code == "q" ~ "tablet"))
                          


###geocode addresses
refine_clean <- refine_clean %>% unite(full_address, address, city, country, sep=",")
###create dummy variables
refine_clean$company_philips=refine_clean$company== "philips"
refine_clean$company_akzo=refine_clean$company=="akzo"
refine_clean$company_van_houten=refine_clean$company=="van houten"
refine_clean$company_unilever=refine_clean$company=="unilever"

refine_clean$product_smartphone=refine_clean$product_category=="smartphone"
refine_clean$product_tv=refine_clean$product_category=="TV"
refine_clean$product_laptop=refine_clean$product_category=="laptop"
refine_clean$product_tablet=refine_clean$product_category=="tablet"
