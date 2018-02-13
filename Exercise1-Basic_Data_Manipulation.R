# load packages
library(plyr)
library(dplyr)
library(tidyr)


refine_clean <- read.csv("refine_original.csv")

# make all variables lowercase
refine_clean$company <- lapply(refine_clean$company, tolower)

#clean up spelling mistakes in company column- use gsub
refine_clean$company <- gsub(pattern = "^a.*", "akzo", refine_clean$company)
refine_clean$company <- gsub(pattern = "^(p|f).*", "philips", refine_clean$company)
refine_clean$company <- gsub(pattern = "^u.*", "unilever", refine_clean$company)

#separate product and code number into two columns-product_code and product_number

refine_clean <- refine_clean %>%
  separate(col = Product.code...number, into = c("product_code", "product_number"), 
         sep = "-", remove = TRUE)

#add product categories
refine_clean <- refine_clean %>%
  mutate(product_category = ifelse(product_code == "p", "Smartphone", ifelse(product_code == "v", "TV", 
                                                                         ifelse(product_code == "x", "Laptop", 
                                                                                ifelse(product_code == "q", "Tablet", "NA")))))


#adding full address for geocoding
refine_clean <- refine_clean %>%
  unite("full_address", c("address", "city", "country"), sep = ",", remove = TRUE)

#create dummy variables for company and product categories
#philips company variable creation
refine_clean <- refine_clean %>%
  mutate(company_philips = ifelse(company == "philips", 1, 0))

#azko company variable creation
refine_clean <- refine_clean %>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0))

#van houten variable creation
refine_clean <- refine_clean %>%
  mutate(company_van_houten = ifelse(company == "van houten", 1, 0))

#unilever variable creation
refine_clean <- refine_clean %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))

#smartphone variable creation
refine_clean <- refine_clean %>%
  mutate(product_smartphone = ifelse(product_code == "p", 1, 0))

#TV variable creation
refine_clean <- refine_clean %>%
  mutate(product_tv = ifelse(product_code == "v", 1, 0))

#Laptop variable creation
refine_clean <- refine_clean %>%
  mutate(product_laptop = ifelse(product_code == "x", 1, 0))

#tablet variable creation
refine_clean <- refine_clean %>%
  mutate(product_tablet = ifelse(product_code == "q", 1, 0))


#write cleaned data set to csv file
write.csv(refine_clean, file = "refine_clean.csv")

