

### dependencies
###########################################################################

library(ggplot2)
library(readr)
library(dplyr)

### data processing
############################################################################

contributions <- read_csv("contributions.csv",
                          col_types = cols(
                            contributor_lastname = col_skip(),
                            contributor_firstname = col_skip()), 
                          na = "0")

committees <- read_csv("committees.csv", 
                       col_types = cols(), 
                       na = "0")

### Views
##############################################################################

## X : full dataset

X = merge(committees, contributions, by = "calaccess_committee_id")

## X2 : modelling subset

X2 = data_frame("amount"=X$amount,
                "zip"=X$contributor_zip,
                "prop"=X$prop_name,
                "number"=X$ccdc_prop_id-19,
                "founder"=X$contributor_is_self_employed,
                "job"=X$contributor_occupation,
                "stance"=X$committee_position,
                "size"=abs(amount))

odd_obs <- X[X$amount < 0 & X$committee_position=="SUPPORT",]

## Paradox : How can the amount be negative 
## while the committee supports the prop the contribution was made under?
#################################################################################

odd_obs <- X[X$amount < 0 & X$committee_position=="SUPPORT",]

# odd_obs per job
odd_obs %>%
  group_by(odd_obs$prop_name) %>%
  summarize(n()) 
## prop 63 over-represented for with this issue.

## Missing Data

###############################################################################
colMeans(is.na(X2)) %>% round(5) * 100
indexsizena = which(is.na(X2$amount))

# X3 : amount censored.
X3 = X2[indexsizena,]

X3 %>% 
  group_by(job) %>%
  summarise(n())
# 16 obs, 10 of which are retired

X3 %>% 
  group_by(number) %>%
  summarise(n())
# 16 obs, 14 of which are for prop 63. 
#These are firearms amounts, why are so many of these missing?


### AVERAGES
##############################################################################

# There are 17 unique propositions
unique_props <- sort(unique(committees$prop_name))

# number of unique contributions
unique_contributions <- X %>%
  summarize(N=n()) %>%
  unlist()

# average contributions per proposition
avg.contr.per.prop = unique_contributions/length(unique_props)

# average amount per proposition
avg.amount.per.prop = sum(X$amount,
                          na.rm = T)/unique_contributions

# average size per prop
avg.size.per.prop = sum(X2$size,
                        na.rm = T)/unique_contributions ### data processing ions

# average amount per prop per contributor
avg.dollar.per.contr=avg.amount.per.prop/avg.contr.per.prop
## this is about one, meaning on average one donor corresponds with one dollar.

## Propositions Analysis
###################################################################################################

### Prop 67
###################################################################################################

prop67 = X2[X2$number==67,]
prop67 %>%
  group_by(founder) %>%
  summarize("mean_size"=mean(size, na.rm=T),
            'sum_size'=sum(size, na.rm=T))

# top 10 jobs by gross contributions
prop67 %>%
  group_by(job) %>%
  summarise("N"=n()) %>%
  arrange(desc(N)) %>%
  mutate("mean"=sum())

# top 10 by avg contribution size
prop67 %>% group_by(job) %>%
  summarise("avgsize"=mean(size, na.rm = T),
            "contribs"=n(), amount=sum(amount, na.rm=T)) %>%
  arrange(desc(avgsize))

## SUPPORT
yes_on_67 = prop67 %>%
  subset(stance=="SUPPORT")

yes_on_67 %>%
  summarize(n()) # 6252 voted in support of over-turning the ban

yes_on_67 %>%
  group_by(founder) %>%
  summarize(mean=n()/nrow(yes_on_67)) 
# 88% of the contributions were non founders

yes_on_67 %>%
  group_by(founder) %>%
  summarize("total"=sum(size,na.rm=T),"N"=n()) %>%
  mutate(total/N) 
# non founder supporters of prop 67 made contributions that were 
# thrice as high on average,
# $110 and ~$300.

yes_on_67 %>%
  summarize("total"=sum(size,na.rm=T),"N"=n()) %>% mutate(total/N) 
#contributions to support the ban average $286


## OPPOSE
no_on_67 = prop67 %>% subset(stance=="OPPOSE")  

no_on_67 %>%
  summarize(n()) # all 27 voted in support of over-turning the ban

no_on_67 %>%
  group_by(founder) %>%
  summarize(n()) # all 27 that opposed were non founders

no_on_67 %>%
  summarize("total"=sum(size,na.rm=T),"N"=n()) %>%
  mutate(total/N) 
# contributions to oppose overturning the ban
# were on average over three hundred times higher.

### Prop 63
prop63 = X2[X2$number==63,]
prop63 %>% group_by(founder) %>%
  summarize("mean_size"=mean(size, na.rm=T),
            'sum_size'=sum(size, na.rm=T))