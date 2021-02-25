###


### dependencies
###########################################################################

library(ggplot2)
library(readr)
library(dplyr)

### data processing
############################################################################

# loading in data
contributions <- read_csv("contributions.csv",
                          col_types = cols(
                            contributor_lastname = col_skip(),
                            contributor_firstname = col_skip()), 
                          na = "0")

committees <- read_csv("committees.csv", 
                       col_types = cols(), 
                       na = "0")

outcomes <- data.frame(
  prop=c(seq(51,67)),
  outcomes=c(1,1,0,1,1,1,1,1,1,0,0,0,1,1,0,1,1)
  )

## X : full dataset

X = merge(committees, contributions, by = "calaccess_committee_id")

## X2 : modeling subset

X2=data_frame("amount"=X$amount,
                "zip"=X$contributor_zip,
                "prop"=X$prop_name,
                "number"=X$ccdc_prop_id-19,
                "founder"=X$contributor_is_self_employed,
                "job"=X$contributor_occupation,
                "stance"=X$committee_position,
                "size"=abs(amount))

X2=merge(x=X2,
         y=outcomes,
         by.x = 'number',
         by.y = 'prop')

## Missing Data Subset
X3 = X2[is.na(X2$amount),]

## Log Modeling View 
X4 = cbind(outcomes, X2 %>% filter(!is.na(size)) %>% group_by(number) %>% summarize("sum"=sum(size)))
           

## Paradox : How can the amount be negative 
## while the committee supports the prop the contribution was made under?
#################################################################################

odd_obs <- X[X$amount < 0 & X$committee_position=="SUPPORT",]

# odd_obs per job
odd_obs %>%
  group_by("G"=odd_obs$prop_name) %>%
  summarize("N"=n()) %>%
  filter(!is.na(G)) %>%
  ggplot(aes(G,N)) +
  geom_col(col="black", fill="lightblue") +
  theme_light() +
  xlab(label="Propositions") +
  ylab(label="") +
  coord_flip() +
  ggtitle(label = "A Glitch in the System",
          subtitle = "Frequency of Negative Supporting Contributions by Proposition") +
  scale_x_discrete(labels=c("Prop 51",
                            "Prop 59",
                            "Prop 61",
                            "Prop 62",
                            "Prop 63",
                            'Prop 64',
                            "Prop 66"))

## prop 63 over-represented for with this issue.

## Missing Data
colSums(is.na(X))
## all the data is here with the exception of amounts in 16 records

X3 %>% 
  group_by(number) %>%
  summarise(n())
# 16 obs, 14 of which are for prop 63. 
#These are firearms amounts, why are so many of these missing?

## Missing Amounts Plot
X3 %>% 
  group_by(number) %>%
  summarize("N"=n()) %>%
  ggplot(aes(number, N)) +
  geom_col(fill="lightblue", col="black") +
  scale_x_discrete(
    name="Proposition",
    labels=c("Prop 63 - Gun Control",
             "Prop 67 - Plastic Bag Referendum")) +
  labs(
    title="Undisclosed Firearms donation amounts",
    subtitle = "Frequency of Missing Data grouped by Proposition") +
  theme_light()

# Prop 63 also overrepresented with in missing data

### AGGREGATES & AVERAGES
##############################################################################

sum(abs(X$amount), na.rm = T)

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
  summarize("total"=sum(size,na.rm=T),"N"=n()) %>%
  mutate(total/N) 

# contributions to oppose overturning the ban
# were on average over three hundred times higher.

### Prop 63
prop63 = X2[X2$number==63,]
prop63 %>% group_by(founder) %>%
  summarize("mean_size"=mean(size, na.rm=T),
            'sum_size'=sum(size, na.rm=T))

### Statistical Analysis

### Logistic Regression
glm(X4$outcomes ~ X4$sum,family = 'binomial') %>% summary()
ggplot(X4, aes(y=sum)) + geom_boxplot() + facet_wrap(~outcomes)
t.test(X4$sum ~ X4$outcomes,alternative="g")
