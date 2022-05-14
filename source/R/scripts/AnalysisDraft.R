

### dependencies
###########################################################################

library(ggplot2)
library(readr)
library(dplyr)
library(car)


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

X2=tibble("amount"=X$amount,
                "name"=X$committee_name.x,
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

## X4 : Logistic Modeling & T-test View 
c("51"=1, "52"=(12756517.3/(12756517.3+3526622.3)), "53"=(3500000.0/(3500000.0+18840500.0)), "54"=1, "55"=1, "56"=(33683287.7/(33683287.7+54686004.0)), "57"=(19307322.7/(19307322.7+791047.0)), "58"=1, "59"=1, "60"=(4655000.0/(4655000.0	+ 379221.0)), "61"=(18897535.2/(18897535.2	+ 108417232.0)), "62"=(16001308.4/(16001308.4	+ 16840069.8)), "63"=(2850469.1)/(2850469.1+701727.6), "64"= (32978406.0/(32978406.0 + 2501211.6)), "65"=1, "66"=(16840069.8/(16840069.8+17455869.5)), "67"=(1789160.8/(1789160.8+2681413.2))) -> prop.dollars.support

c("51"=1, "52"=12/(12+3), "53" = 7/(165+7), "54" = 1, "55"=1, "56"=119/(119+46), "57"=91/(91+146), "58"=1, "59" = 1, "60" = 21/(21+94), "61" = 695/(695+111), "62"=4809/(4809	+ 29633	),	
  "63"=(5457/(5457+5554)), "64"=762/(762+98), "65"=1, "66"=29633/(29633+4195), "67"=6250/(6250+27)) -> prop.contribs.support

outcomes <- data.frame(
  prop=c(seq(51,67)),
  outcomes=c(1,1,0,1,1,1,1,1,1,0,0,0,1,1,0,1,1)
)

X2 %>% filter(!is.na(size)) %>% group_by(number) %>% summarize("sum"=sum(size)) -> M

X4 = cbind(outcomes, prop.dollars.support, prop.contribs.support,
           X2 %>%
             filter(!is.na(size)) %>%
             group_by(number) %>% 
             summarize("sum"=sum(size), N=n()))          

X5 = X2[!is.na(X2$amount),]

X6 <- cbind(outcomes, prop.contribs.support, prop.dollars.support)

X7 <- merge(X6, X5, by.x = "prop", by.y = "number")

X8 <- cbind(X4, tmp$mean) 

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

### Number of contributions per proposition. 
X2 %>% group_by(prop) %>% summarize(sum=n())


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


### GRAPHS

## Contributions by Proposition
ggplot(X2, aes(size, stance, col=stance)) +
  geom_point(size=1, alpha=1/100, na.rm=T) +
  geom_violin(scale="count") + 
  scale_x_continuous(breaks=c(4000000,8000000,12000000), labels=c("$4M","$8M","$12M")) +
  coord_flip() +
  geom_jitter(height = .03) +
  facet_wrap(~number, ncol = 4) +
  theme_light(base_rect_size = .01) +
#  labs(title = "Violin Dot-Plot of Contributions to the 2016 California Ballot ",
#       subtitle = "contributions amounts split by stance, facetted over proposition.") +
  theme(strip.background =element_rect(fill="cadetblue"))
  
###

X2 %>%
  subset(!is.na(amount)) %>%
  group_by(number) %>%
  summarize("total"=sum(amount), "N"=n()) %>%
  ggplot(aes(N, total)) +
  theme_light() +
  geom_point(col="cadetblue") +
  geom_text(aes(label=ifelse(total>5e+07 | N>20000,as.character(number),'')),hjust=-1.25,vjust=0) 
  


summary(glm(data=X4, outcomes ~ prop.contribs.support , family = "binomial"))

popbio::logi.hist.plot(X4$prop.contribs.support,X4$outcomes,boxp=T,type="hist",col="cadetblue",rug = T, logi.mod = 1)

