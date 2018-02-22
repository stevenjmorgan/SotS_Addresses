### Steven Morgan
### Poisson-based models predicting the number of anti-LGBTQ bills proposed
### in each state-year (2013-2017) as a function of religious signals by
### governors and appropriate institutional and public opinion controls.



# Read in religious langauge data from spreadsheet created by Python script
gov <- read.csv('gov.csv', header = T)

# Read in data collected from ACLU reports of anti-LGBTQ bills in state 
# legislatures
bills <- read.csv('mf_bills.csv', header = T)


# Remove all governor religious speech data prior to 2013 (this is when data
# collected from ACLU reports begins)
gov <- gov[which(gov$Year > 2012),]

# Merge proposed bill data and religious language data
LGBT <- base::merge(gov, bills, by = c('State', 'Year'))
rm(gov, bills)

# Read in data on governor characteristics from the Civil Service USA dataset
URL <- 'https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/source/us-governors.csv'
civil <- read.csv(URL, header = T)
rm(URL)

# Rename columns and merge governor characteristics data w/ newly merged data
names(civil)[names(civil) == "last_name"] <- 'Governor'
names(civil)[names(civil) == "state_code"] <- 'State'
LGBT <- merge(LGBT, civil[,c('State','party','Governor', 'gender', 'ethnicity', 
                             'religion')], 
              by = c('State', 'Governor'),
              all.x = TRUE)
rm(civil)

write.csv(LGBT, 'merged_gov.csv')

# Hand code partisanship of governors not currently in office and thus not coded
# by the Civil Service USA data set
LGBT <- read.csv('merged_gov_edit.csv', header = T)


# Read in The Correlates of State Policy Project dataset from the INSTITUTE 
# FOR PUBLIC POLICY AND SOCIAL RESEARCH (IPPSR) at Michigan State University
# http://ippsr.msu.edu/public-policy/correlates-state-policy
# (saved locally for the purposes of this script)
policy <- read.csv("correlatesofstatepolicyproject.csv", 
                   stringsAsFactors = FALSE)

# Subset data for desired variables
policy <- policy[,c('state', 'year', 'odd_even_year', 'evangelical_pop', 
                    'legprofscore', 'inst6014_nom', 'citi6013')]

# Subset data for time period 2013-2017
policy <- policy[which(policy$year >= 2013 & policy$year <= 2017),]
# Remove District of Columbia from dataset for all years
policy <- policy[which(policy$st != 'DC'),]
# Rename columns names and merge w/ LGBT dataset
names(policy)[names(policy) == "state"] <- 'State.Name'
names(policy)[names(policy) == "year"] <- 'Year'
policy$State.Name <- as.character(policy$State.Name)
LGBT <- LGBT[,-c(1)]
LGBT$State.Name <- as.character(LGBT$State.Name)

# Returns string w/o trailing whitespace
trim.trailing <- function(x) sub("\\s+$", "", x)
LGBT$State.Name <- trim.trailing(LGBT$State.Name)

LGBT <- merge(LGBT, policy, 
              by = c('State.Name', 'Year'),
              all.x = TRUE)

# Recode governor party
library(car)
LGBT$gov.GOP <- recode(LGBT$party, "'republican' = 1; else = 0")
LGBT$gov.GOP <- as.numeric(as.character(LGBT$gov.GOP))
LGBT$gov.dem <- recode(LGBT$party, "'democrat' = 1; else = 0")
LGBT$gov.dem <- as.numeric(as.character(LGBT$gov.dem))

rm(policy)

#library(foreign)
#census2010 <- read.dta("census2010.dta", convert.underscore = TRUE)



### Descriptive statistics ###
library(ggplot2)

# Histogram of # of anti-LGBTQ bills proposed 2013-2017
a <- ggplot(LGBT, aes(Anti.Total))
a <- a + geom_histogram(binwidth = 1, 
                    color = 'blue', 
                    fill = 'red')
a + labs(x = "Anti-LGBTQ Bills Proposed", y = 'Count') # State-Years

# Histogram of religious language used by governors in state-of-the-state 
# addresses 2013-2017
a <- ggplot(LGBT, aes(total_rel))
a <- a + geom_histogram(binwidth = 1, 
                        color = 'purple', 
                        fill = 'purple')
a + labs(x = "Religious Unigrams", y = '') # State-of-the-State Addresses

# Histogram of religious language normalized by total number of Porter-stemmed
# non-stop words
a <- ggplot(LGBT, aes(ratio))
a <- a + geom_density(kernel = 'gaussian', 
                      color = 'purple', 
                      fill = 'purple')
a + labs(x = "Normalized Religious Language", y = '') # State-of-the-State Addresses

# Histogram of net sentiment normalized by total number of Porter-stemmed
# non-stop words
a <- ggplot(LGBT, aes(net_sent))
a <- a + geom_density(kernel = 'gaussian', 
                        color = 'green', 
                        fill = 'green')
a + labs(x = "Normalized Net Sentiment", y = '') # State-of-the-State Addresses
hist(LGBT$net_sent)

#ggplot(LGBT, aes(total_rel, Anti.Total)) + geom_point()

### Modeling number of Anti-LGBTQ bills proposed in state legislatures as a
### function of religious langauge in S.o.S. Addresses, % evangelical Prot. and
### Mormons, legislature professionalism (Squire Index), state government 
### ideology, governor partisanship, and conditional effect of % evangelical + 
### Mormon on legislature professionalism w/ Count Models
### Run second set of models substituting normalized religious language for
### "raw" religious language count

# Poisson Model
anti.pois <- glm(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
                     inst6014_nom + gov.dem + citi6013, 
                 data = LGBT, family = 'poisson')
summary(anti.pois)

ratio.pois <- glm(Anti.Total ~ ratio + evangelical_pop + legprofscore + 
                     inst6014_nom + gov.dem + citi6013, 
                 data = LGBT, family = 'poisson')
summary(ratio.pois)
stargazer(anti.pois, ratio.pois)

# Incidence Rate Ratios
library(mfx)
anti.pois.IRR <- poissonirr(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
                                inst6014_nom + gov.dem + citi6013, 
                            data = LGBT)
anti.pois.IRR

ratio.pois.IRR <- poissonirr(Anti.Total ~ ratio + evangelical_pop + legprofscore 
                                + inst6014_nom + gov.dem + citi6013, 
                            data = LGBT)
ratio.pois.IRR


# In-Sample Predicted Probabilities
LGBT$phat <- predict(anti.pois, type = "response")
ggplot(LGBT, aes(x = total_rel, y = phat)) +
    geom_point(aes(y = Anti.Total), alpha = .5) +
    geom_line(size = 1) +
    labs(x = "Math Score", y = "Expected number of awards")

# Out-of-Sample Predicted Probabilities
total_rel <- seq(0, 25, 1)
evangelical_pop <- mean(LGBT$evangelical_pop)
legprofscore <- mean(LGBT$legprofscore)
inst6014_nom <- mean(LGBT$inst6014_nom)
citi6013 <- mean(LGBT$citi6013)
#net_sent <- mean(LGBT$net_sent)
gov.dem <- median(as.numeric(as.character((LGBT$gov.dem))))
simdata <- as.data.frame(cbind(total_rel, evangelical_pop, legprofscore,
                               inst6014_nom, gov.dem, citi6013))
nullhats <- predict(anti.pois, newdata = simdata, se.fit = TRUE)

# NOTE: These are XBs, not predicted counts.
# Transforming:
nullhats$Yhat <- exp(nullhats$fit)
nullhats$UB <- exp(nullhats$fit + 1.96 * (nullhats$se.fit))
nullhats$LB <- exp(nullhats$fit - 1.96 * (nullhats$se.fit))
plot(simdata$total_rel, nullhats$Yhat, t = "l", lwd = 3, ylim = c(0,4), ylab =
         "Predicted Count", xlab = "# of Religious Unigrams")
lines(simdata$total_rel, nullhats$UB, lwd = 2, lty = 2)
lines(simdata$total_rel, nullhats$LB, lwd = 2, lty = 2)

# Nromalized Rel. Language Pred. Probs
ratio <- seq(0, 0.15, length.out = 25)
simdata.ratio <- as.data.frame(cbind(ratio, evangelical_pop, legprofscore,
                                     inst6014_nom, gov.dem, citi6013))
nullhats.ratio <- predict(ratio.pois, newdata = simdata.ratio, se.fit = TRUE)
nullhats.ratio$Yhat <- exp(nullhats.ratio$fit)
nullhats.ratio$UB <- exp(nullhats.ratio$fit + 1.96 * (nullhats.ratio$se.fit))
nullhats.ratio$LB <- exp(nullhats.ratio$fit - 1.96 * (nullhats.ratio$se.fit))
plot(simdata.ratio$ratio, nullhats.ratio$Yhat, t = "l", lwd = 3, 
     ylab = "Predicted Count", xlab = "Normalized Religious Lanugage")
lines(simdata.ratio$ratio, nullhats.ratio$UB, lwd = 2, lty = 2)
lines(simdata.ratio$ratio, nullhats.ratio$LB, lwd = 2, lty = 2)


## Test for overdispersion
mean(LGBT$Anti.Total)
sqrt(var(LGBT$Anti.Total)) #Dependent variable appears slightly overdispersed

# Test for overdispersion "by hand"
# Calculate error terms from poisson model
y.fitted <- anti.pois$fitted.values
uhats <- ((LGBT$Anti.Total - anti.pois$fitted.values)^2 - LGBT$Anti.Total) / 
    (anti.pois$fitted.values * sqrt(2))

# Regress error terms from poisson model on predicted counts from poisson model
over.test <- lm(uhats ~ anti.pois$fitted.values)
summary(over.test) # p-value: 0.07

y.fitted.r <- ratio.pois$fitted.values
uhats.r <- ((LGBT$Anti.Total - ratio.pois$fitted.values)^2 - LGBT$Anti.Total) / 
    (ratio.pois$fitted.values * sqrt(2))
over.test.ratio <- lm(uhats.r ~ ratio.pois$fitted.values)
summary(over.test.ratio) # p-value: 0.01


## Negative Binomial Regression
anti.NB <- glm.nb(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
                      inst6014_nom + gov.dem + citi6013, 
                  data = LGBT)
summary(anti.NB)
1 / anti.NB$theta

anti.NB.rat <- glm.nb(Anti.Total ~ ratio + evangelical_pop + legprofscore + 
                      inst6014_nom + gov.dem + citi6013, 
                  data = LGBT)
summary(anti.NB.rat)
1 / anti.NB.rat$theta
stargazer(anti.NB, anti.NB.rat)

plot(anti.pois$fitted.values, anti.NB$fitted.values,xlab = "Poisson",
     ylab = "Negative Binomial", main = "Predicted Counts", ylim = c(1,5))
abline(a = 0, b = 1, lwd = 2)

anti.NB.r <- glm.nb(Anti.Total ~ ratio + evangelical_pop + legprofscore + 
                      inst6014_nom + gov.dem + citi6013, 
                  data = LGBT)
summary(anti.NB.r)
1 / anti.NB.r$theta

plot(ratio.pois$fitted.values, anti.NB.r$fitted.values,xlab = "Poisson",
     ylab = "Negative Binomial", main = "Predicted Counts", ylim = c(1,5))
abline(a = 0, b = 1, lwd = 2)


### Zero-Inflated Models
## Zero-Inflated Poisson Model
library(pscl)
anti.ZIP <- zeroinfl(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
                         inst6014_nom + gov.dem + citi6013, 
                     data = LGBT,
                     dist = 'poisson',
                     link = 'logit')
summary(anti.ZIP)

anti.ZIP.rat <- zeroinfl(Anti.Total ~ ratio + evangelical_pop + legprofscore + 
                         inst6014_nom + gov.dem + citi6013, 
                     data = LGBT,
                     dist = 'poisson',
                     link = 'logit')
summary(anti.ZIP.rat)



## Zero-Inflated Negative Binomial
anti.ZINB <- zeroinfl(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
                         inst6014_nom + gov.dem + citi6013, 
                     data = LGBT,
                     dist = 'negbin',
                     link = 'logit')
summary(anti.ZINB)

anti.ZINB.rat <- zeroinfl(Anti.Total ~ ratio + evangelical_pop + legprofscore + 
                          inst6014_nom + gov.dem + citi6013, 
                      data = LGBT,
                      dist = 'negbin',
                      link = 'logit')
summary(anti.ZINB.rat)



## Hurdle Model
anti.hurdle <- hurdle(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
                          inst6014_nom + gov.dem + citi6013, 
                      data = LGBT,
                      dist = c('poisson'),
                      zero.dist = c('poisson'),
                      link = c('log'))
summary(anti.hurdle)

anti.hurdle.rat <- hurdle(Anti.Total ~ ratio + evangelical_pop + legprofscore + 
                          inst6014_nom + gov.dem + citi6013, 
                      data = LGBT,
                      dist = c('poisson'),
                      zero.dist = c('poisson'),
                      link = c('log'))
summary(anti.hurdle.rat)

library(stargazer)
stargazer(anti.ZIP, anti.ZIP.rat, anti.ZINB, anti.ZINB.rat, anti.hurdle, anti.hurdle.rat)


### Account for the fact that data are time-series cross-sectional w/ panel/
### TCSC event count model
## Random-Effects Poisson Model
library(glmmML)

# Subset data since glmmML models do not run w/ missing data
LGBT.glmmML <- LGBT[,c('Anti.Total', 'total_rel', 'evangelical_pop',
                       'legprofscore', 'inst6014_nom', 'gov.dem', 'State.Name',
                       'Governor', 'citi6013', 'ratio')]

anti.RE <- glmmML(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
                      inst6014_nom + gov.dem + citi6013,
                  data = LGBT.glmmML,
                  family = 'poisson',
                  cluster = as.factor(Governor))
summary(anti.RE)

anti.RE.rat <- glmmML(Anti.Total ~ ratio + evangelical_pop + legprofscore + 
                      inst6014_nom + gov.dem + citi6013,
                  data = LGBT.glmmML,
                  family = 'poisson',
                  cluster = as.factor(Governor))
summary(anti.RE.rat)


 summary(glm(Anti.Total ~ total_rel + evangelical_pop + legprofscore + 
            inst6014_nom + gov.dem + (State) + citi6013, 
        data = LGBT, family = 'poisson'))


### Ancillary Descriptive Statistics 
stargazer(LGBT)

us <- map_data("state")
LGBT$region <- tolower(LGBT$State.Name)
LGBT <- LGBT[LGBT$Year == 2017,]
dim(LGBT)

ggplot() + geom_map(data=us, map=us, aes(long, lat, map_id=region), color="#2b2b2b", fill=NA, size=0.15) +
    geom_map(data=LGBT, map=us, aes(fill=evangelical_pop, map_id=region), color="#ffffff", size=0.15) + labs(x=NULL, y=NULL) +
    scale_fill_continuous(low='gray80', high='gray20',  guide='colorbar') +
    theme(panel.border = element_blank(), panel.background = element_blank(), 
          plot.title = element_text(hjust = 0.5,size = 25, face = "bold"), axis.ticks = element_blank(),
          axis.text = element_blank()) + guides(fill = guide_colorbar(title=NULL)) + coord_map() + 
    ggtitle("Percent Evangelical and Mormon: 2017")
