
#Packages:
library(tidyr);library(tidyverse);library(mcp)


#####     \\    --    Filter V-Dem data   --    //      #####
load("Changepoint_data.rdata")

# Order dataset
vdem <- vdem[order(vdem$country_id, vdem$year),]

# Indicate countries without elections
vdem$v2elparlel <- ifelse(vdem$v2xlg_elecreg==0, "dictatorship", vdem$v2elparlel)

# Add electoral system to years in-between elections
vdem <- vdem %>% group_by(country_id) %>% fill(v2elparlel)
vdem$elparlel <- ifelse(is.na(vdem$v2elparlel)==TRUE, "dictatorship", vdem$v2elparlel)
vdem$proportional <- ifelse(vdem$elparlel=="1", 1, 0)

# Sample countries existing in 1899, and limit timeline (1899-2001)
countries_in_1899 <- vdem$country_id[which(vdem$year==1899)]
sample <- vdem[vdem$country_id %in% countries_in_1899,]
sample <- sample[which(sample$year>1899),]
sample <- sample[which(sample$year<2001),]

#Make mean proportion of PR among all types of regimes
pr_by_year <- sample %>% group_by(year) %>% summarise("sum_pr" = sum(proportional),
                                                           "countries" = n())



#####     \\    --    Figure 1    --    //      #####
ggplot(pr_by_year, aes(x = year, y = sum_pr)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = 1918, linetype = "dashed") +
  scale_x_continuous(breaks = c(seq(1900, 1910, 10), 1918, seq(1920, 2000, 10))) +
  xlab("") + ylab("Number of countries with PR") +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 1.5, hjust = 1.4),
        axis.title = element_text(size = 16),
        panel.grid.minor = element_blank())



#####     \\    --    Changepoint   --    //    #####
pr_by_year <- na.omit(pr_by_year)


# Define the model
model = list(
  sum_pr ~ 1,  # plateau (int_1)
  ~ 1 + year,    # 
  ~ 1 + year     # 
)
model_many = list(
  sum_pr ~ 1,  # plateau (int_1)
  ~ 1 + year,    # 
  ~ 1 + year,    # 
  ~ 1 + year,    # 
  ~ 1 + year     # 
)

# Fit model
fit = mcp(model, data = pr_by_year, sample = "both", adapt = 20e3,
          iter = 10e3)

# Diagnosis
plot_pars(fit, regex_pars = "cp_1")
plot(fit)
summary(fit)
pp_check(fit)

# Prepare for ggplot
##  Yearly change (Figure B2)
dat <- fitted(fit)
dat <- full_join(dat, pr_by_year, by = "year")

##    Posterior for the change in 1918-1919 (Figure B3)
cp <- data.frame(do.call("rbind", fit[[5]]))
cp$draw <- rep(1:3, each = nrow(cp)/3)


#####     \\    --    Figure B2   --    //    #####
ggplot(dat, aes(x = year)) +
  geom_point(aes(y = sum_pr)) +
  geom_line(aes(y = fitted)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 0.25, show.legend = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 1918, linetype = "dashed") +
  scale_x_continuous(breaks = c(seq(1900, 1910, 10), 1918, seq(1920, 2000, 10))) +
  xlab("") + ylab("Number of countries with PR") +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 1.5, hjust = 1.4),
        axis.title = element_text(size = 16),
        panel.grid.minor = element_blank())

#####     \\    --    Figure B3   --    //    #####
ggplot(cp, aes(x = cp_1, group = draw, fill = draw)) +
  geom_density(alpha = 0.2, show.legend = FALSE) +
  theme_minimal() +
  xlab("") + ylab("Posterior density") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))
