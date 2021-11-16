
# Preperations
library(ggplot2);require(gridExtra);library(lmtest);library(sandwich)
load("election19191921.rdata")



#####     \\    --    Table 4   --    //      #####
DNA_electionimpact    <- lm(DNA_voteshare_1921_standardized    ~ sum_events_since_election_standardized + occupInduPct_standardized + town + Conservative + Socialist_voteshare_1918_standardized + fylke, data=election_subdf)
NSA_electionimpact    <- lm(NSA_voteshare_1921_standardized    ~ sum_events_since_election_standardized + occupInduPct_standardized + town + Conservative + Socialist_voteshare_1918_standardized + fylke, data=election_subdf)
DNANSA_electionimpact <- lm(DNANSA_voteshare_1921_standardized ~ sum_events_since_election_standardized + occupInduPct_standardized + town + Conservative + Socialist_voteshare_1918_standardized + fylke, data=election_subdf)
DNA_electionimpact_ses     <- vcovHC(DNA_electionimpact   , type="HC3")
NSA_electionimpact_ses     <- vcovHC(NSA_electionimpact   , type="HC3")
DNANSA_electionimpact_ses  <- vcovHC(DNANSA_electionimpact, type="HC3")

# Model 1
tidy(coeftest(DNA_electionimpact, DNA_electionimpact_ses))

# Model 2
tidy(coeftest(NSA_electionimpact, NSA_electionimpact_ses))

# Model 3
tidy(coeftest(DNANSA_electionimpact, DNANSA_electionimpact_ses))



#####     \\    --    Table D4   --    //      #####
#Estimates
DNA_electionimpact_council    <- lm(DNA_voteshare_1921_standardized    ~ soldier_wc + occupInduPct_standardized + town + Conservative + Socialist_voteshare_1918_standardized + fylke, data=election_subdf)
NSA_electionimpact_council    <- lm(NSA_voteshare_1921_standardized    ~ soldier_wc + occupInduPct_standardized + town + Conservative + Socialist_voteshare_1918_standardized + fylke, data=election_subdf)
DNANSA_electionimpact_council <- lm(DNANSA_voteshare_1921_standardized ~ soldier_wc + occupInduPct_standardized + town + Conservative + Socialist_voteshare_1918_standardized + fylke, data=election_subdf)

#VCOVs
DNA_electionimpact_council_ses     <- vcovHC(DNA_electionimpact_council   , type="HC3")
NSA_electionimpact_council_ses     <- vcovHC(NSA_electionimpact_council   , type="HC3")
DNANSA_electionimpact_council_ses  <- vcovHC(DNANSA_electionimpact_council, type="HC3")

# Model 1
tidy(coeftest(DNA_electionimpact_council, DNA_electionimpact_council_ses))

# Model 2
tidy(coeftest(NSA_electionimpact_council, NSA_electionimpact_council_ses))

# Model 3
tidy(coeftest(DNANSA_electionimpact_council, DNANSA_electionimpact_council_ses))



#####     \\    --    Figure C6   --    //      #####
basebase <- ggplot(election_subdf, aes(x=sum_events_since_election, y=DNANSA_voteshare_1921)) +
  geom_point() +
  xlab("Conflicts since election") +
  ylab("DNA/NSA vote share 1921") +
  geom_smooth(colour="black", span=1) +
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(size = 0.7, colour = grey(0.5,0.7)), 
        panel.grid.minor = element_line(size = 0.7, colour = grey(0.7,0.7)))

logbase <- ggplot(election_subdf, aes(x=log(sum_events_since_election+1), y=DNANSA_voteshare_1921)) +
  xlab("Ln conflicts since election") +
  ylab("DNA/NSA vote share 1921") +
  geom_point() +
  geom_smooth(colour="black", span=1) +
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(size = 0.7, colour = grey(0.5,0.7)), 
        panel.grid.minor = element_line(size = 0.7, colour = grey(0.7,0.7)))

baselog <- ggplot(election_subdf, aes(x=sum_events_since_election, y=log(DNANSA_voteshare_1921+0.01))) +
  xlab("Conflicts since election") +
  ylab("Ln DNA/NSA vote share 1921") +
  geom_point() +
  geom_smooth(colour="black", span=1) +
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(size = 0.7, colour = grey(0.5,0.7)), 
        panel.grid.minor = element_line(size = 0.7, colour = grey(0.7,0.7)))
loglog <- ggplot(election_subdf, aes(x=log(sum_events_since_election+1), y=log(DNANSA_voteshare_1921+0.01))) +
  xlab("Ln conflicts since election") +
  ylab("Ln DNA/NSA vote share 1921") +
  geom_point() +
  geom_smooth(colour="black", span=1) +
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill='white'),
        panel.grid.major = element_line(size = 0.7, colour = grey(0.5,0.7)), 
        panel.grid.minor = element_line(size = 0.7, colour = grey(0.7,0.7)))

grid.arrange(basebase, logbase,
             baselog, loglog, ncol=2, nrow=2)
g<- arrangeGrob(basebase, logbase,
                baselog, loglog, ncol=2, nrow=2)
