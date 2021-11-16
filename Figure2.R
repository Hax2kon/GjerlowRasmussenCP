
# Preperations
library(dplyr);library(ggplot2)

load("yearly_industrial_conflict.rdata")

#####   \\    --    Figure 2    --    //      #####
ggplot(yearly, aes(x=year, y=year_event)) +
  geom_line(lwd=1) + geom_point(cex=2) +
  xlab("") + ylab("Industrial conflicts") +
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

