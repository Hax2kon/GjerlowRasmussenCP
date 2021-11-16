

library(lmtest);library(sandwich);library(tidyverse);library(broom);library(tidyr);library(sensemakr);library(corrgram)

#####   \\    --      Preparation   --    //    #####

load("data.rdata")

#Remove the socialists:
subdata <- subset(df, party=="h_fv" | party=="v_a")


#####     \\    --    Table 3    --    //    #####
district_events_voteP  <- lm(ReformYes_Postpone  ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
district_events_voteB  <- lm(ReformYes_A_vi      ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
district_events_voteC  <- lm(ReformYes_A_ii      ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
district_events_voteD  <- lm(ReformYes_B_i       ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
district_events_voteE  <- lm(ReformYes_A_iv      ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
district_events_sesP   <- vcovHC(district_events_voteP, type="HC3")
district_events_sesB   <- vcovHC(district_events_voteB, type="HC3")
district_events_sesC   <- vcovHC(district_events_voteC, type="HC3")
district_events_sesD   <- vcovHC(district_events_voteD, type="HC3")
district_events_sesE   <- vcovHC(district_events_voteE, type="HC3")

# Model 1
tidy(coeftest(district_events_voteP, district_events_sesP))

# Model 2
tidy(coeftest(district_events_voteB, district_events_sesB))

# Model 3
tidy(coeftest(district_events_voteC, district_events_sesC))

# Model 4
tidy(coeftest(district_events_voteD, district_events_sesD))

# Model 5
tidy(coeftest(district_events_voteE, district_events_sesE))



#####     \\    --    Figure 3   --    //     #####
plot(0,0,
     xlim=c(1, 5), ylim=c(-0.6, 0.6),
     type="n",
     ylab="", xlab="", 
     bty="n", axes=FALSE)
abline(h=0, lty="dashed")
abline(h=seq(-0.6, 0.6, 0.05), lty="dashed", col=grey(0.2, 0.2))
text(x=1:5, y=-0.7, rev(c("Vote E", "Vote D", "Vote C", "Vote B", "Postpone")), xpd=TRUE, srt=45, cex=1.5)
axis(2, at=seq(-0.8, 0.7, 0.1), col="white", cex.axis=1.5, las=2)
points(c(1:5),
       c(coef(district_events_voteP)[2], 
         coef(district_events_voteB)[2], 
         coef(district_events_voteC)[2], 
         coef(district_events_voteD)[2], 
         coef(district_events_voteE)[2]),
       pch=c(rep(16, 5), rep(17, 5)), cex=1.5)
lines(c(1, 1), confint(coeftest(district_events_voteP, district_events_sesP))[2,], lwd=2)
lines(c(2, 2), confint(coeftest(district_events_voteB, district_events_sesB))[2,], lwd=2)
lines(c(3, 3), confint(coeftest(district_events_voteC, district_events_sesC))[2,], lwd=2)
lines(c(4, 4), confint(coeftest(district_events_voteD, district_events_sesD))[2,], lwd=2)
lines(c(5, 5), confint(coeftest(district_events_voteE, district_events_sesE))[2,], lwd=2)
abline(v=1.5:4.5, lty="dashed", col=gray(0.2,0.5))


#####     \\    --    Table B1    --    //      #####
descvars <- c("ReformYes_Postpone",
              "ReformYes_A_vi",
              "ReformYes_A_ii",
              "ReformYes_B_i",
              "ReformYes_A_iv",
              "sum_district_events_since_election_standardized",
              "district_occupInduPct_standardized",
              "town",
              "Conservative",
              "SocialistVS",
              "temperancePct_standardized",
              "medlemmer_pc_standardized",
              "soldier_wc")

TableB1 <- round(do.call("rbind",lapply(descvars, function(x) c(min(subdata[,x]), median(subdata[,x]), mean(subdata[,x]), max(subdata[,x])))), 2)
colnames(TableB1) <- c("Min", "Median", "Mean", "Max")
TableB1

#####   \\    --    Figure B1    --    //    #####
correlations <- round(cor(subdata[,c(descvars)]), 2)
colnames(correlations) <- c("Postpone", "Alternative B", "Alternative C", "Alternative D", "Alternative E",
                            "Conflict\nevents",
                            "Pct. occupied\nin industry",
                            "Town",
                            "Conservative MP",
                            "Socialist\nvote share\n1918",
                            "Pct.\ntemperance\nmovement",
                            "Union members\nper capita",
                            "Council\npresence")
rownames(correlations) <- c("Postpone", "Alternative B", "Alternative C", "Alternative D", "Alternative E",
                            "Conflict\nevents",
                            "Pct. occupied\nin industry",
                            "Town",
                            "Conservative MP",
                            "Socialist\nvote share\n1918",
                            "Pct.\ntemperance\nmovement",
                            "Union members\nper capita",
                            "Council\npresence")


corrgram(correlations, type="cor",
         upper.panel = NULL, lower.panel = "panel.cor",
         col.regions = colorRampPalette(c(grey(0), grey(0.5), grey(0))))


####    \\    --    Table C1    --   //    #####
events_voteE                 <- lm(ReformYes_A_iv     ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
seriatim_voteE_occup         <- lm(ReformYes_A_iv     ~ sum_district_events_since_election_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
seriatim_voteE_town          <- lm(ReformYes_A_iv     ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + Conservative + SocialistVS + fylke, data=subdata)
seriatim_voteE_conservative  <- lm(ReformYes_A_iv     ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + SocialistVS + fylke, data=subdata)
seriatim_voteE_socialist     <- lm(ReformYes_A_iv     ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + fylke, data=subdata)

events_sesE                     <- vcovHC(events_voteE, type="HC3")
seriatim_voteE_occup_ses        <- vcovHC(seriatim_voteE_occup, type="HC3")
seriatim_voteE_town_ses         <- vcovHC(seriatim_voteE_town, type="HC3")
seriatim_voteE_conservative_ses <- vcovHC(seriatim_voteE_conservative, type="HC3")
seriatim_voteE_socialist_ses    <- vcovHC(seriatim_voteE_socialist, type="HC3")

# Model 1
tidy(coeftest(events_voteE, events_sesE))

# Model 2
tidy(coeftest(seriatim_voteE_occup, seriatim_voteE_occup_ses))

# Model 3
tidy(coeftest(seriatim_voteE_town, seriatim_voteE_town_ses))

# Model 4
tidy(coeftest(seriatim_voteE_conservative, seriatim_voteE_conservative_ses))

# Model 5
tidy(coeftest(seriatim_voteE_socialist, seriatim_voteE_socialist_ses))



####    \\    --    Table C2    --   //    #####
largemod_voteP  <- lm(ReformYes_Postpone ~ sum_district_events_since_election_standardized + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_voteB  <- lm(ReformYes_A_vi     ~ sum_district_events_since_election_standardized + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_voteC  <- lm(ReformYes_A_ii     ~ sum_district_events_since_election_standardized + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_voteD  <- lm(ReformYes_B_i      ~ sum_district_events_since_election_standardized + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_voteE  <- lm(ReformYes_A_iv     ~ sum_district_events_since_election_standardized + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)

largemod_sesP   <- vcovHC(largemod_voteP, type="HC3")
largemod_sesB   <- vcovHC(largemod_voteB, type="HC3")
largemod_sesC   <- vcovHC(largemod_voteC, type="HC3")
largemod_sesD   <- vcovHC(largemod_voteD, type="HC3")
largemod_sesE   <- vcovHC(largemod_voteE, type="HC3")

# Model 1
tidy(coeftest(largemod_voteP, largemod_sesP))

# Model 2
tidy(coeftest(largemod_voteB, largemod_sesB))

# Model 3
tidy(coeftest(largemod_voteC, largemod_sesC))

# Model 4
tidy(coeftest(largemod_voteD, largemod_sesD))

# Model 5
tidy(coeftest(largemod_voteE, largemod_sesE))



#####     \\    --    Jackknife Alternative E   --    //      #####
modelfunction <- function(data){
  mod  <- lm(ReformYes_A_iv      ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=data)
  mod <- coef(mod)[2]
  return(mod)
}

#####     \\    Figure C1   //    #####
res <- lapply(1:nrow(subdata), function(x) modelfunction(subdata[!1:nrow(subdata) %in% x, ] ))
hist(unlist(res), xlim=c(-0.05, 0.4), bty="n", main="", xlab="", ylab="", axes=FALSE)
grid()
hist(unlist(res), add=TRUE, col="white")
abline(v=0, lty="dashed")
axis(1, seq(-0.05, 0.4, 0.05), col="white")
axis(2, seq(0, 1000, 20), las=2)

#####     \\    Figure C2   //    #####
res_fylke <- lapply(unique(subdata$fylke), function(x) modelfunction(subdata[which(subdata$fylke!=x), ] ))
hist(unlist(res_fylke), xlim=c(-0.05, 0.4), bty="n", main="", xlab="", ylab="", axes=FALSE)
grid()
hist(unlist(res_fylke), add=TRUE, col="white")
abline(v=0, lty="dashed")
axis(1, seq(-0.05, 0.4, 0.05), col="white")
axis(2, seq(0, 20, 1), las=2)



#####     \\    --    Jackknife Postpone   --    //     #####

modelfunction <- function(data){
  mod  <- lm(ReformYes_Postpone      ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + town + Conservative + SocialistVS + fylke, data=data)
  mod <- coef(mod)[2]
  return(mod)
}

#####     \\    Figure C3   //    #####
res <- lapply(1:nrow(subdata), function(x) modelfunction(subdata[!1:nrow(subdata) %in% x, ] ))
hist(unlist(res), xlim=c(-0.4, 0.05), bty="n", main="", xlab="", ylab="", axes=FALSE)
grid()
hist(unlist(res), add=TRUE, col="white")
abline(v=0, lty="dashed")
axis(1, seq(-0.4, 0.05, 0.05), col="white")
axis(2, seq(0, 1000, 20), las=2)

#####     \\    Figure C4   //    #####
res_fylke <- lapply(unique(subdata$fylke), function(x) modelfunction(subdata[which(subdata$fylke!=x), ] ))
hist(unlist(res_fylke), xlim=c(-0.4, 0.05), bty="n", main="", xlab="", ylab="", axes=FALSE)
grid()
hist(unlist(res_fylke), add=TRUE, col="white")
abline(v=0, lty="dashed")
axis(1, seq(-0.4, 0.05, 0.05), col="white")
axis(2, seq(0, 20, 1), las=2)



#####   \\    --    Sensitivity analysis    --    //    #####
summary( sensemakr(estimate = coef(district_events_voteE)[2],
          se = sqrt(diag(district_events_sesE))[2],
          dof = district_events_voteE$df.residual ) )

summary( sensemakr(estimate = coef(district_events_voteP)[2],
                   se = sqrt(diag(district_events_sesP))[2],
                   dof = district_events_voteP$df.residual ) )

#####     \\    --    Figure C5   --    //      #####
predict_full       <- lm(ReformYes_A_iv      ~ sum_district_events_since_election_standardized + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
predict_conflict   <- lm(ReformYes_A_iv      ~ district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
predict_council    <- lm(ReformYes_A_iv      ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
predict_CFS        <- lm(ReformYes_A_iv      ~ PartyElitePre + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)



subdata$predict_fullmodel <- predict(predict_full)>=0.5
subdata$predict_conflict  <- predict(predict_conflict)>=0.5
subdata$predict_council   <- predict(predict_council)>=0.5
subdata$predict_cfs       <- predict(predict_CFS)>=0.5

predictcomparison <- matrix(NA, nrow=5, ncol=5)
predictcomparison[2:5, 2] <- as.vector(table(subdata$ReformYes_A_iv, subdata$predict_fullmodel))[c(1,4,2,3)] # 20 Errors
predictcomparison[2:5, 3] <- as.vector(table(subdata$ReformYes_A_iv, subdata$predict_conflict))[c(1,4,2,3)] # 20 Errors
predictcomparison[2:5, 4] <- as.vector(table(subdata$ReformYes_A_iv, subdata$predict_council))[c(1,4,2,3)] # 20 Errors
predictcomparison[2:5, 5] <- as.vector(table(subdata$ReformYes_A_iv, subdata$predict_cfs))[c(1,4,2,3)] # 23 Errors
predictcomparison[2:5, 1] <- c("True negatives", "True positives", "False negatives", "False positives")
predictcomparison[1, 2:5] <- c("Full model", "Excluding industrial conflict",
                               "Using councils",
                               "Using Party elite")

predictfig <- data.frame(predictcomparison[2:5, 2:5], stringsAsFactors = FALSE)
for(i in 1:ncol(predictfig)){
  predictfig[,i] <- as.numeric(predictfig[,i])
}
predictfig <- colSums(predictfig[1:2, ])
predictfig <- predictfig[c(1, 3:4)] - predictfig[2]

names(predictfig) <- c("Industrial conflict",
                       "Soldiers' and workers' councils",
                       "Party elite")


#####   Figure C5
barplot(predictfig, yaxt="n", ylab="Difference in correctly predicted cases compared to core model")
axis(2, seq(-2, 10, 1), col="white", las=2)




####      \\    --    Table D1    --    //      #####
councilmod_voteP  <- lm(ReformYes_Postpone ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
councilmod_voteB  <- lm(ReformYes_A_vi     ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
councilmod_voteC  <- lm(ReformYes_A_ii     ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
councilmod_voteD  <- lm(ReformYes_B_i      ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
councilmod_voteE  <- lm(ReformYes_A_iv     ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
councilmod_sesP   <- vcovHC(councilmod_voteP, type="HC3")
councilmod_sesB   <- vcovHC(councilmod_voteB, type="HC3")
councilmod_sesC   <- vcovHC(councilmod_voteC, type="HC3")
councilmod_sesD   <- vcovHC(councilmod_voteD, type="HC3")
councilmod_sesE   <- vcovHC(councilmod_voteE, type="HC3")

# Model 1
tidy(coeftest(councilmod_voteP, councilmod_sesP))

#Model 2
tidy(coeftest(councilmod_voteB, councilmod_sesB))

# Model 3
tidy(coeftest(councilmod_voteC, councilmod_sesC))

# Model 4
tidy(coeftest(councilmod_voteD, councilmod_sesD))

# Model 5
tidy(coeftest(councilmod_voteE, councilmod_sesE))


#####     \\    --    Figure D1   --    //      #####
plot(1:5, rep(0, 5), type="n",
     ylim=c(-1.2, 1), ylab="", xlab="",
     bty="n", axes=FALSE)
abline(v=seq(1.5, 4.5, 1), lty=3, col=grey(0.5,0.5))
abline(h=seq(-1, 1, 0.5), lty=3, col=grey(0.5,0.5))
abline(h=0)
options(scipen=10)
text(x=1:5, y=-1.2, rev(c("Vote E", "Vote D", "Vote C", "Vote B", "Postpone")),
     xpd=TRUE, srt=45, cex=1.5)
axis(2, seq(from=-1, to=1, by=0.2), las=2, col="white")

points(1, coef(councilmod_voteP)[2], pch=16, cex=1.5)
points(2, coef(councilmod_voteB)[2], pch=16, cex=1.5)
points(3, coef(councilmod_voteC)[2], pch=16, cex=1.5)
points(4, coef(councilmod_voteD)[2], pch=16, cex=1.5)
points(5, coef(councilmod_voteE)[2], pch=16, cex=1.5)

lines(rep(1, 2), confint(coeftest(councilmod_voteP, councilmod_sesP))[2, ])
lines(rep(2, 2), confint(coeftest(councilmod_voteB, councilmod_sesB))[2, ])
lines(rep(3, 2), confint(coeftest(councilmod_voteC, councilmod_sesC))[2, ])
lines(rep(4, 2), confint(coeftest(councilmod_voteD, councilmod_sesD))[2, ])
lines(rep(5, 2), confint(coeftest(councilmod_voteE, councilmod_sesE))[2, ])


#####     \\    --    Table D2   --    //      #####
council_voteE                        <- lm(ReformYes_A_iv     ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + SocialistVS + fylke, data=subdata)
council_seriatim_voteE_occup         <- lm(ReformYes_A_iv     ~ soldier_wc + town + Conservative + SocialistVS + fylke, data=subdata)
council_seriatim_voteE_town          <- lm(ReformYes_A_iv     ~ soldier_wc + district_occupInduPct_standardized + Conservative + SocialistVS + fylke, data=subdata)
council_seriatim_voteE_conservative  <- lm(ReformYes_A_iv     ~ soldier_wc + district_occupInduPct_standardized + town + SocialistVS + fylke, data=subdata)
council_seriatim_voteE_socialist     <- lm(ReformYes_A_iv     ~ soldier_wc + district_occupInduPct_standardized + town + Conservative + fylke, data=subdata)

council_sesE                            <- vcovHC(council_voteE, type="HC3")
council_seriatim_voteE_occup_ses        <- vcovHC(council_seriatim_voteE_occup, type="HC3")
council_seriatim_voteE_town_ses         <- vcovHC(council_seriatim_voteE_town, type="HC3")
council_seriatim_voteE_conservative_ses <- vcovHC(council_seriatim_voteE_conservative, type="HC3")
council_seriatim_voteE_socialist_ses    <- vcovHC(council_seriatim_voteE_socialist, type="HC3")

# Model 1
tidy(coeftest(council_voteE, council_sesE))

# Model 2
tidy(coeftest(council_seriatim_voteE_occup, council_seriatim_voteE_occup_ses))

# Model 3
tidy(coeftest(council_seriatim_voteE_town, council_seriatim_voteE_town_ses))

# Model 4
tidy(coeftest(council_seriatim_voteE_conservative, council_seriatim_voteE_conservative_ses))

# Model 5
tidy(coeftest(council_seriatim_voteE_socialist, council_seriatim_voteE_socialist_ses))



####      \\    --    Table D3    --    //      #####
largemod_council_voteP  <- lm(ReformYes_Postpone ~ soldier_wc + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_council_voteB  <- lm(ReformYes_A_vi     ~ soldier_wc + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_council_voteC  <- lm(ReformYes_A_ii     ~ soldier_wc + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_council_voteD  <- lm(ReformYes_B_i      ~ soldier_wc + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)
largemod_council_voteE  <- lm(ReformYes_A_iv     ~ soldier_wc + temperancePct_standardized + medlemmer_pc_standardized + district_occupInduPct_standardized + town + PartyElitePre + Conservative + SocialistVS + fylke, data=subdata)

largemod_council_sesP   <- vcovHC(largemod_council_voteP, type="HC3")
largemod_council_sesB   <- vcovHC(largemod_council_voteB, type="HC3")
largemod_council_sesC   <- vcovHC(largemod_council_voteC, type="HC3")
largemod_council_sesD   <- vcovHC(largemod_council_voteD, type="HC3")
largemod_council_sesE   <- vcovHC(largemod_council_voteE, type="HC3")

# Model 1
tidy(coeftest(largemod_council_voteP, largemod_council_sesP))

# Model 2
tidy(coeftest(largemod_council_voteB, largemod_council_sesB))

# Model 3
tidy(coeftest(largemod_council_voteC, largemod_council_sesC))

# Model 4
tidy(coeftest(largemod_council_voteD, largemod_council_sesD))

# Model 5
tidy(coeftest(largemod_council_voteE, largemod_council_sesE))
