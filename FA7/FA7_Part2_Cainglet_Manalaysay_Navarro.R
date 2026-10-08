#Demographics
Prog <- Daily_Screen_Time$Program
table(Prog)

YL <- Daily_Screen_Time$`Year Level`
table(YL)

AR <- Daily_Screen_Time$`Age Range`
table(AR)

#1. Frequency Distribution Tables
FB <- Daily_Screen_Time$Facebook
table(FB)

IG <- Daily_Screen_Time$Instagram
table(IG)

TT <- Daily_Screen_Time$Tiktok
table(TT)

Tot <- Daily_Screen_Time$Total
table(Tot)

#2. Mean & Standard Deviations

#Facebook
mean_FB <- mean(FB)
sd_FB <- sd(FB)
var_FB <- var(FB)

#Instagram
mean_IG <- mean(IG)
sd_IG <- sd(IG)
var_IG <- var(IG)

#Tiktok
mean_TT <- mean(TT)
sd_TT <- sd(TT)
var_TT <- var(TT)

#Total
mean_Tot <- mean(Tot)
sd_Tot <- sd(Tot)
var_Tot <- var(Tot)

#3. Normal Curves

#Facebook
xlim_FB <- c(min(FB), max(FB))
curve(dnorm(x, mean_FB, sd_FB), xlim = xlim_FB, xlab = "Hours of Screentime on Facebook a Day", ylab = "f(x)")

#Instagram
xlim_IG <- c(min(IG), max(IG))
curve(dnorm(x, mean_IG, sd_IG), xlim = xlim_IG, xlab = "Hours of Screentime on Instagram a Day", ylab = "f(x)")

#Tiktok
xlim_TT <- c(min(TT), max(TT))
curve(dnorm(x, mean_TT, sd_TT), xlim = xlim_TT, xlab = "Hours of Screentime on Tiktok a Day", ylab = "f(x)")

#Total Daily Screentime
xlim_Tot <- c(min(Tot), max(Tot))
curve(dnorm(x, mean_Tot, sd_Tot), xlim = xlim_Tot, xlab = "Total Hours of Screentime a Day", ylab = "f(x)")

#4. Percentage of data that falls within 1σ, 2σ, and 3σ from the mean

#Facebook
FB_R_ind_1 <- mean(FB) + sd(FB)
FB_R_ind_2 <- mean(FB) + sd(FB) * 2
FB_R_ind_3 <- mean(FB) + sd(FB) * 3

#Theoretical
FB_1SD <- pnorm(FB_R_ind_1, mean(FB), sd(FB)) - pnorm(0, mean(FB), sd(FB))
FB_2SD <- pnorm(FB_R_ind_2, mean(FB), sd(FB)) - pnorm(0, mean(FB), sd(FB))
FB_3SD <- pnorm(FB_R_ind_3, mean(FB), sd(FB)) - pnorm(0, mean(FB), sd(FB))

#Actual
sum(FB <= FB_R_ind_1) / 50
sum(FB <= FB_R_ind_2) / 50
sum(FB <= FB_R_ind_3) / 50

#Instagram
IG_R_ind_1 <- mean(IG) + sd(IG)
IG_R_ind_2 <- mean(IG) + sd(IG) * 2
IG_R_ind_3 <- mean(IG) + sd(IG) * 3

#Theoretical
IG_1SD <- pnorm(IG_R_ind_1, mean(IG), sd(IG)) - pnorm(0, mean(IG), sd(IG))
IG_2SD <- pnorm(IG_R_ind_2, mean(IG), sd(IG)) - pnorm(0, mean(IG), sd(IG))
IG_3SD <- pnorm(IG_R_ind_3, mean(IG), sd(IG)) - pnorm(0, mean(IG), sd(IG))

#Actual
sum(IG <= IG_R_ind_1) / 50
sum(IG <= IG_R_ind_2) / 50
sum(IG <= IG_R_ind_3) / 50




#Tiktok
TT_R_ind_1 <- mean(TT) + sd(TT)
TT_R_ind_2 <- mean(TT) + sd(TT) * 2
TT_R_ind_3 <- mean(TT) + sd(TT) * 3

#Theoretical
TT_1SD <- pnorm(TT_R_ind_1, mean(TT), sd(TT)) - pnorm(0, mean(TT), sd(TT))
TT_2SD <- pnorm(TT_R_ind_2, mean(TT), sd(TT)) - pnorm(0, mean(TT), sd(TT))
TT_3SD <- pnorm(TT_R_ind_3, mean(TT), sd(TT)) - pnorm(0, mean(TT), sd(TT))

#Actual
sum(TT <= TT_R_ind_1) / 50
sum(TT <= TT_R_ind_2) / 50
sum(TT <= TT_R_ind_3) / 50

#Total
Tot_L_ind_1 <- mean(Tot) - sd(Tot)
Tot_R_ind_1 <- mean(Tot) + sd(Tot)
Tot_R_ind_2 <- mean(Tot) + sd(Tot) * 2
Tot_R_ind_3 <- mean(Tot) + sd(Tot) * 3

#Theoretical
Tot_1SD <- pnorm(Tot_R_ind_1, mean(Tot), sd(Tot)) - pnorm(Tot_L_ind_1, mean(Tot), sd(Tot))
Tot_2SD <- pnorm(Tot_R_ind_2, mean(Tot), sd(Tot)) - pnorm(0, mean(Tot), sd(Tot))
Tot_3SD <- pnorm(Tot_R_ind_3, mean(Tot), sd(Tot)) - pnorm(0, mean(Tot), sd(Tot))

#Actual
sum(Tot_L_ind_1 <= Tot <= Tot_R_ind_1) / 50
sum(Tot <= Tot_R_ind_2) / 50
sum(Tot <= Tot_R_ind_3) / 50
