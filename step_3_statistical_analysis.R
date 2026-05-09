###### soil respiration
# glm, Gamma distribution
msr <- glmmTMB(Normalized_Rsoil ~ Ctreat * P_addition * VWC + 
                (1|Block/Plot/Collar) + (1|Campaign),
              family = Gamma(link = "log"),
              data = REsoil)
Anova(msr, type = "III")

# calculate slope and P value for each Ctreat
tc <- emtrends(msr, ~ Ctreat, var = "VWC", 
                            weights = "proportional") 
tc # slope
test(tc)  # p value

# calculate slope and P value for each P addition
tp <- emtrends(msr, ~ P_addition, var = "VWC", 
                            weights = "proportional")  
tp # slope
test(tp)  # p value





###### microbial biomass carbon
mmbc <- lmer(sqrt(Cmic) ~ Ctreat * P_addition + (1|Block), data = MBC)
Anova(mmbc, type = "III", test = "F")
### check Homogeneity of Variance and normality
resid <- resid(mmbc)
model_data <- model.frame(mmbc)
model_data$resid <- resid
# Levene's test on residuals grouped by Season × Depth × Treatment
leveneTest(resid ~ interaction(Ctreat, Season, Depth), data = model_data)
# Shapiro-Wilk test on residuals
shapiro.test(resid)






###### fine root production
##### before P addition
### model
mfrpb <- lmer(sqrt(Fine_root_production) ~   Ctreat * Season * Depth + (1|Block), data = FRP_before)
Anova(mfrpb, type = "II", test = "F")
### check Homogeneity of Variance and normality
resid <- resid(mfrpb)
model_data <- model.frame(mfrpb)
model_data$resid <- resid
# Levene's test on residuals grouped by Season × Depth × Treatment
leveneTest(resid ~ interaction(Ctreat, Season, Depth), data = model_data)
# Shapiro-Wilk test on residuals
shapiro.test(resid)

##### after P addition
### model
mfrpa <- lmer(sqrt(Fine_root_production ) ~   Ctreat * Season * Depth + (1|Block), data = FRP_after)
Anova(mfrpa, type = "II", test = "F")
### check Homogeneity of Variance and normality
resid <- resid(mfrpa)
model_data <- model.frame(mfrpa)
model_data$resid <- resid
# Levene's test on residuals grouped by Season × Depth × Treatment
leveneTest(resid ~ interaction(Ctreat, Season, Depth), data = model_data)
# Shapiro-Wilk test on residuals
shapiro.test(resid)

##### interactions include P_addition
mcpsd <- lmer(sqrt(Fine_root_production ) ~   Ctreat * P_addition * Season * Depth + (1|Block), data = FRP)
Anova(mcpsd, type = "II", test = "F")

mcps <- lmer(sqrt(Fine_root_production ) ~   Ctreat * P_addition * Season + (1|Block), data = FRP)
Anova(mcps, type = "II", test = "F")

mcpd <- lmer(sqrt(Fine_root_production ) ~   Ctreat * P_addition * Depth  + (1|Block), data = FRP)
Anova(mcpd, type = "II", test = "F")

mcp <- lmer(sqrt(Fine_root_production ) ~   Ctreat * P_addition + (1|Block), data = FRP)
Anova(mcp, type = "II", test = "F")


###### fine root diameter
##### before P addition
mfrdb <- lmer(sqrt(Fine_root_diameter) ~   Ctreat * Season * Depth + (1|Block), data = FRD_before)
Anova(mfrdb, type = "II", test = "F")
### check Homogeneity of Variance and normality
resid <- resid(mfrdb)
model_data <- model.frame(mfrdb)
model_data$resid <- resid
# Levene's test on residuals grouped by Season × Depth × Treatment
leveneTest(resid ~ interaction(Ctreat, Season, Depth), data = model_data)
# Shapiro-Wilk test on residuals
shapiro.test(resid)

##### after P addition
mfrda <- lmer(sqrt(Fine_root_diameter) ~   Ctreat * Season * Depth + (1|Block), data = FRD_after)
Anova(mfrda, type = "II", test = "F")
### check Homogeneity of Variance and normality
resid <- resid(mfrda)
model_data <- model.frame(mfrda)
model_data$resid <- resid
# Levene's test on residuals grouped by Season × Depth × Treatment
leveneTest(resid ~ interaction(Ctreat, Season, Depth), data = model_data)
# Shapiro-Wilk test on residuals
shapiro.test(resid)


##### interactions include P_addition
mcpsd_d <- lmer(sqrt(Fine_root_diameter ) ~   Ctreat * P_addition * Season * Depth + (1|Block), data = FRD)
Anova(mcpsd_d, type = "II", test = "F")

mcps_d <- lmer(sqrt(Fine_root_diameter ) ~   Ctreat * P_addition * Season + (1|Block), data = FRD)
Anova(mcps_d, type = "II", test = "F")

mcpd_d <- lmer(sqrt(Fine_root_diameter ) ~   Ctreat * P_addition * Depth  + (1|Block), data = FRD)
Anova(mcpd_d, type = "II", test = "F")

mcp_d <- lmer(sqrt(Fine_root_diameter ) ~   Ctreat * P_addition + (1|Block), data = FRD)
Anova(mcp_d, type = "II", test = "F")



###### rr/rh
### rr
mrr <- gam(Rroot_derived ~ s(VWC), data = R_com)
summary(mrr)
### rh
mrh <- gam(Rhetero ~ s(VWC), data = R_com)
summary(mrh)
### prop
mprop <- gam(proportion_of_Rroot_derived ~ s(VWC), data = R_com)
summary(mprop)






###### relationship between measured Rsoil and Tsoil, VWC
mtsoil <- lm(Meaured_Rsoil ~ Tsoil, data = REsoil)
anova(mtsoil)

mvwc <- lm(Meaured_Rsoil ~ VWC, data = REsoil)
anova(mtsoil)

