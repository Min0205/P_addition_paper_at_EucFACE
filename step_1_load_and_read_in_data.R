###### load packages
library(tidyverse)
library(lubridate)
library(lme4) # lmer
library(patchwork)
library(car) ### Anova for lmer p value
library(mgcv) # gam
library(glmmTMB) # glmm
Sys.setlocale("LC_TIME", "C")


###### soil respiration
REsoil <- read.csv("data/figshare/soil_respiration.csv")
REsoil$log_Normalized_Rsoil <- log(REsoil$Normalized_Rsoil)
REsoil$log_Meaured_Rsoil <- log(REsoil$Meaured_Rsoil)
REsoil <- REsoil %>% mutate(P_addition = case_when(Campaign < "2018-01-01" ~ "before",
                                         Campaign >= "2018-01-01" ~ "after"))
REsoil$Treatment <- paste(REsoil$Ctreat, REsoil$P_addition, sep="_")
REsoil <- REsoil %>% mutate(treatment = case_when(Treatment == "amb_before" ~ "cp",
                                        Treatment == "ele_before" ~ "Cp",
                                        Treatment == "amb_after" ~ "cP",
                                        Treatment == "ele_after" ~ "CP"))
REsoil$treatment <- factor(REsoil$treatment, levels=c("cp", "Cp", "cP", "CP"))

### for figure 4
REsoil1 <- doBy::summaryBy(log_Normalized_Rsoil + log_Meaured_Rsoil + VWC ~ Campaign + treatment, FUN=c(mean, sd),
                           na.rm = T, keep.names = T, data = REsoil)
REsoil1 <- REsoil1 %>% mutate(P_addition = case_when(Campaign < "2018-01-01" ~ "before",
                                                     Campaign >= "2018-01-01" ~ "after"))
REsoil1$P_addition <- factor(REsoil1$P_addition, levels = c("before", "after"))
REsoil1 <- REsoil1 %>% mutate(Ctreat = case_when(treatment %in% c("cp", "cP") ~ "amb",
                                                 treatment %in% c("Cp", "CP") ~ "ele"))
### for figure 3
REsoil2 <- doBy::summaryBy(Meaured_Rsoil + Normalized_Rsoil + VWC + Tsoil ~ Plot + Block + treatment + Campaign, FUN=c(mean, sd),
                           na.rm = T, keep.names = T, data = REsoil)
REsoil3 <- doBy::summaryBy(Meaured_Rsoil.mean + Normalized_Rsoil.mean + VWC.mean + Tsoil.mean ~ treatment + Campaign, FUN=c(mean, sd),
                           na.rm = T, keep.names = T, data = REsoil2)
REsoil3$Campaign <- lubridate::ymd(REsoil3$Campaign)
REsoil3 <- REsoil3 %>% mutate(Campaign1 = if_else(lubridate::year(Campaign) >= 2018, Campaign - lubridate::years(9), Campaign))



###### microbial biomass carbon
MBC <- read.csv("data/figshare/microbial_biomass_carbon.csv")
MBC <- MBC %>% mutate(P_addition = case_when(Campaign == "20150310" ~ "before",
                                             Campaign == "20200318" ~ "after"))



###### fine root variables
##### production
FRP <- read.csv("data/figshare/fine_root_production.csv")
FRP <- FRP %>% mutate(P_addition = case_when((Campaign %in% c("201412", "201505")) ~ "before",
                                            (Campaign %in% c("202406", "202312")) ~ "after"),
                      Treatment = case_when((Plot %in% c("2", "3")) & P_addition == "after" ~ "cP",
                                            (Plot %in% c("2", "3")) & P_addition == "before" ~ "cp",
                                            (Plot %in% c("1", "4")) & P_addition == "after" ~ "CP",
                                            (Plot %in% c("1", "4") & P_addition == "before") ~ "Cp"),
                      Season = case_when((Campaign %in% c("201412", "202312")) ~ "Summer",
                                         (Campaign %in% c("201505", "202406")) ~ "Winter"))
FRP_before <- subset(FRP, P_addition == "before")
FRP_after <- subset(FRP, P_addition == "after")

##### diameter
FRD <- read.csv("data/figshare/fine_root_diameter.csv")
FRD <- FRD %>% mutate(P_addition = case_when((Campaign %in% c("201412", "201505")) ~ "before",
                                             (Campaign %in% c("202406", "202312")) ~ "after"),
                      Treatment = case_when((Ring %in% c("2", "3")) & P_addition == "after" ~ "cP",
                                            (Ring %in% c("2", "3")) & P_addition == "before" ~ "cp",
                                            (Ring %in% c("1", "4")) & P_addition == "after" ~ "CP",
                                            (Ring %in% c("1", "4") & P_addition == "before") ~ "Cp"),
                      Season = case_when((Campaign %in% c("201412", "202312")) ~ "Summer",
                                         (Campaign %in% c("201505", "202406")) ~ "Winter"))
FRD_before <- subset(FRD, P_addition == "before")
FRD_after <- subset(FRD, P_addition == "after")


###### Rr/Rh
R_com <- read.csv("data/figshare/root_derived_and_SOM_derived_respiraiton_in_2018.csv")

