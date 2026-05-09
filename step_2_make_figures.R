###### figure 3
Esoil <- readRDS("data/figshare/soil_environmental_variables.rds")

f3a <- ggplot(Esoil, aes(date, Tsoil.mean, color = treatment)) +
  scale_color_manual(values=c("Cp"="#f28ca6",
                              "cp"="#5A8BE6",
                              "CP"="red",
                              "cP"="blue2"))+
  geom_point() +
  theme_linedraw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.position = c(.12, .65),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.1)) +
  scale_x_date(breaks = seq(as.Date("2014-07-01"), as.Date("2015-03-31"), by = "2 months"), labels = function(x) substr(format(x, "%b"), 1, 3)) +
  labs(title = "", x = "", y = expression(italic(T)[soil]~ ~(degree*C))); f3a

f3b <- ggplot(Esoil_all, aes(date, VWC.mean, color = treatment)) +
  geom_point() +
  scale_color_manual(values=c("Cp"="#f28ca6",
                              "cp"="#5A8BE6",
                              "CP"="red",
                              "cP"="blue2"))+
  ylim(0, 0.35) +
  theme_linedraw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.position = "none",
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.1)) +
  scale_x_date(breaks = seq(as.Date("2014-07-01"), as.Date("2015-03-31"), by = "2 months"), labels = function(x) substr(format(x, "%b"), 1, 3)) +
  labs(title = "", x = "", y = expression("VWC " * " (" * m^3 * " " * m^-3 * ")")); f3b

f3c <- ggplot(data = REsoil3, aes(x = Campaign1, y = Meaured_Rsoil.mean.mean, color = treatment)) +
  geom_point(stat="identity", position=position_dodge(width = 8)) +
  geom_errorbar(stat="identity", position=position_dodge(width = 8), aes(ymin = Meaured_Rsoil.mean.mean-Meaured_Rsoil.mean.sd, ymax = Meaured_Rsoil.mean.mean + Meaured_Rsoil.mean.sd), width=0) +
  theme_linedraw() + 
  ylim(0, 8) +
  scale_color_manual(values=c("Cp"="#f28ca6",
                              "cp"="#5A8BE6",
                              "CP"="red",
                              "cP"="blue2"))+
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position = "none",
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) + 
  scale_x_date(breaks = seq(as.Date("2014-07-01"), as.Date("2015-03-31"), by = "2 months"), labels = function(x) substr(format(x, "%b"), 1, 3)) +
  labs(title = "", x = "", y = expression(atop(italic(R)[soil],
                                               (µmol~CO[2]~m^-2~s^-1)))); f3c


f3d <- ggplot(data = REsoil3, aes(x = Campaign1, y = Normalized_Rsoil.mean.mean, color = treatment)) +
  geom_point(stat="identity", position=position_dodge(width = 8)) +
  geom_errorbar(stat="identity", position=position_dodge(width = 8), aes(ymin = Normalized_Rsoil.mean.mean-Normalized_Rsoil.mean.sd, ymax = Normalized_Rsoil.mean.mean + Normalized_Rsoil.mean.sd), width=0) +
  theme_linedraw() + 
  ylim(0, 8) +
  scale_color_manual(values=c("Cp"="#f28ca6",
                              "cp"="#5A8BE6",
                              "CP"="red",
                              "cP"="blue2"))+
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position = "none", 
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) + 
  scale_x_date(breaks = seq(as.Date("2014-07-01"), as.Date("2015-03-31"), by = "2 months"), labels = function(x) substr(format(x, "%b"), 1, 3)) +
  labs(title = "", x = "", y = expression(atop("*"~italic(R)[soil], (µmol~CO[2]~m^-2~s^-1)))); f3d

f3 <- f3a + f3b + f3c + f3d + plot_layout(nrow = 4, ncol = 1)




              

##### figure 4
##### fig4a
f4a <- ggplot(data = REsoil1, aes(x = VWC.mean, y = log_Normalized_Rsoil.mean)) +
  geom_point(stat="identity", aes(color = treatment, shape = treatment), size = 2) + 
  theme_linedraw() + 
  geom_smooth(method = "glm",
              aes(color = treatment, linetype = treatment, fill = treatment),
              se = TRUE, alpha = 0.2) +
  ylim(0.5, 1.8) +
  scale_color_manual(values=c("Cp"="#f28ca6",
                              "cp"="#5A8BE6",
                              "CP"="red",
                              "cP"="blue"))+
  scale_fill_manual(values=c("Cp"="#f28ca6",
                             "cp"="#5A8BE6",
                             "CP"="red",
                             "cP"="blue"))+
  scale_shape_manual(values=c("Cp"=16,
                              "cp"=16,
                              "CP"=17,
                              "cP"=17))+
  scale_linetype_manual(values=c("CP"="solid",
                                 "cP"="solid",
                                 "Cp"="dashed",
                                 "cp"="dashed"))+
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        legend.position = c(.8, 0.08),
        legend.direction="horizontal", 
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) +
  labs(title = "", x = expression(VWC~(m^3~m^-3)), y = expression(atop(Log~"*"~italic(R)[soil],
                                                                       (µmol~CO[2]~m^-2~s^-1)))); f4a


##### fig4b
f4b <- ggplot(data = REsoil1, aes(x = VWC.mean, y = log_Normalized_Rsoil.mean)) +
  geom_point(stat="identity", aes(color = P_addition, shape = P_addition), size = 2) + #aes(color = P_addition)
  # geom_smooth(method = "lm", aes(linetype = P_addition), se = TRUE) +
  geom_smooth(method = "glm", 
              method.args = list(family = Gamma), color = "grey2", aes(linetype = P_addition, fill = P_addition), alpha = 0.2, se = TRUE) +
  # stat_regline_equation(aes(color = treatment, label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
  # geom_errorbar(aes(color = treatment, ymin = log_Rsoilmax15.mean-log_Rsoilmax15.sd, ymax = log_Rsoilmax15.mean + log_Rsoilmax15.sd), width=0.005) +
  theme_linedraw() + 
  # xlim(0,0.2) +
  ylim(0.5, 1.8) +
  scale_color_manual(values=c("before"="grey",
                              "after"="black")) +
  scale_fill_manual(values=c("before"="grey",
                             "after"="black")) +
  scale_shape_manual(values=c("before"=16,
                              "after"=17)) +
  scale_linetype_manual(values=c("after"="solid",
                                 "before"="dashed")) +
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=32),
        axis.title.x=element_text(size=32),
        axis.text.y=element_text(size=32),
        axis.title.y=element_text(size=32),
        legend.text=element_text(size=32),
        legend.title=element_blank(),
        legend.position = c(.8, 0.12),
        legend.direction="horizontal", 
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) +
  labs(title = "", x = expression(VWC~(m^3~m^-3)), y = expression(atop(Log~"*"~italic(R)[soil],
                                                                       (µmol~CO[2]~m^-2~s^-1)))); f4b


##### fig4c
f4c <- ggplot(data = REsoil1, aes(x = VWC.mean, y = log_Normalized_Rsoil.mean)) +
  geom_point(stat="identity", aes(color = Ctreat), size = 2) + #aes(color = Ctreat)
  geom_smooth(method = "glm", 
              method.args = list(family = Gamma), aes(color = Ctreat, fill = Ctreat), alpha = 0.05,
              se = TRUE) +
  theme_linedraw() + 
  ylim(0.5, 1.8) +
  scale_color_manual(values=c("amb"="blue",
                              "ele"="red"))+
  scale_fill_manual(values=c("amb"="blue",
                             "ele"="red"))+
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=32),
        axis.title.x=element_text(size=32),
        axis.text.y=element_text(size=32),
        axis.title.y=element_text(size=32),
        legend.text=element_text(size=32),
        legend.title=element_blank(),
        legend.position = c(.8, 0.10),
        legend.direction="horizontal", 
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) +
  labs(title = "", x = expression(VWC~(m^3~m^-3)), y = expression(atop(Log~"*"~italic(R)[soil],
                                                                       (µmol~CO[2]~m^-2~s^-1)))); f4c




###### figure 5
MBC$P_addition <- factor(MBC$P_addition, levels = c("before", "after"))
f5 <- ggplot(micro, aes(x = P_addition, y = Cmic, fill = Ctreat)) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.7), width = 0.6,
               color = "black") +  
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.7), width = 0.2) +
  scale_fill_manual(values = c("amb" = "#5A8BE6", "ele" = "#f28ca6"),
                    labels = c("amb" = "amb", "ele" = "ele"),
                    name = expression(CO[2]),
                    guide = guide_legend(keywidth = 0.6, keyheight = 0.6, default.unit = "cm")) +
  theme_linedraw() + 
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 18, face="bold.italic"),
        axis.text.x=element_text(size=18),
        axis.title.x=element_text(size=18, face="bold.italic"),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        panel.grid.major=element_blank()) + 
  labs(x = "", y = expression(italic(C)[mic]~(µg~g^-1))); f5






###### figure 6
f6_before <- ggplot(FRP_before, aes(x = as.factor(Depth), y = Fine_root_production , fill = Ctreat)) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.7), width = 0.6,
               color = "black") +  # subtle black outline for clarity
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.7), width = 0.2) +
  facet_wrap(~Season, nrow = 1) +
  scale_fill_manual(values = c("amb" = "#5A8BE6", "ele" = "#f28ca6"),
                    labels = c("amb" = "ambient", "ele" = "elevated"),
                    name = expression(CO[2]),
                    guide = guide_legend(keywidth = 0.6, keyheight = 0.6, default.unit = "cm")) +
  labs(x = "Soil depth (cm)",
       y = expression("Root diameter (mm)")) +
  theme_minimal(base_size = 12, base_family = "sans") +  # good base font
  theme(
    strip.text = element_text(size = 18, face = "bold.italic"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.ticks = element_line(color = "black", size = 0.4),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "right",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),  # no border
    axis.line = element_line(size = 0.4),  # subtle axis line
    plot.margin = margin(10, 10, 10, 10))+ 
  labs(x = "Before P addition", y = expression("Fine root production"~(g~m^-2~day^-1))); f6_before


f6_after <- ggplot(FRP_after, aes(x = as.factor(Depth), y = Fine_root_production, fill = Ctreat)) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.7), width = 0.6,
               color = "black") +  # subtle black outline for clarity
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.7), width = 0.2) +
  ylim(0, 0.4) +
  facet_wrap(~Season, nrow = 1) +
  scale_fill_manual(values = c("amb" = "blue", "ele" = "red"),
                    labels = c("amb" = "ambient", "ele" = "elevated"),
                    name = expression(CO[2]),
                    guide = guide_legend(keywidth = 0.6, keyheight = 0.6, default.unit = "cm")) +
  labs(x = "Soil depth (cm)",
       y = expression("Root diameter (mm)")) +
  theme_minimal(base_size = 12, base_family = "sans") +  # good base font
  theme(
    strip.text = element_text(size = 18, face = "bold.italic"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.ticks = element_line(color = "black", size = 0.4),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "right",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),  # no border
    axis.line = element_line(size = 0.4),  # subtle axis line
    plot.margin = margin(10, 10, 10, 10))+ 
  labs(x = "After P addition", y = expression("Fine root production"~(g~m^-2~day^-1))); f6_after






###### figure 2
f2a <- ggplot(data = R_com, aes(x = VWC, y = Rroot_derived)) +
  geom_point(stat="identity", color = "darkgrey", size = 2, alpha = 0.6) +
  # stat_smooth(method = 'nls', method.args = list(start = c(a=1, b=1)),
  #             formula = y ~ a*exp(b*x), colour = 'royalblue', se = FALSE) +
  # geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "royalblue", se = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), color = "royalblue", se = FALSE) +
  # stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
  theme_linedraw() + 
  # xlim(0.03, 0.20) +
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=12),
        # legend.position = c(.08, .82),
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) + 
  labs(title = "", x = expression(VWC~(m^3~m^-3)), y = expression(italic(R)[r]~(µmol~CO[2]~m^-2~s^-1))); f2a



f2b <- ggplot(data = R_com, aes(x = VWC, y = Rhetero)) +
  geom_point(stat="identity", color = "darkgrey", size = 2, alpha = 0.6) +
  # stat_smooth(method = 'nls', method.args = list(start = c(a=1, b=1)),
  #             formula = y ~ a*exp(b*x), colour = 'royalblue', se = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), color = "royalblue", se = FALSE) +
  # geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "royalblue", se = FALSE) +
  # stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
  theme_linedraw() + 
  # xlim(0.03, 0.20) +
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=12),
        # legend.position = c(.08, .82),
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) + 
  labs(title = "", x = "", y = expression(italic(R)[h]~(µmol~CO[2]~m^-2~s^-1))); f2b


f2c <- ggplot(data = R_com, aes(x = VWC, y = proportion_of_Rroot_derived)) +
  geom_point(stat="identity", color = "darkgrey", size = 2, alpha = 0.6) +
  # stat_smooth(method = 'nls', method.args = list(start = c(a=1, b=1)),
  #             formula = y ~ a*exp(b*x), colour = 'royalblue', se = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), color = "royalblue", se = FALSE) +
  # geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "royalblue", se = FALSE) +
  # stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
  theme_linedraw() + 
  # xlim(0.03, 0.20) +
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=12),
        # legend.position = c(.08, .82),
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) + 
  labs(title = "", x = expression(VWC~(m^3~m^-3)), y = expression(Proportion~of~italic(R)[r])); f2c


f2 <- f2a + f2b + f2c + plot_layout(nrow = 3, ncol = 1)






###### figure s4
ptsoil <- ggplot(data = REsoil, aes(x = Tsoil, y = Meaured_Rsoil)) +
  geom_point(color = "darkgrey", stat="identity", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "royalblue") +
  theme_linedraw() + 
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=12),
        legend.position="none",
        legend.direction="vertical", 
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) +
  labs(title = "", x = expression(italic(T)[soil]~ ~(degree*C)), y = expression(italic(R)[soil]~(µmol~CO[2]~m^2~s^-1))); ptsoil


pvwc <- ggplot(data = REsoil, aes(x = VWC, y = Meaured_Rsoil)) +
  geom_point(stat="identity", size = 2, color = "darkgrey") +
  geom_smooth(method = "lm", color = "royalblue", se = FALSE) +
  theme_linedraw() + 
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.direction="vertical", 
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) +
  labs(title = "", x = expression(VWC~(m^3~m^-3)), y = expression(italic(R)[soil]~(µmol~CO[2]~m^2~s^-1))); pvwc

fs4 <- ptsoil + pvwc + plot_layout(nrow = 1, ncol = 2)



###### figure s6
p <- ggplot(data = REsoil1, aes(x = VWC.mean, y = log_Meaured_Rsoil.mean)) +
  geom_point(stat="identity", aes(color = treatment, shape = treatment), size = 2) + #aes(color = treatment, shape = treatment)
  theme_linedraw() + 
  geom_smooth(method = "glm",
              aes(color = treatment, linetype = treatment, fill = treatment),
              se = TRUE, alpha = 0.2) +
  ylim(0.5, 1.8) +
  scale_color_manual(values=c("Cp"="#f28ca6",
                              "cp"="#5A8BE6",
                              "CP"="red",
                              "cP"="blue"))+
  scale_fill_manual(values=c("Cp"="#f28ca6",
                             "cp"="#5A8BE6",
                             "CP"="red",
                             "cP"="blue"))+
  scale_shape_manual(values=c("Cp"=16,
                              "cp"=16,
                              "CP"=17,
                              "cP"=17))+
  scale_linetype_manual(values=c("CP"="solid",
                                 "cP"="solid",
                                 "Cp"="dashed",
                                 "cp"="dashed"))+
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_blank(),
        legend.position = c(.8, 0.08),
        legend.direction="horizontal", 
        panel.grid.major=element_blank(),
        plot.title = element_text(size=14, face="bold.italic", 
                                  hjust = 0.5)) +
  labs(title = "", x = expression(VWC~(m^3~m^-3)), y = expression(atop(Log~italic(R)[soil],
                                                                       (µmol~CO[2]~m^-2~s^-1)))); p






###### figure S7
fs6_before <- ggplot(FRD_before, aes(x = as.factor(Depth), y = Fine_root_diameter, fill = Ctreat)) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.7), width = 0.6,
               color = "black") +  # subtle black outline for clarity
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.7), width = 0.2) +
  facet_wrap(~Season, nrow = 1) +
  scale_fill_manual(values = c("amb" = "#5A8BE6", ele = "#f28ca6"),
                    labels = c("amb" = "ambient", "ele" = "elevated"),
                    name = expression(CO[2]),
                    guide = guide_legend(keywidth = 0.6, keyheight = 0.6, default.unit = "cm")) +
  theme_minimal(base_size = 12, base_family = "sans") +  # good base font
  theme(
    strip.text = element_text(size = 18, face = "bold.italic"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.ticks = element_line(color = "black", size = 0.4),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "right",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),  # no border
    axis.line = element_line(size = 0.4),  # subtle axis line
    plot.margin = margin(10, 10, 10, 10))+ 
  labs(x = "Before P addition", y = expression("Root diamter"~(mm))); fs6_before


fs6_after <- ggplot(FRD_after, aes(x = as.factor(Depth), y = Fine_root_diameter , fill = Ctreat)) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.7), width = 0.6,
               color = "black") +  # subtle black outline for clarity
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.7), width = 0.2) +
  facet_wrap(~Season, nrow = 1) +
  scale_fill_manual(values = c("amb" = "blue", ele = "red"),
                    labels = c("amb" = "ambient", "ele" = "elevated"),
                    name = expression(CO[2]),
                    guide = guide_legend(keywidth = 0.6, keyheight = 0.6, default.unit = "cm")) +
  labs(x = "Soil depth (cm)",
       y = expression("Root diameter (mm)")) +
  theme_minimal(base_size = 12, base_family = "sans") +  # good base font
  theme(
    strip.text = element_text(size = 18, face = "bold.italic"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.ticks = element_line(color = "black", size = 0.4),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "right",
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),  # no border
    axis.line = element_line(size = 0.4),  # subtle axis line
    plot.margin = margin(10, 10, 10, 10))+ 
  labs(x = "After P addition", y = expression("Root diamter"~(mm))); fs6_after


