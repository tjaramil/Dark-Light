#load library

library(dplyr)

# Load data

DL <- read.csv("DL.csv", stringsAsFactors = F)


# Look at the data 

summary(DL)
str(DL)
names(DL)

#Determine WT, HET, KO

DL$Group[DL$Group == "S"] <- "WT"  #Change "s" to whatever your letter is for WT
DL$Group[DL$Group == "R"] <- "HET" #Change "r" to whatever your letter is for HET
DL$Group[DL$Group == "T"] <- "KO"  #Change "t" to whatever your letter is for KO

#Create your dataframe

DL.Stats <-  DL %>% 
                group_by(Group) %>% 
                summarise(Count = n(),
                  Lat.2.expl.mean = mean(Crosses),
                  Lat.2.expl.sd = sd(Crosses),
                  Lat.2.expl.sem = Lat.2.expl.sd/sqrt(Count),
                  Lat.2.enter.mean = mean(Crosses),
                  Lat.2.enter.sd = sd(Crosses),
                  Lat.2.enter.sem = Lat.2.enter.sd/sqrt(Count),
                  Crosses.mean = mean(Crosses),
                  Crosses.sd = sd(Crosses),
                  Crosses.sem = Crosses.sd/sqrt(Count),
                  Dark.Expl.mean = mean(Dark.Exploration), 
                  Dark.Expl.sd = sd(Dark.Exploration),
                  Dark.Expl.sem = Dark.Expl.sd/sqrt(Count),
                  Dark.Activ.mean = mean(Dark.Activity),
                  Dark.Activ.sd = sd(Dark.Activity),
                  Dark.Activ.sem = Dark.Activ.sd/sqrt(Count),
                  Light.Explor.mean = mean(Light.Exploration),
                  Light.Explor.sd = sd(Light.Exploration),
                  Light.Explor.sem = Light.Explor.sd/sqrt(Count),
                  Dark.Time.mean = mean(Dark.Time),
                  Dark.Time.sd = sd(Dark.Time),
                  Dark.Time.sem = Dark.Time.sd/sqrt(Count),
                  Light.Time.mean = mean(Light.Time),
                  Light.Time.sd = sd(Light.Time),
                  Light.Time.sem = Light.Time.sd/sqrt(Count))%>% 
                ungroup() %>%
                mutate(Group = factor(Group, levels = x)) %>% 
                arrange(Group)

#Save Stats

write.csv(DL.Stats, "DL_Stats.csv")

#Create graphs


Latency.to.enter.lt <-  ggplot(DL.Stats, aes(Group, Lat.2.enter.mean, fill=Group))+
                        geom_bar(stat = "identity", width = 0.7, colour = "black")+
                        scale_fill_manual(values = c("white", "grey", "black"))+
                        geom_errorbar(aes(ymin = Lat.2.enter.mean, ymax = Lat.2.enter.mean + Lat.2.enter.sem), width = .4)+
                        xlab(NULL)+
                        ylab("Latency to Enter Light Side (sec)")+
                        theme(panel.border = element_blank())+
                        theme(axis.line = element_line(color = "black", size = 0.5))+
                        scale_y_continuous(expand = c(0,0))+
                        theme(aspect.ratio = 9/16)+
                        theme(legend.position = "none")

Latency.to.expl.lt <-   ggplot(DL.Stats, aes(Group, Lat.2.expl.mean, fill=Group))+
                        geom_bar(stat = "identity", width = 0.7, colour = "black")+
                        scale_fill_manual(values = c("white", "grey", "black"))+
                        geom_errorbar(aes(ymin = Lat.2.expl.mean, ymax = Lat.2.expl.mean + Lat.2.expl.sem), width = .4)+
                        xlab(NULL)+
                        ylab("Latency to Explore Light Side (sec)")+
                        theme(panel.border = element_blank())+
                        theme(axis.line = element_line(color = "black", size = 0.5))+
                        scale_y_continuous(expand = c(0,0))+
                        theme(aspect.ratio = 9/16)+
                        theme(legend.position = "none")

Crosses             <-  ggplot(DL.Stats, aes(Group, Crosses.mean, fill=Group))+
                        geom_bar(stat = "identity", width = 0.7, colour = "black")+
                        scale_fill_manual(values = c("white", "grey", "black"))+
                        geom_errorbar(aes(ymin = Crosses.mean, ymax = Crosses.mean + Crosses.sem), width = .4)+
                        xlab(NULL)+
                        ylab("Crosses Between Light and Dark Chambers")+
                        theme(panel.border = element_blank())+
                        theme(axis.line = element_line(color = "black", size = 0.5))+
                        scale_y_continuous(expand = c(0,0))+
                        theme(aspect.ratio = 9/16)+
                        theme(legend.position = "none")

Dark.Time          <-   ggplot(DL.Stats, aes(Group, Dark.Time.mean, fill=Group))+
                        geom_bar(stat = "identity", width = 0.7, colour = "black")+
                        scale_fill_manual(values = c("white", "grey", "black"))+
                        geom_errorbar(aes(ymin = Dark.Time.mean, ymax = Dark.Time.mean + Dark.Time.sem), width = .4)+
                        xlab(NULL)+
                        ylab("Duration in Dark Chamber (sec)")+
                        theme(panel.border = element_blank())+
                        theme(axis.line = element_line(color = "black", size = 0.5))+
                        scale_y_continuous(expand = c(0,0))+
                        theme(aspect.ratio = 9/16)+
                        theme(legend.position = "none")

Light.Time         <-   ggplot(DL.Stats, aes(Group, Light.Time.mean, fill=Group))+
                        geom_bar(stat = "identity", width = 0.7, colour = "black")+
                        scale_fill_manual(values = c("white", "grey", "black"))+
                        geom_errorbar(aes(ymin = Light.Time.mean, ymax = Light.Time.mean + Light.Time.sem), width = .4)+
                        xlab(NULL)+
                        ylab("Duration in Light Chamber (sec)")+
                        theme(panel.border = element_blank())+
                        theme(axis.line = element_line(color = "black", size = 0.5))+
                        scale_y_continuous(expand = c(0,0))+
                        theme(aspect.ratio = 9/16)+
                        theme(legend.position = "none")

multiplot(Latency.to.expl.lt, Latency.to.enter.lt, Crosses, Dark.Time, Light.Time, cols=2)

###################################################################################################################################
# Stats
# If you only have 1 independent variable perform a t-test
# Perform the t-Test 

library(stats)

# H0: mean WT = HET or KO
# two-sided ttest
# assume non-equal variance
# mu(Ho hypothesis)
# alternative is 2sided
# confidence interval is 95%
# variance is not equal
# It is not a paired tTest

# Bouts tTest: Open
#t.test(Latency.to.enter.lt, HET.Open.Bouts, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs HET                
#t.test(WT.Open.Bouts,KO.Open.Bouts, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs KO
#t.test(HET.Open.Bouts,KO.Open.Bouts, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #HET vs KO

# Bouts tTest: Closed
#t.test(WT.Closed.Bouts,HET.Closed.Bouts, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs HET                
#t.test(WT.Closed.Bouts,KO.Closed.Bouts, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs KO
#t.test(HET.Closed.Bouts,KO.Closed.Bouts, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #HET vs KO

#Duration tTest: Open
#t.test(WT.Open.Dur,HET.Open.Dur, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs HET                
#t.test(WT.Open.Dur,KO.Open.Dur, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs KO
#t.test(HET.Open.Dur,KO.Open.Dur, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #HET vs KO

#Duration tTest: Closed
#t.test(WT.Closed.Dur,HET.Closed.Dur, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs HET                
#t.test(WT.Closed.Dur,KO.Closed.Dur, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #WT vs KO
#t.test(HET.Closed.Dur,KO.Closed.Dur, mu=0, alternative = "two.sided", conf=0.95, var.equal = F, paired = F) #HET vs KO

# Welch's t-test performs better than Student t-test whenever sample sizes and variances are unequal between groups, 
# and gives the same result when sample sizes and variances are equal.

#########################################################################################

## ANOVA

# If you have 2 indep variables (eg. Genotype and Sex) conducted a 2-way ANOVA
# Indep variables are Genotype and Sex
# Dep variables is arm frequency

names(DL)
Anova.DL <- aov(Lat..Exp. ~ sex * Group, data = DL) 
summary(Anova.1)

#Post hoc test 
library(agricolae)

tukey <- TukeyHSD(Anova.1, trt='Group')
tukey

scheffe.Geno <- scheffe.test(Anova.1, "Group", group = F, console = F)
scheffe.Geno 

scheffe.Sex <- scheffe.test(Anova.1, c("sex", "Group"), group = F, console = F)
scheffe.Sex 

LSD.test <- LSD.test(Anova.1, "Group", p.adj = "hochberg", group = F, console = F)
LSD.test
