                            #+++PREPARATIONS+++#

#+++removing everything from R+++####
rm(list=ls())

#+++installing packages from Github+++####
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE, 
                         force = TRUE)
devtools::install_github('ISAAKiel/quantAAR', ref = "master", 
                         dependencies = TRUE, force = TRUE)
devtools::install_github("nevrome/varnastats")
install.packages("git2r", dependencies=TRUE, INSTALL_opts = c('--no-lock'))

#+++load library+++####
source("/home/keltoskytoi/Szentloerinz/libraries_Szentloerinc.R")

#+++ Creating a folder structure+++####
projRootDir <- "/home/keltoskytoi/Multivariate_Statistics_Szentloerinc/"
paths<-link2GI::initProj(projRootDir = projRootDir,
                         projFolders = c("DATA/", "OUTPUT/", 
                                         "RESULTS/") ,
                         global = TRUE,
                         path_prefix = "path_")
setwd(path_DATA)

                          ####+READING THE INPUT DATA+####
#read the dataset: all graves, only artifacts####
data <- read.csv("Grave_Artefacts_GO_all.csv",
                                  header = TRUE, sep = ",", row.name=1)
#check your data!
names(data)

#manipulate your data in a ready-to-use format: only to have 0 or 1 as data####
#choose the columns your R interested in from your dataset - we R interested in 
#all in our case
presence_absence_data <- data[,1:42]

#check your data again!
names(presence_absence_data)
str(presence_absence_data) # the are all integers, good!


               ####+TRANSFORM THE INPUT DATA - FOR FIGS 1-4+####

#booleanize the dataset: to have 0 and 1
pam <- quantAAR::booleanize(presence_absence_data)
#pam <- quantAAR::delempty(pam)

               #FIGURE 1 - PRESENCE OF ARTIFACTS TYPE IN SZL####
#let's count the presence of the individual grave goods with presencecount() 
presencematerial <- varnastats::presencecount(pam[,1:42], dim=1)
#let's reform our count with reshape2: 
presence <- reshape2::melt(presencematerial)

#let's plot the counted artifact types in SZL 
plot_1 <- ggplot(presence,
                 aes(x = variable,
                     y = value)) +
          geom_bar(stat = "identity", fill= "darkslategray4", colour = "darkslategray4") + 
          coord_flip()
Figure_1 <- plot_1 + labs(title= "The presence of artifact types in Szentloerinc",
                          x = "Artifact type", y = "Amount") +
            theme(plot.title = element_text(hjust = 0.5, size = 12))
plot(Figure_1)

#let's write the plot in a file
setwd(path_OUTPUT)
ggsave("Figure_1.jpeg", width = 30, height = 20, units = "cm")
ggsave("Figure_1.pdf", width = 40, height = 25, units = "cm")

               #FIGURE 2 - NUMBER OF ARTIFACT TYPES PER GRAVE####
#let's count the number of artifacts per graves in SZL
presencematerial_nbr <- varnastats::presencecount(pam[, 1:42], dim = 2)
#let's reform our count with reshape2: 
presence_nbr <- reshape2::melt(presencematerial_nbr)

#let's plot the amount of artifact types per grave in SZL 
plot_2 <- ggplot(presence_nbr,
                 aes(x = variable,
                     y = value)) +
          geom_bar(stat = "identity", fill= "darkslategray4", colour = "darkslategray4") + 
          coord_flip()
Figure_2 <- plot_2 + labs(title= "The number of artifact types per grave",
                          x = "Grave number", y = "Amount") +
            theme(plot.title = element_text(hjust = 0.5, size = 12))
plot(Figure_2)

#let's write the plot in a file
setwd(path_OUTPUT)
ggsave("Figure_2.jpeg", width = 30, height = 20, units = "cm")
ggsave("Figure_2.pdf", width = 40, height = 25, units = "cm")

        #FIGURE 3 - NUMBER OF ARTIFACT TYPES MORE THAN 1x IN SZL####
#let´s eliminate all artifacts which are present less than 2x in the cemetery
presencematerial_3 <- quantAAR::itremove(pam[,1:42], cmin = 2, rmin = 0)
#let's count the presence of the grave goods with presencecount() 
pres_3 <- varnastats::presencecount(presencematerial_3, dim = 1)
#let's plot the amount of artifact types more than 1x in SZL 
presence_3 <- reshape2::melt(pres_3)

#let's plot the amount of artifact types more than once per grave in SZL 
plot_3 <- ggplot(presence_3,
                 aes(x = variable,
                     y = value)) +
          geom_bar(stat = "identity", fill= "darkslategray4", colour = "darkslategray4") + 
          coord_flip()
Figure_3 <- plot_3 + labs(title= "Number of artifact types present more than 1x in Szentloerinc",
                          x = "Artifact type", y = "Amount")+ 
            theme(plot.title = element_text(hjust = 0.5, size = 12))
plot(Figure_3)

#writing the plot in a file
setwd(path_OUTPUT)
ggsave("Figure_3.jpeg", width = 30, height = 20, units = "cm")
ggsave("Figure_3.pdf", width = 40, height = 25, units = "cm")

      #FIGURE 4 - NUMBER OF ARTIFACT TYPES PER GRAVE MORE THAN ONCE####
#let´s eliminate all artifacts which are present less than 2x in the cemetery
presencematerial_4 <- quantAAR::itremove(pam[,1:42], cmin = 2, rmin = 0)
#let's count the presence of the grave goods per graves
presence_4 <- varnastats::presencecount(presencematerial_4, dim = 2)

#let's plot the amount of artifact types more than once per grave in SZL 
pres_4 <- reshape2::melt(presence_4)
plot_4 <- ggplot(pres_4,
                 aes(x = variable,
                     y = value)) +
          geom_bar(stat = "identity", fill= "darkslategray4", colour = "darkslategray4") + 
          coord_flip()
Figure_4 <- plot_4 + labs(title= "The number of artifact types per grave present more than 1x",
                          x = "Grave number", y = "Amount")+
            theme(plot.title = element_text(hjust = 0.5, size = 12))
plot(Figure_4)

#writing the plot in a file
setwd(path_OUTPUT)
ggsave("Figure_4.jpeg", width = 30, height = 20, units = "cm")
ggsave("Figure_4.pdf", width = 40, height = 25, units = "cm")
                


                 #FIGURE 5 - ARTIFACT TYPES PER GRAVE####
#read data: grave depth vs. absolute artifact quantity/types 
setwd(path_DATA)
graves_goods_depth <- read.csv("all_graves_depth_artifact_nbr.csv",
                               header = TRUE, sep = ",", row.name=1)
#check your data
names(graves_goods_depth)

#some statistics to Fig 5
range(graves_goods_depth$Artifact_types)
#[1]  0 11
mean(graves_goods_depth$Artifact_types)
#[1] 2.414286
median(graves_goods_depth$Artifact_types)
#2
var(graves_goods_depth$Artifact_types)
#[1] 5.637474
sd(graves_goods_depth$Artifact_types)
#[1] 2.374337
#variationcoefficient
sd(graves_goods_depth$Artifact_types)/mean(graves_goods_depth$Artifact_types)
#[1] 0.983453

#the frequency histogram
myhist <- hist(graves_goods_depth$Artifact_types)
multiplier <- myhist$counts / myhist$density
mydensity <- density(graves_goods_depth$Artifact_types)
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist, col="salmon4",
     main="Artifact Types per Grave",
     xlab="Amount of different artifact types per grave",
     ylab="Frequency of graves with multiple artifact types",
     sub= "(Szentloerinc)", cex.sub=.75)
myx <- seq(min(graves_goods_depth$Artifact_types), 
           max(graves_goods_depth$Artifact_types), length.out= 100)
mymean <- mean(graves_goods_depth$Artifact_types)
mysd <- sd(graves_goods_depth$Artifact_types)
normal <- dnorm(x = myx, mean = mymean, sd = mysd)
lines(myx, normal * multiplier[1], col = "burlywood3", lwd = 2)

#to save the result of an R base plot you have to use the Export fucntion in 
#the graphic window!

                            #FIGURE 6 - QQ-PLOT#### 

#Let's check if the distribution of the frequency of artifact types in 
#Szentloeric is normally distributed!

                         #+Shapiro-Wilk test+####
#Normality test
#Our Null Hypothesis is (H0): the frequency is normally distributed
#Our Alternative Hypothesis (H1): the frequency is not normally distributed

#If the p-value < or = to the significance level 0.05, we can reject the 
#null hypothesis and accept the alternative hypothesis. In other words, 
#we would conclude that the sample mean is significantly different from the 
#theoretical mean.

#We can test for normal distribution in different ways:

shapiro.test(graves_goods_depth$Artifact_types)

#Shapiro-Wilk normality test
#data:  graves_goods_depth$Artifact_types
#W = 0.87806, p-value = 5.845e-06 =5,845 * 10-6 = 0,000005845

#0,000005845 < 0.05 = our data is not normally distributed; 
#but the Shapiro-Wilk test is very sensitive so we can check also another test:

                     #+Kolmogorov-Smirnoff (KS) - Test+####
#H0: the data does not differ from a normal distribution
#H1: the data is different from a normal distribution

ks.test(graves_goods_depth$Artifact_types, "pnorm", 
        mean(graves_goods_depth$Artifact_types), 
        sd(graves_goods_depth$Artifact_types))
#One-sample Kolmogorov-Smirnov test

#data:  graves_goods_depth$Artifact_types
#D = 0.15497, p-value = 0.06931
#alternative hypothesis: two-sided

#0.06931 > 0.05, so the data is normally distributed

#so what? 

#We can also check the distribution by a visual representation of the data 
# -> by a QQ Plot

                              ####+QQ-PLOT+####
#first we have to normalize the data
graves <- rnorm(graves_goods_depth$Artifact_types)

qqPlot(graves,
       main="Q-Q (Quantile-Quantile) Probability Plot - Artifact types")

      ####FIGURE 7 - GRAVE DEPTH VS. ABSOLUTE ARTIFACT QUANTITY ETC: #### 

#first reorder the data after depth
ggd_ord <- graves_goods_depth[order(graves_goods_depth$Depth_cm),]
ggd_ord

#plot
g_nb <- ggd_ord$Grave
paste("G", (g_nb)) -> nam
myhist <- barplot(ggd_ord$Depth_cm, names.arg=nam,
                  col="beige", xlab= "Graves", ylab= "Depth in cm")
lines(ggd_ord$Abs_Art_quantity, lwd = 3, col="darkslategrey")
lines(ggd_ord$Artifact_quantity, lwd = 3, col= "black")
lines(ggd_ord$Artifact_types, lwd = 3, col= "coral4")
title(main="Grave depth vs. Absolute Artifact Quantity/Quantity/Types", 
      sub= "(Szentloerinc)", cex.sub=.75)
cols <- c("darkslategrey", "black", "coral4")
legend("topleft", legend = c("Absolute Quantity", "Artifact Quantity", 
                             "Artifact Types"), fill=cols, lty=1, bg="white")

#to save the result of an R base plot you have to use the Export fucntion in 
#the graphic window!

#Shapiro-Wilk Normality test & Spearman's Rank Correlation

#Checking the dirstibution of the absolute artifact quantiy 

#Shapiro-Wilk normality test
shapiro.test(graves_goods_depth$Depth_cm)
#data:  graves_goods_depth$Depth_cm
#W = 0.91983, p-value = 0.000259

# -> H1 = not normally distributed # keep in mind: very sensitive test

shapiro.test(graves_goods_depth$Abs_Art_quantity)
#data:  graves_goods_depth$Abs_Art_quantity
#W = 0.60637, p-value = 2.079e-12 = 2,079 * 10-12

# -> H1 = not normally distributed # keep in mind: very sensitive test

#Q-Q Plots 
#first we have to normalize the data
graves_depth <- rnorm(graves_goods_depth$Depth_cm)
graves_art_quant <- rnorm(graves_goods_depth$Abs_Art_quantity)

qqPlot(graves_depth,
       main="Q-Q (Quantile-Quantile) Probability Plot - Grave depth")

qqPlot(graves_art_quant,
       main="Q-Q (Quantile-Quantile) Probability Plot - Absolute artifact quantity")

#our data is not continuous but ordinal and not normally distributed: 
#=> we have to use SPEARMAN'S RANK CORRELATION

# -> does the number of artifacts depend on/correlate with the grave depth? 
cor.test(graves_goods_depth$Abs_Art_quantity, graves_goods_depth$Depth_cm, 
         method = "spearman")
#Spearman's rank correlation rho

#data:  graves_goods_depth$Abs_Art_quantity and graves_goods_depth$Depth_cm
#S = 45591, p-value = 0.09302
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.2023204 

# 0.2023204 translates into a weak correlation 

# FIGURE 10 - OIRENTATION of MALE, FEMALE, ? and HORSE GRAVES in SZL ####

#create a mtrix from the orientation of the graves
orientation <-  matrix(c(10, 4, 2, 3, 2, 1, 0, 1, 0,
                         9, 2, 3, 3, 1, 1, 0, 0, 0,
                         3, 1, 0, 0, 2, 0, 1, 0, 0,
                         0, 2, 2, 0, 0, 1, 0, 0, 1), 
                         ncol=9, byrow=TRUE)

#give column and rownames
colnames(orientation) <- c("W-E","E-W", "NW-SE", "S-N", "EW-WE", "SW-NE", "N-S", "NE-SW", "SE-NW")
rownames(orientation) <- c("female", "male", "?","Horse")

orientation <- as.table(orientation)
orientation

#create the stacked barplot
barplot(orientation, col= c("darkgoldenrod", "darkslategray4", "salmon4",
                            "darkolivegreen"), 
        border="white", space=0.04, font.axis=2, xlab="Orientation", ylab="Quantity")
col= c("darkgoldenrod", "darkslategray4", "salmon4", "darkolivegreen")
title(main="Orientation of female, male, unknown & Horse graves", 
      sub= "(Szentloerinc)", cex.sub=.75)
legend("topright", legend = c("female", "male", "?", "Horse"), 
       fill=col, lty=1, bg="white")

# we could of course test if the orientation of the graves is normally distributed,
# but we have still a lot to do! 

#to save the result of an R base plot you have to use the Export fucntion in 
#the graphic window!