                               #+++PREPARATIONS+++#

#+++removing everything from R+++####
rm(list=ls())

#+++load library+++####
source("/home/keltoskytoi/Szentloerinz/libraries_Szentloerinc.R")

#+++ Creating a folder structure+++#####
projRootDir <- "/home/keltoskytoi/Multivariate_Statistics_Szentloerinc/"
paths<-link2GI::initProj(projRootDir = projRootDir,
                         projFolders = c("DATA/", "OUTPUT/", 
                                         "RESULTS/") ,
                         global = TRUE,
                         path_prefix = "path_")
setwd(path_DATA)

                 #FIG 8 - GENDER AND ORIENTATION####
#we need an extra dataset with all graves and information about gender, 
#orientation and burial custom - it is not important if some do not contain
#grave goods or no information about any relevant feature

gend_ori <- read.csv("gender_orientation_binary.csv",
                     header = TRUE, sep = ";", row.name=1)
names(gend_ori)

#let's test our dataset on normal distribution!####

#Shapiro-Wilk Normality test, KS-test & Spearman's Rank Correlation

female <- ggqqplot(gend_ori$F, ylab = "female graves")
setwd(path_OUTPUT)
ggsave("qqplot_female_graves.jpeg", width = 40, height = 20, units = "cm") 

#KS-test
ks.test(gend_ori$F, "pnorm", 
        mean(gend_ori$F), 
        sd(gend_ori$F))
#D = 0.33944, p-value = 7.871e-07

#SW-test
shapiro.test(gend_ori$F)
#W = 0.63656, p-value = 2.57e-11

#because our data is binary, it won't be normally distributed 

#let's calculate a correlation matrix with significance levels (p-value)####

#our data is not continuous but ordinal and not normally distributed
#=> we have to use SPEARMAN'S RANK CORRELATION

corr_matr <- rcorr(as.matrix(gend_ori), type=c("spearman"))

#              F     M     U inhumation cremation  EWWE    WE    EW    NS  NWSE  SWNE    SN  SENW    XX
#F           1.00 -0.61 -0.37       0.16      0.12  0.00 -0.03  0.05  0.13  0.00  0.00  0.00 -0.13  0.00
#M          -0.61  1.00 -0.36       0.22     -0.11 -0.07  0.05 -0.08 -0.10  0.07  0.04  0.19 -0.10 -0.07
#U          -0.37 -0.36  1.00      -0.42      0.12  0.22 -0.05 -0.03 -0.06 -0.15 -0.08 -0.12  0.28  0.08
#inhumation  0.16  0.22 -0.42       1.00     -0.63 -0.37  0.25  0.04  0.06  0.15  0.09  0.12  0.06 -0.32
#cremation   0.12 -0.11  0.12      -0.63      1.00  0.05 -0.32 -0.03 -0.06 -0.15 -0.08  0.05 -0.06  0.45
#EWWE        0.00 -0.07  0.22      -0.37      0.05  1.00 -0.18 -0.09 -0.03 -0.08 -0.05 -0.07 -0.03 -0.16
#WE         -0.03  0.05 -0.05       0.25     -0.32 -0.18  1.00 -0.24 -0.09 -0.22 -0.13 -0.18 -0.09 -0.44
#EW          0.05 -0.08 -0.03       0.04     -0.03 -0.09 -0.24  1.00 -0.04 -0.11 -0.06 -0.09 -0.04 -0.22
#NS          0.13 -0.10 -0.06       0.06     -0.06 -0.03 -0.09 -0.04  1.00 -0.04 -0.02 -0.03 -0.02 -0.08
#NWSE        0.00  0.07 -0.15       0.15     -0.15 -0.08 -0.22 -0.11 -0.04  1.00 -0.06 -0.08 -0.04 -0.20
#SWNE        0.00  0.04 -0.08       0.09     -0.08 -0.05 -0.13 -0.06 -0.02 -0.06  1.00 -0.05 -0.02 -0.11
#SN          0.00  0.19 -0.12       0.12      0.05 -0.07 -0.18 -0.09 -0.03 -0.08 -0.05  1.00 -0.03 -0.16
#SENW       -0.13 -0.10  0.28       0.06     -0.06 -0.03 -0.09 -0.04 -0.02 -0.04 -0.02 -0.03  1.00 -0.08
#XX          0.00 -0.07  0.08      -0.32      0.45 -0.16 -0.44 -0.22 -0.08 -0.20 -0.11 -0.16 -0.08  1.00

#n= 64 

#P
#             F      M      U      inhumation cremation EWWE   WE     EW     NS     NWSE   SWNE   SN     SENW   XX    
#F                 0.0000 0.0024 0.2062     0.3280    1.0000 0.7941 0.6944 0.3212 1.0000 1.0000 1.0000 0.3212 1.0000
#M          0.0000        0.0030 0.0800     0.3865    0.5589 0.6697 0.5540 0.4277 0.5713 0.7521 0.1323 0.4277 0.5640
#U          0.0024 0.0030        0.0006     0.3377    0.0744 0.6731 0.8326 0.6524 0.2479 0.5204 0.3546 0.0270 0.5119
#inhumation 0.2062 0.0800 0.0006            0.0000    0.0025 0.0459 0.7532 0.6347 0.2228 0.4978 0.3288 0.6347 0.0093
#cremation  0.3280 0.3865 0.3377 0.0000               0.6748 0.0104 0.8326 0.6524 0.2479 0.5204 0.6748 0.6524 0.0002
#EWWE       1.0000 0.5589 0.0744 0.0025     0.6748           0.1536 0.4770 0.7986 0.5141 0.7160 0.6007 0.7986 0.2023
#WE         0.7941 0.6697 0.6731 0.0459     0.0104    0.1536        0.0511 0.4890 0.0742 0.3230 0.1536 0.4890 0.0003
#EW         0.6944 0.5540 0.8326 0.7532     0.8326    0.4770 0.0511        0.7290 0.3752 0.6212 0.4770 0.7290 0.0818
#NS         0.3212 0.4277 0.6524 0.6347     0.6524    0.7986 0.4890 0.7290        0.7505 0.8591 0.7986 0.9009 0.5359
#NWSE       1.0000 0.5713 0.2479 0.2228     0.2479    0.5141 0.0742 0.3752 0.7505        0.6503 0.5141 0.7505 0.1109
#SWNE       1.0000 0.7521 0.5204 0.4978     0.5204    0.7160 0.3230 0.6212 0.8591 0.6503        0.7160 0.8591 0.3768
#SN         1.0000 0.1323 0.3546 0.3288     0.6748    0.6007 0.1536 0.4770 0.7986 0.5141 0.7160        0.7986 0.2023
#SENW       0.3212 0.4277 0.0270 0.6347     0.6524    0.7986 0.4890 0.7290 0.9009 0.7505 0.8591 0.7986        0.5359
#XX         1.0000 0.5640 0.5119 0.0093     0.0002    0.2023 0.0003 0.0818 0.5359 0.1109 0.3768 0.2023 0.5359       

                  # FIG 8A - Plotting of the correlation matrix####
#let's visualize the correlation matrix with a correlogram, (without the hierarchical clustering)!
matr <- corr_matr$r 
col <- colorRampPalette(c("darkslategray4","white", "salmon4"))(20)
corrplot(matr, method = "color", type = "upper", addCoef.col = "darkslategray4",
         diag = FALSE, #you hide the correlation coefficient on the principal diagonal
         col=col, tl.col = "darkslategray4", tl.cex = 1.2)

               # FIG 8B - plotting of the correlation coefficient####
#let's visualize the significance of the correlation coefficients (without the hierarchical clustering)
signif <- corr_matr$P 
col <- colorRampPalette(c("darkslategray4","white", "salmon4"))(20)
corrplot(signif, method = "color", type = "upper", addCoef.col = "darkslategray4",
         diag = FALSE, #you can hide the correlation coefficient on the principal diagonal
         col=col, tl.col = "darkslategray4", tl.cex = 1.2)

####ADDITAMENTUM - as comparision ####
#let's visualize the correlation matrix with a correlogram!
matr <- corr_matr$r
col <- colorRampPalette(c("darkslategray4","white", "salmon4"))(20)
corrplot(matr, method = "color", type = "upper", 
         order = "hclust", addCoef.col = "darkslategray4",
         diag = FALSE, #you can hide the correlation coefficient on the principal diagonal
         col=col, tl.col = "darkslategray4", tl.cex = 1.2)


                   #FIG 14 - CA OF THE FIBULAE####
#read the file containing the data about the fibulae  
fibulae <- read.csv("fibulae.csv", header = TRUE, sep = ",", row.name=1)
names(fibulae)

#calculating the CA object (a list with Principla Intertias)
ca_fib <- ca(fibulae)

#plotting dimensions 1&2
plot(ca_fib, dim = c(1,2), map = "rowprincipal", what = c("all", "all"), 
     mass=c(TRUE, TRUE), contrib=c("relative", "relative"),
     col=c("darkslategray4", "salmon4"), labels=c(2,2),
     col.lab = c("darkslategray4", "salmon4"),
     xlim=c(1,1), ylim=c(-2,3),  
     xlab = "1st CA-Axis", ylab = "2nd CA-Axis", cex.lab=.9)
title(main="Biplot of the CA of the Fibulae in Szentloerinc", 
      sub= "(symmetric biplot with rows in Principalcoordinates)", cex.sub=.75)

              #FIGURE 15 - CLUSTERDENTROGRAMM FIBULAE####

#Hierarchical Cluster Analysis - CA
#Cluster Analysis needs a dissimilarity matrix/index as input:
diss_fibulae <- vegdist(fibulae)
#hclust deliveres different clustering algorithms 
#we use UPGMA, because it gives the best result for our endeavour 
clust_AV <- hclust(diss_fibulae, "average") 
plot(clust_AV, main="Cluster Dendrogramm UPGMA - Fibulae Szentloerinc",
     xlab = "data: fibulae Szentlőrinc",
     sub = "function hclust, average  (vegan package)")
rect.hclust(clust_AV, k= 2, border = "darkslategray4")

                #FIGURE 16 - CLUSTERED CA FIBULAE####

#plotting the clustering on the Ordinationplot
grouping <- cutree(clust_AV, 2)

ord <- cca(fibulae)
plot(ord)#, display = "sites")
ordihull(ord, grouping, lty =2, col= "darkslategray4")
title(main="Biplot of the CA with Clusters", 
      sub= "data: fibulae Szentlőrinc, hclust, average/UPGMA (vegan package)")
#overlaying classification in ordination can be used as cross-check: 
#if the clusters look distinct in the ordniation diagramm, then probably 
#both analyses were adequate 
#one possibly needs 3 or more axes to display the multivariate class structure

    #FIGURE 17 - SERIATION OF THE FIBULAE WITH CLUSTER ANALYSIS ####
taxontree <- hclust(taxa2dist(fibulae))
plotree <- hclust(vegdist(fibulae), "average")
sptree <- hclust(vegdist(t(fibulae), "raup"), "average")
tabasco(fibulae, plotree, sptree, col = "darkslategray4")

                  # FIGURE 21 - CORRELATION PLOT####

#read data
graves_nom <- read.csv("Grave_Artefacts_GO_all_no multi_coded.csv",
                              header = TRUE, sep = ",", row.name=1)
names(graves_nom)

#all graves with all information (graves with more than 1 artifact), 
#except the multiple mixed graves, to get a clear picture of the co-occurence 
#of artifacts in female, male and with unknown gendered graves

#booleanize the infornmation
graves_nom <- quantAAR::booleanize(graves_nom)
#make a dataframe out of it
SZL <- data.frame(graves_nom)

#calculate the chi2 values for the dataset with chi2 = 1, to see the absoulte occurences
chi2 <- varnastats::corrmat(SZL, method = "chi2", dim = 1, chi2limit = 1.0)
#remove the negative values to have a clearer picture
corr_no_neg <- varnastats::rmnegcorr(chi2, SZL, niv = 0.1, dim = 1)

#chi2 1.0
#the clustered absolute correlations between artifacts, gender and orientation
col2 <- grDevices::colorRampPalette(c("darkslategray4", "white", "salmon4")) (20)

corrplot::corrplot(
  t(corr_no_neg),
  method = c("color"),
  tl.col="darkslategray4",
  col = col2,
  order =c("hclust"),
  hclust.method = c("average")
  #title ="Correlationplot of Szentloerinc (no multigraves) - Chi2 1"
)

                  # FIGURE 22 - NETWORK ANALYSIS####
#read data
graves_nom <- read.csv("Grave_Artifacts_no_multi_network.csv",
                       header = TRUE, sep = ",", row.name=1)
names(graves_nom)

#it's the same dataset as for Figure 21, but without coding

#booleanize the infornmation
graves_nom <- quantAAR::booleanize(graves_nom)
#make a dataframe out of it
SZL <- data.frame(graves_nom)

#calculate the chi2 values for the dataset with chi2 = 1, to see the absoulte occurences
chi2 <- varnastats::corrmat(SZL, method = "chi2", dim = 1, chi2limit = 1.0)
#remove the negative values to have a clearer picture
corr_no_neg <- varnastats::rmnegcorr(chi2, SZL, niv = 0.1, dim = 1)

#Correlation with all artifacts on the basis of the Chi2
sign_corr_chi2 <- varnastats::reltable(corr_no_neg, chi2)
sign_corr_chi2 <- dplyr::filter(sign_corr_chi2, corrvalue2 == TRUE)

#instpect the prepared dataframe!
sign_corr_chi2
#245 observations

#we can also write it out in a file if we want to use if later 
#or not want to repeat the calculations - if you'll come back to the file later!
setwd(path_RESULTS)
write_csv(sign_corr_chi2, "sign_corr_chi2_no_multi.csv")

#Plotting the Network of bivariate realtionship in the dataframe#
#with the igraph-package

#chi2=1
signicorrmod_chi1_n <- data.frame(
  from = sign_corr_chi2$namevar1, 
  to = sign_corr_chi2$namevar2, 
  weight = sign_corr_chi2$corrvalue)

graphbasis <- igraph::graph.data.frame(signicorrmod_chi1_n, directed = TRUE)
igraph::plot.igraph(graphbasis,
                    vertex.size=4,
                    vertex.color= "darkslategray4",
                    vertex.label.color = "salmon4",
                    vertex.label.cex = 1,
                    vertex.label.font = 2,
                    vertex.label.dist = 2)
#this is the network of all artifacts

#BUT: WE ARE INTERESTED IN THE RELATIONSHIP OF GENDER AND CORRELATING ARTIFACTS

# we can filter the gendered artifacts with dplyr 
gender_related_artifacts <- sign_corr_chi2 %>%
                            filter(namevar2 %in% c("F", "M", "U"))

#write the file out for later use
setwd(path_DATA)
write_csv(gender_related_artifacts, "sign_corr_chi2_no_multi.csv")

#change the format into a graph-table
signicorrmod_chi2_gender_g <- data.frame(
  from = gender_related_artifacts$namevar1, 
  to = gender_related_artifacts$namevar2)

#plot the graph
graphbasis <- igraph::graph.data.frame(signicorrmod_chi2_gender_g, directed = FALSE)
igraph::plot.igraph(graphbasis,
                    vertex.size=10,
                    vertex.color= "darkslategray4",
                    vertex.label.color = "salmon4",
                    vertex.label.cex = 1,
                    vertex.label.font = 2,
                    vertex.label.dist = 2,
                    main = "Artifact Network of Gendered Graves (no multiple graves) - X2 limit 1")

#inspecting individual attributes#
#Within *signicorr* it's easy to search for the directly to an individual 
#attribute linked attributes (level 1 relations).

signicorrmod_chi2_gender

#->we can also filter the genderbound artifacts separately

#the male artifacts

male_graves <- sign_corr_chi2 %>%
               filter(namevar2 =="M")

male_graves

#indexvar1 indexvar2 corrvalue             namevar1 namevar2 corrvalue2
#1          4         2         1           inhumation        M          1
#2          5         2         1            cremation        M          1
#3          7         2         1                   WE        M          1
#4         10         2         1                 NWSE        M          1
#5         27         2         1    Iron_knife_curved        M          1
#6         28         2         1      Iron_knife_long        M          1
#7         29         2         1 Iron_spearhead_short        M          1
#8         30         2         1  Iron_spearhead_long        M          1
#9         31         2         1      Iron_belt_clasp        M          1
#10        32         2         1   Iron_belt_buckle_c        M          1
#11        34         2         1           Buttons_FE        M          1
#12        35         2         1              Ring_FE        M          1
#13        40         2         1      Spher_bowl_Omph        M          1

# we can filter the female artifacts

female_graves <- sign_corr_chi2 %>%
                 filter(namevar2 =="F")
female_graves

#indexvar1 indexvar2 corrvalue          namevar1 namevar2 corrvalue2
#1          4         1         1        inhumation        F          1
#2          5         1         1         cremation        F          1
#3          7         1         1                WE        F          1
#4          8         1         1                EW        F          1
#5         14         1         1       Glass_beads        F          1
#6         15         1         1       Amber_beads        F          1
#7         17         1         1 Spindlewhorl_clay        F          1
#8         21         1         1     Certosa_XIIIc        F          1
#9         22         1         1     Certosa_XIIIh        F          1
#10        23         1         1        Certosa_FE        F          1
#11        25         1         1            EAZ_FE        F          1
#12        27         1         1 Iron_knife_curved        F          1

#we can filter the artifacts of unknown gender

ungendered_graves <- sign_corr_chi2 %>%
                     filter(namevar2 =="U")
ungendered_graves

#indexvar1 indexvar2 corrvalue                namevar1 namevar2 corrvalue2
#1          7         3         1                      WE        U          1
#2         13         3         1                    SENW        U          1
#3         19         3         1                Iron_awl        U          1
#4         27         3         1       Iron_knife_curved        U          1
#5         29         3         1    Iron_spearhead_short        U          1
#6         34         3         1              Buttons_FE        U          1
#7         36         3         1 Biconical_1_handled.mug        U          1
#8         41         3         1  Sph_bowl_Omph_retr_rim        U          1
#9         44         3         1         One_handled_cup        U          1
#10        45         3         1                     Pot        U          1

#FIGURE 23 - MALE GRAVES####

#read data
male <- read.csv("male_graves.csv",header = TRUE, sep = ",", row.name=1)
names(male)

taxontree <- hclust(taxa2dist(male))
plotree <- hclust(vegdist(male), "average")
sptree <- hclust(vegdist(t(male), "raup"), "average")
tabasco(male, plotree, sptree, col = "deepskyblue4")

#FIGURE 24 - FEMALE GRAVES####

female <- read.csv("female_graves.csv", header = TRUE, sep = ",", row.name=1)
names(female)

taxontree <- hclust(taxa2dist(female))
plotree <- hclust(vegdist(female), "average")
sptree <- hclust(vegdist(t(female), "raup"), "average")
tabasco(female, plotree, sptree, col = "palevioletred4")
