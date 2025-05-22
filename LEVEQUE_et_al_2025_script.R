#Script for LEVEQUE et al. figures

#loading packages
library(vegan)
library(viridis)
library('lme4')
library ("car")
library("ggplot2")
library(basicPlotteR)
library(SRS)
library(munsell)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)

#########################
####FIGURE 2 SCRIPT #####
#########################

#loading dataset for figure 2
dat<-read.csv("matrix_dataset_symb_species.csv",header=T,row.names=1, sep=";",dec=",")
#loading metadataset
env_dat<-read.csv("matrix_dataset_env_para.csv",header=T, sep=";",dec=",")

dat
env_dat
##Conversion of data as numeric and/or factorial attributes to fit with functions requirements.
##parameters
## as.factor()
env_dat$SST_intervals<-as.factor(env_dat$SST_intervals)
env_dat$SST_intervals_V2<-as.factor(env_dat$SST_intervals_V2)
env_dat$SST_intervals_V3<-as.factor(env_dat$SST_intervals_V3)
env_dat$bleaching_year<-as.factor(env_dat$bleaching_year)
env_dat$severe.bleaching.events<-as.factor(env_dat$severe.bleaching.events)
env_dat$SST2intervals<-as.factor(env_dat$SST2intervals)
## as.numeric()
env_dat$nb.symb.species<- as.numeric(env_dat$nb.symb.species)
env_dat$Year<-as.numeric(env_dat$Year)
env_dat$DHM<-as.numeric(env_dat$DHM)
env_dat$nb.of.records<-as.numeric(env_dat$nb.of.records)
env_dat$X.50records<-as.numeric(env_dat$X.50records)
env_dat$nb.severe...30...bleaching<-as.numeric(env_dat$nb.severe...30...bleaching)
env_dat$nb.moderate..30...bleaching<-as.numeric(env_dat$nb.moderate..30...bleaching)

## Check on env_dat data
str(env_dat)

#### Stacked barplot on the subclades levels displaying first number of records and second proprtions#### 
## Here use the "dat" dataset

stackbplot_bis1<-dat

####barplot with subclade proportions per year + another barplot at the top with the number of records for each year####
# create a dataset for the nb of records barplot #
year<- c(1994,1995,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)
nb_rec<- c(5,0,1,91,8,5,19,109,24,194,68,95,1,7)
rec_data<- data.frame(nb_rec)
rec_data

dev.off()

#defining plot window limit for figure 2 (a)
par(fig=c(0,1,0.76,1), mar = c(0.1,5,0.5,9), new=TRUE, xpd= TRUE)
#plotting figure 2 (a)
barplot(t(rec_data),beside=F,col=c("black"),ylab="nb of records",las=1,horiz=F,cex.lab=1.15,
        
        ylim=c(0,200),xlim=c(0,18),space=0.2)
text(18,180,"(a)",cex=1)
#legend(x="right",inset=c(-.01,.4), legend=c("nb of records"), cex=0.6,fill=c("orange"),bty="n")
years<-c(1994,1995,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)

#defining plot window limit for figure 2 (b)
par(fig=c(0,1,0,0.75), mar = c(2,5,0.5,9), new=TRUE, xpd= TRUE)
#color palette setup
coltotv4<-c("#E69F00","#009E73","#0072B2","#D55E00","#CC79A7","grey")
coltotv5<-c("grey","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7",
            "#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00",
            "#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#E69F00",
            "#009E73")

coltotv5<-c("grey","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7","#CC79A7",
            "#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00",
            "#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00","#D55E00",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2","#0072B2",
            "#E69F00",
            "#E69F00")

#plotting figure 2(b)
barplot((prop.table(t(rev(stackbplot_bis1)),margin=2)),
        beside=F,
        border=NA,
        col=rev(coltotv5),
        ylab="Symbiodiniaceae data proportions",
        names=years,las=1,horiz=F,
        xlab="Years",
        ylim=c(0,1),
        cex.names = 1.2,cex.lab=1.15,
        xlim=c(0,18),
        space=0.2)
text(18,.9,"(b)",cex=1)

#legend color palette
coltotv4<-c("#E69F00","#0072B2","#D55E00","#CC79A7","grey")
#plotting legend
legend(x="right", ncol=1, inset=c(-0.075, .4),
       legend=expression(italic("Durusdinium"), italic("Symbiodinium"), 
                         italic("Breviolium"), italic("Cladocopium"), italic("Unidentified")),
       cex=1.2, fill=coltotv4, bty="n")

#########################
####FIGURE 3 SCRIPT #####
#########################

#loading dataset for figure 3
dat<-read.csv("genbank_host_genus_data.csv",header=T,row.names=1,sep=";",dec=",")
dat

#color palette setup
col<-viridis(23)

years<-c(1994,1995,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)

# Add extra space to right of plot area; change clipping to figure
par(mar=c(4, 4, 2, 8), xpd=TRUE)

# transpose data set to set correctly the data for the proportion stackdbarplot a the sublclades level
tdat<-t(dat)

####Barplot with host genera proportions per year + another barplot at the top with the number of records for each year####
# create a dataset for the nb of records barplot #
year<- c(1994,1995,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)
nb_rec<- c(5,0,1,91,8,5,19,109,24,194,68,95,1,7)
rec_data<- data.frame(nb_rec)
rec_data

####combine both barplots informations#####

dev.off()
#defining plot window limit for figure 3 (a)
par(fig=c(0,1,0.76,1), mar = c(0.1,5,0.5,9), new=TRUE, xpd= TRUE)
#plotting figure 3 (a)
barplot(t(rec_data),beside=F,col=c("black"),ylab="nb of records",las=1,horiz=F,cex.lab=1.15,
        
        ylim=c(0,200),xlim=c(0,17),space=0.2)
text(19,180,"(a)",cex=1)
#legend(x="right",inset=c(-.01,.4), legend=c("nb of records"), cex=0.6,fill=c("orange"),bty="n")

#defining plot window limit for figure 3 (b)
par(fig=c(0,1,0,0.75), mar = c(2,5,0.5,9), new=TRUE, xpd= TRUE)

#plotting figure 3 (b) + plotting legend
tdat<-t(dat)
barplot((prop.table(tdat,margin=2)),
        beside=F,
        col=col,
        ylab="Scleractinian host genera proportions",
        names=years,
        las=1,
        horiz=F,
        xlab="Years",
        ylim=c(0,1),
        cex.names = 1.2,
        cex.lab=1.15,
        xlim=c(0,17),
        space=0.2)
text(19,1,"(b)",cex=1)
legend(x = "right", ncol = 1, inset = c(-0.1, -0.1),
       legend = expression("Unknown", italic("Acropora"), italic("Agaricia"), italic("Cladocora"),
                           italic("Colpophyllia"), italic("Dendrogyra"), italic("Dichocoenia"),
                           italic("Diploria"), italic("Eusmilia"), italic("Favia"),
                           italic("Isophyllia"), italic("Leptoseris"), italic("Madracis"),
                           italic("Manicina"), italic("Meandrina"), italic("Montastrea"),
                           italic("Mussa"), italic("Mycetophyllia"), italic("Orbicella"),
                           italic("Porites"), italic("Scolymia"), italic("Siderastrea"),
                           italic("Stephanocoenia")),
       cex = 1.2, fill = col, bty = "n")


#########################
####FIGURE 4 SCRIPT #####
#########################

#Focus on Hosts harboring Durusdinium spp. across time
#loading dataset for figure 4
Dudat<-read.csv("genbank_host_genus_Durusdinium_data.csv",header=T,row.names=1,sep=";",dec=",")
Dudat

#color palette setup
Ducol<-viridis(14)
Ducol<- c("#471264FF","#482173FF","#433E85FF","#32648EFF","#3BBB75FF","#85D54AFF","#C2DF23FF","#E0E318FF")

#years setup for plotting
years<-c(1999,2000,2001,2002,2003,2004,2005,2006,2007)
years<-c(1994,1995,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)

# Add extra space to right of plot area; change clipping to figure
par(mar=c(4, 4, 2, 8), xpd=TRUE)

years<-c(1994,1995,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009)

# Transpose the data matrix
tDudat <- t(Dudat)

#plotting figure 4
bar_positions <- barplot(
  prop.table(tDudat, margin = 2), # Convert to proportions column-wise
  beside = FALSE, # Bars are stacked
  col = Ducol, # Colors for bars
  ylab = "Scleractinian host genera proportions harboring Durusdinium", # Y-axis label
  names.arg = years, # Names for the X-axis (years)
  las = 1, # Orientation of axis labels
  horiz = FALSE, # Vertical bars
  xlab = "Years", # X-axis label
  ylim = c(0, 1), # Y-axis limits
  cex.names = 1.2, # Size of axis labels
  cex.lab=1.15,
  xlim = c(0, 18), # X-axis limits
  space = 0.2, # Space between bars
  border = NA # No borders on bars
)

#plotting legend
legend(
  x = "right", 
  ncol = 1, 
  inset = c(-0.1, 0), 
  legend = expression(
    italic("Acropora"), italic("Agaricia"), italic("Colpophyllia"), italic("Diploria"),
    italic("Montastrea"), italic("Orbicella"), italic("Scolymia"), italic("Siderastrea")
  ), 
  cex = 1.5, 
  fill = Ducol, 
  bty = "n"
)


#plotting "NA" text for years missing data
na_years <- c(1994, 1995,1998, 2008, 2009)

# Add "NA" text where there are no bars for specific years
proportions <- prop.table(tDudat, margin = 2)
for (i in 1:ncol(proportions)) {
  if (years[i] %in% na_years) {
    cum_y <- 0  # Cumulative y-position for stacked bars
    for (j in 1:nrow(proportions)) {
      if (is.na(proportions[j, i]) || proportions[j, i] == 0) {
        text(
          x = bar_positions[i], # X position of the text
          y = cum_y + 0.5 * (1 - cum_y), # Dynamic Y position of the text
          labels = "NA", # Text label
          cex = 1.2, # Size of the text
          col = "black" # Color of the text, adjust as needed
        )
      } else {
        cum_y <- cum_y + proportions[j, i]  # Increment cumulative y-position
      }
    }
  }
}


#########################
####FIGURE 5 SCRIPT #####
#########################

##### Dataset import for all coral species from 1994 to 2009
##### + environmental parameters
dat<-read.csv("matrix_dataset_symb_species.csv",header=T,row.names=1, sep=";",dec=",")
dat

##### Method "Scaling with ranked subsampling (SRS)" function on the dataset (all coral species from 1994 to 2009)
## following Beule & Karlovsky 2020 paper
## first transpose dataset to fit in the requirement of SRS function
## then convert it as dataframe
dat<- t(dat)
dat
dat <- as.data.frame(dat)

##Selection of the desired number of records 
##Cmin : the number of records which all samples will be normalized
Cmin <- min(colSums(dat))
Cmin<- 14
Cmin

##Running the SRS function
SRS_output <- SRS(dat, Cmin)
SRS_output

##Samples that have a total number of counts < Cmin will be discarded:
SRS_output <- SRS(dat, Cmin+1)
SRS_output

## Extract Symbiont species labels from initial dataset
symbiont_names <- rownames(dat) 
symbiont_names
## Add symbiont species labels in the rownames in SRS output 
row.names(SRS_output)<-symbiont_names
SRS_output

## Transpose datset to get the initial dataset format 
SRS_output<-t(SRS_output)
SRS_output
View(SRS_output)

# Extract dataset from the SRS normalization
# Write data to csv files:  
# decimal point = "." and value separators = comma (",")
#write.csv(SRS_output, file = "SRS_Cmin_14_matrix_symb_species.csv")


#loading dataset for figure 5
dat<-read.csv("SRS_Cmin_14_matrix_symb_species_Copy_2.csv",header=T,row.names=1,sep=";",dec=",")
#loading metadataset
env_dat<-read.csv("matrix_dataset_env_para.csv",header=T, sep=";",dec=",")

dat
env_dat

# subset from 2003 to 2007 around the 2005 mass bleaching event in the Caribbean region for SRS dataset and environmental parameters
dat<-subset(dat, rownames(dat) %in% c("2003","2004","2005","2006","2007"))
env_dat<-subset(env_dat, rownames(env_dat) %in% c(8,9,10,11,12))

# subset with only Durusinium species and Symbiodiniaceae from the other genus to highlight the overtake of Durusdinium
dat<-subset(dat, select = c("D1","D1a","Symbiodinium","Breviolium","Cladocopium"))
#env_dat<-subset(env_dat, rownames(env_dat) %in% c(8,9,10,11,12))

##Conversion of data as numeric and/or factorial attrivbutes to fit with functions requirements.
##parameters
## as.factor()
env_dat$SST_intervals<-as.factor(env_dat$SST_intervals)
env_dat$SST_intervals_V2<-as.factor(env_dat$SST_intervals_V2)
env_dat$SST_intervals_V3<-as.factor(env_dat$SST_intervals_V3)
env_dat$bleaching_year<-as.factor(env_dat$bleaching_year)
env_dat$severe.bleaching.events<-as.factor(env_dat$severe.bleaching.events)
env_dat$SST2intervals<-as.factor(env_dat$SST2intervals)
## as.numeric()
env_dat$Year<-as.numeric(env_dat$Year)
env_dat$DHM<-as.numeric(env_dat$DHM)
env_dat$nb.of.records<-as.numeric(env_dat$nb.of.records)
env_dat$X.50records<-as.numeric(env_dat$X.50records)
env_dat$nb.severe...30...bleaching<-as.numeric(env_dat$nb.severe...30...bleaching)
env_dat$nb.moderate..30...bleaching<-as.numeric(env_dat$nb.moderate..30...bleaching)
env_dat$ACE<-as.numeric(env_dat$ACE)
env_dat$total.nb.bleaching.events<- as.numeric(env_dat$total.nb.bleaching.events)
env_dat$richness.host.species<-as.numeric(env_dat$richness.host.species)
env_dat$richness.host.species.with.DUR<-as.numeric(env_dat$richness.host.species.with.DUR)
#check on data status
str(env_dat)
str(dat)

####create matrix with number of records, nb of severe bleaching events, prevalence based on Hughes et al., 2018
year<- c(2003,2004,2005,2006,2007)
nb_records<- c(15,15,15,15,15)
nb_b_events<- c(10,5,20,7,5)
prevalence_b_perc<- c(10,0,70,14.29,0)
prevalence_b<- c(1,0,14,1,0)
moderate_b_events<- c(9,5,6,6,5)

# transpose data set to set correctly the data for the proportion stackdbarplot a the sublclades level
dat<-t(dat)

#coltot<- viridis(5)
#coltot<- c("#453781FF","#31688EFF","#35B779FF")
coltot<-c("#440154FF","#3B528BFF","#21908CFF","#5DC863FF","#B4DE2CFF")
palette.colors(palette = "Okabe-Ito")
coltot<-c("#E69F00","#009E73","#0072B2","#D55E00","#CC79A7")
coltot<-c("#E69F00","#E69F00","#0072B2","#D55E00","#CC79A7")
bdata <- data.frame(prevalence_b,moderate_b_events)
#bdata <- data.frame(year,nb_records,nb_b_events,prevalence_b_perc,prevalence_b)
bdata


##### Stacked barplot displaying nb of bleaching events and nb of severe bleaching events
dev.off()
#defining plot window limit for figure 5(a)
par(fig=c(0,1,0.76,1), mar = c(0.1,5,0.5,2), new=TRUE)

#plotting figure 5(a) + legend
barplot(t(bdata),
        beside=F,
        col=c("red3","orange"),
        ylab="nb of bleaching events",
        las=1,
        horiz=F,
        border=NA,
        cex.lab=1.15,
        ylim=c(0,20),
        xlim=c(0,7),
        space=0.2)
legend(x="right",
       inset=c(0,.4),
       legend=c("Severe>30%","Moderate<30%"),
       cex=1.2,
       fill=c("red3","orange"),
       bty="n")
text(7,18,"(a)",cex=1)

years<-c(2003,2004,2005,2006,2007)

#defining plot window limit for figure 5(b)
par(fig=c(0,1,0,0.75), mar = c(2,5,0.5,2), new=TRUE)

#plotting figure 5(b) + legend
barplot((prop.table(dat,margin=2)),
        beside=F,
        col=coltot,
        border=NA,
        ylab="Symbiodiniaceae SRS data proportions",
        names=years,
        las=1,
        horiz=F,
        xlab="Years",
        ylim=c(0,1),
        cex.names = 1.2,
        cex.lab=1.15,
        xlim=c(0,7),
        space=0.2)

legend(x = "right",
       ncol = 1,
       inset = c(0.01, 0.4),  # or c(-0.1, 0.4) for further out
       legend = expression(italic("Durusdinium"), italic("Symbiodinium"),
                           italic("Breviolium"), italic("Cladocopium")),
       cex = 1.2,
       fill = c("#E69F00", "#0072B2", "#D55E00", "#CC79A7"),
       bty = "n")

text(7,.95,"(b)",cex=1)

#########################
####FIGURE 6 SCRIPT #####
#########################

#loading dataset for figure 6
dat<-read.csv("SRS_Cmin_14_matrix_symb_species.csv",header=T,row.names=1, sep=",",dec=",")
#loading metadataset
env_dat<-read.csv("matrix_dataset_env_para.csv",header=T, sep=";",dec=",")

#change name of vectors for clarity in DCA plot later
names(env_dat)
names(env_dat)[names(env_dat) == "prevalence.of.severe.bleaching.events"] <- "SBE"
names(env_dat)[names(env_dat) == "total.nb.bleaching.events"] <- "BE"
names(env_dat)[names(env_dat) == "ACE"] <- "H"

# subset from 2003 to 2007 around the 2005 mass bleaching event in the Caribbean region for SRS dataset and environmental parameters
dat<-subset(dat, rownames(dat) %in% c("2003","2004","2005","2006","2007"))
dat<-subset(dat, select= c("A13","A3","A4","A4.1","A4.3","A4a","A4a.1","B1","B10","B17","B1j","B5","B5a","B6","B7","C1",
                           "C11","C12","C3","C3a","C4","C7",
                           "C7a","C82","D1","D1a","unknown_s"))
env_dat<-subset(env_dat, rownames(env_dat) %in% c(8,9,10,11,12))

attach(env_dat)

rowSums(dat)
#Detrended correspondence analysis
DCA.dat <-decorana(dat)
# Extract scores
site_scores <- as.data.frame(scores(DCA.dat, display = "sites"))
site_scores$Year <- rownames(site_scores)
site_scores$Bleaching <- c("Moderate", "Low", "Severe", "Moderate", "Low")  # Adjust as needed

species_scores <- as.data.frame(scores(DCA.dat, display = "species"))
species_scores$Taxon <- rownames(species_scores)

# Assign Symbiodiniaceae groups
species_scores <- species_scores %>%
  mutate(Group = case_when(
    Taxon %in% c("D1", "D1a") ~ "Durusdinium",
    Taxon %in% c("A13","A3","A4","A4.1","A4.3","A4a","A4a.1") ~ "Symbiodinium",
    Taxon %in% c("B1","B10","B17","B1j","B5","B5a","B6","B7") ~ "Breviolum",
    Taxon %in% c("C1","C11","C12","C3","C3a","C4","C7","C7a","C82") ~ "Cladocopium",
    TRUE ~ "Other"
  ))

# Fit environmental vectors
fit_env <- envfit(DCA.dat, env_dat, permutations = 999)
arrows_df <- as.data.frame(scores(fit_env, display = "vectors"))
arrows_df$Variable <- rownames(arrows_df)

# Subset environmental variables
selected_vars <- c("ErSST.data", "OiSST.data", "Coral.Temp.SST", "H", "BE", "SBE")
arrows_subset <- subset(arrows_df, Variable %in% selected_vars)

# ---- SCALE VECTORS TO FIT IN -2 TO 2 FRAME ----
arrow_max_length <- 1.5
max_arrow_length <- sqrt(max(arrows_subset$DCA1^2 + arrows_subset$DCA2^2))
scaling_factor <- arrow_max_length / max_arrow_length

arrows_subset <- arrows_subset %>%
  mutate(
    DCA1_scaled = DCA1 * scaling_factor,
    DCA2_scaled = DCA2 * scaling_factor
  )
# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Define colors and shapes
#group_colors <- c(
# "Symbiodinium" = "#31688EFF",
#"Breviolum"    = "#453781FF",
#"Cladocopium"  = "#FDE725FF",
#"Durusdinium"  = "#35B779FF",
#"Other"        = "grey70"
#)

group_colors <- c(
  "Symbiodinium" = "#0072B2",
  "Breviolum"    = "#E69F00",
  "Cladocopium"  = "#CC79A7",
  "Durusdinium"  = "#009E73",
  "Other"        = "grey70"
)

group_shapes <- c(
  "Symbiodinium" = 16,
  "Breviolum"    = 16,
  "Cladocopium"  = 16,
  "Durusdinium"  = 16,
  "Other"        = 16
)

bleaching_colors <- c(
  "Severe" = "#D73027",
  "Moderate" = "#FC8D59",
  "Low" = "#4575B4"
)

# ---- PLOT ----
ggplot() +
  # Species points
  #geom_point(
  #data = species_scores,
  #aes(x = DCA1, y = DCA2, color = Group, shape = Group),
  #size = 4, 
  #) +
  
  geom_point(
    data = species_scores,
    aes(x = DCA1, y = DCA2, fill = Group, color = Group, shape = 21),  # use fill for interior color
    shape = 21,       # circle with fill and border
    #color = "black",  # border color
    #stroke = 0.8,     # thickness of the border
    size = 8
  ) +
  scale_fill_manual(
    name = "Symbiodiniaceae genus",
    values = group_colors,
    breaks = setdiff(names(group_colors), "Other")
  ) +
  
  # Site labels (colored manually by bleaching, no legend)
  geom_text_repel(
    data = site_scores,
    aes(x = DCA1, y = DCA2, label = Year),
    color = site_scores$Bleaching %>% recode(
      "Severe" = "#D73027", "Moderate" = "#FC8D59", "Low" = "#4575B4"
    ),
    size = 8, fontface = "bold", show.legend = FALSE
  ) +
  
  # Arrows
  geom_segment(
    data = arrows_subset,
    aes(x = 0, y = 0, xend = DCA1_scaled, yend = DCA2_scaled),
    arrow = arrow(length = unit(0.3, "cm")),
    color = "black", linewidth = 0.8
  ) +
  
  # Arrow labels on the left side of arrow tips
  geom_text(
    data = arrows_subset,
    aes(x = DCA1_scaled, y = DCA2_scaled, label = Variable),
    size = 8, color = "black",fontface = "bold",
    nudge_x = 0, nudge_y = -0.1
  ) +
  
  # Legends
  scale_color_manual(
    name = "Symbiodiniaceae genus",
    values = group_colors,
    breaks = setdiff(names(group_colors), "Other")
  ) +
  scale_shape_manual(
    name = "Symbiodiniaceae genus",
    values = group_shapes,
    breaks = setdiff(names(group_shapes), "Other")
  ) +
  
  # Axis limits
  xlim(-2, 2) +
  ylim(-2, 2) +
  
  # Labels and light background
  labs(
    x = "DCA1",
    y = "DCA2"
  ) +
  theme_light(base_size = 20) +
  theme(
    panel.background = element_rect(fill = "grey100", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.15,0.9),legend.background = element_rect(colour = "black")
  )