# This project aims to categorize over 1000 Pokemon into specific competitive tiers.
# The tier system used in this project is the crowd sourced Smogon system where 
# competitive viability is based on how likely you are to encounter a Pokemon in a given tier.
# Pokemon who do no meet this threshold are dropped to a lower tier. 
# As a community, Smogon can also decide to ban a Pokemon if it is too powerful for the tier.
# The rankings are as follows: Ubers, OUBL, OU, UUBL, UU, RUBL, RU, NUBL, NU, PUBL, PU, ZUBL, ZU
# There is a special tier for first staged evolution pokemon known as LC
# There is also a psuedo tier called NFE for the second stage pokemon of a three stage evolution line

# This project will simplify the complexities of competitive Pokemon down to Typing, Stats, and Abilities
# Movesets, Availability, and Items will not be factored into the initial model even though they 
# are known to effect the competitive rankings.

rm(list=ls()); gc()

library(dplyr)
library(fastDummies)

setwd("C:/Users/Hyrek/OneDrive/Desktop/Pokedex Project")
dat = read.csv('Pokedex.csv', head=T, stringsAsFactors=F)

# Partner Pikachu and Partner Eevee have only ever been playable in the Let's Go games
# and not in any of the other mainlines, and thus should be excluded
# Ultra Necrozma and Eternamax Eternatus are boss exclusive encounters and have never
# been available to the players and thus should be excluded too

dat <- dat[!(dat$Name %in% c("Partner Pikachu",
                             "Partner Eevee",
                             "Ultra Necrozma",
                             "Eternamax Eternatus")),]

# At the time this code was written days before the release of the SV: Indigo Disk DLC

dat <- dat[-which(is.na(dat$Dex..)),]

# There are 18 Pokemon types. It doesn't matter if a type is Primary or Secondary
# It only matters if a Pokemon has the type or not.

dat$Type = paste(dat$Primary.Type,dat$Secondary.Type,sep=',')   # Condense columns into one super column
dat <- subset(dat, select = -c(Primary.Type, Secondary.Type))

dat = dummy_columns(dat,
                     select_columns = c('Type'),                # Create Dummy Variables
                     remove_most_frequent_dummy = F,
                     remove_selected_columns = T,
                     split = (','))

# There are over 300 Abilities. It doesn't matter if its the first, second or hidden ability
# There are some Abilities that share the same effect such as Libero and Protean and Wimpout and Emergency Exit
# There are too many of these cases to address on a case by case basis, so it will be ignored for now.
# TO DO: Condense Abilities that share the same exact effect later

dat$Ability = paste(dat$Ability.1,dat$Ability.2,dat$Hidden.Ability,sep=',')
dat <- subset(dat, select = -c(Ability.1, Ability.2, Hidden.Ability))

dat = dummy_columns(dat,
                    select_columns = c('Ability'),
                    remove_most_frequent_dummy = F,
                    remove_selected_columns = T,
                    split = (','))
dat <- subset(dat, select = -c(Ability_ ))

# Other Dummy Variables 

# TO DO: Re-implement later, some of these categories had strange effects on the results
#dat = dummy_columns(dat,
#                    select_columns = c('Generation'),
#                    remove_most_frequent_dummy = F,
#                    remove_selected_columns = T)

#dat = dummy_columns(dat,
#                    select_columns = c('Starter','Psuedo.Legendary','Alolan','Galarian',
#                                       'Hisuian','Paldean','Sub.Legendary','Legendary',
#                                       'Mythical','Ultra.Beast','Paradox','Mega','Gigantamax',
#                                       'Terastal'),
#                    remove_most_frequent_dummy = T,
#                    remove_selected_columns = T)

# Can't be analyzed, remove from dataset
dat <- subset(dat, select = -c(Ranking.Gen, Dex..,Name))

# Multinominal Logistical Regression is outside of my scope of knowledge.
# TODO: Learn Techniques for data mining data with multiple classifications
# AG, Ubers, OUBL, OU, UUBL, UU, RUBL will be considered Viable
# RU, NUBL, NU, PUBL, PU, ZUBL, ZU, LC, NFE will be considered un-viable
ind = which(dat$Ranking %in% c('AG','Uber', 'OUBL', 'OU', 'UUBL', 'UU', 'RUBL'))
ind2 = which(!dat$Ranking %in% c('AG','Uber', 'OUBL', 'OU', 'UUBL', 'UU', 'RUBL'))
dat$Ranking[ind] = 1
dat$Ranking[ind2] = 0
names(dat)[names(dat) == "Ranking"] <- "Viability"
dat$Viability <- as.integer(as.character(dat$Viability))

#Check Correlation 
dev.new()
View(round(cor(dat), 2))

#Function for finding highest correlations in a large dataset
#https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
corr_simple <- function(data=dat,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  #corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple()

write.csv(dat, "cleaned_pokedex.csv")
dex =read.table("cleaned_pokedex.csv",sep=",",header=TRUE)
#mean(dat$HP, na.rm = TRUE)
#mean(dat$Attack, na.rm = TRUE)
#mean(dat$Sp.Atk, na.rm = TRUE)
#mean(dat$Defense, na.rm = TRUE)
#mean(dat$Sp.Def, na.rm = TRUE)
#mean(dat$Speed, na.rm = TRUE)
#mean(dat$BST, na.rm = TRUE)

#BST.mean.grass <- mean(dat[dat$Type_Grass == 1, 'BST'])




