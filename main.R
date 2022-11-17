# loading functions source file
source("tools.R")

# Pour l'instant je charge les donnees en CSV ce qui est degueulasse
# je changerai quand j aurai acces au LISS Panel en nom propre
# en attendant je dois retravailler les données pour avoir les bons noms de colonnes
dr2 <- read.csv("cw22o_EN_1.0p.csv", header=FALSE, sep=";")
data_raw <- read.csv("cw22o_EN_1.0p.csv", header=FALSE, sep=";", col.names=dr2[1,])
data_raw <- data_raw[2:nrow(data_raw),]

# Je propose de garder essentiellement des booleans pour faciliter le
# traitement des données
# Je propose de reduire le data_set a ces colonnes :
# nomem_encr : id du menage
# cw22o525 : primary occupation
# cw22o001 : respondant has paid work ?
# cw22o005 : highest level of education completed
# cw22o013 : boolean if art education (-9 to delete) considered litt
# cw22o014 : boolean if litt education (-9 to delete) considered litt
# cw22o015 : boolean if social education (-9 to delete) considered litt
# cw22o016 : boolean if economics education (-9 to delete) considered sci
# cw22o017 : boolean if law education (-9 to delete) considered litt
# cw22o018 : boolean if mathematic education (-9 to delete) considered sci
# cw22o019 : boolean if technology education (-9 to delete) considered sci
# cw22o549 : education at level with work position (-9 to delete)
# cw22o550 : education higher level than needed for work position (-9 to delete)
# cw22o551 : education lower level than needed for work position (-9 to delete)
# ...
# cw22o128 : wages satisfaction (range: 0:10, 999 to delete)
# ... completez jai la flemme
data<-clean_data(data=data_raw, 
                 col_to_keep=c("cw22o525", "cw22o001", "cw22o005", "cw22o013", 
                               "cw22o014", "cw22o015", "cw22o016", "cw22o017",
                               "cw22o018", "cw22o019", "cw22o549", "cw22o550",
                               "cw22o551", "cw22o128"), 
                 to_delete=c(-9, " ", 999))

# We make 2 groups of people : one who attended to litt studies
# pour l'instant j'incorpore pas les économistes
litt<-data[data$cw22o013==1|data$cw22o014==1|data$cw22o015==1,]
sci<-data[data$cw22o019==1|data$cw22o018==1,]
# TODO : je pense qu'il faudrait qu'on verifie qu'on a pas un fourbe qui se trouve
# dans les deux groupes

# Pour l'instant j'ai prepare le code que pour la satisfaction des salaires
# mais faites vous plaisir avec le reste !!
# a_ prefix means on average
# sd_ prefix means standard deviation
stats_desc <- data.frame(row.names = c("a_wage_satisfaction", "sd_wage_satisfaction"
                                       ))
stats_desc$litt = c(mean(strtoi(litt$cw22o128)), sd(strtoi(litt$cw22o128)))
stats_desc$sci = c(mean(strtoi(sci$cw22o128)), sd(strtoi(sci$cw22o128)))
print(stats_desc)
print(make_an_ic(litt$cw22o128, 0.05))
print(are_means_different(sci$cw22o128, litt$cw22o128, 0.05))
hist(strtoi(sci$cw22o128), main = "Scientists")
hist(strtoi(litt$cw22o128), main = "Literati")