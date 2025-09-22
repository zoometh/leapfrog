library(sp)
library(raster)
library(dplyr)
library(openxlsx)

########################################################
# prepare les données pour l'appli Leapfrog & son audit
########################################################
# auteurs:
#   - application: Thomas Huet <thomashuet7@gmail.com>
#   - base de données: Didier Binder <didier.binder@cepam.cnrs.fr>
# date:
#   - aout 2021
# institution:
#   - CEPAM-CNRS, UMR 7264 <https://www.cepam.cnrs.fr/>
########################################################

# tableau des périodes
PN_1A <- c(-7050, -6850)
PN_1B <- c(-6850, -6650)
PN_1C <- c(-6650, -6500)
PN_2A <- c(-6500, -6350)
PN_2B <- c(-6350, -6200)
PN_2C <- c(-6200, -6050)
PN_3A <- c(-6050, -5900)
PN_3B <- c(-5900, -5750)
PN_3C <- c(-5750, -5600)
PN_3D <- c(-5600, -5450)
PN_4A <- c(-5450, -5300)
PN_4B <- c(-5300, -5150)
PN_4C <- c(-5150, -5000)
PN_4D <- c(-5000, -4850)
PN_5 <- c(-4850, -4050)
periodes_df <- as.data.frame(cbind(PN_1A,PN_1B,PN_1C,
                                   PN_2A,PN_2B,PN_2C,
                                   PN_3A,PN_3B,PN_3C,PN_3D,
                                   PN_4A,PN_4B,PN_4C,PN_4D,
                                   PN_5))
rownames(periodes_df) <- c("tpq","taq")

# cultures - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
# correspondances entre les cultures et leur couleur d'affichage (hexadecimal)
cultures <- read.xlsx("cultures.xlsx", skipEmptyRows=TRUE)
cultures <- cultures[!is.na(cultures$hexa) & !is.na(cultures$code_aspect), ] 

fperiodes <- function(df){
  for (i in seq(1, nrow(df))){
    dates <- periodes <- c()
    if (df[i,"PN_1A"] != 'NA') {
      dates <- c(dates, periodes_df$PN_1A)
      periodes <- c(periodes, "PN_1A")
    }
    if (df[i,"PN_1B"] != 'NA') {
      dates <- c(dates, periodes_df$PN_1B)
      periodes <- c(periodes, "PN_1B")
    }
    if (df[i,"PN_1C"] != 'NA') {
      dates <- c(dates, periodes_df$PN_1C)
      periodes <- c(periodes, "PN_1C")
    }
    if (df[i,"PN_2A"] != 'NA') {
      dates <- c(dates, periodes_df$PN_2A)
      periodes <- c(periodes, "PN_2A")
    }
    if (df[i,"PN_2B"] != 'NA') {
      dates <- c(dates, periodes_df$PN_2B)
      periodes <- c(periodes, "PN_2B")
    }
    if (df[i,"PN_2C"] != 'NA') {
      dates <- c(dates, periodes_df$PN_2C)
      periodes <- c(periodes, "PN_2C")
    }
    if (df[i,"PN_3A"] != 'NA') {
      dates <- c(dates, periodes_df$PN_3A)
      periodes <- c(periodes, "PN_3A")
    }
    if (df[i,"PN_3B"] != 'NA') {
      dates <- c(dates, periodes_df$PN_3B)
      periodes <- c(periodes, "PN_3B")
    }
    if (df[i,"PN_3C"] != 'NA') {
      dates <- c(dates, periodes_df$PN_3C)
      periodes <- c(periodes, "PN_3C")
    }
    if (df[i,"PN_3D"] != 'NA') {
      dates <- c(dates, periodes_df$PN_3D)
      periodes <- c(periodes, "PN_3D")
    }
    if (df[i,"PN_4A"] != 'NA') {
      dates <- c(dates,periodes_df$PN_4A)
      periodes <- c(periodes, "PN_4A")
    }
    if (df[i,"PN_4B"] != 'NA') {
      dates <- c(dates, periodes_df$PN_4B)
      periodes <- c(periodes, "PN_4B")
    }
    if (df[i,"PN_4C"] != 'NA') {
      dates <- c(dates, periodes_df$PN_4C)
      periodes <- c(periodes, "PN_4C")
    }
    if (df[i,"PN_4D"] != 'NA') {
      dates <- c(dates,periodes_df$PN_4D)
      periodes <- c(periodes, "PN_4D")
    }
    if (df[i,"PN_5"] != 'NA') {
      dates <- c(dates, periodes_df$PN_5)
      periodes <- c(periodes, "PN_5")
    }
    df[i,"tpq"] <- min(dates)
    df[i,"taq"] <- max(dates)
    df[i,"tpq.per"] <- min(periodes)
    df[i,"taq.per"] <- max(periodes)
  }
  return(df)
}

# entites - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
n.file <- "entites.xlsx"
a.file <- paste0(getwd(), "/", n.file)
if (file.exists(a.file)){
  print(paste0("donnees depuis '", n.file, "'"))
} else {print(paste0("Erreur: le fichier '", n.file, "' n'est pas retrouvé"))}
df.tot <- openxlsx::read.xlsx(a.file,
                              skipEmptyRows = TRUE)
df.tot$lbl <- NA
df.tot[is.na(df.tot)] <- 'NA'
df.tot <- fperiodes(df.tot) # calcule tpq/taq BC
df.tot$tpq[df.tot$tpq == 'NA'] <- Inf
df.tot$taq[df.tot$taq == 'NA'] <- -Inf
df.tot <- df.tot[!(df.tot$tpq == Inf), ] # rm
df.tot$tpq <- as.numeric(df.tot$tpq)
df.tot$taq <- as.numeric(df.tot$taq)

# fiabilite - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
df.tot$fiabilite_context[df.tot$fiabilite_context %in% c("autre_pas_infos", "NA")] <- "autre_pas_infos/NA"
df.tot$fiabilite_dates[df.tot$fiabilite_dates %in% c("mediocre", "NA")] <- "mediocre/NA"
df.tot$fiabilite_key <- paste0(df.tot$fiabilite_context, '_', df.tot$fiabilite_dates)
# jointure
df.fiab <- openxlsx::read.xlsx(paste0(getwd(),"/fiabilites.xlsx"),
                               skipEmptyRows = TRUE)
df.fiab$fiabilite_key <- paste0(df.fiab$fiabilite_context, '_', df.fiab$fiabilite_dates)
df.fiab$fiabilite_dates <- df.fiab$fiabilite_context <- NULL
df.tot <- merge(df.tot, df.fiab, by="fiabilite_key", all.x=T)
df.tot$fiabilite_key <- NULL
val.resum <- unique(df.tot$valeur_resume)

# période
lper <- colnames(periodes_df)
dper <- list(PN_1A, PN_1B, PN_1C,
             PN_2A, PN_2B, PN_2C,
             PN_3A, PN_3B, PN_3C, PN_3D, 
             PN_4A, PN_4B, PN_4C, PN_4D,
             PN_5)

# actualise les taq/tpq du tableau a partir des périodes
for (r in seq(1:nrow(df.tot))){
  ldate <- c()
  for (c in lper){
    if (df.tot[r,c] != 'NA'){
      idx_colnme <- match(c,lper) # l'index de la période représentée
      ldate <- c(ldate, as.vector(dper[[idx_colnme]]))
    }
  }
  df.tot[r,"tpq"] <- min(ldate)
  df.tot[r,"taq"] <- max(ldate)
}
# réarrange les colonnes
refcols <- c("site", "type_site", "culture", "cul_descr",
             lper,
             "tpq", "taq",
             "x", "y", "altitude",
             "valeur_resume", "fiabilite_resume",
             "fiabilite_context", "fiabilite_dates", "fiabilite_coord",
             "commune", "departement", "region", "pays")
df.tot <- df.tot[, c(refcols, setdiff(names(df.tot), refcols))]
# supprime certaines colonnes pour la table dynamique
drops.columns <- c("couleur", "lbl", "tpq.per", "taq.per",
                   "fiabilite_context_eng", "fiabilite_dates_eng",
                   "valeur_context", "valeur_datation",
                   "alpha", "contour",
                   "idf")
# labels
df.tot$lbl <- NA
for (i in seq(1,nrow(df.tot))){
  desc <- paste0("<b>", df.tot[i,"site"], "</b> <font size='-2'>", df.tot[i,"type_site"], "</font><br>",
                 "<span style='color: ", df.tot[i,"couleur"],";'><b>", df.tot[i,"culture"], "</b></span>  ",df.tot[i,"cul_descr"],"<br>",
                 df.tot[i,"tpq.per"]," - ", df.tot[i,"taq.per"], " | ", df.tot[i,"tpq"]," / ", df.tot[i,"taq"]," BC<br>",
                 "fiab.resume: ", df.tot[i,"fiabilite_resume"]," (", df.tot[i,"valeur_resume"], ")<br>",
                 "<font size='-2'>",
                 "fiab.context.: ", df.tot[i,"fiabilite_context"]," | fiab.datat.: ", df.tot[i,"fiabilite_dates"],"<br>",
                 "fiab.coord: ", df.tot[i,"fiabilite_coord"],
                 "</font>"
  )
  df.tot[i,"lbl"]  <- desc
}
# nettoyage du tableau
df.tot[,"site"] <- as.character(df.tot[,"site"])
df.tot <- df.tot[df.tot$culture != "Ressources",]
df.tot <- df.tot[df.tot$culture != "Indetermine",]
df.tot <- df.tot[!is.na(df.tot$PN_1A) |
                   !is.na(df.tot$PN_1B) |
                   !is.na(df.tot$PN_1C) |
                   !is.na(df.tot$PN_2A) |
                   !is.na(df.tot$PN_2B) |
                   !is.na(df.tot$PN_2C) |
                   !is.na(df.tot$PN_3A) |
                   !is.na(df.tot$PN_3B) |
                   !is.na(df.tot$PN_3C) |
                   !is.na(df.tot$PN_3D) |
                   !is.na(df.tot$PN_4A) |
                   !is.na(df.tot$PN_4B) |
                   !is.na(df.tot$PN_4C) |
                   !is.na(df.tot$PN_4D) |
                   !is.na(df.tot$PN_5),]
df.tot$idf <- seq(1, nrow(df.tot))

# filtre sur les cultures existantes
cultures <- cultures[cultures$code_aspect %in% df.tot$culture, ]
lcul_col <- as.list(cultures$hexa)
lcul_col <- lapply(lcul_col,toupper)
names(lcul_col) <- cultures$code_aspect

# couleurs
myColors <- c()
for (i in names(lcul_col)){
  myColors <- c(myColors,as.character(lcul_col[i]))
}
df.tot$couleur <- NA
for (i in seq(1:nrow(df.tot))){
  df.tot[i,"couleur"]  <- toupper(as.character(lcul_col[df.tot[i, "culture"]]))
}

# symbologie des entités sur la fiabilité (alpha et contour)
df.tot$alpha <- df.tot$contour <- NA
df.tot$alpha[df.tot$valeur_resume == 3] <- 1
df.tot$contour[df.tot$valeur_resume == 3] <- 2
df.tot$alpha[df.tot$valeur_resume == 2] <- 0.2
df.tot$contour[df.tot$valeur_resume == 2] <- 1.5
df.tot$alpha[df.tot$valeur_resume == 1] <- 0
df.tot$contour[df.tot$valeur_resume == 1] <- 1.5

# schéma spatial
xy <- list(longitude=c(as.numeric(df.tot$x)),
           latitude=c(as.numeric(df.tot$y)))
df.tot.sp <- SpatialPointsDataFrame(coords = xy,
                                    data = df.tot,
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))

