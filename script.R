rm(list=ls())
gc()

# Loading packages
library(rgdal)
library(gfcanalysis)
library(data.table)

# Loading Provincias
Misiones <- readOGR("./shp", "provincias")
# filtering prov misiones
Misiones <- Misiones[which(Misiones@data$nprov=="MISIONES"),]
plot(Misiones)

# Loading departamentos
departamentos <- readOGR("./shp", "departamentos")
names(departamentos@data)

# filtering deps. from misiones
departamentos <- departamentos[which(departamentos@data$provincia=="MISIONES"),]
plot(departamentos)
names(departamentos)
names(departamentos)[2] <- 'label'
departamentos$label <- as.character(departamentos$label)

source("../FGC/fgc_function.R")
View(fgc_fct)
args(fgc_fct)

# organizing deps. in list object
deplist <- list()
for (a in 1:nrow(departamentos)){
  deplist[[a]] <- departamentos[a,]
}

# applying to deps list object the function fgc_fct()
lapply(deplist, fgc_fct, threshold = 90)

# Analisis de los resultados ----
# Area de los departamentos
library(rgeos)
Albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs "
departamentos@data$area_ha <- gArea(spTransform(departamentos, Albers), byid = TRUE)/10000

cover <- list.files('./results/', pattern = "Cover", full.names = T)
cover <- lapply(cover, read.csv)
cover <- rbindlist(cover)
head(cover)
names(cover) <- c("Departamento", paste(2000:2014))

cover <- merge(cover, departamentos@data, by.x="Departamento", by.y="label")
head(cover)
cover <- as.data.frame(cover)
cover <- cover[, c(1:15, ncol(cover)) ]
library(plyr)

write.csv(cover, "ForestCover_Misiones_Result.csv")

Def <- list.files('./results/', pattern = "Def", full.names = T)
Def <- lapply(Def, read.csv)
Def <- rbindlist(Def)
head(Def)
names(Def) <- c("Departamento", paste(2000:2014))

names(departamentos)
names(Def)
Def <- merge(Def, departamentos@data, by.x="Departamento", by.y="label")
head(Def)
Def <- as.data.frame(Def)
Def <- Def[, c(1, 3:15, ncol(Def)) ]

write.csv(Def, "Deforestation_Misiones_Result.csv", row.names = FALSE)



Tax <- (Def[,2:(ncol(Def)-1)] /Def$area_ha)*100
Tax$Departamentos <- Def$Departamento
head(Tax)
write.csv(Tax, "Tax.csv")
Tax <- read.csv("Tax.csv", check.names = F)

library(ggplot2)
Tax <- melt(Tax, id.vars="Departamentos", measure.vars=c(colnames(Tax)[1:13]), variable.name="year", value.name="TazaDef")
Tax <- Tax[-(1:17),]
Tax <- Tax[grep(paste(c("MONTECARLO","IGUAZU","SAN IGNACIO", "ELDORADO", "LIBERTADOR GENERAL SAN MARTIN"), collapse = "|"), Tax$Departamentos),]

ggplot(Tax, aes(x=year, y=TazaDef, group=Departamentos, colour=Departamentos)) + geom_line()
library(plotly)
ggplotly(p = ggplot2::last_plot(), width = NULL, height = NULL, tooltip = "all", dynamicTicks = FALSE, layerData = 1, originalData = TRUE, source = "A")


Gain <- list.files('./results/', pattern = "Gain", full.names = T)
Gain <- lapply(Gain, read.csv)
Gain <- rbindlist(Gain)
head(Gain)
names(Gain)[3] <- "Departamento"

write.csv(Gain, "Summary_Misiones_Result.csv")



#extract_gfc(aoi, data_folder, to_UTM = FALSE, stack = "change",
#            data_year = 2015, ...)
#gfc <- threshold_gfc(gfc, forest_threshold = 25, ...)
#annual_stack(gfc, data_year = 2015)




start <- Sys.time()

## Identifying the tiles of GFC project that contains the Area of Interest (AOI)
tiles <- calc_gfc_tiles(departamentos)

## Downalod the GFC images fr the tiles identifyed before.
download_tiles(tiles, output_folder = './Hansen/', images = c("treecover2000", "loss", "gain", "lossyear", "datamask"), data_year = 2015)

## Creates a raster stack with analysis to be used in the stats
exGFC <- extract_gfc(departamentos, data_folder = "./Hansen/", to_UTM = FALSE, stack = "change", data_year = 2015)

## Defyning threshold
exGFC <- threshold_gfc(exGFC, forest_threshold = 95)
#plot(exGFC[[2]]>0)
plot(exGFC[[1]])
plot(departamentos, add=T)

## Create statistics
esStats <- gfc_stats(departamentos, gfc = exGFC, scale_factor = 1e-04, data_year = 2015)

## Save outputs
resultDF <- as.data.frame(t(esStats$loss_table), stringsAsFactors = F)

colnames(resultDF) <- as.character(resultDF[1,])
cover <- resultDF[3,]
rownames(cover) <- "Misiones"
def <- resultDF[4,]
rownames(def) <- "Misiones"

write.csv(cover, paste0("./results/CoverTable_", as.character(rownames(cover)), ".csv"))
write.csv(def, paste0("./results/DefTable_", as.character(rownames(def)), ".csv"))

write.csv(esStats$gain_table, paste0("./results/GainTable_", as.character(rownames(def)), ".csv"))
#plot(runshp)

# stack 
gfc_stack <- annual_stack(exGFC, data_year = 2015)

# Animated land cover change
animate_annual(runshp, gfc_stack, out_dir = getwd(), out_basename = "gfc_animation", site_name = "Misiones", type = "gif", height = 3, width = 3, dpi = 300, data_year = 2015)

print(Sys.time()-start)
