setwd("~/Desktop/Wild_Davis/capstone")

library(fdapace)
library(ggrepel)
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)
birds = read.csv("birds.csv")

# ggplot(birds, aes(x = x, y = dens, color = species)) +
#   geom_line() + 
#   theme(legend.position = "none")


inputs = MakeFPCAInputs(IDs = birds$species, tVec = birds$x, yVec = birds$dens)


fpca_obj = FPCA(Lt = inputs$Lt, Ly = inputs$Ly)
CreateCovPlot(fpca_obj, 'Fitted',
              isInteractive = TRUE)

# plot(fpca_obj)
# plot(fpca_obj$xiEst[,1], fpca_obj$xiEst[,2])

meandf = data.frame(x = fpca_obj$workGrid, mean =  fpca_obj$mu)

ggplot() +
  geom_line(data = birds, 
            mapping = aes(x = x, y = dens, group = species),
            color=grey(0.3, alpha = 0.5)) + 
  geom_line(data = meandf, mapping = aes(x = x, y = mean),
            lwd = 2) + 
  theme(legend.position = "none")
  


which(fpca_obj$xiEst[,1]>.12)
which.min(fpca_obj$xiEst[,1])
which.min(fpca_obj$xiEst[,2])
which.max(fpca_obj$xiEst[,2])
which(fpca_obj$xiEst[,2]>.09)
which(fpca_obj$xiEst[,2]>.09)


eigendf = data.frame(x = fpca_obj$workGrid,
                     phi1 = fpca_obj$phi[,1],
                     phi2 = fpca_obj$phi[,2])

eigendf_long = eigendf %>% pivot_longer(cols = starts_with("phi"))


idx = c(5,8,21,44,30)

birdspecies = birds$species %>% unique 

highlight = birdspecies[idx]

scoredf = data.frame(species = birdspecies, FPC1 = fpca_obj$xiEst[,1], FPC2 = fpca_obj$xiEst[,2])

ggplot(scoredf, aes(x = FPC1, y = FPC2)) + 
  geom_point() + 
  geom_text_repel(
    mapping = aes(x = FPC1, 
        y = FPC2,
        label = species),
    size = 2.5,min.segment.length=0)

hltcolors <- scales::hue_pal()(15)[c(6,4,2,12,9)]

ggplot() + 
  geom_point(data = scoredf, 
             mapping = aes(x = FPC1, y = FPC2)) + 
  geom_point(data = scoredf %>% filter(species %in% highlight), 
             mapping = aes(x = FPC1,
                           y = FPC2,
                           color = species)) + 
  geom_text_repel(data = scoredf %>% filter(species %in% highlight),
    mapping = aes(x = FPC1, 
                  y = FPC2,
                  label = species,
                  color = species),
    size = 5,min.segment.length=0) +
  scale_color_manual(name = "", values=hltcolors) +
  theme(legend.position = "none")

  
# birds %>% filter(species %in% highlight) %>% 
#   group_by(species) %>% filter(dens == max(dens))


ggplot() +
  geom_line(data = birds, 
            mapping = aes(x = x, y = dens, group = species),
            color=grey(0.3, alpha = 0.5)) + 
  geom_line(data = meandf, mapping = aes(x = x, y = mean),
            lwd = 2) + 
  geom_line(data = birds %>% filter(species %in% highlight), 
            mapping = aes(x = x, y = dens, group = species, color = species),
            lwd = .75) + 
  geom_text_repel(data = birds %>% filter(species %in% highlight) %>% 
                    group_by(species) %>% filter(dens == max(dens)),
                  mapping = aes(x = x, 
                                y = dens,
                                label = species),
                  size = 3.5,
                  min.segment.length=0)  +
  scale_color_manual(name = "", values=hltcolors) + 
  theme(legend.position = "none")

