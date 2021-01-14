# get datasets for panels ------------------------------------------------------
m <- myrcw[myrcw$epo %in% c("SALT_A", "SALT_B") & !is.na(myrcw$dw_00) & !is.na(myrcw$dw_80),]
m$Fitness_in_envX <- m$dw_00
m$Fitness_in_envY <- m$dw_80
m$Treatment <- m$treat


# Create the geomeans dataset --------------------------------------------------
geomeans <- aggregate(cbind(m$Fitness_in_envX+1, m$Fitness_in_envY+1) ~ m$treat, m, psych::geometric.mean)
# geomeans <- aggregate(cbind(m$Fitness_in_envX+1, m$Fitness_in_envY+1) ~ m$treat, m, mean) # no qualitative difference if just "mean" is used. 
# geomeans <- aggregate(cbind(m$Fitness_in_envX+1, m$Fitness_in_envY+1) ~ m$treat, m, median) # no qualitative difference if just "median" is used. 
colnames(geomeans) <-  c("Treatment","Fitness_in_envX", "Fitness_in_envY")
geomeans[,c(2,3)] <- geomeans[,c(2,3)] - 1
m$Fitness_in_envX <- m$Fitness_in_envX*100
m$Fitness_in_envY <- m$Fitness_in_envY*100
geomeans[,c(2,3)] <- geomeans[,c(2,3)]*100


# Create geometric mean fitness surface for plotting --------------------------
ll <- 1000
geomfit <- matrix(nrow= ll, ncol = ll)
x <- seq(from = 0.05,1.6, length.out = ll)
colnames(geomfit) <- x; rownames(geomfit) <- x
for (i in 1:nrow(geomfit)) {
  for (j in 1:ncol(geomfit)) {
    geomfit[i,j] <- (x[j] * x[i])^(0.5)
  }
}
geomfit <- reshape::melt(round(geomfit, digits = 4))
colnames(geomfit) <- c("geomfitx", "geomfity", "geomfitfit")
geomfit <- (geomfit - 1)*100
rm(i, j, ll, x)


# create the legend for the multipanel plot ------------------------------------
ptemp <- ggplot()+
  geom_point(data=geomeans, aes(x=Fitness_in_envX, y=Fitness_in_envY, fill = Treatment),
             color = "black", cex = 5, shape = 21, show.legend = T)+
  scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette)+
  guides(fill = guide_legend(reverse = TRUE))+
  guides(color = guide_legend(reverse = TRUE))+
  theme_VJF
p1leg <- cowplot::get_legend(ptemp); rm(ptemp)



# Create the Kassen Plot (Panel A) Function ------------------------------------
kassenpanel <- function(pxlab, pylab, ptag, pxlim, pylim, pannotate){
  # plot background layer --------------
  pZZ <- ggplot()+ 
    geom_vline(xintercept = 0, color = "darkgray") +
    geom_hline(yintercept = 0, color = "darkgray")+
    # geom_abline(slope = 1, color = "darkgray")+
    xlab(pxlab)+ ylab(pylab)+
    coord_equal(xlim=pxlim, ylim = pylim)+
    theme_VJF +   
    scale_color_manual(values = mypalette) + scale_fill_manual(values = mypalette)
  # for (i in c(0.9,0.95,1.00,1.05,1.10,1.15,1.20,1.25,1.30)) {
  #   pZZ <- pZZ + geom_line(data = geomfit[geomfit$geomfitfit == (i-1)*100,],
  #                          aes(x = geomfitx, y = geomfity), lty  = "solid", color = "gray90")
  # }
  # plot data layer --------------------
  pZZ <- pZZ +
    stat_ellipse(data = m, aes(x=Fitness_in_envX, y=Fitness_in_envY, fill = Treatment, color = Treatment),
                 alpha = 0.1, geom = "polygon", linetype = 6, show.legend = F, level = 0.75)+
    geom_point(data = m, aes(x=Fitness_in_envX, y=Fitness_in_envY, color = Treatment), size = 1, alpha = 0.33, show.legend = F) +
    geom_segment(mapping = aes(x=geomeans[1,2], y=geomeans[1,3], xend=geomeans[7,2], yend=geomeans[7,3]), linetype=1, size=1.25, inherit.aes=F)+
    # geom_segment(mapping = aes(x=geomeans[3,2], y=geomeans[3,3], xend=geomeans[7,2], yend=geomeans[7,3]), linetype=1, size=1.25, inherit.aes=F)+
    # geom_segment(mapping = aes(x=geomeans[1,2], y=geomeans[1,3], xend=geomeans[3,2], yend=geomeans[3,3]), linetype=1, size=1.25, inherit.aes=F)+
    geom_point(data=geomeans, aes(x=Fitness_in_envX, y=Fitness_in_envY, fill = Treatment),color = "black", cex = 5, shape = 21, show.legend = F)+
    # annotate("text", x = c((0.9-1)*100,(0.95-1)*100, (1.00-1)*100, (1.05-1)*100, (1.1-1)*100, (1.15-1)*100),
    #          y = c((0.9-1)*100,(0.95-1)*100, (1.00-1)*100, (1.05-1)*100, (1.1-1)*100, (1.15-1)*100),
    #          label = c("-10","-5", "0","5", "10", "15"), size = 2.91, fontface = "bold")+
    # geom_rug(data = m, aes(x=Fitness_in_envX, color = treat), length = unit(0.015, "npc"), alpha = 0.66, show.legend = F)+
    # geom_rug(data = m, aes(y=Fitness_in_envY, color = treat), length = unit(0.030, "npc"), alpha = 0.66, show.legend = F)+
    annotate("text", x = pannotate[1], y =pannotate[2], label = ptag, fontface = "bold", size = 16)+
    theme(legend.position = "none")
  pZZ
}


# Create the Vector Fitness (Panels B:H) Function ------------------------------
vectorpanel <- function(ptreat, palettecolorpos, ptag, pxlim, pylim, pannotate){
  # plot background layer --------------
  pZZ <- ggplot()+
    geom_vline(xintercept = 0, color = "darkgray") +
    geom_hline(yintercept = 0, color = "darkgray")+
    # geom_abline(slope = 1, color = "darkgray")+
    xlab("")+ ylab(NULL)+
    coord_equal(xlim=pxlim, ylim = pylim)+
    theme_VJF + theme(legend.position = "none")
  # for (i in c(0.9,0.95,1.00,1.05,1.10,1.15,1.20,1.25,1.30)) {
  #   pZZ <- pZZ + geom_line(data = geomfit[geomfit$geomfitfit == (i-1)*100,],
  #                          aes(x = geomfitx, y = geomfity), lty  = "solid", color = "gray90")
  # }
  # plot data layer --------------------
  pZZ <- pZZ +
    geom_segment(mapping = aes(x=geomeans[1,2], y=geomeans[1,3], xend=geomeans[7,2], yend=geomeans[7,3]), linetype=1, size=1.25, inherit.aes=F)+
    geom_segment(data =m[m$treat %in% c(ptreat),],aes(x=1, y=1, xend=m$Fitness_in_envX[m$treat %in% c(ptreat)], yend=m$Fitness_in_envY[m$treat %in% c(ptreat)]),
                 color =mypalette[palettecolorpos], size=2/2, inherit.aes=F, alpha = 0.5)+
    geom_point(data =m[m$treat %in% c(ptreat),],aes(x=m$Fitness_in_envX[m$treat %in% c(ptreat)], y=m$Fitness_in_envY[m$treat %in% c(ptreat)]),
               color=mypalette[palettecolorpos], fill = mypalette[palettecolorpos], size = 2.5/2, alpha = 0.75) +
    geom_point(data =m[m$treat %in% c(ptreat),],aes(x=m$Fitness_in_envX[m$treat %in% c(ptreat)], y=m$Fitness_in_envY[m$treat %in% c(ptreat)]),
               shape = 1, colour = "black", size = 2.5/2)+
    geom_rug(data=m[m$treat %in% c(ptreat),], aes(x=Fitness_in_envX, color = treat), length = unit(0.03, "npc"),
             alpha = 0.66, color = mypalette[palettecolorpos])+
    geom_rug(data=m[m$treat %in% c(ptreat),], aes(y=Fitness_in_envY, color = treat), length = unit(0.060, "npc"),
             alpha = 0.66, color = mypalette[palettecolorpos])+
    annotate("text", x = pannotate[1], y = pannotate[2], label = ptag, fontface = "bold", size = 8)
  pZZ
}


# Visualization ----------------------------------------------------------------
p1 <- kassenpanel(pxlab = "Fitness Change in CM (%)", pylab = "Fitness Change in CM + NaCl (%)", ptag = "A", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-12,44))
p2 <- vectorpanel(ptreat = "EH0", palettecolorpos = 1, ptag = "B", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-11, 44))
p3 <- vectorpanel(ptreat = "EH0_40", palettecolorpos = 2, ptag = "C", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-11, 44))
p4 <- vectorpanel(ptreat = "EH40", palettecolorpos = 3, ptag = "D", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-11, 44))
p5 <- vectorpanel(ptreat = "EH20_60", palettecolorpos = 4, ptag = "E", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-11, 44))
p6 <- vectorpanel(ptreat = "EH0_80", palettecolorpos = 5, ptag = "F", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-11, 44))
p7 <- vectorpanel(ptreat = "EH40_80", palettecolorpos = 6, ptag = "G", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-11, 44))
p8 <- vectorpanel(ptreat = "EH80", palettecolorpos = 7, ptag = "H", pxlim = c(-15,15), pylim = c(-10,45), pannotate = c(-11, 44))


# Save outputs to file ---------------------------------------------------------
lay<- as.matrix(rbind(cbind(1,1,2,3,4,5),
                      cbind(1,1,6,7,8,9)))

setwd(DIR_out); pdf(file = paste("000_Figure_2_oneline_nogeomfit.pdf"),height = 6, width = 11)
grid::grid.draw(grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p1leg, layout_matrix =lay)); dev.off()
