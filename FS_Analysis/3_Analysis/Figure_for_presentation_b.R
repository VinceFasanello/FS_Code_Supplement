# get datasets for panels A, B, C, D -------------------------------------------
myt <- myrc[myrc$treat %in% c("EH0", "EH80"),]
mAA <- myt[myt$mpasp==0.0 & myt$epo %in% c("SALT_A", "SALT_B") & !is.na(myt$dw) & !is.na(myt$reads_dw),]
mCC <- myt[myt$mpasp==0.8 & myt$epo %in% c("SALT_A", "SALT_B") & !is.na(myt$dw) & !is.na(myt$reads_dw),]
mBB <- myt[myt$mpasp==0.0 & myt$epo %in% c("COPR_A", "COPR_B") & !is.na(myt$dw) & !is.na(myt$reads_dw),]
mDD <- myt[myt$mpasp==0.8 & myt$epo %in% c("COPR_A", "COPR_B") & !is.na(myt$dw) & !is.na(myt$reads_dw),]



# Set plotting controls --------------------------------------------------------
fitaxdatlimSALT <- c(-0.15*100, 0.45*100) #; min(c(mAA$dw, mCC$dw)); max(c(mAA$dw, mCC$dw))
fitaxdatlimCOPR <- c(-0.1*100, 0.55*100) #; min(c(mBB$dw, mDD$dw)); max(c(mBB$dw, mDD$dw))

fitaxlablim <- c(-0.1*100, 0.5*100) 
boxSALT <- c(0.2*100, 0.49*100) #; (boxSALT[2]-boxSALT[1]) / (fitaxdatlimSALT[2] - fitaxdatlimSALT[1])
boxCOPR <- c(0.275*100, 0.59*100) #; (boxCOPR[2]-boxCOPR[1]) / (fitaxdatlimCOPR[2] - fitaxdatlimCOPR[1])

treatlabSALT <- "NaCl Evol. Treatment"
treatlabCOPR <- expression(bold(CuSO["4"]*"  Evol. Treatment"))
fitnesslabCM <- "Fitness Change in CM (%)"
fitnesslabSALT <- "Fitness Change in CM + NaCl (%)"
fitnesslabCOPR <- expression(bold("Fitness Change in CM + "*CuSO["4"]* " (%)"))
treatlist <- c("EH0", "EH80")


# Create stats results backer --------------------------------------------------
myres_backer <- myres_est_SALT_0 
myres_backer[myres_backer < 0] <- -1
myres_backer[myres_backer > 0 | is.na(myres_backer)] <- 1
myres_backer <- get_lower_tri(round(myres_backer, 2))
melt_myres_backer <- reshape::melt(myres_backer)
melt_myres_backer$X1 <- factor(melt_myres_backer$X1, levels=treatlist)
melt_myres_backer$X2 <- factor(melt_myres_backer$X2, levels=treatlist)


# Create stats results dataframes for plotting ---------------------------------
lsuffs <- c("AA", "CC", "BB", "DD")
lintabs <- c("SALT_0", "SALT_0.8", "COPR_0", "COPR_0.8")
for (i in 1:4) {
  assign("myres_est_temp", get(paste0("myres_est_", lintabs[i])))
  assign("myres_p_temp", get(paste0("myres_p_", lintabs[i])))
  myres_est_temp[which(myres_p_temp > 0.05)] <- NA
  myres_est_temp <- get_lower_tri(round(myres_est_temp*100, 0))
  melt_myres_est_temp <- reshape::melt(myres_est_temp)
  melt_myres_est_temp$X1 <- factor(melt_myres_est_temp$X1, levels=treatlist)
  melt_myres_est_temp$X2 <- factor(melt_myres_est_temp$X2, levels=treatlist)
  assign(paste0("melt_myres_est_", lsuffs[i]), melt_myres_est_temp)
}; rm(melt_myres_est_temp, myres_est_COPR_0, myres_est_COPR_0.8, myres_est_SALT_0, myres_est_SALT_0.8, myres_est_temp,
      myres_p_COPR_0, myres_p_COPR_0.8, myres_p_SALT_0, myres_p_SALT_0.8, myres_p_temp, i, lintabs, lsuffs, treatlist)


# Define plot function for panels A, B, C, D -----------------------------------
viopanel <- function(dwdata, statsdata, backerdata=melt_myres_backer, dcol="dw", 
                     fitlims, insetlims, fitaxlablims=fitaxlablim, fitaxstep=0.1*100,
                     treatlab, fitlab, tag, usepalette=mypalette){
  # main panel matter ------------------
  pZZ <- ggplot(dwdata, aes(x=treat, y=dwdata[,dcol]*100, group=treat, color=treat)) +
    geom_hline(yintercept=0, lty="dotted") + 
    geom_violin(fill="gray90", color="gray30", draw_quantiles=NULL, trim=T) +
    geom_jitter(width=0.1, height=0, alpha=0.35) + 
    geom_rug(sides="l" , alpha=0.7, length=unit(0.015, "npc")) +
    geom_point(stat="summary", fun="mean", color="black", cex=1.5) + 
    geom_point(stat="summary", fun="median", color="black", cex=3, pch=1) + 
    geom_errorbar(stat="summary", fun.data="mean_se",color="black", width=0.0, lwd=0.5)+
    scale_y_continuous(breaks=seq(fitaxlablims[1], fitaxlablims[2], by=fitaxstep), 
                       limits=c(fitlims[1], fitlims[2]),
                       labels=scales::number_format(accuracy=0.01*100))+
    scale_color_manual(values=usepalette[c(1,length(usepalette))]) + 
    coord_flip() +
    ylab(fitlab) + xlab(treatlab) +  labs(tag=tag)+
    theme_VJF + theme(legend.position="none")

  return(pZZ)
}


# Visualization ----------------------------------------------------------------
pAA <- viopanel(dwdata=mAA, statsdata=melt_myres_est_AA, fitlims=fitaxdatlimSALT, insetlims=boxSALT, treatlab=treatlabSALT, fitlab=fitnesslabCM, tag="A")
pCC <- viopanel(dwdata=mCC, statsdata=melt_myres_est_CC, fitlims=fitaxdatlimSALT, insetlims=boxSALT, treatlab=treatlabSALT, fitlab=fitnesslabSALT, tag="C")
pBB <- viopanel(dwdata=mBB, statsdata=melt_myres_est_BB, fitlims=fitaxdatlimCOPR, insetlims=boxCOPR, treatlab=treatlabCOPR, fitlab=fitnesslabCM, tag="B")
pDD <- viopanel(dwdata=mDD, statsdata=melt_myres_est_DD, fitlims=fitaxdatlimCOPR, insetlims=boxCOPR, treatlab=treatlabCOPR, fitlab=fitnesslabCOPR, tag="D")


# Save outputs to file ---------------------------------------------------------
setwd(DIR_out); pdf(file="000_Figure_1_0_80_only.pdf", width=10, height=5)
cowplot::plot_grid(pAA, pBB, pCC, pDD, nrow=2); dev.off()






# get datasets for panels A, B, C, D -------------------------------------------
mAA <- myrc[myrc$mpasp==0.0 & myrc$epo %in% c("SALT_A", "SALT_B") & !is.na(myrc$dw) & !is.na(myrc$reads_dw),]
mCC <- myrc[myrc$mpasp==0.8 & myrc$epo %in% c("SALT_A", "SALT_B") & !is.na(myrc$dw) & !is.na(myrc$reads_dw),]
mBB <- myrc[myrc$mpasp==0.0 & myrc$epo %in% c("COPR_A", "COPR_B") & !is.na(myrc$dw) & !is.na(myrc$reads_dw),]
mDD <- myrc[myrc$mpasp==0.8 & myrc$epo %in% c("COPR_A", "COPR_B") & !is.na(myrc$dw) & !is.na(myrc$reads_dw),]


# Set plotting controls --------------------------------------------------------
fitaxdatlimSALT <- c(-0.15*100, 0.45*100) #; min(c(mAA$dw, mCC$dw)); max(c(mAA$dw, mCC$dw))
fitaxdatlimCOPR <- c(-0.1*100, 0.55*100) #; min(c(mBB$dw, mDD$dw)); max(c(mBB$dw, mDD$dw))

fitaxlablim <- c(-0.1*100, 0.5*100) 
boxSALT <- c(0.2*100, 0.49*100) #; (boxSALT[2]-boxSALT[1]) / (fitaxdatlimSALT[2] - fitaxdatlimSALT[1])
boxCOPR <- c(0.275*100, 0.59*100) #; (boxCOPR[2]-boxCOPR[1]) / (fitaxdatlimCOPR[2] - fitaxdatlimCOPR[1])

treatlabSALT <- "NaCl Evol. Treatment"
treatlabCOPR <- expression(bold(CuSO["4"]*"  Evol. Treatment"))
fitnesslabCM <- "Fitness Change in CM (%)"
fitnesslabSALT <- "Fitness Change in CM + NaCl (%)"
fitnesslabCOPR <- expression(bold("Fitness Change in CM + "*CuSO["4"]* " (%)"))
treatlist <- c("EH0","EH0_40","EH40","EH20_60","EH0_80","EH40_80","EH80")


# Create stats results backer --------------------------------------------------
myres_backer <- myres_est_SALT_0 
myres_backer[myres_backer < 0] <- -1
myres_backer[myres_backer > 0 | is.na(myres_backer)] <- 1
myres_backer <- get_lower_tri(round(myres_backer, 2))
melt_myres_backer <- reshape::melt(myres_backer)
melt_myres_backer$X1 <- factor(melt_myres_backer$X1, levels=treatlist)
melt_myres_backer$X2 <- factor(melt_myres_backer$X2, levels=treatlist)


# Create stats results dataframes for plotting ---------------------------------
lsuffs <- c("AA", "CC", "BB", "DD")
lintabs <- c("SALT_0", "SALT_0.8", "COPR_0", "COPR_0.8")
for (i in 1:4) {
  assign("myres_est_temp", get(paste0("myres_est_", lintabs[i])))
  assign("myres_p_temp", get(paste0("myres_p_", lintabs[i])))
  myres_est_temp[which(myres_p_temp > 0.05)] <- NA
  myres_est_temp <- get_lower_tri(round(myres_est_temp*100, 0))
  melt_myres_est_temp <- reshape::melt(myres_est_temp)
  melt_myres_est_temp$X1 <- factor(melt_myres_est_temp$X1, levels=treatlist)
  melt_myres_est_temp$X2 <- factor(melt_myres_est_temp$X2, levels=treatlist)
  assign(paste0("melt_myres_est_", lsuffs[i]), melt_myres_est_temp)
}; rm(melt_myres_est_temp, myres_est_COPR_0, myres_est_COPR_0.8, myres_est_SALT_0, myres_est_SALT_0.8, myres_est_temp,
      myres_p_COPR_0, myres_p_COPR_0.8, myres_p_SALT_0, myres_p_SALT_0.8, myres_p_temp, i, lintabs, lsuffs, treatlist)


# Define plot function for panels A, B, C, D -----------------------------------
viopanel <- function(dwdata, statsdata, backerdata=melt_myres_backer, dcol="dw", 
                     fitlims, insetlims, fitaxlablims=fitaxlablim, fitaxstep=0.1*100,
                     treatlab, fitlab, tag, usepalette=mypalette){
  # main panel matter ------------------
  pZZ <- ggplot(dwdata, aes(x=treat, y=dwdata[,dcol]*100, group=treat, color=treat)) +
    geom_hline(yintercept=0, lty="dotted") + 
    geom_violin(fill="gray90", color="gray30", draw_quantiles=NULL, trim=T) +
    geom_jitter(width=0.15, height=0, alpha=0.35) + 
    geom_rug(sides="l" , alpha=0.7, length=unit(0.015, "npc")) +
    geom_point(stat="summary", fun="mean", color="black", cex=1.5) + 
    geom_point(stat="summary", fun="median", color="black", cex=3, pch=1) + 
    geom_errorbar(stat="summary", fun.data="mean_se",color="black", width=0.0, lwd=0.5)+
    scale_y_continuous(breaks=seq(fitaxlablims[1], fitaxlablims[2], by=fitaxstep), 
                       limits=c(fitlims[1], fitlims[2]),
                       labels=scales::number_format(accuracy=0.01*100))+
    scale_color_manual(values=usepalette) + 
    coord_flip() +
    ylab(fitlab) + xlab(treatlab) +  labs(tag=tag)+
    theme_VJF + theme(legend.position="none")
  return(pZZ)
}


# Visualization ----------------------------------------------------------------
pAA <- viopanel(dwdata=mAA, statsdata=melt_myres_est_AA, fitlims=fitaxdatlimSALT, insetlims=boxSALT, treatlab=treatlabSALT, fitlab=fitnesslabCM, tag="A")
pCC <- viopanel(dwdata=mCC, statsdata=melt_myres_est_CC, fitlims=fitaxdatlimSALT, insetlims=boxSALT, treatlab=treatlabSALT, fitlab=fitnesslabSALT, tag="C")
pBB <- viopanel(dwdata=mBB, statsdata=melt_myres_est_BB, fitlims=fitaxdatlimCOPR, insetlims=boxCOPR, treatlab=treatlabCOPR, fitlab=fitnesslabCM, tag="B")
pDD <- viopanel(dwdata=mDD, statsdata=melt_myres_est_DD, fitlims=fitaxdatlimCOPR, insetlims=boxCOPR, treatlab=treatlabCOPR, fitlab=fitnesslabCOPR, tag="D")


# Save outputs to file ---------------------------------------------------------
setwd(DIR_out); pdf(file="000_Figure_1_nostats.pdf", width=15, height=8.5)
cowplot::plot_grid(pAA, pBB, pCC, pDD, nrow=2); dev.off()

