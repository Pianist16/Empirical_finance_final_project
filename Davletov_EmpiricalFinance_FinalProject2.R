setwd("C:/Users/User/Desktop/Чина/CEU/2nd Year/2.Winter2016/Empirical Finance/Project/Data/FTSE100")

#temp = list.files(pattern="*.csv")
#myfiles = lapply(temp, read.delim)

#temp = list.files(pattern="*.csv")
#for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

temp <- list.files(pattern="*.csv")
list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), read.csv), envir = .GlobalEnv)

#### normalized monthly returns function ####

# x is daily data
# y is monthly data

nr <- function(x,y){
  x["logreturns"] <- numeric()
  x1 <- rev(diff(rev(log(x[,7]))))
  x <- x[-nrow(x),]
  x[,8] <- x1
  x["Date1"] <- character()
  x[,9] <- substr(x[,1],1,7)
  x
  
  y["logreturns"] <- numeric()
  y1 <- rev(diff(rev(log(y[,7]))))
  y <- y[-nrow(y),]
  y[,8] <- y1
  y[,"sd"] <- numeric()
  y2 <- tapply(x[,8],x[,9],sd)
  y[,"sd"] <- y2[-length(y2)]
  y[,"normalized returns"] <- numeric()
  co <- 1/sqrt(2*log(nrow(y)))
  de <- sqrt(2*log(nrow(y)))-((log(4*pi)+log(log(nrow(y))))/(2*sqrt(2*log(nrow(y)))))
  y[,10] <- ((y[,8]/(y[,9]*sqrt(20)))-de)*co
  as.matrix(y[,10])
}
###
#all FTSE100
aa = c(
  max(nr(AAL.L.d,AAL.L.m)),max(nr(ADM.L.d,ADM.L.m)),max(nr(AHT.L.d,AHT.L.m)),max(nr(ANTO.L.d,ANTO.L.m)),max(nr(ARM.L.d,ARM.L.m)),
  max(nr(AV.L.d,AV.L.m)),max(nr(AZN.L.d,AZN.L.m)),max(nr(BA.L.d,BA.L.m)),max(nr(BAB.L.d,BAB.L.m)),max(nr(BARC.L.d,BARC.L.m)),
  max(nr(BATS.L.d,BATS.L.m)),max(nr(BDEV.L.d,BDEV.L.m)),max(nr(BKG.L.d,BKG.L.m)),max(nr(BLND.L.d,BLND.L.m)),max(nr(BLT.L.d,BLT.L.m)),
  max(nr(BNZL.L.d,BNZL.L.m)),max(nr(BP.L.d,BP.L.m)),max(nr(BRBY.L.d,BRBY.L.m)),max(nr(BT.A.L.d,BT.A.L.m)),max(nr(CCH.L.d,CCH.L.m)),
  max(nr(CCL.d,CCL.m)),max(nr(CNA.L.d,CNA.L.m)),max(nr(CPG.L.d,CPG.L.m)),max(nr(CPI.L.d,CPI.L.m)),max(nr(CRH.L.d,CRH.L.m)),
  max(nr(DC.L.d,DC.L.m)),max(nr(DCC.L.d,DCC.L.m)),max(nr(DGE.L.d,DGE.L.m)),max(nr(DLG.L.d,DLG.L.m)),max(nr(EXPN.L.d,EXPN.L.m)),
  max(nr(EZJ.L.d,EZJ.L.m)),max(nr(FRES.L.d,FRES.L.m)),max(nr(GKN.L.d,GKN.L.m)),max(nr(GLEN.L.d,GLEN.L.m)),max(nr(GSK.L.d,GSK.L.m)),
  max(nr(HL.L.d,HL.L.m)),max(nr(HMSO.L.d,HMSO.L.m)),max(nr(HSBA.L.d,HSBA.L.m)),max(nr(IAG.L.d,IAG.L.m)),#max(nr(IHG.L.d,IHG.L.m)),
  max(nr(III.L.d,III.L.m)),max(nr(IMB.L.d,IMB.L.m)),max(nr(INF.L.d,INF.L.m)),#max(nr(INTU.L.d,INTU.L.m)),
  max(nr(ISAT.L.d,ISAT.L.m)),
  max(nr(ITRK.L.d,ITRK.L.m)),max(nr(ITV.L.d,ITV.L.m)),max(nr(JMAT.L.d,JMAT.L.m)),max(nr(KGF.L.d,KGF.L.m)),max(nr(LAND.L.d,LAND.L.m)),
  max(nr(LGEN.L.d,LGEN.L.m)),max(nr(LLOY.L.d,LLOY.L.m)),max(nr(LSE.L.d,LSE.L.m)),max(nr(MERL.L.d,MERL.L.m)),max(nr(MKS.L.d,MKS.L.m)),
  #max(nr(MNDI.L.d,MNDI.L.m)),
  max(nr(MRW.L.d,MRW.L.m)),max(nr(NG.L.d,NG.L.m)),max(nr(OML.L.d,OML.L.m)),max(nr(PFG.L.d,PFG.L.m)),
  max(nr(PPB.L.d,PPB.L.m)),max(nr(PRU.L.d,PRU.L.m)),max(nr(PSN.L.d,PSN.L.m)),max(nr(PSON.L.d,PSON.L.m)),max(nr(RB.L.d,RB.L.m)),
  max(nr(RBS.L.d,RBS.L.m)),max(nr(RDSA.L.d,RDSA.L.m)),max(nr(RDSB.L.d,RDSB.L.m)),max(nr(REL.L.d,REL.L.m)),max(nr(REX.L.d,REX.L.m)),
  max(nr(RIO.L.d,RIO.L.m)),max(nr(RMG.L.d,RMG.L.m)),max(nr(RR.L.d,RR.L.m)),max(nr(RRS.L.d,RRS.L.m)),max(nr(RSA.L.d,RSA.L.m)),
  max(nr(SAB.L.d,SAB.L.m)),max(nr(SBRY.L.d,SBRY.L.m)),max(nr(SDR.L.d,SDR.L.m)),max(nr(SGE.L.d,SGE.L.m)),max(nr(SHP.L.d,SHP.L.m)),
  max(nr(SKY.L.d,SKY.L.m)),max(nr(SL.L.d,SL.L.m)),max(nr(SN.L.d,SN.L.m)),max(nr(SSE.L.d,SSE.L.m)),max(nr(STAN.L.d,STAN.L.m)),
  max(nr(STJ.L.d,STJ.L.m)),max(nr(SVT.L.d,SVT.L.m)),max(nr(TPK.L.d,TPK.L.m)),max(nr(TSCO.L.d,TSCO.L.m)),max(nr(ULVR.L.d,ULVR.L.m)),
  max(nr(UU.L.d,UU.L.m)),max(nr(VOD.L.d,VOD.L.m)),max(nr(WOS.L.d,WOS.L.m)),max(nr(WPG.L.d,WPG.L.m)),max(nr(WPP.L.d,WPP.L.m)),max(nr(WTB.L.d,WTB.L.m))
)

#20 years
a = c(
  #max(nr(AAL.L.d,AAL.L.m)),max(nr(ADM.L.d,ADM.L.m)),
  max(nr(AHT.L.d,AHT.L.m)),max(nr(ANTO.L.d,ANTO.L.m)),#max(nr(ARM.L.d,ARM.L.m)),
  max(nr(AV.L.d,AV.L.m)),max(nr(AZN.L.d,AZN.L.m)),max(nr(BA.L.d,BA.L.m)),max(nr(BAB.L.d,BAB.L.m)),max(nr(BARC.L.d,BARC.L.m)),
  max(nr(BATS.L.d,BATS.L.m)),max(nr(BDEV.L.d,BDEV.L.m)),max(nr(BKG.L.d,BKG.L.m)),max(nr(BLND.L.d,BLND.L.m)),#max(nr(BLT.L.d,BLT.L.m)),
  max(nr(BNZL.L.d,BNZL.L.m)),max(nr(BP.L.d,BP.L.m)),#max(nr(BRBY.L.d,BRBY.L.m)),
  max(nr(BT.A.L.d,BT.A.L.m)),#max(nr(CCH.L.d,CCH.L.m)),max(nr(CCL.d,CCL.m)),max(nr(CNA.L.d,CNA.L.m)),max(nr(CPG.L.d,CPG.L.m)),
  max(nr(CPI.L.d,CPI.L.m)),#max(nr(CRH.L.d,CRH.L.m)),max(nr(DC.L.d,DC.L.m)),max(nr(DCC.L.d,DCC.L.m)),
  max(nr(DGE.L.d,DGE.L.m)),#max(nr(DLG.L.d,DLG.L.m)),max(nr(EXPN.L.d,EXPN.L.m)),max(nr(EZJ.L.d,EZJ.L.m)),max(nr(FRES.L.d,FRES.L.m)),
  max(nr(GKN.L.d,GKN.L.m)),#max(nr(GLEN.L.d,GLEN.L.m)),
  max(nr(GSK.L.d,GSK.L.m)),#max(nr(HL.L.d,HL.L.m)),
  max(nr(HMSO.L.d,HMSO.L.m)),max(nr(HSBA.L.d,HSBA.L.m)),#max(nr(IAG.L.d,IAG.L.m)),##max(nr(IHG.L.d,IHG.L.m)),
  max(nr(III.L.d,III.L.m)),max(nr(IMB.L.d,IMB.L.m)),#max(nr(INF.L.d,INF.L.m)),##max(nr(INTU.L.d,INTU.L.m)),
  #max(nr(ISAT.L.d,ISAT.L.m)),#max(nr(ITRK.L.d,ITRK.L.m)),#max(nr(ITV.L.d,ITV.L.m)),
  max(nr(JMAT.L.d,JMAT.L.m)),max(nr(KGF.L.d,KGF.L.m)),max(nr(LAND.L.d,LAND.L.m)),
  max(nr(LGEN.L.d,LGEN.L.m)),max(nr(LLOY.L.d,LLOY.L.m)),#max(nr(LSE.L.d,LSE.L.m)),#max(nr(MERL.L.d,MERL.L.m)),
  max(nr(MKS.L.d,MKS.L.m)),
  ##max(nr(MNDI.L.d,MNDI.L.m)),
  max(nr(MRW.L.d,MRW.L.m)),max(nr(NG.L.d,NG.L.m)),#max(nr(OML.L.d,OML.L.m)),
  max(nr(PFG.L.d,PFG.L.m)),#max(nr(PPB.L.d,PPB.L.m)),
  max(nr(PRU.L.d,PRU.L.m)),max(nr(PSN.L.d,PSN.L.m)),max(nr(PSON.L.d,PSON.L.m)),max(nr(RB.L.d,RB.L.m)),
  max(nr(RBS.L.d,RBS.L.m)),#max(nr(RDSA.L.d,RDSA.L.m)),max(nr(RDSB.L.d,RDSB.L.m)),
  max(nr(REL.L.d,REL.L.m)),max(nr(REX.L.d,REX.L.m)),
  max(nr(RIO.L.d,RIO.L.m)),#max(nr(RMG.L.d,RMG.L.m)),
  max(nr(RR.L.d,RR.L.m)),max(nr(RRS.L.d,RRS.L.m)),max(nr(RSA.L.d,RSA.L.m)),
  #max(nr(SAB.L.d,SAB.L.m)),
  max(nr(SBRY.L.d,SBRY.L.m)),max(nr(SDR.L.d,SDR.L.m)),max(nr(SGE.L.d,SGE.L.m)),max(nr(SHP.L.d,SHP.L.m)),
  max(nr(SKY.L.d,SKY.L.m)),#max(nr(SL.L.d,SL.L.m)),
  max(nr(SN.L.d,SN.L.m)),max(nr(SSE.L.d,SSE.L.m)),max(nr(STAN.L.d,STAN.L.m)),
  max(nr(STJ.L.d,STJ.L.m)),max(nr(SVT.L.d,SVT.L.m)),max(nr(TPK.L.d,TPK.L.m)),max(nr(TSCO.L.d,TSCO.L.m)),max(nr(ULVR.L.d,ULVR.L.m)),
  max(nr(UU.L.d,UU.L.m)),max(nr(VOD.L.d,VOD.L.m)),max(nr(WOS.L.d,WOS.L.m)),#max(nr(WPG.L.d,WPG.L.m)),
  max(nr(WPP.L.d,WPP.L.m)),max(nr(WTB.L.d,WTB.L.m))
  )

ba <- as.matrix(aa)
ba <- ba[!is.na(ba)]
ba <- as.matrix(ba)
ca <- ba[!is.infinite(ba)]
ca <- as.matrix(ca)
#plot(density(ca))

b <- as.matrix(a)
b <- b[!is.na(b)]
b <- as.matrix(b)
#plot(density(b))

c <- b[!is.infinite(b)]
c <- as.matrix(c)
#plot(density(c))


#library(evd)

gum <- rgumbel(10000,loc=0,scale=1)
g <- density(gum)
d1 <- density(b)
d2 <- density(c)
#da <- density(ba)
da <- density(ca)

#20years
plot(range(g$x,d1$x, d2$x, da$x), range(g$y, d1$y, d2$y, da$y), type = "n", xlab = "maxima",
     ylab = "Density", main = "Density of maxima of normalized log returns(centered and rescaled)")
lines(d1, col = "blue",lwd=4)
lines(d2, col = "lightblue",lwd=4,lty=2)
lines(g, col = "black",lwd=4)
lines(da, col = "red",lwd=3)

#10 years
a1 = c(
  max(nr(AAL.L.d,AAL.L.m)),max(nr(ADM.L.d,ADM.L.m)),max(nr(AHT.L.d,AHT.L.m)),max(nr(ANTO.L.d,ANTO.L.m)),max(nr(ARM.L.d,ARM.L.m)),
  max(nr(AV.L.d,AV.L.m)),max(nr(AZN.L.d,AZN.L.m)),max(nr(BA.L.d,BA.L.m)),max(nr(BAB.L.d,BAB.L.m)),max(nr(BARC.L.d,BARC.L.m)),
  max(nr(BATS.L.d,BATS.L.m)),max(nr(BDEV.L.d,BDEV.L.m)),max(nr(BKG.L.d,BKG.L.m)),max(nr(BLND.L.d,BLND.L.m)),max(nr(BLT.L.d,BLT.L.m)),
  max(nr(BNZL.L.d,BNZL.L.m)),max(nr(BP.L.d,BP.L.m)),max(nr(BRBY.L.d,BRBY.L.m)),max(nr(BT.A.L.d,BT.A.L.m)),#max(nr(CCH.L.d,CCH.L.m)),
  max(nr(CCL.d,CCL.m)),max(nr(CNA.L.d,CNA.L.m)),max(nr(CPG.L.d,CPG.L.m)),max(nr(CPI.L.d,CPI.L.m)),max(nr(CRH.L.d,CRH.L.m)),
  #max(nr(DC.L.d,DC.L.m)),
  max(nr(DCC.L.d,DCC.L.m)),max(nr(DGE.L.d,DGE.L.m)),#max(nr(DLG.L.d,DLG.L.m)),#max(nr(EXPN.L.d,EXPN.L.m)),
  max(nr(EZJ.L.d,EZJ.L.m)),#max(nr(FRES.L.d,FRES.L.m)),
  max(nr(GKN.L.d,GKN.L.m)),#max(nr(GLEN.L.d,GLEN.L.m)),
  max(nr(GSK.L.d,GSK.L.m)),
  #max(nr(HL.L.d,HL.L.m)),
  max(nr(HMSO.L.d,HMSO.L.m)),max(nr(HSBA.L.d,HSBA.L.m)),max(nr(IAG.L.d,IAG.L.m)),#max(nr(IHG.L.d,IHG.L.m)),
  max(nr(III.L.d,III.L.m)),max(nr(IMB.L.d,IMB.L.m)),max(nr(INF.L.d,INF.L.m)),#max(nr(INTU.L.d,INTU.L.m)),
  max(nr(ISAT.L.d,ISAT.L.m)),
  max(nr(ITRK.L.d,ITRK.L.m)),max(nr(ITV.L.d,ITV.L.m)),max(nr(JMAT.L.d,JMAT.L.m)),max(nr(KGF.L.d,KGF.L.m)),max(nr(LAND.L.d,LAND.L.m)),
  max(nr(LGEN.L.d,LGEN.L.m)),max(nr(LLOY.L.d,LLOY.L.m)),max(nr(LSE.L.d,LSE.L.m)),#max(nr(MERL.L.d,MERL.L.m)),
  max(nr(MKS.L.d,MKS.L.m)),
  #max(nr(MNDI.L.d,MNDI.L.m)),
  max(nr(MRW.L.d,MRW.L.m)),max(nr(NG.L.d,NG.L.m)),max(nr(OML.L.d,OML.L.m)),max(nr(PFG.L.d,PFG.L.m)),
  max(nr(PPB.L.d,PPB.L.m)),max(nr(PRU.L.d,PRU.L.m)),max(nr(PSN.L.d,PSN.L.m)),max(nr(PSON.L.d,PSON.L.m)),max(nr(RB.L.d,RB.L.m)),
  max(nr(RBS.L.d,RBS.L.m)),max(nr(RDSA.L.d,RDSA.L.m)),max(nr(RDSB.L.d,RDSB.L.m)),max(nr(REL.L.d,REL.L.m)),max(nr(REX.L.d,REX.L.m)),
  max(nr(RIO.L.d,RIO.L.m)),#max(nr(RMG.L.d,RMG.L.m)),
  max(nr(RR.L.d,RR.L.m)),max(nr(RRS.L.d,RRS.L.m)),max(nr(RSA.L.d,RSA.L.m)),
  max(nr(SAB.L.d,SAB.L.m)),max(nr(SBRY.L.d,SBRY.L.m)),max(nr(SDR.L.d,SDR.L.m)),max(nr(SGE.L.d,SGE.L.m)),max(nr(SHP.L.d,SHP.L.m)),
  max(nr(SKY.L.d,SKY.L.m)),max(nr(SL.L.d,SL.L.m)),max(nr(SN.L.d,SN.L.m)),max(nr(SSE.L.d,SSE.L.m)),max(nr(STAN.L.d,STAN.L.m)),
  max(nr(STJ.L.d,STJ.L.m)),max(nr(SVT.L.d,SVT.L.m)),max(nr(TPK.L.d,TPK.L.m)),max(nr(TSCO.L.d,TSCO.L.m)),max(nr(ULVR.L.d,ULVR.L.m)),
  max(nr(UU.L.d,UU.L.m)),max(nr(VOD.L.d,VOD.L.m)),max(nr(WOS.L.d,WOS.L.m)),#max(nr(WPG.L.d,WPG.L.m)),
  max(nr(WPP.L.d,WPP.L.m)),max(nr(WTB.L.d,WTB.L.m))
)

b1 <- as.matrix(a1)
b1 <- b1[!is.na(b1)]
b1 <- as.matrix(b1)
#plot(density(b1))

c1 <- b1[!is.infinite(b1)]
c1 <- as.matrix(c1)
#plot(density(c1))

d1.1 <- density(b1)
d2.1 <- density(c1)

#10 years
plot(range(g$x,d1.1$x, d2.1$x, da$x), range(g$y, d1.1$y, d2.1$y, da$y), type = "n", xlab = "maxima",
     ylab = "Density", main = "Density of maxima of normalized log returns(centered and rescaled)")
lines(d1.1, col = "blue",lwd=4)
lines(d2.1, col = "lightblue",lwd=4,lty=2)
lines(g, col = "black",lwd=4)
lines(da, col = "red",lwd=3)

