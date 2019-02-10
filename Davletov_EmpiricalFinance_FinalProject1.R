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

a = c(
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

b <- as.matrix(a)
b <- b[!is.na(b)]
b <- as.matrix(b)
#plot(density(b))

c <- b[!is.infinite(b)]
c <- as.matrix(c)
#plot(density(c))

setwd("C:/Users/User/Desktop/Чина/CEU/2nd Year/2.Winter2016/Empirical Finance/Project/Data/FTSE250")
temp <- list.files(pattern="*.csv")
list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), read.csv), envir = .GlobalEnv)

a2 = c(
  max(nr(CLDNd,CLDNm)),max(nr(CLId,CLIm)),#max(nr(CLLNd,CLLNm)),
  max(nr(CNEd,CNEm)),max(nr(COBd,COBm)),
  max(nr(CRDAd,CRDAm)),max(nr(CRSTd,CRSTm)),max(nr(CWCd,CWCm)),max(nr(CWDd,CWDm)),max(nr(CWKd,CWKm)),
  max(nr(DCGd,DCGm)),max(nr(DEBd,DEBm)),max(nr(DJANd,DJANm)),max(nr(DLNd,DLNm)),max(nr(DNLMd,DNLMm)),
  max(nr(DOMd,DOMm)),max(nr(DPHd,DPHm)),max(nr(DPLMd,DPLMm)),max(nr(DRXd,DRXm)),max(nr(DTYd,DTYm)),
  max(nr(ECMd,ECMm)),#max(nr(EDINd,EDINm)),
  max(nr(ELMd,ELMm)),#max(nr(ELTAd,ELTAm)),
  max(nr(EMGd,EMGm)),
  max(nr(ERMd,ERMm)),max(nr(ESNTd,ESNTm)),max(nr(ESURd,ESURm)),max(nr(ETOd,ETOm)),max(nr(EVRd,EVRm)),
  max(nr(FCPTd,FCPTm)),max(nr(FCSSd,FCSSm)),max(nr(FDSAd,FDSAm)),#max(nr(FEVd,FEVm)),
  max(nr(FGPd,FGPm)),
  #max(nr(FGTd,FGTm)),
  #max(nr(FRCLd,FRCLm)),
  max(nr(GCPd,GCPm)),max(nr(GFRDd,GFRDm)),max(nr(GFSd,GFSm)),
  max(nr(GFTUd,GFTUm)),max(nr(GNCd,GNCm)),max(nr(GNKd,GNKm)),max(nr(GNSd,GNSm)),max(nr(GOGd,GOGm)),
  max(nr(GPORd,GPORm)),max(nr(GRGd,GRGm)),max(nr(GRId,GRIm)),max(nr(GSSd,GSSm)),max(nr(HASd,HASm)),
  max(nr(HFDd,HFDm)),max(nr(HGGd,HGGm)),max(nr(HICLd,HICLm)),max(nr(HIKd,HIKm)),max(nr(HLMAd,HLMAm)),
  max(nr(HMSFd,HMSFm)),max(nr(HOMEd,HOMEm)),max(nr(HSTNd,HSTNm)),max(nr(HSVd,HSVm)),max(nr(HSXd,HSXm)),
  #max(nr(HVPEd,HVPEm)),
  max(nr(HWDNd,HWDNm)),max(nr(IAPd,IAPm)),max(nr(ICPd,ICPm)),max(nr(IGGd,IGGm)),
  max(nr(IMId,IMIm)),max(nr(INCHd,INCHm)),max(nr(INPPd,INPPm)),max(nr(INVPd,INVPm)),max(nr(IPFd,IPFm)),
  max(nr(IPOd,IPOm)),max(nr(IRVd,IRVm)),#max(nr(JAMd,JAMm)),
  max(nr(JDd,JDm)),max(nr(JDWd,JDWm)),
  max(nr(JLIFd,JLIFm)),max(nr(JLTd,JLTm)),#max(nr(JMGd,JMGm)),
  max(nr(JRGd,JRGm)),max(nr(JUPd,JUPm)),
  max(nr(KAZd,KAZm)),max(nr(KIEd,KIEm)),max(nr(KLRd,KLRm)),max(nr(LADd,LADm)),max(nr(LMPd,LMPm)),
  max(nr(LOOKd,LOOKm)),max(nr(LRDd,LRDm)),max(nr(LREd,LREm)),#max(nr(MABd,MABm)),
  max(nr(MARSd,MARSm)),
  max(nr(MCROd,MCROm)),max(nr(MGAMd,MGAMm)),max(nr(MGGTd,MGGTm)),max(nr(MLCd,MLCm)),#max(nr(MNKSd,MNKSm)),
  max(nr(MONYd,MONYm)),max(nr(MPId,MPIm)),#max(nr(MRCd,MRCm)),
  max(nr(MROd,MROm)),max(nr(MSLHd,MSLHm))
)

b2 <- as.matrix(a2)
b2 <- b2[!is.na(b2)]
b2 <- as.matrix(b2)
#plot(density(b2))

c2 <- b2[!is.infinite(b2)]
c2 <- as.matrix(c2)
#plot(density(c2))

setwd("C:/Users/User/Desktop/Чина/CEU/2nd Year/2.Winter2016/Empirical Finance/Project/Data/FTSESmallCap")
temp <- list.files(pattern="*.csv")
list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), read.csv), envir = .GlobalEnv)

# STOCK EEE is 888 - i changed it from 888 to EEE.
a3 = c(
  #max(nr(AASd,AASm)),
  #max(nr(ABDd,ABDm)),
  max(nr(AEPd,AEPm)),#max(nr(AGITd,AGITm)),
  max(nr(AGRd,AGRm)),max(nr(ALYd,ALYm)),#max(nr(ANWd,ANWm)),
  max(nr(APFd,APFm)),max(nr(AQPd,AQPm)),
  max(nr(ARWd,ARWm)),max(nr(ATSd,ATSm)),#max(nr(AUKTd,AUKTm)),
  max(nr(AVONd,AVONm)),max(nr(B32d,B32m)),
  max(nr(BACTd,BACTm)),max(nr(BBGId,BBGIm)),#max(nr(BEEd,BEEm)),max(nr(BGFDd,BGFDm)),max(nr(BGSd,BGSm)),
  max(nr(BHYd,BHYm)),#max(nr(BIOGd,BIOGm)),
  max(nr(BKTd,BKTm)),max(nr(BMSd,BMSm)),max(nr(BMYd,BMYm)),
  max(nr(BPId,BPIm)),max(nr(BRAMd,BRAMm)),max(nr(BRFId,BRFIm)),max(nr(BRGEd,BRGEm)),#max(nr(BRSCd,BRSCm)),
  max(nr(BRWMd,BRWMm)),max(nr(BSIFd,BSIFm)),#max(nr(BUTd,BUTm)),
  max(nr(BVCd,BVCm)),max(nr(CALd,CALm)),
  max(nr(CARd,CARm)),#max(nr(CDId,CDIm)),max(nr(CGTd,CGTm)),
  max(nr(CHGd,CHGm)),max(nr(CIUd,CIUm)),
  max(nr(CKNd,CKNm)),max(nr(CLIGd,CLIGm)),max(nr(CMSd,CMSm)),max(nr(CNCTd,CNCTm)),max(nr(COSTd,COSTm)),
  max(nr(CPRd,CPRm)),max(nr(CSNd,CSNm)),max(nr(CSRTd,CSRTm)),max(nr(CTRd,CTRm)),max(nr(CVCd,CVCm)),
  max(nr(CYNd,CYNm)),max(nr(DABd,DABm)),#max(nr(DIAd,DIAm)),
  max(nr(DRTYd,DRTYm)),max(nr(DVOd,DVOm)),
  max(nr(ECWOd,ECWOm)),max(nr(EEEd,EEEm)),
  #max(nr(EFMd,EFMm)),
  max(nr(ENQd,ENQm)),#max(nr(EWId,EWIm)),
  max(nr(EXId,EXIm)),
  #max(nr(FASd,FASm)),
  max(nr(FAXd,FAXm)),max(nr(FDLd,FDLm)),max(nr(FENRd,FENRm)),#max(nr(FJVd,FJVm)),
  max(nr(FLYBd,FLYBm)),max(nr(FOURd,FOURm)),max(nr(FSTAd,FSTAm)),#max(nr(FSVd,FSVm)),
  max(nr(FXPOd,FXPOm)),
  #max(nr(GAWd,GAWm)),
  max(nr(GDWNd,GDWNm)),max(nr(GEMDd,GEMDm)),max(nr(GLEd,GLEm)),#max(nr(GPEd,GPEm)),
  max(nr(HDYd,HDYm)),#max(nr(HGTd,HGTm)),
  max(nr(HLCLd,HLCLm)),max(nr(HNTd,HNTm)),max(nr(HOCd,HOCm)),
  max(nr(HRGd,HRGm)),#max(nr(HRId,HRIm)),
  max(nr(HSDd,HSDm)),max(nr(IEMd,IEMm)),#max(nr(IMGd,IMGm)),
  max(nr(ITEd,ITEm)),#max(nr(JAId,JAIm)),max(nr(JESCd,JESCm)),max(nr(JETGd,JETGm)),
  max(nr(JFJd,JFJm)),
  #max(nr(JIId,JIIm)),
  max(nr(JPRd,JPRm)),#max(nr(JRSd,JRSm)),
  max(nr(KCOMd,KCOMm)),max(nr(KMRd,KMRm)),
  max(nr(LAMd,LAMm)),max(nr(LSLd,LSLm)),max(nr(MCBd,MCBm)),max(nr(MMCd,MMCm))#,max(nr(THRGd,THRGm))
)

b3 <- as.matrix(a3)
b3 <- b3[!is.na(b3)]
b3 <- as.matrix(b3)
#plot(density(b3))

c3 <- b3[!is.infinite(b3)]
c3 <- as.matrix(c3)
#plot(density(c3))

library(evd)
d <- dgumbel(1:100,loc=0,scale=1)
gum <- rgumbel(10000,loc=0,scale=1)
g <- density(gum)



#co1 <- (2*log(90))^(-0.5)
#de1 <- sqrt(2*log(90))-((log(4*pi)+log(log(90)))/(2*((2*log(90))^(0.5))))

#co2 <- (2*log(87))^(-0.5)
#de2 <- sqrt(2*log(87))-((log(4*pi)+log(log(87)))/(2*((2*log(87))^(0.5))))

#co3 <- (2*log(82))^(-0.5)
#de3 <- sqrt(2*log(82))-((log(4*pi)+log(log(82)))/(2*((2*log(82))^(0.5))))

#co4 <- (2*log(72))^(-0.5)
#de4 <- sqrt(2*log(72))-((log(4*pi)+log(log(72)))/(2*((2*log(72))^(0.5))))

#co5 <- (2*log(62))^(-0.5)
#de5 <- sqrt(2*log(62))-((log(4*pi)+log(log(62)))/(2*((2*log(62))^(0.5))))

#co6 <- (2*log(43))^(-0.5)
#de6 <- sqrt(2*log(43))-((log(4*pi)+log(log(43)))/(2*((2*log(43))^(0.5))))

d1 <- density(b)
#d1 <- density((b-de1)*co1)
d2 <- density(c)
#d2 <- density((c-de2)/(co2^(-1)))
d3 <- density(b2)
#d3 <- density((b2-de3)/(co3^(-1)))
d4 <- density(c2)
#d4 <- density((c2-de4)/(co4^(-1)))
d5 <- density(b3)
#d5 <- density((b3-de5)/(co5^(-1)))
d6 <- density(c3)
#d6 <- density((c3-de6)/(co6^(-1)))


plot(range(g$x,d1$x, d3$x, d5$x), range(g$y, d1$y, d3$y, d5$y), type = "n", xlab = "maxima",
     ylab = "Density", main = "Density of maxima of normalized log returns(centered and rescaled)")
lines(d1, col = "blue",lwd=4)
lines(d3, col = "red",lwd=4)
lines(d5, col = "orange",lwd=4)
lines(g, col = "black",lwd=4)

plot(range(g$x,d2$x, d4$x, d6$x), range(g$y, d2$y, d4$y, d6$y), type = "n", xlab = "maxima",
     ylab = "Density", main = "Density of maxima of normalized log returns(centered and rescaled)")
lines(d2, col = "blue",lwd=4)
lines(d4, col = "red",lwd=4)
lines(d6, col = "orange",lwd=4)
lines(g, col = "black",lwd=4)

