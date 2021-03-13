#README:

#Name: MAWT_Figure

#Published in : MAWTROLL

#Description : 'Plot all figures'

#See Also : myFUN_roll_lasso, roll_ws_lasso, roll_lasso, MAWT.mean, vare,Netden
#Author : Xingmin Zhang

#Datafile : BANK RETURN AND MACRO CAPM.csv, idio_part.csv, total_in_and_out_idio_424.csv, total_network_scale_424


  rm(list=ls())
  libraries = c("reshape2")
  lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
  })
  lapply(libraries, library, quietly = TRUE, character.only = TRUE)


 # Fig.2. four Global systemic important banks
  data  = read.csv("BANK RETURN AND MACRO CAPM.csv", header = TRUE);
  str(data);

  SIBs = data[,c(6:9)];# the four systemic important banks in the FOB list
  str(SIBs);

  png("Fig.2.The return of G-SIBs.png", units="in", width=35, height=18, res=300);
   par(mar=c(20,10,6,6));
   plot(SIBs[,1],type="l",lty=1,cex.lab = 3.5, cex.axis = 3.5,lwd = 7,ylab="",xlab="", xaxt='n',ylim=c(-0.2,0.2),col="green");
   xtick = c(1,86,197,315,423,546,654,776,892,1015,1130,1253,1369,1491,1608,1731,1843,1964,2056);
   axis(side=1,at=xtick,labels = c("","2011","","2012","","2013","","2014",
                                "","2015","",
                                "2016","","2017","","2018","","2019",""),
     cex.axis = 3.5,font=1,lwd.ticks = 5,mgp=c(1.8,3,0));
   par(new=T);         
   plot(SIBs[,2],type="l",lty=1,lwd = 7, xaxt='n',yaxt='n',ann = FALSE,ylim=c(-0.2,0.2),col="blue4")
   par(new=T);
   plot(SIBs[,3],type="l",lty=1,lwd = 7, xaxt='n', yaxt='n',ann = FALSE,ylim=c(-0.2,0.2),col="aquamarine3")
   par(new=T);         
   plot(SIBs[,4],type="l",lty=1,lwd = 7, xaxt='n',yaxt='n',ann = FALSE,ylim=c(-0.2,0.2),col="brown1")
   legend(x="topleft",legend = c("BOC","ICBC","CCB","ABC"), 
       lty=c(1,1,1,1),col=c("green","blue4","aquamarine3","brown1"), text.font=1,lwd = 7,
       cex=3,seg.len=1.5,x.intersp=1,y.intersp=0.95,xjust=0.5,adj=c(0,0.1),text.width=4.5,bty="n");
  dev.off();


# fig.3
  Rs = read.csv(file="R_square.csv")
  str(Rs);
  png("Fig3.R-Square.png", units="in", width=30, height=15, res=300);
   par(mar=c(5,5,3,3),mfrow=c(1,1));
   barplot(rev(Rs[,3]),horiz=T,xlim=c(-0.2,0.7),ylim=c(1,30.5),axes=F,col="royalblue3",
             border=NA,space=0.6)
   text(seq(from=1,length.out=20,by=1.6),x=-0.025,label=rev(Rs[,2]),cex = 2.15)
   axis(1,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),c('0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8'),cex.axis=3);
   abline(v = 0, col="darkgray");
  dev.off();


# Fig.4 
  # plot the figures of raw stock return and idiosyncratic parts
  idiosyn = read.csv(file = paste("idio_part.csv"))[,-1];
  str(idiosyn);
  SIBs_idio = idiosyn[,c(5:8)];# the four systemic important banks in the FOB list
  str(SIBs_idio);

  data = read.csv("BANK RETURN AND MACRO CAPM.csv", header = TRUE);
  str(data);
  SIBs = data[,c(6:9)];# the four systemic important banks in the FOB list
  str(SIBs);
  SIBs_common = SIBs[,1]-SIBs_idio[,1];
  png("Fig.4. The return of BOC_raw and idio.png", units="in", width=35, height=18, res=300);
   par(mar=c(8.1,8.1,8.1,8.1));
   plot(SIBs[,1],type="l",lty=1,cex.lab = 3.5,cex.axis = 3.5,lwd = 7,ylab="",xlab="", xaxt='n',col="blue");
   xtick = c(1,86,197,315,423,546,654,776,892,1015,1130,1253,1369,1491,1608,1731,1843,1964,2056);
   axis(side=1,at=xtick,labels = c("","2011","","2012","","2013","","2014",
                                "","2015","",
                                "2016","","2017","","2018","","2019",""),
     cex.axis = 3.5,font=1,lwd.ticks = 5,mgp=c(1.8,3,0));
  par(new=T);   
  plot(SIBs_idio[,1],type="l",lty=3,lwd = 7,ylab="",xlab="", xaxt='n',yaxt='n',col="darkgreen");
  par(new=T);  
  plot(SIBs_common,type="l",lty=2,lwd = 7,ylab="",xlab="", xaxt='n',yaxt='n',col="red");
  legend(x="topleft",legend = c("total return","idiosyncratic part","common part"), 
       lty=c(1,3,2),col=c("blue","darkgreen","red"), text.font=1,lwd = 7,
       cex=3,seg.len=1.5,x.intersp=1,y.intersp=0.95,xjust=0.5,adj=c(0,0.1),text.width=4.5,bty= "n");
 dev.off();



# Fig.5 network density
  tn424 = as.matrix(read.csv(file = "total_network_scale_424.csv"))
  str(tn424)
  png("Fig.5 netden_idio.png", units="in", width=35, height=18, res=300);
   par(mar=c(5,5,6,6));
   plot(tn424,type="p",lty=1,cex.lab = 3.5,cex.axis = 3.5,lwd = 7,ylab="",xaxt='n',xlab="",col="darkred");
   lines(smooth.spline(tn424, spar = 0.6),lty=1, lwd = 7, col="black",xaxt="n");
   xtick = c(1,123,231,353,469,592,707,830,946,1068,1185,1308,1420,1541);
   axis(side=1,at=xtick,labels = c("","2013","","2014",
                                "","2015","",
                                "2016","","2017","","2018","","2019"),
     cex.axis=3.5,font=1,lwd.ticks=5,mgp=c(1.8,3,0));
   dev.off();


# Fig.6. boxplot
  # plot the figures of in and out degree
  # tc_out = cbind(total.in.b, total.out.b)
  outin = read.csv(file = "total_in_and_out_idio_424.csv")[,-1];
  str(outin);
# the first 20 cols are in and then out
  indegree = outin[, 5:8];# boc,icbc,ccb,abc
  str(indegree);
  outdegree = outin[, 25:28];
  str(outdegree);
  hbdegree = cbind(indegree,outdegree);

# boxplot
  png("Fig6.boxplot_G_SIBs_in_out_degree.png", units="in", width=30, height=15, res=300);
  par(mar=c(10,9,3,3),mgp=c(1.8,3,0));
  boxplot(hbdegree, 
        xlab = "",
        ylab = "", 
        main = "",
        names = c("BOC-in","ICBC-in","CCB-in","ABC-in","BOC-out","ICBC-out","CCB-out","ABC-out"),
        cex.lab=3.5, cex.axis=3.5,lwd = 7,
        col = c("lightgreen","lightgreen","lightgreen","lightgreen","darkgray","darkgray","darkgray","darkgray"),
        border = c("darkgreen","darkgreen","darkgreen","darkgreen","brown","brown","brown","brown"),notch = TRUE
  )
 dev.off();


# Fig7. heatmaps of the total network
  tot.ct2 = read.csv(file = "tot_c_overtime_idio_424.csv"); 
  str(tot.ct2);
  tot.ct = as.matrix(t(tot.ct2));
  str(tot.ct);
  data   = read.csv("BANK RETURN AND MACRO CAPM.csv", header = TRUE)[,c(2:21)];
  str(data);
  nam    = colnames(data);

  colnames(tot.ct) = colnames(data)
  rownames(tot.ct) = colnames(tot.ct)
  tot.ct;

# heatmaps of the total network
  png("Fig7.heatmap of total network.png", units="in", width=30, height=15, res=300);
   par(mar=c(6,6,3,3));
   melted_cormat <- reshape2::melt(tot.ct);
   ggplot(data = melted_cormat, aes(x= Var1, y=Var2)) + 
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(),axis.text=element_text(size=25,face="bold"))+
    theme(axis.title.y = element_blank(),axis.text=element_text(size=25,face="bold"))+ 
    scale_fill_gradient('value', limits=c(0, 680), breaks = c(0,150,300,450,600),low = "white", high = "blue1")+ 
    theme(legend.text = element_text(size=20),legend.key.size = unit(1, "in"),legend.title=element_text(size=25))+
    guides(fill = guide_colourbar(barwidth = 4, barheight = 10))
  dev.off();


# Fig8.compare different rolling window sizes
# plot the figures
# in order to compare the different rolling window size, we scale the total network degree first
   tole = read.csv( file = "total_network_scale_all.csv",head=T)[,-1];
   str(tole);

   tol115 = as.numeric(tole[-c(1:114),1]);
   tol230 = as.numeric(tole[-c(1:229),2]);
   tol424 = as.numeric(tole[-c(1:423),3]);
   tol460 = as.numeric(tole[-c(1:459),4]);
   bum115 = length(tol115);
   bum230 = length(tol230);
   bum424 = length(tol424);
   bum460 = length(tol460);

  png("Fig8.all_network_density.png", units="in", width=35, height=18, res=300);
   par(mar=c(5,5,6,6));
   plot(tol115,xlim=c(1,bum115),type="n",lty=1,cex.lab = 3.5,cex.axis = 3.5,lwd = 7,ylab="",xaxt='n',xlab="");
   lines(smooth.spline(tol115, spar = 0.7),lty=2, lwd = 7, col="green",xaxt="n");
   xtick = c(115,197,315,423,546,654,776,892,1015,1130,1253,1369,1491,1608,1731,1843,1964)-rep(115,17);
   axis(side=1,at=xtick,labels = c("2011.02","2011.06","2012.01","2012.06","2013.01","2013.06","2014.01",
                                "2014.06","2015.01","2015.06",
                                "2016.01","2016.06","2017.01","2017.06","2018.01","2018.06","2019.01"),
     cex.axis=3,font=1,lwd.ticks=3,mgp=c(1.8,3,0));
   par(new=T);         
   plot(tol230,type="n",xlim=c(bum230-1941,bum230),lty=2,cex.lab = 3.5,cex.axis = 3.5,lwd = 7, xaxt='n',yaxt='n',ann = FALSE)
   lines(smooth.spline(tol230, spar = 0.7),lty=1, lwd = 7,col="blue4",xaxt="n");
   par(new=T);
   plot(tol424,type="n",xlim=c(bum424-1941,bum424),lty=1,cex.lab = 3.5,cex.axis = 3.5,lwd = 7, xaxt='n', yaxt='n',ann = FALSE)
   lines(smooth.spline(tol424, spar = 0.7),lty=1, lwd = 7, col="brown1",xaxt="n");
   par(new=T);         
   plot(tol460,type="n",xlim=c(bum460-1941,bum460),lty=2,cex.lab = 3.5,cex.axis = 3.5,lwd = 7, xaxt='n',yaxt='n',ann = FALSE)
   lines(smooth.spline(tol460, spar = 0.7),lty=2, lwd = 7, col="aquamarine3",xaxt="n");
   legend(x="topleft",legend = c("half-year","one-year","our method","two-year"), 
       lty=c(2,1,1,2),col=c("green","blue4","brown1","aquamarine3"), text.font=1,lwd = 3,
       cex=3.5,seg.len=1.5,x.intersp=1,y.intersp=0.95,xjust=0.5,adj=c(0,0.1),text.width=4.5,bty="n");
   dev.off();











