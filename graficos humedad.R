#### graficos humedad suelo intercapa###
install.packages("plotly")

###grafico precipitaciones puerto de galiz###
library(ggplot2)

library (plotly)
library(grid)
library(Rmisc)
getwd()
setwd("C:/Users/homet/Documents/moisture")



gamir <- read.table("gamir.txt", header=T , sep="\t")
gamir<-na.omit(gamir)
gamir$Date <- as.Date(gamir$Date, "%d/%m/%Y")
  
tablacgamir<- summarySE(gamir, measurevar="SWC", groupvars=c("Treatment","Date"))



###EMPEZAMOS####



##### separamos la base de datos por profundidades

g10 <- gamir[which(gamir$Depth=="10"),]# 10 cm
g20 <- gamir[which(gamir$Depth=="20"),]# 20 cm
g30 <- gamir[which(gamir$Depth=="30"),]# 30 cm
g40 <- gamir[which(gamir$Depth=="40"),]# 40 cm



#### 10 CM DE PROFUNDIDAD####
# Line plot with multiple groups
tabla10<- summarySE(g10, measurevar="SWC", groupvars=c("Treatment","Date"))





# Change line types + colors
graph10<-ggplot(tabla10, aes(x=Date, y=SWC, group=Treatment)) +
  geom_line(aes( color=Treatment), size=1.2)+
  
  geom_point(aes(color=Treatment),size=2)+
  theme(legend.position="top")+
  geom_errorbar(aes(ymin=SWC-se, ymax=SWC+se), width=15)+
  scale_color_manual(values=c('skyblue3',"red" ))+
  ylim(0,60)

  
sm10<-graph10 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))
  
 




smg10<-sm10 +theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


smg10<- smg10+theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels = "%b%y",date_breaks = "1 month")+
   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))+
  
  labs(title = "Soil moisture differences between climatic treatments ",
       subtitle = "10 cm depth",
       x = "Date", y = "SWC (%)")
  #+scale_y_continuous(expand = c(0, 0))


smg10a<-ggplotly(smg10)### convertimos el gráfico a plotly

###???precipitacion#####


precipitacion <- read.table("precipitacion.txt", header=T , sep="\t")
precipitacion$Date <- as.Date(precipitacion$Date, "%d/%m/%Y")
precipitacion <- precipitacion[-c(0:87),]


data2<-precipitacion
#names(data2)

#smg   ### OTRA MANERA DE PINTARLO QUE QUEDA MUY BIEN####
#par(new=TRUE)

#graph1<-tapply(data2$mm,data2$Date,I)



#ggplot(data = data2, aes(x = Date, y = mm)) +
 #geom_bar(stat = "identity", fill = "purple") +
#labs(title = "Total daily precipitation in Gamir, Cadiz",
 #   subtitle = "2016-2020",
  # x = "Date", y = "Daily Precipitation (mm)")+
  #scale_x_date(date_labels = "%b%y",date_breaks = "1 month")+
  #theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))

#barplot(graph,col=" purple",border=NA,xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,130))
#axis(4,las="1",cex.axis=0.7,font.axis=2)
#mtext("Rainfall (mm)",side=4,line=3,font=2,cex=0.8)+
 #scale_y_continuous(expand = c(0, 0))


#str(precipitacion)




####precipitacion por separado@####


precipitacion <- read.table("precipitacion.txt", header=T , sep="\t")
precipitacion$Date <- as.Date(precipitacion$Date, "%d/%m/%Y")
precipitacion <- precipitacion[-c(0:87),]



PrecipDaily <- ggplot(precipitacion, aes(Date, mm)) +
  geom_bar(stat="identity", na.rm = TRUE) +
  #ggtitle("Daily Precipitation\Los Alcornocales") +
  xlab("Date") + ylab("Precipitation (mm)") +
  scale_x_date(date_labels = "%b%y",date_breaks = "1 month")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),###eliminar la cuadricula del fondo
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))+
  labs(title = "Daily precipitation at the study site",
       subtitle = "2016-2020",
       x = "Date", y = "Daily precipitation (mm)")


PrecipDailya<-ggplotly(PrecipDaily)


  #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  #theme(plot.title = element_text(lineheight=.8, face="bold", size = 5)) +
  #theme(text = element_text(size=18))


###unimos los dos gráficos



grid.newpage()
grid.draw(rbind(ggplotGrob(smg10), ggplotGrob(PrecipDaily), size = "last"))

a10<-subplot(smg10a, PrecipDailya,shareX=TRUE, titleY=TRUE,titleX=TRUE, nrows=2) %>%


  layout(title = "Soil moisture at 10 cm depth")


       




#### 20 CM DE PROFUNDIDAD####
# Line plot with multiple groups
tabla20<- summarySE(g20, measurevar="SWC", groupvars=c("Treatment","Date"))






graph20<-ggplot(tabla20, aes(x=Date, y=SWC, group=Treatment)) +
  geom_line(aes( color=Treatment), size=1.2)+
  
  geom_point(aes(color=Treatment),size=2)+
  theme(legend.position="top")+
  geom_errorbar(aes(ymin=SWC-se, ymax=SWC+se), width=15)+
  scale_color_manual(values=c('skyblue3',"red" ))+
  ylim(0,60)


sm20<-graph20 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))






smg20<-sm20 +theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


smg20<- smg20+theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels = "%b%y",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))+
  labs(title = "Soil moisture differences between climatic treatments ",
       subtitle = "20 cm depth",
       x = "Date", y = "SWC (%)")

smg20a<-ggplotly(smg20)

###unimos PRECIPITACION CONN HUMEDAD 20####



grid.newpage()
grid.draw(rbind(ggplotGrob(smg20), ggplotGrob(PrecipDaily), size = "last"))

a20<-subplot(smg20a, PrecipDailya,shareX=TRUE, titleY=TRUE,titleX=TRUE, nrows=2)

#### 30 CM DE PROFUNDIDAD####
# Line plot with multiple groups
tabla30<- summarySE(g30, measurevar="SWC", groupvars=c("Treatment","Date"))






graph30<-ggplot(tabla30, aes(x=Date, y=SWC, group=Treatment)) +
  geom_line(aes( color=Treatment), size=1.2)+
  
  geom_point(aes(color=Treatment),size=2)+
  theme(legend.position="top")+
  geom_errorbar(aes(ymin=SWC-se, ymax=SWC+se), width=15)+
  scale_color_manual(values=c('skyblue3',"red" ))+
  ylim(0,60)


sm30<-graph30 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))






smg30<-sm30 +theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


smg30<- smg30+theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels = "%b%y",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))+
  
  labs(title = "Soil moisture differences between climatic treatments ",
       subtitle = "30 cm depth",
       x = "Date", y = "SWC (%)")

smg30a<-ggplotly(smg30)

###unimos PRECIPITACION CONN HUMEDAD 30####



grid.newpage()
grid.draw(rbind(ggplotGrob(smg30), ggplotGrob(PrecipDaily), size = "last"))

a30<-subplot(smg30a, PrecipDailya,shareX=TRUE, titleY=TRUE,titleX=TRUE, nrows=2)

#### 40 CM DE PROFUNDIDAD####
# Line plot with multiple groups
tabla40<- summarySE(g40, measurevar="SWC", groupvars=c("Treatment","Date"))






graph40<-ggplot(tabla40, aes(x=Date, y=SWC, group=Treatment)) +
  geom_line(aes( color=Treatment), size=1.2)+
  
  geom_point(aes(color=Treatment),size=2)+
  theme(legend.position="top")+
  geom_errorbar(aes(ymin=SWC-se, ymax=SWC+se), width=15)+
  scale_color_manual(values=c('skyblue3',"red" ))+
  ylim(0,60)


sm40<-graph40 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))






smg40<-sm40 +theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


smg40<- smg40+theme_set(theme_bw())+
  theme(legend.position=c(0.94,0.94), legend.justification=c(1,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels = "%b%y",date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))+


labs(title = "Soil moisture differences between climatic treatments ",
      subtitle = "40 cm depth",
      x = "Date", y = "SWC (%)")
     #scale_x_date(date_labels = "%b%y",date_breaks = "1 month")+

smg40a<-ggplotly(smg40)

###unimos PRECIPITACION CONN HUMEDAD 40####



grid.newpage()
grid.draw(rbind(ggplotGrob(smg40), ggplotGrob(PrecipDaily), size = "last"))


###todos juntos por profundidades ##### en principio no se ve bien asi.
grid.draw(rbind(ggplotGrob(smg10),ggplotGrob(smg20),ggplotGrob(smg30),ggplotGrob(smg40), ggplotGrob(PrecipDaily), size = "last"))
