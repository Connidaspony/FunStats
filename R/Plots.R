plot_date<-function(){
  require(ggplot2)
  p<-ggplot(FunStats[FunStats$sex==TRUE,],aes(x=as.Date(date,format="%d/%m/%Y")))+
    geom_bar(fill="darkgreen")+
    labs(title="New partners over time")+
    theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),axis.title.x=element_blank(),
          panel.background=element_rect(fill="black"),
          panel.grid.minor.y=element_blank())
  return(p)
}


plot_country<-function(weighted=FALSE){
  require(forcats)
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  f<-fct_infreq(fct_inorder(as.factor(FunStats$country)))
  p<-ggplot(FunStats,aes(x=factor(country,levels=levels(f))))+
    geom_bar(fill="red")+
    labs(title="Frequency of countries")+
    xlab("Country")+
    ylab("Count")+
    theme_test(ink="yellow")+
    theme(panel.background=element_rect(fill="yellow"),
          plot.background=element_rect(fill="blue"))
  return(p)
}


plot_ethnicity<-function(weighted=FALSE){
  require(forcats)
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  f<-fct_infreq(fct_inorder(as.factor(FunStats$ethnicity)))
  p<-ggplot(FunStats,aes(x=factor(ethnicity,levels=levels(f))))+
    geom_bar(fill="orange")+
    labs(title="Frequency of ethnicities")+
    xlab("Ethnicity")+
    ylab("Count")+
    theme_test(ink="white")+
    theme(panel.background=element_rect(fill="white"),
          plot.background=element_rect(fill="brown"))
  return(p)
}


plot_age<-function(weighted=FALSE){
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  m<-mean(FunStats$age)
  s<-sd(FunStats$age)
  p<-ggplot(FunStats,aes(x=age))+
    geom_histogram(bins=diff(range(FunStats$age)),
                   fill="lightblue",colour="black")+
    geom_vline(xintercept=m,colour="red",lwd=2)+
    geom_segment(x=m-s,y=0,xend=m+s,yend=0,colour="red",lwd=1)+
    labs(title="Age distribution")+
    xlab("Age")+
    ylab("Count")+
    scale_x_continuous(breaks=seq(min(FunStats$age),
                                  max(FunStats$age),1))+
    scale_y_continuous(breaks=seq(0,max(summary(as.factor(FunStats$age))),1))+
    theme_test()+
    theme(panel.grid=element_blank(),
          panel.background=element_rect(fill="orange"))
  return(p)
}


plot_age_time<-function(){
  require(ggplot2)
  p<-ggplot(FunStats[FunStats$sex==TRUE,],aes(x=number,y=age))+
    geom_line(colour="orange")+
    geom_point(colour="black")+
    geom_smooth(method="lm",se=FALSE,colour="red",linetype=2)+
    labs(title="Age over time")+
    scale_y_continuous(breaks=seq(18,max(FunStats$age[FunStats$sex==TRUE]),1))+
    ylab("Age")+
    theme_test()+
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),panel.grid=element_blank(),
          panel.grid.major.y=element_line(colour="white"),
          panel.background=element_rect(fill="lightblue"))
  return(p)
}


plot_agegap<-function(weighted=FALSE){
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  m<-mean(FunStats$agegap)
  s<-sd(FunStats$agegap)
  h<-max(c(-min(FunStats$agegap),
           max(FunStats$agegap)))
  p<-ggplot(FunStats,aes(x=agegap))+
    geom_histogram(bins=h*2+3,fill="purple",colour="black")+
    geom_vline(xintercept=m,colour="red",lwd=2)+
    geom_segment(x=m-s,y=0,xend=m+s,yend=0,colour="red",lwd=1)+
    labs(title="Age gap distribution")+
    xlab("Age Gap")+
    ylab("Count")+
    scale_x_continuous(breaks=seq(-h,h,1),limits=c(-h-1,h+1))+
    scale_y_continuous(breaks=seq(0,max(summary(as.factor(FunStats$agegap))),1))+
    theme_test()+
    theme(panel.grid=element_blank(),
          panel.background=element_rect(fill="darkblue"),
          plot.background=element_rect(fill="orange"))
  return(p)
}

plot_agegap_time<-function(){
  require(ggplot2)
  h<-max(c(-min(FunStats$agegap[FunStats$sex==TRUE]),
           max(FunStats$agegap[FunStats$sex==TRUE])))
  p<-ggplot(FunStats[FunStats$sex==TRUE,],aes(x=number,y=agegap))+
    geom_line(colour="darkblue")+
    geom_point(colour="black")+
    geom_smooth(method="lm",se=FALSE,colour="red",linetype=2)+
    labs(title="Age gap over time")+
    scale_y_continuous(breaks=seq(-h,h,1))+
    ylab("Age Gap")+
    theme_test()+
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),panel.grid=element_blank(),
          panel.grid.major.y=element_line(colour="grey"),
          panel.background=element_rect(fill="purple"),
          plot.background=element_rect(fill="orange"))
  return(p)
}


plot_gender<-function(weighted=FALSE){
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  l<-c("Woman","Woman-adjacent","Non-binary","Man-adjacent","Man")
  p<-ggplot(FunStats,aes(x=factor(gender,levels=l)))+
    geom_bar(fill="yellow")+
    labs(title="Frequency of genders")+
    xlab("Gender")+
    ylab("Count")+
    scale_x_discrete(limits=l)+
    theme_test(ink="white")+
    theme(panel.background=element_rect(fill="black"),
          plot.background=element_rect(fill="purple"))
  return(p)
}


plot_genitals<-function(weighted=FALSE){
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  l<-c("Vulva","Penis","Other")
  p<-ggplot(FunStats,aes(x=factor(genitals,levels=l)))+
    geom_bar(fill="white")+
    labs(title="Frequency of genitals")+
    xlab("Genitals")+
    ylab("Count")+
    scale_x_discrete(limits=l)+
    theme_test(ink="black")+
    theme(panel.background=element_rect(fill="brown"),
          plot.background=element_rect(fill="pink"))
  return(p)
}


plot_cupsize<-function(weighted=FALSE){
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  m<-mean(FunStats$cupsize)
  s<-sd(FunStats$cupsize)
  p<-ggplot(FunStats,aes(x=cupsize))+
    geom_histogram(bins=max(FunStats$cupsize)+3,
                   fill="green",colour="white")+
    geom_vline(xintercept=m,colour="blue",lwd=2)+
    geom_segment(x=m-s,y=0,xend=m+s,yend=0,colour="blue",lwd=1)+
    labs(title="Cup size distribution")+
    xlab("Cup Size")+
    ylab("Count")+
    scale_x_continuous(breaks=seq(0,max(FunStats$cupsize),1),
                       limits=c(-1,max(FunStats$cupsize)+1),
                       labels=c("AA",LETTERS[1:max(FunStats$cupsize)]))+
    scale_y_continuous(breaks=seq(0,max(summary(as.factor(FunStats$cupsize))),1))+
    theme_test(ink="white")+
    theme(panel.grid=element_blank(),
          panel.background=element_rect(fill="purple"),
          plot.background=element_rect(fill="black"))
  return(p)
}


plot_cupsize_time<-function(){
  require(ggplot2)
  p<-ggplot(FunStats[FunStats$sex==TRUE,],aes(x=number,y=cupsize))+
    geom_line(colour="purple")+
    geom_point(colour="purple")+
    geom_smooth(method="lm",se=FALSE,colour="blue",linetype=2)+
    labs(title="Cup size over time")+
    scale_y_continuous(breaks=seq(0,max(FunStats$cupsize[FunStats$sex==TRUE]),1),
                       limits=c(0,max(FunStats$cupsize[FunStats$sex==TRUE])),
                       labels=c("AA",LETTERS[1:max(FunStats$cupsize[FunStats$sex==TRUE])]))+
    ylab("Cup Size")+
    theme_test(ink="white")+
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),panel.grid=element_blank(),
          panel.grid.major.y=element_line(colour="white"),
          panel.background=element_rect(fill="green"),
          plot.background=element_rect(fill="black"))
  return(p)
}


plot_firsttime<-function(){
  require(ggplot2)
  d<-data.frame(ft=c("Yes","No"),c=c(sum(FunStats$firsttime[FunStats$sex==TRUE]),
                                     sum(FunStats$firsttime[FunStats$sex==TRUE]==FALSE)))
  p<-ggplot(d,aes(x="",y=c,fill=ft))+
    geom_bar(stat="identity")+
    coord_polar("y",start=0)+
    labs(title="Proportion of first times",fill="First Time")+
    theme_void(ink="white")+
    theme(plot.background=element_rect(fill="black"))+
    scale_fill_brewer(palette="Set1")
  return(p)
}


plot_context<-function(weighted=FALSE){
  require(ggplot2)
  require(tidyr)
  FunStats<-FunStats[FunStats$sex==TRUE,]
  if(weighted==TRUE){
    FunStats<-uncount(FunStats,times)
  }
  l<-c("Intimate","Exploratory","Casual","One-Night-Stand","Professional")
  p<-ggplot(FunStats,aes(x=factor(context,levels=l)))+
    geom_bar(fill="red")+
    labs(title="Frequency of contexts")+
    xlab("Context")+
    ylab("Count")+
    scale_x_discrete(limits=l)+
    theme_test(ink="white")+
    theme(panel.background=element_rect(fill="black"),
          plot.background=element_rect(fill="darkred"))
  return(p)
}


plot_times<-function(){
  require(ggplot2)
  m<-mean(FunStats$times[FunStats$sex==TRUE])
  s<-sd(FunStats$times[FunStats$sex==TRUE])
  p<-ggplot(FunStats[FunStats$sex==TRUE,],aes(x=times))+
    geom_histogram(binwidth=10,fill="grey",colour="black")+
    geom_vline(xintercept=m,colour="lightgreen",lwd=2)+
    geom_segment(x=m-s,y=0,xend=m+s,yend=0,colour="lightgreen",lwd=1)+
    labs(title="Times distribution")+
    xlab("Times")+
    ylab("Count")+
    theme_test(ink="black")+
    theme(panel.grid=element_blank(),
          panel.background=element_rect(fill="white"),
          plot.background=element_rect(fill="lightblue"))
  return(p)
}


plot_times_time<-function(){
  require(ggplot2)
  p<-ggplot(FunStats[FunStats$sex==TRUE,],aes(x=number,y=times))+
    geom_line(colour="white")+
    geom_point(colour="white")+
    geom_smooth(method="lm",se=FALSE,colour="lightgreen",linetype=2)+
    labs(title="Times over time")+
    ylab("Times")+
    theme_test(ink="lightblue")+
    theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),panel.grid=element_blank(),
          panel.grid.major.y=element_line(colour="black"),
          panel.background=element_rect(fill="grey"),
          plot.background=element_rect(fill="black"))
  return(p)
}
