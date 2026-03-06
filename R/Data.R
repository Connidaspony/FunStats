#Open document
get_data<-function(){
  FunStats<-read.csv2("C:/Users/conni/OneDrive/Desktop/FunStats.csv")
  assign("FunStats",FunStats,pos=1)
}

#Save document
save_data<-function(){
  setwd("C:/Users/conni/OneDrive/Desktop")
  write.csv2(FunStats,"FunStats.csv",row.names=FALSE)
}

#Update count
add_count<-function(name,count){
  if(sum(FunStats$name==name)==0){
    stop("Name not found.")
  }else if(sum(FunStats$name==name)>1){
    stop("More than one person with that name.")
  }else{
  FunStats[FunStats$name==name,"times"]<<-FunStats[FunStats$name==name,"times"]+count
  }
}

#Edit other stats
edit_data<-function(name,stat,new){
  if(sum(FunStats$name==name)==0){
    stop("Name not found.")
  }else if(sum(FunStats$name==name)>1){
    stop("More than one person with that name.")
  }else{
    FunStats[FunStats$name==name,stat]<<-new
  }
}

#Add new person
add_person<-function(name,date,country,ethnicity,age,agegap,gender,genitals,
                     cupsize,firsttime,context,sex){
  if(sex==TRUE){
    num<-max(FunStats$number)+1
  }else{
    num<-0
  }
  p<-list("number"=num,"name"=name,"date"=date,"country"=country,
          "ethnicity"=ethnicity,"age"=age,"agegap"=agegap,"gender"=gender,
          "genitals"=genitals,"cupsize"=cupsize,"firsttime"=firsttime,
          "context"=context,"sex"=sex,"times"=1)
  FunStats<<-rbind(FunStats,p)
}
