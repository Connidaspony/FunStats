#Order weighted stats
fun_stats_weighted<-function(){
  require(forcats)
  require(tidyr)
  
  #Create subset of data
  stats<-FunStats[FunStats$sex==TRUE,]
  stats<-uncount(stats,times)
  
  #Country
  #Get factor
  country<-fct_infreq(fct_inorder(as.factor(stats$country)))
  
  #Print country stats
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("COUNTRY")
  print(summary(country))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Ethnicity
  #Get factor
  ethnicity<-fct_infreq(fct_inorder(as.factor(stats$ethnicity)))

  #Print ethnicity stats
  print("ETHNICITY")
  print(summary(ethnicity))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Age
  #Create matrix
  age<-matrix(c(mean(stats$age,na.rm=TRUE),
                sd(stats$age,na.rm=TRUE),
                min(stats$age),
                max(stats$age)),
              1,4,byrow=TRUE)
  age<-round(age,2)
  colnames(age)=c("Mean","SDev","Lowest","Highest")
  
  #Print age stats
  print("AGE")
  print(age)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Age gap
  #Create matrix
  agegap<-matrix(c(mean(stats$agegap,na.rm=TRUE),
                   sd(stats$agegap,na.rm=TRUE),
                   min(stats$agegap),
                   max(stats$agegap)),
                1,4,byrow=TRUE)
  agegap<-round(agegap,2)
  colnames(agegap)=c("Mean","SDev","Lowest","Highest")
  
  #Print age gap stats
  print("AGE GAP")
  print(agegap)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Gender
  #Create factor
  g_fac<-c("Woman","Woman-adjacent","Non-binary","Man-adjacent","Man")
  gender<-factor(stats$gender,levels=g_fac)

  #Print genders
  print("GENDER")
  print(summary(gender))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Genitals
  #Create factor
  p_fac<-c("Vulva","Penis","Other")
  genitals<-factor(stats$genitals,levels=p_fac)

  #Print genitals
  print("GENITALS")
  print(summary(genitals))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Cup size
  #Create matrix
  cupsize<-data.frame(Mean=mean(stats$cupsize,na.rm=TRUE),
                      Size=LETTERS[round(mean(stats$cupsize,na.rm=TRUE))],
                      SDev=sd(stats$cupsize,na.rm=TRUE))
  cupsize[,c(1,3)]<-round(cupsize[,c(1,3)],2)

  #Print cup size stats
  print("CUP SIZE")
  print(cupsize)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Context
  #Create factor
  c_fac<-c("Intimate","Exploratory","Casual","One-Night-Stand","Professional")
  context<-factor(stats$context,levels=c_fac)

  #Print contexts
  print("CONTEXT")
  print(summary(context))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}
