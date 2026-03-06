#Order stats
fun_stats<-function(){
  require(forcats)

  #Create subset of data
  stats<-FunStats[FunStats$sex==TRUE,]

  #Date
  #Get years
  for(i in 1:nrow(stats)){
    d<-strsplit(stats$date,"/")
  }
  years<-sapply(d, `[`, 3)
  #Get overall years
  for(i in 1:nrow(FunStats)){
    d<-strsplit(FunStats$date,"/")
  }
  years_c<-sapply(d, `[`, 3)
  #Bind years into matrix
  year<-matrix(0,3,length(unique(years_c)))
  year[1,1:length(unique(years))]<-summary(as.factor(years))
  year[2,1:length(unique(years[-length(years)]))]<-summary(as.factor(years[-length(years)]))
  year[3,]<-summary(as.factor(years_c))
  colnames(year)<-unique(years_c)
  rownames(year)<-c("Current","Before","All cases")
  #Print year stats
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("YEAR")
  print(year)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Country
  #Get factors
  countries<-fct_infreq(fct_inorder(as.factor(stats$country)))
  countries_b<-countries[-length(countries)]
  countries_c<-c(countries,as.factor(FunStats$country[FunStats$sex==FALSE]))
  #Bind countries into matrix
  country<-matrix(0,3,length(levels(countries_c)))
  colnames(country)<-levels(countries_c)
  rownames(country)<-c("Current","Before","All cases")
  country[1,]<-c(summary(countries),rep(0,ncol(country)-length(levels(countries))))
  country[2,]<-c(summary(countries_b),rep(0,ncol(country)-length(levels(countries_b))))
  country[3,]<-summary(countries_c)
  #Print country stats
  print("COUNTRY")
  print(country)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Ethnicity
  #Get factors
  ethnicities<-fct_infreq(fct_inorder(as.factor(stats$ethnicity)))
  ethnicities_b<-ethnicities[-length(ethnicities)]
  ethnicities_c<-c(ethnicities,as.factor(FunStats$ethnicity[FunStats$sex==FALSE]))
  #Bind ethnicities into matrix
  ethnicity<-matrix(0,3,length(levels(ethnicities_c)))
  colnames(ethnicity)<-levels(ethnicities_c)
  rownames(ethnicity)<-c("Current","Before","All cases")
  ethnicity[1,]<-c(summary(ethnicities),rep(0,ncol(ethnicity)-length(levels(ethnicities))))
  ethnicity[2,]<-c(summary(ethnicities_b),rep(0,ncol(ethnicity)-length(levels(ethnicities_b))))
  ethnicity[3,]<-summary(ethnicities_c)
  #Print ethnicity stats
  print("ETHNICITY")
  print(ethnicity)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Age
  #Create matrix
  age<-matrix(c(mean(stats$age,na.rm=TRUE),
                sd(stats$age,na.rm=TRUE),
                min(stats$age),
                max(stats$age),
                mean(stats$age[-nrow(stats)],na.rm=TRUE),
                sd(stats$age[-nrow(stats)],na.rm=TRUE),
                min(stats$age[-nrow(stats)]),
                max(stats$age[-nrow(stats)]),
                mean(FunStats$age,na.rm=TRUE),
                sd(FunStats$age,na.rm=TRUE),
                min(FunStats$age),
                max(FunStats$age)),
              3,4,byrow=TRUE)
  age<-round(age,2)
  colnames(age)=c("Mean","SDev","Lowest","Highest")
  rownames(age)=c("Current","Before","All cases")
  #Print age stats
  print("AGE")
  print(age)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Age gap
  #Create matrix
  agegap<-matrix(c(mean(stats$agegap,na.rm=TRUE),
                   sd(stats$agegap,na.rm=TRUE),
                   min(stats$agegap),
                   max(stats$agegap),
                   mean(stats$agegap[-nrow(stats)],na.rm=TRUE),
                   sd(stats$agegap[-nrow(stats)],na.rm=TRUE),
                   min(stats$agegap[-nrow(stats)]),
                   max(stats$agegap[-nrow(stats)]),
                   mean(FunStats$agegap,na.rm=TRUE),
                   sd(FunStats$agegap,na.rm=TRUE),
                   min(FunStats$agegap),
                   max(FunStats$agegap)),
                3,4,byrow=TRUE)
  agegap<-round(agegap,2)
  colnames(agegap)=c("Mean","SDev","Lowest","Highest")
  rownames(agegap)=c("Current","Before","All cases")
  #Print age gap stats
  print("AGE GAP")
  print(agegap)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Gender
  #Create factors
  g_fac<-c("Woman","Woman-adjacent","Non-binary","Man-adjacent","Man")
  genders<-factor(stats$gender,levels=g_fac)
  genders_b<-factor(stats$gender[-nrow(stats)],levels=g_fac)
  genders_c<-factor(FunStats$gender,levels=g_fac)
  #Bind genders into matrix
  gender<-matrix(c(summary(genders),summary(genders_b),summary(genders_c)),
                 3,5,byrow=TRUE)
  colnames(gender)<-g_fac
  rownames(gender)<-c("Current","Before","All cases")
  #Print genders
  print("GENDER")
  print(gender)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Genitals
  #Create factors
  p_fac<-c("Vulva","Penis","Other")
  genitals<-factor(stats$genitals,levels=p_fac)
  genitals_b<-factor(stats$genitals[-nrow(stats)],levels=p_fac)
  genitals_c<-factor(FunStats$genitals,levels=p_fac)
  #Bind genders into matrix
  genital<-matrix(c(summary(genitals),summary(genitals_b),summary(genitals_c)),
                 3,3,byrow=TRUE)
  colnames(genital)<-p_fac
  rownames(genital)<-c("Current","Before","All cases")
  #Print genders
  print("GENITALS")
  print(genital)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Cup size
  #Create matrix
  cupsize<-data.frame(Mean=c(mean(stats$cupsize,na.rm=TRUE),
                             mean(stats$cupsize[-nrow(stats)],na.rm=TRUE),
                             mean(FunStats$cupsize,na.rm=TRUE)),
                      Size=c(LETTERS[round(mean(stats$cupsize,na.rm=TRUE))],
                             LETTERS[round(mean(stats$cupsize[-nrow(stats)],na.rm=TRUE))],
                             LETTERS[round(mean(FunStats$cupsize,na.rm=TRUE))]),
                      SDev=c(sd(stats$cupsize,na.rm=TRUE),
                           sd(stats$cupsize[-nrow(stats)],na.rm=TRUE),
                           sd(FunStats$cupsize,na.rm=TRUE)))
  cupsize[,c(1,3)]<-round(cupsize[,c(1,3)],2)
  rownames(cupsize)=c("Current","Before","All cases")
  #Print cup size stats
  print("CUP SIZE")
  print(cupsize)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #First time
  #Create matrix
  firsttime<-data.frame(Count=c(sum(stats$firsttime),
                                sum(stats$firsttime[-nrow(stats)]),
                                sum(FunStats$firsttime)),
                        Percentage=c(sum(stats$firsttime)/nrow(stats),
                                     sum(stats$firsttime[-nrow(stats)])/(nrow(stats)-1),
                                     sum(FunStats$firsttime)/nrow(FunStats)))
  firsttime$Percentage<-paste0(firsttime$Percentage*100,"%")
  rownames(firsttime)=c("Current","Before","All cases")
  #Print first time
  print("FIRST TIME")
  print(firsttime)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Context
  #Create factors
  c_fac<-c("Intimate","Exploratory","Casual","One-Night-Stand","Professional")
  contexts<-factor(stats$context,levels=c_fac)
  contexts_b<-factor(stats$context[-nrow(stats)],levels=c_fac)
  contexts_c<-factor(FunStats$context,levels=c_fac)
  #Bind contexts into matrix
  context<-matrix(c(summary(contexts),summary(contexts_b),summary(contexts_c)),
                  3,5,byrow=TRUE)
  colnames(context)<-c_fac
  rownames(context)<-c("Current","Before","All cases")
  #Print contexts
  print("CONTEXT")
  print(context)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

  #Times
  #Create matrix
  times<-matrix(c(mean(stats$times,na.rm=TRUE),
                sd(stats$times,na.rm=TRUE),
                min(stats$times),
                max(stats$times),
                mean(stats$times[-nrow(stats)],na.rm=TRUE),
                sd(stats$times[-nrow(stats)],na.rm=TRUE),
                min(stats$times[-nrow(stats)]),
                max(stats$times[-nrow(stats)]),
                mean(FunStats$times,na.rm=TRUE),
                sd(FunStats$times,na.rm=TRUE),
                min(FunStats$times),
                max(FunStats$times)),
              3,4,byrow=TRUE)
  times<-round(times,2)
  colnames(times)=c("Mean","SDev","Lowest","Highest")
  rownames(times)=c("Current","Before","All cases")
  #Print time stats
  print("TIMES")
  print(times)
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}
