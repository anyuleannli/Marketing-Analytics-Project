data1=read.csv("join_final.csv")
levels(data1$Race)
levels(data1$Ethnicity)
levels(data1$Fin.Class)

##For each ethnicity(Preferred Language), what kind of payment method(insurance type do they use)
summarydata1=data1%>%
  filter(Fin.Class!="")%>%
  filter(Race!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure,Fin.Class)%>%
  summarise(count=n())
  
data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  filter(Race!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure,Fin.Class)%>%
  summarise(count=n())%>%
  ggplot(aes(x=reorder(Fin.Class,count),y=count,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~Race,nrow=2)+
  xlab("Insurance Type")+
  ylab("Count")

##separate into 2 graphs
data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  filter(Type.of.Procedure=="Cosmetic")%>%
  filter(Race!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure,Fin.Class)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Fin.Class,y=count))+
  geom_col(position="dodge")+
  coord_flip()+
  theme_minimal()+
  xlab("Insurance Type")+
  ylab("Count")

data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  filter(Type.of.Procedure=="Cosmetic")%>%
  group_by(Type.of.Procedure,Fin.Class)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Fin.Class,y=count))+
  geom_col(position="dodge")+
  coord_flip()+
  theme_minimal()+
  xlab("Insurance Type")+
  ylab("Count")

data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  filter(Type.of.Procedure!="Cosmetic")%>%
  filter(Race!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure,Fin.Class)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Fin.Class,y=count))+
  geom_col(position="dodge")+
  coord_flip()+
  theme_minimal()+
  xlab("Insurance Type")+
  ylab("Count")

data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  filter(Type.of.Procedure!="Cosmetic")%>%
  group_by(Type.of.Procedure,Fin.Class)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Fin.Class,y=count))+
  geom_col(position="dodge")+
  coord_flip()+
  theme_minimal()+
  xlab("Insurance Type")+
  ylab("Count")

levels(data1$Fin.Class)
data1$Fin.Class=factor(data1$Fin.Class,levels=c("Patient Pay","Workmans Comp","MediCal",
                                              "Blue Shield","Patient Payment","Commercial","PPO","HMO","Medicare"
                                               ))

##don't group by patient id when seeing how many people are in each department as
##one patient can be performed surgery in different department 

data1 %>%
  filter(Race!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Race,Type.of.Procedure)%>%
  summarise(count=n())%>%
  ggplot(aes(x=Race,y=count, fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  ylab("Count")+
  xlab("Race")+
  theme_minimal()

##For each ethnicity, how much do they spent
data1$Chg.Amt=as.character(data1$Chg.Amt)
data1$Pay.Amt=as.character(data1$Pay.Amt)

data1$Chg.Amt=str_replace_all(data1$Chg.Amt,'\\$','')
data1$Chg.Amt=str_replace_all(data1$Chg.Amt,'\\,','')
data1$Pay.Amt=str_replace_all(data1$Pay.Amt,'\\$','')
data1$Pay.Amt=str_replace_all(data1$Pay.Amt,'\\,','')
data1$Chg.Amt=str_replace_all(data1$Chg.Amt,'\\-','')
data1$Pay.Amt=str_replace_all(data1$Pay.Amt,'\\-','')

data1$Chg.Amt=as.numeric(data1$Chg.Amt)
data1$Pay.Amt=as.numeric(data1$Pay.Amt)


summarydata2=data1%>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  filter(Race!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure,Fin.Class)%>%
  summarise(totalcharge=sum(Chg.Amt),totalpay=sum(Pay.Amt))

summarydata5=data1%>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  filter(Race!="")%>%
  group_by(Race)%>%
  summarise(averagecharge=sum(Chg.Amt)/n(),averagepay=sum(Pay.Amt)/n())


summarydata7=data1%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Type.of.Procedure)%>%
  summarise(averagecharge=sum(Chg.Amt)/n(),averagepay=sum(Pay.Amt)/n())

summarydata6=data1%>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Fin.Class)%>%
  summarise(averagecharge=sum(Chg.Amt)/n(),averagepay=sum(Pay.Amt)/n())
##test if the summarydata2 calculate the sum of payment correctly

data1%>%
  filter(Race=="Indian",Type.of.Procedure=="Cosmetic")%>%
  summarise(sum=sum(Chg.Amt))
test=summarydata2%>%
  filter(Race=="Indian",Type.of.Procedure=="Cosmetic")
sum(test$totalcharge)

##2 results are the same 
  
  
data1 %>%
  filter(Fin.Class!="")%>%
  filter(Race!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure,Fin.Class)%>%
  summarise(totalcharge=sum(Chg.Amt))%>%
  ggplot(aes(x=reorder(Fin.Class,totalcharge),y=totalcharge,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  facet_wrap(~Race,nrow=2)+
  ylab("Total Charge Amount in USD")+
  xlab("Insurance Type")+
  theme_minimal()+
  coord_flip()
data1$Fin.Class=factor(data1$Fin.Class,levels=c("Patient Pay","Patient Payment","Workmans Comp","PPO","MediCal",
                                                "Blue Shield","Commercial","HMO","Medicare"
))
data1 %>%
  filter(Race!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure)%>%
  summarise(totalcharge=sum(Chg.Amt))%>%
  ggplot(aes(x=Race,y=totalcharge,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  ylab("Total Charge Amount in USD")+
  theme_minimal()+
  xlab("Race")+
  coord_flip()

data1 %>%
  filter(Race!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Race,Type.of.Procedure)%>%
  summarise(averagecharge=sum(Chg.Amt)/n())%>%
  ggplot(aes(x=Race,y=averagecharge,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  ylab("Average Charge Amount in USD")+
  theme_minimal()+
  xlab("Race")+
  coord_flip()
levels(data1$Race)
data1$Race=factor(data1$Race,levels=c("Native American Indian","Greek","Indian","Black or African American","Pacific Islander","White","Hispanic","Hawaiian","Asian","American Indian or Alaska Native"
                                     ))

data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Fin.Class,Type.of.Procedure)%>%
  summarise(totalcharge=sum(Chg.Amt))%>%
  ggplot(aes(x=Fin.Class,y=totalcharge,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  ylab("Total Charge Amount in USD")+
  theme_minimal()+
  xlab("Insurance Plan")+
  coord_flip()

data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Fin.Class,Type.of.Procedure)%>%
  summarise(average=sum(Chg.Amt)/n())%>%
  ggplot(aes(x=Fin.Class,y=average,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  ylab("Average Charge Amount in USD")+
  theme_minimal()+
  xlab("Insurance Plan")+
  coord_flip()

summarydata6=data1 %>%
  filter(Fin.Class!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Fin.Class)%>%
  summarise(average=sum(Chg.Amt)/n())

data1$Fin.Class=factor(data1$Fin.Class,levels=c("Patient Pay","Workmans Comp","Patient Payment","PPO","Commercial","Blue Shield","HMO","MediCal",
                                                "Medicare"))
data1 %>%
  filter(Fin.Class!="")%>%
  filter(Race!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure,Fin.Class)%>%
  summarise(totalpay=sum(Pay.Amt))%>%
  ggplot(aes(x=reorder(Fin.Class,totalpay),y=totalpay,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  facet_wrap(~Race,nrow=2)+
  theme_minimal()+
  coord_flip()+
  ylab("Total Pay Amount in USD")+
  xlab("Insurance Type")

data1 %>%
  filter(Race!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Race,Type.of.Procedure)%>%
  summarise(totalpay=sum(Pay.Amt))%>%
  ggplot(aes(x=Race,y=totalpay,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  ylab("Total Pay Amount in USD")+
  theme_minimal()+
  xlab("Race")+
  coord_flip()


##see what kind of customers that brought the greatest value(race, surgery type, payment method)
## to the organization

##for race based on payment, see previous part

##surgery type

summarydata3=data1 %>%
  filter(Sv.It.Desc!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Sv.It.Desc,Type.of.Procedure)%>%
  summarise(totalpay=sum(Pay.Amt),totalcharge=sum(Chg.Amt))

summarydata4=data1 %>%
  filter(Sv.It.Desc!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Sv.It.Desc,Type.of.Procedure)%>%
  summarise(averagepay=sum(Pay.Amt)/n(),averagecharge=sum(Chg.Amt)/n())

data1 %>%
  filter(Sv.It.Desc!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Sv.It.Desc,Type.of.Procedure)%>%
  summarise(totalpay=sum(Pay.Amt))%>%
  ggplot(aes(x=Sv.It.Desc,y=totalpay,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  coord_flip()

data1 %>%
  filter(Sv.It.Desc!="")%>%
  filter(Type.of.Procedure!="")%>%
  group_by(Per.Nbr,Sv.It.Desc,Type.of.Procedure)%>%
  summarise(totalcharge=sum(Chg.Amt))%>%
  ggplot(aes(x=Sv.It.Desc,y=totalcharge,fill=Type.of.Procedure))+
  geom_col(position="dodge")+
  coord_flip()

