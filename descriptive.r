


# DEMOGRAPHICS
#By race
NROW(na.omit(acs2$RAC1P[acs2$RAC1P == 1])) #number whites 
NROW(na.omit(acs2$RAC1P[acs2$RAC1P == 6])) #number of asians

#By Sex 
NROW(na.omit(acs2$RAC1P[acs2$RAC1P == 6 & acs2$SEX ==1])) #asian men
NROW(na.omit(acs2$RAC1P[acs2$RAC1P == 6 & acs2$SEX ==2])) #asian women
NROW(na.omit(acs2$RAC1P[acs2$RAC1P == 1 & acs2$SEX ==2])) #white women
NROW(na.omit(acs2$RAC1P[acs2$RAC1P == 1 & acs2$SEX ==1])) #white men

#By age
mean(acs3$AGEP[acs3$RAC1P ==1] , na.rm = TRUE) #average white age
mean(acs3$AGEP[acs3$RAC1P ==6] , na.rm = TRUE) #average asian age

#Where Born
NROW(acs3$CIT[(acs3$CIT == 1) & acs3$RAC1P == 1]) #Native Born White
NROW(acs3$CIT[(acs3$CIT == 1) & acs3$RAC1P == 6]) #Native Born Asian

NROW(acs3$CIT[(acs3$CIT == 4 | acs3$CIT == 5) & acs3$RAC1P == 1]) #immigrant White
NROW(acs3$CIT[(acs3$CIT == 4 | acs3$CIT == 5) & acs3$RAC1P == 6]) #Immigrant Asian

NROW(acs2$CIT[(acs2$CIT == 2) & acs2$RAC1P == 1]) #US Territory
NROW(acs2$CIT[(acs2$CIT == 2) & acs2$RAC1P == 6]) #US Territory

NROW(acs2$CIT[(acs3$CIT == 3) & acs2$RAC1P == 1]) #Abroad to US parents
NROW(acs2$CIT[(acs2$CIT == 3) & acs2$RAC1P == 6]) #Abroad to US parents

#Employment rates

NROW(na.omit(acs2$ESR[(acs2$ESR ==1 | acs2$ESR ==2) & acs2$asian ==0])) #employed whites
NROW(na.omit(acs2$ESR[(acs2$ESR ==1 | acs2$ESR ==2) & acs2$asian ==1])) #employed asians

NROW(na.omit(acs2$ESR[(acs2$ESR ==3) & acs2$asian ==0])) #unemployed whites
NROW(na.omit(acs3$ESR[(acs3$ESR ==3) & acs2$asian ==1])) #unemployed asians

NROW(na.omit(acs2$ESR[(acs2$ESR ==6) & acs2$asian ==0])) #not in labor force whites
NROW(na.omit(acs2$ESR[(acs2$ESR ==6) & acs2$asian ==1])) #not in labor force asians

#Wages

mean(acs2$WAGP[acs2$asian ==0] , na.rm = TRUE)
mean(acs2$WAGP[acs2$asian == 1] , na.rm = TRUE)

#Hours Worked 

summary(acs3$WKHP[acs3$asian == 0])
summary(acs3$WKHP[acs3$asian == 1])

#Managers Hours Worked

summary(acs3$WKHP[acs3$asian == 0 & acs3$manager ==1 ])
summary(acs3$WKHP[acs3$asian == 1 & acs3$manager ==1 ])

plot(density(acs3$WKHP[acs3$asian == 0 & acs3$manager ==1 ]) , 
     xlim = c(40,90), col = "blue")
lines(density(acs3$WKHP[acs3$asian == 1 & acs3$manager ==1 ]) , 
      xlim = c(40,90), col = "red")

ggplot(subset(acs3, manager ==1), aes(x = WKHP)) +   
  geom_density(aes(group = factor(asian), colour = factor(asian) ,
                   fill = factor(asian)) , alpha = 0.2  , size = 1.1) +
  labs(x = "Hours" , y = "Density") + xlim(41,90)

ggplot(acs3, aes(x = WKHP)) +   
  geom_density(aes(group = factor(asian), colour = factor(asian) ,
                   fill = factor(asian)) , alpha = 0.2  , size = 1.1) +
  labs(x = "Hours" , y = "Density") + xlim(41,90)


#Weeks Worked

NROW(na.omit(acs3$WKW[acs3$asian == 1 & (acs3$WKW == 1  | acs3$WKW == 2)])) /66660
NROW(na.omit(acs3$WKW[acs3$asian ==1 & acs3$WKW == 3])) /66660
NROW(na.omit(acs3$WKW[acs3$asian ==1 & acs3$WKW == 4])) /66660
NROW(na.omit(acs3$WKW[acs3$asian ==1 & (acs3$WKW == 5 | acs3$WKW == 6)])) /66660

#Manager Weeks Worked

NROW(na.omit(acs3$WKW[acs3$asian == 0 & acs3$manager ==1 & (acs3$WKW == 1  | acs3$WKW == 2)])) /115117
NROW(na.omit(acs3$WKW[acs3$asian ==0 & acs3$manager ==1 & acs3$WKW == 3])) /115117
NROW(na.omit(acs3$WKW[acs3$asian ==0 & acs3$manager ==1 & acs3$WKW == 4])) /115117
NROW(na.omit(acs3$WKW[acs3$asian ==0 & acs3$manager ==1 & (acs3$WKW == 5 | acs3$WKW == 6)])) /115117


#Education

NROW(na.omit(acs2$SCHL[acs2$highschool ==1 & acs2$asian ==1 ]))
NROW(na.omit(acs2$SCHL[acs2$some.college ==1 & acs2$asian ==1 ]))
NROW(na.omit(acs2$SCHL[acs2$bachelors ==1 & acs2$asian ==1 ]))
NROW(na.omit(acs2$SCHL[acs2$advanced.deg ==1 & acs2$asian ==1 ]))

#Closs of Worker
#self employed whites
NROW(na.omit(acs2$ESR[(acs2$ESR ==1 | acs2$ESR ==2) & acs2$asian ==0 & 
                        (acs2$COW == 6 | acs2$COW ==7) ])) 
NROW(na.omit(acs2$ESR[(acs2$ESR ==1 | acs2$ESR ==2) & acs2$asian ==1 & 
                        (acs2$COW == 6 | acs2$COW ==7) ])) 

# MANAGER POSITIONS

sum(acs3$manager[acs3$asian ==0]) #white managers

sum(acs3$manager[acs3$asian ==1]) #asian managers

#nativity

sum(acs3$manager[acs3$CIT == 1 & acs3$asian == 0]) /115117

sum(acs3$manager[(acs3$CIT == 4 | acs3$CIT == 5) & acs3$asian == 1]) /115117 #immigrant asian manager


#asians who speak other language at home
sum(acs3$manager[ acs3$asian == 0 & acs3$other.lang == 1]) / 115117 #other language asian

#Managers Spousal Status
NROW(na.omit(acs2$MAR[acs2$manager == 1 & acs2$MAR == 1 & acs2$asian == 1]))

#Schooling 
#some college, no degree
NROW(na.omit(acs3$SCHL[acs3$manager ==1 & acs3$asian ==0 &
  (acs3$SCHL == 18 | acs3$SCHL ==19) ]))
#bachelors 
NROW(na.omit(acs3$SCHL[acs3$manager ==1 & acs3$asian ==1 &
acs3$SCHL == 21 ]))
#above bachelors 
NROW(na.omit(acs3$SCHL[acs3$manager ==1 & acs3$asian ==1 &
(acs3$SCHL == 22 |acs3$SCHL == 23 |acs3$SCHL == 24) ]))
#high school
NROW(na.omit(acs3$SCHL[acs3$manager ==1 & acs3$asian ==0 &
(acs3$SCHL == 16 |acs3$SCHL == 17 ) ]))

#Wages for managers 
summary(acs3$WAGP[acs3$manager == 1 & acs3$asian ==0 & acs3$WAGP != 0 ])
#Wages for non-manager
summary(acs3$WAGP[ acs3$asian ==1 & acs3$WAGP != 0 ])

#Ages 
summary(acs3$AGEP[acs3$manager == 1 & acs3$asian ==0])
summary(acs3$AGEP[acs3$manager == 1 & acs3$asian ==1])


#Other Languages 
NROW(na.omit(acs2$other.lang[acs2$other.lang == 1 & acs2$manager == 1 &
acs2$asian == 0 ]))


#employed
NROW(na.omit(acs2$ESR[(acs2$ESR ==1 | acs2$ESR ==2) & acs2$asian.A ==1]))

#Age
summary(acs2$AGEP[acs2$asian.A == 1])


save(acs2 , file = "acs2.rda")
save(acs3, file="acs3.rda")

#Employed Education

NROW(na.omit(acs3$manager[acs3$asian == 0 & acs3$bachelors == 1 & acs3$manager ==1 ])) /115117
NROW(na.omit(acs3$manager[acs3$asian == 0 & acs3$advanced.deg ==1 & acs3$manager ==1])) /115117
NROW(na.omit(acs3$manager[acs3$asian == 0 & acs3$some.college ==1 & acs3$manager ==1 ])) /115117
NROW(na.omit(acs3$manager[acs3$asian == 0 & acs3$highschool ==1 & acs3$manager ==1])) /115117


#self-employed manager

sum(acs3$self.employed[ acs3$manager ==1 & acs3$asian == 0 & acs3$self.employed ==1]) / 115117

#English Proficiency

NROW(na.omit(acs3$ENG[acs3$asian == 1 & acs3$ENG ==1 ])) /51785
NROW(na.omit(acs3$ENG[acs3$asian == 1 & acs3$ENG ==2 ])) /51785
NROW(na.omit(acs3$ENG[acs3$asian == 1 &  (acs3$ENG ==4 | acs3$ENG ==3)])) /51785

#managers 
NROW(na.omit(acs3$ENG[acs3$asian == 1 & acs3$ENG ==1 & acs3$manager == 1])) /5369
NROW(na.omit(acs3$ENG[acs3$asian == 1 & acs3$ENG ==2 & acs3$manager == 1 ])) /5369
NROW(na.omit(acs3$ENG[acs3$asian == 1 &  (acs3$ENG ==4 | acs3$ENG ==3) & 
                        acs3$manager ==1])) /5369

#Asian A v. Asian B

NROW(na.omit(acs3$asian.A[acs3$asian.A ==1 ]))

#Wages
summary((acs3$WAGP[acs3$asian.B ==1 ]) , na.rm == TRUE)

#managers
NROW(na.omit(acs3$manager[acs3$asian.B ==1 & acs3$manager == 1])) / 19097

#education
sum(acs3$highschool[acs3$asian.A == 1]) / 46320
sum(acs3$bachelors[acs3$asian.A == 1]) / 46320
sum(acs3$some.college [acs3$asian.A == 1]) / 46320
sum(acs3$advanced.deg[acs3$asian.A == 1]) / 46320

#age
summary((acs3$AGEP[acs3$asian.A ==1 ]) , na.rm == TRUE)
