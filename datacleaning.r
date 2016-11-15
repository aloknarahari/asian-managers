#Download 2014 ACS PUMS Data

library(data.table)
ss14pusa <- fread("~/Downloads/csv_pus/ss14pusa.csv")
ss14pusb <- fread("~/Downloads/csv_pus/ss14pusb.csv")
ss14husa = fread("~/Downloads/csv_hus/ss14husa.csv")
ss14husb = fread("~/Downloads/csv_hus/ss14husb.csv")

#Merge the persons datasets

acs = rbind(ss14pusa , ss14pusb)
save(acs, file= "acs.rda")

#Merge the housing datasets

house = rbind(ss14husa, ss14husb)

#Merge Housing and Persons
acsmerge = merge(acs , house , by = c("SERIALNO"))

#Data Set of Only Asians and White People
acs1 = acsmerge[acsmerge$RAC1P== 1 | acsmerge$RAC1P == 6 ,]
save(acs1 , file = "acs1.rda")

#Data Set People 25 to 65 - acs2

acs2 = acs1[acs1$AGEP>24 & acs1$AGEP <66,]

#Data Set of Only Employed People 

acs3 = acs2[acs2$ESR == 1 | acs2$ESR ==2 , ]

#DATA SET FOR NATIVE BORNS 

acs4 = acs3[acs3$CIT == 1, ]

#DATA SET FOR IMMIGRANTS 

acs5 = acs3[acs3$CIT == 4 | acs3$CIT ==5 , ]

#Data Set for White v. Asian Blinder Decomposition

acs.blinder = subset(acs3 , select = c("asian" ,  "AGEP" , "WKHP" , 
                                       "some.college" ,
                                       "bachelors" , "highschool" , "advanced.deg" ,
                                       "other.lang" ,"south" 
                                       ,"northeast" ,"midwest" ,"west" ,
                                       "self.employed" , "manager" , 'eng.notwell' , "tech"))

#Data Set for Asian A v. Asian B Blinder

blinder.asian = subset(acs3 , asian == 1, select = c("asian.B" ,  "AGEP" , "WKHP" , 
                                                     "some.college" ,
                                                     "bachelors" , "highschool" , "advanced.deg" ,
                                                     "other.lang" ,"south" 
                                                     ,"northeast" ,"midwest" ,"west" ,
                                                     "self.employed" , "manager" , 'eng.notwell' 
                                                     , "tech"))
# *** DUMMIES***

#Asian Dummy
acs3$asian = 0                   
acs3$asian[acs3$RAC1P == 6] = 1

acs3$white = 0                   
acs3$white[acs3$RAC1P == 1] = 1

#Manager Dummy 
acs3$manager = 0
acs3$manager[(acs3$OCCP == 10 | acs3$OCCP == 20 | acs3$OCCP == 40
              | acs3$OCCP == 50 | acs3$OCCP == 60  | acs3$OCCP == 100
              | acs3$OCCP == 110 | acs3$OCCP == 110 | acs3$OCCP == 120
              | acs3$OCCP == 135 | acs3$OCCP == 136 | acs3$OCCP == 137
              | acs3$OCCP == 140 | acs3$OCCP == 150 | acs3$OCCP == 160
              | acs3$OCCP == 205 | acs3$OCCP == 220 | acs3$OCCP == 230
              | acs3$OCCP == 300 | acs3$OCCP == 310 | acs3$OCCP == 330
              | acs3$OCCP == 340 | acs3$OCCP == 350 | acs3$OCCP == 360
              | acs3$OCCP == 410 | acs3$OCCP == 420 | acs3$OCCP == 425
              | acs3$OCCP == 430)] = 1

#Other Language Dummy
acs3$other.lang = 0
#Yes, other language is spoken at home. 
acs3$other.lang[ acs3$LANX == 1] = 1


#Education Dummy 
#high school
acs3$highschool = 0
acs3$highschool[acs3$SCHL == 16 |acs3$SCHL == 17 ] = 1

#some college
acs3$some.college = 0
acs3$some.college[acs3$SCHL == 18 | acs3$SCHL ==19] = 1

#bachelors
acs3$bachelors = 0
acs3$bachelors[acs3$SCHL == 21] = 1

#advanced degree
acs3$advanced.deg = 0
acs3$advanced.deg[acs3$SCHL == 22 |acs3$SCHL == 23 |acs3$SCHL == 24] = 1

#DUMMY BY RACE

#Asian A - chinese japanese filipino Indian taiwanese
acs3$asian.A = 0
acs3$asian.A[acs3$RAC2P == 38 | acs3$RAC2P == 43 | acs3$RAC2P == 45 
             | acs3$RAC2P == 48 | acs3$RAC2P == 44] = 1
#Asian B - korean, vietnamese, cambodians, others (bangladeshi, buthanese, burmese, 
#hmong, indonesian, laotian, malaysian, mongolain, nepalese, pakistani, sri lankan,
#thai, other.)
acs3$asian.B = 0
acs3$asian.B[acs3$RAC2P == 42 | acs3$RAC2P == 49 | acs3$RAC2P == 57
             | acs3$RAC2P == 39 |acs3$RAC2P == 40 | acs3$RAC2P == 41 | 
               acs3$RAC2P == 46 | acs3$RAC2P == 47 | acs3$RAC2P == 50 |
               acs3$RAC2P == 51 | acs3$RAC2P == 52 | acs3$RAC2P == 53 | 
               acs3$RAC2P == 54 | acs3$RAC2P == 55 | acs3$RAC2P == 56 | 
               acs3$RAC2P == 58] = 1

#Region Dummies
#SOUTH
acs3$south = 0
acs3$south[acs3$REGION == 3] = 1

#Northeast
acs3$northeast = 0
acs3$northeast[acs3$REGION == 1] = 1

#Midwest
acs3$midwest = 0
acs3$midwest[acs3$REGION == 2] = 1

#West
acs3$west = 0
acs3$west[acs3$REGION == 4] = 1

#ASIAN A VS ASIAN B

sum(acs3$asian.A)
sum(acs3$asian.B)

#Self-Employed Dummy

acs3$self.employed = 0
acs3$self.employed[acs3$COW == 6 | acs3$COW == 7] = 1

#English Proficiency Dummy 

acs3$eng.well = 0
acs3$eng.well[acs3$ENG ==1 & acs3$CIT ==1 ] = 1

acs3$eng.notwell = 0
acs3$eng.notwell[(acs3$ENG == 2 | acs3$ENG == 3 | acs3$ENG ==4)] = 1

#Industry Dummy

acs3$tech = 0
acs3$tech[( acs3$NAICSP == 3254 | acs3$NAICSP == 3341 | acs3$NAICSP == "334M2" |
              acs3$NAICSP == "334M1" | acs3$NAICSP == 3345| acs3$NAICSP == "335M" |
              acs3$NAICSP == 3391 | acs3$NAICSP == 5112 | acs3$NAICSP == "517Z" |
              acs3$NAICSP == 5182 | acs3$NAICSP == 5412 | acs3$NAICSP == 5415 |
              acs3$NAICSP == 5416 | acs3$NAICSP == 3254 | acs3$NAICSP == 5417 |  
              acs3$NAICSP == 5418 | acs3$NAICSP == 5613 | acs3$NAICSP == 5614 )] = 1 










