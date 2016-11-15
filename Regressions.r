#Regressions on White and Asians 

#Basic Model
reg1 = glm(acs3$manager ~ acs3$asian , family = binomial(link = "logit"))

#Education and Work Combined
reg2 = glm(acs3$manager ~ acs3$asian + acs3$AGEP + acs3$WKHP + acs3$self.employed + 
            +acs3$eng.notwell + acs3$other.lang + acs3$highschool + acs3$some.college 
           + acs3$advanced.deg + acs3$tech,
           family = binomial(link = "logit"))

#Regional Controls 
reg3 = glm(acs3$manager ~ acs3$asian + acs3$AGEP + acs3$WKHP + acs3$self.employed 
           + acs3$eng.notwell   + acs3$other.lang + acs3$highschool+ acs3$some.college 
           + acs3$advanced.deg + acs3$tech 
           + acs3$south + acs3$northeast + acs3$midwest, 
           family = binomial(link = "logit"))

#Interaction Term
reg4 = glm(acs3$manager ~ acs3$asian + acs3$AGEP + acs3$WKHP + acs3$self.employed 
           + acs3$eng.notwell   + acs3$other.lang + acs3$highschool+ acs3$some.college 
           + acs3$advanced.deg + acs3$tech 
           + acs3$other.lang:acs3$asian
           + acs3$tech:acs3$asian
           + acs3$south + acs3$northeast + acs3$midwest, 
           family = binomial(link = "logit"))


#Asian A v. Asian B
reg5 = glm(acs3$manager ~ acs3$asian.A + acs3$asian.B,
           family = binomial(link = "logit"))


#Asian A v. Asian B and controls 
reg6 = glm(acs3$manager ~ acs3$asian.A + acs3$asian.B + acs3$AGEP + acs3$WKHP 
           + acs3$self.employed + acs3$eng.notwell   + acs3$other.lang
           + acs3$highschool+ acs3$some.college 
           + acs3$advanced.deg + acs3$tech
           + acs3$south + acs3$northeast + acs3$midwest,
           family = binomial(link = "logit"))

#Interaction Terms 
reg7 = glm(acs3$manager ~ acs3$asian.A + acs3$asian.B + acs3$AGEP + acs3$WKHP 
           + acs3$self.employed 
           + acs3$eng.notwell + acs3$other.lang + acs3$highschool+ acs3$some.college 
           + acs3$advanced.deg + acs3$tech
           +acs3$other.lang:acs3$asian.A + acs3$other.lang:acs3$asian.B
           +acs3$tech:acs3$asian.A + acs3$tech:acs3$asian.B
           + acs3$south + acs3$northeast + acs3$midwest,
           family = binomial(link = "logit"))



#library(stargazer)

stargazer( reg1 , reg2 , reg3 , reg4 , reg5 , reg6 , reg7,
           apply.coef = exp,
           type = "html" , 
           out = "table.htm" , style = "aer" ,
           title = "Manager liklihood Regression" , digits.extra = 3)

#Regression on Native born populations 
#Basic Model
reg1 = glm(acs4$manager ~ acs4$asian , family = binomial(link = "logit"))

#Education and Work Combined
reg2 = glm(acs4$manager ~ acs4$asian + acs4$AGEP + acs4$WKHP + acs4$self.employed + 
             +acs4$eng.notwell + acs4$other.lang + acs4$highschool + acs4$some.college 
           + acs4$advanced.deg + acs4$tech,
           family = binomial(link = "logit"))

#Regional Controls 
reg3 = glm(acs4$manager ~ acs4$asian + acs4$AGEP + acs4$WKHP + acs4$self.employed 
           + acs4$eng.notwell   + acs4$other.lang + acs4$highschool+ acs4$some.college 
           + acs4$advanced.deg + acs4$tech 
           + acs4$south + acs4$northeast + acs4$midwest, 
           family = binomial(link = "logit"))

#Interaction Term
reg4 = glm(acs4$manager ~ acs4$asian + acs4$AGEP + acs4$WKHP + acs4$self.employed 
           + acs4$eng.notwell   + acs4$other.lang + acs4$highschool+ acs4$some.college 
           + acs4$advanced.deg + acs4$tech 
           + acs4$other.lang:acs4$asian
           + acs4$tech:acs4$asian
           + acs4$south + acs4$northeast + acs4$midwest, 
           family = binomial(link = "logit"))


#Asian A v. Asian B
reg5 = glm(acs4$manager ~ acs4$asian.A + acs4$asian.B,
           family = binomial(link = "logit"))


#Asian A v. Asian B and controls 
reg6 = glm(acs4$manager ~ acs4$asian.A + acs4$asian.B + acs4$AGEP + acs4$WKHP 
           + acs4$self.employed + acs4$eng.notwell   + acs4$other.lang
           + acs4$highschool+ acs4$some.college 
           + acs4$advanced.deg + acs4$tech
           + acs4$south + acs4$northeast + acs4$midwest,
           family = binomial(link = "logit"))

#Interaction Terms 
reg7 = glm(acs4$manager ~ acs4$asian.A + acs4$asian.B + acs4$AGEP + acs4$WKHP 
           + acs4$self.employed 
           + acs4$eng.notwell + acs4$other.lang + acs4$highschool+ acs4$some.college 
           + acs4$advanced.deg + acs4$tech
           +acs4$other.lang:acs4$asian.A + acs4$other.lang:acs4$asian.B
           +acs4$tech:acs4$asian.A + acs4$tech:acs4$asian.B
           + acs4$south + acs4$northeast + acs4$midwest,
           family = binomial(link = "logit"))




#library(oaxaca)

#White v. Asian Oaxaca
oaxaca = oaxaca( manager ~ AGEP + WKHP + self.employed +some.college + bachelors 
        +advanced.deg + other.lang + eng.notwell + tech + south + northeast + midwest 
        | asian, 
        data= acs.blinder, R = NULL, reg.fun = glm , family = binomial(link = "logit"))

#Asian A v.s Asian B Oaxaca
oaxaca2 = oaxaca( manager ~ AGEP + WKHP + self.employed + some.college + bachelors
                 +advanced.deg + other.lang + eng.notwell + tech + south + northeast + midwest 
                 | asian.B, 
                 data= blinder.asian, R = NULL, reg.fun = glm , 
                 family = binomial(link = "logit"))





