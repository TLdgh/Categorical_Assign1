


#Table 1 
Bnhanes<-nhanes
Bnhanes<-mutate(Bnhanes,BMI_level = case_when(
  Bnhanes$bmi < 18.5 ~"Underweight",
  Bnhanes$bmi < 25 ~"Healthy weight",
  Bnhanes$bmi < 30 ~"Overweight",
  Bnhanes$bmi >= 30 ~"Obesity "))%>%
  filter(!is.na(Bnhanes$bmi))

label(Bnhanes$gender) <- "Gender"
label(Bnhanes$age)<-"Age"
label(Bnhanes$marstat)<-"Marital Status"
label(Bnhanes$samplewt)<-"Statistical Weight"
label(Bnhanes$psu)<-"Pseudo-PSU"
label(Bnhanes$strata)<-"Pseudo-stratum"
label(Bnhanes$tchol)<-"Total Cholesterol"
label(Bnhanes$hdl)<-"HDL-Cholesterol"
label(Bnhanes$sysbp)<-"Systolic Blood Pressure"
label(Bnhanes$dbp)<-"Diastolic Blood Pressure "
label(Bnhanes$wt)<-"Weight"
label(Bnhanes$ht)<-"Standing Height"
label(Bnhanes$bmi)<-"Body mass Index"
label(Bnhanes$vigwrk)<-"Vigorous Work Activity"
label(Bnhanes$modwrk)<-"Moderate Work Activity "
label(Bnhanes$wlkbik)<-"Walk or Bicycle "
label(Bnhanes$vigrecexr)<-"Vigorous Recreational Activities "
label(Bnhanes$modrecexr)<-"Moderate Recreational Activities"
label(Bnhanes$sedmin)<-"Minutes of Sedentary Activity per Week "
label(Bnhanes$obese)<-"Obese "

units(Bnhanes$age) <- "years"
units(Bnhanes$tchol) <- "mg/dL"
units(Bnhanes$hdl) <- "mg/dL"
units(Bnhanes$sysbp) <- "mm Hg"
units(Bnhanes$dbp) <- "mm Hg"
units(Bnhanes$wt) <- "Kg"
units(Bnhanes$ht) <- "cm"
units(Bnhanes$bmi) <- "Kg/m^2"
units(Bnhanes$sedmin) <- "mins"

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("",
                                                           "Mean (SD)"=sprintf("%s (± %s)", MEAN, SD)))
}

tbl<-table1 (~ gender+age+marstat+samplewt+psu+strata+tchol+hdl+sysbp+dbp+wt+ht+vigwrk+modwrk+wlkbik+vigrecexr+modrecexr+sedmin+obese |BMI_level,render.continuous=my.render.cont, data=Bnhanes)
kable(tbl,"html",caption="Characteristics of the data set NHANES")%>%
  kable_styling(bootstrap_options = "striped", full_width = F)

#exploratory average weight in every age grouped by gender
gender<-nhanes%>%
  group_by(gender,age)%>%
  summarise(Avgwt=mean(wt,na.rm=TRUE), .groups="drop")
ggplot(gender,aes(x=age, y=Avgwt), group=gender)+
  geom_point(aes(colour=gender),size=1 )+
  geom_line(aes(fill=gender,colour=gender,linetype=gender) )+
  geom_hline(yintercept = 86.2, linetype="dotted", size = 0.5)+
  geom_hline(yintercept = 74.9, linetype="dotted", size = 0.5)+
  annotate("text", y=86.2, x=18, label=" Male Avg.")+
  annotate("text", y=74.9, x=18, label="Female Avg.")+
  scale_y_continuous( labels = scales::comma  ) +
  theme(axis.text.x = element_text(size =7, vjust = 0.5, hjust = 0.5, angle = 45))+
  
  labs(x="Age", y="Average weight (Kg)")+
  theme(plot.title = element_text(hjust = 0.5))

# Categorize age 
Catenhanes<-nhanes%>%
  mutate(agegroup=case_when(
    age<18~"Adolescents",
    age<65~"Adults",
    age>=65~"Older Adults"),
    BMI_level=case_when(
      bmi <18.5 ~ "Underweight", 
      bmi>=18.5 & bmi<=24.9 ~ "Healthy",
      bmi>24.9 & bmi<=29.9 ~ "Overweight",
      bmi>29.9  ~ "Obesity")
  )%>%
  filter(!is.na(bmi))
Catenhanes$agegroup<-factor(Catenhanes$agegroup,levels=c("Adolescents","Adults","Older Adults"),labels=c("Adolescents","Adults","Older Adults"))
Catenhanes$BMI_level<-factor(Catenhanes$BMI_level,levels=c("Underweight", "Healthy","Overweight","Obesity" ),labels=c("Underweight", "Healthy","Overweight","Obesity"))

IndepTest<-setRefClass(
  "IndepTest",
  fields = list(data="data.frame", variable_x="character", reference_y="character",pval_df="data.frame",ContingencyTbl="list" ),
  methods=list(
    initialize=function(data, variable_x, reference_y){
      #make a contingency table for the variable X and Y
      .self$ContingencyTbl<-.self$create_list(data, variable_x, reference_y)
      
      #calculate the pvalues and make a data frame
      .self$pval_df<- .self$get_pvalue(my_list=ContingencyTbl)
    },
    
    create_list=function(data, variable_x, reference_y){
      my_list <- list()
      for(x in variable_x){
        my_list[[x]]<-data%>%select(all_of(c(x, reference_y)))%>%na.omit()%>%
          group_by(across(everything(), as.factor))%>%tally()%>%spread(key=reference_y, value=n)}
      return(my_list)
    }, 
    
    get_pvalue=function(my_list){
      #do the chi-squared test
      pvalues<-map(my_list, function(x){
        if(any(is.na(x))==FALSE){
          y<-ungroup(x)%>%select(-any_of(group_vars(x)))%>%as.matrix()%>%chisq.test()
          return(y$p.value)
        }else{return("Has no values in Contingency Table")}
      })
      
      #format it nicely
      pvalues<-pvalues%>%data.frame(row.names = "p-value")%>%
        mutate(across(where(is.numeric), .fns = ~as.character(signif(.,4))))
      
      return(pvalues)}
  )
)

#contingency table: BMI vs agegroup
testBA<-IndepTest$new(Catenhanes, "BMI_level", "agegroup")
tableBA<-testBA$ContingencyTbl$BMI_level%>%
  rename("\t" =BMI_level) %>%
  add_column(" " = c("BMI level", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Contingency Table for BMI and Agegroup")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width = F,position="float_left")%>%
  add_header_above(c(" " = 1, " " = 1, "Agegroup" = 3))%>%
  column_spec(1:2,width = "6em",bold=TRUE)
tableBA

#contingency table: BMI vs Gender
testBG<-IndepTest$new(Catenhanes, "BMI_level", "gender")
tableBG<-testBG$ContingencyTbl$BMI_level%>%
  rename("\t" =BMI_level) %>%
  add_column(" " = c("BMI level", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Contingency Table for BMI and Gender")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width = F,position="left")%>%
  add_header_above(c(" " = 1, " " = 1, "Gender" = 2))%>%
  column_spec(1:2,width = "8em",bold=TRUE)
tableBG 

#likelihood ratio test (age)
ba<-Catenhanes%>%
  group_by(BMI_level,agegroup)%>%
  summarise(count=n(),.groups="drop")
BA<-matrix(ba$count,nrow=3,ncol=4)
totalcol=c((17+86+21),(188+1322+343),(74+1536+518),(63+1765+512))
totalrow=c((17+188+74+63),(86+1322+1536+1765),(21+343+518+512),sum(BA))
BAT<-rbind(BA,totalcol)
BAT<-cbind(BAT,totalrow)
rownames(BAT)=c("Adolescents","Adults","Older Adults","Total")
colnames(BAT)=c("Underweight", "Healthy","Overweight","Obesity","Total")
propBAT<-round(BAT/6445,4)

P_i<-matrix(propBAT[,5])
P_j<-matrix(propBAT[4,])
Exp<-round((P_i%*%t(P_j))*6445,4)
ExpBA<-as.data.frame(Exp[1:3,1:4])
ObsBA<-as.data.frame(BA)
G_value=2*sum(ObsBA*log(ObsBA/ExpBA))
PA=format(pchisq(G_value, df=6, lower.tail=FALSE), digits = 4)

#likelihood ratio test (gender)
bg<-Catenhanes%>%
  group_by(BMI_level,gender)%>%
  summarise(count=n(),.groups="drop")
BG<-matrix(bg$count,nrow=2,ncol=4)
totalcol=c((40+84),(884+969),(1168+960),(1052+1288))
totalrow=c((40+884+1168+1052),(84+969+960+1288),sum(BG))
BGT<-rbind(BG,totalcol)
BGT<-cbind(BGT,totalrow)
rownames(BGT)=c("Male","Female","Total")
colnames(BGT)=c("Underweight", "Healthy","Overweight","Obesity","Total")
propBGT<-round(BGT/6445,4)

p_i<-matrix(propBGT[,5])
p_j<-matrix(propBGT[3,])
exp<-round((p_i%*%t(p_j))*6445,4)
ExpBG<-as.data.frame(exp[1:2,1:4])
ObsBG<-as.data.frame(BG)
G_value=2*sum(ObsBG*log(ObsBG/ExpBG))
PG=format(pchisq(G_value, df=3, lower.tail=FALSE), digits = 4)

#Chi-square table
ptbl<-data.frame(testBA$pval_df, testBG$pval_df)
colnames(ptbl)<-c("Weight VS Age ", "Weight VS.Gender")
ptbl%>%
  kable(caption = "Chi-square test p-values of Independence Tests between Variables") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),full_width = F,position = "float_left")

#LRT table
PA=as.character(PA)
PG=as.character(PG)
ltbl<-data.frame(PA,PG)
colnames(ltbl)<-c("Weight VS Age ", "Weight VS.Gender")
rownames(ltbl)<-"p-value"
ltbl%>%
  kable(caption = "Likelihood ratio test p-values of Independence Tests between Variables") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),full_width = F,position = "float_left")

#box plot for weight VS marital status
Nhanes<-nhanes%>%
  na.omit()
ggplot(Nhanes, aes(x=marstat, y=wt, fill=marstat))+
  geom_boxplot() + 
  scale_fill_brewer(palette="Blues") +
  labs(x="Marital Status", y="Weight (Kg)")+
  theme(axis.text.x = element_text(size =7, vjust = 0.5, hjust = 0.5, angle = 45),
        plot.title = element_text(hjust = 0.5)
  )

test_marstat<-IndepTest$new(nhanes, "marstat", "obese")
test_marstat$ContingencyTbl$marstat%>%
  rename("\t" = marstat) %>%
  add_column(" " = c("Marital Status", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Contingency Table")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),position="float_left")%>%
  add_header_above(c(" " = 1, " " = 1, "Obesity" = 2))%>%
  column_spec(1:2,width = "10em",bold=TRUE)

test_marstat$pval_df%>%
  kable(caption = "pvalues for Chi-squared Independence Test")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width=F,position="float_left")%>%
  column_spec(2,width = "15em",bold=TRUE)

nhanes%>%mutate(CDC_obese=case_when(
  bmi <18.5 ~ "Underweight", 
  bmi>=18.5 & bmi<=24.9 ~ "Healthy",
  bmi>24.9 & bmi<=29.9 ~ "Overweight",
  bmi>29.9  ~ "Obesity"
))%>%transmute(CDC_obese=factor(CDC_obese, levels=c("Underweight", "Healthy", "Overweight", "Obesity")), tchol, hdl)%>%na.omit()%>% group_by(CDC_obese)%>%summarise(Mean_tchol=mean(tchol), Mean_hdl=mean(hdl))%>%gather("Type", "Value",Mean_tchol:Mean_hdl)%>%
  plot_ly(x=~CDC_obese, y=~Value, color = ~Type, type = "scatter", mode="lines", height = 300,width = 800)%>%
  layout(
    yaxis = list(range = c(45, 200),dtick = 50)
  )

#Categorization
nhanes_data <- nhanes

nhanes_data <- nhanes_data[complete.cases(nhanes_data[, c("bmi", "gender", "tchol", "hdl", "sysbp", "dbp")]), ]

# Categorize body weight based on BMI
nhanes_data$weight_category <- with(nhanes_data, ifelse(
  bmi < 18.5, "Underweight",
  ifelse(bmi >= 18.5 & bmi <= 24.9, "Healthy",
         ifelse(bmi >= 25 & bmi <= 29.9, "Overweight", "Obese"))))

# Categorizing cholesterol levels
nhanes_data$chol_category <- with(nhanes_data, ifelse(
  tchol >= 240 | hdl < 40, "Dangerous",
  ifelse(
    tchol >= 200 & tchol < 240 | 
      (gender == "Male" & hdl >= 40 & hdl < 60) |  # Male and HDL between 40-59
      (gender == "Female" & hdl >= 50 & hdl < 60),   # Female and HDL between 50-59
    "At Risk",
    "Healthy"
  )
))

# Preview
#head(nhanes_data[, c("gender","tchol", "hdl", "chol_category")])

# Categorizing blood pressure
nhanes_data$bp_category <- with(nhanes_data, ifelse(
  sysbp < 120 & dbp < 80, "Normal",
  ifelse(sysbp >= 120 & sysbp <= 129 & dbp < 80, "Elevated",
         ifelse(sysbp > 180 | dbp > 120, "Hypertensive Crisis",
                ifelse(sysbp >= 140 | dbp >= 90, "Hypertension Stage 2",
                       ifelse((sysbp >= 130 & sysbp <= 139) | (dbp >= 80 & dbp <= 89), "Hypertension Stage 1", NA))))))

# Preview
#head(nhanes_data[, c("sysbp", "dbp", "bp_category")])

test1 <- IndepTest$new(nhanes_data, "weight_category", "chol_category")
test2 <- IndepTest$new(nhanes_data, "weight_category", "bp_category")
test3 <- IndepTest$new(nhanes_data, "chol_category", "bp_category")

#contingency table for Weight vs. Cholesterol
table_weight_chol <- test1$ContingencyTbl$weight_category %>%
  rename("\t" = weight_category) %>%
  add_column(" " = c("Weight", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Contingency Table: Weight vs. Cholesterol") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "float_left") %>%
  add_header_above(c(" " = 1, " " = 1, "Cholesterol" = 3)) %>%
  column_spec(1:2, width = "10em", bold = TRUE)
table_weight_chol

#contingency table for Weight vs. Blood Pressure
table_weight_bp <- test2$ContingencyTbl$weight_category %>%
  rename("\t" = weight_category) %>%
  add_column(" " = c("Weight", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Contingency Table: Weight vs. Blood Pressure") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "float_left") %>%
  add_header_above(c(" " = 1, " " = 1, "Blood Pressure" = 5)) %>%
  column_spec(1:2, width = "10em", bold = TRUE)
table_weight_bp

#contingency table for Cholesterol vs. Blood Pressure
table_chol_bp <- test3$ContingencyTbl$chol_category %>%
  rename("\t" = chol_category) %>%
  add_column(" " = c("Cholesterol", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Contingency Table: Cholesterol vs. Blood Pressure") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "float_left") %>%
  add_header_above(c(" " = 1, " " = 1, "Blood Pressure" = 5)) %>%
  column_spec(1:2, width = "10em", bold = TRUE)
table_chol_bp

pvals<-data.frame(test1$pval_df, test2$pval_df, test3$pval_df)
colnames(pvals)<-c("Weight VS Cholesterol", "Weight VS. Blood Pressure", "Cholesterol VS. Blood Pressure")

pvals%>%kable(caption = "p-values of Independence Tests between Variables") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), position = "float_left")

#boxplot for Vigorous activities VS bmi
Nhanes1<-nhanes%>%select(bmi, vigwrk, vigrecexr)%>%na.omit()%>%
  mutate(vigrecexr=factor(vigrecexr, levels = c("Yes", "No"), labels = c("VigR=Yes", "VigR=No")))%>%
  mutate(RecGivenWrk=factor(paste0(vigrecexr,"|", "VigW=", vigwrk)))

#boxplot for Moderate activities VS bmi
Nhanes2<-nhanes%>%select(bmi, modwrk, modrecexr)%>%na.omit()%>%
  mutate(modrecexr=factor(modrecexr, levels = c("Yes", "No"), labels = c("ModR=Yes", "ModR=No")))%>%
  mutate(RecGivenWrk=factor(paste0(modrecexr,"|", "ModW=", modwrk)))

p1<-ggplot(Nhanes1, aes(x=RecGivenWrk, y=bmi, fill=vigwrk))+
  geom_boxplot() +
  geom_hline(yintercept = 18.5, linetype="dotted", size = 0.5)+
  geom_hline(yintercept = 30, linetype="dotted", size = 0.5)+
  annotate("text", y=17, x=" ", label=" underweight")+
  annotate("text", y=31, x=" ", label=" obsity")+
  theme(axis.text.x = element_text(size =7, vjust = 0.5, hjust = 0.5, angle = 45))
p1<-ggplotly(p1)

p2<-ggplot(Nhanes2, aes(x=RecGivenWrk, y=bmi, fill=modwrk))+
  geom_boxplot() + 
  geom_hline(yintercept = 18.5, linetype="dotted", size = 0.5)+
  geom_hline(yintercept = 30, linetype="dotted", size = 0.5)+
  annotate("text", y=17, x=" ", label=" underweight")+
  annotate("text", y=31, x=" ", label=" obsity")+
  labs(x="Moderate Recreational Activity given Work Activity", y="BMI")+
  theme(axis.text.x = element_text(size =7, vjust = 0.5, hjust = 0.5, angle = 45))
p2<-ggplotly(p2)

subplot(p1,p2,shareY = TRUE)%>%
  layout(
    width=900,
    margin=list(l=0, r=0, b=0, t=20),
    xaxis=list(title=list(text="Vigorous Recreational Activity | Vigorous Work Activity", font=list(size=11))),
    xaxis2=list(title=list(text="Moderate Recreational Activity | Moderate Work Activity", font=list(size=11))),
    yaxis=list(title="BMI"))

Nhanes<-nhanes%>%select(bmi,vigwrk,modwrk,wlkbik,vigrecexr,modrecexr)%>%na.omit()%>%
  mutate(InRange=factor((ifelse(bmi>18.5 & bmi <30, "Yes", "No")), levels = c("Yes", "No")))
tests<-IndepTest$new(Nhanes, c("vigwrk","modwrk","wlkbik","vigrecexr","modrecexr"), "InRange")
tests$pval_df%>%
  kable(caption = "p-values of Independence Tests between Different Variables and HealthyRange of BMI")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),position="float_left")

Nhanes1<-Nhanes1%>%mutate(InRange=factor((ifelse(bmi>18.5 & bmi <30, "Yes", "No")), levels = c("Yes", "No")))
Nhanes2<-Nhanes2%>%mutate(InRange=factor((ifelse(bmi>18.5 & bmi <30, "Yes", "No")), levels = c("Yes", "No")))

tbl1<-Nhanes1%>%filter(vigwrk=="Yes")%>%group_by(across(c(InRange, vigrecexr), as.factor))%>%tally()%>%spread(key=vigrecexr, value=n)
tbl1%>%
  rename("\t" = InRange)%>%
  add_column(" " = c("InRange", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Conditional Contingency Table")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width=F,position="float_left")%>%
  add_header_above(c(" " = 1, " " = 1, "Recreational|VigW=Y" = 2))%>%
  column_spec(1:2,bold=TRUE)

tbl2<-Nhanes1%>%filter(vigwrk=="No")%>%group_by(across(c(InRange, vigrecexr), as.factor))%>%tally()%>%spread(key=vigrecexr, value=n)
tbl2%>%
  rename("\t" = InRange)%>%
  add_column(" " = c("InRange", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Conditional Contingency Table")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width=F, position="float_left")%>%
  add_header_above(c(" " = 1, " " = 1, "Recreational|VigW=N" = 2))%>%
  column_spec(1:2,bold=TRUE)

tbl3<-Nhanes1%>%group_by(across(c(InRange, vigrecexr), as.factor))%>%tally()%>%spread(key=vigrecexr, value=n)
tbl3%>%
  rename("\t" = InRange)%>%
  add_column(" " = c("InRange", rep(" ", nrow(.) - 1)), .before = "\t") %>%
  kable(caption = "Marginal Contingency Table, Aggregate VigW")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width=F, position="left")%>%
  add_header_above(c(" " = 1, " " = 1, "Recreational" = 2))%>%
  column_spec(1:2,bold=TRUE)

oddsratio<-function(tbl,row.names=NULL){
  res<-do.call(rbind,map(tbl, function(x){y<-as.matrix(x); y[1,1]*y[2,2]/(y[1,2]*y[2,1])}))
  colnames(res)<-"Estimated Odds Ratio"
  rownames(res)<-row.names
  return(res)
}

list(tbl1, tbl2, tbl3)%>%map(function(x){ungroup(x)%>%select(-InRange)})%>%oddsratio(row.names = c("VigW=Y", "VigW=N", "Aggregated VigW"))%>%
  kable(caption = "OddsRatio for Conditional and Marginal Probability")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width=F, position="float_left")%>%
  column_spec(1,bold=TRUE)

Vig_Rec1<-nhanes%>%transmute(InRange=factor((ifelse(bmi>18.5 & bmi <30, "Yes", "No")), levels = c("Yes", "No")), vigrecexr)%>%na.omit()%>%
  filter(vigrecexr=="Yes")%>%group_by(InRange)%>%count()
colnames(Vig_Rec1)<-c("InRange", "Vigorous Recreational=Y")

Mod_Rec1<-nhanes%>%transmute(InRange=factor((ifelse(bmi>18.5 & bmi <30, "Yes", "No")), levels = c("Yes", "No")), modrecexr)%>%na.omit()%>%
  filter(modrecexr=="Yes")%>%group_by(InRange)%>%count()
colnames(Mod_Rec1)<-c("InRange", "Moderate Recreational=Y")

p1<-1028/(1028+371)
p2<-1657/(1657+828)
z1<-(p1-p2)/sqrt(p1*(1-p1)/(1028+371) + p2*(1-p2)/(1657+828))

Vig_Rec2<-nhanes%>%transmute(InRange=factor((ifelse(bmi>18.5 & bmi <30, "Yes", "No")), levels = c("Yes", "No")), vigrecexr)%>%na.omit()%>%
  filter(vigrecexr=="No")%>%group_by(InRange)%>%count()
colnames(Vig_Rec2)<-c("InRange", "Vigorous Recreational=N")

Mod_Rec2<-nhanes%>%transmute(InRange=factor((ifelse(bmi>18.5 & bmi <30, "Yes", "No")), levels = c("Yes", "No")), modrecexr)%>%na.omit()%>%
  filter(modrecexr=="No")%>%group_by(InRange)%>%count()
colnames(Mod_Rec2)<-c("InRange", "Moderate Recreational=N")

p1<-2981/(2981+2064)
p2<-2351/(2351+1607)
z2<-(p1-p2)/sqrt(p1*(1-p1)/(2981+2064) + p2*(1-p2)/(2351+1607))

merge(merge(Vig_Rec1, Vig_Rec2, by="InRange"), merge(Mod_Rec1, Mod_Rec2, by="InRange"), by="InRange")%>%
  kable(caption="Frequency Table")%>%kable_styling(bootstrap_options=c("striped","condensed"),position="left")%>%
  column_spec(1,bold = TRUE)

data.frame(VigR_Yes=pnorm(-z1), VigR_No=pnorm(z2), row.names = "pvalue")%>%
  kable(caption = "pvalues for two-sample test")%>%
  kable_styling(bootstrap_options=c("striped","condensed"),full_width=F,position="float_left")%>%
  column_spec(1, width = "10em", bold = TRUE)
