oddsratio<-function(tbl,row.names=NULL){
  res<-do.call(rbind,map(tbl, function(x){y<-as.matrix(x); y[1,1]*y[2,2]/(y[1,2]*y[2,1])}))
  colnames(res)<-"Estimated Odds Ratio"
  rownames(res)<-row.names
  return(res)
}

IndepTest<-setRefClass(
  "IndepTest",
  fields = list(data="data.frame", variable_x="character", reference_y="character",pval_df="data.frame",ContingencyTbl="list" ),
  methods=list(
    initialize=function(data, variable_x, reference_y){
      #make a contingency table for the variable X and Y
      .self$ContingencyTbl<-.self$create_list(data, variable_x, reference_y)
      
      #calculate the pvalues and make a data frame
      .self$pval_df<- .self$chi_square_test(my_list=ContingencyTbl)
    },
    
    create_list=function(data, variable_x, reference_y){
      my_list <- list()
      for(x in variable_x){
        my_list[[x]]<-data%>%select(all_of(c(x, reference_y)))%>%na.omit()%>%
          group_by(across(everything(), as.factor))%>%tally()%>%spread(key=reference_y, value=n)}
      return(my_list)
    }, 
    
    chi_square_test=function(my_list){
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
