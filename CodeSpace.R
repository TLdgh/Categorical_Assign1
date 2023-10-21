oddsratio<-function(tbl,row.names=NULL){
  res<-do.call(rbind,map(tbl, function(x){y<-as.matrix(x); y[1,1]*y[2,2]/(y[1,2]*y[2,1])}))
  colnames(res)<-"Estimated Odds Ratio"
  rownames(res)<-row.names
  return(res)
}

IndepTest<-setRefClass(
  "IndepTest",
  fields = list(data="data.frame", variable_x="character", variable_y="character",res="list"),
  methods=list(
    initialize=function(data, variable_x, variable_y){
      #make a contingency table for the variable X and Y
      for(x in variable_x){
        .self$res[[x]]<-data%>%select(all_of(c(x, variable_y)))%>%na.omit()%>%
          group_by(across(everything(), as.factor))%>%tally()%>%spread(key=variable_y, value=n)}
    },
    
    get_pvalue=function(){
      #do the chi-squared test
      pvalues<-map(res, function(x){
        y<-ungroup(x)%>%select(-any_of(group_vars(x)))%>%as.matrix()%>%chisq.test()
        return(y$p.value)
      })
      
      #format it nicely
      pvalues<-pvalues%>%data.frame(row.names = "p-value")%>%
        mutate(across(where(is.numeric), .fns = ~as.character(signif(.,4))))
      
      return(pvalues)}
  )
)
