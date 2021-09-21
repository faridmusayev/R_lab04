

linreg<-setRefClass("linreg",
                    fields=list(formula='formula',data='data.frame',data_name="character",
                                coefficients="numeric", resids="numeric",
                                predictors="numeric",df="numeric",
                                summ="data.frame",resid_se="numeric"),
                    methods=list(
                      pred=function(){

                        return(predictors)
                      },
                      resid=function(){

                        return(resids)
                      },
                      coef=function(){

                        return(coefficients)
                      },
                      print=function(){

                        cat(paste("linreg(formula = ",format(formula),", data = ",data_name,")\n\n",sep=""))

                        x<-data.frame(t(coefficients))

                        colnames(x)<-names(coefficients)

                        write.table(x,quote=FALSE)

                      },
                      summary=function(){

                        #write.table(round(summ,digits=2),quote=FALSE)

                        cat("Residual standard error:",round(resid_se,digits=2),"on",df,"degrees of freedom")


                      },
                      plot=function(){



                        pl1<-ggplot()+

                          theme(
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            plot.title=element_text(hjust=0.5),
                            panel.background = element_rect(fill='transparent'))+

                          geom_point(aes(x=predictors, y=resids), size=4, shape=21 )+

                          geom_smooth(aes(x=predictors, y=resids),

                                      formula=y~x, method="loess",color='red', se=F)+

                          geom_hline(aes(yintercept =0),linetype="dashed")+

                          xlab(label=paste("Fitted values\n","lm(",format(formula),")"))+

                          ylab("Residuals")+

                          ggtitle("Residuals vs Fitted")



                        std_res <- resids/resid_se

                        pl2<-ggplot()+

                          theme(
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            plot.title=element_text(hjust=0.5),
                            panel.background = element_rect(fill='transparent'))+

                          geom_point(aes(x=predictors, y=std_res), size=4, shape=21 )+

                          geom_smooth(aes(x=predictors, y=std_res),

                                      formula=y~x, method="loess",color='red', se=F)+

                          xlab(label=paste("Fitted values\n","lm(",format(formula),")"))+

                          ylab(expression(sqrt("Standardized Residuals")))+

                          ggtitle("Scale-Location")

                          grid.arrange(grobs=list(pl1,pl2))
                      }
                    )
)
#initialize object

linreg$methods(initialize=function(formula,data){

  .self$formula<<-formula

  .self$data<<-data

  .self$data_name<<-deparse(substitute(data))

  #model matrix or X

  mm<-model.matrix(formula,data)

  #response data

  y<-as.matrix(data[,all.vars(formula)[1]])

  #regression coefficients

  betta<-solve((t(mm)%*%mm))%*%t(mm)%*%y

  .self$coefficients<<-round(betta[,1],digits=2)

  #fitted values

  y_fitted<-mm%*%betta

  .self$predictors<<-as.vector(y_fitted)

  #residuals

  res<-y-y_fitted

  .self$resids<<-as.vector(res)

  #degress of freedom

  df1<-dim(mm)[1]-dim(mm)[2]

  .self$df<<-df1

  #residual variance

  sgm_2<-(t(res)%*%res)/df1

  #variance of regression coefficients

  var_betta<-sgm_2[1,1]*solve(t(mm)%*%mm)

  #eliminate negative cov values

  ref_var_betta<-c()

  for (i in 1:dim(var_betta)[1]){
    ref_var_betta<-c(ref_var_betta,var_betta[i,i])
  }

  #t-values for each coefficient

  t_val<-betta/as.matrix(ref_var_betta^0.5)

  #p-values for each regression coefficient

  p_val<-pt(q=abs(as.vector(t_val)), df=df1,lower.tail=FALSE)

  #summary for summary function

  summat<-data.frame(Estimate=as.vector(betta),Std.Error=ref_var_betta^0.5,
                     t_value=as.vector(t_val),p_value=p_val)

  rownames(summat)<-rownames(betta)

  .self$summ<<-summat

  residual_se<-sqrt(sum(res^2)/148)

  .self$resid_se<<-residual_se

  cat("Well done, my brave knight. Now...off with your head.\n")
})

#BEN BÖYLE YAPTIM DIŞA EKLEDİM BAKIN DİYE#

summary = function(){
                        
                          
                          beta<-Coefficients
                          namn<-names(beta)
                          names(beta)<-NULL
                          beta<-round(beta,4)
                          
                          for(i in 1:length(beta)){
                            beta[i]<-format(beta[i], width=max(nchar(beta[i]),nchar(namn[i])),justify = c("right"))
                            namn[i]<-format(namn[i], width=max(nchar(beta[i]),nchar(namn[i])),justify = c("right"))
                          }
                          
                          Variable<-as.character(names(Coefficients))
                          Estimate<-round(Coefficients,3)
                          Std_Error<-round(Std_betas,3)
                          t_value<-round(tBetas,3)
                          Pr_t<-round(Pvalues,5)
                          
                          svar<-data.frame(Variable,Estimate,Std_Error,t_value,Pr_t)
                          row.names(svar)<-NULL
                          svar$Variable<-as.character(svar$Variable)
                          
                          cat("Call:",sep="\n")
                          cat(paste("linreg(","formula = ",formula[2]," ",formula[1]," ",formula[3],", ","data = ",DataName,")",sep=""), sep="\n")
                          cat(sep="\n")
                          cat("Coefficients:",sep="\n")
                          cat()
                          for(i in 1:nrow(svar)){
                            cat(paste(svar[i,],collapse = " "),sep="",collapse=" ***\n")
                          }
                          cat("",sep="\n")
                          cat(paste("Residual standard error: ",round(sqrt(Var_residuals),5) ," on " ,df, " degrees of freedom",sep=""))
                        } 
                      ))
