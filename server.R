library(shiny)
library(shinythemes)
library(sqldf)
library(png)
library(shinyjs)
library(xtable)
library(ggplot2)
library(readr)
library(dplyr)
library(nortest)
library(car)
library(ggplot2)
library(data.table)
library(reshape2)
library(corrplot)
library(DMwR)
library(randomForest)
library(MASS)
library(Metrics)
library(caTools)
library(caret)
library(ROCR)
library(rms)
library(ResourceSelection)
library(mfx)
library(e1071)
library(glmnet)
library(ROSE)
library(mice)



shinyServer(function(input, output,session) {
    
    output$table1 <- renderDataTable({
        #print("Inside")
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        data <-  read.csv(inFile$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)
        
        s = data
        data
        
        
    })
    
    ## Reactive functions
    ## A reactive function helps you feed in the dataset to other fucntions inside the server.
    
    ##Raw reactive function without any pre-processing
    s = reactive({
        
        inFile <- input$file1
        data <-  read.csv(inFile$datapath, header = input$header,
                          sep = input$sep, quote = input$quote)
        data
        
        
    })
    
    ##Reactive fucntion with outlier treatment and missing value impututation
    ## Step 1 : Capturing columns with missing values. 
    ## Step 2 : Use one column at a time (from step 1) as the dependent variable.
    ## Step 3 : Train data must have values of the dependent variable and the 
    ##          test data will be the one where we predict the model's results.
    ## Step 4 : Run a simple linear regression and predict the values for 
    ##          the missing data points in the test. [ Missing value imputation for that variable]
    ## Step 5 : Once this process is done, we look into outliers. 
    ##          We use a process called as Capping (i.e) values above 
    ##          2*IQR(Inter-Quartile Range) from 3rd quartile will be set to the 
    ##          95th percentile and  values below 2*IQR(Inter-Quartile Range) 
    ##          from 1st quartile will be set to the 5th Percentile
    
    s_temp = reactive({
        
        inFile <- input$file1
        data1 <-  read.csv(inFile$datapath, header = input$header,
                           sep = input$sep, quote = input$quote)
        
        ##########################################################################################################
        ## Crude way to detect ID Variable - Allan
        
        temp1= data1[,names(data1[,sapply(data1,is.factor)])]
        for (i in names(temp1))
        {
            if(length(unique(temp1[,i]))>70)
            {
                data1[,i]=NULL
            }
        }
        
        temp2= data1[,names(data1[,sapply(data1,is.character)])]
        for (i in names(temp2))
        {
            if(length(unique(temp2[,i]))>70)
            {
                data1[,i]=NULL
            }
        }
        
        for (i in names(data1))
        {
            if(length(unique(data1[,i]))==nrow(data1))
            {
                data1[,i]=NULL
            }
        }
        
        temp3= data1[,names(data1[,sapply(data1,is.logical)])]
        for (i in names(temp3))
        {
            data1[,i]=as.factor(data1[,i])
        }
        
        rm(temp1,temp2,temp3)
        
        ##Check for Factor errors and rectify before imputing ## [Error:Factor has new levels] while imputing
        
        errorfactor=data.frame('sno' = seq(1:nrow(data1)))
        data2=data1
        
        for (i in names(data1))
        {
            data1$target =  data1[,i]
            data1[i] = NULL
            
            if(sum(is.na(data1$target))>0)
            {
                train = subset(data1,data1$target != "NA")
                test = subset(data1,is.na(data1$target))
                
                for(j in names(data2[,sapply(data2,is.factor)]))
                {
                    k<-setdiff(unique(test[,j]),unique(train[,j]))
                    if(identical(k,character(0))==FALSE)
                    {
                        errorfactor=cbind(errorfactor,data1[,j])
                        names(errorfactor)=ifelse(names(errorfactor)== "data1[, j]",j,names(errorfactor))
                        data2[,j]=NULL
                    }
                }
            }
            data1[i]=data1$target
        }
        rm(train,test)
        
        impute<-mice(data2,m=1,defaultMethod = c("pmm", "logreg", "polyreg", "polr"), seed = 100)
        data2<- complete(impute,1)
        
        
        errorfactor$sno=NULL
        data1=cbind(data2,errorfactor)
        
        rm(impute,errorfactor,data2)
        
        ##Filtering out for variables which are numeric , using unique length as a constraint (70) //this can be changed 
        a1 = apply(data1, 2, function(x) length(unique(x)) > 70)
        
        a = data.frame(a1[a1==T])
        row.names(a)
        
        
        for(i in row.names(a))
        {
            data1$target = data1[,i]
            
            data1[i] = NULL
            
            ## Outlier Analysis 
            q_25 = quantile(data1$target,na.rm = TRUE)[2] 
            q_75 = quantile(data1$target,na.rm = TRUE)[4]
            
            iqr =  q_75 - q_25 
            
            above = (iqr * 2) + q_75
            below = q_25 - (iqr * 2)
            
            q_95 = quantile(data1$target,.95,na.rm = TRUE)
            q_5  = quantile(data1$target,.5,na.rm = TRUE)
            
            data1$target = ifelse((data1$target>above),q_95,ifelse((data1$target<below),q_5,data1$target))
            
            data1[i] = data1$target
            
        }
        
        data1$target=NULL
        
        for (i in names(data1[,sapply(data1,is.factor)]))
        {
            data1[,i]=factor(data1[,i],exclude = "")
        }
        
        impute<-mice(data1,m=1,defaultMethod = c("pmm", "logreg", "polyreg", "polr"), seed = 100)
        data1<- complete(impute,1)
        rm(impute,a)
        ###########################################################################################################
        
        df= data1
        
        df
        
    })
    
    
    
    
    
    
    ##Reactive function which contains the variable transformations in order to obtain normality in the data.
    ## Step 1 : Let you variable be X 
    ##          Apply the following transformations : Exponential(x), Logarithmic(x) , Square root(x), x^2, x^-1
    ##                                                x^(-1/2), x^(-2)
    ## Step 2 : The transformation with maximum p-value in shapiro test, will be the best transformation 
    ##          for obtaining normality.
    s1 = reactive({
        
        # inFile <- input$file1
        # data1 <- data.frame(read.csv(inFile$datapath, header = input$header,
        #                     sep = input$sep, quote = input$quote))
        # ##########################################################################################################
        # ## Crude way to detect ID Variable - Allan
        # 
        # temp1= data1[,names(data1[,sapply(data1,is.factor)])]
        # for (i in names(temp1))
        # {
        #   if(length(unique(temp1[,i]))>70)
        #   {
        #     data1[,i]=NULL
        #   }
        # }
        # 
        # temp2= data1[,names(data1[,sapply(data1,is.character)])]
        # for (i in names(temp2))
        # {
        #   if(length(unique(temp2[,i]))>70)
        #   {
        #     data1[,i]=NULL
        #   }
        # }
        # 
        # rm(temp1,temp2)
        # 
        # ##Check for Factor errors and rectify before imputing ## [Error:Factor has new levels] while imputing
        # 
        # errorfactor=data.frame('sno' = seq(1:nrow(data1)))
        # data2=data1
        # 
        # for (i in names(data1))
        # {
        #   data1$target =  data1[,i]
        #   data1[i] = NULL
        #   
        #   if(sum(is.na(data1$target))>0)
        #   {
        #     train = subset(data1,data1$target != "NA")
        #     test = subset(data1,is.na(data1$target))
        #     
        #     for(j in names(data2[,sapply(data2,is.factor)]))
        #     {
        #       k<-setdiff(unique(test[,j]),unique(train[,j]))
        #       if(identical(k,character(0))==FALSE)
        #       {
        #         errorfactor=cbind(errorfactor,data1[,j])
        #         names(errorfactor)=ifelse(names(errorfactor)== "data1[, j]",j,names(errorfactor))
        #         data2[,j]=NULL
        #       }
        #     }
        #   }
        #   data1[i]=data1$target
        # }
        # rm(train,test)
        # 
        # impute<-mice(data2,m=1,defaultMethod = c("pmm", "logreg", "polyreg", "polr"), seed = 100)
        # data2<- complete(impute,1)
        # 
        # errorfactor$sno=NULL
        # data1=cbind(data2,errorfactor)
        # 
        # rm(impute,errorfactor,data2)
        # 
        # ##Filtering out for variables which are numeric , using unique length as a constraint (70) //this can be changed 
        # a1 = apply(data1, 2, function(x) length(unique(x)) > 70)
        # 
        # a = data.frame(a1[a1==T])
        # row.names(a)
        # 
        # 
        # for(i in row.names(a))
        # {
        #   data1$target = data1[,i]
        #   
        #   data1[i] = NULL
        #   
        #   ## Outlier Analysis 
        #   q_25 = quantile(data1$target,na.rm = TRUE)[2] 
        #   q_75 = quantile(data1$target,na.rm = TRUE)[4]
        #   
        #   iqr =  q_75 - q_25 
        #   
        #   above = (iqr * 2) + q_75
        #   below = q_25 - (iqr * 2)
        #   
        #   q_95 = quantile(data1$target,.95,na.rm = TRUE)
        #   q_5  = quantile(data1$target,.5,na.rm = TRUE)
        #   
        #   data1$target = ifelse((data1$target>above),q_95,ifelse((data1$target<below),q_5,data1$target))
        #   
        #   data1[i] = data1$target
        #   
        # }
        # 
        # data1$target=NULL
        # 
        # for (i in names(data1[,sapply(data1,is.factor)]))
        # {
        #   data1[,i]=factor(data1[,i],exclude = "")
        # }
        # 
        # impute<-mice(data1,m=1,defaultMethod = c("pmm", "logreg", "polyreg", "polr"), seed = 100)
        # data1<- complete(impute,1)
        # rm(impute,a)
        ###########################################################################################################
        
        
        
        
        
        input_data = s_temp()
        
        copy1 = input_data
        df_transformed = data.frame(sno = seq(1:nrow(input_data)))  
        
        input_data = data.frame(input_data)
        for(i in names(input_data[,sapply(input_data,is.numeric)]))
        {
            
            variable = input_data[i]
            names(variable)[1] = 'v1'
            
            
            variable$v1 = ifelse(variable$v1<0,variable$v1+abs(min(variable$v1))+1,variable$v1)
            
            Log1p.Transformation = log1p(variable$v1) 
            Expm1.Transformation = ifelse(variable$v1<=709,expm1(variable$v1),0)
            Sqrt.Transformation = sqrt(variable$v1)
            Square.Transformation = (variable$v1)^2
            Power_minus1.Transformation = 1/(variable$v1)
            Power_minus2.Transformation= 1/((variable$v1)^2)
            Power_minus1by2.Transformation = 1/(sqrt(variable$v1))
            No.Transformation = variable$v1
            
            df_main = data.frame('Variable'=i,Expm1.Transformation,Log1p.Transformation,Power_minus1.Transformation,Power_minus1by2.Transformation,
                                 Power_minus2.Transformation,Square.Transformation,Sqrt.Transformation,No.Transformation)
            if(nrow(df_main)>4999)
            {
                df1 = df_main[sample(nrow(df_main), 4999), ]
            }else
            { 
                df1=df_main
            }
            
            
            dummy = data.frame()
            
            for(j in colnames(df1[,c(2:9)]))
            {
                if(length(unique(df1[,j]))>1)
                {
                    a = unlist(shapiro.test(df1[,j])[2])
                    a = a[['p.value']]
                    
                    df2 = data.frame('variable'=i,'iter'=j,a)
                    
                    dummy = rbind(dummy,df2)
                }
                
                else if(length(unique(df1[,j]))<=0)
                {
                    a=0
                    df2 = data.frame('variable'=i,'iter'=j,a)
                    dummy = rbind(dummy,df2)
                }
            }
            
            best_transformation = as.character(dummy$iter[which.max(dummy$a)])
            df_transformed = cbind(df_transformed,df_main[,best_transformation])
            
            print("-----------------------------")
            print(sprintf("Variable Name : %s",i))
            print(sprintf("STEP 1 --- Recommended Changes : %s",best_transformation))
            print(sprintf("STEP 2 --- Transformated Histogram"))
            print("-----------------------------")
            
            ggdf_temp = subset(df1,select=c(best_transformation))
            
        }
        
        
        for(i in 2:ncol(df_transformed))
        {
            a = names(input_data[,sapply(input_data,is.numeric)])[i-1]
            names(df_transformed)[i] = a
        }
        
        df_transformed = cbind(df_transformed,copy1[names(copy1[,sapply(copy1,is.factor)])])
        df_transformed$sno = NULL
        rm(copy1,data1,df_main,df1,df2,dummy,ggdf_temp,input_data,variable)
        
        print("### Processing Completed ###")
        
        df_transformed
        
        
    })
    
    ## Tell me about the data
    
    ##Displaying columns in a UI (Shiny)
    output$choose_columns = renderUI({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        colnames = names(data1)[1:ncol(data1)]
        
        radioButtons("columns", label = h3("All Variables"),
                     choices = colnames,selected = colnames[3])
        
    })
    
    ## This function provides basic summary of variables
    ## a) For continuous  - min, max, etc..  
    ## b) For Categorical - % of a subcategory in a variable is displayed
    
    output$data_table = renderDataTable({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        data1 <- data1[, input$columns, drop = FALSE]
        
        
        
        data1 = data.frame(data1)
        names(data1) = 'v1'
        data2 = data.frame()
        
        if(is.numeric(data1$v1)==TRUE)
            
        {
            
            input_data = data1
            
            data_summary = data.frame()
            
            
            
            variable <- input_data
            names(variable)[1] = 'v1'
            
            Min = round(min(variable,na.rm = F),2) ##Min
            Max = round(max(variable,na.rm = F),2) ##Max
            
            Median = round(apply(variable,2,median),2) ##Median
            Median = Median[['v1']]
            
            Mean = round(apply(variable,2,mean),2) ##Mean
            Mean = Mean[['v1']]
            
            ## 1st quartile
            a = as.data.frame(quantile(variable$v1,0.25)[1])
            names(a)[1] = 'v1'
            First_Quartile = round(a$v1[1],2)
            
            ## 3rd quartile
            a = as.data.frame(quantile(variable$v1,0.75)[1])
            names(a)[1] = 'v1'
            Third_Quartile = round(a$v1[1],2)
            
            ##Shapiro Wilk test
            if(nrow(variable)>4999)
            {
                shapiro_statistic = unlist(shapiro.test(variable[sample(nrow(variable), 4999), ])[1]) 
                shapiro_pvalue = unlist(shapiro.test(variable[sample(nrow(variable), 4999), ])[2])
            }else
            {
                shapiro_statistic = unlist(shapiro.test(variable[,1])[1])
                shapiro_pvalue = unlist(shapiro.test(variable[,1])[2])
            }
            shapiro_statistic = shapiro_statistic[['statistic.W']]
            
            
            shapiro_pvalue = shapiro_pvalue[['p.value']]
            
            ##Anderson Darling test
            
            anderson_statistic = unlist(ad.test(variable$v1)[1])
            anderson_statistic = anderson_statistic[['statistic.A']]
            
            anderson_pvalue = unlist(ad.test(variable$v1)[2])
            anderson_pvalue = anderson_pvalue[['p.value']]
            
            df1=data.frame()
            df1 = data.frame('Variable Name is '=input$columns,'The Minimum value is' = Min,'The 25th Percentile is at' = First_Quartile,"The Median is"=
                                 Median,'The Average value is' = Mean,
                             'The 75th Percentile is at' = Third_Quartile,'The Maximum value is' = Max,check.names = F)
            
            #df1$dummy = paste()
            
            data_summary = rbind(data_summary,df1)
            
            
            rm(list=setdiff(ls(), c('dataset','input_data','data_summary')))
            
            
            
            print("### Process Completed ###")
            data2 = t(data_summary)
            data2 = data.frame(data2)
            data2$Variable = row.names(data2)
            names(data2)[1] = 'data2'
            data2$dummy = paste(data2$Variable,data2$data2,sep=" ")
            b = data.frame('Summary Statistics'=data2[,c('dummy')],check.names = F) 
            data3 = DT::datatable(b, options = list(lengthMenu = c(10, 15, 20), pageLength = 7),rownames = F)
            
            
            
        }
        
        else{
            input_data = data1
            
            print("-----------------------------")
            print(sprintf("Variable name : %s",1))
            a = data.frame(table(input_data))
            
            names(a)[1] = 'variable'
            a = a[order(-a$Freq),]
            
            names(a)[1] = input$columns
            names(a)[2] = 'Count'
            a$Percentage = a$Count/sum(a$Count)
            a$dummy = paste(round((a$Percentage*100),0), "% of the Count is in ",a[,c(input$columns)],sep="")
            b = data.frame('Summary Statistics'=a[,c('dummy')],check.names = F) 
            
            data3 = DT::datatable(b, options = list(lengthMenu = c(10, 15, 20), pageLength = 7,autoWidth=T),rownames = F)
            
        }
        
        data3
        
        
    })
    
    output$plot1 = renderPlot({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        data1 <- data1[, input$columns, drop = FALSE]
        
        
        data1 = data.frame(data1)
        names(data1) = 'v1'
        #data2 = data.frame()
        
        if(is.numeric(data1$v1)==TRUE)
            
        {
            
            input_data = data1
            data_summary = data.frame()
            
            
            variable <- input_data
            names(variable)[1] = 'v1'
            
            
            plot1 =  (ggplot(data=variable, aes(variable$v1)) +
                          geom_histogram(col="black", fill="tan3") +
                          labs(title=paste("Histogram of",input$columns)) +
                          labs(x=paste("",input$columns), y="Count"))
        }
        
        else
        {
            input_data = data1
            
            print("-----------------------------")
            print(sprintf("Variable name : %s",1))
            a = data.frame(table(input_data))
            
            names(a)[1] = 'variable'
            a = a[order(-a$Freq),]
            
            plot1 = (ggplot(data=a, aes(x=reorder(variable,-Freq), y=Freq),width=44) +
                         geom_bar(stat="identity",col="black", fill="tan3") +
                         labs(title=paste("Distribution of",input$columns)) +
                         labs(x=paste("",input$columns), y="Count") + theme(axis.text.x = element_text(angle=90/2, vjust=0.5))) 
            
        }
        
        
        plot(plot1)
        
    })
    
    output$plot2 = renderPlot({
        
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        data1 <- data1[, input$columns, drop = FALSE]
        
        
        data1 = data.frame(data1)
        names(data1) = 'v1'
        
        if(is.numeric(data1$v1)==TRUE)
            
        {
            
            variable = data1
            names(variable)[1] = 'v1'
            
            
            variable$v1 = ifelse(variable$v1<0,variable$v1+abs(min(variable$v1))+1,variable$v1)
            
            Log1p.Transformation = log1p(variable$v1) 
            Expm1.Transformation = ifelse(variable$v1<=709,expm1(variable$v1),0)
            Sqrt.Transformation = sqrt(variable$v1)
            Square.Transformation = (variable$v1)^2
            Power_minus1.Transformation = 1/(variable$v1)
            Power_minus2.Transformation= 1/((variable$v1)^2)
            Power_minus1by2.Transformation = 1/(sqrt(variable$v1))
            No.Transformation = variable$v1
            
            df_main = data.frame('Variable'=input$columns,'Exponential transformation' = Expm1.Transformation,
                                 'a log Transformation' = Log1p.Transformation,
                                 'its reciprocated value' = Power_minus1.Transformation,
                                 'reciprocating to the power of 1/2' = Power_minus1by2.Transformation,
                                 'reciprocating to the power of 2' = Power_minus2.Transformation,
                                 'a Square Transformation' = Square.Transformation,
                                 'its Square root' = Sqrt.Transformation,
                                 'No Transformation' = No.Transformation,check.names = F)
            
            if(nrow(df_main)>4999)
            {
                df1 = df_main[sample(nrow(df_main), 4999), ]
            }else
            {
                df1 = df_main 
            }
            
            
            
            dummy = data.frame()
            
            for(j in colnames(df1[,c(2:9)]))
            {
                if(length(unique(df1[,j]))>1)
                {
                    a = unlist(shapiro.test(df1[,j])[2])
                    a = a[['p.value']]
                    
                    df2 = data.frame('variable'=input$columns,'iter'=j,a)
                    
                    
                    dummy = rbind(dummy,df2)
                }
                
                else if(length(unique(df1[,j]))<=0)
                {
                    a=0
                    df2 = data.frame('variable'=input$columns,'iter'=j,a)
                    dummy = rbind(dummy,df2)
                }
            }
            
            best_transformation = as.character(dummy$iter[which.max(dummy$a)])
            
            if(best_transformation!='No Transformation')
            {
                #show('plot2')
                ggdf_temp = subset(df1,select=c(best_transformation))
                
                plot2 = (ggplot(data=ggdf_temp, aes(ggdf_temp[,1])) +
                             geom_histogram(col="black", fill="royalblue3") +
                             labs(title=paste("For Machine Learning purposes,\nwe recommend transforming this variable by using",best_transformation)) +
                             labs(x=paste("",input$columns), y="Count"))
                
                
                plot(plot2)
                
            }
            
            else { #hide('plot2')
            }
            
            
            
            
        }
        
        else { #hide('plot2')
        }
        
        
        
    })
    
    ## Relationship between 2 variables
    
    ##Displaying columns in a UI (Shiny)
    output$choose_columns2 = renderUI({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        #data1[,'Outlet_Establishment_Year'] = NULL
        
        cont_names = names(data1)
        
        data1 = data1[,cont_names]
        
        colnames = names(data1)[1:ncol(data1)]
        
        radioButtons("columns2", label = h3("All Variables"),
                     choices = colnames,selected = colnames[2])
        
        
        
    })
    
    ##Displaying columns in a UI (Shiny)
    output$choose_columns3 = renderUI({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        #data1[,'Outlet_Establishment_Year'] = NULL
        
        colnames = names(data1)[1:ncol(data1)]
        print(input$columns2)
        
        colnames = colnames[!(colnames %in% input$columns2)]
        
        radioButtons("columns3", label = h3("All Variables"),
                     choices = colnames,selected = colnames[1])
        
    })
    
    ##Bi-variate relationships between two selected variables
    output$plot3 = renderPlot({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        v1 <- data1[, input$columns2, drop = FALSE]
        v1 = data.frame(v1)
        names(v1) = 'v1'
        
        v2 <- data1[, input$columns3, drop = FALSE]
        v2 = data.frame(v2)
        names(v2) = 'v2'
        
        df_scatter = data.frame(v1,v2) 
        
        if(is.numeric(df_scatter$v2)==T&is.numeric(df_scatter$v1)==T)
        {
            plot3 =  (ggplot(df_scatter, aes(x=v1, y=v2)) +
                          geom_point(size=2, shape=16,col="purple3", fill="purple3") +
                          labs(title=paste("Scatter Plot :",input$columns2,"vs",input$columns3)) +
                          labs(x=paste("",input$columns2),y=paste("",input$columns3)))
        }
        
        if(is.numeric(df_scatter$v2)==T&is.numeric(df_scatter$v1)==F)
        {
            Types = df_scatter$v1
            one = paste("1.The colored box denote where the 25th percentile (bottom) and 75th percentile (top) of ",input$columns2,"lies in relation to ",input$columns3,"'s categories. The middle line represents the median")
            two = paste("2.The lines coming out of the box identify either the min/max, or where outliers where the outlier limits lie. Any dots at the end are the outliers")
            
            tt1 = (paste(one,two,sep="<br/>"))
            
            plot3 =  (ggplot(df_scatter, aes(x=v1, y=v2,fill = Types)) 
                      + geom_boxplot() 
                      + labs(title=paste("Box-Plot :",input$columns3,"vs",input$columns2)
                             ,subtitle=paste("1.The colored box denote where the 25th percentile (bottom) and 75th percentile (top) of ",input$columns2,"lies in relation to ",input$columns3,"'s categories. The middle line represents the median\n2.The lines coming out of the box identify either the min/max, or where outliers where the outlier limits lie. Any dots at the end are the outliers")) 
                      + labs(x=paste("",input$columns2),y=paste("",input$columns3)))
        }
        
        if(is.numeric(df_scatter$v2)==F&is.numeric(df_scatter$v1)==T)
        {
            Types = df_scatter$v2
            plot3 =  (ggplot(df_scatter, aes(x=v2, y=v1,fill = Types)) 
                      + geom_boxplot() 
                      + labs(title=paste("Box-Plot :",input$columns2,"vs",input$columns3)
                             ,subtitle=paste("1.The colored box denote where the 25th percentile (bottom) and 75th percentile (top) of ",input$columns2,"lies in relation to ",input$columns3,"'s categories. The middle line represents the median\n2.The lines coming out of the box identify either the min/max, or where outliers where the outlier limits lie. Any dots at the end are the outliers")) 
                      + labs(x=paste("",input$columns3),y=paste("",input$columns2)))
        }
        
        if(is.numeric(df_scatter$v2)==F&is.numeric(df_scatter$v1)==F)
        {
            dat <- data.frame(table(df_scatter$v1,df_scatter$v2))
            
            names(dat) <- c("v1","v2","Count")
            dat$perc = NA
            
            for(i in unique(dat$v1))
            {
                dat$perc[dat$v1==i] = round(((dat$Count[dat$v1==i]/sum(dat$Count[dat$v1==i]))*100),2)
            }
            
            Types = dat$v2
            
            #  print(dat)
            dat = dat[dat$perc>0,]
            # print(dat)
            row.names(dat) = 1:nrow(dat)
            # print(dat)
            
            
            for(i in unique(dat$v1))
            {
                dat$perc[dat$v1==i] = round(((dat$Count[dat$v1==i]/sum(dat$Count[dat$v1==i]))*100),2)
            }
            
            Types = dat$v2
            
            plot3 =  (ggplot(data=dat, aes(x=v1, y=perc, fill=Types)) + geom_bar(stat="identity")
                      + labs(title=paste("Stacked Column :",input$columns2,"vs",input$columns3))
                      + labs(x=paste("",input$columns2),y=paste("Percentage distribution of ",input$columns3))
                      + geom_text(data = dat,aes(y=perc, label = paste(perc,"%")),size = 3, position = position_stack(vjust = 0.5))
                      + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()))
            
            
        }
        
        plot(plot3)
        
    })
    
    output$box1 = renderValueBox({
        data1 = s_temp()
        data1 = data.frame(data1)
        
        
        v1 <- data1[, input$columns2, drop = FALSE]
        v1 = data.frame(v1)
        names(v1) = 'v1'
        
        v2 <- data1[, input$columns3, drop = FALSE]
        v2 = data.frame(v2)
        names(v2) = 'v2'
        
        df_scatter = data.frame(v1,v2) 
        
        if(is.numeric(df_scatter$v2)==T&is.numeric(df_scatter$v1)==T)
        {
            c = cor.test(df_scatter$v2,df_scatter$v1)
            cor_coef = round(abs(c$estimate[[1]]),3)
            pvalue = NA
            
            chisq = NA
            a=1
            
        }
        
        if(is.numeric(df_scatter$v2)==T&is.numeric(df_scatter$v1)==F)
        {
            cor_coef = NA
            chisq = NA
            r_sq1 = NA
            pvalue = NA
            target = df_scatter$v2
            predictor = df_scatter$v1
            fit = summary(lm(target~predictor))
            r_sq = round(fit$adj.r.squared,2)*100
            a=2
            print(r_sq)
            
            
        }
        
        if(is.numeric(df_scatter$v2)==F&is.numeric(df_scatter$v1)==T)
        {
            cor_coef = NA
            chisq = NA
            r_sq = NA
            pvalue = NA
            target = df_scatter$v1
            predictor = df_scatter$v2
            fit = summary(lm(target~predictor))
            r_sq1 = round(fit$adj.r.squared,2)*100
            print(r_sq1)
            a=3
        }
        
        if(is.numeric(df_scatter$v2)==F & is.numeric(df_scatter$v1)==F)
        {
            cor_coef = NA
            r_sq = NA
            r_sq1 = NA
            tt  = chisq.test(table(data1[,c(input$columns2)],data1[,c(input$columns3)]))
            pvalue = round(tt$p.value,3)
            chisq = round(tt$statistic[[1]],3)
            tbl = matrix(table(data1[,c(input$columns2)],data1[,c(input$columns3)]))
            chisq = round(sqrt(chisq / sum(tbl)),2)
            print(chisq)
            print(pvalue)
            print(cor_coef)
            a=4
        }
        
        
        valueBox(
            tags$p("Variable Relationship",style = "font-size: 55%;"),
            subtitle = ifelse(a==1,ifelse(cor_coef<=0.19,paste(input$columns2," and",input$columns3," are Not Correlated with a correlation value of ",cor_coef),ifelse(cor_coef<=0.39,paste(input$columns2," and",input$columns3," are Weakly Correlated with a correlation value of ",cor_coef),ifelse(cor_coef<=0.59,paste(input$columns2," and",input$columns3," are Moderately Correlated with a correlation value of ",cor_coef),paste(input$columns2," and",input$columns3," are Strongly Correlated with a correlation value of ",cor_coef)))),
                              ifelse(a==2,ifelse(r_sq>5,paste(input$columns2,"explains significant variation",input$columns3),paste(input$columns2,"doesn't explain significant variation in",input$columns3)),
                                     ifelse(a==3,ifelse(r_sq1>5,paste(input$columns2,"explains significant variation",input$columns3),paste(input$columns2,"doesn't explain significant variation in",input$columns3)),
                                            ifelse(chisq<=0.19,paste(input$columns2," and",input$columns3," are Not Correlated with a correlation value of ",chisq),ifelse(chisq<=0.39,paste(input$columns2," and",input$columns3," are Weakly Correlated with a correlation value of ",chisq),ifelse(chisq<=0.59,paste(input$columns2," and",input$columns3," are Moderately Correlated with a correlation value of ",chisq),paste(input$columns2," and",input$columns3," are Strongly Correlated with a correlation value of ",chisq)))) ))     ),
            color = 'blue'
        )
        
    })
    
    output$box2_temp = renderValueBox({
        
        dataset = s_temp()
        
        df_scatter = dataset
        
        
        if(is.numeric(df_scatter[,c(input$columns2)])==T&is.numeric(df_scatter[,c(input$columns3)])==T)
        {
            cor_coef = cor.test(df_scatter[,c(input$columns2)],df_scatter[,c(input$columns3)])
            cor_coef = round((cor_coef$estimate[[1]]),3)
            tt = ifelse(cor_coef>0.6,"You may notice a pattern resembling a line going upward, which means these variables are positively related",
                        ifelse(cor_coef< (-0.6),"You may notice a pattern resembling a line going downward, which means these variables are negatively related",
                               "Since there is a lot of random distribution of dots, this means there is no correlation"))
        }
        
        if(is.numeric(df_scatter[,c(input$columns2)])==T&is.numeric(df_scatter[,c(input$columns3)])==F)
        {
            
            V1 = df_scatter[,c(input$columns2)]
            V2 = df_scatter[,c(input$columns3)]
            
            a = data.frame(V1,V2)
            
            a = data.frame(tapply(a$V1, a$V2, median))
            
            names(a) = 'Freq'
            a$Variable = row.names(a)
            
            a = a[,c(2,1)]
            
            n1 = a$Variable[which(a$Freq==max(a$Freq))]
            n2 = a$Variable[which(a$Freq==min(a$Freq))]
            
            medmax = round(max(a$Freq),2)
            medmin = round(min(a$Freq),2)
            
            three =  ifelse(medmax!=medmin,paste("Analysing",input$columns3,",","it is evident that the maximum median value is",medmax,"for",n1,"category","and the minimum median value is",medmin,"for",n2,'Category'),paste("Analysing",input$columns3,",","it is evident that the maximum median value is",medmax,"for",n1,"category"))
            
            tt = HTML(paste(three,sep="<br/>"))
            
        }
        
        if(is.numeric(df_scatter[,c(input$columns2)])==F&is.numeric(df_scatter[,c(input$columns3)])==T)
        {
            
            V1 = df_scatter[,c(input$columns3)]
            V2 = df_scatter[,c(input$columns2)]
            
            a = data.frame(V1,V2)
            
            a = data.frame(tapply(a$V1, a$V2, median))
            
            names(a) = 'Freq'
            a$Variable = row.names(a)
            
            a = a[,c(2,1)]
            
            n1 = a$Variable[which(a$Freq==max(a$Freq))]
            n2 = a$Variable[which(a$Freq==min(a$Freq))]
            
            medmax = round(max(a$Freq),3)
            medmin = round(min(a$Freq),3)
            
            three =  ifelse(medmax!=medmin,paste("Analysing",input$columns2,",","it is evident that the maximum median value is",medmax,"for",n1,"category","and the minimum median value is",medmin,"for",n2,'Category'),paste("Analysing",input$columns2,",","it is evident that the maximum median value is",medmax,"for",n1,"category"))
            
            tt = HTML(paste(three,sep="<br/>"))
            
        }
        
        if(is.numeric(df_scatter[,c(input$columns2)])==F&is.numeric(df_scatter[,c(input$columns3)])==F)
        {
            #hide(box2_temp)
            #data1 = df_transformed
            data1 = s1()
            v1 <- data1[, input$columns2, drop = FALSE]
            v1 = data.frame(v1)
            names(v1) = 'v1'
            
            v2 <- data1[, input$columns3, drop = FALSE]
            v2 = data.frame(v2)
            names(v2) = 'v2'
            
            df_scatter = data.frame(v1,v2)
            
            dat <- data.frame(table(df_scatter$v1,df_scatter$v2))
            
            names(dat) <- c("v1","v2","Count")
            dat$perc = NA
            
            
            for(i in unique(dat$v1))
            {
                dat$perc[dat$v1==i] = round(((dat$Count[dat$v1==i]/sum(dat$Count[dat$v1==i]))*100),2)
            }
            
            Types = dat$v2
            
            # print(dat)
            dat = dat[dat$perc>0,]
            # print(dat)
            row.names(dat) = 1:nrow(dat)
            # print(dat)
            
            
            for(i in unique(dat$v1))
            {
                dat$perc[dat$v1==i] = round(((dat$Count[dat$v1==i]/sum(dat$Count[dat$v1==i]))*100),2)
            }
            
            tt1 = sqldf("select v1,v2,max(perc) as perc from dat group by v1,v2")
            tt2 = sqldf("select v1,v2,min(perc) as perc from dat group by v1,v2")
            
            name1 = tt1$v1[tt1$perc == max(tt1$perc)][1]
            name2 = tt1$v2[tt1$perc == max(tt1$perc)][1]
            perc = max(tt1$perc)
            
            tt2 = tt1[tt1$v2==name2,]
            tt2 = tt2[order(tt2$perc),]
            name3 = tt2$v1[1]
            name4 = tt2$v2[1]
            perc1 = tt2$perc[1]
            list = list()
            
            
            
            #dat <- data.frame(table(df_scatter$v1,df_scatter$v2))
            
            tt1  = chisq.test(table(df_scatter$v1,df_scatter$v2))
            #pvalue = round(tt$p.value,3)
            chisq = round(tt1$statistic[[1]],3)
            tbl = matrix(table(df_scatter$v1,df_scatter$v2))
            chisq = round(sqrt(chisq / sum(tbl)),2)
            
            one = paste("Note that distributions across categories do not vary much due to their low correlation value")
            two = paste("Note that there are differences in the distribution.")
            three = paste("For eg,",name1,"has",perc,"% going into",name2,",")
            four = paste("while",name3,"has",perc1,"% for",name4)
            
            
            final = HTML(paste(two,three))
            final1 = HTML(paste(final,four))
            #tt = HTML(paste(three,sep="<br/>"))
            tt = ifelse(chisq<0.4,one,final1)
            #tt = HTML(paste(tt,three,sep="<br/>"))
            
            #  Types = dat$v2
            
            # print("Heloooooooooooo")
            # tt=""
            #bb = 1
            
        }  
        
        
        valueBox(
            tags$p("Plot Description",style = "font-size: 55%;"),
            #subtitle = paste(pred_power$Variable[1]," with predictive power of ",pred_power$Prediction_Power[1],", ",pred_power$Variable[2]," with predictive power of ",pred_power$Prediction_Power[2]," and",pred_power$Variable[3]," with predictive power of ",pred_power$Prediction_Power[3]),
            subtitle = tt,
            
            color = "blue"
        )
        
        
    })
    
    ##Predcitve Analysis
    
    ##Displaying columns in a UI (Shiny)
    output$choose_columns4 = renderUI({
        
        data1 = s1()
        
        vector = list()
        j = 1
        
        for(i in names(data1))
        {
            
            
            if(length(unique(data1[,c(i)]))>70)
            {
                vector[j] = i
                j = j + 1
            } 
            
            else if(length(unique(data1[,c(i)]))==2)
            {
                vector[j] = i
                j = j +1
            }
            
            else
            {
                print('== High Levels ==')
            }
            
        }
        
        # vector
        
        
        colnames = unlist(vector)
        
        radioButtons("columns4", label = h3("Prediction Variables"),
                     choices = colnames,selected = colnames[2])
        
        
    })
    
    
    output$data_table2 = renderDataTable({
        
        transformed_dataset = s1()
        
        if(length(unique(transformed_dataset[,c(input$columns4)]))>70)
        {
            
            target = transformed_dataset[,c(input$columns4)]
            
            
            df_sample = transformed_dataset
            df_sample[,input$columns4] = NULL
            
            pred_power = data.frame()
            
            for(i in names(df_sample))
            {
                predictor = df_sample[,i]
                
                x = summary(lm(target~predictor))
                x  = data.frame('Variable' = i,'Predition_Power' = (x$adj.r.squared*100))
                
                pred_power = rbind(pred_power,x)
                pred_power = pred_power[order(-pred_power$Predition_Power),]
                pred_power$Predition_Power = ifelse(pred_power$Predition_Power<0,0,round(pred_power$Predition_Power,2))
            }
            
            
            #print(head(pred_power))
            #pred_power$dummy = paste('Prediction power of',pred_power$Variable,'=',pred_power$Predition_Power)
            # a = paste('Predictive Strength of variables when used individually (R^2 in bivariate models)')
            
            # names(pred_power) = c('Variables','Predictive Strength')
            tt = data.frame('Predictive Strength of variables when used individually (R^2 in bivariate models)'=pred_power$Variable, 'Predictive Strength' = pred_power$Predition_Power,check.names = F)
            #tt = data.frame('Predictive Strength of variables when used individually (R^2 in bivariate models)' = pred_power$dummy,check.names = F)
            #print(pred_power$dummy)
            #print(a)
            #print(pred_power)
            
        }
        
        else
        {
            #print("Inside")
            target = ifelse(transformed_dataset[,c(input$columns4)]=='low fat',0,1)
            # print(target)
            
            df_sample = transformed_dataset
            df_sample[,c('Item_Fat_Content')] = NULL
            #df_sample[,c('Outlet_Establishment_Year')] = NULL
            
            pred_power = data.frame()
            
            for(i in names(df_sample))
            {
                predictor = df_sample[,c(i)]
                #print(target)
                #print(predictor)
                print(i)
                x = glm(target~predictor,family='binomial')
                a = hoslem.test(target, x$fitted)
                
                x  = data.frame('Variable' = i,'Predition_Power' = (a$statistic[['X-squared']]))
                
                pred_power = rbind(pred_power,x)
                pred_power = pred_power[order(-pred_power$Predition_Power),]
                pred_power$Predition_Power = ifelse(pred_power$Predition_Power<0,0,round(pred_power$Predition_Power,2))
            }
            
            #pred_power$dummy = paste('Prediction power of',pred_power$Variable,'=',pred_power$Predition_Power)
            #a = paste('Predictive Strength of variables when used individually to predict',input$columns4,"(R^2 in bivariate models)")
            tt = data.frame('Predictive Strength of variables when used individually (R^2 in bivariate models)'=pred_power$Variable, 'Predictive Strength' = pred_power$Predition_Power,check.names = F)
            #tt = data.frame('Predictive Strength of variables when used individually (R^2 in bivariate models)' = pred_power$dummy,check.names = F)
        }
        
        
        tt = tt[tt$`Predictive Strength`>0,]
        DT::datatable(tt, options = list(lengthMenu = c(10, 15, 20), pageLength = 10),rownames = FALSE)
        
    },caption ='Predictive strength of Variables')
    
    output$plot4 = renderPlot({
        
        transformed_dataset = s1()
        
        set.seed(100)
        
        if(length(unique(transformed_dataset[,c(input$columns4)]))>70)
        {
            
            target = transformed_dataset[,c(input$columns4)]
            
            
            df_sample = transformed_dataset
            df_sample[,input$columns4] = NULL
            
            pred_power = data.frame()
            
            for(i in names(df_sample))
            {
                predictor = df_sample[,i]
                
                x = summary(lm(target~predictor))
                x  = data.frame('Variable' = i,'Predition_Power' = (x$adj.r.squared*100))
                
                pred_power = rbind(pred_power,x)
                
            }
            
            pred_power = pred_power[order(-pred_power$Predition_Power),]
            pred_power$Predition_Power = ifelse(pred_power$Predition_Power<0,0,round(pred_power$Predition_Power,2))
            
            pred_power = pred_power[pred_power$Predition_Power>0,]
            print(pred_power)
            row.names(pred_power) = NULL
            
            plot4 = ggplot(pred_power, aes(x=reorder(Variable,-Predition_Power), y=Predition_Power)) + 
                geom_bar(stat="identity", width=.5, fill="tan3") + 
                labs(title="Predictive Strength",
                     #subtitle = paste("This graph describes how well",input$columns4,"varies with respect to each variable"), 
                     subtitle = paste("Predictive strength (R2) of each variable when considered individually to predict",input$columns4), 
                     xlab='',ylab='Strength')+
                theme(axis.text.x = element_text(angle=45*2, vjust=0,hjust=0),
                      axis.title.x =  element_blank())
            
        }
        
        else
        {
            #print("Inside")
            binary_breaker = data.frame(prop.table(table(transformed_dataset[,c(input$columns4)])))
            
            target = ifelse(transformed_dataset[,c(input$columns4)] == as.character(binary_breaker$Var1[binary_breaker$Freq == max(binary_breaker$Freq)]),0,1)
            
            
            #target = ifelse(transformed_dataset[,c(input$columns4)]=='low fat',0,1)
            # print(target)
            
            df_sample = transformed_dataset
            df_sample[,c(input$columns4)] = NULL
            #df_sample[,c('Outlet_Establishment_Year')] = NULL
            
            pred_power = data.frame()
            
            for(i in names(df_sample))
            {
                predictor = df_sample[,c(i)]
                #print(target)
                #print(predictor)
                print(i)
                x = glm(target~predictor,family='binomial')
                a = hoslem.test(target, x$fitted)
                
                x  = data.frame('Variable' = i,'Predition_Power' = (a$statistic[['X-squared']]))
                
                pred_power = rbind(pred_power,x)
                
            }
            
            pred_power = pred_power[order(-pred_power$Predition_Power),]
            pred_power$Predition_Power = ifelse(pred_power$Predition_Power<0,0,round(pred_power$Predition_Power,2))
            
            pred_power = subset(pred_power,pred_power$Predition_Power>0)
            
            row.names(pred_power) = NULL
            
            # plot4 = (ggplot(pred_power, aes(x=reorder(Variable,-Predition_Power), y=Predition_Power)) + geom_point(size=4)   + 
            #          geom_segment(aes(x=Variable, xend=Variable, y=0, yend=Predition_Power)) + 
            #          labs(title="Predictive Strength", xlab='Variables',ylab='Strength')) 
            
            plot4 = ggplot(pred_power, aes(x=reorder(Variable,-Predition_Power), y=Predition_Power)) + 
                geom_bar(stat="identity", width=.5, fill="tan3") + 
                labs(title="Predictive Strength",
                     subtitle = paste("Predictive strength (R2) of each variable when considered individually to predict",input$columns4), xlab='Variables',ylab='Strength')+
                theme(axis.text.x = element_text(angle=45*2, vjust=0,hjust=0),axis.title.x =  element_blank())
            
        }
        
        plot(plot4)
        
    })
    
    output$plot5 = renderPlot({
        
        dataset = s_temp()
        
        set.seed(100)
        
        transformed_dataset = dataset
        
        if(length(unique(transformed_dataset[,c(input$columns4)]))>70)
        { print("inside")
            
            smp_size <- floor(0.80 * nrow(transformed_dataset))
            
            train_ind <- sample(seq_len(nrow(transformed_dataset)), size = smp_size)
            
            train <- transformed_dataset[train_ind, ]
            test <- transformed_dataset[-train_ind, ]
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            if(min(train$target)<0)
            {
                g=abs(min(train$target))
                train$target = train$target + g
                
                test$target = test[,c(input$columns4)]
                test[,c(input$columns4)] = NULL
                test$target= test$target + g
                
                m1 = lm(log1p(target)~.,data = train)
                m1_summary = summary(m1)
                print(m1_summary$adj.r.squared) ##caption this as baseline R2 value with transoformations
                
                # mrf    = randomForest(log1p(target)~.,data = train,ntree = 250)
                # m_svms = svm(log1p(target)~.,data = train)
                
                mrf    = lm(log1p(target)~.,data = train)
                m_svms = lm(log1p(target)~.,data = train)
                
                
                ##Predictions and evaluations 
                Actuals = test$target
                
                pred1 = expm1(predict(m1,newdata = test)) -g
                pred2 = expm1(predict(mrf,newdata = test)) -g
                pred3 = expm1(predict(m_svms,newdata = test)) -g
                
                pred = 0.3*(pred1) + 0.4*(pred2) + 0.3*(pred3)
                
                a = round(((1 - smape(Actuals,pred))*100),2) ##caption this as baseline accuracy of the model 
                
                df_scatter = data.frame('Actuals' = Actuals, 'Predicted' = pred)
                
                ##Scatter plot
                #plot(df_scatter$Actuals,df_scatter$Predicted)
                
                plot5 = ggplot(df_scatter, aes(x= Actuals, y= Predicted)) +
                    geom_point(size=2, shape=16,col="purple3", fill="purple3") +
                    labs(title=paste("Actuals vs Predicted"))
                #subtitle = paste(" Baseline model explains ",round((1 - smape(Actuals,pred1))*100,1),"%"))
                
            }else
            {
                
                m1 = lm(log1p(target)~.,data = train)
                m1_summary = summary(m1)
                print(m1_summary$adj.r.squared) ##caption this as baseline R2 value with transoformations
                
                mrf    = randomForest(log1p(target)~.,data = train,ntree = 250)
                m_svms = svm(log1p(target)~.,data = train)
                
                
                ##Predictions and evaluations 
                Actuals = test[,c(input$columns4)]
                
                pred1 = expm1(predict(m1,newdata = test))
                pred2 = expm1(predict(mrf,newdata = test))
                pred3 = expm1(predict(m_svms,newdata = test))
                
                pred = 0.3*(pred1) + 0.4*(pred2) + 0.3*(pred3)
                
                a = round(((1 - smape(Actuals,pred))*100),2) ##caption this as baseline accuracy of the model 
                
                df_scatter = data.frame('Actuals' = Actuals, 'Predicted' = pred)
                
                ##Scatter plot
                #plot(df_scatter$Actuals,df_scatter$Predicted)
                
                plot5 = ggplot(df_scatter, aes(x= Actuals, y= Predicted)) +
                    geom_point(size=2, shape=16,col="purple3", fill="purple3") +
                    labs(title=paste("Actuals vs Predicted"))
                #subtitle = paste(" Baseline model explains ",round((1 - smape(Actuals,pred1))*100,1),"%"))
            }
        }
        
        else
        {
            df_sample = transformed_dataset
            binary_breaker = data.frame(prop.table(table(df_sample[,c(input$columns4)])))
            df_sample[,c(input$columns4)] = ifelse(df_sample[,c(input$columns4)] == as.character(binary_breaker$Var1[binary_breaker$Freq == max(binary_breaker$Freq)]),0,1)
            
            
            #df_sample[,c(input$columns4)] = ifelse(df_sample[,c(input$columns4)]=='low fat',0,1)
            
            
            a = sample.split(df_sample[,c(input$columns4)],SplitRatio = 0.8)
            
            train = data.frame(subset(df_sample,a==T))
            test = data.frame(subset(df_sample,a==F))
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            test$target = test[,c(input$columns4)]
            test[,c(input$columns4)] = NULL
            
            m2 = glm(target~.,data = train,family = binomial)
            
            print(summary(m2))
            
            predtest  =  predict(m2, newdata=test,type='response')
            
            con=table(test$target,predtest>=0.5)
            con=as.matrix(con)
            
            r1 = prediction(predtest,test$target)
            
            # tn  = con[1,1]
            # fp  = con[1,2]
            # fn  = con[2,1]
            # tp  = con[2,2]
            # Precision = as.numeric((tp)/(tp+fp))
            # Recall    = as.numeric((tp)/(tp+fn))
            # F1Score   = ((2*Precision*Recall)/(Precision+Recall))
            # Accuracy  = as.numeric((tn+tp)/(tn+fp+fn+tp))
            AUC       = as.numeric(performance(r1, 'auc')@y.values)
            
            tpr <- r1@tp[[1]]/max(r1@tp[[1]])
            fpr <- r1@fp[[1]]/max(r1@fp[[1]])
            
            df = data.frame('TPR' = tpr ,'FPR'= fpr)
            
            plot5 = ggplot(df,aes(FPR,TPR)) + geom_line(size = 2, alpha = 0.7,col='darkblue') +
                labs(title= paste("ROC curve of Model"), #subtitle = paste(" Baseline model explains ",round((AUC*100),1),"%"),  
                     x = "False Positive Rate (1-Specificity)", y = "True Positive Rate (Sensitivity)")
            
            # Model_Metrics = data.frame(Precision,Recall,F1Score,Accuracy,AUC)
            # Model_Metrics = data.frame('Value' = round(t(Model_Metrics)*100,2))
            # 
            # Model_Metrics$Metrics = row.names(Model_Metrics)
            # Model_Metrics = Model_Metrics[,c(2,1)]
            # 
            # row.names(Model_Metrics) = NULL
            
            
        }
        
        plot(plot5)
        
    })
    
    output$plot6 = renderPlot({
        
        transformed_dataset = s1()
        
        set.seed(100)
        
        print("entry")
        print(length(unique((transformed_dataset[,c(input$columns4)]))))
        
        if(length(unique(transformed_dataset[,c(input$columns4)]))>70)
        { print("inside")
            
            smp_size <- floor(0.80 * nrow(transformed_dataset))
            
            train_ind <- sample(seq_len(nrow(transformed_dataset)), size = smp_size)
            
            train <- transformed_dataset[train_ind, ]
            test <- transformed_dataset[-train_ind, ]
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            m1 = summary(lm(target~.,data = train))
            coefficients = data.frame(m1$coefficients)
            coefficients$Variable = row.names(coefficients)
            
            coefficients = coefficients[,c(5,1)]
            coefficients= coefficients[-1,] 
            
            coefficients$similar = NA
            
            
            for(i in 1:nrow(coefficients))
            {
                
                for(j in 1:ncol(transformed_dataset))
                {
                    
                    if(grepl(names(transformed_dataset)[j],coefficients$Variable[i])==TRUE)
                    {
                        coefficients$similar[i] = names(transformed_dataset)[j]
                        break
                    }
                    
                }
                
            }
            
            names(coefficients)[1:3] = c('categories','beta_est','Variable')
            
            coefficients$beta_est = abs(round(coefficients$beta_est,2))
            
            aggr = sqldf("select Variable,avg(beta_est) from coefficients group by 1")
            
            aggr$Perc = (aggr$`avg(beta_est)`/sum(aggr$`avg(beta_est)`))*100
            
            aggr = aggr[order(-aggr$Perc),]
            
            aggr = aggr[aggr$Perc>0,]
            
            lbl = aggr$Variable
            slices    = aggr$Perc
            
            plot6 = ggplot(aggr, aes(x=reorder(Variable,-Perc), y=Perc)) + 
                geom_bar(stat="identity", width=.5, fill="green4") + 
                labs(title="Variable Strength (Estimates)",subtitle=
                         paste("Total predictive strength that each variable has on",input$columns4,"using a full model"))+
                theme(axis.text.x = element_text(angle=90, vjust=0,hjust = 0),axis.title.x =  element_blank())
            
        }
        
        else
        {
            df_sample = transformed_dataset
            df_sample[,c(input$columns4)] = ifelse(df_sample[,c(input$columns4)]=='low fat',0,1)
            
            
            a = sample.split(df_sample[,c(input$columns4)],SplitRatio = 0.8)
            
            train = data.frame(subset(df_sample,a==T))
            test = data.frame(subset(df_sample,a==F))
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            test$target = test[,c(input$columns4)]
            test[,c(input$columns4)] = NULL
            
            m1 = mfx::logitmfx(glm(target~.,data = train,family = binomial),data = train)
            
            coefficients = data.frame(m1$mfxest)
            coefficients$Variable = row.names(coefficients)
            
            coefficients = coefficients[,c(5,1)]
            coefficients$similar = NA
            
            
            for(i in 1:nrow(coefficients))
            {
                
                for(j in 1:ncol(transformed_dataset))
                {
                    
                    if(grepl(names(transformed_dataset)[j],coefficients$Variable[i])==TRUE)
                    {
                        coefficients$similar[i] = names(transformed_dataset)[j]
                        break
                    }
                    
                }
                
            }
            
            names(coefficients)[1:3] = c('categories','beta_est','Variable')
            
            #coefficients$beta_est = scale(coefficients$beta_est)
            coefficients$beta_est = abs(round(coefficients$beta_est,2))
            
            aggr = sqldf("select Variable,avg(beta_est) from coefficients group by 1")
            
            aggr$Perc = (aggr$`avg(beta_est)`/sum(aggr$`avg(beta_est)`))*100
            
            aggr = aggr[order(-aggr$Perc),]
            
            aggr = aggr[aggr$Perc>0,]
            
            plot6 = ggplot(aggr, aes(x=reorder(Variable,-Perc), y=Perc)) + 
                geom_bar(stat="identity", width=.5, fill="green4") + 
                labs(title="Variable Strength (Estimates)",subtitle=
                         paste("Total predictive strength that each variable has on",input$columns4,"using a full model"))+
                theme(axis.text.x = element_text(angle=90, vjust=0,hjust=0),axis.title.x =  element_blank())
            
            
        }
        
        plot(plot6)
        
    })
    
    output$plot7 = renderPlot({
        
        transformed_dataset = s1()
        set.seed(100)
        
        print("entry")
        print(length(unique((transformed_dataset[,c(input$columns4)]))))
        
        if(length(unique(transformed_dataset[,c(input$columns4)]))>70)
        { print("inside")
            
            smp_size <- floor(0.80 * nrow(transformed_dataset))
            
            train_ind <- sample(seq_len(nrow(transformed_dataset)), size = smp_size)
            
            train <- transformed_dataset[train_ind, ]
            test <- transformed_dataset[-train_ind, ]
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            m1 = summary(lm(target~.,data = train))
            coefficients = data.frame(m1$coefficients)
            coefficients$Variable = row.names(coefficients)
            
            coefficients = coefficients[,c(5,1)]
            coefficients= coefficients[-1,] 
            
            coefficients$similar = NA
            
            
            for(i in 1:nrow(coefficients))
            {
                
                for(j in 1:ncol(transformed_dataset))
                {
                    
                    if(grepl(names(transformed_dataset)[j],coefficients$Variable[i])==TRUE)
                    {
                        coefficients$similar[i] = names(transformed_dataset)[j]
                        break
                    }
                    
                }
                
            }
            
            names(coefficients)[1:3] = c('categories','beta_est','Variable')
            
            #coefficients$beta_est = scale(coefficients$beta_est)
            #coefficients$beta_est = abs(round(((coefficients$beta_est - mean(coefficients$beta_est))/sd(coefficients$beta_est)),2))
            
            aggr = sqldf("select Variable,avg(beta_est) from coefficients group by 1")
            
            aggr$Coefficients = aggr$`avg(beta_est)` #(aggr$`avg(beta_est)`/sum(aggr$`avg(beta_est)`))*100
            
            aggr = aggr[order(-aggr$Coefficients),]
            
            aggr = aggr[!aggr$Coefficients==0,]
            
            plot7 = ggplot(aggr, aes(x=reorder(Variable,-Coefficients), y=Coefficients)) + 
                geom_bar(stat="identity", width=.5, fill="forestgreen") + 
                labs(title="Variable Strength (Z-scored Estimates)", ylab = 'Beta-Coefficients' ,subtitle=
                         paste("How much",'Item_Weight',"changes based on all the other variables\n(beta coefficients are standardized and transformed)"))+
                theme(axis.text.x = element_text(angle=90, vjust=0,hjust = 0),axis.title.x =  element_blank())
            
        }
        
        
        else
        {
            df_sample = transformed_dataset
            df_sample[,c(input$columns4)] = ifelse(df_sample[,c(input$columns4)]=='low fat',0,1)
            
            
            a = sample.split(df_sample[,c(input$columns4)],SplitRatio = 0.8)
            
            train = data.frame(subset(df_sample,a==T))
            test = data.frame(subset(df_sample,a==F))
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            test$target = test[,c(input$columns4)]
            test[,c(input$columns4)] = NULL
            
            m1 = mfx::logitmfx(glm(target~.,data = train,family = binomial),data = train)
            
            coefficients = data.frame(m1$mfxest)
            coefficients$Variable = row.names(coefficients)
            
            coefficients = coefficients[,c(5,1)]
            coefficients$similar = NA
            
            
            for(i in 1:nrow(coefficients))
            {
                
                for(j in 1:ncol(transformed_dataset))
                {
                    
                    if(grepl(names(transformed_dataset)[j],coefficients$Variable[i])==TRUE)
                    {
                        coefficients$similar[i] = names(transformed_dataset)[j]
                        break
                    }
                    
                }
                
            }
            
            
            names(coefficients)[1:3] = c('categories','beta_est','Variable')
            
            #coefficients$beta_est = scale(coefficients$beta_est)
            #coefficients$beta_est = abs(round(((coefficients$beta_est - mean(coefficients$beta_est))/sd(coefficients$beta_est)),2))
            
            aggr = sqldf("select Variable,avg(beta_est) from coefficients group by 1")
            
            aggr$Coefficients = aggr$`avg(beta_est)` #(aggr$`avg(beta_est)`/sum(aggr$`avg(beta_est)`))*100
            
            aggr = aggr[order(-aggr$Coefficients),]
            
            aggr = aggr[!aggr$Coefficients==0,]
            
            plot7 = ggplot(aggr, aes(x=reorder(Variable,-Coefficients), y=Coefficients)) + 
                geom_bar(stat="identity", width=.5, fill="forestgreen") + 
                labs(title="Variable Strength (Z-scored Estimates)", ylab = 'Beta-Coefficients' ,subtitle=
                         paste("How much",'Item_Weight',"changes based on all the other variables\n(beta coefficients are standardized and transformed)"))+
                theme(axis.text.x = element_text(angle=90, vjust=0,hjust = 0),axis.title.x =  element_blank())
            
            
        }
        
        plot(plot7)
        
    })
    
    output$box2 = renderValueBox({
        
        transformed_dataset = s1()
        
        if(length(unique(transformed_dataset[,c(input$columns4)]))>70)
        {
            
            target = transformed_dataset[,c(input$columns4)]
            
            
            df_sample = transformed_dataset
            df_sample[,input$columns4] = NULL
            
            pred_power = data.frame()
            
            for(i in names(df_sample))
            {
                predictor = df_sample[,i]
                
                x = summary(lm(target~predictor))
                x  = data.frame('Variable' = i,'Prediction_Power' = (x$adj.r.squared*100))
                
                pred_power = rbind(pred_power,x)
                pred_power = pred_power[order(-pred_power$Prediction_Power),]
                pred_power$Prediction_Power = ifelse(pred_power$Prediction_Power<0,0,round(pred_power$Prediction_Power,1))
            }
            
        }else
        {
            #print("Inside")
            #target = ifelse(transformed_dataset[,c(input$columns4)]=='Low Fat',0,1)
            
            
            #Binary decoding
            
            
            binary_breaker = data.frame(prop.table(table(transformed_dataset[,c(input$columns4)])))
            
            target = ifelse(transformed_dataset[,c(input$columns4)] == as.character(binary_breaker$Var1[binary_breaker$Freq == max(binary_breaker$Freq)]),0,1)
            
            # print(target)
            
            
            df_sample = transformed_dataset
            df_sample[,c(input$columns4)] = NULL
            #df_sample[,c('Outlet_Establishment_Year')] = NULL
            
            pred_power = data.frame()
            
            for(i in names(df_sample))
            {
                predictor = df_sample[,c(i)]
                #print(target)
                #print(predictor)
                print(i)
                x = glm(target~predictor,family='binomial')
                a = hoslem.test(target, x$fitted)
                
                x  = data.frame('Variable' = i,'Prediction_Power' = (a$statistic[['X-squared']]))
                
                pred_power = rbind(pred_power,x)
                pred_power = pred_power[order(-pred_power$Prediction_Power),]
                pred_power$Prediction_Power = ifelse(pred_power$Prediction_Power<0,0,round(pred_power$Prediction_Power,1))
            }
        }
        
        
        one = paste("1. ",pred_power$Variable[1]," explains",pred_power$Prediction_Power[1],"%"," of the bounce in",input$columns4)
        two = paste("2. ",pred_power$Variable[2]," explains",pred_power$Prediction_Power[2],"%"," of the bounce  in",input$columns4)
        three = paste("3. ",pred_power$Variable[3]," explains ",pred_power$Prediction_Power[3],"%"," of the bounce in",input$columns4)
        tt = HTML(paste(one,two,sep="<br/>"))
        tt1 = HTML(paste(tt,three,sep="<br/>"))
        print(tt1)
        
        
        
        valueBox(
            
            
            value = tags$p(HTML(paste("Top 3 Predictors when used individually",br(),"(R2 in bivariate models)")),style = "font-size: 50%;"),
            #subtitle = paste(pred_power$Variable[1]," with predictive power of ",pred_power$Prediction_Power[1],", ",pred_power$Variable[2]," with predictive power of ",pred_power$Prediction_Power[2]," and",pred_power$Variable[3]," with predictive power of ",pred_power$Prediction_Power[3]),
            subtitle = tt1,
            #icon = icon('thumbs-up'),
            color = "blue"
        )
        
        
    })
    
    output$box3 = renderValueBox({
        
        dataset = s_temp()
        
        transformed_dataset = dataset
        
        set.seed(100)
        
        print("entry")
        print(length(unique((transformed_dataset[,c(input$columns4)]))))
        
        if(length(unique(transformed_dataset[,c(input$columns4)]))>70)
        { print("inside")
            
            smp_size <- floor(0.80 * nrow(transformed_dataset))
            
            train_ind <- sample(seq_len(nrow(transformed_dataset)), size = smp_size)
            
            train <- transformed_dataset[train_ind, ]
            test <- transformed_dataset[-train_ind, ]
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            if(min(train$target)<0)
            {
                g = abs(min(train$target))
                train$target= train$target + g
                
                test$target = test[,c(input$columns4)]
                test[,c(input$columns4)] = NULL
                test$target= test$target + g
                
                m1 = lm(log1p(target)~.,data = train)
                m1_summary = summary(m1)
                print(m1_summary$adj.r.squared) ##caption this as baseline R2 value with transoformations
                
                # mrf    = randomForest(log1p(target)~.,data = train,ntree = 250)
                # m_svms = svm(log1p(target)~.,data = train)
                
                mrf = lm(log1p(target)~.,data = train)
                m_svms = lm(log1p(target)~.,data = train)
                
                
                ##Predictions and evaluations 
                Actuals = test$target
                
                pred1 = expm1(predict(m1,newdata = test)) -g
                pred2 = expm1(predict(mrf,newdata = test)) -g
                pred3 = expm1(predict(m_svms,newdata = test)) -g
                
                pred = 0.3*(pred1) + 0.4*(pred2) + 0.3*(pred3)
                
                a = round(((1 - smape(Actuals,pred))*100),2) ##caption this as baseline accuracy of the model 
                
                tt = a
                
            }else
            {
                
                m1 = lm(log1p(target)~.,data = train)
                m1_summary = summary(m1)
                print(m1_summary$adj.r.squared) ##caption this as baseline R2 value with transoformations
                
                mrf    = randomForest(log1p(target)~.,data = train,ntree = 250)
                m_svms = svm(log1p(target)~.,data = train)
                
                
                ##Predictions and evaluations 
                Actuals = test[,c(input$columns4)]
                
                pred1 = expm1(predict(m1,newdata = test))
                pred2 = expm1(predict(mrf,newdata = test))
                pred3 = expm1(predict(m_svms,newdata = test))
                
                pred = 0.3*(pred1) + 0.4*(pred2) + 0.3*(pred3)
                
                a = round(((1 - smape(Actuals,pred))*100),2) ##caption this as baseline accuracy of the model 
                
                tt = a
            }
            
        } 
        
        
        else
        {
            df_sample = transformed_dataset
            #df_sample[,c(input$columns4)] = ifelse(df_sample[,c(input$columns4)]=='Low Fat',0,1)
            
            
            a = sample.split(df_sample[,c(input$columns4)],SplitRatio = 0.8)
            
            train = data.frame(subset(df_sample,a==T))
            test = data.frame(subset(df_sample,a==F))
            
            train$target = train[,c(input$columns4)]
            train[,c(input$columns4)] = NULL
            
            test$target = test[,c(input$columns4)]
            test[,c(input$columns4)] = NULL
            
            m2 = glm(target~.,data = train,family = binomial)
            
            print(summary(m2))
            
            predtest  =  predict(m2, newdata=test,type='response')
            
            ##Predictions and evaluations 
            Actuals = test$target
            
            #pred1 = expm1(predict(m2,newdata = test))
            
            con=table(test$target,predtest>=0.5)
            con=as.matrix(con)
            
            r1 = prediction(predtest,test$target)
            
            # tn  = con[1,1]
            # fp  = con[1,2]
            # fn  = con[2,1]
            # tp  = con[2,2]
            # Precision = as.numeric((tp)/(tp+fp))
            # Recall    = as.numeric((tp)/(tp+fn))
            # F1Score   = ((2*Precision*Recall)/(Precision+Recall))
            # Accuracy  = as.numeric((tn+tp)/(tn+fp+fn+tp))
            AUC       = as.numeric(performance(r1, 'auc')@y.values)
            
            tpr <- r1@tp[[1]]/max(r1@tp[[1]])
            fpr <- r1@fp[[1]]/max(r1@fp[[1]])
            
            
            tt = round(AUC * 100,1)
            
        }
        
        valueBox(
            
            
            value = "",
            #subtitle = paste(pred_power$Variable[1]," with predictive power of ",pred_power$Prediction_Power[1],", ",pred_power$Variable[2]," with predictive power of ",pred_power$Prediction_Power[2]," and",pred_power$Variable[3]," with predictive power of ",pred_power$Prediction_Power[3]),
            subtitle = paste("Baseline model Accuracy is ",round(tt,1),"%"),
            
            color = "blue"
        )
        
    })
    
    ##Performance improvement function.
    ## Step 1 : A specific continuos variable is taking into consideration as DV 
    ## Step 2 : A linear regression model is built from which Beta-coefficients are extracted
    ##          and predicted. Name the prediction as 'Original'
    ## Step 3 : (Continuous IV) Variable with Max positive Beta-coefficients is increased by 20% 
    ##          and the variable with the Min Beta-coefficient is reduced by 10%
    ## Step 4 : (Binary IV) If Beta-coefficient is positive , increase the minority category using 
    ##          SMOTE
    ## Step 5 : Run a regression with the changed variables and name the predictions as changed.
    ## Step 6 : Delta = mean((original- Changed)/(original)). This will be the resultant increase in the DV
    
    output$boxperf = renderValueBox({
        
        df = s_temp()
        
        df$target = df[,c(input$columns4)]
        #x1 = as.character(names(df[,c("Item_Weight")]))
        df[,c(input$columns4)] = NULL
        
        if(length(unique(df$target))>2){
            c = apply(df, 2, function(x) length(unique(x)) == 2 )
            
            c = data.frame(c[c==T])
            row.names(c)
            
            
            ##Binary decoding
            # for(i in row.names(c))
            # {
            #   
            #   binary_breaker = data.frame(prop.table(table(df[i])))
            #   
            #   df[i] = ifelse(df[i] == as.character(binary_breaker$Var1[binary_breaker$Freq == max(binary_breaker$Freq)]),0,1)
            #   
            # }
            
            for(i in row.names(c))
            {
                df[,i] = as.factor(df[,i])
            }
            
            model    = lm(target~.,data = df)
            predorg = predict(model,newdata = df)
            
            m1 = summary(model)
            coefficients = data.frame(m1$coefficients)
            coefficients$Variable = row.names(coefficients)
            
            coefficients = coefficients[,c(5,1)]
            coefficients= coefficients[-1,] 
            
            coefficients$similar = NA
            
            
            for(i in 1:nrow(coefficients))
            {
                
                for(j in 1:ncol(df))
                {
                    
                    if(grepl(names(df)[j],coefficients$Variable[i])==TRUE)
                    {
                        coefficients$similar[i] = names(df)[j]
                        break
                    }
                    
                }
                
            }
            
            names(coefficients)[1:3] = c('categories','beta','Variable')
            
            aggr = sqldf("select Variable,avg(beta) as beta from coefficients group by 1 order by 2")
            
            dummy = data.frame(sapply(df,class))
            dummy$Variable = row.names(dummy)
            dummy$len = sapply(df,function(i) length(unique(i)))
            names(dummy)[1] = 'type'
            dummy = dummy[,c(2,1,3)]
            
            aggr1 = sqldf("select a.Variable,beta,b.type,b.len from aggr a
                    left join dummy b
                    on a.Variable = b.variable
                    where len>70 or len=2 order by 2 desc")
            
            
            a = aggr1[aggr1$type=='numeric',]
            
            n1 = a$Variable[a$beta==max(a$beta)]
            #summary(df[,c(n1)])
            
            df[,c(n1)] = df[,c(n1)] + (0.20*df[,c(n1)])
            summary(df[,c(n1)])
            
            #a = aggr1[aggr1$type=='numeric',]
            n2 = a$Variable[a$beta==min(a$beta)]
            
            #summary(df[,c(n2)])
            df[,c(n2)] = df[,c(n2)] - (0.15*df[,c(n2)])
            summary(df[,c(n2)])
            
            n3 = aggr1$Variable[aggr1$type=='factor'][1]
            if(!is.na(n3))
            {
                df$binary_scale = df[,c(n3)]
                df[,c(n3)] = NULL
                
                a = data.frame(table(df$binary_scale))
                
                
                v1 = round(min(a$Freq)*0.10,2)
                v2 = round(max(a$Freq) - v1,2)
                
                value = round((v2*100)/(v1),0) + 4
                
                rm(list=setdiff(ls(),c('aggr1','df','n1','n2','n3','predorg','value')))
                
                df = SMOTE(binary_scale~.,data = df,perc.over = 10, perc.under = value)
                
                m2 = lm(target~.,data = df)
                
                predchd = predict(m2,newdata = df)
                delta = ((predorg - predchd)/(predorg))*100
                
                r1 = round(mean(delta),2)
                result = ifelse(r1<0,'increase','decrease')
                
                r1 = ifelse(r1<0,r1*-1,r1)
                
                valueBox(value = "",
                         subtitle = paste('20% increase in',n1,', 15% decrease in',n2,'and a 10% increase in',n3,'results in a',paste0(r1,'%'),result,'in',input$columns4),
                         color = "blue")
            }
            else{
                m2 = lm(target~.,data = df)
                
                predchd = predict(m2,newdata = df)
                delta = ((predorg - predchd)/(predorg))*100
                
                r1 = round(mean(delta),2)
                result = ifelse(r1<0,'increase','decrease')
                
                r1 = ifelse(r1<0,r1*-1,r1)
                
                valueBox(value = "",
                         subtitle = paste('20% increase in',n1,', 15% decrease in',n2,'results in a',paste0(r1,'%'),result,'in',input$columns4),
                         color = "blue")
                
            }
            
            
        }
        
        else
        {
            valueBox(value = "",
                     subtitle = "The ROC curve is a fundamental tool for diagnostic test evaluation",
                     color = "blue")
        }
        
    })       
    
    ##Error Analysis
    
    ##Displaying columns in a UI (Shiny)
    output$choose_columns6 = renderUI({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        data1[,c('Outlet_Establishment_Year')] = NULL
        
        cont_names = names(data1[,sapply(data1,is.numeric)])
        
        data1 = data1[,cont_names]
        
        colnames = names(data1)[1:ncol(data1)]
        
        radioButtons("columns6", label = h3("Response Variables"),
                     choices = colnames,selected = colnames[3])
        
        
    })
    
    ##Displaying columns in a UI (Shiny)
    output$choose_columns7 = renderUI({
        
        data1 = s_temp()
        data1 = data.frame(data1)
        
        data1[,c('Outlet_Establishment_Year')] = NULL
        
        colnames = names(data1)[1:ncol(data1)]
        print(input$columns2)
        
        colnames = colnames[!(colnames %in% input$columns6)]
        
        radioButtons("columns7", label = h3("Prediction Variables"),
                     choices = colnames,selected = colnames[3])
    })
    
    ## Error correct function
    ## The predicted values of a linear regression model and the actual values are subtracted to calculate error
    ## and every IDV is plotted against errors of the Dv (Continuous variable are taken as DV)
    output$ploterror = renderPlot({
        
        ##Missing Imputation 
        
        
        data1 = s_temp()
        
        set.seed(100)
        data1 = data.frame(data1)
        
        data1$target = data1[,c(input$columns6)]
        data1[,c(input$columns6)] = NULL
        
        ##Error Analysis 
        
        
        
        if(min(data1$target)<0)
        {
            g=abs(min(data1$target))
            data1$target =data1$target +g
            
            smp_size <- floor(0.80 * nrow(data1))
            
            train_ind <- sample(seq_len(nrow(data1)), size = smp_size)
            
            train <- data1[train_ind, ]
            test <- data1[-train_ind, ]
            
            
            m3 = lm(log1p(target) ~ .,data = train)
            
            test$pred  = expm1(predict(m3,newdata = test)) -g
            
            test$error = test$target - test$pred
            
            test[,c('target','pred')] = NULL
            
            v2 = test$error
            v1 = test[,c(input$columns7)]
            
            df_scatter = data.frame('error' = v2, 'var' = v1)
            
            if(is.numeric(df_scatter$var)==T)
            {
                ploterror = ggplot(df_scatter, aes(x= var, y= error)) +
                    geom_point(size=2, shape=16,col="purple3", fill="purple3") +
                    labs(title=paste("Error vs",input$columns7), xlab = paste("",input$columns7) , ylab = 'error')
            }
            
            if(is.numeric(df_scatter$var)==F)
            {
                Types = df_scatter$var
                
                ploterror = ggplot(df_scatter, aes(x= var, y= error,fill = Types )) + geom_boxplot() +
                    labs(title=paste("Error vs",input$columns7), xlab = paste("",input$columns7) , ylab = 'error')
            }
        }else
        {
            smp_size <- floor(0.80 * nrow(data1))
            
            train_ind <- sample(seq_len(nrow(data1)), size = smp_size)
            
            train <- data1[train_ind, ]
            test <- data1[-train_ind, ]
            
            m3 = lm(log1p(target) ~ .,data = train)
            
            test$pred  = expm1(predict(m3,newdata = test))
            
            test$error = test$target - test$pred
            
            test[,c('target','pred')] = NULL
            
            v2 = test$error
            v1 = test[,c(input$columns7)]
            
            df_scatter = data.frame('error' = v2, 'var' = v1)
            
            if(is.numeric(df_scatter$var)==T)
            {
                ploterror = ggplot(df_scatter, aes(x= var, y= error)) +
                    geom_point(size=2, shape=16,col="purple3", fill="purple3") +
                    labs(title=paste("Error vs",input$columns7), xlab = paste("",input$columns7) , ylab = 'error')
            }
            
            if(is.numeric(df_scatter$var)==F)
            {
                Types = df_scatter$var
                
                ploterror = ggplot(df_scatter, aes(x= var, y= error,fill = Types )) + geom_boxplot() +
                    labs(title=paste("Error vs",input$columns7), xlab = paste("",input$columns7) , ylab = 'error')
            }
        }
        
        
        
        plot(ploterror)
        
        
        
        
        
        
        
    })
})