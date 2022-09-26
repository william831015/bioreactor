library(shiny)
library(colourpicker)
library(shinythemes)
options(shiny.maxRequestSize = 100*1024^2)

#global variables----
columnname = c("Duration"=2,"Dissolved_Oxygen"=5,"PumpA_PV"=9,"pumpA_SP"=10,"PumpB_PV"=12,"PumpB_SP"=13,"PumpC_PV"=14,"PumpC_SP"=15
               ,"PumpD_PV"=17,"PumpD_SP"=18,"PumpE_PV"=19,"PumpE_SP"=20,"PumpF_PV"=21,"PumpF_SP"=22,"IntA"=25,"IntB"=26,"IntC"=27,"IntD"=28,"Level"=29,
               "AU"=33,"pH_PV"=40,"pH_SP"=41,"Temperature_PV"=43,"Temperature_SP"=44,"Vessel_Volume"=45)



# User Interface ----
ui <- fluidPage(
  #navbarPage
  navbarPage(
    "Saenz Lab ",
    
    
    #first tap(Data processing)----
    tabPanel("Process Raw Data",
             titlePanel("Data processing"),
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               #Input: Vessel number----
               radioButtons("VesselNumber", "Vessel Number",
                            choices = c("1"=1,"2"=2,"3"=3,"4"=4), inline=T,
                            selected = 1),
               #Input: select columns----
               checkboxGroupInput("selected_column", "select column:",
                                  columnname, selected = c("2","14","17","27","33","40","43"),inline=T),
               #Input: Select a file ----
               fileInput("file1", "Upload File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               #Input: process button----
               actionButton("button", "Process Data")

             ),
             
             
             
             #Main panel Four subpanels disply data with table----
             mainPanel(
               tabsetPanel(
                 #vessel 1----
                 tabPanel("vessel 1",
                          textOutput("debug"),
                          tableOutput("v1_table")),
                 #vessel 2----
                 tabPanel("vessel 2",
                          tableOutput("v2_table")),
                 #vessel 3----
                 tabPanel("vessel 3",
                          tableOutput("v3_table")),
                 #vessel 4----
                 tabPanel("vessel 4",
                          tableOutput("v4_table"))))),
    
    
    
    #Second Tap(plottin section)----
    tabPanel("Plotting",
             titlePanel("Plotting"),
             uiOutput("Plotting_Control"),
             
             mainPanel(
               
               tabsetPanel(

                 #vessel 1----
                 tabPanel("vessel 1",
                          plotOutput("v1_plot"),
                          actionButton("calculate1", "Calculate"),
                          tableOutput("v1_sta")),
                        
                 #vessel 2----
                 tabPanel("vessel 2",
                          plotOutput("v2_plot"),
                          textOutput("v2_mean"),
                          textOutput("v2_std")),
                 #vessel 3----
                 tabPanel("vessel 3",
                          plotOutput("v3_plot"),
                          textOutput("v3_mean"),
                          textOutput("v3_std")),
                 #vessel 4----
                 tabPanel("vessel 4",
                          plotOutput("v4_plot"))))

    ),
    
    
    #Third Tap(Script Download) ----
    tabPanel("Download Scripts")
  )
)










# server file ----
server <- function(input, output) {
  
  #process data function----
  process_data <- reactive({
    req(input$file1)
    #input file
    inFile <- input$file1
    #skip line selection
    if (input$VesselNumber==1){skipN = 201}
    else if(input$VesselNumber==2){skipN = 255}  
    else if(input$VesselNumber==3){skipN = 254}#need to be updated
    else{skipN = 254}
    
    rawData <- read.table(inFile$datapath,header = FALSE, sep = ';',dec=',', skip = skipN ,fill=TRUE)
    # Rename columns
    s=0
    for (i in (columnname)){
      s=s+1
      names(rawData)[i]<-names(columnname[s])
    }
    
    vesselNumber = input$VesselNumber
    selectedColumn <<- as.numeric(input$selected_column)
    selectedColumn_name<<-names(columnname[match(selectedColumn,columnname)])
    startPoint = which(rawData$Duration==0)
    endPoint =  which(rawData$Duration=="")
    dataLength = endPoint[1]-startPoint[1]
    mydata <- list()
    
    for (i in 1:(vesselNumber)){
      vn =paste("vessel",i,sep="")
      mx = (rawData[(((i-1)*dataLength+3):(i*dataLength)),selectedColumn])
      for (l in 1:length(selectedColumn)){
        mx[,l]=as.character(mx[,l])
        mx[,l]=as.numeric(mx[,l])
      }
      mx[1]=mx[1]*24
      assign(vn,mx)
      mydata[[i]]=mx
    }

    return(mydata)
  })
  
  
  #send processed data to table----
  processing_data<- observeEvent(input$button,
                                { req(process_data())
                                  processed_data<<-process_data()
                                  if(input$VesselNumber ==1){
                                    #output$debug <- renderText({class(as.numeric(input$selected_column))})
                                    output$v1_table <- renderTable({head(processed_data[[1]],n=100)},na="")
                                  }
                                  else if (input$VesselNumber ==2){
                                    output$v1_table <- renderTable({head(processed_data[[1]],n=100)},na="")
                                    output$v2_table <- renderTable({head(processed_data[[2]],n=100)},na="")
                                  }
                                  else if (input$VesselNumber ==3){
                                    output$v1_table <- renderTable({head(processed_data[[1]],n=100)},na="")
                                    output$v2_table <- renderTable({head(processed_data[[2]],n=100)},na="")
                                    output$v3_table <- renderTable({head(processed_data[[3]],n=100)},na="")
                                  }
                                      else{
                                    output$v1_table <- renderTable({head(processed_data[[1]],n=100)},na="")
                                    output$v2_table <- renderTable({head(processed_data[[2]],n=100)},na="")
                                    output$v3_table <- renderTable({head(processed_data[[3]],n=100)},na="")
                                    output$v4_table <- renderTable({head(processed_data[[4]],n=100)},na="")
                                      }
                                  #plotting control panel----
                                  output$Plotting_Control<-renderUI({
                                    sidebarPanel(
                                      checkboxGroupInput("plot_column","select column you want to plot",choices=selectedColumn_name[2:length(selectedColumn_name)],inline = TRUE),
                                      actionButton("plot_button", "Plot"),
                                      hr(),
                                      selectizeInput("parameters","select column",choices = selectedColumn_name[2:length(selectedColumn_name)],selected=selectedColumn_name[2]),
                                      # create some select input
                                      lapply(2:length(selectedColumn), function(i){
                                        ymin=round(min(na.omit(processed_data[[1]][,i]))*0.8)
                                        ymax=round(max(na.omit(processed_data[[1]][,i]))*1.2)
                                        conditionalPanel(condition = paste0("input.parameters==",paste("'"),selectedColumn_name[i],paste("'")),
                                                         #h4(selectedColumn_name[i]),
                                                         colourInput(paste0("color_",selectedColumn_name[i]), "Color","black"),
                                                         sliderInput(paste0("lwd_",selectedColumn_name[i]), "Line Width",step=0.1,min = 0, max = 5.0
                                                                     ,value = 1.0),
                                                         sliderInput(paste0("slider_",selectedColumn_name[i]),"y_axis Scale",
                                                                     min = ymin, max = ymax,step=(ymax-ymin)/100
                                                                     ,value = c(ymin,ymax))
                                        )}
                                      ),
                                      
                                      sliderInput("slider_Duration","Time scale",min = round(min(processed_data[[1]][,1])), max = round(max(processed_data[[1]][,1]))
                                                  ,value = c(round(min(processed_data[[1]][,1]),2),round(max(processed_data[[1]][,1]),2)))
                                      
                                    )
                                  })
                                  
                                  })
  
  #plotting function----
    #plotting panel control----
    #plotting----
    plotting_data <- observeEvent(input$plot_button,{
    plot_column<<-input$plot_column
    vn = input$VesselNumber
    if(vn>0){
      s=0
      output$v1_plot<-renderPlot({
        time=processed_data[[1]][,1]
        duration= input$slider_Duration
        start=which(abs(time-duration[1])==min(abs(time-duration[1])))
        end=which(abs(time-duration[2])==min(abs(time-duration[2])))
        par(mar=c(5,length(plot_column)*5,3,2))
        mean_vector1<<-c()
        std_vector1<<-c()
        max_vector1<<-c()
        min_vector1<<-c()
        for(i in plot_column[1:length(plot_column)]){
          s=s+1
          cutted_data=processed_data[[1]][start:end,]
          ix.na=which(!is.na(cutted_data[,i]))
          mean_vector1[s]<<-round(mean(cutted_data[ix.na,i]),2)
          std_vector1[s]<<-round(sd(cutted_data[ix.na,i]),2)
          max_vector1[s]<<-round(max(cutted_data[ix.na,i]),2)
          min_vector1[s]<<-round(min(cutted_data[ix.na,i]),2)
          yaxis=paste0('input$slider_',i)
          color=paste0('input$color_',i)
          lineWidth=paste0('input$lwd_',i)
          plot(cutted_data[ix.na,1],cutted_data[ix.na,i],axes=F,type='l',lwd = as.numeric(eval(parse(text=lineWidth))),ylim = as.numeric(eval(parse(text=yaxis))),xlab="",ylab="",col=eval(parse(text=color)))
          axis(2,ylim=as.numeric(eval(parse(text=yaxis))),col=eval(parse(text=color)),line=s*4-4,xpd = TRUE)
          mtext(2,text=i,col=eval(parse(text=color)),line=s*4-2)
          par(new=T)}
          axis(1,xlim=min(cutted_data[ix.na,1]):max(cutted_data[ix.na,1]))
          mtext(1,text="Hour",line=3,tck=1)
      },res=60)
      if(vn>1){
        s=0
        output$v2_plot<-renderPlot({
          time=processed_data[[2]][,1]
          duration= input$slider_Duration
          start=which(abs(time-duration[1])==min(abs(time-duration[1])))
          end=which(abs(time-duration[2])==min(abs(time-duration[2])))
          par(mar=c(5,length(plot_column)*5,3,2))
          for(i in plot_column[1:length(plot_column)]){
            s=s+1
            cutted_data=processed_data[[2]][start:end,]
            ix.na=which(!is.na(cutted_data[,i]))
            mean_vector2[s]<<-round(mean(cutted_data[ix.na,i]),1)
            std_vector2[s]<<-round(sd(cutted_data[ix.na,i]),1)
            yaxis=paste0('input$slider_',i)
            color=paste0('input$color_',i)
            lineWidth=paste0('input$lwd_',i)
            plot(cutted_data[ix.na,1],cutted_data[ix.na,i],axes=F,type='l',lwd = as.numeric(eval(parse(text=lineWidth))),ylim = as.numeric(eval(parse(text=yaxis))),xlab="",ylab="",col=eval(parse(text=color)))
            axis(2,ylim=as.numeric(eval(parse(text=yaxis))),col=eval(parse(text=color)),line=s*4-4,xpd = TRUE)
            mtext(2,text=i,col=eval(parse(text=color)),line=s*4-2)
            par(new=T)}
            axis(1,xlim=min(cutted_data[ix.na,1]):max(cutted_data[ix.na,1]))
            mtext(1,text="Hour",line=3,tck=1)
          
        })
        if(vn>2){
          output$v3_plot<-renderPlot({
          time=processed_data[[3]][,1]
          duration= input$slider_Duration
          start=which(abs(time-duration[1])==min(abs(time-duration[1])))
          end=which(abs(time-duration[2])==min(abs(time-duration[2])))
          mean_vector1<<-c()
          std_vector1<<-c()
          for(i in plot_column[1:length(plot_column)]){
            cutted_data=processed_data[[3]][start:end,]
            ix.na=which(!is.na(cutted_data[,i]))
            
            yaxis=paste0('input$slider_',i)
            color=paste0('input$color_',i)
            lineWidth=paste0('input$lwd_',i)
            plot(cutted_data[ix.na,1],cutted_data[ix.na,i],axes=F,type='l',lwd = as.numeric(eval(parse(text=lineWidth))),ylim = as.numeric(eval(parse(text=yaxis))),col=eval(parse(text=color)))
            par(new=T)}
            
        })
          if(vn>3){
            output$v4_plot<-renderPlot({
            time=processed_data[[4]][,1]
            duration= input$slider_Duration
            start=which(abs(time-duration[1])==min(abs(time-duration[1])))
            end=which(abs(time-duration[2])==min(abs(time-duration[2])))
            for(i in plot_column[1:length(plot_column)]){
              cutted_data=processed_data[[4]][start:end,]
              ix.na=which(!is.na(cutted_data[,i]))
              yaxis=paste0('input$slider_',i)
              color=paste0('input$color_',i)
              lineWidth=paste0('input$lwd_',i)
              plot(cutted_data[ix.na,1],cutted_data[ix.na,i],axes=F,type='l',lwd = as.numeric(eval(parse(text=lineWidth))),ylim = as.numeric(eval(parse(text=yaxis))),col=eval(parse(text=color)))
              par(new=T)}
          })
            
          }
        }
      }
    }
    
    })            

    
    calculate1 <- observeEvent(input$calculate1,{
      data_table1 = cbind(input$plot_column,mean_vector1,std_vector1,max_vector1,min_vector1)
      colnames(data_table1)<-c(" ","mean","std","max","min")
      output$v1_sta<-renderTable({
          (data_table1)
      })
      
    })
    
    
    
}




# Create Shiny app ----
shinyApp(ui, server)
