#server
source("load.R")

server <-function(input,output){
  

  
  
  data = reactive({
   
    
    
    infile = input$file1  
    if (is.null(infile))
      return(NULL)
    { data<-list()
      # Upload the FB data.
      FB<-read.xlsx(infile$datapath, "FB",as.data.frame = TRUE)
      FB<-FB%>%rename_all(tolower)%>%select(sex,age,birthda,inciden,topo,morp,beha,basi,icd10)
      #Upload the SW data.
      SW<-read.xlsx(infile$datapath, "SW",as.data.frame = TRUE)
      SW<-SW%>%rename_all(tolower)%>%select(sex,age,birthda,inciden,deathda,topo,morp,beha,icd10)
      #Upload the population data.
      POP<-read.xlsx(infile$datapath, "POP",as.data.frame = TRUE,stringsAsFactors=FALSE)
      POP<-POP%>%rename_all(tolower)
      data<-list(FB,SW,POP)
      return(data)
      }
  })
  
  


  
  
  
  
rose <- reactive({
  rose1 <- data()[[1]]%>%mutate(m=as.numeric(strftime(inciden, "%m")))%>%count(m,name="fbs")
  rose2 <- data()[[2]]%>%mutate(m=as.numeric(strftime(deathda, "%m")))%>%count(m,name="sws")
  rose <- rose1%>%left_join(rose2,by="m")
  return(rose)
})




  
res_s <- reactive({
  infile = input$file1  
  if (is.null(infile))
    return(res_ss)
  {
  FB<-data()[[1]]
  SW<-data()[[2]]
  POP<-data()[[3]]
  res_s<-reg_merge(FB,SW,POP,sex)
  return(res_s)
  }
})

output$table1 <- DT::renderDataTable(res_s()%>%select(sex,icdd,fhj,shj,ihj,dhj,iws_segi,dws_segi,ics2000,dcs2000,iws2000,dws2000)
                                     )


# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("res-",Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(res_s(), file, row.names = FALSE)
  }
)

output$plot <- renderPlot({
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  plot(cars)
})

  
  output$plot2 <- renderPlot({
    ggplot(rose())+
      geom_bar(stat="identity",aes(x=factor(m,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),y=fbs,fill=fbs))+
      coord_polar()+
      scale_fill_gradient(low="white",high="darkgreen")+
      theme_void()+
      theme(
        plot.title = element_text(face = "bold",hjust = 0.5,size = 18),
        plot.subtitle = element_text(face = "bold",hjust = 0.5,size = 15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = 'black', face = 'bold',size = 12))+
      guides(fill=FALSE)
    })
  
  output$plot3 <- renderPlot({
    ggplot(rose())+
      geom_bar(stat="identity",aes(x=factor(m,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),y=sws,fill=sws))+
      coord_polar()+
      scale_fill_gradient(low="white",high="red")+
      theme_void()+
      theme(
        plot.title = element_text(face = "bold",hjust = 0.5,size = 18),
        plot.subtitle = element_text(face = "bold",hjust = 0.5,size = 15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = 'black', face = 'bold',size = 12))+
      guides(fill=FALSE)
  })
  
  
  output$fbview <- renderTable({head(data()[[2]])})
  

  
  output$table <- DT::renderDataTable(
   
        DT::datatable(
          res_s()%>%
            filter(sex==input$sexx)%>%
                   select(icdd,sex,fhj,ihj,iws_segi,ics2000,iws2000),
                   
      extensions = c('Buttons','Responsive'),
      options = list(paging = TRUE,searching = TRUE,autoWidth = TRUE,ordering = TRUE,dom = 'lftripB',buttons = c('copy', 'csv', 'pdf','print')),
      class="display"
    ))

 
  
}







