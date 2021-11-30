library(shiny)
#library(dplyr)
library(plotly)

  
  
#source("functions/DraftSim.R")
#source("functions/Positions.R")

batters<-  read.csv(file='PROJBATTERS.csv')
pitchers<-read.csv(file='PROJPITCHERS.csv')
#teams<-matrix(data=rep(NA,324),nrow=27,ncol=12,byrow=TRUE,dimnames=list(c("C","1B","2B","3B","SS","1B/3B","2B/SS","OF","OF","OF","OF","OF","UTIL","UTIL", "Bench","Bench","Bench","Bench", "P","P","P","P","P","P","P","P","P"))) 
#counter<-0
ui<-   fluidPage( 
  titlePanel("Fantasy Baseball Auto-Drafter and Draft Aid"),
  ####side panel 
  sidebarPanel(
  textInput(inputId = "Player",label = "Type who was Drafted",value = "Clayton Kershaw"),
actionButton(inputId = "submit",label = "submit"),
actionButton(inputId = "start",label ="Autodraft"),
numericInput(inputId = "pos",label = "Enter your draft Position",value = 1)),

##main panel
mainPanel(
  h4("Please select your draft position"),
  h4("Once a player has been drafted, type in his name and hit submit"),
  h4("Select autodraft for a generated version of the whole draft (this may take a moment)"),
  tabsetPanel(
tabPanel("Best choices",tableOutput("table")),
tabPanel("My team",tableOutput("team")),
tabPanel("Plot",plotlyOutput("plot",width = "100%")),
tabPanel("Autodraft",tableOutput("auto")) 

   )))
  


server<-function(input,output){
  DraftSim<-function(batters,pitchers){
   # library(plotly)
    # batters<-read.csv("C:/Users/Harris/Documents/shiny/PROJBATTERS.csv")
    #pitchers<-read.csv("C:/Users/Harris/Documents/shiny/PROJPITCHERS.csv")
    ##creates the team matrices 
    teams<-matrix(data=rep(NA,324),nrow=27,ncol=12,byrow=TRUE,dimnames=list(c("C","1B","2B","3B","SS","1B/3B","2B/SS","OF","OF","OF","OF","OF","UTIL","UTIL", "Bench","Bench","Bench","Bench", "P","P","P","P","P","P","P","P","P"))) 
    ##creates the order of a normal 12 team snake draft
    order <- rep(c(seq(1:12),seq(12,1)),27)
    all<-data.frame()
    for (ii in 1:324){
      
      
      
      ###creates the forumla for the value score
      PITCHERDIFF<-pitchers[1,14]-mean(pitchers[2:6,14])  
      PITCHERSCORE<-pitchers[1,14]+PITCHERDIFF  
      
      FIRST<-subset(batters,batters$POS1=="1B"| batters$POS2=="1B" | batters$POS3=="1B")
      FIRSTDIFF<-FIRST[1,15]-mean(FIRST[2:6,15])
      FIRSTSCORE<-FIRST[1,15]+FIRSTDIFF
      
      SECOND<-subset(batters,batters$POS1=="2B" | batters$POS2=="2B" | batters$POS3=="2B")
      SECONDDIFF<-SECOND[1,15]-mean(SECOND[2:6,15])
      SECONDSCORE<-SECOND[1,15]+SECONDDIFF
      
      OUTFIELD <-subset(batters,batters$POS1=="OF"| batters$POS2=="OF" |batters$POS3=="OF")
      OUTFIELDDIFF<-OUTFIELD[1,15]-mean(OUTFIELD[2:6,15])
      OUTFIELDSCORE<-OUTFIELD[1,15]+OUTFIELDDIFF
      
      CATCHER<-subset(batters,batters$POS1=="C"| batters$POS2=="C" | batters$POS3=="C")
      CATCHERDIFF<-CATCHER[1,15]-mean(CATCHER[2:6,15])
      CATCHERSCORE<-CATCHER[1,15]+CATCHERDIFF
      
      THIRD<-subset(batters,batters$POS1=="3B"| batters$POS2=="3B" | batters$POS3=="3B")
      THIRDDIFF<-THIRD[1,15]-mean(THIRD[2:6,15])
      THIRDSCORE<-THIRD[1,15]+THIRDDIFF
      
      
      SHORTSTOP<-subset(batters,batters$POS1=="SS"| batters$POS2=="2B" | batters$POS3=="2B")
      SHORTSTOPDIFF<-SHORTSTOP[1,15]-mean(SHORTSTOP[2:6,15])
      SHORTSTOPSCORE<-SHORTSTOP[1,15]+SHORTSTOPDIFF
      
      MAX<-max(FIRSTSCORE,SECONDSCORE,THIRDSCORE,OUTFIELDSCORE,PITCHERSCORE,CATCHERSCORE,SHORTSTOPSCORE)
      
      
      
      if(FIRSTSCORE==MAX){
        y<-as.character(FIRST[1,4])}
      if (SECONDSCORE==MAX){
        y<-as.character(SECOND[1,4])}
      if (THIRDSCORE==MAX){
        y<-as.character(THIRD[1,4])}
      if (CATCHERSCORE==MAX){
        y<- as.character(CATCHER[1,4])}
      if (OUTFIELDSCORE==MAX){
        y<- as.character(OUTFIELD[1,4])}
      if (SHORTSTOPSCORE==MAX){
        y<-as.character(SHORTSTOP[1,4])} 
      if (PITCHERSCORE==MAX){
        y<- as.character(pitchers[1,1])
      }
      
      
      
      ###identifies position of each player
      d<-"P"
      for(k in 1:length(batters$NAME)){ 
        if (y==batters[k,4]){
          d<-as.character(batters[k,1])
        }
        
      }
      
      
      
      
      ##subsets dataframe by removing player that was drafted
      batters<-subset(batters,batters$NAME!= y)
      pitchers<-subset(pitchers,pitchers$Name!=y) 
      
      ##function that places drafted player onto roster
      # teams<-Positions(teams,d,order,ii,y)
      all[ii,1]<-y
      
    }
    #return(teams)
    return(all)
  }
  Positions<-function(teams,d,order,ii,y){
    if (is.na(teams[d,order[ii]]))
    {teams[d,order[ii]]<-y}
    else if (d=="OF"){
      OF<-0
      for (i in 9:18){
        if (is.na(teams[i,order[ii]])  && OF==0){
          teams[i,order[ii]]<-y
          OF<-OF+1
        }
        
      }
    }
    else if (d=="c"){
      c<-0
      for (i in 13:18){
        if (is.na(teams[i,order[ii]])  && c==0){
          teams[i,order[ii]]<-y
          c<-c+1
        }
      }
    }
    
    else if (d=="1B"){
      first<-0
      for (i in c(6,13,14,15,16,17,18)){
        if (is.na(teams[i,order[ii]])  && first==0){
          teams[i,order[ii]]<-y
          first<-first+1
        }
      }
    }
    else if (d=="3B"){
      third<-0
      for (i in c(6,13,14,15,16,17,18)){
        if (is.na(teams[i,order[ii]])  && third==0){
          teams[i,order[ii]]<-y
          third<-third+1
        }
      }
    }
    else if (d=="2B"){
      second<-0
      for (i in c(7,13,14,15,16,17,18)){
        if (is.na(teams[i,order[ii]])  && second==0){
          teams[i,order[ii]]<-y
          second<-second+1
        }
      }
    }
    else if (d=="SS"){
      short<-0
      for (i in c(7,13,14,15,16,17,18)){
        if (is.na(teams[i,order[ii]])  && short==0){
          teams[i,order[ii]]<-y
          short<-short+1
        }
      }
    }
    else if (d=="P"){
      pitcher<-0
      for (i in c(20,21,22,23,24,25,26,27,15,16,17,18)){
        if (is.na(teams[i,order[ii]])  && pitcher==0){
          teams[i,order[ii]]<-y
          pitcher<-pitcher+1
        }
      }
    }
    else if (d=="DH"){
      dh<-0
      for (i in c(13:18)){
        if (is.na(teams[i,order[ii]])  && dh==0){
          teams[i,order[ii]]<-y
          dh<-dh+1
        }
      }
    }
    
    
    return(teams)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  cnt<-reactiveValues(x=0)
  teams<-reactiveValues(teams=matrix(data=rep(NA,324),nrow=27,ncol=12,byrow=TRUE,dimnames=list(c("C","1B","2B","3B","SS","1B/3B","2B/SS","OF","OF","OF","OF","OF","UTIL","UTIL", "Bench","Bench","Bench","Bench", "P","P","P","P","P","P","P","P","P"))) )
 # teams$teams<-matrix(data=rep(NA,324),nrow=27,ncol=12,byrow=TRUE,dimnames=list(c("C","1B","2B","3B","SS","1B/3B","2B/SS","OF","OF","OF","OF","OF","UTIL","UTIL", "Bench","Bench","Bench","Bench", "P","P","P","P","P","P","P","P","P"))) 

   
Position<-c("C","1B","2B","3B","SS","1B/3B","2B/SS","OF1","OF2","OF3","OF4","OF5","UTIL1","UTIL2", "Bench1","Bench2","Bench3","Bench4", "P1","P2","P3","P4","P5","P6","P7","P8","P9") 
order <- rep(c(seq(1:12),seq(12,1)),27)
  

      vals<-reactiveValues()
      values<-reactiveValues()
      vals$batters<-batters
      values$pitchers<-pitchers
 
      
  
  
  player<-reactive({
   input$Player
 })
  

 
observeEvent(input$submit,{

isolate({ cnt$x<-cnt$x+1})

  players<- player()
  
 ###identifies position of each player
 batters<-vals$batters
 d<-"P"
 for(k in 1:length(batters$NAME)){ 
   if (player()==batters[k,4]){
     d<-as.character(batters[k,1])
   }
   
 }
############################# 
 #######################
 ##function that places drafted player onto roster

 teams$teams<-Positions(teams$teams,d,order,cnt$x,players)
 teams<-teams$teams 
 myteam<-teams[,as.numeric(input$pos)]
team_tracker<-data.frame(myteam,Position)
colnames(team_tracker)<-c("Player","Position")  
output$team<-renderTable(team_tracker)
 

 ###subsets the dataframes

 for (i in 1:length(values$pitchers)){
   x<-reactive({x<-values$pitchers[i,1]})
   
   if(x()==player()){
     values$pitchers<-values$pitchers[-i,]
     
   }
   
 }
 for (i in 1:length(vals$batters)){
   x<-reactive({x<-vals$batters[i,4]})
   
   if(x()==player()){
     vals$batters<-vals$batters[-i,]
     
   }
   
 }

  pitchers<-values$pitchers  
  PITCHERDIFF<-values$pitchers[1,14]-mean(values$pitchers[2:6,14])  
  PITCHERSCORE<-values$pitchers[1,14]+PITCHERDIFF  
  
  batters<-vals$batters
  
  
  FIRST<-subset(batters,batters[ ,1]=="1B"| batters[ ,1]=="1B" | batters[ ,1]=="1B")
  FIRSTDIFF<-FIRST[1,15]-mean(FIRST[2:6,15])
  FIRSTSCORE<-FIRST[1,15]+FIRSTDIFF
  
 SECOND<-subset(vals$batters,batters[ ,1]=="2B" | batters[ ,1]=="2B" | batters[ ,1]=="2B")
  SECONDDIFF<-SECOND[1,15]-mean(SECOND[2:6,15])
  SECONDSCORE<-SECOND[1,15]+SECONDDIFF
  
  OUTFIELD <-subset(batters,batters[ ,1]=="OF"| batters[ ,1]=="OF" |batters[ ,1]=="OF")
  OUTFIELDDIFF<-OUTFIELD[1,15]-mean(OUTFIELD[2:6,15])
  OUTFIELDSCORE<-OUTFIELD[1,15]+OUTFIELDDIFF
  
  CATCHER<-subset(batters,batters[ ,1]=="C"| batters[ ,1]=="C" | batters[ ,1]=="C")
  CATCHERDIFF<-CATCHER[1,15]-mean(CATCHER[2:6,15])
  CATCHERSCORE<-CATCHER[1,15]+CATCHERDIFF
  
  THIRD<-subset(batters,batters[ ,1]=="3B"| batters[ ,1]=="3B" | batters[ ,1]=="3B")
  THIRDDIFF<-THIRD[1,15]-mean(THIRD[2:6,15])
  THIRDSCORE<-THIRD[1,15]+THIRDDIFF
  
  
  SHORTSTOP<-subset(batters,batters[ ,1]=="SS"| batters[ ,1]=="2B" | batters[ ,1]=="2B")
  SHORTSTOPDIFF<-SHORTSTOP[1,15]-mean(SHORTSTOP[2:6,15])
  SHORTSTOPSCORE<-SHORTSTOP[1,15]+SHORTSTOPDIFF
  
 MAX<-max(FIRSTSCORE,SECONDSCORE,THIRDSCORE,OUTFIELDSCORE,PITCHERSCORE,CATCHERSCORE,SHORTSTOPSCORE)
 
 
######creates the table that shows the best at each position
 output$table<-renderTable({
   x<-  c(FIRSTSCORE,SECONDSCORE,THIRDSCORE,OUTFIELDSCORE,PITCHERSCORE,CATCHERSCORE,SHORTSTOPSCORE)
   y<- c(as.character(FIRST[1,4]),  as.character(SECOND[1,4]),as.character(THIRD[1,4]), as.character(OUTFIELD[1,4]),as.character(pitchers[1,1]),as.character(CATCHER[1,4]),as.character(SHORTSTOP[1,4]))
  z<-c("1B","2B","3B","OF","P","C","SS")
   df<-data.frame(x,y,z)
   colnames(df)<-c("Value Score","Name","POS")
     df<-df[order(-df[,1]),]
     df             
                  })
 
  
  
  
  
  output$plot<-renderPlotly({x<-c(1,2,3,4,5)
  
  
  
  data<-data.frame(x,pitchers[1:5,14],OUTFIELD[1:5,15],FIRST[1:5,15],SECOND[1:5,15],THIRD[1:5,15],SHORTSTOP[1:5,15],CATCHER[1:5,15])
  
  
  
  
  
  
  
    plot_ly(data,x= ~x,y = pitchers[1:5,14],text=values$pitchers[1:5,1],hoverinfo='text+y',name='pitchers',type='scatter',mode='lines+markers')  %>%
    add_trace(data=data,y=~OUTFIELD[1:5,15],text=OUTFIELD[1:5,4],hoverinfo='text+y',name='Outfielders',mode='lines+markers')%>%
    add_trace(data=data,y=~FIRST[1:5,15],text=FIRST[1:5,4],hoverinfo='text+y',name='First Basemen',mode='lines+markers')%>%
    add_trace(data=data,y=~SECOND[1:5,15],text=SECOND[1:5,4],hoverinfo='text+y',name='Second Basemen',mode='lines+markers')%>%
    add_trace(data=data,y=~CATCHER[1:5,15],text=CATCHER[1:5,4],hoverinfo='text+y',name='Catcher',mode='lines+markers')%>%
    add_trace(data=data,y=~SHORTSTOP[1:5,15],text=SHORTSTOP[1:5,4],hoverinfo='text+y',name='Short Stop',mode='lines+markers')%>%
    add_trace(data=data,y=~THIRD[1:5,15],text=THIRD[1:5,4],hoverinfo='text+y',name='Third Basemen',mode='lines+markers')%>%
    layout(xaxis=list(title="Rank"),yaxis=list(title="Projected Points"))  
  

 })
 
  
  

  })
  
 observeEvent(input$start,{
   output$auto<-renderTable({
   
   all<-DraftSim(batters,pitchers)
  
   # output$auto<-renderTable(data.frame(all,Position))
    df<-data.frame(all,1:324)
   colnames(df)<-c("Player","Pick number")
   df
    })
 })



}
  
     
  
  
  
  
shinyApp(ui=ui,server = server)
