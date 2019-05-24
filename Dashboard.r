library("shiny")
library("ggplot2")
library("plotly")
library(formatR)
library(stringr)
library("shinythemes")
library("dplyr")
library(shinydashboard)
library(readr)

#Dataset import

Player_master <- read_csv("C:/RPROJECT/Player_master.csv")
Team_master <- read_csv("C:/RPROJECT/Team_match_master.csv")
Team_loss <- read_csv("C:/RPROJECT/Team_loss.csv")

##for circle bar chart
circle_bar_function_player <- function(season_input,season_output)
{
  Player_Goals <- Player_master %>% filter(Season == season_input) %>% 
    group_by(Name) %>% 
    summarise (Total_Goals = sum(Goals)) 
  
  Player_Goals<- Player_Goals %>% mutate(rank = dense_rank(desc(Total_Goals)))
  
  Player_Goals_top_20_fn <- Player_Goals[which(Player_Goals$rank <= 20),]
  Player_Goals_top_20_fn <- Player_Goals_top_20_fn[order(Player_Goals_top_20_fn$rank),]
  Player_Goals_top_20_fn <- head(Player_Goals_top_20_fn, n=20)
  Player_Goals_top_20_fn <- Player_Goals_top_20_fn[c('Name','Total_Goals')]
  
  # Set a number of 'empty bar'
  empty_bar=10
  
  # Add lines to the initial dataset
  to_add = matrix(NA, empty_bar, ncol(Player_Goals_top_20_fn))
  colnames(to_add) = colnames(Player_Goals_top_20_fn)
  Player_Goals_top_20_fn=rbind(Player_Goals_top_20_fn, to_add)
  Player_Goals_top_20_fn$id=seq(1, nrow(Player_Goals_top_20_fn))
  
  # Get the name and the y position of each label
  label_data_fn=Player_Goals_top_20_fn
  number_of_bar=nrow(label_data_fn)
  angle= 90 - 360 * (label_data_fn$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data_fn$hjust<-ifelse( angle < -90, 1, 0)
  label_data_fn$angle<-ifelse(angle < -90, angle+180, angle)
  
  Player_Goals_top_20_fn$season = season_output
  label_data_fn$season = season_output
  
  return(Player_Goals_top_20_fn)
} 

circle_bar_function_label <- function(season_input,season_output)
{
  Player_Goals <- Player_master %>% filter(Season == season_input) %>% 
    group_by(Name) %>% 
    summarise (Total_Goals = sum(Goals)) 
  
  Player_Goals<- Player_Goals %>% mutate(rank = dense_rank(desc(Total_Goals)))
  
  Player_Goals_top_20_fn <- Player_Goals[which(Player_Goals$rank <= 20),]
  Player_Goals_top_20_fn <- Player_Goals_top_20_fn[order(Player_Goals_top_20_fn$rank),]
  Player_Goals_top_20_fn <- head(Player_Goals_top_20_fn, n=20)
  Player_Goals_top_20_fn <- Player_Goals_top_20_fn[c('Name','Total_Goals')]
  
  # Set a number of 'empty bar'
  empty_bar=10
  
  # Add lines to the initial dataset
  to_add = matrix(NA, empty_bar, ncol(Player_Goals_top_20_fn))
  colnames(to_add) = colnames(Player_Goals_top_20_fn)
  Player_Goals_top_20_fn=rbind(Player_Goals_top_20_fn, to_add)
  Player_Goals_top_20_fn$id=seq(1, nrow(Player_Goals_top_20_fn))
  
  # Get the name and the y position of each label
  label_data_fn=Player_Goals_top_20_fn
  number_of_bar=nrow(label_data_fn)
  angle= 90 - 360 * (label_data_fn$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data_fn$hjust<-ifelse( angle < -90, 1, 0)
  label_data_fn$angle<-ifelse(angle < -90, angle+180, angle)
  
  Player_Goals_top_20_fn$season = season_output
  label_data_fn$season = season_output
  
  return(label_data_fn)
} 


season_input <- c("2011/2012","2012/2013","2013/2014","2014/2015","2015/2016")
season_output <- c("2011/2012","2012/2013","2013/2014","2014/2015","2015/2016")
season = data.frame(season_input,season_output)

for(i in 1:nrow(season))
{
  Player_Goals_top_20 <- circle_bar_function_player(season$season_input[i],season$season_output[i])
  label_data <- circle_bar_function_label(season$season_input[i],season$season_output[i])
  
  if(i==1){
    Player_Goals_top_20_collated <- Player_Goals_top_20
    label_data_collated <- label_data} else{
      Player_Goals_top_20_collated <- rbind(Player_Goals_top_20_collated,Player_Goals_top_20)
      label_data_collated <- rbind(label_data_collated,label_data)
    }}

#use this data to create pie charts for teams tab
Wins_home = Team_master %>% group_by(Team_Name,Season) %>% summarise(HomeWins=round(sum(HomeTeam_AwayTeam=="Home" & Result=="Win")/sum(HomeTeam_AwayTeam=="Home")*100,2),
                                                              HomeLosses=round(sum(HomeTeam_AwayTeam=="Home" & Result=="Lose")/sum(HomeTeam_AwayTeam=="Home")*100,2),
                                                              HomeDraws=round(sum(HomeTeam_AwayTeam=="Home" & Result=="Draw")/sum(HomeTeam_AwayTeam=="Home")*100,2),
                                                              AwayWins=round(sum(HomeTeam_AwayTeam=="Away" & Result=="Win")/sum(HomeTeam_AwayTeam=="Away")*100,2),
                                                              AwayLosses=round(sum(HomeTeam_AwayTeam=="Away" & Result=="Lose")/sum(HomeTeam_AwayTeam=="Away")*100,2),
                                                              AwayDraws=round(sum(HomeTeam_AwayTeam=="Away" & Result=="Draw")/sum(HomeTeam_AwayTeam=="Away")*100,2)) 
#For Total and average goals of Teams
Sum_goals = Team_master %>% group_by(Team_Name,Season) %>% summarise(Total_Goals = sum(Goals_scored))
Avg_goals = Team_master %>% group_by(Team_Name,Season) %>% summarise(Total_Goals = round(mean(Goals_scored),2))

##
matches_goals<-Player_master %>%
  select(Season,`Appearances in Starting XI`,Goals,'Team Name',Name) %>%
  group_by(Season)

colnames(matches_goals)[2] <- "Appearances"
matches_goals["LastName"] = word(matches_goals$Name,2:5)

#Biggest win of each team
biggestwins<-Team_master %>%
  group_by(Team_Name,Season) %>%
  select(Team_Name,Opponent_TeamName,Season,Goals_scored,Goals_conceded) %>%
  mutate(gdrank=Goals_scored - Goals_conceded) %>%
  top_n(1,gdrank)

team_season_table<-Team_master %>%
  group_by(Team_Name,Season) %>%
  summarise(Wins=sum(Result=="Win"),Loss=sum(Result=="Lose"),Draw=sum(Result=="Draw"),Scored=sum(Goals_scored),Conceded=sum(Goals_conceded),
            HomeWins=sum(HomeTeam_AwayTeam=="Home" & Result=="Win"),
            HomeLosses=sum(HomeTeam_AwayTeam=="Home" & Result=="Lose"),
            AwayWins=sum(HomeTeam_AwayTeam=="Away" & Result=="Win"),
            AwayLosses=sum(HomeTeam_AwayTeam=="Away" & Result=="Lose"))

#Points table calculation
points_table=mutate(team_season_table,Points = as.numeric(((Wins * 3) + Draw)))
points_table = arrange(points_table,Season,desc(Points))
points_table_1112 = subset(points_table, Season == "2011/2012")
points_table_1112$Position <- 1:nrow(points_table_1112) 
points_table_1213 = subset(points_table, Season == "2012/2013")
points_table_1213$Position <- 1:nrow(points_table_1213) 
points_table_1314 = subset(points_table, Season == "2013/2014")
points_table_1314$Position <- 1:nrow(points_table_1314) 
points_table_1415 = subset(points_table, Season == "2014/2015")
points_table_1415$Position <- 1:nrow(points_table_1415) 
points_table_1516 = subset(points_table, Season == "2015/2016")
points_table_1516$Position <- 1:nrow(points_table_1516)
points_table = rbind(points_table_1112,points_table_1213,points_table_1314,points_table_1415,points_table_1516)

#Team that scored most in each season
max_Scored_1112 = points_table_1112[order(points_table_1112$Scored,decreasing = T),][1,]
max_Scored_1213 = points_table_1213[order(points_table_1213$Scored,decreasing = T),][1,]
max_Scored_1314 = points_table_1314[order(points_table_1314$Scored,decreasing = T),][1,]
max_Scored_1415 = points_table_1415[order(points_table_1415$Scored,decreasing = T),][1,]
max_Scored_1516 = points_table_1516[order(points_table_1516$Scored,decreasing = T),][1,]
max_scored = rbind(max_Scored_1516,max_Scored_1415,max_Scored_1314,max_Scored_1213,max_Scored_1112)

#Team that conceded most in each season
max_Conceded_1112 = points_table_1112[order(points_table_1112$Conceded,decreasing = T),][1,]
max_Conceded_1213 = points_table_1213[order(points_table_1213$Conceded,decreasing = T),][1,]
max_Conceded_1314 = points_table_1314[order(points_table_1314$Conceded,decreasing = T),][1,]
max_Conceded_1415 = points_table_1415[order(points_table_1415$Conceded,decreasing = T),][1,]
max_Conceded_1516 = points_table_1516[order(points_table_1516$Conceded,decreasing = T),][1,]
max_Conceded = rbind(max_Conceded_1516,max_Conceded_1415,max_Conceded_1314,max_Conceded_1213,max_Conceded_1112)

#Team that won most in each season
max_wins_1112 = points_table_1112[order(points_table_1112$Wins,decreasing = T),][1,]
max_wins_1213 = points_table_1213[order(points_table_1213$Wins,decreasing = T),][1,]
max_wins_1314 = points_table_1314[order(points_table_1314$Wins,decreasing = T),][1,]
max_wins_1415 = points_table_1415[order(points_table_1415$Wins,decreasing = T),][1,]
max_wins_1516 = points_table_1516[order(points_table_1516$Wins,decreasing = T),][1,]
max_Wins = rbind(max_wins_1516,max_wins_1415,max_wins_1314,max_wins_1213,max_wins_1112)

#Team that lost most in each season
max_Loss_1112 = points_table_1112[order(points_table_1112$Loss,decreasing = T),][1,]
max_Loss_1213 = points_table_1213[order(points_table_1213$Loss,decreasing = T),][1,]
max_Loss_1314 = points_table_1314[order(points_table_1314$Loss,decreasing = T),][1,]
max_Loss_1415 = points_table_1415[order(points_table_1415$Loss,decreasing = T),][1,]
max_Loss_1516 = points_table_1516[order(points_table_1516$Loss,decreasing = T),][1,]
max_Loss = rbind(max_Loss_1516,max_Loss_1415,max_Loss_1314,max_Loss_1213,max_Loss_1112)

#Team that drew most in each season
max_Draw_1112 = points_table_1112[order(points_table_1112$Draw,decreasing = T),][1,]
max_Draw_1213 = points_table_1213[order(points_table_1213$Draw,decreasing = T),][1,]
max_Draw_1314 = points_table_1314[order(points_table_1314$Draw,decreasing = T),][1,]
max_Draw_1415 = points_table_1415[order(points_table_1415$Draw,decreasing = T),][1,]
max_Draw_1516 = points_table_1516[order(points_table_1516$Draw,decreasing = T),][1,]
max_Draw = rbind(max_Draw_1516,max_Draw_1415,max_Draw_1314,max_Draw_1213,max_Draw_1112)

#Sum home and away wins of each team
winsaggregate = points_table %>%
  select(Team_Name,HomeWins,AwayWins) %>%
  group_by(Team_Name) %>%
  summarise_all(funs(sum))

#Sum losses by month for each team
Team_loss_n = Team_master %>%
  select(Team_Name,Month_Name,Month,Result) %>%
  group_by(Team_Name,Month_Name,Month) %>%
  summarise(Loss=sum(Result=="Lose")) %>%
  arrange(Month)

#

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Teams", tabName = "Teams", icon = icon("dashboard")),
    menuItem("Players", icon = icon("dashboard"), tabName = "Players"),
    menuItem("Performance", tabName = "Performance", icon = icon("dashboard")),
    menuItem("Insights", tabName = "Insights", icon = icon("dashboard")),
    menuItem("Points Table", tabName = "Points_Table", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Teams",
            fluidRow(
            box(selectInput(inputId =  "var", 
                            label = "Select Team",
                            choices = sort(Wins_home$Team_Name),
                            selected = T),
                selectInput(inputId =  "var2", 
                            label = "Select season",
                            choices = sort(Team_master$Season),
                            selected = T)
             ),  
            uiOutput("clublogo")
            ),
            fluidRow(
               # Dynamic valueBoxes
              valueBoxOutput("Avg_goals_output"),
              valueBoxOutput("Sum_goals_output")
            ),
            fluidRow(
              box(
                title = "Home performance"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput(outputId = "pie_home", height = "300px")
              ),
              box(
                title = "Away performance"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput(outputId = "pie_away", height = "300px")
              )),
    fluidRow(
      box(
        title = "Magic quadrant"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotlyOutput(outputId = "plot2", height = "300px")
      ),
      box(
        title = "Results across seasons"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotlyOutput(outputId = "plot3", height = "300px")
      )) 
    ),
    
    tabItem(tabName = "Players",
            fluidRow(
              box( selectInput("var3", 
                               label = "Select a player",
                               choices = sort(Player_master$Name),
                               selected = T)
                  
              ),
              uiOutput("test")),
            fluidRow(
             
              box(
                title = "Across seasons"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput(outputId = "ploty",height="300px")
                
              )
            ),
              fluidRow(
                valueBoxOutput("selected_var5"),
                valueBoxOutput("selected_var1"),
                valueBoxOutput("selected_var3"),
                valueBoxOutput("selected_var4"),
                valueBoxOutput("selected_var6"),
                valueBoxOutput("selected_var7")
              )

  ),
  tabItem(tabName = "Performance",
          fluidRow(
            box(selectInput(inputId =  "var4", 
                label = "Select season",
                choices = sort(Team_master$Season,decreasing = T),
                selected = T)
                
            )  
          ),
          fluidRow(
            # Dynamic valueBoxes
            valueBoxOutput("Titlewinner")
          ),
          fluidRow(
            # Dynamic valueBoxes
            valueBoxOutput("Max_scored_output"),
            valueBoxOutput("Max_conceded_output")
          ),
          fluidRow(
            # Dynamic valueBoxes
            valueBoxOutput("Max_wins_output"),
            valueBoxOutput("Max_losses_output"),
            valueBoxOutput("Max_draw_output")
          ),
    fluidRow(
      box(
        title = "Season top scorers"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput(outputId = "plotx", height = "300px")
      )) 
    ),
  tabItem(tabName = "Points_Table",
          h2("Points Table"),
          dataTableOutput("points_tabl")
  ),
  tabItem(tabName = "Insights",
          fluidRow(
            box(title = "Wins at Home/Away"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE
                ,plotlyOutput(outputId = "plotz",height=800
                ),width = "600px",height = "900px"
            )  
          ),
          fluidRow(
            box(title = "London teams % of loss"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE
                ,plotOutput(outputId = "plot11",height="300px"
                )
            ),
            box(title = "Other teams % of loss"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE
                ,plotOutput(outputId = "plot12",height = "300px"
                )
            )
          )
  )
   
  )
)


# Put them together into a dashboardPage
ui = dashboardPage(skin= "blue",
  dashboardHeader(title = "O JOGA BONITO"),
  sidebar,
  body
)

server <- function(input, output) {
  #Teams output - Vishali
  output$Avg_goals_output <- renderValueBox({
    valueBox(
      paste0(Avg_goals$Total_Goals[which(Avg_goals$Team_Name == input$var & Avg_goals$Season == input$var2)],""), "Average goals/match", icon = icon("fa-futbol-o",lib = "font-awesome",class="fas fa-futbol-o fa-spin"),
      color = "blue"
    )
  })
  output$Sum_goals_output <- renderValueBox({
    valueBox(
      paste0( Sum_goals$Total_Goals[which(Sum_goals$Team_Name == input$var & Sum_goals$Season == input$var2)],""), "Total goals scored", icon = icon("fa-futbol-o",lib = "font-awesome",class="fas fa-futbol-o fa-spin"),
      color = "blue"
    )
  })
  output$clublogo <- renderUI({
    tags$img(src= Team_master$ClubLogo[which(Team_master$Team_Name==input$var)])
  })
  #Sum_goals$Total_Goals[which(Sum_goals$Team_Name == input$var & Sum_goals$Season == input$var2)]
  output$pie_home <- renderPlotly({
    #xx <- list(Wins_home$HomeWins[which(Wins_home$Team_Name == input$var)],Wins_home$HomeLosses[which(Wins_home$Team_Name == "input$var")],Wins_home$HomeDraws[which(Wins_home$Team_Name == "input$var")])
    plot_ly(Wins_home[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2),], labels = c("Wins","Losses","Draws"), values = list(Wins_home$HomeWins[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2)],Wins_home$HomeLosses[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2)],Wins_home$HomeDraws[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2)]), type = 'pie',textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = c("forestgreen","tomato","deepskyblue"),   
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = '% Win - Loss - Draw',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$pie_away <- renderPlotly({
    #xx <- list(Wins_home$HomeWins[which(Wins_home$Team_Name == input$var)],Wins_home$HomeLosses[which(Wins_home$Team_Name == "input$var")],Wins_home$HomeDraws[which(Wins_home$Team_Name == "input$var")])
    plot_ly(Wins_home[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2),], labels = c("Wins","Losses","Draws"), values = list(Wins_home$AwayWins[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2)],Wins_home$AwayLosses[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2)],Wins_home$AwayDraws[which(Wins_home$Team_Name == input$var & Wins_home$Season == input$var2)]), type = 'pie',textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = c("forestgreen","tomato","deepskyblue"),   
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = '% Win - Loss - Draw',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  #Teams output - work on it
  output$plot1 <- renderPlotly({
    ggplot(data = subset(aggregate(match$Goals_scored, by = list(Season = match$Season, 
                                                                 Team_ShortName = match$Team_ShortName, Team_Name = match$Team_Name), 
                                   FUN = sum), Team_Name %in% input$team), aes(y = x, x = Team_ShortName, 
                                                                               fill = Season)) + geom_bar(stat = "identity", position = "dodge") 
  })
  output$plot2 = renderPlotly({ggplot(subset(matches_goals, matches_goals$`Team Name` == input$var & matches_goals$Season == input$var2),aes(x=Appearances,y=Goals,color=Name))+
      geom_point() +
      theme_classic()+
      geom_vline(xintercept = 4.5,linetype="dashed",color="black",size=1)+
      geom_hline(yintercept = 6,linetype="dashed",color="black",size=1)+
      geom_text(x=2,y=13,label="Super Sub",size=2,color="black")+
      geom_text(x=30,y=20,label="Match winners",size=2,color="black")+
      geom_text(x=2,y=3,label="Bench warmers",size=2,color="black")+
      geom_text(x=30,y=3,label="Rare scorers",size=2,color="black") + labs(x="Appearances")
  })
  
  #Players output

  output$ploty = renderPlot({ggplot(subset(Player_master,Player_master$Name == input$var3),aes(group=Name))+
      geom_line(aes(x=Season,y=Goals,colour="Goals")) + geom_point(aes(x=Season,y=Goals,colour="Goals")) +
      geom_line(aes(x=Season,y=`Appearances in Starting XI`,colour="Appearances in Starting XI")) + geom_point(aes(x=Season,y=`Appearances in Starting XI`,colour="Appearances in Starting XI")) + 
      theme_classic() + labs(y="Appearances/Goals",colour="Appearances vs Goals")
      })
  
  output$selected_var1 <- renderValueBox({
    valueBox(
      paste0(Player_master$Height[which(Player_master$Name==input$var3)],""), "Height", icon = icon("male",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$selected_var3 <- renderValueBox({
    valueBox(
      paste0(Player_master$Birthday[which(Player_master$Name==input$var3)],""), "Birthday", icon = icon("birthday-cake",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$selected_var2 <- renderValueBox({
    valueBox(
      paste0(Player_master$Weight[which(Player_master$Name==input$var3)],""), "Weight", icon = icon("fa-futbol-o",lib = "font-awesome",class="fas fa-futbol-o fa-spin"),
      color = "blue"
    )
  })
  output$selected_var4 <- renderValueBox({
    valueBox(
      paste0(Player_master$`Preferred Foot`[which(Player_master$Name==input$var3)],""), "Preferred foot", icon = icon("fa-futbol-o",lib = "font-awesome",class="fas fa-futbol-o fa-spin"),
      color = "blue"
    )
  })
  output$selected_var6 <- renderValueBox({
    valueBox(
      paste0(Player_master$Nationality[which(Player_master$Name==input$var3)],""), "Nationality", icon = icon("flag",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$selected_var7 <- renderValueBox({
    valueBox(
      paste0(Player_master$Wage[which(Player_master$Name==input$var3)],""), "Wages", icon = icon("credit-card",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$selected_var5 <- renderValueBox({
    valueBox(
      paste0(sum(Player_master$Goals[which(Player_master$Name==input$var3)]),""), "Goals scored in 5 seasons", icon = icon("fa-futbol-o",lib = "font-awesome",class="fas fa-futbol-o fa-spin"),
      color = "blue"
    )
  })
  

  output$test <- renderUI({
    tags$img(src= Player_master$PlayerPhoto[which(Player_master$Name==input$var3)])
  })
  #performace
  output$plot3 = renderPlotly({ggplot(subset(team_season_table,team_season_table$Team_Name == input$var),aes(group=Team_Name))+
      geom_line(aes(x=Season,y=Wins,colour="Wins")) + geom_point(aes(x=Season,y=Wins,colour="Wins")) +
      geom_line(aes(x=Season,y=Draw,colour="Draw")) + geom_point(aes(x=Season,y=Draw,colour="Draw")) +
      geom_line(aes(x=Season,y=Loss,colour="Loss")) + geom_point(aes(x=Season,y=Loss,colour="Loss")) +
      theme_classic() + labs(colour = "Result",y = "Matches")
  })
  
  output$plotx = renderPlot({
    # Make the plot
    ggplot(subset(Player_Goals_top_20_collated,season == input$var4), aes(x=as.factor(id), y=Total_Goals)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      geom_bar(stat="identity", fill=alpha("green", .3)) +
   ##   ylim(-100,120) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar(start = 0) + 
      geom_text(data=subset(label_data_collated,season == input$var4)[c('Name','Total_Goals','id','hjust','angle')], aes(x=id, y=Total_Goals+10, label=Name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
    
    
  })
  
  
  output$points_tabl = renderDataTable(points_table)
  
  output$Max_scored_output <- renderValueBox({
    valueBox(
      paste0(max_scored$Team_Name[which(max_scored$Season==input$var4)],""), "Most goals scored", icon = icon("crosshairs",lib = "font-awesome"),
      color = "green"
    )
  })
  output$Max_conceded_output <- renderValueBox({
    valueBox(
      paste0(max_Conceded$Team_Name[which(max_Conceded$Season==input$var4)],""), "Most goals conceded", icon = icon("times",lib = "font-awesome"),
      color = "red"
    )
  })
  output$Max_wins_output <- renderValueBox({
    valueBox(
      paste0(max_Wins$Team_Name[which(max_Wins$Season==input$var4)],""), "Most wins", icon = icon("smile-o",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$Max_losses_output <- renderValueBox({
    valueBox(
      paste0(max_Loss$Team_Name[which(max_Loss$Season==input$var4)],""), "Most losses", icon = icon("frown-o",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$Max_draw_output <- renderValueBox({
    valueBox(
      paste0(max_Draw$Team_Name[which(max_Draw$Season==input$var4)],""), "Most draws", icon = icon("meh-o",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$Titlewinner <- renderValueBox({
    valueBox(
      paste0(points_table$Team_Name[which(points_table$Season==input$var4 & points_table$Position == 1)],""), "Title winner", icon = icon("trophy",lib = "font-awesome"),
      color = "blue"
    )
  })
  output$plotx = renderPlot({
    # Make the plot
    ggplot(subset(Player_Goals_top_20_collated,season == input$var4), aes(x=as.factor(id), y=Total_Goals)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      geom_bar(stat="identity", fill=alpha("green", 0.3)) +
      ylim(-100,120) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar(start = 0) + 
      geom_text(data=subset(label_data_collated,season == input$var4)[c('Name','Total_Goals','id','hjust','angle')], aes(x=id, y=Total_Goals+10, label=Name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
    
    
  })
  output$plotz = renderPlotly({ggplot(winsaggregate, aes(x=HomeWins, y=AwayWins,color=Team_Name)) +
      theme_classic() +
      geom_text(check_overlap = T,aes(label=Team_Name),position = position_dodge(width=.5),size=3.5) + 
      theme(legend.text=element_text(size = rel(1))) +
      coord_fixed(ratio = 1) + 
      geom_abline(slope = .68,intercept = 0,linetype="dashed",color="black",size=0.25)})
  
  output$plot11 = renderPlot({ggplot(subset(Team_loss,Team_Name == "Chelsea" | Team_Name == "Arsenal" | Team_Name == "Fulham" | Team_Name == "Tottenham Hotspur" | Team_Name == "Queens Park Rangers"))+aes(x=Month_Name,y=Percent_Loss/5,fill=Month_Name)+geom_col()+
      theme_classic() + labs(fill = "Month",y = "% Matches lost", x="Month") + scale_x_discrete(limits=c("JAN","FEB","MAR","APR","MAY","AUG","SEP","OCT","NOV","DEC"))
  })
  
  output$plot12 = renderPlot({ggplot(subset(Team_loss,Team_Name != "Chelsea" & Team_Name != "Arsenal" & Team_Name != "Fulham" & Team_Name != "Tottenham Hotspur" & Team_Name != "Queens Park Rangers"))+aes(x=Month_Name,y=Percent_Loss/25,fill=Month_Name)+geom_col()+
      theme_classic() + labs(fill = "Month",y = "% Matches lost", x="Month") + scale_x_discrete(limits=c("JAN","FEB","MAR","APR","MAY","AUG","SEP","OCT","NOV","DEC"))
  })
}

shinyApp(ui, server)
