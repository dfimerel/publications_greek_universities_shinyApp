#load packages and data
library(shiny)
library(shinythemes)
library(ggplot2)
library(gridExtra)
source("helper.R")


# Define the ui code  
ui <- fluidPage(theme = shinytheme("flatly"),
titlePanel("Greek Universities publication records"),
 
           sidebarLayout(
              
               sidebarPanel(
		   h4("Choose the data you want to display"),
                   selectInput("dataTypes","",c("Show all universities"="all","Select specific university"="specific"),selected="all"),

                   # which university to show
	           conditionalPanel("input.dataTypes == 'specific'",uiOutput("dataTypeUi")), 
                   br(), br(),br(),br(),br(),

                   p("Only publications from ",a("Pubmed",href="https://www.ncbi.nlm.nih.gov/pubmed/",target = "_blank"), "were obtained."),
		   p("Data was obtained from ",a("Pubmed",href="https://www.ncbi.nlm.nih.gov/pubmed/",target = "_blank"),"on 06/06/2018 and include only papers published the last 5 years."),
		   br(),
		   p("Created by Danai Fimereli. Available code on ",a("Github",href="https://github.com/dfimerel"))
              
               ), #sidebar

               mainPanel(
			plotOutput("allPlot")   
               )
           ) #sidebarLayout
) #fluidPage


# Define the server code
server <- function(input, output) {

        # Create select box input for choosing cancer types
	output$dataTypeUi <- renderUI({
		selectizeInput("uniType", "",universities, selected = NULL, multiple = FALSE,options = list(placeholder = "Select university"))
	})     

	output$allPlot <- renderPlot ({
	   if (input$dataTypes=="all") {

		p1=ggplot(topJournals,aes(x=reorder(journals,-value),y=value))+geom_bar(stat="identity",fill="darkgrey")+theme_bw()+theme(axis.text.x=element_text(angle = 60, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(x="",y="Number of publications",title="Number of publications in each journal for all Greek universities")
    
                p2=ggplot(unisPubIds,aes(x=reorder(university,-NumPub),y=NumPub))+geom_bar(stat="identity",fill="darkgrey")+theme_bw()+theme(axis.text.x=element_text(angle = 70, hjust = 1),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(x="",y="Number of publications",title="Number of publications in each University")

                grid.arrange(p2,p1,ncol=2)
	   } else {

		i=input$uniType
		ggplot(yearList[[i]],aes(x=Year,y=NumPub))+geom_bar(stat="identity",fill="darkgrey")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+labs(x="",y="Number of publications",title=paste("Number of publications per year for\n",names(yearList[i]),sep=""))+scale_x_continuous(breaks=c(2013,2014,2015,2016,2017,2018),labels=c("2013","2014","2015","2016","2017","2018"))
           }

	})    
	
   

 
}


shinyApp(ui = ui, server = server)

