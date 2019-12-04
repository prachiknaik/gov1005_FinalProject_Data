library(sas7bdat)
library(shinythemes)
library(ggplot2)
library(grid)
library(caTools)
library(corrplot)
library("Hmisc")
library(ggridges)
library(broom)
library(gt)
library(sjPlot)
library(stargazer)
library(tidyverse)

# A. Read in relevant data that we copied over from the clean-data file to App 

diag_conducted_by <- read_rds("diag_conducted_by.rds")
diagcorrelogram <- read_rds("diagcorrelogram.rds")
schooltypes <- read_rds("schooltypes.rds")
school_characteristics_vis <- read_rds("school_characteristics_vis.rds")
diagprovidedgraph_ELL <- read_rds("diagprovidedgraph_ELL.rds")
diagprovidedgraph_bfif <- read_rds("diagprovidedgraph_bfif.rds")
diagprovidedgraph_cb <- read_rds("diagprovidedgraph_cb.rds")
diagprovidedgraph_sm <- read_rds("diagprovidedgraph_sm.rds")
diagprovidedgraph_SpEd <- read_rds("diagprovidedgraph_SpEd.rds")


#B Defining user interface, including navigation bar, tabs, titles, user inputs and server reference for visualization. 

#B Defining page in Navbar layout and yeti theme

ui <- navbarPage("Making the Case for Investing in School-Based Mental Health Services", 
                 theme = shinytheme("superhero"),
                      
  #B1 Define the first tab panel about the background of the survey respondents
  
                 tabPanel("Surveying The Survey",
                      titlePanel("The 2016 School Survey of Crime and Safety"),
                      br(),
                      br(),
                               mainPanel(
                                  h4("The School Survey of Crime and Safety is the primary source of school-level data on crime and safety for the U.S. Department of Education's National Center for Education Statistics."),
                                   br(),
                                  plotOutput("schooltypes"),
                                   br(),
                                   h4("Explanation of Graph Here"),
                                   br(),
                                   plotOutput("school_characteristics_vis"),
                                   br(),
                                   h4("Explanation of Graph Here"),
                                   br()
                               )
                      ),
  
  #B2 Define the second tab panel about Diagnostic Assessments of Mental Health
  
                  tabPanel("Diagnostic Assessments of Mental Health",
                           titlePanel("Are Schools Getting a Sense of What Students Need?"),
                           br(),
                           br(),
                                
                                mainPanel(
                                  h4("How are Diagnostics Conducted?"),
                                  br(),
                                  plotOutput("diag_conducted_by"),
                                  br(),
                                  br(),
                                  h4("Do the Characteristics of a School Population Impact the Kinds of Diagnostics Provided?"),
                                  br(),
                                  h5("Population of Students Considered English Language Learners"),
                                  br(),
                                  plotOutput("diagprovidedgraph_ELL"),
                                  br(),
                                  h5("Population of Students Considered College-Bound"),
                                  br(),
                                  plotOutput("diagprovidedgraph_cb"),
                                  br(),
                                  h5("Population of Students Within Special Education"),
                                  br(),
                                  plotOutput("diagprovidedgraph_SpEd"),
                                  br(),
                                  h5("Population of Students in the Bottom 15th Percentile Academically"),
                                  br(),
                                  plotOutput("diagprovidedgraph_bfif"),
                                  br(),
                                  h5("Population of Students Who Consider Academic Achievement Important"),
                                  br(),
                                  plotOutput("diagprovidedgraph_sm"),
                                  br(),
                                  htmlOutput("access_to_mhp_reg")
                                )
  
                  ),
          
  #B3 What is the relationship between diagnostics and treatment? 
              
                tabPanel("The Relationship Between Diagnosis and Treatment",
                         titlePanel("Does Seeing the Problem Help Fix It?"),
                         br(),
                         br(),
                              mainPanel(
                                h4("The Correlation Between Mode of Diagnostic and Mode of Treatment"),
                                br(),
                                plotOutput("diagcorrelogram"),
                                br(),
                                h5("The visualization of this correlation matrix indicates that the mode of treatment follows the mode of diagnostic.")
                              )
                ),
                        

  #B4 About Section
            
                tabPanel("About",
                         titlePanel("A Call to Action"),
                         br(),
                         br(),
                              mainPanel(
                                h4("Violence and intergenerational poverty continue to plague America’s youth underserved youth of color, impacting their academic achievement, socioemotional development, and general well-being. Major urban areas like the City of Chicago present a particularly dire picture of this reality, where an estimated 81% of public school students have experienced at least one Adverse Childhood Experience (ACE) before the age of 18. The more ACEs a child is exposed to, the greater her risk for trauma. The link between the incidence of trauma and the educational opportunity gap is well-researched and documented. Untreated trauma not only impairs students’ ability to succeed academically, but has long-term negative impacts on brain development, emotional well-being, and behavioral stability. It is also a gateway for a host of debilitating mental health disorders in adulthood, with long-term impacts at the community level. The need for school-based mental health services is loud and clear-- but are schools themselves responding to this call for action? This project seeks to explore the availability and effectiveness of models of mental health delivery that are integrated into a variety of school systems."),
                                br(),
                                br(),
                                h3("Research Questions"),
                                br(),
                                h4("1. What are the most effective methods and models to capture student need for services?"),
                                br(),
                                h4("2. What schools characteristics are most conducive to providing diagnostic services?"),
                                br(),
                                h4("3. Does an expanded menu of diagnostic assessment translate to the greater availability of services?"),
                                br(),
                                br(),
                                h3("Researcher"),
                                br(),
                                h4("Prachi Naik is former middle school educator pursuing a Master's of Public Policy and Master's of Business Administration dual degree at the Harvard Kennedy School and University of Chicago Booth School of Business. She is passionate about creating equitable educational outcomes for students from vulnerable communities.")
                              )
                         ))
            

#C Define server logic that takes the data read in under A and builds the desired visualization to be interacted with and referred to in B
  
server <- function(input, output) {
  
  # C1.a Visualizing the types of schools in the survey
  
      output$schooltypes  <- renderPlot({

        schooltypes <- school_characteristics %>% 
          ggplot(aes(x = school_type)) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) +
          labs(title ="Initial Breakdown of School Types Represented in the Survey", 
               caption = "Data from the Nationa; Center on Education Statistics, 2019",
               x = "Type of School",
               y = "Percentage of Sample") +
          scale_y_continuous(labels = scales::percent) +
          coord_flip()
        
        schooltypes
        })
                   
  # C1.b Visualizing School Characteristics 
      
      output$school_characteristics_vis <- renderPlot({
        
        school_characteristics_vis <- school_characteristics %>% 
          ggplot(aes(factor(x = school_pop, level = c("small", "medium", "large", "very large")))) + 
          geom_bar(aes(fill = school_type)) + 
          labs(title ="Characteristics of Schools Represented in the 2016 School Survey on Crime and Safety", 
               subtitle = "Small: <300 | Medium: 300-499 | Large: 500-999 | Very Large: 1000+", 
               caption = "Data from the Nationa; Center on Education Statistics, 2019",
               x = "School Size",
               y = "Number of Schools") +
          scale_fill_discrete(name = "Type of School") +
          theme(plot.caption = element_text(hjust = 0, face = "italic"))
        
        school_characteristics_vis
        
      })
      
  # C2.a How are diags conducted?
      
      output$diag_conducted_by <- renderPlot({
        
        diag_conducted_by <- ggplot(data = school_mental_health) + 
          geom_bar(aes(x = school_type, fill = diag_location))
        
        diag_conducted_by
        
      })
      
      
  # C2.b GGridges: English Language Learners
      
      output$diagprovidedgraph_ELL <- renderPlot({
        
        diagprovidedgraph_ELL <- diagprofile2 %>% 
          ggplot(aes(x= ELL, y = diag_location)) + geom_density_ridges()
        
        diagprovidedgraph_ELL
        
      })
      
  # C2.c GGRidges: College-Bound
      
      output$diagprovidedgraph_cb <- renderPlot({
        
        diagprovidedgraph_cb <- diagprofile2 %>% 
          ggplot(aes(x= collegebound, y = diag_location)) + geom_density_ridges()
        
        diagprovidedgraph_cb
        
      })
      
  # C2.d GGRidges: Special Education
      
      output$diagprovidedgraph_SpEd <- renderPlot({
        
        diagprovidedgraph_SpEd <- diagprofile2 %>% 
          ggplot(aes(x= SpEd, y = diag_location)) + geom_density_ridges()
        
        diagprovidedgraph_SpEd
        
      })
      
  # C2.e GGRidges: Bottom 15
      
      output$diagprovidedgraph_bfif <- renderPlot({
        
        diagprovidedgraph_bfif <- diagprofile2 %>% 
          ggplot(aes(x= bottomfifteen, y = diag_location)) + geom_density_ridges()
        
        diagprovidedgraph_bfif
        
      })
      
  #C2.f GGRidges: School Matters
      
      output$diagprovidedgraph_sm <- renderPlot({
        
        diagprovidedgraph_sm <- diagprofile2 %>% 
          ggplot(aes(x= schoolmatters, y = diag_location)) + geom_density_ridges()
        
        diagprovidedgraph_sm
        
      })
      
  #C3.a Correlogram of Diag and Treatment
      
      output$diagcorrelogram <- renderPlot({
        
        diagmatrix <- cor(school_diag_outcome)
        round(diagmatrix, 2)
        
        colnames(diagmatrix) <- c("Diagnostic by School Employee", "Diagnostic by Nonemployee at School", "Diagnostic Outside School", "Treatment by School Employee", "Treatment by Nonemployee at School", "Treatment Outside School") 
        
        rownames(diagmatrix)<- c("Diagnostic by School Employee", "Diagnostic by Nonemployee at School", "Diagnostic Outside School", "Treatment by School Employee", "Treatment by Nonemployee at School", "Treatment Outside School")
        
        diagcorrelogram <- corrplot(diagmatrix, method = "color") 
        
        diagcorrelogram
        
      })
      
  # Regression tab deal with formatting later
      getPage <- function() {
        return(includeHTML("access_to_mhp_reg.html"))
      }
      
      output$access_to_mhp_reg <- renderUI({
        
        getPage()})
      
}
      
      
shinyApp(ui = ui, server = server)
