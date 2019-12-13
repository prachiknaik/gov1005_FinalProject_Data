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
library(RColorBrewer)
library(tidyverse)

# UPDATE THIS
# A. Read in relevant data objects that we copied over from the clean-data file to App

school_characteristics <- read_rds("school_characteristics.rds")
school_diag_outcome <- read_rds("school_diag_outcome.rds")
school_mental_health <- read_rds("school_mental_health.rds")
test2 <- read_rds("test2.rds")
diagprofile <- read_rds("diagprofile.rds")
popcharacteristics <- read_rds("popcharacteristics.rds")
diagprofile2 <- read_rds("diagprofile2.rds")
limitations <- read_rds("limitations.rds")
diagprofile3 <- read_rds("diagprofile3.rds")
treatmentprofile4 <- read_rds("treatmentprofile4.rds")
impact1 <- read_rds("impact1.rds")
impact2 <- read_rds("impact2.rds")

# A.2 Now read in graphs/ analytical objects

schooltypes <- read_rds("schooltypes.rds")
school_characteristics_vis <- read_rds("school_characteristics_vis.rds")
diagcorrelogram <- read_rds("diagcorrelogram.rds")
diag_conducted_by2 <- read_rds("diag_conducted_by2.rds") #NECESSARY?

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
                                   h4("Comparatively speaking, in 2016, there were 24.6% secondary schools, 68.4% primary schools, and 6.9% combined schools in the US public education system."),
                                   br(),
                                   plotOutput("school_characteristics_vis"),
                                   br(),
                                   h4("A majority of the very large schools in the sample are high schools, while a majority of small schools are primary schools. Since these types of schools have very different needs, counting each observation as equally weighted could present problems for representativeness."),
                                   br()
                               )
                      ),
  
  #B2 Define the second tab panel about Diagnostic Assessments of Mental Health: 
  
                  tabPanel("Diagnostic Assessments of Mental Health",
                           titlePanel("Are Schools Getting a Sense of What Students Need?"),
                           br(),
                           br(),
                                
                                mainPanel(
                                  h4("Do the Characteristics of a School Population Impact the Kinds of Diagnostics Provided?"),
                                  br(),
                                  h5("Population of Students Considered English Language Learners"),
                                  br(),
                                  plotOutput("diagprovidedgraph_ELLtest"),
                                  br(),
                                  plotOutput("diagprovidedgraph_ELL"),
                                  br(),
                                  h5("Population of Students Considered College-Bound"),
                                  br(),
                                  plotOutput("diagprovidedgraph_cbtest"),
                                  br(),
                                  plotOutput("diagprovidedgraph_cb"),
                                  br(),
                                  h5("Population of Students Within Special Education"),
                                  br(),
                                  plotOutput("diagprovidedgraph_SpEdtest"),
                                  br(),
                                  plotOutput("diagprovidedgraph_SpEd"),
                                  br(),
                                  h5("Population of Students in the Bottom 15th Percentile Academically"),
                                  br(),
                                  plotOutput("diagprovidedgraph_bfiftest"),
                                  br(),
                                  plotOutput("diagprovidedgraph_bfif"),
                                  br(),
                                  h5("Population of Students Who Consider Academic Achievement Important"),
                                  br(),
                                  plotOutput("diagprovidedgraph_smtest"),
                                  br(),
                                  plotOutput("diagprovidedgraph_sm"),
                                  br()
                                )
  
                  ),
  #ALTERNATIVE INTERACTION SECTION TO ABOVE
  
                  #tabPanel("Diagnostics in Light of Student Populations",
                          #titlePanel("Does the Make Up of the Student Body Drive Diagnostics?"),
                                  #br(),
                                  #br(),
                          
                               #sidebarLayout(
                                 #sidebarPanel(
                                   #CAN YOU USE THE SAME SIDE BAR FOR TWO DIFF GRAPHS?
                                   #radioButtons("popcharacteristics", label = strong("Student Population Characteristics"),
                                                #choices = unique(diagprofile3$popcharacteristics),
                                                #selected = ELL
                                   #), width = 2),
             
                                         #mainPanel(
                                           #plotOutput("diagprovidedgraph_binary"),
                                           #br(),
                                           #plotOutput("diagprovidedgraph_factor")
                                         #)
                                       #),
           
  
  #What types of Diagnostics are offered by different schools? INTERACTIVE
                  tabPanel("What Kinds of Diagnostics Are Conducted Across Various Schools?",
                                 titlePanel("A Breakdown of Different Modes of Diagnosis Across School Types"),
                                 br(),
                                 br(),
  
                        sidebarLayout(
                                sidebarPanel(
                                      selectInput("diag_type", label = strong("Types of Diagnostics:"),
                                            choices = unique(school_mental_health$diag_type),
                                            selected = "None",
                                            #multiple = TRUE
                                      ), width = 2),
                  
                                
                                mainPanel(
                                  plotOutput("diag_conducted_by2")
                                  , width = 10))),
                                
    
    
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
                                h5("The visualization of this correlation matrix indicates that the mode of treatment follows the mode of diagnostic."),
                                br()
                              )
                ),

  
  # Attempts at Impact Measurement INTERACTIVE
                #navbarMenu("Atempting Impact Measurement: Regressions and Densities",
                           #tabPanel("Regressions: A Combined Model",
                                    #htmlOutput("limitationscombined2")
                           #),
                           #tabPanel("Regressions: Limitations on Diagnostics",
                                    #sidebarLayout(
                                      #sidebarPanel(
                                        #radioButtons("limitations",label = strong("Types of Limitations"),
                                                     #choices = unique(diagprofile3$limitations),
                                                     #selected = access_to_mhp
                                        #), width = 2),
                                      
                                      # Show development
                                      
                                      #mainPanel(
                                        #plotOutput("diaglimitations_regplot")
                                        #, width = 10)
                                    #)
                           #),
                           #tabPanel("Regressions: Limitations on Treatment",
                                    #sidebarLayout(
                                      #sidebarPanel(
                                        #radioButtons("limitations",label = strong("Types of Limitations"),
                                                     #choices = unique(diagprofile3$limitations),
                                                     #selected = access_to_mhp
                                        #), width = 2),
                                      
                                      # Show development
                                      
                                      #mainPanel(
                                        #plotOutput("treatmentlimitations_regplot")
                                        #, width = 10)
                                    
                                    
                           #),
                           #tabPanel("Relating Treatment to Discipline?",
                                    #plotOutput("impact_discipline2")
                           #)
                #),
                
  
  #Attempts at Impact Measurement (STATIC)
                        navbarMenu("Atempting Impact Measurement: Regressions and Densities",
                                   tabPanel("Regressions: A Combined Model",
                                      titlePanel("Which Limitations Most Inhibit Schools' Ability to Provide Diagnosis and/or Treatment?"),
                                            htmlOutput("limitationscombined2")
                                   ),
                                   tabPanel("Regressions: Limitations on Diagnostics",
                                            plotOutput("diaglimitations_regplot")
                                   ),
                                   tabPanel("Regressions: Limitations on Treatment",
                                            plotOutput("treatmentlimitations_regplot")
                                   ),
                                   tabPanel("Relating Treatment to Discipline?",
                                            plotOutput("impact_discipline2")
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
    school_characteristics %>% 
      ggplot(aes(x = school_type)) + 
      geom_bar(aes(fill = school_type, y = (..count..)/sum(..count..))) +
      labs(title ="Initial Breakdown of School Types Represented in the Survey", 
           caption = "Data from the National Center on Education Statistics, 2019",
           x = "Type of School",
           y = "Percentage of Sample") +
      scale_y_continuous(labels = scales::percent) +coord_flip()
  })
  
  # C1.b Visualizing School Characteristics
  output$school_characteristics_vis <- renderPlot({
    school_characteristics %>% 
      ggplot(aes(factor(x = school_pop, level = c("small", "medium", "large", "very large")))) + 
      geom_bar(aes(fill = school_type)) + 
      labs(title ="Characteristics of Schools Represented in the 2016 School Survey on Crime and Safety", 
           subtitle = "Small: <300 | Medium: 300-499 | Large: 500-999 | Very Large: 1000+", 
           caption = "Data from the Nationa; Center on Education Statistics, 2019",
           x = "School Size",
           y = "Number of Schools") +
      scale_fill_discrete(name = "Type of School") +
      theme(plot.caption = element_text(hjust = 0, face = "italic"))
  })
  
  
  #TEST FOR DROPDOWN MENU SELECTION OF DIAGNOSIS CONDUCTED BY GRAPH
  # Define a reactive susbset that uses the user input in the school selector as a filter to create a data set that defines the changes in the graph 
  
  #LOOK CLOSELY AT HOW HE DEFINED THE SUBSET HERE. IS IT JUST SCHOOLS, OR IS IT SCHOOLS + PERTINENT VARIABLES
  #subset<-reactive({school_mental_health %>% filter(diag_type == input$diag_type)})
  
  # MAKE SURE YOU UNDERSTAND REACTIVITY IN SHINY
  
  output$diag_conducted_by2 <- renderPlot({
    school_mental_health <- school_mental_health %>% filter(diag_type == input$diag_type)
    
    diag_typegraph<- ggplot(data = school_mental_health) +
    geom_bar(aes(x = school_type, fill = school_type))
    
    diag_typegraph
  })
    

  
  # C2.b GGridges: English Language Learners
  output$diagprovidedgraph_ELLtest <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= ELL, y = diag, fill = diag)) + 
      geom_density_ridges(jittered_points = TRUE, 
                          position = position_points_jitter(height = 0),
                          point_shape = '|', point_size = 3, 
                          point_alpha = 1, alpha = 0.7) +
      labs(title = "A Density Distribution of Schools with Certain Percentage English Language Learners Where Some vs No Diagnostics are Provided", 
           x = "Percentage of Student Body Considered ELL", 
           y = " ") 
  })
  
  output$diagprovidedgraph_ELL <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x = ELL, y = diag_type, fill = diag_type)) + 
      geom_density_ridges() +
      labs(title = "A Density Distribution of Schools with Certain Percentage English Language Learners Based on Type of Diagnostic Provided", 
           x = "Percentage of Student Body Considered ELL", 
           y = " ") 
  })
      
  # C2.c GGRidges: Special Education
  output$diagprovidedgraph_SpEdtest <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= SpEd, y = diag, fill = diag)) + 
      geom_density_ridges(jittered_points = TRUE, 
                          position = position_points_jitter(height = 0),
                          point_shape = '|', point_size = 3, 
                          point_alpha = 1, alpha = 0.7) +
      labs(title = "A Density Distribution of Schools with Certain Percentage Special Education Learners Where Some vs No Diagnostics are Provided", 
           x = "Percentage of Student Body Considered Special Education", 
           y = " ") 
  })
  
  output$diagprovidedgraph_SpEd <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= SpEd, y = diag_type, fill = diag_type)) + geom_density_ridges() +
      labs(title = "A Density Distribution of Schools with Certain Percentage Special Education Learners Based on Type of Diagnostic Provided", 
           x = "Percentage of Student Body Considered Special Education", 
           y = " ") 
  })
      
  # C2.d GGRidges: Bottom Fifteen
  output$diagprovidedgraph_bfiftest <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= bottomfifteen, y = diag, fill = diag)) + 
      geom_density_ridges(jittered_points = TRUE, 
                          position = position_points_jitter(height = 0),
                          point_shape = '|', point_size = 3, 
                          point_alpha = 1, alpha = 0.7) +
      labs(title = "A Density Distribution of Schools with Students Performing in the Bottom 15 Percentile on National Exams Where Some vs No Diagnostics are Provided", 
           x = "Percentage of Student Body Performing in the Bottom Fifteen Percentile on National Exams", 
           y = " ")
  })
  
  output$diagprovidedgraph_bfif <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= bottomfifteen, y = diag_type, fill = diag_type)) + 
      geom_density_ridges() + 
      labs(title = "A Density Distribution of Schools with Students Performing in the Bottom 15 Percentile on National Exams Based on Type of Diagnostic Provided", 
           x = "Percentage of Student Body Performing in the Bottom Fifteen Percentile on National Exams", 
           y = " ")
  })
      
  # C2.e GGRidges: College-Bound
  output$diagprovidedgraph_cbtest <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= bottomfifteen, y = diag, fill = diag)) + 
      geom_density_ridges(jittered_points = TRUE, 
                          position = position_points_jitter(height = 0),
                          point_shape = '|', point_size = 3, 
                          point_alpha = 1, alpha = 0.7) +
      labs(title = "A Density Distribution of Schools with Students Considered to Be College Bound Where Some vs No Diagnostics are Provided", 
           x = "Percentage of Student Body Considered College Bound", 
           y = " ")
  })
  
  output$diagprovidedgraph_cb <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= collegebound, y = diag_type, fill = diag_type)) + 
      geom_density_ridges() + 
      labs(title = "A Density Distribution of Schools with Students Considered to Be College Bound Based on Type of Diagnostic Provided", 
           x = "Percentage of Student Body Considered College Bound", 
           y = " ")
  })
      
  #C2.f GGRidges: School Matters
      
  output$diagprovidedgraph_smtest <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= schoolmatters, y = diag, fill = diag)) + 
      geom_density_ridges(jittered_points = TRUE, 
                          position = position_points_jitter(height = 0),
                          point_shape = '|', point_size = 3, 
                          point_alpha = 1, alpha = 0.7) +
      labs(title = "A Density Distribution of Schools with Students Considered to be 'Invested in School' Where Some vs No Diagnostics are Provided", 
           x = "Percentage of Student Body Considered to be 'Invested in School'", 
           y = " ")
  })
      
  output$diagprovidedgraph_sm <- renderPlot({
    diagprofile2 %>% 
      ggplot(aes(x= schoolmatters, y = diag_type, fill = diag_type)) + 
      geom_density_ridges() + 
      labs(title = "A Density Distribution of Schools with Students Considered to be 'Invested in School' Based on Type of Diagnostic Provided", 
           x = "Percentage of Student Body Considered to be 'Invested in School'", 
           y = " ")
  })
  
  #C3.a Correlogram of Diag and Treatment
  output$diagcorrelogram <- renderPlot({
        diagmatrix <- cor(school_diag_outcome)
        round(diagmatrix, 2)
        colnames(diagmatrix) <- c("Diag by School Employee", "Diag by Nonemployee at School", "Diag Outside School", "Tmt by School Employee", "Tmt by Nonemployee at School", "Tmt Outside School") 
        rownames(diagmatrix)<- c("Diag by School Employee", "Diag by Nonemployee at School", "Diag Outside School", "Tmt by School Employee", "Tmt by Nonemployee at School", "Tmt Outside School")
        corrplot(diagmatrix, method = "color", tl.col="black", tl.cex=0.8, tl.srt=30) 
    })
      
  #Impact Measurement (Static)
  
  # Separate format for retreiving html
  getPage <- function() {
    return(includeHTML("limitationscombined2.html"))
  }
  output$limitationscombined2 <- renderUI({
    getPage()
    })
  
  output$diaglimitations_regplot <- renderPlot({
    diagprofile3 %>% 
      ggplot(aes(x= inadequate_access_to_mhp, y = diagprovided)) +
      geom_jitter(color = 'black',
                  alpha = 0.25) +
      geom_smooth(method=glm, color = 'blue') +
      labs(title= "How Much Do Limitations Impact Schools' Ability to Provide a Diagnostic Assessment?")
  })
  
  output$treatmentlimitations_regplot <- renderPlot({
    treatmentprofile4 %>% 
      ggplot(aes(x= inadequate_access_to_mhp, y = treatmentprovided)) +
      geom_jitter(color = 'black',
                  alpha = 0.25) +
      geom_smooth(method=glm, color = 'blue') +
      labs(title= "How Much Do Limitations Impact Schools' Ability to Provide Treatment?")
  })
  
  output$impact_discipline2 <- renderPlot({
    impact2 %>% 
      ggplot(aes(x= total_disciplinary_actions, y = ..count.., fill = treatment_type)) + 
      geom_density(alpha = 0.2) +
      scale_x_log10()+
      facet_grid(treatment_type ~ .)
  })

}

shinyApp(ui = ui, server = server)