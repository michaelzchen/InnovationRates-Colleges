#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram

ui <- navbarPage(theme = shinytheme("superhero"),
                 "Innovation Rates and College Characteristics",
                 tabPanel("Mapping Characteristics",
                          # Application title
                          titlePanel("Map of United States"),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                              plotOutput("plot3")
                          ),
                 ),
                 tabPanel("Linear Relationships",
                          fluidPage(
                              titlePanel("Model Title"),
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput(
                                          "plot_type",
                                          "Plot Type",
                                          c("Option A" = "a", "Option B" = "b")
                                      )),
                                  mainPanel(plotOutput("line_plot")))
                          )),
                 tabPanel("Regression",
                          h1("Selectiveness and Inventiveness"),
                          fixedRow(
                              column(4,
                                     p("I examined a ")),
                              column(5, 
                                     plotOutput("plot1", height = "100%"))
                          ),
                          br(),
                          h1("Regression"),
                          fixedRow(
                              column(4,
                                     p("The graphic shows the regression of three sets of data: the violent crime
                            data from San Francisco, the violent crime data from Oakland, and the number of gun
                            control laws in California. The violent crime data is the same that is shown in the
                            first grpahic and the data on gun control laws is from "),
                              column(5, 
                                     plotOutput("plot2", height = "100%")))
                 )),
                 tabPanel("Discussion",
                          h2("Modeling"),
                          p("For my graphics, I chose to include several types: one showing trends between inventor
                 rates and various college characteristics (there is a dropdown menu in the Modeling panel 
                 that allows someone to choose which characteristic they want to examine - example characteristics
                 for people to choose from include admission rates, average faculty salary, undergraduate
                 population size, average SAT score, and many more), one showing the regression of admissions
                 rates and inventor/patent rates, and one that shows datapoints of colleges around the US 
                 that are covered in my dataset"),
                          h2("Map of Colleges"),
                          p("In these graphics, I display a map of the US, with different regions highlighted (filled) depending
                   on various characteristics - for example, total patents, inventor rates, etc. The goal is so
                   that prospective students can see various college characteristics by region. As expected, areas 
                   near Silicon Valley and Boston are higher than the average in terms of total patents and 
                   inventor rates, but the maps also depict several surprising results as well in terms of what
                   regions are hotspots for inventiveness."),
                          h2("Relationships between College Characteristics and Patent Rates"),
                          p("These graphics depict any linear relatinoships (if applicable) between different college characteristics
                   and patent rates, as calculated by Opportunity Insights. An individual is defined as an inventor if he or 
                   she is listed on a patent application between 2001 and 2012 or grant between 1996 and 2014 
                   (see Section II.B of the paper - “Mobility Report Cards: The Role of Colleges in Intergenerational Mobility. 
                   The vast majority of characteristics seem to not really have any statistially significant correlations with
                   the share of inventors per college, although some characterisics do display rather counter-intuitive results."),
                          h2("Regression"),
                          p("Given the above, I now seek to actually test if there are causal relationships (using linear models and 
                 logistic regressions) between specific college characteristics that I hypothesize could actually affect the
                   share of inventors that come out of a college. These results will give colleges insight into what
                   areas they should focus research on to improve the innovation and desire to build within their students."),
                          h2("Conclusion"),
                          p("Discussion of Final Results")),
                 tabPanel("About", 
                          titlePanel("About This Project"),
                          h3("Project Background and Motivations"),
                          p("The goal of this project is to conduct a data-driven analysis on 
                   the relationship between innovation rates by college (obtained from 
                   Raj Chetty's Opportunity Insights) and institution level data obtained 
                   from College Scorecard. With so much scrutiny going into whether college 
                   is really worth it or not, the key factor that most researchers have 
                   been examining is the earnings of students right out of college - this 
                   is the most common barometer of success. However, having read Andrew Yang's
                   book 'Smart People Should Build Things', my main concern with the 'outcome'
                   of college is whether people are creating something of value - I define value
                   as higher innovation rates (which is measured through number of patents registered).
                   Throughout the project, I will run regressions on various data for innovation 
                   rates (total number of patents granted to sstudents, share of inventors among 
                   students, total number of patent citations obtained by students, etc.) and 
                   various college characteristics (percentage of high income students, average 
                   SAT/ACT score, percentage of degrees awarded in various fields)."),
                          h3("Dataset 1 Used"),
                          p("The first data source comes from Raj Chetty's Opportunity Insights data, which harnesses 
                   the power of big data to create scalable policy solutions to restore the American Dream. 
                   In the data source, the table presents estimates of students' patent rates by the college
                   they attended, where college is what place each child attends as the institution the child
                   attended for the greatest amount of time during the 4 calendar years in which the child 
                   turned 19-22. The most important variable is called 'inventor' and represents the share of
                   inventors among students at each college. For more information about the data source and 
                   other data on upward mobility, race and economic opportunity in the US, impact of neighborhoods,
                   and impacts of teacher, please visit this", 
                            a("link.", href = "https://opportunityinsights.org/data/")),
                          h3("Dataset 2 Used"),
                          p("The other dataset I use is from the College Scorecard project - this is designed to increase 
                   transparency and provide data to help students and families compare college costs and outcomes 
                   as they weigh the tradeoffs of different colleges. The data I choose to focus on are data files
                   with data about institutions as a whole and include variables that map each college's name, 
                   location, degree type, instituion revenue, academic areas offered, admissions rates, 
                   SAT/ACT scores, and much more. The goal is to compare this dataset with innovation rates 
                   measured at each college and see if there are correlations between characteristics of each 
                   college and innovation rates. For more information about the College Scorecard project and 
                   its data sources, please visit this",
                            a("link.", href = "https://collegescorecard.ed.gov/data/")),
                          h3("About Me"),
                          p("My name is Michael Chen and I'm a current sophomore at Harvard studying Applied Math and 
                 Economics with a secondary in Government. As a co-founder of a biotech startup, I'm quite
                 intrigued by the innovation rates at colleges and how to increase this 'builder'
                 mentality among our students. My Github repo for this project can be accessed",
                            a("here.", href = "https://github.com/michaelzchen/finalproject.git"),
                            "You can reach me at: ",
                            a("chen_michael@college.harvard.edu", href = "chen_michael@college.harvard.edu"),
                            "or on", 
                            a("LinkedIn.", href = "https://www.linkedin.com/in/michael-c-134086135/"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Send a pre-rendered image, and don't delete the image after sending it
    output$plot1 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        filename <- normalizePath(file.path('Reg_Adm_Rates_Inventors.png'))
        
        # Return a list containing the filename and alt text
        list(src = filename, width = 700, length = 800)
        
    }, deleteFile = FALSE)
    
    output$plot2 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        filename <- normalizePath(file.path('Reg_Adm_Rates_Inventors.png'))
        
        # Return a list containing the filename and alt text
        list(src = filename, width = 700, length = 800)
        
    }, deleteFile = FALSE)
    
    output$plot3 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        filename <- normalizePath(file.path('MapsOriginal.png'))
        
        # Return a list containing the filename and alt text
        list(src = filename, width = 700, length = 800)
        
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)