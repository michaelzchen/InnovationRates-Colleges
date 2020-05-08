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
library(readr)
library(janitor)
library(tidyverse)
library(stringr)
library(gt)
library(moderndive)
library(broom)
library(htmltools)
library(vembedr)
library(shiny)

# Read in the data and remove all data that can't be plotted on the x axis

collegePatents <- read_csv("collegePatents.csv", na = c("NULL"), col_types = cols(
  sat_avg = col_integer(), avgfacsal = col_integer(), debt_n = col_integer(),
  age_entry = col_double(), tuititionfee_prog = col_double(),
  median_hh_inc = col_double(), first_gen = col_double(), md_faminc = col_double()
)) %>% rename(
  "Average SAT Score" = "sat_avg", "Undergraduate Size" = "ugds",
  "Average Faculty Salary" = "avgfacsal", "Median Number of Students in Debt" = "debt_n",
  "Average Age of Entry" = "age_entry",
  "Tuition Costs" = "tuitionfee_prog",
  "Median Household Income" = "median_hh_inc",
  "Median Family Income" = "md_faminc"
)

# Define UI for application that draws a histogram

ui <- navbarPage(
  theme = shinytheme("superhero"),
  "Innovation Rates and College Characteristics",
  tabPanel(
    "Linear Relationships",
    fluidPage(
      titlePanel("Correlations"),
      h2("Discover Relationships Between Key College Characteristics and Different Proxies
                                 for Inventiveness"),
      br(),
      sidebarLayout(
        sidebarPanel(
          h2("Correlations Against Share of Inventors"),
          column(
            10,
            p("We can now run linear regressions and see the relationships
                                          between various important college characteristics and the share of 
                                          inventors in the undergrad population, as measured by patent data obtained from Opportunity
                                          Insights.")
          ),
          selectInput(
            "variable",
            "College Characteristic",
            choices = c(
              "Average SAT Score" = "Average SAT Score",
              "Undergraduate Size" = "Undergraduate Size",
              "Average Faculty Salary" = "Average Faculty Salary",
              "Median Number of Students in Debt" = "Median Number of Students in Debt",
              "Average Age of Entry" = "Average Age of Entry",
              "Tuition Costs" = "Tuition Costs",
              "Median Household Income" = "Median Household Income",
              "Median Family Income" = "Median Family Income"
            )
          )
        ),
        mainPanel(
          plotOutput("inventorschart"),
          column(4, align = "center"),
        )
      ),
      br(),
      sidebarLayout(
        sidebarPanel(
          h2("Correlations Against Total Patents"),
          column(
            10,
            p("We can now run linear regressions and see the relationships
                                          between various important college characteristics and the total patents
                                          created, as measured by patent data obtained from Opportunity
                                          Insights.")
          ),
          selectInput(
            "variable1",
            "College Characteristic",
            list(
              "Average SAT Score" = "Average SAT Score",
              "Undergraduate Size" = "Undergraduate Size",
              "Average Faculty Salary" = "Average Faculty Salary",
              "Median Number of Students in Debt" = "Median Number of Students in Debt",
              "Average Age of Entry" = "Average Age of Entry",
              "Tuition Costs" = "Tuition Costs",
              "Median Household Income" = "Median Household Income",
              "Median Family Income" = "Median Family Income"
            )
          )
        ),
        mainPanel(
          plotOutput("patentschart"),
          column(4, align = "center")
        )
      ),
      br(),
      sidebarLayout(
        sidebarPanel(
          h2("Correlations Against Total Citations"),
          column(
            10,
            p("We can now run linear regressions and see the relationships
                                          between various important college characteristics and the total citations
                                          received, as measured by patent data obtained from Opportunity
                                          Insights.")
          ),
          selectInput(
            "variable2",
            "College Characteristic",
            list(
              "Average SAT Score" = "Average SAT Score",
              "Undergraduate Size" = "Undergraduate Size",
              "Average Faculty Salary" = "Average Faculty Salary",
              "Median Number of Students in Debt" = "Median Number of Students in Debt",
              "Average Age of Entry" = "Average Age of Entry",
              "Tuition Costs" = "Tuition Costs",
              "Median Household Income" = "Median Household Income",
              "Median Family Income" = "Median Family Income"
            )
          )
        ),
        mainPanel(
          plotOutput("citeschart"),
          column(4, align = "center")
        )
      )
    )
  ),
  tabPanel(
    "A Closer Look/Modeling",
    h1("Selectiveness and Inventiveness"),
    fixedRow(
      column(
        4,
        p("The regression on the right shows that, quite intuitively,
                                       the less selective a college is (marked by an increase in admissions
                                       rate), the fewer share of inventors it has among its undergraduate population.
                                       However, there could be many confounding factors that could influence
                                       the results of this analysis. Additionally, it is important to note that this
                                       graph could also demonstrate a counterintuitive result - one could have the
                                       perspective that more selective colleges lead to students who are less
                                       willing to fail, and thus less inventiveness. The linear model demonstrated
                                       in the table shows")
      ),
      column(
        5,
        plotOutput("plot1", height = "100%"),
        tableOutput("model_table_1"),
        tableOutput("model_table_1_2")
      )
    ),
    br(),
    h1("Share of 1st Gen Students and Inventiveness"),
    fixedRow(
      column(
        4,
        p("The regression on the right shows that as the share of first gen students
                                     increases, the share of inventors slightly decreases, although the relationship
                                     is almost 0. This is quite intuitive, as there are no studies showing that 1st Gen 
                                     students show significantly less aptitude than others in inventiveness. Like before, 
                                     however, we aren't sure if there are confounding factors that could
                                       have affected this relationship.")
      ),
      column(
        5,
        plotOutput("plot2", height = "100%"),
        tableOutput("model_table_2"),
        tableOutput("model_table_2_2")
      )
    ),
    br(),
    h1("Percent of Full-Time Faculty and Inventiveness"),
    fixedRow(
      column(
        4,
        p("This maps (in 3 colors), different SAT midpoint scores in reading, writing, and math.
                                       The goal was to identify whether there was a difference in strength in subject area on 
                                       inventors. As is quite evident in the graph, there is too much noise for us to make any
                                       conclusive judgments.")
      ),
      column(
        5,
        plotOutput("plot3", height = "100%"),
        tableOutput("model_table_3"),
        tableOutput("model_table_3_2")
      )
    )
  ),
  tabPanel(
    "Race, Gender, and Tests Scores",
    h1("Does Race Affect Innovation Rates?"),
    fixedRow(
      column(
        4,
        p("The regression on the right shows that, quite intuitively,
                                       the less selective a college is (marked by an increase in admissions
                                       rate), the fewer share of inventors it has among its undergraduate population.
                                       However, there could be many confounding factors that could influence
                                       the results of this analysis.")
      ),
      column(
        5,
        plotOutput("plot4", height = "100%")
      )
    ),
    br(),
    h1("Does Gender Affect Innovation Rates?"),
    fixedRow(
      column(
        4,
        p("The regression on the right shows that quite clearly, as faculty salary
                                       increases, the share of inventors increases as well - this is also quite intuitive.
                                       Like before, however, we aren't sure if there are confounding factors that could
                                       have affected this relationship.")
      ),
      column(
        5,
        plotOutput("plot5", height = "100%")
      )
    ),
    br(),
    h1("Do SAT Test Scores Affect Innovation Rates?"),
    fixedRow(
      column(
        4,
        p("This maps (in 3 colors), different SAT midpoint scores in reading, writing, and math.
                                       The goal was to identify whether there was a difference in strength in subject area on 
                                       inventors. As is quite evident in the graph, there is too much noise for us to make any
                                       conclusive judgments.")
      ),
      column(
        5,
        plotOutput("plot6", height = "100%")
      )
    ),
    br(),
    h1("Do ACT Test Scores Affect Innovation Rates?"),
    fixedRow(
      column(
        4,
        p("This maps (in 3 colors), different SAT midpoint scores in reading, writing, and math.
                                       The goal was to identify whether there was a difference in strength in subject area on 
                                       inventors. As is quite evident in the graph, there is too much noise for us to make any
                                       conclusive judgments.")
      ),
      column(
        5,
        plotOutput("plot7", height = "100%")
      )
    )
  ),
  tabPanel(
    "Discussion",
    h2("Relationships between College Characteristics and Patent Rates"),
    p("These graphics depict any linear relatinoships (if applicable) between different college characteristics
                   and patent rates, as calculated by Opportunity Insights. An individual is defined as an inventor if he or 
                   she is listed on a patent application between 2001 and 2012 or grant between 1996 and 2014 
                   (see Section II.B of the paper - “Mobility Report Cards: The Role of Colleges in Intergenerational Mobility. 
                   The vast majority of characteristics seem to not really have any statistially significant correlations with
                   the share of inventors per college, although some characterisics do display rather counter-intuitive results."),
    h2("A Closer Look/Modeling"),
    p("For my graphics, I chose to include several types: one showing trends between inventor
                 rates and various college characteristics (there is a dropdown menu in the Modeling panel 
                 that allows someone to choose which characteristic they want to examine - example characteristics
                 for people to choose from include admission rates, average faculty salary, undergraduate
                 population size, average SAT score, and many more), one showing the regression of admissions
                 rates and inventor/patent rates, and one that shows datapoints of colleges around the US 
                 that are covered in my dataset"),
    h2("Regression"),
    p("Given the above, I now seek to actually test if there are causal relationships (using linear models and 
                 logistic regressions) between specific college characteristics that I hypothesize could actually affect the
                   share of inventors that come out of a college. These results will give colleges insight into what
                   areas they should focus research on to improve the innovation and desire to build within their students."),
    h2("Conclusion"),
    p("Discussion of Final Results")
  ),
  tabPanel(
    "About",
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
    p(
      "The first data source comes from Raj Chetty's Opportunity Insights data, which harnesses 
                   the power of big data to create scalable policy solutions to restore the American Dream. 
                   In the data source, the table presents estimates of students' patent rates by the college
                   they attended, where college is what place each child attends as the institution the child
                   attended for the greatest amount of time during the 4 calendar years in which the child 
                   turned 19-22. The most important variable is called 'inventor' and represents the share of
                   inventors among students at each college. For more information about the data source and 
                   other data on upward mobility, race and economic opportunity in the US, impact of neighborhoods,
                   and impacts of teacher, please visit this",
      a("link.", href = "https://opportunityinsights.org/data/")
    ),
    h3("Dataset 2 Used"),
    p(
      "The other dataset I use is from the College Scorecard project - this is designed to increase 
                   transparency and provide data to help students and families compare college costs and outcomes 
                   as they weigh the tradeoffs of different colleges. The data I choose to focus on are data files
                   with data about institutions as a whole and include variables that map each college's name, 
                   location, degree type, instituion revenue, academic areas offered, admissions rates, 
                   SAT/ACT scores, and much more. The goal is to compare this dataset with innovation rates 
                   measured at each college and see if there are correlations between characteristics of each 
                   college and innovation rates. For more information about the College Scorecard project and 
                   its data sources, please visit this",
      a("link.", href = "https://collegescorecard.ed.gov/data/")
    ),
    h3("About Me"),
    p(
      "My name is Michael Chen and I'm a current sophomore at Harvard studying Applied Math and 
                 Economics with a secondary in Government. As a co-founder of a biotech startup, I'm quite
                 intrigued by the innovation rates at colleges and how to increase this 'builder'
                 mentality among our students. My Github repo for this project can be accessed",
      a("here.", href = "https://github.com/michaelzchen/finalproject.git"),
      "You can reach me at: ",
      a("chen_michael@college.harvard.edu", href = "chen_michael@college.harvard.edu"),
      "or on",
      a("LinkedIn.", href = "https://www.linkedin.com/in/michael-c-134086135/")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Send a pre-rendered image, and don't delete the image after sending it
  output$plot1 <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path("Reg_Adm_Rates_Inventors.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  model_1 <- lm(inventor ~ adm_rate, data = collegePatents)
  model_table_1 <- get_regression_table(model_1)
  model_1_glance <- as.data.frame(glance(model_1))

  # render the linear regression table

  output$model_table_1 <- renderTable(model_table_1)

  # render the r^2 table

  output$model_table_1_2 <- renderTable(model_1_glance)

  output$plot2 <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path("Reg_1st_Gen_Inventors.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  model_2 <- lm(inventor ~ first_gen, data = collegePatents)
  model_table_2 <- get_regression_table(model_2)
  model_2_glance <- as.data.frame(glance(model_2))

  # render the linear regression table

  output$model_table_2 <- renderTable(model_table_2)

  # render the r^2 table

  output$model_table_2_2 <- renderTable(model_2_glance)

  output$plot3 <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path("Reg_Perc_Faculty_Inventors.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # create regression table for the 3rd graph

  model_3 <- lm(inventor ~ pftfac, data = collegePatents)
  model_table_3 <- get_regression_table(model_3)
  model_3_glance <- as.data.frame(glance(model_3))

  # render the linear regression table

  output$model_table_3 <- renderTable(model_table_3)

  # render the r^2 table

  output$model_table_3_2 <- renderTable(model_3_glance)

  output$plot4 <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path("Races.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  output$plot5 <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path("Gender.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  output$plot6 <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path("SATScores.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  output$plot7 <- renderImage(
    {
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path("ACTScores.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  output$inventorschart <- renderPlot({
    # create the graph
    ggplot(collegePatents %>% drop_na(input$variable), aes(x = get(input$variable), y = inventor)) +
      geom_jitter(color = "blue") +
      labs(title = paste0("Relationship Between ", input$variable, " and Percent of Inventors"), x = input$variable, y = "Percentage of Students That Are Inventors") +
      scale_y_continuous(labels = scales::percent) +
      geom_smooth(method = "lm", se = TRUE, color = "black")
  })

  output$patentschart <- renderPlot({

    # create the graph
    ggplot(collegePatents %>% drop_na(input$variable), aes(x = get(input$variable1), y = total_patents)) +
      geom_jitter() +
      labs(title = paste0("Relationship Between ", input$variable1, " and Total Patents"), x = input$variable1, y = "Total Patents") +
      geom_smooth(method = "lm", se = TRUE, color = "black")
  })

  output$citeschart <- renderPlot({

    # create the graph
    ggplot(collegePatents %>% drop_na(input$variable), aes(x = get(input$variable2), y = total_cites)) +
      geom_jitter(color = "red") +
      labs(title = paste0("Relationship Between ", input$variable2, " and Total Citations"), x = input$variable2, y = "Total Citations") +
      geom_smooth(method = "lm", se = TRUE, color = "black")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
