#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Install the necessary libraries

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

# Read in the data and ensure that all data that can be selected will be of the
# numeric type. Rename variables to align with user selection options on the UI.

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

# Define UI for the project and the various tab panels

ui <- navbarPage(

  # Use superhero shinytheme to brighten up the UI
  theme = shinytheme("superhero"),
  "Innovation Rates and College Characteristics",

  # Create tab panel that allows for user input and compare different linear
  # relationships between key college characteristics and different proxies for
  # inventiveness. Includes 3 graphs, with 8 user options for each graph.

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

          # Allow for user input

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

  # This tab is where I conduct user linear modeling techniques on 3 different
  # relationships (results displayed in a table) and graph the relationships as
  # well.

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
                                       willing to fail, and thus less inventiveness. The linear model displayed
                                       in the table shows that there is a very slight negative relationship
                                       between admissions rate and share of inventors.")
      ),
      column(
        5,

        # Plot scatterplot and output regression table

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
                                     is almost 0, especially around 40% - 50%. This is quite intuitive, as there are no studies showing that 1st Gen 
                                     students show significantly less aptitude than others in inventiveness. Like before, 
                                     however, we aren't sure if there are confounding factors that could
                                     have affected this relationship. For example, we see a positive relationship
          once the share of 1st Gen students is greater than 60%, but there are few data points to go off of. The linear model
          displayed in the table below shows a very slightly negative relationship between share of 1st Gen students and share of inventors.")
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
        p("The regression on the right shows that as the percent of full time faculty increases, there is
          a very slight positive increase in the percentage of students that are inventors. However, as
          in the previous cases, the relationship is not strong at all and in fact, hovers around
          0. The linear model displayed in the table confirms what we see visually, which is a slightly positive
          relationship, with a slope of 0.02. As in previous cases, the r squared value is quite low, showing that
          our regression line does not fit the data that well.")
      ),
      column(
        5,
        plotOutput("plot3", height = "100%"),
        tableOutput("model_table_3"),
        tableOutput("model_table_3_2")
      )
    )
  ),

  # This panel provides a detailed breakdown of the relationship between
  # different races, different genders, and different test score subject areas
  # on the share of inventors. Contains 4 plots that have multiple geom_smooth()
  # lines for ease of comparison between different races, different genders,
  # etc.

  tabPanel(
    "Race, Gender, and Tests Scores",
    h1("Does Race Affect Innovation Rates?"),
    fixedRow(
      column(
        4,
        p("A key question students (and colleges) often debate is the value of attending a diverse institution, with people
          from all different races and backgrounds. Furthermore, with the controversy of affirmative action
          and the historic inequity between different races, it is worthwhile to break down the amount of inventiveness
          displayed by different races - Whites, Hispanics, Asians, and Blacks. As we can see on the graph at the right,
          the Asian population shows an incredibly strong positive relationship between increasing numbers and share of students
          in that college who become inventors. However, the Asian percent demographic stops at 50%, unlike the other 3 - a curious result. The other 3 demographics seem to all show the exact opposite relationship, with increasing percent demographics all leading
          to ultimately 0 percent of students that become inventors. There could be many explanations for these trends, such as
          different access to resources among the different races, or a lack of data points.")
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
        p("Another oft talked about issue about colleges is the distinction between all-male and all-female institutions,
          and what the correct balance between the 2 genders is in a college. This graph on the right seems to indicate that, quite
          counterintuitively, the optimal balance of genders in an institution with regards to inventiveness is 25% female and 75% male, as indicated
          by the maximums at those respective percentages. We can also see quite clearly that all male
          and all female institutions fare quite poorly in terms of inventiveness. A 50/50 balance seems to be the inflection point.")
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
        p("Standardized test scores are another huge area of debate among college admissions and prospective
          students. One large camp believes that test scores are a poor indication of a person's aptitude and ability to succeed,
          while the other camp believes that a certain base level of intelligence is required, and standardized scores
          best capture that level in the fairest way. In the graph to the right, I compare SAT scores in different subject
          areas, including verbal, math, and writing. Intuitively, one would think that the math line (outlined in black) would 
          show a stronger positive relationship than the other 2, because inventors are often in the
          STEM fields. However, we can see here that all 3 lines basically overlap each other, indicating
          that colleges where students score anything 700 and above (out of 800) leads to a sharp increase in the number of inventors in that college.")
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
        p("As mentioned previously, standardized test scores are a huge area of debate among college admissions and prospective
          students. In the graph to the right, I compare ACT scores in different subject areas, including English, Math, and Cumulative. 
          Intuitively, one would think that the math line (outlined in black) would 
          show a stronger positive relationship than the other 2, because inventors are often in the
          STEM fields. In fact, in contrast to the SAT graph above, we do see that black, particularly after a score of 30 (out of 36),
          really shows a much stronger positive relationship with share of inventors than the other 2 areas do.")
      ),
      column(
        5,
        plotOutput("plot7", height = "100%")
      )
    )
  ),

  # Text only panel. Explains motivations behind the project, datasets used,
  # discussion of final results, and contact information.

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
                   as higher innovation rates/higher percentage of the student body becoing inventors (Chetty defines an individual as an inventor if he or she is listed on a patent application between 2001 and 2012 or grant between 1996 and 2014).
                   Throughout the project, I will run regressions on various data for innovation 
                   rates (total number of patents granted to students, share of inventors among 
                   students, total number of patent citations obtained by students, etc.) and 
                   various college characteristics (percentage of high income students, average 
                   SAT/ACT score, median family income)."),
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
    h3("Conclusion/Discussion of Results"),
    p("Having performed linear regressions and graphed multiple relationships between varying college characteristics and share of inventors at each college,
    the overarching conclusion is that no single characteristic has a statistically significant impact on the percentage of students that
    go on to become inventors. However, this preliminary analysis does show some clearly intuitive and nonintuitive results that merit further
    exploration. A key next step would be to run similar regressions, while controlling for confounding factors that might have influenced the
    results. I am hopeful that this project will help better inform prospective students on which colleges will give them the best bang for their
    buck, with bang measured as the amount of innovation and building that they will be exposed to or educated in during their 4 years."),
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

# Define server logic required to complete the project successfully

server <- function(input, output) {

  # Send a pre-rendered image, and don't delete the image after sending it

  output$plot1 <- renderImage(
    {
      filename <- normalizePath(file.path("Reg_Adm_Rates_Inventors.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # Create the first model by using the lm() function on the dataset that was
  # read in. Then, create a regression table and the r squared table output.

  model_1 <- lm(inventor ~ adm_rate, data = collegePatents)
  model_table_1 <- get_regression_table(model_1)
  model_1_glance <- as.data.frame(glance(model_1))

  # render the linear regression table

  output$model_table_1 <- renderTable(model_table_1)

  # render the r^2 table

  output$model_table_1_2 <- renderTable(model_1_glance)

  # Send a pre-rendered image, and don't delete the image after sending it

  output$plot2 <- renderImage(
    {
      filename <- normalizePath(file.path("Reg_1st_Gen_Inventors.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # Create the second model by using the lm() function on the dataset that was
  # read in. Then, create a regression table and the r squared table output.

  model_2 <- lm(inventor ~ first_gen, data = collegePatents)
  model_table_2 <- get_regression_table(model_2)
  model_2_glance <- as.data.frame(glance(model_2))

  # render the linear regression table

  output$model_table_2 <- renderTable(model_table_2)

  # render the r^2 table

  output$model_table_2_2 <- renderTable(model_2_glance)

  # Send a pre-rendered image, and don't delete the image after sending it

  output$plot3 <- renderImage(
    {
      filename <- normalizePath(file.path("Reg_Perc_Faculty_Inventors.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # Create the third model by using the lm() function on the dataset that was
  # read in. Then, create a regression table and the r squared table output.

  model_3 <- lm(inventor ~ pftfac, data = collegePatents)
  model_table_3 <- get_regression_table(model_3)
  model_3_glance <- as.data.frame(glance(model_3))

  # render the linear regression table

  output$model_table_3 <- renderTable(model_table_3)

  # render the r^2 table

  output$model_table_3_2 <- renderTable(model_3_glance)

  # Send a pre-rendered image, and don't delete the image after sending it

  output$plot4 <- renderImage(
    {
      filename <- normalizePath(file.path("Races.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # Send a pre-rendered image, and don't delete the image after sending it

  output$plot5 <- renderImage(
    {
      filename <- normalizePath(file.path("Gender.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # Send a pre-rendered image, and don't delete the image after sending it

  output$plot6 <- renderImage(
    {
      filename <- normalizePath(file.path("SATScores.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # Send a pre-rendered image, and don't delete the image after sending it

  output$plot7 <- renderImage(
    {
      filename <- normalizePath(file.path("ACTScores.png"))

      # Return a list containing the filename and alt text
      list(src = filename, width = 700, length = 800)
    },
    deleteFile = FALSE
  )

  # Loop through the potential user choices and create the appropriate
  # scatterplot, substituting in the user input for the x variable

  output$inventorschart <- renderPlot({
    # create the graph
    ggplot(collegePatents %>% drop_na(input$variable), aes(x = get(input$variable), y = inventor)) +
      geom_jitter(color = "blue") +
      labs(title = paste0("Relationship Between ", input$variable, " and Percent of Inventors"), x = input$variable, y = "Percentage of Students That Are Inventors") +
      scale_y_continuous(labels = scales::percent) +
      geom_smooth(method = "lm", se = TRUE, color = "black")
  })

  # Loop through the potential user choices and create the appropriate
  # scatterplot, substituting in the user input for the x variable

  output$patentschart <- renderPlot({

    # create the graph
    ggplot(collegePatents %>% drop_na(input$variable), aes(x = get(input$variable1), y = total_patents)) +
      geom_jitter() +
      labs(title = paste0("Relationship Between ", input$variable1, " and Total Patents"), x = input$variable1, y = "Total Patents") +
      geom_smooth(method = "lm", se = TRUE, color = "black")
  })

  # Loop through the potential user choices and create the appropriate
  # scatterplot, substituting in the user input for the x variable

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
