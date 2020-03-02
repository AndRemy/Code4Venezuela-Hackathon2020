library(shinydashboard)
library(shiny)
## any plot library

shinyUI(                                                     ## the page
    dashboardPage(                                           ##the layout of the page is going to follow a dashboard layout
        skin = "yellow",
        header = dashboardHeader(                            ## defining the header (always keep it header)
            title = "Venezuela - General Public Information" ## you can other things as well
        ),
        sidebar = dashboardSidebar(     ## dashboardsidebar is the name of the control, sidebar is what you are doing
            sidebarMenu( ## collection of tabs
                menuItem( ## this are the tabs
                    "General Info",    ## this it the title
                    tabName = "general"     ## calling it an object to connect it to our body, no space
                ),
                menuItem(
                    "By Region",
                    tabName = "region"
                )
                
            ) ## closing sidebarMenu --> you only define tabs
        ), ## closing dashboardSidebat
        body = dashboardBody(
            tabItems(                 ## connect to tab, first in plural because its the collection of tabs
                tabItem(
                    tabName = 'general', ## the name of your tab from sidebar
                    fluidPage(        ## to put in the actual content 
                        mainPanel(                  ## your main content page part
                            h1("General Healthcare Information"),
                            h4(textOutput("numberHospitals")),
                            h4(textOutput("hospitalsPE")),
                            h1("Regional Healthcare Information"),
                            h4(htmlOutput("regionalInfo"))
                        )
                    )
                ),
                tabItem(
                    tabName = "region",
                    fluidPage(        ## to put in the actual content 
                        titlePanel("Information by region"),
                        sidebarPanel( ## sidebar inside the body - you can add you inputs
                            selectInput( ## this creates the control 
                                ## controls can be different types (check datacamp)
                                ## each control type will have a number of parameters you need to spcify
                                inputId  = "region",  ## each control requires a name or id
                                label    = "Select a region", ## have a text attached to guide the user
                                choices  = "--SELECT--",
                                selected = "Amazonas" ## default selection when you open the page
                            ) 
                        ),
                        mainPanel(                  ## your main content page part
                            ## the id that will connect to whatever i am rendering in my server (can be a plot, a text, an image, wtf i want)
                            h1("General Information"),
                            h4(htmlOutput("generalRegion")),
                            h1("Hospital Information"),
                            h4(htmlOutput("hospitalInfo"))
                        )
                    )
                )
            )
        ) # dahsboardBody close
    )
)
