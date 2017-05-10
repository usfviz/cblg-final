source('trees.R')

shinyUI(
  fluidPage(
    fluidRow(
      column(4,headerPanel(HTML("<b><font color = '#36802D'> A Tree Grows in San Francisco </font></b>")) )  ,
      column(8, sliderInput("time", "Plant Date", min =min(data.in$year), max = max(data.in$year), value=min(data.in$year),
                                     step=1, animate=animationOptions(interval = 1000, playButton=img(src='treeplay.png'))))
            ),
    fluidRow(
      navbarPage (" ",theme = "garden.css",
                  
     # Panel 1: SF Map Animation ---------------------------------------------- #
               
     tabPanel("Map of Trees Over Time",
          tags$div(id = "year.facts", class= tags$strong()),
          leafletOutput("sf.trees"),
          
          absolutePanel(bottom = 200, left = 50, uiOutput("year.facts")),
          absolutePanel(bottom = 400, left = 50, 
                   selectInput("family", label = h4("Family Name"), 
                               choices = c('ALL', sort(unique(data.in$family))), 
                               selected = 'ALL',
                               multiple = F)
                   ),
          absolutePanel(bottom = 250, left = 50, 
                        radioButtons("care", 
                                     label   = h4("Tree Caretaker"),
                                     choices = c('All', 'Public', 'Private'), 
                                     selected = 'Public'))
          ),
  
  # Panel 2: Species Animation ---------------------------------------------- #
  tabPanel("Species Count over Time",
           sidebarPanel(
             selectInput("types", label = "Tree type", choices = all.types, multiple = TRUE, selected = list("Sycamore", "New Zealand Xmas Tree", "Brisbane Box", "Victorian Box", "Swamp Myrtle"))
           ),
           mainPanel(plotOutput("bars"))
          ),
  # Panel 3: Neighborhood Heatmap ---------------------------------------------- #
  
  tabPanel("Neighborhood Tree Density over Time",
           leafletOutput("hood.map")
           ),

  # Panel 4: Bubble by Neighborhood ----------------------------------------- # 
  tabPanel("Population, Income, and Invasiveness",
           sidebarPanel(
             selectInput("bub.x", "X axis", choices = x.axis.choices, selected = "Population density"),
             selectInput("bub.y", "Y axis", choices = y.axis.choices, selected = "Tree density"),
             selectInput("bub.size", "Size", choices = vis.choices, selected = "Income"),
             selectInput("bub.color", "Color", choices = vis.choices, selected = "Percent Invasive")
           ),
           mainPanel(
             uiOutput("ggvis_ui"),
             ggvisOutput("ggvis")
           ))
        )
)
)
)
