library(ggplot2)
# library(leaflet)
library(shiny)
library(dplyr)


# functions ---------------------------------------------------------------

plotStatus <- function(spec = NA, 
                       occs, 
                       bbox = c(-180, -60, 180, 80), 
                       status = c("native", "introduced"),
                       alpha = NA,
                       title = NA){
  
  if(!is.na(spec)){
    df_plot <- dplyr::filter(occs, species == spec & status %in% !!status)
  } else {
    df_plot <- dplyr::filter(occs, status %in% !!status)
  }
  
  if(is.na(title)){
    if(!is.na(spec)){
      title <- spec
    } else {title = ""}
  }
  
  
  world <- map_data("world")
  if(nrow(df_plot) == 0){return("No matching occurrences")}
  if(is.na(alpha)){alpha <- 1/log10(nrow(df_plot))}
  
  ggplot(df_plot, aes(x = lon, y = lat, color = criterion_1)) +
    geom_map(data = world, map = world, aes(map_id = region), 
             fill = "grey80", color = "grey80", inherit.aes = FALSE) +
    geom_point(shape = 1, size = 2, alpha = alpha) +
    scale_color_manual("Biogeographic status",
                       values = c(native = "#0570b0", introduced = "#cb181d")) +
    # ggtitle(title) +
    xlim(bbox[1], bbox[3]) +
    ylim(bbox[2], bbox[4]) +
    coord_fixed() +
    theme_bw() +
    theme(legend.position = "top",
          plot.margin = unit(c(0,0,0,0), "cm")) 
}


# required data -----------------------------------------------------------


load(file = file.path("data","shiny_overview_comparison.RData"))
load(file = file.path("data","occ_status_AC.RData"))
load(file = file.path("data","spp_suitable_AC.RData"))
load(file = file.path("data","input_TA_unstand.RData"))


# ui ----------------------------------------------------------------------





# Define UI for application that draws a histogram
ui <- navbarPage("PPI Niche Comparison", id ="nav",
                 
                 tabPanel("Species Occurences",
                          
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         
                                         selectInput("species",
                                                     label = "Select a species:",
                                                     choices = as.list(spp_suitable_AC)),
                                         
                                         htmlOutput("text_spp"),
                                         
                                         radioButtons("niche_selection", "Which dynamics do you want to see?",
                                                      c("All five niche dynamics", "ESU", "original ecospat output")),
                                         
                                         
                                        #  actionButton("go", "Go!")
                                         
                                         
                            ), # end of sidebarPanel
                            
                            mainPanel(
                              column(10,
                                     tableOutput("niche_dynamics"),
                                     
                                     plotOutput("map_status")
                                     
                                     # plotOutput("map_status", 
                                     #            width = "100%", height = "90vh")
                                     
                                  
                              ) #vh = viewport height, css-unit; allows to set height relative to window size)
                            ) # end of mainPanel
                          ) # end of sidebarLayout
                 ), # end of tabPanel Species Occurrences
                 
                 # tabPanel("Trait Analysis",
                 #          
                 #          fluidPage(
                 #            
                 #            
                 # 
                 #            # actionButton("click", "Multivariate Results")
                 #            imageOutput("TA_figure", width = "100%")
                 # 
                 #          ) # end of fluidPage
                 #          ) # end of tabPanel Trait Analydis
                 
                 
) # end of navbarPage 

# server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # observeEvent(input$go, {
    
    
  
# TAB1:=======================================================================

  output$map_status <- renderPlot(
    
    plotStatus(input$species, occ_status_AC)
    
  ) # end of renderPlot
  
  output$text_spp <- renderText({
    paste("<b> Species: </b>", input$species, "<br/> 
          <b> Family: </b>", overview_comparison[overview_comparison$species == input$species, "family"][1], "<br/>
          <br/>
          <b> Traits: </b> <br/>
          Mean height (m):", overview_comparison[overview_comparison$species == input$species, "mean_height"][1],"<br/>
          Mean seedmass (g):", overview_comparison[overview_comparison$species == input$species, "mean_seedmass"][1],"<br/>
          Growth form:", overview_comparison[overview_comparison$species == input$species, "growth_form"][1],"<br/>
          Life cycle:", overview_comparison[overview_comparison$species == input$species, "lifecycle"][1],"<br/>
          <br/>")
    
  }) # end of renderText with HTML
  
  output$niche_dynamics <- renderTable(
    
    if(input$niche_selection == "All five niche dynamics") {
      
      overview_comparison %>% 
        dplyr::filter(species == input$species) %>% 
        select(region, years_since_intro, similarity, rel_abandonment, rel_unfilling, rel_stability, rel_expansion, rel_pioneering) %>% 
        rename("Region" = "region",
               "Years since introduction" = "years_since_intro",
               "Niche similarity" = "similarity",
               "Abandonment (%)" = "rel_abandonment",
               "Unfilling (%)" = "rel_unfilling",
               "Stability (%)" = "rel_stability",
               "Expansion (%)" = "rel_expansion",
               "Pioneering (%)" = "rel_pioneering") 
      
    } else {
      if ((input$niche_selection == "ESU")) {
        overview_comparison %>% 
          dplyr::filter(species == input$species) %>% 
          select(region, years_since_intro, similarity, unfilling, stability, expansion) %>% 
          rename("Region" = "region",
                 "Years since introduction" = "years_since_intro",
                 "Niche similarity" = "similarity",
                 "Unfilling (%)" = "unfilling",
                 "Stability (%)" = "stability",
                 "Expansion (%)" = "expansion") 
      } else {
        overview_comparison %>% 
          dplyr::filter(species == input$species) %>% 
          select(region, years_since_intro, similarity, orig_unfilling, orig_stability, orig_expansion) %>% 
          rename("Region" = "region",
                 "Years since introduction" = "years_since_intro",
                 "Niche similarity" = "similarity",
                 "Unfilling (%)" = "orig_unfilling",
                 "Stability (%)" = "orig_stability",
                 "Expansion (%)" = "orig_expansion") 
      }
    }
  )
  
  # output$text_spp <- renderText(input$species) # end of renderPrint

# TAB2:=======================================================================
  
  output$TA_figure <- renderImage({
    list(
      # src = file.path("plots", paste0(input$id, ".jpg")),
      src = file.path("plots", "TA_all.jpg"),
      contentType = "image/jpeg",
      width = 1600,
      height = 1000
    )}, deleteFile = FALSE)
 #  }) # end of observeEvent
  
} # end of server function


# Run the application 
shinyApp(ui = ui, server = server)
