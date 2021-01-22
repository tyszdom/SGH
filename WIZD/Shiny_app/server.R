### TOP TEAMS IN GAME

shinyServer(
  
  
  function(input, output, session){
    
    output$myPlot <- renderPlot({
      
      
      ### TEAM AGE
      
      distNo <- input$Number
      
      top_overall_clubs <- fifa_data %>%
        group_by(Club) %>%
        summarise(AverageRating = mean(Overall, na.rm = T)) %>%
        arrange(desc(AverageRating)) %>%
        head(n = distNo) %>% pull(Club) 
      
      fifa_data %>%
        filter(Club %in% top_overall_clubs) %>%
        mutate(Top1 = ifelse(Club %in% c("Juventus"), "Yes", "No")) %>%
        ggplot(aes(x= reorder(Club,Overall), y= Overall, fill = Top1)) +
        geom_boxplot(color = "black") +
        scale_fill_manual(values = c("lightgrey", "blue")) +
        ggtitle("SREDNIE RATINGI NAJMOCNIEJSZYCH KLUBOW \nMALEJACO") +
        coord_flip() +
        theme_fivethirtyeight() +
        theme(legend.position = "none")
        
      
    }
    
    )
    
  }  
)