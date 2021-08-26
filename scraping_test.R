library(rvest)


# Link to site
simple <- read_html('https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/2021/20210121.html')

str(simple)

popNodes <- html_nodes(simple, ".col-12 col-md-6 mx-md-0")
popNodes


pop <- html_table(popNodes, header = TRUE, fill = TRUE)[[1]]
popNodes
tail(pop)





# Locate variables

scraped_nodes <- html_nodes(simple,'.col-md-11')

  html_text()

  
  Hockey_table <- function(htmlObject) { 
    
    titlerow <- htmlObject %>% html_nodes(xpath = '//div[@class="stats_pullout"]/div/div/h4') %>%  html_text('data-tip')
    firstrow <- htmlObject %>% html_nodes(xpath = '//div[@class="stats_pullout"]/div/div/p[1]') %>% html_text()
    secondrow <- htmlObject %>% html_nodes(xpath = '//div[@class="stats_pullout"]/div/div/p[2]') %>% html_text()
    
    data.frame(titlerow, firstrow, secondrow)  
  