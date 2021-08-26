
############################
#GOOGLE SEARCH ANALYSIS
############################

# PROBLEM YOU CAN ONLY HAVE 5 SEARCH TERMS AND THEY STANDARDISE THE Y-AXIS AMONGST THE 5, SO CANT EVEN RUN MULTIPLE
# SEARCH TERMS AND MERGE TOGETHER


library(gtrendsR)
library(tidyverse)

search_terms <- c("Jack Steele")
search_terms2 <- c("Seb Ross")

output_results <- gtrends(keyword = search_terms,
        geo = "AU",
        time = "today 12-m") 
output_results2 <- gtrends(keyword = search_terms2,
                          geo = "AU",
                          time = "today 12-m") 


output_results %>% summary()


t <- output_results %>% 
  .$interest_over_time %>%
  glimpse()



plot <- output_results %>%
  .$interest_over_time %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line(colour = "darkblue", size = 1.5) +
  facet_wrap(~keyword, ncol = 1)



plot2 <- output_results2 %>%
  .$interest_over_time %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line(colour = "darkblue", size = 1.5) +
  facet_wrap(~keyword, ncol = 1)
