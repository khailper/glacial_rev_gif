library(tidyverse)
library(gganimate)
library(cowplot)
library(scales)

# defaulting to deck_size = 40 since it's for Limited
create_hits_distibution <- function(snow_card_count, deck_size = 31){
  tibble(hits = 0:6) %>% 
    mutate(prob = dhyper(x= hits, 
                         m = snow_card_count, 
                         # non-snow cards
                         n = deck_size - snow_card_count, 
                         6),
           possible_hits = snow_card_count
           )
}


hits_distibutions_play <- map_dfr(5:20, create_hits_distibution)
hits_distibutions_draw <- map_dfr(5:20, create_hits_distibution, deck_size = 30)

play_gif <- ggplot(hits_distibutions_play, aes(hits, prob)) + 
  geom_col(color = "black") +
  transition_states(possible_hits, transition_length = 1, state_length = 2) +
  theme_cowplot() +
  scale_x_continuous(breaks = pretty_breaks(n = 7)) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_y_continuous(breaks = seq(0.1, 0.5, by = 0.1)) + 
  labs(x = "Number of snow cards", 
       y = "Probability",
       title = "How many hits can you expect from Glacial Revelation?",
       subtitle = "Snow cards left in library:{next_state}",
       caption = "Assuming you play Glacial Revelation turn three on the play.")

draw_gif <- ggplot(hits_distibutions_draw, aes(hits, prob)) + 
  geom_col(color = "black") +
  transition_states(possible_hits, transition_length = 1, state_length = 2) +
  theme_cowplot() +
  scale_x_continuous(breaks = pretty_breaks(n = 7)) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_y_continuous(breaks = seq(0.1, 0.5, by = 0.1)) + 
  labs(x = "Number of snow cards", 
       y = "Probability",
       title = "How many hits can you expect from Glacial Revelation?",
       subtitle = "Snow cards left in library:{next_state}",
       caption = "Assuming you play Glacial Revelation turn three on the draw.")

anim_save("on_the_play.gif", play_gif)
anim_save("on_the_draw.gif", draw_gif)
