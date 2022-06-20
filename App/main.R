# Main script for glasto 2022 app

# Read in initial data
all_stages <- read_csv("./data/all_stages.csv") %>%
  mutate_if(is.character, utf8::utf8_encode)
pivot_time <- read_csv("./data/pivot_time.csv") %>%
  mutate_if(is.character, utf8::utf8_encode)



# Make a list of stage names for the checkboxGroupInput(id = stages_pick)
unique_stages <- unique(all_stages$stage)
stages_list <- as.list(unique_stages)
names(stages_list) <- unique_stages


# For a default choice of key stages. 
key_stages <- c("PYRAMID STAGE",
                "OTHER STAGE",
                "WEST HOLTS STAGE",
                "JOHN PEEL STAGE",
                "THE PARK STAGE",
                "ACOUSTIC STAGE",
                "AVALON STAGE",
                "ARCADIA",
                "BBC MUSIC INTRODUCING",
                "WILLIAM'S GREEN")

days <- c("WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY")
day_list <- as.list(days)
names(day_list) <- days



CompPlot <- function(df) {
  # frame of annotations. 
  labels_unpicked <- df %>%
    filter(!picked) %>%
    filter(time_position == "start_datetime")
  labels_picked <- df %>%
    filter(picked) %>%
    filter(time_position == "start_datetime")
  
  picked_df <- df %>%
    filter(picked)
  unpicked_df <- df %>%
    filter(!picked)

  # Plotting
  plot <- ggplot() +
    # Unpicked section
    geom_point(inherit.aes = F, data = unpicked_df, aes(x = datetime, y = act_order), colour = "grey20") +
    geom_line(inherit.aes = F, data = unpicked_df, aes(x = datetime, y = act_order, group = act, color = stage), alpha = 0.5, size = 5) + 
    geom_label_repel(inherit.aes = F, data = labels_unpicked, aes(x = datetime, y = act_order, label = act_name_short),
                     size = 3, alpha = 0.7, nudge_y = 1, max.overlaps = Inf) +
    geom_text(inherit.aes = F, data = unpicked_df, aes(x = datetime, y = act_order, label = format(as.POSIXct(datetime), "%H:%M")), size = 2.5, vjust = 1.5) +
    # Picked section
    geom_point(inherit.aes = F, data = picked_df, aes(x = datetime, y = act_order), colour = "red") +
    geom_line(inherit.aes = F, data = picked_df, aes(x = datetime, y = act_order, group = act), colour = "red", size = 5) +
    geom_label_repel(inherit.aes = F, data = labels_picked, aes(x = datetime, y = act_order, label = act_name_short),
                                                                size = 3, alpha = 0.7, color = "red", nudge_y = 1, max.overlaps = Inf) +
    geom_text(inherit.aes = F, data = picked_df, aes(x = datetime, y = act_order, label = format(as.POSIXct(datetime), "%H:%M")), colour = "red", size = 2.5, vjust = 1.5) +
    # All plot theme
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    facet_wrap(~stage, ncol = 1, scales = "free_y") +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour") +
    scale_y_continuous(expand = c(0,2)) +
    labs(x = "Time",
         title = unique(df$day))
  return(plot)
}

# Declare reactive values
values <- reactiveValues(indices_df = tibble(index = all_stages$index,
                                             picked = F))

