pacman::p_load(tidyverse,dplyr,googlesheets4,ggplot2,bbplot2,scales,zoo)

#######

india_states_full_data <- read_sheet("https://docs.google.com/spreadsheets/d/1t66DYgeberDy-rFOIjXOcXRau3AzG97vHopTX18EdkU/edit?usp=sharing") %>%
  filter(Type=="Cases") %>%
  mutate(Date = seq(from=as.Date("2020-03-14",format="%Y-%m-%d"),by="1 day",along.with = Date_YMD)) %>%
  select(-Date_YMD,-Type,-India)

india_state_cases <- india_states_full_data %>%
  gather(state,new_daily_cases,1:37) %>%
  group_by(state) %>%
  mutate(rolling_mean_cases = zoo::rollmean(new_daily_cases, 7, fill = NA, align = "right"),
         cases_pc_peak = rolling_mean_cases[Date == max(Date)]/max(rolling_mean_cases, na.rm = T),
         rising_cases = rolling_mean_cases > lag(rolling_mean_cases, 7)) %>%
  ungroup() %>%
  select(state, Date, new_daily_cases,rolling_mean_cases,cases_pc_peak,rising_cases)

# plotting places with highest cases in each region looks like a good candidate
india_highest_state_cases <- india_state_cases %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(latest_rolling_mean_cases = rolling_mean_cases[Date == max(Date)]) %>%
  ungroup() %>%
  dplyr::mutate(rank = ntile(-latest_rolling_mean_cases,n=37)) %>%
  dplyr::filter(rank %in% c(1:16)) %>%
  dplyr::group_by(state) %>%
  dplyr::arrange(rank) %>%
  dplyr::mutate(state_upper_f = factor(state, unique(.$state))) %>%
  dplyr::select(Date, rank, rolling_mean_cases, state, state_upper_f) %>%
  ungroup()


india_highest_state_cases_with_label <- india_highest_state_cases %>%
  data.frame(.) %>%
  mutate(round_rolling_mean_cases =
           ifelse(rolling_mean_cases >= 1000, paste0(round(rolling_mean_cases/1000), "k"),
                  ifelse(rolling_mean_cases < 1000 & rolling_mean_cases >= 1, round(rolling_mean_cases),
                         round(rolling_mean_cases, 1))))


# Plot India state cases
states.plot <- ggplot(india_highest_state_cases_with_label) +
    geom_line(aes(Date, rolling_mean_cases),
              colour = bbplot2::bbc_pal(palette = "blue", values = 1),
              size = 1) +
    geom_point(data = india_highest_state_cases_with_label %>%
                 dplyr::filter(Date == max(Date)),
               aes(Date, rolling_mean_cases),
               colour = "#222222",
               size = 2.5) +
    geom_label(data = india_highest_state_cases_with_label %>%
                 dplyr::filter(Date == max(Date)),
               aes(Date, rolling_mean_cases, label = round_rolling_mean_cases),
               size = 5, fill = NA, hjust = 0,
               label.size = 0, nudge_x = 10, fontface = "bold") +
    facet_wrap(~ state_upper_f, scales = "free", nrow = 4) +
    reith_style() +
    all_reith_underneath() +
    scale_x_date(date_labels = "%b",
                 date_breaks = "4 month",
                 expand = expansion(c(0,0.3))) +
    scale_y_continuous(expand = expansion(c(0.1,0.1))) +
    coord_cartesian(clip = 'off') +
    theme(panel.spacing.x = unit(0.4, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(r = 0.1, l = 0.1, b = 0.5, unit = "cm"),
          strip.text = element_text(size = 18)) +
    labs(title = paste("Indian states with the highest cases"),
         subtitle = paste0("Number of cases per day from 14 Mar 2020 to ", strftime(max(india_highest_state_cases_with_label$Date), "%e %b %Y"), ",\nseven-day rolling average. Each state on its own scale"))

states.plot

finalise_plot(states.plot,
              source_name = paste("Source: Indian Ministry of Health and Family Welfare, updated",stamp_date),
              #save_filepath = "~/Downloads/india_states_test-nc.png",
              save_filepath = paste0("~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/output/India_cases_sm_",lubridate::today(), "-nc.png"),
              width_pixels = 640,
              height_pixels = 640,
              tinify_file = tinify)

finalise_plot(states.plot,
              source_name = paste("Source: Indian Ministry of Health and Family Welfare, updated",stamp_date),
              save_filepath = paste0("~/Dropbox (BBC)/Visual Journalism/Data/2020/vjdata.26204.coronavirus/output/India_cases_sm_", lubridate::today(), "-nc.pdf"),
              width_pixels = 640,
              height_pixels = 640)

