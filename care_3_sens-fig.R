packages <- c("dplyr", "tidyr", "magrittr", "readr", "lubridate", "ggplot2", "patchwork")
for (package in packages) {
  library(package, character.only = T)
}


analysis <- "crude" # {crude, wt}
subgroup <- "leskostates"
mpr_limit <- 80

data_path <- paste0("../results/mpr", mpr_limit, "/risk_", analysis, "_", subgroup, ".csv")
filename <- paste0("../figures/mpr", mpr_limit, "/fig_", analysis, "_", subgroup, ".jpeg")

  
  res <- read_csv(data_path)
  
  res2 <- res %>%
    mutate(
      est68 = est6 + est8,
      est681 = est6 + est8 + est1,
      est6813 = est6 + est8 + est1 + est3,
      est68123 = est6 + est8 + est1 + est2 + est3,
      est681234 = est6 + est8 + est1 + est2 + est3 + est4,
      est6812347 = est6 + est8 + est1 + est2 + est3 + est4 + est7,
      est68123475 = est6 + est8 + est1 + est2 + est3 + est4 + est7 + est5,
      date = mdy("07-01-2001") + days(time)
    )
  
  cols <- c(
    "Dead before ART" = "#084594",
    "Disenrolled before ART" = "#4292c6",
    "Not retained; Not ART adherent" = "#9ecae1",
    "Retained; Not ART adherent" = "#c6dbef",
    "Not retained; ART adherent" = "#deebf7",
    "Retained; ART adherent" = "#f7fbff",
    "Disenrolled after ART" = "#6baed6",
    "Dead after ART" = "#2171b5"
  )
  
  p <- ggplot() +
    labs(
      x = "Calendar Time", y = "Prevalence", fill = ""
    ) +
    geom_area(aes(x = res2$date, y = res2$est68123475, fill = "Dead before ART")) +
    geom_area(aes(x = res2$date, y = res2$est6812347, fill = "Disenrolled before ART")) +
    geom_area(aes(x = res2$date, y = res2$est681234, fill = "Not retained; Not ART adherent")) +
    geom_area(aes(x = res2$date, y = res2$est68123, fill = "Retained; Not ART adherent")) +
    geom_area(aes(x = res2$date, y = res2$est6813, fill = "Not retained; ART adherent")) +
    geom_area(aes(x = res2$date, y = res2$est681, fill = "Retained; ART adherent")) +
    geom_area(aes(x = res2$date, y = res2$est68, fill = "Disenrolled after ART")) +
    geom_area(aes(x = res2$date, y = res2$est6, fill = "Dead after ART")) +
    theme(legend.position = "bottom") + 
    scale_fill_manual(
      breaks = factor(
        c(
          "Dead before ART",
          "Disenrolled before ART",
          "Not retained; Not ART adherent",
          "Retained; Not ART adherent",
          "Not retained; ART adherent",
          "Retained; ART adherent",
          "Disenrolled after ART",
          "Dead after ART"
        ),
        levels = c(
          "Dead before ART",
          "Disenrolled before ART",
          "Not retained; Not ART adherent",
          "Retained; Not ART adherent",
          "Not retained; ART adherent",
          "Retained; ART adherent",
          "Disenrolled after ART",
          "Dead after ART"
        )
      ),
      values = cols,
      guide = guide_legend(
        direction = "horizontal",
        # nrow = 1,
        nrow = 2,
        byrow = TRUE
      )
    ) +
    scale_x_date(
      expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y",
      limits = c(mdy("01-01-2002"), mdy("09-30-2015"))
    ) +
    scale_y_continuous(expand = c(0, 0))
  
  ggsave(
    filename = filename,
    plot = p,
    height = 5, width = 7, units = "in", dpi = 300
  )
  


# combine figures
for (mpr_limit in c(0, 70, 80, 90)) {
  main_fig <- generate_figure(analysis, mpr_limit)
  
  for (subgroup in subgroups) {
    subgroup_fig <- generate_figure(analysis, mpr_limit, subgroup)
  }
  
  # combine subgroup figures by region
  region_subgroups <- c("northeast", "midwest", "south", "west")
  region_figures <- lapply(1:length(region_subgroups), function(i) {
    subgroup <- region_subgroups[i]
    # title <- region_titles[i]
    fig <- generate_figure(analysis, mpr_limit, subgroup)
    return(fig)
  })
  fig_by_region <- wrap_plots(region_figures) + plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = "bottom")
  
  ggsave(
    filename = paste0("./figures/mpr", mpr_limit, "_fig_by_region_", analysis, ".jpeg"),
    plot = fig_by_region,
    height = 10, width = 14, units = "in", dpi = 300
  )
  
  # combine subgroup figures by sex
  sex_subgroups <- c("F", "M")
  sex_figures <- lapply(1:length(sex_subgroups), function(i) {
    subgroup <- sex_subgroups[i]
    # title <- sex_titles[i]
    fig <- generate_figure(analysis, mpr_limit, subgroup)
    return(fig)
  })
  fig_by_sex <- wrap_plots(sex_figures) + plot_layout(ncol = 1, guides = "collect") &
    theme(legend.position = "bottom")
  
  ggsave(
    filename = paste0("./figures/mpr", mpr_limit, "_fig_by_sex_", analysis, ".jpeg"),
    plot = fig_by_sex,
    height = 10, width = 7, units = "in", dpi = 300
  )
  
  # combine subgroup figures by race
  race_subgroups <- c("black", "white", "hisp")
  race_figures <- lapply(1:length(race_subgroups), function(i) {
    subgroup <- race_subgroups[i]
    # title <- race_titles[i]
    fig <- generate_figure(analysis, mpr_limit, subgroup)
    return(fig)
  })
  fig_by_race <- wrap_plots(race_figures) + plot_layout(ncol = 1, guides = "collect") &
    theme(legend.position = "bottom")
  
  ggsave(
    filename = paste0("./figures/mpr", mpr_limit, "_fig_by_race_", analysis, ".jpeg"),
    plot = fig_by_race,
    height = 15, width = 7, units = "in", dpi = 300
  )
  
  # save the main figure directly
  ggsave(
    filename = paste0("./figures/mpr", mpr_limit, "_fig_main_", analysis, ".jpeg"),
    plot = main_fig,
    height = 5, width = 7, units = "in", dpi = 300
  )
}
