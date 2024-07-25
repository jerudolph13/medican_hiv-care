
################################################################################
#
# Project: HIV Care Cascade
#
# Purpose: Generate figures
#
# Author: Yiyi Zhou, Jacqueline Rudolph
#
# Last Update: 11 Dec 2023
#
################################################################################


packages <- c("dplyr", "tidyr", "magrittr", "readr", "lubridate", "ggplot2", "patchwork")
for (package in packages) {
    library(package, character.only = T)
}


analysis <- "wt" # {crude, wt}

subgroups <- c(
    "northeast", "midwest", "south", "west",
    "M", "F",
    "black", "white", "hisp"
)


# function to generate figures
generate_figure <- function(analysis, mpr_limit, subgroup = NULL) {
    if (is.null(subgroup)) {
        data_path <- paste0("../results/mpr", mpr_limit, "/risk_", analysis, ".csv")
        filename <- paste0("../figures/mpr", mpr_limit, "/fig_", analysis, ".jpeg")
    } else {
        data_path <- paste0("../results/mpr", mpr_limit, "/risk_", analysis, "_", subgroup, ".csv")
        filename <- paste0("../figures/mpr", mpr_limit, "/fig_", analysis, "_", subgroup, ".jpeg")
    }

    res <- read_csv(data_path)

    res2 <- res %>%
        mutate(
            est13 = est1 + est3,
            est123 = est1 + est2 + est3,
            est1234 = est1 + est2 + est3 + est4,
            est12345 = est1 + est2 + est3 + est4 + est5,
            date = mdy("07-01-2001") + days(time)
        )
    
    res3 <- res2 %>%
      mutate(
        est1_s = est1/est1234,
        est13_s = est13/est1234,
        est123_s = est123/est1234,
        est1234_s = est1234/est1234
      )

    cols <- c(
        "Dead" = "#08519c",
        "Not retained; Not ART adherent" = "#3182bd",
        "Retained; Not ART adherent" = "#6baed6",
        "Not retained; ART adherent" = "#9ecae1",
        "Retained; ART adherent" = "#c6dbef"
    )

    title_mapping <- c(
        "northeast" = "Northeast",
        "midwest" = "Midwest",
        "south" = "South",
        "west" = "West",
        "M" = "Male",
        "F" = "Female",
        "black" = "Non-Hispanic Black",
        "white" = "Non-Hispanic White",
        "hisp" = "Hispanic"
    )

    title <- ifelse(is.null(subgroup), "", title_mapping[subgroup])

    p <- ggplot(data=res2) +
        labs(
            title = title,
            x = "Calendar Time", y = "Prevalence", fill = ""
        ) +
        geom_area(aes(x = date, y = est12345, fill = "Dead")) +
        geom_area(aes(x = date, y = est1234, fill = "Not retained; Not ART adherent")) +
        geom_area(aes(x = date, y = est123, fill = "Retained; Not ART adherent")) +
        geom_area(aes(x = date, y = est13, fill = "Not retained; ART adherent")) +
        geom_area(aes(x = date, y = est1, fill = "Retained; ART adherent")) +
        theme(legend.position = "bottom") + 
        scale_fill_manual(
            breaks = factor(
                c(
                    "Dead",
                    "Not retained; Not ART adherent",
                    "Retained; Not ART adherent",
                    "Not retained; ART adherent",
                    "Retained; ART adherent"
                ),
                levels = c(
                    "Dead",
                    "Not retained; Not ART adherent",
                    "Retained; Not ART adherent",
                    "Not retained; ART adherent",
                    "Retained; ART adherent"
                ),
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

    return(p)
}


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
        filename = paste0("../figures/mpr", mpr_limit, "/fig_by_region_", analysis, ".jpeg"),
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
        filename = paste0("../figures/mpr", mpr_limit, "/fig_by_sex_", analysis, ".jpeg"),
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
        filename = paste0("../figures/mpr", mpr_limit, "/fig_by_race_", analysis, ".jpeg"),
        plot = fig_by_race,
        height = 15, width = 7, units = "in", dpi = 300
    )

    # save the main figure directly
    ggsave(
        filename = paste0("../figures/mpr", mpr_limit, "/fig_main_", analysis, ".jpeg"),
        plot = main_fig,
        height = 5, width = 7, units = "in", dpi = 300
    )
}
