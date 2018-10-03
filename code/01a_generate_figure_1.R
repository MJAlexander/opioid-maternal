## 01a_generate_figure_1.R
## 
## Code to reproduce Figure 1 of our research letter: 
##      Gemmill A, Kiang MV, and Alexander MJ. Trends in pregnancy-associated
##      mortality involving opioids in the United States, 2007-2016. American 
##      Journal of Obstetrics and Gynecology. Published ahead-of-print 
##      (September 2018). doi: 10.1016/j.ajog.2018.09.028
##  
## See README for more details regarding the entire reproducible pipeline.

## Libraries ----
library(tidyverse)
library(here)
source(here("code", "mk_nytimes.R"))

## Load working data ----
mmrs <- read_csv(here("data", "working_data_2007_2016.csv"))

## Make a dataframe just for the on-plot labels
mmrs_labels <- tribble(
    ~year, ~mmr,  ~race3,        ~type,               ~label,
     2009,   89,   "nhb", "all deaths", "Non-Hispanic Black", 
     2011,   30,   "nhw", "all deaths", "Non-Hispanic White", 
     2013,   52, "total", "all deaths",              "Total"
    )%>% 
    mutate(type_cat = factor(type, 
                             levels = c("all deaths", "opioid-related deaths"), 
                             labels = c("All causes", "Opioid-related"), 
                             ordered = TRUE), 
           race_cat = factor(race3, 
                             levels = c("total", "nhw", "nhb"), 
                             labels = c("Total", "Non-Hispanic White", 
                                        "Non-Hispanic Black"), 
                             ordered = TRUE)
    )

## Create the plot
p1 <- ggplot(clip = FALSE) + 
    geom_line(data = mmrs, 
              aes(x = year, y = mmr, color = race_cat))+ 
    geom_point(data = mmrs, 
               aes(x = year, y = mmr, color = race_cat), 
               size = 3) + 
    geom_errorbar(data = mmrs, 
                  aes(x = year, color = race_cat, 
                      ymin = lower, ymax = upper), 
                  width = .25) + 
    geom_text(data = mmrs_labels, 
              aes(x = year, color = race_cat, y = mmr, label = label)) + 
    scale_color_brewer(palette = "Set1") + 
    scale_y_continuous(expand = c(.01, 0)) + 
    # scale_x_continuous(breaks = seq(2008, 2016, 2), 
    #                    labels = sprintf("'%02i", seq(8 ,16, 2)), 
    #                    expand = c(0, .25)) + 
    facet_wrap(~ type_cat, scales = "free_y", ncol = 1) + 
    mk_nytimes(legend.position = "none", 
               panel.grid.major.y = element_line(color = "grey80")) + 
    labs(#title = "Pregnancy-associated Deaths", 
        y = "Deaths (per 100,000 live births)", 
        x = NULL) 

ggsave(filename = "./figs/figure1.pdf", plot = p1, 
       device = cairo_pdf, width = 6, height = 6, scale = 1.25)
ggsave(filename = "./figs/figure1.png", plot = p1, 
       width = 6, height = 6, scale = 1.25)
