library(tidyverse)
library(magrittr)
library(igraph)
library(tidygraph)
library(ggraph)
library(RColorBrewer)


# Node tibble
## Node IDs
node_ids = 
  c("prim_obj", 
    # Descriptive, cause-and-effect
    "desc_time", # Descriptive ->
    # cross-sectional, retrospective, prospective
    "desc_time_cross", # TERMINAL
    "desc_time_retro", # TERMINAL 
    "desc_time_prosp", # TERMINAL
    #
    "exper_or_obs", # Cause-and-effect -> 
    # Experiment, Observational study
    "rare", # Cause-and-effect -> Observational ->
    # matched, time
    "matched", # Cause-and-effect -> Observational -> rare ->
    # casecontrol (unmatched), casecontrol (matched)
    "casecontrol_unmatched", # TERMINAL
    "casecontrol_matched", # TERMINAL
    "obs_rare_time", # Cause-and-effect -> Observational ->
    # cross-sectional, retrospective, prospective
    "obs_rare_time_cross", # TERMINAL
    "obs_rare_time_retro", # TERMINAL
    "obs_rare_time_prosp", # TERMINAL
    #
    "ctrlgrp",  # Cause-and-effect -> Experimental ->
    # Yes, No
    "ctrlgrp_no_waves", # Cause-and-effect -> Experimental -> No
    # Yes, No
    "interrupted", # TERMINAL
    "steppedwedge", # TERMINAL
    "ctrlgrp_yes_unitrand", # Cause-and-effect -> Experimental -> Yes
    # Individual, Group
    "crt", # TERMINAL
    "rct" # TERMINAL
  )

## Node labels
node_labels = 
  c("Primary objective?", 
    # Descriptive, cause-and-effect
    "Timeframe?", # Descriptive ->
    # cross-sectional, retrospective, prospective
    "Cross-sectional\nobservational study", # Desc -> cross-sectional
    "Retrospective cohort\nobservational study", # Desc -> retro
    "Prospective cohort \nobservational study", # Desc -> prosp
    #
    "Observational\n or Experimental?", #  Cause-and-effect ->
    # Experiment, Observational study
    "Rare disease?", # Cause-and-effect -> Observational -> 
    "Matched?", # Cause-and-effect -> Observational -> rare
    "Case-control\n(unmatched)", # TERMINAL
    "Case-control\n(matched)", # TERMINAL
    "Timeframe?", # Cause-and-effect -> Observational
    "Cross-sectional\nobservational study", # TERMINAL
    "Retrospective cohort \nobservational study", # TERMINAL
    "Prospective cohort \nobservational study", # TERMINAL
    #
    "Control group?",  # Cause-and-effect -> Experimental ->
    # Yes, No
    "Can you do\nit in waves?", # Cause-and-effect -> Experimental -> No
    # Yes, No
    "Interrupted \nTime Series", # TERMINAL
    "Stepped\nWedge", # TERMINAL
    "Unit of\nRandomization?", # Cause-and-effect -> Experimental -> Yes
    # Individual, Group
    "Cluster\nRandomized\nTrial", # TERMINAL
    "Randomized\nClinical\nTrial" # TERMINAL
  )


## Put them together
node_tbl = 
  tibble(id = node_ids,
         label = node_labels)


# Edge tibble
edge_tbl =
  matrix(c("prim_obj","desc_time",                  "Description",
           "desc_time","desc_time_cross",           "Snapshot",
           "desc_time","desc_time_retro",           "Past",
           "desc_time","desc_time_prosp",           "Future",
           "prim_obj","exper_or_obs",               "Cause-and-Effect",
           "exper_or_obs","rare",                   "Observational",
           "rare","matched",                        "Rare",
           "matched","casecontrol_unmatched",       "No",
           "matched","casecontrol_matched",         "Yes",
           "rare","obs_rare_time",                  "Not rare",
           "obs_rare_time", "obs_rare_time_cross",  "Snapshot",
           "obs_rare_time", "obs_rare_time_prosp",  "Future",
           "obs_rare_time","obs_rare_time_retro",   "Past",
           "exper_or_obs","ctrlgrp",                "Experimental",
           "ctrlgrp","ctrlgrp_no_waves",            "No",
           "ctrlgrp_no_waves","interrupted",        "No",
           "ctrlgrp_no_waves","steppedwedge",       "Yes",
           "ctrlgrp","ctrlgrp_yes_unitrand",        "Yes",
           "ctrlgrp_yes_unitrand","crt",            "Group/\nCluster",
           "ctrlgrp_yes_unitrand","rct",            "Individual"),
         ncol = 3,
         byrow = TRUE,
         dimnames = list(NULL,
                         c("from","to","label"))) %>% 
  as_tibble()


# Create graph
dendro = 
  tbl_graph(nodes = node_tbl,
            edges = edge_tbl,
            directed = TRUE,
            node_key = "id")

dendro %<>% 
  activate(nodes) %>% 
  mutate(terminal = ifelse(igraph::degree(dendro, mode = "out") > 0,"nonterm","term"))


# Plot graph
lo = 
  dendro %>% 
  create_layout(layout = "dendrogram")

dendro_plot =
  dendro %>% 
  ggraph() +
  geom_edge_link(aes(label = label,
                     start_cap = label_rect(node1.label),
                     end_cap = label_rect(node2.label)),
                 arrow = arrow(length = unit(10, "points"),
                               type = "closed",
                               angle = 15),
                 label_colour = brewer.pal(5,"Blues")[3]) +
  geom_node_label(aes(label = label,
                      color = terminal,
                      fill = terminal)) +
  scale_color_manual(values = c("term" = brewer.pal(5,"Blues")[1],
                                "nonterm" = brewer.pal(5,"Blues")[5])) +
  scale_fill_manual(values = c("nonterm" = brewer.pal(5,"Blues")[1],
                               "term" = brewer.pal(5,"Blues")[5])) +
  theme_graph() +
  theme(legend.position = "none")

dendro_plot %>% 
  ggsave(filename = "C:/Users/dksewell/Documents/PRC/projects/study_design_webapp/dendrogram/study_design_dendrogram.png",
         height = 3500 / sqrt(2),
         width = 3500 * sqrt(2),
         units = "px")



