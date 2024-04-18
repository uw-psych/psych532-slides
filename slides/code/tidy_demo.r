# Demo code for lecture 03-working-with-big-data

# Demonstrate melting
library(tidyr)
library(tibble)
patients <- tribble(
  ~name,         ~treatmenta,     ~treatmentb,
  "John Smith",   NA,             2,
  "Jane Doe",     16,             11,
  "Mary Johnson", 3,              1,
  )
patients

patients_long <- patients %>%
  pivot_longer(cols = starts_with("treatment"),
               names_to = "treatment",
               values_to = "result")
patients_long

# Demonstrate split-apply-combine
library(dplyr)
library(ggplot2)

subjects <- read.csv(
    "https://yeatmanlab.github.io/AFQBrowser-demo/data/subjects.csv")

subjects %>% head()

# Split-apply-combine

female <- subjects %>% filter(Gender == "Female")

female %>% head()

age_by_gender <- group_by(subjects, Gender) %>%
    summarise(mean_age = mean(Age, na.rm = TRUE))

age_by_gender %>% head()

group_by(subjects, Gender) %>%
    summarise(Age = mean(Age), IQ = mean(IQ, na.rm = TRUE))

subjects %>%
  group_by(Gender) %>%
  summarise(Age = mean(Age), IQ = mean(IQ)) %>%
  arrange(Gender)

# Merging
nodes <- read.csv(
    "https://yeatmanlab.github.io/AFQBrowser-demo/data/nodes.csv")

nodes %>% head()

nodes_and_subjects <- inner_join(subjects, nodes, by = "subjectID")

nodes_and_subjects <- mutate(
    nodes_and_subjects,
    Gender = factor(Gender),
    subjectID = factor(subjectID))

nodes_and_subjects %>% head()

mean_cgc_fa <- nodes_and_subjects %>%
    filter(Age < 10 & tractID == "Left Cingulum Cingulate") %>%
    group_by(nodeID) %>%
    summarise(mean_fa = mean(fa, na.rm=TRUE))

ggplot(mean_cgc_fa,
    aes(x = nodeID, y = mean_fa)) +
    geom_line() +
    labs(x = "Node ID", y = "Mean FA")


