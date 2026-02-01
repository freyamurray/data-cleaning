---
title: "data cleaning practice"
author: "Freya Murray"
date: "2026-01-30"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r message=FALSE}
#==================
# Load dependencies
#==================

library(tidyverse)
library(data.table)
```

```{r}
#==================
# Load in data
#==================

df <- read.csv("my_file (1).csv")
```

```{r}
#==================
# Remove unnecessary columns
#==================

df_remove <- df %>% 
  select(-Peak, -All.Time.Peak, -Ref., -Rank)
```

```{r}
#==================
# Renaming columns to more suitable keywords
#==================

colnames(df_remove) <- c("gross_revenue", "gross_2022", "artist", "tour_title", "years", "shows", "gross_avg") 
```

```{r}
#==================
# Reordering columns so all revenue columns are next to each other
#==================

df_ordered <- df_remove[ , c(3, 4, 5, 6,1, 2, 7)]
```

```{r}
#==================
# Removing special characters from tour title column
#==================

df_renamed <- df_ordered %>%
  mutate("tour_title"= gsub("[[:punct:]]|[[4a]]|[[21a]]", "", df_ordered$tour_title)) %>%


print(df_ordered$tour_title)
print("FIX APPLIED")
print(df_renamed$tour_title)
```

```{r}
#==================
# Removing special characters from gross columns, converting column type to numeric
#==================

df_renamed2 <- df_renamed %>%
  mutate("gross_revenue" = gsub("[[:punct:]]|[b]|[e]", "", df_renamed$gross_revenue)) %>%
  mutate("gross_2022" = gsub("[[:punct:]]", "", df_renamed$gross_2022)) %>%
  mutate("gross_avg" = gsub("[[:punct:]]|[b]|[e]", "", df_renamed$gross_avg))

df_renamed2$gross_revenue <- as.numeric(df_renamed2$gross_revenue)  
df_renamed2$gross_2022 <- as.numeric(df_renamed2$gross_2022) 
df_renamed2$gross_avg <- as.numeric(df_renamed2$gross_avg) 
  

print(head(df_renamed$gross_revenue))
print("FIX APPLIED")
print(head(df_renamed2$gross_revenue))
```

```{r}
#==================
# Adding a total revenue column
#==================

df_totals <- df_renamed2 %>%
  group_by(artist) %>%
  mutate("total_revenue" = sum(gross_revenue))
```

```{r}
#==================
# Setting final data frame
#==================

df_final <- df_totals
```

```{r}
#==================
# Creating visualisations
#==================

ggplot(df_final, aes(reorder(artist, total_revenue), (gross_revenue/1000000), fill = artist)) +
  geom_bar(stat = "identity", show.legend = FALSE, alpha = .75) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Total Tour Revenue by Artist", x = "Artist", y = "Gross Revenue ($ million)") 

```

```{r}
#==================
# making a sub data frame for only Taylor Swift
#==================

df_ts <- df_final %>%
  filter(artist == "Taylor Swift")

ggplot(df_ts, aes(reorder(tour_title, gross_revenue), (gross_revenue/1000000), fill = tour_title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Taylor Swift Tour Revenue", x = "Tour Name", y="Revenue ($ million)")
```
