### import and process raw data files
library(tidyverse)
# theme_set(
#   theme(plot.title = ggtext::element_markdown(size = 11),
#         plot.subtitle = ggtext::element_markdown(size = 9))
# )


# MSCI Spain ----------------------------------------------------------------

msci_spain_index <- read_excel(
  "data/msci_spain.xlsx",
  sheet = "MSCI Spain",
  col_types = c("date", "numeric", "numeric", "numeric"),
  skip = 17)

msci_spain_index <- msci_spain_index %>%
  rename("date" = "Exchange Date",
         "close" = "Close",
         "net" = "Net",
         "change" = "%Chg")

summary(msci_spain_index)

# note that for the first day no net return and relative change is reported and
# thus noted as NA
msci_spain_index %>%
  filter(is.na(net) | is.na(change))


msci_spain_top_const <- read_excel(
  "data/msci_spain.xlsx",
  sheet = "Assers", col_names = paste0("var_", 1:14),
  col_types = c("text", "text", "numeric",
                "numeric", "date", "numeric", "numeric",
                "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric", "numeric"),
  na = "NA",
  skip = 7)
msci_spain_top_const <- msci_spain_top_const %>%
  select(5:14)

msci_spain_top_colnames <- c(
  "date",
  read_excel(
    "data/msci_spain.xlsx",
    sheet = "Assers", range = "F5:N5", col_names = paste(1:9)
  ) %>%
    as.character() %>%
    str_remove("\\s\\(~E \\)")
)

colnames(msci_spain_top_const) <- msci_spain_top_colnames

# there are quite some NA's so have a look at how the NA's behave over time
yes_no_cols <- c("#92B8DE", "#db4f59")

msci_spain_top_const %>%
  rowid_to_column("id") %>%
  pivot_longer(-c(date, id)) %>%
  mutate(missing = is.na(value)) %>%
  ggplot() +
  geom_point(aes(x = date, y = factor(name), col = missing),
             shape = 15, size = 10) +
  scale_color_manual(values = yes_no_cols) +
  guides(col = "none") +
  labs(x = "", y = "",
       title = paste("Display asset returns that are ",
                        "<span style='color:",
                        yes_no_cols[1],
                        "'>**available**</span>",
                        " or ",
                        "<span style='color:",
                        yes_no_cols[2],
                        "'>**missing**</span>",
                        ".")) +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 11),
        plot.subtitle = ggtext::element_markdown(size = 9))

# some assets joined the index well after the first reported returns in this
# data set. Caixabank seems to be missing although it is one of the current
# top 10 assets of the index

summary(msci_spain_top_const)

# as the minimum date in the first data set containing the index data is roughly
# 2010 one can have a look at the visualization above filtered for later returns

msci_spain_top_const %>%
  filter(date >= min(msci_spain_index$date)) %>%
  rowid_to_column("id") %>%
  pivot_longer(-c(date, id)) %>%
  mutate(missing = is.na(value)) %>%
  ggplot() +
  geom_point(aes(x = date, y = factor(name), col = missing),
             shape = 15, size = 10) +
  scale_color_manual(values = yes_no_cols) +
  guides(col = "none") +
  labs(x = "", y = "",
       title = paste("Display asset returns that are ",
                     "<span style='color:",
                     yes_no_cols[1],
                     "'>**available**</span>",
                     " or ",
                     "<span style='color:",
                     yes_no_cols[2],
                     "'>**missing**</span>",
                     ".")) +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 11),
        plot.subtitle = ggtext::element_markdown(size = 9))


# get log returns TBD





