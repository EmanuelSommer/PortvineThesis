### import and process raw data files
library(tidyverse)
library(readxl)


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
    str_remove("\\s\\(~E \\)") %>%
    tolower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("\\.", "_")
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
# top 10 assets of the index. For now one proceeds just with the other 9 top
# constituents.

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


# Having these two repective data sets it is time to join them to one and
# calculate the daily log returns
msci_spain_data <- msci_spain_index %>%
  select(date, close) %>%
  left_join(msci_spain_top_const, by = "date") %>%
  rename("msci_spain_index" = "close") %>%
  # compute the daily log returns
  arrange(date) %>%
  mutate(across(!date, ~ log(.x / lag(.x)))) %>%
  # remove first row with just NAs
  slice(-1)

# have a visual look on the data again
msci_spain_data %>%
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

# What is the maximum date for a row having at least one missing value
# (cellnex_telecom as the plot suggests)

first_not_NA_row_msci_spain <- msci_spain_data %>%
  filter(!if_any(.fns = is.na)) %>%
  slice_min(date) %>%
  select(date) %>%
  pull()
first_not_NA_row_msci_spain
# 2015.05.08

# Have a look how the log returns of the index performed over time
ggplot(msci_spain_index, aes(x = date, y = close)) +
  geom_line() +
  labs(x = "", y = "Closing price",
       title = "MSCI Spain index") +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 11),
        plot.subtitle = ggtext::element_markdown(size = 9))

ggplot(msci_spain_data, aes(x = date, y = msci_spain_index)) +
  geom_line() +
  labs(x = "", y = "Daily log return",
       title = "MSCI Spain index") +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 11),
        plot.subtitle = ggtext::element_markdown(size = 9))

# provide also a "complete msci spain" data set that has no missing values and
msci_spain_complete_data <- msci_spain_data %>%
  filter(date >= first_not_NA_row_msci_spain)



range(msci_spain_complete_data$date)

# add data from the S&P500
# data source publicly available:
# https://www.nasdaq.com/de/market-activity/index/spx/historical

sp500_data <- read_csv("data/SP500.csv")
sp500_data <- sp500_data %>%
  transmute(date = lubridate::mdy(Datum),
            sp500 = `Schluss/Letzter`) %>%
  mutate(across(!date, ~ log(.x / lag(.x))))
# append the log returns
msci_spain_complete_data <- msci_spain_complete_data %>%
  left_join(sp500_data, by = "date")

# add data from EUROSTOXX 50
# data source publicly available:
# https://de.finance.yahoo.com/quote/%5ESTOXX50E?p=%5ESTOXX50E

eurostoxx50_data <- read_csv("data/STOXX50E.csv")
eurostoxx50_data <- eurostoxx50_data %>%
  transmute(date = Date,
            eurostoxx50 = Close) %>%
  mutate(across(!date, ~ log(.x / lag(.x))))
# append the log returns
msci_spain_complete_data <- msci_spain_complete_data %>%
  left_join(eurostoxx50_data, by = "date")

msci_spain_complete_data %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything())

# notably there are some missing values apparent. As the closing price of the
# last available day is then still considered to be the valuation one has
# no price movement. This means that for these days an imputation with 0 is
# needed. In the data from Reuters they have apparently used exactly this method
msci_spain_complete_data[is.na(msci_spain_complete_data)] <- 0
# the most recent date (corresponding row) however has to be omitted due
# to the fact that the eurostoxx data was just collected here from this day on
# so an imputation is not correct in that case
msci_spain_complete_data <- msci_spain_complete_data[-1, ]
range(msci_spain_complete_data$date)

ggplot(msci_spain_complete_data, aes(x = date, y = msci_spain_index)) +
  geom_line() +
  labs(x = "", y = "Daily log return",
       title = "MSCI Spain index") +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(size = 11),
        plot.subtitle = ggtext::element_markdown(size = 9))

msci_spain_16_19 <- msci_spain_complete_data %>%
  filter(lubridate::year(date) %in% 2016:2019)

msci_spain_17_20 <- msci_spain_complete_data %>%
  filter(lubridate::year(date) %in% 2017:2020)

save(msci_spain_complete_data, msci_spain_16_19, msci_spain_17_20,
     file = "data/msci_spain_data_clean.RData")

