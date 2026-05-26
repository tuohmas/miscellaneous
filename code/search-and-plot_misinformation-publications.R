# 12 April 2024
# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi

# Script to reproduce Heikkilä (2025) fig 1, https://doi.org/10.23983/mv.148332
# Combine and create data sets, plot an area chart with publications (Semantic
# Scholar), and news mentions and policy mentions (Atlmetric Explorer)

# PREPARATIONS #################################################################

# Check working directory
getwd()

# Clean the environment
rm(list = ls())

# (Install) and load packages
if(!require("pacman")) { install.packages("pacman") }

pacman::p_load(dplyr,        # For data manipulation
               tidyr,        # For data manipulation
               readr,        # For data manipulation
               purrr,        # For data manipulation
               lubridate,    # For data manipulation
               httr2,        # For API queries
               polite,       # For API queries
               textstem,     # For API queries
               ggplot2,      # For visualizations
               RColorBrewer, # For visualizations
               hrbrthemes,   # For visualizations
               viridis,      # For visualizations
               facetscales,  # For visualizations
               ggh4x         # For visualizations
)

sessionInfo()

# Load and clean exported Altmetric data

colnames <- c("date", "news", "policy")
keywords <- c("misinformation", "disinformation", "fake news",
              "deepfake", "conspiracy theories")

# Iterate through keywords
output <- vector(mode = "list", length = length(keywords))

for (i in seq_along(keywords)) {

  # Show progress
  cat(paste0("\nProcessing data related to keyword \n", keywords[i]))

  path <- paste0("data/altmetric_", keywords[i],".csv")

  output[[i]] <- read_csv(path) %>%
    # Select: News mentions and policy mentions, date
    select(`Date`, `News mentions`, `Policy mentions`) %>%
    # Filter by date
    # filter(`Date` >= as.Date("2015-01-01"), `Date` < as.Date("2024-12-31")) %>%
    filter(`Date` >= as.Date("2016-01-01"), `Date` < as.Date("2025-12-31")) %>%
    # Simplify column names
    rename("date" = `Date`, "news" = `News mentions`,
           "policy" = `Policy mentions`) %>%
    # Add keyword
    mutate(keyword = keywords[i], .before = everything())

}

# Inspect output
output[[1]]

# Bind together
altmetric_data <- do.call("rbind", output) %>%
  # Group by keyword and year and summarise
  group_by(keyword, year = lubridate::floor_date(date, "year")) %>%
  summarize(news = sum(news), policy = sum(policy)) %>%
  ungroup()

# Input missing dates
all_dates <- altmetric_data %>%
  mutate(year = ymd(year))  %>%
  # expand(keyword, year = seq.Date(ymd("2015-01-01"), ymd("2024-01-01"), by = "year"))
  expand(keyword, year = seq.Date(ymd("2016-01-01"), ymd("2025-01-01"), by = "year"))

altmetric_data <- altmetric_data %>%
  right_join(all_dates, by = c("year", "keyword")) %>%
  replace(is.na(.), 0)

altmetric_data

View(altmetric_data)

## RETRIEVE BIBLIOMETRIC DATA FROM SEMANTIC SCHOLAR ACADEMIC GRAPH API #########

# Wrap request function inside politely (because good manners)
# Throws an error, because expects a function with explicit 'url' argument
politely_req <- politely(httr2::request, verbose = TRUE)

# Request Semantic Scholar, Academic Graph API
req <- politely_req("https://api.semanticscholar.org/graph/v1")

# Tell who your are
my_user_agent <- paste(
  "polite", "(mailto:tuomas.k.heikkila@helsinki.fi, University of Helsinki)", # Replace
  getOption("HTTPUserAgent"))

req <- req %>%
  req_user_agent(my_user_agent)

# Key token (optional)
Sys.setenv("S2_KEY" = "") # Replace with key token

keywords <- c("misinformation", "disinformation", "fake news",
              "deepfake", "conspiracy theories")

# List of parameters we would like to pass to the requests (Read the
# documentation here https://api.semanticscholar.org/api-docs/)

params <- list(
  fields           = c("paperId", "corpusId", "title", "abstract", "venue",
                       "citationCount", "publicationDate"),
  fieldsOfStudy    = c("Psychology", "Sociology", "Political Science"),
  publicationTypes = c("Review", "JournalArticle", "Editorial", "MetaAnalysis",
                       "Study", "Book", "BookSection")
)

# Define time range
years <- seq(2016, 2025, by = 1)

# Initialize an output list for iterations
output_scholar <- vector(mode = "list",
                         length = length(keywords) * length(years))


# Initialize k for indexing output list
k <- 1

# Iterate: query S2 Academic Graph API for every keyword year combination
# (Paper search bulk endpoint)
for (i in seq_along(keywords)) {

  for (j in seq_along(years)) {

    # Build query
    query <- req %>%
      # Optional: pass acess token to the header
      # req_headers(`x-api-key` = Sys.getenv("S2_KEY"))
      req_progress() %>%  # Monitor progress

      # Limit strain on API (thread lightly when querying w/o an access token)
      req_throttle(rate = 1 / 5) %>%       # Limit requests to once every 5 sec
      req_retry(max_tries = 5, backoff = ~ 60) %>%

      req_url_path_append("paper/search/bulk") %>%  # Paper bulk search endpoint

      # Pass iteration specific information: keyword and year and
      req_url_query(query = paste0("\"", keywords[i], "\""), # Inside quotes
                    year = years[j],
                    !!!params, .multi = "comma") # Additional params from a list

    # Try query out
    query %>% req_dry_run()

    # Perform the request
    resp <- query %>%
      req_perform()

    if (resp_content_type(resp) != "application/json") {

      # FIXME raise an error: response is of wrong type
      break }

    json <- resp %>% httr2::resp_body_json(simplifyVector = TRUE)

    data <- json %>% pluck("data") %>% as_tibble() %>%
      mutate(keyword = keywords[i], year = years[j])

    glimpse(data)

    output_scholar[[k]] <- data

    k <- k + 1

    # In case exceeds 1,000 results, use a string token used to continue
    # fetching additional results
    while (!is.null(json$token)) {

      message("\nFetching additional results with a token...\n")

      query <- req %>%
        # Optional: pass acess token to the header
        # req_headers(`x-api-key` = Sys.getenv("S2_KEY")) %>%
        req_progress() %>%

        # Limit strain on API (thread lightly when querying w/o an access token)
        req_throttle(rate = 1 / 5) %>%
        req_retry(max_tries = 5, backoff = ~ 60) %>%

        req_url_path_append("paper/search/bulk") %>%
        req_url_query(query = paste0("\"", keywords[i], "\""),
                      year = years[j],
                      token = json$token,
                      !!!params, .multi = "comma")

      resp <- query %>%
        req_perform()

      json <- resp %>% httr2::resp_body_json(simplifyVector = TRUE)

      data <- json %>% pluck("data") %>% as_tibble() %>%
        mutate(keyword = keywords[i], year = years[j])

      glimpse(data)

      # Increase output list size by one
      output_scholar <- append(output_scholar,
                               vector(mode = "list", length = 1L))

      output_scholar[[k]] <- data

      k <- k + 1

    }
  }
}

# Bind results
results <- do.call("bind_rows", output_scholar)

# Validate results
results %>% group_by(keyword) %>% summarise(years = n_distinct(year))

glimpse(results)

results <- results %>%

  # Unite title and abstract to one text column
  unite("text_cols", title:abstract, na.rm = T, sep = " ", remove = F) %>%

  # Lowercase text
  mutate(text_cols = tolower(text_cols)) %>%

  # Check if text col matches the keyword
  rowwise() %>%

  mutate(keyword_check =
           ifelse(grepl(stem_words(keyword), text_cols), 1, 0)) %>%

  mutate(keyword_check =
           ifelse(grepl(gsub("-", " ", keyword), text_cols), 1, keyword_check))

results %>% group_by(keyword) %>% summarise(check = sum(keyword_check))

# Discard documents without any keyword mathces
results <- results %>% filter(keyword_check == 1)

glimpse(results)

# Keep distinct papers, datefy year, merge with altmetric data
master_data <- results %>%
  distinct(paperId, .keep_all = TRUE) %>%
  group_by(keyword, year) %>% summarise(papers = n()) %>%
  ungroup() %>%
  mutate(year = ymd(year, truncated = 2L)) %>%
  left_join(altmetric_data, ., by = c("year", "keyword")) %>%
  arrange(keyword)

# Fill NA with zeros
master_data[is.na(master_data)] <- 0

glimpse(master_data)

gather <- master_data %>% gather("type", "mentions", news, policy, papers)

glimpse(gather)

# Arrange factor levels based on frequencies
levels <- gather %>% group_by(keyword) %>% summarise(sum = sum(mentions)) %>%
  arrange(sum) %>%
  pull(keyword)

gather$keyword <- factor(gather$keyword, levels = levels)
gather$type <- factor(gather$type, levels = c("papers", "news", "policy"))

# Color palette imitated from: https://mojomox.com/color-yellow
my_colours <- c(
  # "#8B2F99",    # Dark purple
  "#D154E4",     # Purple
  "#FFAE42",     # Yellow-Orange
  "#F9D460",     # Yellow
  "#71E7C5",     # Mint
  "#4C9C86")     # Dark Mint

# Define custom scales for the facet wrap
df_scales <- data.frame(
  Panel = c("papers",
            "news",
            "policy"),
  ymin = c(0, 0, 0),
  ymax = c(5000, 5000, 500))

df_scales <- split(df_scales, df_scales$Panel)

df_scales

scales <- lapply(df_scales, function(x) {
  scale_y_continuous(limits = c(x$ymin, x$ymax)) } )


# Prepare the plot
type.labs <- c("Social scientific publications",
               "Publications mentioned in news media",
               "Publications mentioned in policy papers")

names(type.labs) <- c("papers", "news", "policy")

gplot <- ggplot(gather, aes(x = year, y = mentions,
                        fill = factor(keyword, levels = levels)))

gplot <- gplot + geom_area() +

  labs(color = "Keyword") +

  # scale_colour_brewer(palette = "YlOrRd", direction = -1) +
  scale_fill_manual(values = my_colours) +

  facet_wrap( ~ type, nrow = 1, scales = "free",
              labeller = labeller(type = type.labs)
              ) +

  ggh4x::facetted_pos_scales(y = scales) +

  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                date_minor_breaks	= "1 year") +

  labs(fill = "Search term",
       x = "Year",
       y = "Mentions") +

  theme_bw(base_size = 9) +

  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save outputs
png("plots/misinformation_paper_mentions.png", width = 170*1.5, height = 96*1.5, units = 'mm', res = 300)
plot(gplot) # Make plot
dev.off()

tiff("plots/misinformation_paper_mentions.tiff", width = 170*1.5, height = 96*1.5, units = 'mm', res = 600)
plot(gplot) # Make plot
dev.off()

jpeg("plots/misinformation_paper_mentions.jpg", width = 170*1.5, height = 96*1.5, units = 'mm', res = 300)
plot(gplot) # Make plot
dev.off()

pdf("plots/misinformation_paper_mentions.pdf",
    width = 170*1.5*0.0393701,
    height = 96*1.5*0.0393701,
    colormodel = "cmyk",
    paper = "special")
plot(gplot)
dev.off()
