# This script will read in the text of downloaded strike reports
# It will then make a table of the report texts and their dates created
# The table will be saved as strike_reports.RData


proj_dir <- "/Users/loganscalder/Desktop/Research/airstrikes" 
data_dir <- paste0(proj_dir, "/data") 

file_names <- list.files(data_dir, pattern="file\\d+\\.pdf")
file_paths <- paste0(data_dir, "/", file_names)

report_texts <- file_paths %>%
    sapply(FUN = function(x){pdf_text(x) %>%
            paste(collapse = "NEWPAGE")})

report_dates <- file_paths %>%
    lapply(FUN = function(x){       # can't sapply... more info below *
        pdf_info(x)$created %>%     # time stamp created
            as.Date()     # I only care about the day
    }) %>%
    do.call(c, args = .)

strike_reports <-
    list(
        report_created_date = report_dates,
        report_text = report_texts 
    ) %>%   
    as_tibble()

save(strike_reports, file = "data_processed/strike_reports.RData")
