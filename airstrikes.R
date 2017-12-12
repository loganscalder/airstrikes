#### Project Description #####
### Motivation
# I wanted to monitor US airstrikes agains ISIS in Syia. Several years ago,
# the Department of Defense started a coalition against ISIS. The website for 
# the coalition is www.http://inherentresolve.mil/ 
### Data
# The coalition publishes 
# a report on some of their activity nearly every day. In this report is 
# information about the number of airstrikes in Syia and Iraq each day. Those 
# reports can be found in pdf format at 
# http://www.inherentresolve.mil/News/Strike-Releases/
### Process
# I scraped the website http://www.inherentresolve.mil/News/Strike-Releases/
# for pdf reports on airstrikes. The text from these reports was then mined for 
# the number of airstrikes in Syria and Iraq. I aproximate the date for these
# airstrikes by looking at the dates the reports were created. 
### Result
# A graph of cumulative airstrikes overtime


# packages
library(pdftools)
library(tidytext)
library(stringr)
library(tidyverse)


proj.dir <- "/Users/loganscalder/Desktop/Research/airstrikes" 
data.dir <- paste0(proj.dir, "/data") 


# Data 
    # Data reports were found by scraping http://www.inherentresolve.mil/News/Strike-Releases/
        library(XML)
        doc.html<- htmlParse("http://www.inherentresolve.mil/News/Strike-Releases/")
        doc.links <- xpathSApply(doc.html, "//a/@href")
        pdf.url <- paste0("http://www.inherentresolve.mil/",
                          as.character(doc.links[grep('pdf',doc.links)]))
    # Once found, I downloaded the reports, saving them as file1.pdf, file2, 
    # for 1026 files as of 15 November 2017etc
        # destfile = paste0(data.dir,
        #                   "file",
        #                   1:length(pdf.url)   # or 1:1026 for what I have now
        #                   ".pdf")
        # mapply(FUN = download.file, 
        #        url = pdf.url,
        #        destfile = destfile)

    # Data read-in:
        # I will read in data with pdf_text() function. 
        # Each report is about 2 pages; 
            # I decided to collapse each report into one page
        # If I were to read in and cat() one report, it would look like this:
            # report1 = pdf_text(file_names[1]) %>% 
            #     paste(collapse = "NEWPAGE")  %>%   # collapse multiple pages 
            #     cat()
        # To read in all reports then, it would look like:
file_names <- list.files(data.dir, pattern="file\\d+\\.pdf")
file_paths <- paste0(data.dir, "/", file_names)

report_texts <- file_paths %>%
    sapply(FUN = function(x){pdf_text(x) %>%
            paste(collapse = "NEWPAGE")})

        # And to find the dates that the reports were created can come from
            #pdf_info(file_paths)$created %>% as.Date
report_dates <- file_paths %>%
    lapply(FUN = function(x){       # can't sapply: http://r.789695.n4.nabble.com/unlist-list-of-dates-td4186744.html
        pdf_info(x)$created %>%     # time stamp created
            as.Date()     # I only care about the day
        }) %>%
    do.call(c, args = .)  # unlist() has no method for Date objects


# Process
    
    # If you look at the reports, you'll notice a pattern. From my flawed 
    # observations, it looks like this pattern gets more consistent for later
    # reports. The reports give the number of strikes in Syria and Iraq with 
    # sentences that look like: 
    # "In Syria, coalition military forces conducted [number] strikes... 
    # ...in Iraq, coalition military forces conducted [number] strikes..."
    # In years outside 2017, they might talk of strikes in other countries. 
    # But I only focus Syria and Iraq here though. 
    # I'll look for other countries at a later time
    # So I will tokenize the sentences of the reports and keep only those
    # sentences that fit that pattern. 

common_pattern = "coalition\\smilitary\\sforces\\sconducted\\s.+?\\sstrikes?"
syria_pattern <-
    paste0("in\\ssyria,\\s", common_pattern)
iraq_pattern <-
    paste0("in\\siraq,\\s", common_pattern)
    
syria_iraq_strikes <-
    list(
        file_name = file_names,
        report_date = report_dates,
        report_text = report_texts) %>%
    as_tibble() %>%
    unnest_tokens(output = "sentence", input = "report_text", 
                  token = "sentences",
                  drop = F    # maybe I'll want the whole report_text?
                ) %>%
    filter( 
        grepl(x = sentence, pattern = syria_pattern) |
            grepl(x = sentence, pattern = iraq_pattern)) 

    # Extract country and numbers
        # I'll use the str_extract() function from the stringr package to 
        # extract the country and sub() to pull the number of strikes
            # later we'll have to format the numbers so they're all writen in 
            # digits. In the report, numbers less than ten are spelled out.
syria_iraq_strikes <-
    syria_iraq_strikes %>%
    mutate(
        country = str_extract(sentence, "(syria)|(iraq)"),
        strikes = sentence %>% 
            sub(x = .,
                pattern = ".+conducted\\s",
                replacement = "") %>%
            sub(x = .,
                pattern = "\\sstrike.+",
                replacement = "")
    )
        # Now for the silly part of changing spelled out numbers to digits,
        # I only need to do this for 1 through ten. Maybe there's a more elegant
        # way to do it, but we'll just do a lot of sub()ing


str_to_digits <- function(x){
    # input a charater vector, substitute written numbers with digits
    # output will be character
    written = c("one", "two", "three", "four", "five", 
                "six", "seven", "eight", "nine", "ten")
    for (i in 1:10){
        x = sub(    
            x = x,
            pattern = written[i],  # ex: look for "one"
            replacement = i)       # replace with "1"
    }
    x
}

syria_iraq_strikes <-    
    syria_iraq_strikes %>% 
    mutate(
        strikes = strikes %>%
            str_to_digits() %>%
            as.numeric()
    )
    
    # Cummulative number of strikes
        # I'll want to make a graph of the cummulative strikes in each country,
        # so we'll add up the strikes in each country as time goes on.

syria_iraq_strikes <- 
    syria_iraq_strikes %>%
    group_by(country) %>%
    mutate(
        country.cummulative.strikes = cumsum(strikes)
    )

# Check
    # The data should only have one number for each country each day. 
    # Maybe it was bad to use the dates from when the reports were
    # created. Let's see how much that will hurt us. 

one_a_day <- syria_iraq_strikes %>%
    group_by(report_date, country) %>%
    summarize(
        file_name_1 = file_name[1],
        file_name_2 = file_names[2],
        this.should.be.one = length(report_date)
    ) %>% 
    filter(
        this.should.be.one > 1
    ) %>% View()

# Visualize
    # Let's get a graph of the strikes each day in each country. 
    # We cover so much 
    # time, it will be a fuzzy graph. Maybe not the way we want it.
    # Bso later we'll later graph the cummulative strikes
ggplot(syria_iraq_strikes,
       mapping = aes(x = report_date, y = strikes, fill = country)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Date", y = "Daily Strikes") 
    

    # cummulative strikes
ggplot(syria_iraq_strikes,
       mapping = aes(x = report_date, y = country.cummulative.strikes,
                     fill = country)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Date", y = "Cummulative Strikes") 

