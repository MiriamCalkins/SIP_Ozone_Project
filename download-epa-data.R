# download-epa-data.R

# Download 2012-2014 ozone data for Washington State from the US EPA.

# Setup
library(XML)

# Create data directory if not already exists
datadir <- "Data"
dir.create(file.path(datadir), showWarnings=FALSE, recursive=TRUE)

# ------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------

# Download EPA ozone data for Washington State given the year, all sites
download_WAozone <- function(year) {
    # The url contains the search parameters: year and state (WA), all sites
    url <- paste('http://www.epa.gov/cgi-bin/broker?_service=data&_debug=0&_program=dataprog.ad_data_daily.sas&querytext=&fld=&areaname=&areacontacts=&areasearchurl=&result_template=epafiles_default.xsl&filter=sample4filt.hts&poll=44201&year=', year, '&state=53&cbsa=-1&county=-1&site=-1&flag=Y', sep='')
    
    # Parse the resulting page to get the link to the download file (CSV)
    doc <- htmlParse(url)
    links <- xpathSApply(doc, "//a/@href")
    free(doc)
    
    download.file(paste('http://www.epa.gov', links[[1]], sep='/'), 
                  paste(datadir, '/WAozone', year, '.csv', sep=''))
}

# Read WA EPA ozone CSV, downloading as needed, given the year
read_WAozone <- function(year) {
    ozfile <- paste(datadir, '/WAozone', year, '.csv', sep='')
    
    # Download if missing
    if (!file.exists(ozfile)) {
        download_WAozone(year)
    }
    
    # Read CSV files if found
    if (file.exists(ozfile)) {
        read.csv(ozfile, header=T)
    } else {
        # Abort if still missing
        stop(paste(c("Can't find ", ozfile, "!"), sep=''))
    }
}

# ------------------------------------------------------------------
# Main Routine
# ------------------------------------------------------------------

# Read CSV files into dataframes, stored in a list called "ozone"
years <- c('2012', '2013', '2014')
ozone <- lapply(years, function(x) { read_WAozone(x) })

# Combine dataframes into a single dataframe
ozoneall <- do.call("rbind", ozone)
