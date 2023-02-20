# (a) Download JSON file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20230209&lang=en"')

# (b) Print the file to the console
system('cat dates.json')

# (c) Convert to data frame
library(jsonlite)
library(tidyverse)

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# (d) Check object types
class(mydf)
class(mydf$date)

# (e) List first n rows
head(mydf, n = 10)
