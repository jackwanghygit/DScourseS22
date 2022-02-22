system('wget -O dates.json "https://www.
vizgr.org/historical-events/search.php?format=json&begin_date=00000101&
end_date=20220219&lang=en"')
system('cat dates.json')

library(rvest)
library(tidyverse)
library(jsonlite)

mylist<-fromJSON('dates.json')
mydf<-bind_rows(dates.json$result[-1])

# Check data type
class(mydf)
class(mydf$date)

head(mydf)
