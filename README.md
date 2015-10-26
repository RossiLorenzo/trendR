# trendR
R pacakge to easily scrape trends data from [Google Trends](https://www.google.com/trends/) or [Naver Trends](http://trend.naver.com/) and import the results into a well formatted R object. 

## Installation
You can install the package using the **devtools** package as follows:
```S
devtools::install_github("RossiLorenzo/trendR")
```

## Usage
There are only two exported functions in the package called `google_trends` and `naver_trends`. 
Using one of those two functions you'll scrape the relative trend service and import the results into R. The syntax for the two functions is similar. For instance a valid call is"
```S
results <- google_trends(query = "test me", region = "England", date = "last 30 days")
```
this call will return the Google Trends for the query 'test me' in England for the last 30 days.

## Note
Since this package is scraping Google it might happen that if you send too many queries your access to Google Trends get blocked for some time. In that case you just need to wait until you get unbloked
