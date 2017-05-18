########################################################
####### Get Revenue over a range of days ###############
########################################################


start_date = "2017-05-12"
end_date = "2017-05-14"


source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')

temp <- amplitude("CT", event="unverified_revenue",
                  group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                  measured_by="sums", start=start_date, end=end_date)

names(temp)[names(temp)=="property"] <- "code"


conv <- get_exchange_rates(unique(temp$code), toCurr="USD", fromDt=min(temp$date), toDt=max(temp$date))

revenue <- inner_join(temp, conv, by=c("code", "date"))
revenue$revenue <- revenue$value * revenue$exch 

rm(conv,temp)

total_revenue <- sum(revenue$revenue, na.rm = T)
print(total_revenue)  
