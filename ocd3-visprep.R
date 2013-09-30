library(ggplot2)
library(plyr)

# Source for job impact data:
# http://news.investors.com/politics-obamacare/092513-669013-obamacare-employer-mandate-a-list-of-cuts-to-work-hours-jobs.htm

emp.f <- read.csv("~/employers.csv", stringsAsFactors=FALSE)
colnames(emp.f) <- c("State","Employer","Type","Action","Jobs.Cut","Action.Date")
emp.f[is.na(emp.f$Jobs.Cut),]$Jobs.Cut = median(emp.f$Jobs.Cut, na.rm=TRUE)
emp.f[emp.f$State=="Virgina", ]$State = "Virginia"
emp.f[emp.f$State=="Washington DC", ]$State = "District of Columbia"

p <- ggplot(emp.f, aes(x=Action.Date, y=Jobs.Cut))
p <- p + geom_bar(aes(fill=State), stat="identity")
p <- p + facet_wrap(~State)
p <- p + theme_bw()
p <- p + theme(legend.position=0, axis.text.x = element_text(angle = 90))
p <- p + labs(x="Action Date", y="# Jobs Cut")
p


# aggregate state data
emp.state.sum.df <- count(emp.f,c("State"),c("Jobs.Cut"))
colnames(emp.state.sum.df) <- c("State","Total.Jobs.Cut")

# get total (estimated) jobs impacted
total.jobs <- sum(emp.state.sum.df$Total.Jobs.Cut)

# Source for the red v blue state data:
# http://www.gallup.com/poll/125066/State-States.aspx
# read political leanings
red.blue.df <- read.csv("~/red-blue.csv", stringsAsFactors=FALSE)

# join the jobs and leaning data together
s <- join(emp.state.sum.df, red.blue.df, by="State")

# cheat and get leaning range for manual input into the datavis
leaning.range <- range(s$Conservative.Advantage)

# build the JSON data file. store state summary data for the bubbles, but also include
# the detail level for extra data for the viz
# need to clean up this file post-write and definitely run it through http://jsonlint.com/
jsfile = file("states.tmp","w")
by(s, 1:nrow(s), function(row) {
  writeLines(sprintf('      {"name": "%s", "size":%d, "leaning":%2.1f, "detail":[',row$State,row$Total.Jobs.Cut,row$Conservative.Advantage),jsfile)
  employers = emp.f[emp.f$State == row$State,]
  by(employers, 1:nrow(employers), function(emp.row) {
    writeLines(sprintf('          { "employer":"%s", "emptype":"%s", "actiondetail":"%s", "jobsimpacted":%d, "when":"%s"},',
                       emp.row$Employer, emp.row$Type, gsub('"',"'",emp.row$Action), emp.row$Jobs.Cut, emp.row$Action.Date),jsfile)
    
  })
  writeLines("]},\n",jsfile)   
})
close(jsfile)
