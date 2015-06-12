library(rmarkdown)
library(knitr)
setwd("~/GitHub/RepData_PeerAssessment1")
## Used the following to test out the YAML header rendering
#render("PA1_template.Rmd")
## Used for standard figure output for assignment 1, but
## have to override the header
knit2html("PA1_template.Rmd")
browseURL("PA1_template.html")

