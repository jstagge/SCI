
require(devtools)
require(roxygen2)


### Create the license
use_mit_license()
use_package("tidyverse", type = "imports")
use_package("lmomco", type = "imports")
use_package("zoo", type = "imports")


### Document all the functions
#use_package_doc()
document()


### Create vignette
use_vignette()

### Create Readme
use_readme_rmd()
use_news_rmd()


### Create data 
#use_data_raw()
#use_data()



### Load all the code to test
load_all()


### To leave VIM :q

