############################################################################### 
# Startup of the project

# clear environment
rm(list=ls())   


# install.packages("tidyverse")
# install.packages("labelled")
# install.packages("haven")
# install.packages("knitr")
# install.packages("apollo")
# install.packages("kableExtra")
# install.packages("devtools")
# install.packages("texreg")
# install.packages("janitor")
# install.packages("tinytex")
# install.packages("lmtest")
# tinytex::install_tinytex()
# install.packages("texreg")

# load packages
library(apollo)
library(tidyverse)
library(tinytex)
library(haven)
library(labelled)
library(haven)
library(knitr)
library(kableExtra)
library(devtools)
library(texreg)
library(janitor)
library(lmtest)
devtools::source_gist("1fda3215ee548d64d42b1db78f880ec5")
library(texreg)
