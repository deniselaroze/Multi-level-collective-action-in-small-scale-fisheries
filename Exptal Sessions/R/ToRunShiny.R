##########
# Shiny
##########



 library(rsconnect)
# rsconnect::deployApp('path/to/your/app')

library(shiny)
library(rsconnect)


rsconnect::setAccountInfo(name='denise-laroze',
                          token='C995B97E994B83E5FFABDA0958618A9C',
                          secret='<SECRET>')

rsconnect::deployApp("C:/Shiny")
#runapp()