##########
# Shiny
##########


library(shiny)
library(rsconnect)
library(here)


rsconnect::setAccountInfo(name='denise-laroze',
                          token='C995B97E994B83E5FFABDA0958618A9C',
                          secret='+lHsyVic5l+8Ycu4IqoqwfdiujUNl1h2eWSp0WAk')
rsconnect::deployApp(here(), appName = "Fondecyt_Estrategias_b")


#runapp()