library(rsconnect)

rsconnect::setAccountInfo(
    name=Sys.getenv("SHINYAPPS_IO_NAME"), 
    token=Sys.getenv("SHINYAPPS_IO_TOKEN"), 
    secret=Sys.getenv("SHINYAPPS_IO_SECRET_KEY")
)

deployApp()