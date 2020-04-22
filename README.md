# README for DFO Science - Sustainability Survey 2018 Shiny Application

## Application Information:
Created: January 21, 2020
Last Modified: March 4, 2020
R-Studio Version 1.1.463; R Version 3.6.1 (2019-07-05); Shiny Version 1.4.0
Packages Used: DT(v0.11), ggthemes(v4.2.0), tidyr(v1.0.0), shinyjs(v1.1), reshape2(v1.4.3), plotly(v4.9.1), ggplot2(v3.2.1), shinythemes(v1.1.2), formattable(v.0.2.0.1), data.table(v1.12.8), knitr(v1.27), kableExtra(v.1.1.0), dplyr(v0.8.3), shiny(v.1.4.0)

## Instructions for Running the Application:
1. Before attempting to run the application, ensure that all files included in the "Sustainability Survey 2018 Shiny App_2020_02_04" folder (".Rhistory", "2018_survey_data_en.csv", "App.R", "Readme.txt")    remain in the folder or are all saved together in another location.
1. Open "App.R" in R-studio either by double-clicking the "App.R" file or by opening R-studio and and using the "Open File" option (located under File -> Open File).
1. With the "App.R" R-script open, run the application by clicking "Run App" in the upper right corner of the code.
  Shiny applications can be opened in either the Viewer panel of R-studio, a separate window within R-studio, or in an external browser. It is recommended that this application be opened in an HTML browser. To do this, select the dropdown arrow on the right side of the "Run App" button. Select "Run External" from the dropdown list. Now click "Run App", the application will open immediately in the users default browser.
  *R-studio will always open the application in the computers default browser. To open the application in a different browser, change the default browser of the computer*

1. To close the application, click the "Stop" button (looks like a stop sign) in the console window or close R-studio. This will not automatically close the application if it is open in an external browser, however, it will remove all functionality.

## Trouble-Shooting:
- As a result of some of the data transformations performed at the beginning of the code, the warning "Warning in `[<-.factor`(`*tmp*`, thisvar, value = "No") : invalid factor level, NA generated" appears in the console everytime the application is run. This warning can be safely ignored and the application will run as intended.
- If the warning "Warning in file(file, "rt") : cannot open file '2018_survey_data_en1.csv': No such file or directory Error in file(file, "rt") : cannot open the connection" appears, this means that the "App.R" R-script and the "2018_survey_data_en.csv" datafile are not saved in the same location. The application sets the location of the code as the working directory for R-studio and therefore expects to find the datafile in the same folder. Moving the two files to the same location will resolve this issue.
- Running the application should install and load all required packages automatically. However, if a package fails to load and prevents the app from functioning, packages can be loaded manually by clicking "Tools" -> "Install Packages", entering the package name, and clicking "Install.
- The application currently lacks built-in printing or downloading functionality, but this can be down when launched in an external web-browser. Simply right-click on the desired page and click "print" to print or download the current page as is. 
