# Human Resources Mini App

Human Resources Mini Phone Application in R

This is a mini application made in R which is usable using a server for computing actions performed by the user.

Rendering is very smooth as long as the server is powerful enough (Intel Atom is enough for such task).

1:15 Phone-like video:

![Phone GIF](https://github.com/Laurae2/HR_MiniApp/blob/master/HR_MiniApp.gif)

1:20 Tablet-like video:

![Tablet GIF](https://github.com/Laurae2/HR_MiniApp/blob/master/HR_MiniApp_Tablet.gif)

# Requirements

You will need the following libraries in R:

```r
library(shiny)
library(miniUI)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)
library(qgraph)
library(plotly)
library(reshape2)
library(cluster)
library(rpart)
library(rpart.plot)
```

You can install most of them from CRAN doing the following:

```r
install.packages(c("shiny", "ggplot2", "plotly", "data.table", "DT", "qgraph", "plotly", "reshape2", "cluster", "rpart", "rpart.plot"))
```

You can install `miniUI` from GitHub using the following:

```r
install.packages("devtools")
devtools::install_github("rstudio/miniUI")
```

You also need to download the data `HR_comma_sep.csv`, and point the `HR_MiniApp.R` line 15 to the CSV file using absolute path if you can (so you do not risk of messing up). Data is from [HR Analytics dataset](https://www.kaggle.com/ludobenistant/hr-analytics).

# Running the App

You may run the App on a full window using an interactive R console or an Rscript. For the latter:

```bash
Rscript path_to_my_app/HR_MiniApp.R
```

Or, if you do not have Rscript in your PATH, from a console, replace the path I use for Rscript by yours:

```bash
D:\Data Science\HRAnalysis>"C:\Program Files\Microsoft\R Client\R_SERVER\bin\Rscript.exe" HR_MiniApp.R
```

Then, you can access the Application from the port provided in a browser! (just copy `http://127.0.0.1:<the port number>` in your browser).

You may interrupt the App by pressing Ctrl+C.

0:20 Console version video:

![Console GIF](https://github.com/Laurae2/HR_MiniApp/blob/master/HR_Console.gif)

To use the App on phone/tablet, make sure you know the IP of your computer and remain in your local Intranet. Then, you just need to replace the IP from the http to the IP of your computer. Make sure to open the Firewall if it denies permission to access.

Click "Allow access":

![Firewall](https://cloud.githubusercontent.com/assets/9083669/24083727/122136f0-0cdd-11e7-98ba-46241d6268f0.png)
