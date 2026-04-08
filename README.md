# pe122

Helper package for the PE 1.22 module of the German Sport University Cologne.

Install the package via:

```r
remotes::install_github("pe1-22/pe122")
```

## Development Workflow

First, always pull the latest version from GitHub using GitHub Desktop or the Source control panel in Positron.

Go to the .Rmd file in the `inst/tutorials` subfolder. For example, the first exercise has the path `inst/tutorials/01-linear-model/01-linear-model.Rmd`. Create a new file or modify the content of the existing file by using learnr exercise chunks (if code should be submitted by the user) or learnr quizzes for quiz questions. More details can be found on the [website](https://rstudio.github.io/learnr/) of the learnr package.

If you want to test the tutorials locally use the following workflow:

1. Change the code in the .Rmd file
2. run `devtools::install()` 
3. open the tutorial with, e.g. `learnr::run_tutorial(name = "01-linear-model", package = "pe122")`

Commit your changes regularly. When you finish your work for the day, push the changes to the GitHub repo.
