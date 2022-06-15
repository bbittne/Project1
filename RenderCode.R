rmarkdown::render("_Rmd/Project1RMarkdown.Rmd", 
                  output_format = "github_document",
                  output_dir = "./",
                  output_file = "README",
                  output_options = list(html_preview= FALSE)
)