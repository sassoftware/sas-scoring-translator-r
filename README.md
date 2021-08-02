rsct - SAS Scoring Code Translator for R 
================

# Overview

This package was made to facilitate the translation of scoring codes
which SAS outputs from its interfaces, specially Model Studio and Visual
Text Analytics. The functions will wrap an DS/DS2 code or make
equivalent call using the `SWAT` package and SAS Viya API. The output is
an R code, ready or almost ready to use in any other application, as a
good starting point for integration.

## What can be translated

This package will translate (or wrap) some Scoring Codes that are export
through some of Viya interfaces. All the output R codes are based on the
[R SWAT](https://github.com/sassoftware/R-swat) package. It’s not
necessary for this package to do the translation, but it is necessary to run
the output code.

The following table will help you to know which function to user for
which kind of code:

-   **Interface**: Where you can obtain the sas generated score code
    (not the only place)

-   **Code Type**: The code type expected by the translator

-   **Base File Name**: The file which the translator will read from
    inside the exported zip File. You can also use that to identify which kind of
    code it is. (dataStep is usually called `dmcas_scorecode.sas` and
    DS2 `dmcas_epscorecode.sas`)

-   **Translation Function**: Name of the function to translate it
    properly

-   **Output Type**: The way the R code is going to be translated to be
    used outside of the SAS environment

-   **Sample File**: Sample of the usual name that is exported by the SAS visual interfaces


| Interface             | Code Type                  | Base File Name         | Translation Function        | Output Type                                                             | Sample File                          |
|:----------------------|:---------------------------|:-----------------------|:----------------------------|:------------------------------------------------------------------------|:-------------------------------------|
| Model Studio          | DataStep                   | dmcas\_scorecode.sas   | `DS_translate()`            | R code that calls a DataStep using `dataStep` actionset                 | score\_code\_Logistic Regression.zip |
| Model Studio          | DS2                        | dmcas\_epscorecode.sas | `EPS_translate()`           | R code that calls `astore` actionSet instead of calling the DS2 wrapper  | score\_code\_Gradient Boosting.zip   |
| Visual Text Analytics | Sentiment - CAS Procedure  | scoreCode.sas          | `nlp_sentiment_translate()` | R code that calls the `sentimentAnalysis` actionset                     | SentimentScoreCode.zip               |
| Visual Text Analytics | Categories - CAS Procedure | scoreCode.sas          | `nlp_category_translate()`  | R code that calls `textRuleScore` actionset with `applyCategory` action | CategoriesScoreCode.zip              |
| Visual Text Analytics | Topics - CAS Procedure     | AstoreScoreCode.sas    | `nlp_topics_translate()`    | R code that calls `astore` actionSet                                    | TopicsScoreCode.zip                  |
| Visual Text Analytics | Concepts - CAS Procedure   | ScoreCode.sas          | `nlp_concepts_translate()`  | R code that calls `textRuleScore` actionset with `applyConcept` action  | ConceptsScoreCode.zip                |

## Installation

``` r
# Since it's not on CRAN, you will need to use the remotes package to install directly from github
# install.packages("remotes") # uncomment if you don't have it yet

remotes::install_github("sassoftware/sas-scoring-translator-r") 

# loading the package
library("rsct")
```

## Running

For this example here we've used the `hmeq` dataset available in SAS support [examples datasets](https://support.sas.com/documentation/onlinedoc/viya/examples.htm). You can use this data to generate a model in SAS Model Studio (Gradient Boosting) and export to do this first example, you don't have to unzip the code, but opening it to know which type was exporte may be useful. 

``` r
## For models that generates DS2 code such as Gradient Boosting

## it will show the place where the file was written to

output_infos <- rsct::EPS_translate(in_file = "path/to/score_code_Gradient Boosting.zip",
                         out_file = "path/to/gb_translated.R",
                         in_caslib = "public", 
                         in_castable = "hmeq", 
                         out_caslib = "casuser",
                         out_castable = "hmeq_scored",
                         copyVars = c("BAD", "LOAN"))
```

    ## File successfully written to path/to/gb_translated.R

``` r
# there are some info inside `output_infos` object, but no need to show in here
```

And here is a sample of the generated code, as you will see, credentials
and servers may be needed to be changed manually, other than that, it’s
fully operational:

``` r
## install swat package from github if needed, uncomment OS version
# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.1/R-swat-1.6.1-linux64.tar.gz',repos=NULL, type='file') ## linux
# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.1/R-swat-1.6.1-win64.tar.gz',repos=NULL, type='file') ## windows
# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.1/R-swat-1.6.1-REST-only-osx64.tar.gz',repos=NULL, type='file') ## osx

## Load library
library("swat")

## Defining tables and models variables
in_caslib <- "public"
in_castable <- "hmeq"
out_caslib <- "casuser"
out_castable <- "hmeq_scored"
astore_name <- "_27BOXIOLWOLETZVC66TN25JAF_ast"
astore_file_name <- "_27BOXIOLWOLETZVC66TN25JAF_ast.sashdat"

## Connecting to SAS Viya
conn <- CAS(hostname = "sasserver.com", ## change if needed
                        port = 8777,
                        protocol='http',  ## change protocol to cas and port to 5570 if using binary connection (unix)
                        username='sasusername', ## use your own credentials
                        password='password') ## we encorage using  .authinfo

## Loading model to memory
cas.table.loadTable(conn,
                      caslib= "Models",
                      path = astore_file_name , #case sensitive
                      casOut = list(name = astore_name,
                                    caslib = "Models")
              )

column_names <- c("BAD", "LOAN")

## loading astore actionset and scoring
loadActionSet(conn, "astore")

cas.astore.score(conn,
                   table = list(caslib= in_caslib, name = in_castable),
                   out = list(caslib = out_caslib, name = out_castable, replace = TRUE),
                   copyVars = column_names,
                   rstore = list(name = astore_name, caslib = "Models")
              )

## Obtaining output/results table
scored_table <- defCasTable(conn,
                            tablename = out_castable,
                            caslib = out_caslib)

head(scored_table)
```

Each of the functions follows similar structure but have some different
variables to have consistant naming, due to that, reference the
documentation of the one you want to use to know more about minimun
specifications (Using R helper function: `?nlp_sentiment_translate()`).

``` r
## For models that generates simple DataStep code such as Logistic Regression or Decision Trees
ds <- DS_translate(in_file = "path/to/score_code_Logistic Regression.zip",
                   out_file = "path/to/reg_translated.R",
                   in_caslib = "public", out_caslib = "casuser",
                   in_castable = "hmeq", out_castable = "hmeq_scored")



## For Sentiment model generated from VTA
sentSc <- rsct::sentiment_translate(
                          in_file = "path/to/SentimentScoreCode.zip",
                           out_file = "path/to/sentiment.R",
                           in_caslib = "public", 
                           in_castable = "reports",
                           out_caslib = "public",
                           out_castable_sentiment = "reports_sentiment", 
                           #out_castable_matches, out_castable_features,
                           key_column = "ID", # ID column 
                           document_column = "text", # Text variable column
                           hostname = NULL)

## For Category model generated from VTA

catSc <- rsct::nlp_category_translate(
            in_file = "path/to/CategoriesScoreCode.zip",
            out_file = "path/to/categoryScore.R",
            in_caslib = "public", 
            in_castable = "reports",
            out_caslib = "public",
            out_castable_category = "reports_categories", 
            #out_castable_matches, out_castable_features,
            key_column = "ID", # ID column 
            document_column = "text", # Text variable column
            hostname = NULL)

## For topic model generated from VTA

topSc <- rsct::nlp_topics_translate(in_file = "path/to/TopicsScoreCode.zip",
                           out_file = "path/to/topicsScore.R",
                           in_caslib = "public", out_caslib = "casuser",
                           in_castable = "hmeq",
                           out_castable = "hmeq_scored",
                           copyVars = "ALL")

## For concepts model generated from VTA

conceptSc <- rsct::nlp_concepts_translate(in_file = "path/to/ConceptsScoreCode.zip",
                                    out_file = "path/to/conceptScore.R",
                                    in_caslib = "public", out_caslib = "casuser",
                                    in_castable = "hmeq",
                                    key_column = "ID", # ID column 
                                    document_column = "text", # Text variable column.
                                    out_castable_concepts = "hmeq_scored")
```
## Troubleshooting

Most of the work here assumes that the code is going to be used in the
same server that the model was generated, therefore, not needing to upload astores binaries. Some codes generates the uploading code, but it is not fully implemented yet.

## Contributing

We welcome your contributions! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to submit contributions to this project.

## License

This project is licensed under the [Apache 2.0 License](LICENSE).

## Additional Resources

 - A similar library is available for [Python](https://github.com/sassoftware/sas-scoring-translator-python)
 
 - SAS Communities Post (coming soon)
