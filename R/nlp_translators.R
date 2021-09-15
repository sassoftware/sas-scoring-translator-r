# Copyright © 2020, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' NLP Sentiment Generator/Translator
#'
#' It will read the score code that is written as SAS Code extract the language and hostame, 
#' then write an R code equivalent using the `SWAT` package.
#'
#' @param in_file Path to zip/sas file with the score code.
#' @param out_file Path to the translated `.R` code file.
#' @param zip Boolean, if the score code is inside a zip file. Default to `TRUE`. It will look for `dmcas_epscorecode.sas` file inside the zip.
#' @param in_caslib caslib name of the input table
#' @param in_castable input table name
#' @param out_caslib caslib name of the output table
#' @param out_castable_sentiment sentiment output table name
#' @param out_castable_matches matches output table name, if the argument is not defined the table name will be the same as `out_castable_sentiment` with "_matches" added
#' @param out_castable_features features output table name, if the argument is not defined the table name will be the same as `out_castable_sentiment` with "_features" added
#' @param key_column Key column name for unique identifier 
#' @param document_column text variable column name
#' @param hostname `NULL` by default, extracts the server name from scoring code file.
#' @param astore If set to `TRUE`, it will create a scoring code that uploads the astore, available for SAS Viya 2021.1.4 or higher
#' @param astore_caslib Only used when `astore = TRUE`. The caslib where the astore model is uploaded
#' @param astore_name Only used when `astore = TRUE`. The castable name where the astore model is uploaded
#' @param astore_path Only used when `astore = TRUE`. The filepath to the astore file (extract from .zip first)
#' @param copyVars Only used when `astore = TRUE` default `NULL`, will not copy variables of the scoring table to the output. If `"ALL"` will copy all variables to the scored table output, if it is a vector, will copy named vars e.g: `c("var1", "var2)`


#' @return 
#' List object with the Rscore code, out castable, out caslib and the written file path.
#' 
#' @examples
#' nlp_sentiment_translate("filepath.zip")
#' 
#' @export 
#' 


nlp_sentiment_translate <- function(in_file = NULL, 
                                    out_file = "SentimentScoreCode.R",
                                    zip = TRUE,
                                    ## Defining tables and models variables
                                    in_caslib, in_castable,
                                    out_caslib, out_castable_sentiment, 
                                    out_castable_matches, out_castable_features,
                                    key_column, # ID column 
                                    document_column, # Text variable column
                                    hostname = NULL,
                                    astore = FALSE,
                                    astore_caslib = "casuser",
                                    astore_name = "Sentiment_Astore",
                                    astore_path = "SentimentModel.astore",
                                    copyVars = NULL)
{
  
  ### Writing extra output column names
  if(missing(out_castable_sentiment)){
    stop("out_castable_sentiment must be defined.")
  }
  if(missing(out_castable_matches)){
    out_castable_matches <- paste0(out_castable_sentiment, "_matches")
  }
  if(missing(out_castable_features)){
    out_castable_features <- paste0(out_castable_sentiment, "_features")
  }
  
  
  if (is.null(in_file)) stop("Read file must be specified")
  
  ### reading text
  if (zip & !astore) {
    file_con <- unz(in_file, "ScoreCode.sas")
    rawScore <- readLines(file_con)
    close(file_con)
  } 
  if (zip & astore) {
    file_con <- unz(in_file, "AstoreScoreCode.sas")
    rawScore <- readLines(file_con)
    close(file_con)
  }
  if (!zip) {
    rawScore <- readLines(in_file)
  }
  
  Rscore <- c("## install swat package from github if needed, uncomment OS version",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-linux64.tar.gz',repos=NULL, type='file') ## linux",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-win64.tar.gz',repos=NULL, type='file') ## windows",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-REST-only-osx64.tar.gz',repos=NULL, type='file') ## osx",
              "",
              'library("swat")')
  
  ## getting hostname
  
  if(is.null(hostname)){
    hostname <- grep("%let cas_server_hostname", rawScore, value = TRUE)
    hostname <- stringr::str_extract( hostname, '(?<=\")(.*)(?=\")')
  }
  
  ## getting language
  if (!astore){
  language <- grep("%let language", rawScore, value = TRUE)
  language <- stringr::str_extract( language, '(?<=\")(.*)(?=\")')
  }
  ## scorecode APPLY SENT Directly
  
  if (!astore){
  Rscore <- c(Rscore, 
              '',
              ## Defining Variables
              paste0('## Defining tables and models variables'),
              paste0('in_caslib <- ', '\"', in_caslib, '\"'),
              paste0('in_castable <- ', '\"', in_castable, '\"'),
              paste0('out_caslib <- ', '\"', out_caslib, '\"'),
              paste0('out_castable_sentiment <- ', '\"', out_castable_sentiment, '\"'),
              paste0('out_castable_matches <- ', '\"', out_castable_matches, '\"'),
              paste0('out_castable_features <- ', '\"', out_castable_features, '\"'),
              paste0('key_column <- ', '\"', key_column, '\"'),
              paste0('document_column <- ', '\"', document_column, '\"'),
              paste0('language <- ', '\"', language, '\"'),
              
              '',
              ### Writing connection
              paste0("## Connecting to SAS Viya"),
              paste0('conn <- CAS(hostname = \"', hostname, '\", ## change if needed'),
              paste0("\t\t\t\t\t\tport = 8777,"),
              paste0("\t\t\t\t\t\tprotocol='http',  ## change protocol to cas and port to 5570 if using binary connection (unix)"),
              paste0("\t\t\t\t\t\tusername='sasusername', ## use your own credentials"),
              paste0("\t\t\t\t\t\tpassword='password') ## we encorage using  .authinfo"),
              paste0(""),
              
              ## writting score action
              paste0("## loading sentimentAnalysis actionset and scoring"),
              paste0('loadActionSet(conn, \"sentimentAnalysis\")'),
              
              '',
              paste0('cas.sentimentAnalysis.applySent(conn,
                      table = list(caslib = in_caslib, name = in_castable),
                      docId = key_column,
                      text = document_column,
                      language = language,
                      casOut = list(caslib = out_caslib, name = out_castable_sentiment, replace = TRUE),
                      matchOut = list(caslib = out_caslib, name = out_castable_matches, replace = TRUE),
                      featureOut = list(caslib = out_caslib, name = out_castable_features, replace = TRUE),
                    )'),
              
              '',
              paste0("## Obtaining output/results table"),
              paste0('scored_sentiment_table <- defCasTable(conn,
                                      tablename = out_castable_sentiment,
                                      caslib = out_caslib)'),
              '',
              paste0('head(scored_sentiment_table)')
  )
  }
## scorecode Astore upload method

  
  if (astore) {
    
    ##### Will create CopyVars statement
    if(is.null(copyVars)){
      copyVars_ <- "column_names <- NULL"
    }
    
    if(length(copyVars) == 1 && copyVars == "ALL"){
      copyVars_ <- 
        c(paste0("## Defining scoring table obtaining column names"),
          paste0('score_table <- defCasTable(conn,
                             tablename = in_castable,
                             caslib = in_caslib)'),
          '',
          paste0('column_names <- names(score_table)')
        )} 
    else {
      if(length(copyVars) >= 1 ){
        copyVars_ <- 
          paste0('column_names <- c(', paste(paste0('\"', copyVars, '\"'), collapse = ", "), ')')
      }
    }
    
    
    Rscore <- c(Rscore, 
                '',
                ## Defining Variables
                paste0('## Defining tables and models variables'),
                paste0('in_caslib <- ', '\"', in_caslib, '\"'),
                paste0('in_castable <- ', '\"', in_castable, '\"'),
                paste0('out_caslib <- ', '\"', out_caslib, '\"'),
                paste0('out_castable_sentiment <- ', '\"', out_castable_sentiment, '\"'),
                paste0('astore_caslib <- ', '\"', astore_caslib, '\"'),
                paste0('astore_name <- ', '\"', astore_name, '\"'),
                paste0('astore_path <- ', '\"', astore_path, '\" ## same folder/working directory of this file'),
                

                '',
                ### Writing connection
                paste0("## Connecting to SAS Viya"),
                paste0('conn <- CAS(hostname = \"', hostname, '\", ## change if needed'),
                paste0("\t\t\t\t\t\tport = 8777,"),
                paste0("\t\t\t\t\t\tprotocol='http',  ## change protocol to cas and port to 5570 if using binary connection (unix)"),
                paste0("\t\t\t\t\t\tusername='sasusername', ## use your own credentials"),
                paste0("\t\t\t\t\t\tpassword='password') ## we encorage using  .authinfo"),
                paste0(""),
                
                ## copyVars will define castable to get column names if needed
                copyVars_,
                
                '',
                ## Reading astore file
                
                "##Reading astore binary",
                "",
                
                'con <- file(astore_path, \"rb\")',
                "",
                
                'store_ <- readBin(con = con, what = raw(),
                          n = file.size(astore_path)) ', ## n has to be guessed
                'close(con)',
                "",

                
                ### writing score action

                
                "## loading astore actionset and uploading",
                'loadActionSet(conn, \"astore\")',
                
                '',
                ### uploading astore
                paste0('cas.astore.upload(conn,
                   rstore = list(name = astore_name, caslib = astore_caslib),
                   store = astore_path
                  )'),
                
                "",
                "## Scoring table, table assumed to be uploaded",
                ### scoring astore, table assumed to be uploaded
                
                paste0('cas.astore.score(conn,
                   table = list(caslib= in_caslib, name = in_castable),
                   out = list(caslib = out_caslib, name = out_castable_sentiment, replace = TRUE),
                   copyVars = column_names,
                   rstore = list(name = astore_name, caslib = astore_caslib) ## if you uploaded manually, change may be required
                  )'),
              
              '',
              paste0("## Obtaining output/results table"),
              paste0('scored_table <- defCasTable(conn,
                            tablename = out_castable,
                            caslib = out_caslib)'),
              '',
              paste0('head(scored_table)')
    )
  }
  
  ## writting R score code
  writeLines(Rscore, 
             out_file)
  
  
  message(paste0("File successfully written to ", out_file))
  
  ## function output
  list(r_code = Rscore, 
       out_file = out_file, 
       out_caslib = out_caslib, 
       out_castable_sentiment = out_castable_sentiment,
       out_castable_matches = ifelse(astore, "", out_castable_matches),
       out_castable_features = ifelse(astore, "", out_castable_features)
       )
  
}

#' NLP Categories Translator
#'
#' It will read the score code that is written as SAS Code extract the mco binary and hostame information, 
#' then write an R code equivalent using the `SWAT` package.
#'
#' @param in_file Path to zip/sas file with the score code.
#' @param out_file Path to the translated `.R` code file.
#' @param zip Boolean, if the score code is inside a zip file. Default to `TRUE`. It will look for `dmcas_epscorecode.sas` file inside the zip.
#' @param in_caslib caslib name of the input table
#' @param in_castable input table name
#' @param out_caslib caslib name of the output table
#' @param out_castable_category category output table name
#' @param out_castable_matches matches output table name, if the argument is not defined the table name will be the same as `out_castable_category` with "_matches" added
#' @param out_castable_model_table features output table name, if the argument is not defined the table name will be the same as `out_castable_category` with "_modeling" added
#' @param key_column Key column name for unique identifier 
#' @param document_column text variable column name
#' @param hostname `NULL` by default, extracts the server name from scoring code file.
#' @return 
#' List object with the Rscore code, out castable, out caslib and the written file path.
#' 
#' @examples
#' nlp_category_translate("filepath.zip")
#' 
#' @export 
#' 

nlp_category_translate <- function(in_file = NULL, 
                                   out_file = "categoryScoreCode.R",
                                   zip = TRUE,
                                   ## Defining tables and models variables
                                   in_caslib, in_castable,
                                   out_caslib, out_castable_category, 
                                   out_castable_matches, out_castable_model_table,
                                   key_column, # ID column 
                                   document_column, # Text variable column
                                   hostname = NULL)
{
  
  ### Writing extra output column names
  if(missing(out_castable_category)){
    stop("out_castable_category must be defined.")
  }
  if(missing(out_castable_matches)){
    out_castable_matches <- paste0(out_castable_category, "_matches")
  }
  if(missing(out_castable_model_table)){
    out_castable_model_table <- paste0(out_castable_category, "_modeling")
  }
  
  
  if (is.null(in_file)) stop("File must be specified")
  
  ### reading text
  if (zip){
    file_con <- unz(in_file, "ScoreCode.sas")
    rawScore <- readLines(file_con)
    close(file_con)
  } else {
    rawScore <- readLines(in_file)
  }
  
  Rscore <- c("## install swat package from github if needed, uncomment OS version",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-linux64.tar.gz',repos=NULL, type='file') ## linux",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-win64.tar.gz',repos=NULL, type='file') ## windows",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-REST-only-osx64.tar.gz',repos=NULL, type='file') ## osx",
              "",
              'library("swat")')
  
  ## getting hostname
  
  if(is.null(hostname)){
    hostname <- grep("%let cas_server_hostname", rawScore, value = TRUE)
    hostname <- stringr::str_extract( hostname, '(?<=\")(.*)(?=\")')
  }
  
  ## getting mco binary caslib and name
  mco_binary_caslib <- grep("%let mco_binary_caslib", rawScore, value = TRUE)
  mco_binary_caslib <- stringr::str_extract( mco_binary_caslib, '(?<=\")(.*)(?=\")')
  
  mco_binary_name <- grep("%let mco_binary_table_name", rawScore, value = TRUE)
  mco_binary_name <- stringr::str_extract( mco_binary_name, '(?<=\")(.*)(?=\")')
  
  
  score_variables <- c('## Defining tables and models variables',
                       paste0('in_caslib <- ', '\"', in_caslib, '\"'),
                       paste0('in_castable <- ', '\"', in_castable, '\"'),
                       paste0('out_caslib <- ', '\"', out_caslib, '\"'),
                       paste0('out_castable_category <- ', '\"', out_castable_category, '\"'),
                       paste0('out_castable_matches <- ', '\"', out_castable_matches, '\"'),
                       paste0('out_castable_model_table <- ', '\"', out_castable_model_table, '\"'),
                       paste0('key_column <- ', '\"', key_column, '\"'),
                       paste0('document_column <- ', '\"', document_column, '\"'),
                       paste0('mco_binary_caslib <- ', '\"', mco_binary_caslib, '\"'),
                       paste0('mco_binary_table_name <- ', '\"', mco_binary_name, '\"')
  )
  
  ## cas connection
  
  cas_connection <- c("## Connecting to SAS Viya",
                      paste0('conn <- CAS(hostname = \"', hostname, '\", ## change if needed'),
                      "\t\t\t\t\t\tport = 8777,",
                      "\t\t\t\t\t\tprotocol='http',  ## change protocol to cas and port to 5570 if using binary connection (unix)",
                      "\t\t\t\t\t\tusername='sasusername', ## use your own credentials",
                      "\t\t\t\t\t\tpassword='password') ## we encorage using  .authinfo")

  ## scorecode
  Rscore <- c(Rscore, 
              '',
              score_variables,
              '',
              cas_connection,
              '',
              "## loading textRuleScore actionset and scoring",
              'loadActionSet(conn, \"textRuleScore\")',
              
              '',
              'cas.textRuleScore.applyCategory(conn,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tmodel = list(caslib = mco_binary_caslib, name = mco_binary_table_name),',                              
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\ttable = list(caslib = in_caslib, name = in_castable),',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tdocId = key_column,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\ttext = document_column,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tcasOut = list(caslib = out_caslib, name = out_castable_category, replace = TRUE),',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tmatchOut = list(caslib = out_caslib, name = out_castable_matches, replace = TRUE),',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tmodelOut = list(caslib = out_caslib, name = out_castable_model_table, replace = TRUE)',
              ')',
              
              '',
              "## Obtaining output/results table",
              'scored_category_table <- defCasTable(conn,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\ttablename = out_castable_category,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tcaslib = out_caslib)',
              '',
              'head(scored_category_table)'
              
  )
  
  ## writting R score code
  writeLines(Rscore, 
             out_file)
  
  
  message(paste0("File successfully written to ", out_file))
  
  ## function output
  list(r_code = Rscore, 
       out_file = out_file, 
       out_caslib = out_caslib, 
       out_castable_category = out_castable_category,
       out_castable_matches = out_castable_matches,
       out_castable_model_table = out_castable_model_table)
  
}


#' NLP Topics Translator
#'
#' It will read the score code that is written as SAS Code extract the astore and hostame information, 
#' then write an R code equivalent in `SWAT`.
#'
#' @param in_file Path to zip/sas file with the score code.
#' @param out_file Path to the translated `.R` code file.
#' @param zip Boolean, if the score code is inside a zip file. Default to `TRUE`. It will look for `dmcas_epscorecode.sas` file inside the zip.
#' @param in_caslib caslib name of the input table
#' @param in_castable input table name
#' @param out_caslib caslib name of the output table
#' @param out_castable name of the output table
#' @param copyVars default `NULL`, will not copy variables to the output. If `"ALL"` will copy all variables to the scored table output, if it is a vector, will copy named vars e.g: `c("var1", "var2)`
#' @param hostname `NULL` by default, extracts the server name from scoring code file.
#' @return 
#' List object with the Rscore code, out castable, out caslib and the written file path.
#' 
#' @examples
#' nlp_topics_translate("filepath.zip")
#' 
#' @export 
#' 

nlp_topics_translate <- function(in_file = NULL, 
                                                  out_file = "topicsScoreCode.R",
                                                  zip = TRUE,
                                                  in_caslib, 
                                                  in_castable,
                                                  out_caslib, 
                                                  out_castable,
                                                  hostname = NULL,
                                                  copyVars = NULL) {
  
  if (is.null(in_file)) stop("File must be specified")
  
  if (zip){
    file_con <- unz(in_file, "AstoreScoreCode.sas")
    rawScore <- readLines(file_con)
    close(file_con)
  } else {
    rawScore <- readLines(in_file)
  }
  
  ## getting hostname
  
  if(is.null(hostname)){
    hostname <- grep("%let cas_server_hostname", rawScore, value = TRUE)
    hostname <- stringr::str_extract( hostname, '(?<=\")(.*)(?=\")')
  }
  
  ## getting astore binary caslib and name
  astore_caslib <- grep("%let input_astore_caslib_name", rawScore, value = TRUE)
  astore_caslib <- stringr::str_extract( astore_caslib, '(?<=\")(.*)(?=\")')
  
  astore_name <- grep("%let input_astore_name", rawScore, value = TRUE)
  astore_name <- stringr::str_extract( astore_name, '(?<=\")(.*)(?=\")')
  
  
  Rscore <- c("## install swat package from github if needed, uncomment OS version",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-linux64.tar.gz',repos=NULL, type='file') ## linux",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-win64.tar.gz',repos=NULL, type='file') ## windows",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-REST-only-osx64.tar.gz',repos=NULL, type='file') ## osx",
              "",
              "## Load library",
              'library("swat")')
  
  ## creating copyVars acording to the input
  if(is.null(copyVars)){
    copyVars_ <- "column_names <- NULL"
  }
  
  if(length(copyVars) == 1 && copyVars == "ALL"){
    copyVars_ <- 
      c(paste0("## Defining scoring table obtaining column names"),
        paste0('score_table <- defCasTable(conn,
                             tablename = in_castable,
                             caslib = in_caslib)'),
        '',
        paste0('column_names <- names(score_table)')
      )} 
  else {
    if(length(copyVars) >= 1 ){
    copyVars_ <- 
      paste0('column_names <- c(', paste(paste0('\"', copyVars, '\"'), collapse = ", "), ')')
  }
  }
  
  
  Rscore <- c(Rscore, 
              '',
              ## Defining Variables
              
              '## Defining tables and models variables',
              paste0('in_caslib <- ', '\"', in_caslib, '\"'),
              paste0('in_castable <- ', '\"', in_castable, '\"'),
              paste0('out_caslib <- ', '\"', out_caslib, '\"'),
              paste0('out_castable <- ', '\"', out_castable, '\"'),
              paste0('astore_caslib <- ', '\"', astore_caslib, '\"'),
              paste0('astore_name <- ', '\"', astore_name, '\"'),

              '',
              ### Writing connection
              "## Connecting to SAS Viya",
              paste0('conn <- CAS(hostname = \"', hostname, '\", ## change if needed'),
              "\t\t\t\t\t\tport = 8777,",
              "\t\t\t\t\t\tprotocol='http',  ## change protocol to cas and port to 5570 if using binary connection (unix)",
              "\t\t\t\t\t\tusername='sasusername', ## use your own credentials",
              "\t\t\t\t\t\tpassword='password') ## we encorage using  .authinfo",
              
              '',
              ### Loading astore table into memory (astore should already be inside server)
              "## If Uploading model to a new server uncomment this section and add correct filepath",
              '',
              paste0('#cas.table.loadTable(conn,
                      #caslib= "Models",
                      #path = "/path/to/TopicsModel.astore" , #case sensitive
                      #casOut = list(name = astore_name,
                      #              caslib = "Models") ## change caslib as well if needed
              #)'),
              '',
              
              ## copyVars will define castable to get column names if needed
              copyVars_,
              
              '',
              
              ### writing score action
              
              "## loading astore actionset and scoring",
              'loadActionSet(conn, \"astore\")',
              
              '',
              paste0('cas.astore.score(conn,
                   table = list(caslib= in_caslib, name = in_castable),
                   out = list(caslib = out_caslib, name = out_castable, replace = TRUE),
                   copyVars = column_names,
                   rstore = list(name = astore_name, caslib = astore_caslib) ## if you uploaded manually, change may be needed
              )'),
              
              '',
              paste0("## Obtaining output/results table"),
              paste0('scored_table <- defCasTable(conn,
                            tablename = out_castable,
                            caslib = out_caslib)'),
              '',
              paste0('head(scored_table)')
              
  )
  
  ## writting R score code
  writeLines(Rscore, 
             out_file)
  
  
  message(paste0("File successfully written to ", out_file))
  
  ## function output
  list(r_code = Rscore, 
       out_file = out_file, 
       out_caslib = out_caslib, 
       out_castable = out_castable)
  
}

#' NLP Concepts Translator
#'
#' It will read the score code that is written as SAS Code extract the liti binary and hostame information, 
#' then write an R code equivalent in `SWAT`.
#'
#' @param in_file Path to zip/sas file with the score code.
#' @param out_file Path to the translated `.R` code file.
#' @param zip Boolean, if the score code is inside a zip file. Default to `TRUE`. It will look for `dmcas_epscorecode.sas` file inside the zip.
#' @param in_caslib caslib name of the input table
#' @param in_castable input table name
#' @param out_caslib caslib name of the output table
#' @param out_castable_concepts sentiment output table name
#' @param out_castable_facts matches output table name, if the argument is not defined the table name will be the same as `out_castable_concepts` with "_facts" added
#' @param key_column Key column name for unique identifier 
#' @param document_column text variable column name
#' @param hostname `NULL` by default, extracts the server name from scoring code file.
#' @return 
#' List object with the Rscore code, out castable, out caslib and the written file path.
#' 
#' @examples
#' nlp_category_translate("filepath.zip")
#' 
#' @export 
#' 

nlp_concepts_translate <- function(in_file = NULL, 
                                   out_file = "categoryScoreCode.R",
                                   zip = TRUE,
                                   ## Defining tables and models variables
                                   in_caslib, in_castable,
                                   out_caslib, 
                                   out_castable_concepts, 
                                   out_castable_facts, 
                                   key_column, # ID column 
                                   document_column, # Text variable column
                                   hostname = NULL)
{
  
  ### Writing extra output column names
  if(missing(out_castable_concepts)){
    stop("out_castable_concepts must be defined.")
  }
  if(missing(out_castable_facts)){
    out_castable_facts <- paste0(out_castable_concepts, "_facts")
  }

  
  if (is.null(in_file)) stop("File must be specified")
  
  ### reading text
  if (zip){
    file_con <- unz(in_file, "ScoreCode.sas")
    rawScore <- readLines(file_con)
    close(file_con)
  } else {
    rawScore <- readLines(in_file)
  }
  
  Rscore <- c("## install swat package from github if needed, uncomment OS version",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-linux64.tar.gz',repos=NULL, type='file') ## linux",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-win64.tar.gz',repos=NULL, type='file') ## windows",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-REST-only-osx64.tar.gz',repos=NULL, type='file') ## osx",
              "",
              'library("swat")')
  
  ## getting hostname
  
  if(is.null(hostname)){
    hostname <- grep("%let cas_server_hostname", rawScore, value = TRUE)
    hostname <- stringr::str_extract( hostname, '(?<=\")(.*)(?=\")')
  }
  
  ## getting mco binary caslib and name
  liti_binary_caslib <- grep("%let liti_binary_caslib", rawScore, value = TRUE)
  liti_binary_caslib <- stringr::str_extract( liti_binary_caslib, '(?<=\")(.*)(?=\")')
  
  liti_binary_table_name <- grep("%let liti_binary_table_name", rawScore, value = TRUE)
  liti_binary_table_name <- stringr::str_extract( liti_binary_table_name, '(?<=\")(.*)(?=\")')

  
  score_variables <- c('## Defining tables and models variables',
                       paste0('in_caslib <- ', '\"', in_caslib, '\"'),
                       paste0('in_castable <- ', '\"', in_castable, '\"'),
                       paste0('out_caslib <- ', '\"', out_caslib, '\"'),
                       paste0('out_castable_concepts <- ', '\"', out_castable_concepts, '\"'),
                       paste0('out_castable_facts <- ', '\"', out_castable_facts, '\"'),
                       paste0('key_column <- ', '\"', key_column, '\"'),
                       paste0('document_column <- ', '\"', document_column, '\"'),
                       paste0('liti_binary_caslib <- ', '\"', liti_binary_caslib, '\"'),
                       paste0('liti_binary_table_name <- ', '\"', liti_binary_table_name, '\"')
  )
  
  ## cas connection
  
  cas_connection <- c("## Connecting to SAS Viya",
                      paste0('conn <- CAS(hostname = \"', hostname, '\", ## change if needed'),
                      "\t\t\t\t\t\tport = 8777,",
                      "\t\t\t\t\t\tprotocol='http', ## change protocol to cas and port to 5570 if using binary connection (unix)",
                      "\t\t\t\t\t\tusername='sasusername', ## use your own credentials",
                      "\t\t\t\t\t\tpassword='password') ## we encorage using  .authinfo")
  
  ## scorecode
  Rscore <- c(Rscore, 
              '',
              score_variables,
              '',
              cas_connection,
              '',
              "## loading textRuleScore actionset and scoring",
              'loadActionSet(conn, \"textRuleScore\")',
              
              '',
              'cas.textRuleScore.applyConcept(conn,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tmodel = list(caslib = liti_binary_caslib, name = liti_binary_table_name),',                              
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\ttable = list(caslib = in_caslib, name = in_castable),',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tdocId = key_column,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\ttext = document_column,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tcasOut = list(caslib = out_caslib, name = out_castable_concepts, replace = TRUE),',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tfactOut = list(caslib = out_caslib, name = out_castable_facts, replace = TRUE)',
              ')',
              
              '',
              "## Obtaining output/results table",
              'scored_concepts_table <- defCasTable(conn,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\ttablename = out_castable_concepts,',
              '\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tcaslib = out_caslib)',
              '',
              'head(scored_concepts_table)'
              
  )
  
  ## writting R score code
  writeLines(Rscore, 
             out_file)
  
  
  message(paste0("File successfully written to ", out_file))
  
  ## function output
  list(r_code = Rscore, 
       out_file = out_file, 
       out_caslib = out_caslib, 
       out_castable_concepts = out_castable_concepts,
       out_castable_facts = out_castable_facts)
  
}

