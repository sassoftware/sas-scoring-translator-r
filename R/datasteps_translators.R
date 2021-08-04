# Copyright © 2020, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' DataStep Wrapper
#'
#' Wraps a simple DataSetp code (not DS2) to be run through `SWAT`
#'
#' @param in_file Path to zip/sas file with the score code.
#' @param out_file Path to the translated `.R` code file.
#' @param zip Boolean, if the score code is inside a zip file. Default to `TRUE`. It will look for `dmcas_scorecode.sas` file inside the zip.
#' @param in_caslib caslib name of the input table
#' @param in_castable input table name
#' @param out_caslib caslib name of the output table
#' @param out_castable output table name
#' @param hostname `NULL` by default, extracts the server name from `dmcas_scorecode.sas` file.
#' 
#' @return
#' List object with the DS code, Rscore code, out castable, out caslib and the written file path.
#'
#' @examples
#' DS_translate("filepath.zip")
#' 
#' @export 

DS_translate <- function(in_file = NULL, 
                         out_file = "dmcas_scorecode.R",
                         zip = TRUE,
                         hostname = NULL,
                         in_caslib, in_castable,
                         out_caslib, out_castable) {
  
  if (is.null(in_file)) stop("File must be specified")

  if (zip){
    file_con <- unz(in_file, "dmcas_scorecode.sas")
    rawScore <- readLines(file_con)
    close(file_con)
  } else {
    rawScore <- readLines(in_file)
  }
    
    ## Adding input_output to datastep
    DSScore <- paste0("data ", out_caslib, ".", out_castable,";")
    DSScore <- c(DSScore,
                 paste0("    set ", in_caslib, ".", in_castable,";")
    )
    
    DSScore <- c(DSScore,  paste0("    ", rawScore))
    
    ## finalizing datastep
    DSScore <- c(DSScore, "run;")
    
    ## replacing " due to char needs
    DSScore <- gsub('"', "'", DSScore)

    
    Rscore <- c("## install swat package from github if needed, uncomment OS version",
                "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-linux64.tar.gz',repos=NULL, type='file') ## linux",
                "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-win64.tar.gz',repos=NULL, type='file') ## windows",
                "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-REST-only-osx64.tar.gz',repos=NULL, type='file') ## osx",
                "",
                'library("swat")')
    
    ## getting hostname
    if ( is.null(hostname) ) {
    hostname <- grep("\\* Host", rawScore, value = TRUE)
    hostname <- gsub("\\* Host:\\s+|;", "", hostname)
    }
    
    Rscore <- c(Rscore, 
                '',
    ### Writing connection
        paste0("## Connecting to SAS Viya"),
        paste0('conn <- CAS(hostname = \"', hostname, '\", ## change if needed'),
        paste0("\t\t\t\t\t\tport = 8777,"),
        paste0("\t\t\t\t\t\tprotocol='http',  ## change protocol to cas and port to 5570 if using binary connection (unix)"),
        paste0("\t\t\t\t\t\tusername='sasusername', ## use your own credentials"),
        paste0("\t\t\t\t\t\tpassword='password') ## we encorage using  .authinfo"),
        '',
    ### writing runCode
    
        paste0("out <- cas.dataStep.runCode(conn,"),
        paste0('\t\t\tcode = \"' ),
        DSScore,
        paste0('\"'),
        paste0(")"),
        '',
        '',
    
    ### commented drop table for help
    
    paste0("### uncomment following lines if you want to drop previous table"),
    "", 
    paste0("#cas.table.dropTable(conn,"),
    paste0("#\t\t\t\t\t\t\t\t\t\t\tcaslib = \"", out_caslib, "\","),
    paste0("#\t\t\t\t\t\t\t\t\t\t\tname = \"", out_castable, "\")"),
    "",
    "",
    ### getting output
    paste0("## Obtaining output/results table"),
    "", 
    paste0("scored_table <- defCasTable(conn,"),
    paste0("\t\t\t\t\t\t\t\tcaslib = \"", out_caslib, "\","),
    paste0("\t\t\t\t\t\t\t\ttablenamename = \"", out_castable, "\")"),
    '',
    paste0('head(scored_table)'),
    "",
    "",
    ### writing table promote for persistence
        paste0("## will fail if there is already a promoted table with the same name"),
        paste0("cas.table.promote(conn,"),
        paste0('\t\t\t\t\t\t\t\t\tcaslib = \"', out_caslib, '\",'),
        paste0('\t\t\t\t\t\t\t\t\tname = \"', out_castable, 
               '\"\n\t\t\t\t\t\t\t\t\t)')
    
    )
    
    ## writting R score code
    writeLines(Rscore, 
               out_file)
    
    
    message(paste0("File successfully written to ", out_file))
    
    return(list(data_step = DSScore,
                r_code = Rscore,
                out_caslib = out_caslib, 
                out_castable = out_castable)
           )
    
    
}

#' EPS Translator
#'
#' It will read the score code that is written in DS2, extract the astore name and
#' create an astore call written using `SWAT`. The reason for that is because the DS2 is
#' not reliable/self contained
#'
#' @param in_file Path to zip/sas file with the score code.
#' @param out_file Path to the translated `.R` code file.
#' @param zip Boolean, if the score code is inside a zip file. Default to `TRUE`. It will look for `dmcas_epscorecode.sas` file inside the zip.
#' @param in_caslib caslib name of the input table
#' @param in_castable input table name
#' @param out_caslib caslib name of the output table
#' @param out_castable output table name
#' @param hostname sas viya hostname to be used, not available inside the DS2 code
#' @param copyVars default `NULL`, will not copy variables to the output. If `"ALL"` will copy all variables to the scored table output, if it is a vector, will copy named vars e.g: `c("var1", "var2)` 
#' @return 
#' List object with the Rscore code, out castable, out caslib and the written file path.
#' 
#' @examples
#' EPS_translate("filepath.zip")
#' 
#' @export 

EPS_translate <- function(in_file = NULL, 
                          out_file = "dmcas_epscorecode.R",
                          zip = TRUE,
                          in_caslib, in_castable,
                          out_caslib, out_castable,
                          hostname = "myserver.com",
                          copyVars = NULL) {
  
  if (is.null(in_file)) stop("File must be specified")
  
  if (zip){
    file_con <- unz(in_file, "dmcas_epscorecode.sas")
    rawScore <- readLines(file_con)
    close(file_con)
  } else {
    rawScore <- readLines(in_file)
  }
  
  astore_row <- grep("_ast", rawScore, value = TRUE)
  astore_name <- gsub(" \\*\\s+", "", astore_row)

  Rscore <- c("## install swat package from github if needed, uncomment OS version",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-linux64.tar.gz',repos=NULL, type='file') ## linux",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-win64.tar.gz',repos=NULL, type='file') ## windows",
              "# install.packages('https://github.com/sassoftware/R-swat/releases/download/v1.6.2/R-swat-1.6.2-REST-only-osx64.tar.gz',repos=NULL, type='file') ## osx",
              "",
              "## Load library",
              'library("swat")')
  
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

  if(length(copyVars) > 1 ){
    copyVars_ <- 
        paste0('column_names <- c(', paste(paste0('\"', copyVars, '\"'), collapse = ", "), ')')
      }
  
  
  
  Rscore <- c(Rscore, 
              '',
              ## Defining Variables
              
              paste0('## Defining tables and models variables'),
              paste0('in_caslib <- ', '\"', in_caslib, '\"'),
              paste0('in_castable <- ', '\"', in_castable, '\"'),
              paste0('out_caslib <- ', '\"', out_caslib, '\"'),
              paste0('out_castable <- ', '\"', out_castable, '\"'),
              paste0('astore_name <- ', '\"', astore_name, '\"'),
              paste0('astore_file_name <- ', '\"',paste0(astore_name, '.sashdat'), '\"'),
              
              '',
              ### Writing connection
              paste0("## Connecting to SAS Viya"),
              paste0('conn <- CAS(hostname = \"', hostname, '\", ## change if needed'),
              paste0("\t\t\t\t\t\tport = 8777,"),
              paste0("\t\t\t\t\t\tprotocol='http',  ## change protocol to cas and port to 5570 if using binary connection (unix)"),
              paste0("\t\t\t\t\t\tusername='sasusername', ## use your own credentials"),
              paste0("\t\t\t\t\t\tpassword='password') ## we encorage using  .authinfo"),
              '',
              ### Loading astore table into memory (astore should already be inside server)
              paste0("## Loading model to memory"),
              paste0('cas.table.loadTable(conn,
                      caslib= "Models",
                      path = astore_file_name , #case sensitive
                      casOut = list(name = astore_name,
                                    caslib = "Models")
              )'),
              '',
              ### obtaining scoring columns
              # paste0("## Defining scoring table obtaining column names"),
              # paste0('score_table <- defCasTable(conn,
              #                tablename = in_castable,
              #                caslib = in_caslib)'),
              # '',
              # 
              # paste0('column_names <- names(score_table)'),
              # 
              copyVars_,
              '',
              
              ### writing score action
              
              paste0("## loading astore actionset and scoring"),
              paste0('loadActionSet(conn, \"astore\")'),
              
              '',
              paste0('cas.astore.score(conn,
                   table = list(caslib= in_caslib, name = in_castable),
                   out = list(caslib = out_caslib, name = out_castable, replace = TRUE),
                   copyVars = column_names,
                   rstore = list(name = astore_name, caslib = "Models")
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


