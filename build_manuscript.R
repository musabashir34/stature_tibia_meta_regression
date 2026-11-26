## build_manuscript.R
## Builds the formatted Word manuscript in one step, entirely in R.
##
## Usage (from this folder):
##   source("build_manuscript.R")        # in R or RStudio
##   Rscript build_manuscript.R          # from a terminal
##
## Steps:
##   1. Renders article.Rmd, which runs MASTER_analysis_script.R: every model is fitted,
##      every figure (PDF + PNG) and table is written, and every number in the text is
##      computed from the data.
##   2. Sets A4 paper, 2.54 cm margins and continuous line numbers.
##   3. Gives Tables 1 and 2 fixed column widths across the full text width.
##   4. Saves the result as Manuscript_blinded.docx.
##
## Requires R (>= 4.3) with metafor, ggplot2, dplyr, tidyr, forcats, patchwork, knitr,
## rmarkdown and zip, plus pandoc (bundled with RStudio).

needed <- c("metafor", "ggplot2", "dplyr", "tidyr", "forcats", "patchwork",
            "knitr", "rmarkdown", "zip")
## check installation without loading anything into the session
missing <- needed[!nzchar(vapply(needed, function(p) system.file(package = p), character(1)))]
if (length(missing) > 0)
  stop("Install these packages first: install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), ")), then restart R and run ",
       "source(\"build_manuscript.R\") again.")

## UTF-8 text (I-squared, en dashes) in tables and figures
if (.Platform$OS.type == "unix") invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

## Edits the Word file's main XML part: A4 page, 2.54 cm margins, line numbers and,
## for the manuscript, fixed column widths for Tables 1 and 2
finish_docx <- function(in_docx, out_docx, fix_tables = TRUE, line_numbers = TRUE) {
  tmp <- tempfile("docx_")
  dir.create(tmp)
  utils::unzip(in_docx, exdir = tmp)
  doc_xml <- file.path(tmp, "word", "document.xml")
  d <- readChar(doc_xml, file.info(doc_xml)$size, useBytes = TRUE)
  Encoding(d) <- "UTF-8"

  replace_at <- function(x, start, len, value)
    paste0(substr(x, 1, start - 1), value, substr(x, start + len, nchar(x)))

  sect <- gregexpr("(?s)<w:sectPr\\b[^>]*?(/>|>.*?</w:sectPr>)", d, perl = TRUE)[[1]]
  k <- length(sect)
  d <- replace_at(d, sect[k], attr(sect, "match.length")[k], paste0(
    '<w:sectPr><w:pgSz w:w="11906" w:h="16838"/>',
    '<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" ',
    'w:header="708" w:footer="708" w:gutter="0"/>',
    if (line_numbers) '<w:lnNumType w:countBy="1" w:restart="continuous"/>',
    '</w:sectPr>'))

  if (fix_tables) {
    ## text width = 9026 twips
    widths <- list(c(4300, 700, 3000, 1026), c(3300, 500, 900, 1600, 800, 900, 1026))
    tbl <- gregexpr("(?s)<w:tbl>.*?</w:tbl>", d, perl = TRUE)[[1]]
    n_tbl <- min(length(widths), sum(tbl > 0))
    for (i in rev(seq_len(n_tbl))) {
      x <- substr(d, tbl[i], tbl[i] + attr(tbl, "match.length")[i] - 1)
      x <- sub("<w:tblW [^>]*/>",
               '<w:tblW w:type="dxa" w:w="9026" /><w:tblLayout w:type="fixed" />',
               x, perl = TRUE)
      grid <- paste0("<w:tblGrid>",
                     paste0('<w:gridCol w:w="', widths[[i]], '" />', collapse = ""),
                     "</w:tblGrid>")
      x <- sub("(?s)<w:tblGrid>.*?</w:tblGrid>", grid, x, perl = TRUE)
      d <- replace_at(d, tbl[i], attr(tbl, "match.length")[i], x)
    }
  }
  writeChar(d, doc_xml, eos = NULL, useBytes = TRUE)

  out_docx <- file.path(normalizePath("."), out_docx)
  if (file.exists(out_docx)) invisible(file.remove(out_docx))
  parts <- list.files(tmp, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  zip::zip(out_docx, files = parts, root = tmp)
  unlink(tmp, recursive = TRUE)
  if (file.exists(in_docx)) invisible(file.remove(in_docx))
}

## 2. Blinded manuscript for double-anonymized review: no author block or CRediT
##    statement, and the repository identifier withheld
rmarkdown::render("article.Rmd", output_format = "word_document",
                  output_file = "article_blinded.docx", params = list(blind = TRUE),
                  envir = new.env(), quiet = TRUE)
finish_docx("article_blinded.docx", "Manuscript_blinded.docx")

message("Build complete: Manuscript_blinded.docx, figures as PDF and PNG, tables and ",
        "manuscript_key_numbers.csv")
