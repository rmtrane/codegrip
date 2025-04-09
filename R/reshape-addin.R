#' Reshape expressions longer or wider
#'
#' @description `addin_reshape()` lets you cycle between different shapes of
#'   function calls. For instance, reshaping transforms code from wide to long
#'   shape and vice versa:
#'   ```
#'   list(a, b, c)
#'
#'   list(
#'     a,
#'     b,
#'     c
#'   )
#'   ```
#'   Note that for function definitions, `addin_reshape()` cycles through two
#'   different long shapes. The traditional L form uses more horizontal space
#'   whereas the flat form uses less horizontal space and the arguments are
#'   always aligned at single indent:
#'   ```
#'   foo <- function(a, b, c) {
#'     NULL
#'   }
#'
#'   foo <- function(a,
#'                   b,
#'                   c) {
#'     NULL
#'   }
#'
#'   foo <- function(
#'     a,
#'     b,
#'     c
#'   ) {
#'     NULL
#'   }
#'   ```
#' @export
addin_reshape <- function() {
  tryCatch(
    addin_reshape_unsafe(),
    error = function(...) NULL
  )
}

addin_reshape_unsafe <- function() {
  context <- rstudioapi::getActiveDocumentContext()

  # Check if file is .qmd or .rmd. First, by file extension of file path.
  path <- context$path
  ext <- tolower(tools::file_ext(path))

  if (ext == "qmd" | ext == "rmd") {
    qmd_or_rmd <- TRUE
  } else {
    qmd_or_rmd <- FALSE
  }

  # If extension is empty, see if contents start with '---' and contains at
  # least one start of code cell (i.e. "```{r") and one end of code cell
  # (i.e. "```").
  if (ext == "") {
    qmd_or_rmd <- context$contents[1] == "---" & any(grepl(pattern = "^```\\{r", x = context$contents)) & any(context$contents == "```")
  }

  lines <- context$contents
  sel <- context$selection[[1]]$range

  # No reshaping for selections
  if (!identical(sel$start, sel$end)) {
    return()
  }

  line <- sel$start[[1]]
  col <- sel$start[[2]]

  # Adjustments for Quarto and Rmarkdown files
  if (qmd_or_rmd) {
    # Get start and end lines for code cells (similar to
    # quarto::quarto_inspect(...)$fileInformation[[1]]$codeCells, but want
    # to avoid dependency on quarto R package + want this to work for unsaved
    # files.)
    code_cells <- data.frame(
      start = grep(pattern = "^```\\{r", x = context$contents),
      end = grep(pattern = "^```$", x = context$contents)
    )

    # Find cell cursor is in.
    cur_cell <- code_cells[code_cells$start <= line & line <= code_cells$end, c("start", "end")]

    # Get lines of cell only.
    lines <- lines[cur_cell$start:cur_cell$end]
    # "Delete" rows that start with "#|" (i.e. code block options) or "`"
    # (i.e. start/end of code block)
    lines[grepl(pattern = "^[#\\||`]", x = lines, fixed = F)] <- ""

    # Adjust line to refer to position in code block
    line <- line - cur_cell$start + 1
  }

  parse_info <- parse_info(lines = lines)
  out <- reshape_info(line, col, info = parse_info)

  # Adjustments for Quarto and Rmarkdown files part 2
  if (qmd_or_rmd) {
    # Adjust start/end line to refer to entire document instead of code block.
    out$start[["line"]] <- out$start[["line"]] + cur_cell$start - 1
    out$end[["line"]] <- out$end[["line"]] + cur_cell$start - 1
  }

  pos1 <- rstudioapi::document_position(out$start[["line"]], out$start[["col"]])
  pos2 <- rstudioapi::document_position(out$end[["line"]], out$end[["col"]])
  range <- rstudioapi::document_range(pos1, pos2)

  rstudioapi::modifyRange(range, out$reshaped)

  ## Set cursor position to start of newly formatted code rather than the initially selected position.
  rstudioapi::setCursorPosition(rstudioapi::document_position(out$start[["line"]], out$start[["col"]]))
}
