#' Read JHU Transcript
#'
#' @param pdf_file path to the filename
#'
#' @return A \code{data_frame} of the data
#' @export
#'
#' @importFrom pdftools pdf_text
#' @importFrom dplyr mutate select filter recode bind_cols bind_rows
#' @importFrom dplyr as_data_frame data_frame %>%
#' @importFrom tidyr separate
#' @importFrom zoo na.locf
#' @examples
#' pdf_file = "~/Downloads/Transcript.pdf"
#' res = read_jhu_transcript(pdf_file)
read_jhu_transcript = function(pdf_file) {
  pdf_file = path.expand(pdf_file)
  pdf_file = normalizePath(pdf_file)
  res = pdf_text(pdf_file)
  # dat = readPDF(
  #   control = list(text="-layout -fixed 4"),
  #   engine = "xpdf")(
  #     elem = list(uri = path.expand(pdf_file)),
  #     language="en", id="id1")
  # dat = c(as.character(dat))


  # split_up = strsplit(res, split = "\n")
  # split_up = unlist(split_up)
  # max(nchar(split_up, type = "width"))

  res = trimws(res)
  res = strsplit(res, split = "\n")

  # res = lapply(res, trimws)
  x = res[[1]]
  res = lapply(res, function(x) {
    start = grep(pattern = "^COURSE NUMBER", trimws(x))
    start = start[length(start)]
    end = grep("^[***]", trimws(x))[1]
    if (length(end) == 0) {
      end = length(x)
    }
    stopifnot(length(start) == 1 && length(end) == 1)
    ind = seq(start + 1, end - 1)
    x = x[ind]
  })

  width = 109
  r = res[[2]]

  ss = sapply(res, function(r) {
    r = sapply(r, function(x){
      nc = nchar(x)
      x = c(substr(x, 1, min(width, nc)),
            ifelse(nc > width + 1, substr(x, width + 1, nc),
                   ""))
      trimws(x)
    })
    r = t(r)
  })

  ss = do.call(rbind, ss)
  rownames(ss) = NULL

  ss = c(ss[,1], ss[,2])
  ss = trimws(ss)

  ss = ss[ ss != "" ]

  rem_ind = grepl("^GPA CRS", ss)
  if (any(rem_ind)) {
    remove = ss[rem_ind]
    end_correct = grepl("CRS: \\d*[.]\\d*$", remove)
    if (!all(end_correct)) {
      warning("May have removed records")
    }
    ss = ss[!rem_ind]
  }



  advisor_ind = grep("Advisor History", ss)
  if (length(advisor_ind) > 0) {
    award = grep("Awarded", ss)
    if (length(award) > 0) {
      award = award[ award > advisor_ind]
      award = max(award)
    } else {
      # may be wrong
      award = length(ss)
    }
    advisor = ss[ seq(advisor_ind, award)]
    ss = ss[ -seq(advisor_ind, award)]
  }

  ss = data_frame(x = ss)

  term_str = "^2\\d*-\\d* .* Term   "
  ss = ss %>%
    mutate(is_term = grepl(term_str, x),
           term = ifelse(is_term, x, NA_character_),
           term = na.locf(term, na.rm = FALSE)) %>%
    filter(!is_term) %>%
    select(-is_term)

  x = strsplit(ss$x, split = "   ")
  x = lapply(x, function(r) {
    r = r[ r != ""]
    r = trimws(r)
    r
  })
  lengths = sapply(x, length)
  stopifnot(all(lengths == 4))

  x = do.call("rbind", x)
  rownames(x) = NULL
  colnames(x) = c("course_number", "course_title", "grade", "credits")


  x = as_data_frame(x)
  ss = bind_cols(ss, x)
  ss$x = NULL
  ss = ss %>%
    mutate(credits = as.numeric(credits))
  term = strsplit(ss$term, split = "   ")
  term = sapply(term, function(r) {
    r = r[ r != ""]
    r = trimws(r)
    r
  })
  term = t(term)
  colnames(term) = c("term", "program")
  ss$term = NULL
  term = as_data_frame(term)
  ss = bind_cols(ss, term)

  ss = ss %>%
    separate(term, into = c("year", "term", "blah"), sep = " ") %>%
    select(-blah) %>%
    mutate(term = recode(term,
                         "First" = "1",
                         "Second" = "2",
                         "Third" = "3",
                         "Fourth" = "4"),
           term = as.numeric(term))

  return(ss)

}

#' @rdname read_jhu_transcript
#' @param type Type of Transcript
#' @export
read_transcript = function(pdf_file, type = "jhu") {
  type = match.arg(type)
  func = switch(type,
                "jhu" = read_jhu_transcript
  )
  func(pdf_file)
}
