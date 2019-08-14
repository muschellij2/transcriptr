#' Read JHU Transcript
#'
#' @param file path to the filename
#' @param remove_withdrawn should remove withdrawn courses
#'
#' @return A \code{tibble} of the data
#' @export
#'
#' @importFrom pdftools pdf_text
#' @importFrom dplyr mutate select filter recode bind_cols bind_rows
#' @importFrom dplyr as_tibble tibble %>% first
#' @importFrom tidyr separate
#' @importFrom zoo na.locf
#' @examples
#' file = "~/Downloads/Transcript.pdf"
#' if (file.exists(file)) {
#'     res = read_jhu_transcript(file)
#' }
read_jhu_transcript = function(
  file,
  remove_withdrawn = TRUE) {
  pdf_file = path.expand(file)
  pdf_file = normalizePath(pdf_file)
  res = pdf_text(pdf_file)

  credits = blah = is_term = term = NULL
  rm(list = c("credits", "blah", "is_term", "term"))

  grade = NULL
  rm(list = c("grade"))
  # dat = readPDF(
  #   control = list(text="-raw"),
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
    l = list(data = x[ind], other = x[-ind])
  })


  other = lapply(res, function(x) {
    x$other
  })

  res = lapply(res, function(x) {
    x$data
  })

  width = 109
  # r = res[[2]]

  cutter = function(x){
    nc = nchar(x)
    x = c(substr(x, 1, min(width, nc)),
          ifelse(nc > width + 1, substr(x, width + 1, nc),
                 ""))
    trimws(x)
  }

  # widths = c(11, 40, 5, 10)
  # cs = cumsum(widths)
  # cutter2 = function(x){
  #   x = trimws(x)
  #   x = gsub("\\s+", " ", x)
  #   nc = nchar(x)
  #   x = c(
  #     substr(x, 1, min(cs[1], nc)),
  #     ifelse(nc >=  cs[2],
  #            substr(x, cs[1] + 1, min(nc, cs[2])),
  #            ""),
  #     ifelse(nc >= cs[3],
  #            substr(x, cs[2] + 1, min(nc, cs[3])),
  #            ""),
  #     ifelse(nc >= cs[4],
  #            substr(x, cs[3] + 1, max(nc, cs[4])),
  #            "")
  #     )
  #   trimws(x)
  # }
  #
  # ss2 = lapply(res, function(r) {
  #   r = sapply(r, cutter2)
  #   r = t(r)
  # })
  # ss2 = do.call(rbind, ss2)
  # rownames(ss2) = NULL

  ss = lapply(res, function(r) {
    r = sapply(r, cutter)
    r = t(r)
  })

  other = lapply(other, function(r) {
    r = sapply(r, cutter)
    r = t(r)
  })

  ss = do.call(rbind, ss)
  rownames(ss) = NULL



  other = do.call(rbind, other)
  rownames(other) = NULL

  other = c(other[,1], other[,2])
  other = trimws(other)
  other = other[ other != "" ]
  stud_name = grep("student\\s+name", tolower(other))[1]
  stud_name = other[stud_name + 1]
  stud_name = strsplit(stud_name, split = "   ")[[1]]
  stud_name = trimws(stud_name)
  stud_name = stud_name[ stud_name != "" ]
  stud_name = stud_name[1:2]
  names(stud_name) = c("student_name", "student_id")


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
    # remove after award
    # ss = ss[ -seq(advisor_ind, award)]
    ss = ss[ -seq(advisor_ind, length(ss))]
  } else {
    advisor = NULL
  }

  ss = dplyr::tibble(x = ss)

  term_str = "^2\\d*-\\d* .*(Winter|Summer|Term)   "
  ss = ss %>%
    mutate(is_term = grepl(term_str, x),
           term = ifelse(is_term, x, NA_character_),
           term = zoo::na.locf(term, na.rm = FALSE)) %>%
    filter(!is_term) %>%
    select(-is_term)

  x = strsplit(ss$x, split = "   ")
  x = lapply(x, function(r) {
    r = r[ r != ""]
    r = trimws(r)
    r
  })
  lengths = sapply(x, length)
  if (!all(lengths == 4)) {
    warning("Not all data may be retrieved!")
    message(ss[lengths != 4,])
    x = x[ lengths == 4 ]
    ss = ss[ lengths == 4, ]
    lengths = sapply(x, length)
  }
  stopifnot(all(lengths == 4))

  x = do.call("rbind", x)
  rownames(x) = NULL
  colnames(x) = c("course_number", "course_title", "grade", "credits")


  x = as_tibble(x)
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
  term = dplyr::as_tibble(term)
  ss = bind_cols(ss, term)

  grade = ss$grade
  grade = strsplit(grade, split = " ")
  grade = sapply(grade, dplyr::first)
  ss$grade = grade

  ss = ss %>%
    separate(term, into = c("year", "term", "blah"), sep = " ") %>%
    select(-blah) %>%
    mutate(term = recode(term,
                         "First" = "1",
                         "Second" = "2",
                         "Third" = "3",
                         "Fourth" = "4"),
           term = as.numeric(term))
  if (remove_withdrawn) {
    ss = ss %>%
      filter( !(grade %in% "W"))
  }

  if (!is.null(advisor)) {
    attr(ss, "advisor_info") = advisor
  }
  attr(ss, "student_info") = stud_name

  return(ss)

}


# read_jhu_transcript2 = function(file) {
#   pdf_file = path.expand(file)
#   pdf_file = normalizePath(pdf_file)
#
#   credits = blah = is_term = term = NULL
#   rm(list = c("credits", "blah", "is_term", "term"))
#   dat = tm::readPDF(
#     control = list(text="-raw"),
#     engine = "xpdf")(
#       elem = list(uri = path.expand(pdf_file)),
#       language = "en", id = "id1")
#   dat = c(as.character(dat))
#   dat = dat[ !grepl("^[**]", dat) ]
#   dat = dat[ !grepl("^GPA CRS", dat) ]
#
# }


#' @rdname read_jhu_transcript
#' @param type Type of Transcript
#' @param ... additional arguments to pass to reader function
#' @export
read_transcript = function(file, type = "jhu", ...) {
  type = match.arg(type)
  func = switch(
    type,
    "jhu" = read_jhu_transcript
  )
  func(file, ...)
}

