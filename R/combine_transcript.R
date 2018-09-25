#' Combine JHU Transcript
#'
#' @param file a vector of paths to PDFs
#'
#' @return A \code{data_frame} of the data
#' @export
#'
#' @examples
#' file = "~/Downloads/Transcript.pdf"
#' if (file.exists(file)) {
#'     res = combine_jhu_transcript(c(file, file))
#' }
combine_jhu_transcript = function(file) {
  pass_grades = c("A", "B", "C", "P")
  course_number = grade = credits = pass = NULL
  rm(list = c("course_number", "grade", "credits", "pass"))

  sub_course_number = ss = NULL
  rm(list = c("ss", "sub_course_number"))

  df <- lapply(
    file,
    read_transcript,
    type = "jhu")
  df = lapply(df, function(x) {
    id = attributes(x)$student_info["student_id"]
    x$student_id = id
    x
  })
  if (!is.null(file)) {
    df = mapply(function(x, name) {
      x$file = name
      x
    }, df, file, SIMPLIFY = FALSE)
  }

  df = do.call(rbind, df)
  # if (!input$grade) {
  # df$grade = NULL
  # }
  df = df %>%
    separate(course_number, sep = "[.]", remove = FALSE,
             into = c("school", "department",
                      "sub_course_number"))
  df = df %>%
    mutate(public_health = grepl("PH", course_number))
  df = df %>%
    mutate(pass = grade %in% pass_grades,
           pass_credits = credits * pass,
           ss = as.numeric(sub_course_number),
           is_800 = ss >= 800 & ss < 900) %>%
    select(-ss)
  return(df)
}

#' @rdname combine_jhu_transcript
#' @param type Type of Transcript
#' @param ... additional arguments to pass to combining function
#' @export
combine_transcript = function(file, type = "jhu", ...) {
  type = match.arg(type)
  func = switch(
    type,
    "jhu" = combine_jhu_transcript
  )
  func(file, ...)
}
