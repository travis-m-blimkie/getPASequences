
#' retrieveAnnotations
#'
#' @param inputDF Data frame which contains input IDs
#' @param strain Strain of P. aeruginosa to find annotations for
#'
#' @return Data frame of matched genes
#'
#' @export
#'
retrieveAnnotations <- function(inputDF, strain) {

  if (strain == "PAO1") {
    inner_join(inputDF, annosPAO1, by = "Locus Tag")

  } else if (strain == "PA14") {
    inner_join(inputDF, annosPA14, by = "Locus Tag")

  } else if (strain == "LESB58") {
    inner_join(inputDF, annosLESB58,  by = "Locus Tag")

  } else {
    return(NULL)
  }
}




#' mapOrthosGenerally
#'
#' @param inputDF Data frame containing input locus tags
#' @param strain1 Starting strain, corresponding to the input locus tags
#' @param strain2 Strain in which we wish to find orthologs
#'
#' @return Data frame containing ortholog information
#'
#' @export
#'
mapOrthosGenerally <- function(inputDF, strain1, strain2) {

  if (strain1 %in% c("PAO1", "PA14") & strain2 %in% c("PAO1", "PA14")) {
    inner_join(inputDF, orthologs_PAO1_PA14)

  } else if (strain1 %in% c("PAO1", "LESB58") & strain2 %in% c("PAO1", "LESB58")) {
    inner_join(inputDF, orthologs_PAO1_LESB58)

  } else if (strain1 %in% c("PA14", "LESB58") & strain2 %in% c("PA14", "LESB58")) {
    inner_join(inputDF, orthologs_PA14_LESB58)

  } else {
    return(NULL)
  }
}



#' insertAlert
#'
#' @param location Name of UI element determining where the alert will be
#'   inserted. Should begin with a "#".
#' @param ID ID to be assigned to the alert
#' @param type Type of alert; one of "success", "info", "warning", or "danger"
#' @param content Text to be displayed in the alert
#'
#' @return
#'
#' @export
#'
insertAlert <- function(location, ID, type, content) {
  insertUI(
    selector = location,
    where = "beforeEnd",
    ui = tags$div(
      id = ID,
      class = paste0("alert alert-dissmissible alert-", type),
      tags$button(
        HTML("&times;"),
        type = "button",
        class = "close",
        `data-dismiss` = "alert",
        style = "padding-left: 3px;"
      ),
      tags$b(content)
    )
  )
}
