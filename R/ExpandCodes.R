

#' Convert groups of ICD-10 codes to single codes \lifecycle{stable}
#'
#' Creates the lookup files for search for single ICD-10 codes related to tobacco and/or alcohol.
#'
#' For example, if one disease category is C00-C06 (oral cancer), this includes the single codes
#' C00,C01,C02,C03,C04,C05,C06. The number of rows will be expanded to give each single code
#' its own row.
#'
#' @param lkup Data frame containing the disease list.
#'
#' @return Returns a data frame containing a row for each single ICD-10 code to be searched for.
#' @importFrom data.table := setDT setnames
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' ExpandCodes(lkup)
#'
#' }
#'
ExpandCodes <- function(lkup) {

  colnames(lkup) <- tolower(colnames(lkup))

  ICD_names <- unique(lkup$icd10_lookups)

  lkup <- dplyr::distinct(lkup, condition, icd10_lookups)

  for (ind in ICD_names) {
    # ind <- ICD_names[1]

    temp1 <- dplyr::filter(lkup, icd10_lookups == ind)

    subcodes <- stringr::str_split(temp1$icd10_lookups[1], ",", simplify = T)

    n <- length(subcodes)

    if (n == 1) {
      out <- temp1
    }

    if (n > 1) {
      for (i in 1:n) {
        temp1$icd10_lookups <- subcodes[i]

        if (i == 1) {
          out <- temp1

        } else {
          out <- rbind(out, temp1)
        }
      }
    }

    if (ind == ICD_names[1]) {
      lkup1 <- out

    } else {
      lkup1 <- rbind(lkup1, out)
    }
  }

  data.table::setDT(lkup1)

return(lkup1[])
}


