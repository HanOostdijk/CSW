#' Display an xml document or node
#'
#' This function displays the contents of an xml document or node in the console log.
#' @param n1 An object of type `xml_document` or `xml_node`
#' @param file Path to file or connection to write to. Default NULL i.e. write to console log
#' @export
#' @examples
#' \dontrun{
#' CSW_display_node(xml1)
#' }
CSW_display_node <- function(n1, file = NULL) {
	if (is.null(file)) {
		tmp1 = fs::file_temp(pattern = 'tmp1_', tmp_dir = '.')
		xml2::write_xml(n1, tmp1, options = 'format')
		cat(readLines(tmp1, warn = FALSE), sep = '', fill=TRUE)
		fs::file_delete(tmp1)
	}
	else
		xml2::write_xml(n1, file, options = 'format')
} # end display_node

match_arg_m <- function (m,var,vec,several.ok=FALSE,free=FALSE) {
	# choose first when missing otherwise match (unless free =T)
	# m = missing(var) in parent environment
	if (m)
		vec[1]
	else if (free)
		var
	else
		match.arg(var,vec,several.ok = several.ok)
}

encode_parm   <- function (x) {
	# encode one or more parameter values
	glue::glue_collapse(
		purrr::map_chr(x,
			~URLencode(as.character(.), reserved = T)),sep='%2C')
}

handle_parm <- function(x,m) {
	# format parameter
	y = encode_parm(m[[x]])
	if (nchar(y) ==0 || y == 'default')
		''
	else
		glue::glue("&{x}={y}")
}

hard_split <- function(strings,
	width = getOption('width', 110) - 3) {
	regarg <- sprintf(".{1,%d}", width)
	strings1 = stringi::stri_extract_all_regex(strings, regarg)
	purrr::flatten_chr(strings1)
}

display_wrapped <- function (strings,
	width = getOption('width', 110) - 3,
	force_wrap = FALSE) {
	if (force_wrap == FALSE) {
		cat(paste0(hard_split(strings, width)), sep = "\n")
	} else {
		cat(stringr::str_wrap(strings, width), sep = "\n")
	}
}


