
.CSW_options <- new.env()

.onLoad <- function(libname, pkgname) {
  invisible()
}

CSW_default_url = "http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?"
CSW_default_version = "2.0.2"

#' Sets the url for CSW catalog structure
#'
#' Determines from which structure catalog information  will be extracted. If this function is not called
#' the default \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} (the url for the PDOK/NGR structures) will be used
#' @param url NULL for the default or the url of the structure otherwise
#' @return Character vector with (invisible) the url.
#' @export
#' @examples
#' CSW_set_url("http://nationaalgeoregister.nl/geonetwork/srv/dut/inspire?")
#' @seealso \code{\link{CSW_get_url}}

CSW_set_url <- function (url=NULL) {
  if (is.null(url)) {
    url = CSW_default_url
  }
  .CSW_options$url = url
  invisible(url)
}

#' Gets the url for the catalog structure
#'
#' Determines from which structure catalog information will be extracted. If the function \code{\link{CSW_set_url}}
#' is not called yet this will be done first with the default \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} (the url for the PDOK/NGR structures).
#' @return Character vector with the active url.
#' @export
#' @examples
#' CSW_get_url()
#' @seealso \code{\link{CSW_set_url}}
CSW_get_url <- function () {
  url = .CSW_options$url
  if (is.null(url)) {
    url = CSW_set_url()
  }
  url
}

#' Sets the version for CSW catalog structure
#'
#' Determines the version of the  structure catalog information that will be extracted. If this function is not called
#' the default \code{2.0.2} (the version for the PDOK/NGR structures) will be used
#' @param version NULL for the default or the version of the structure otherwise
#' @return Character vector with (invisible) the active version.
#' @export
#' @examples
#' CSW_set_version()
#' CSW_set_version('2.0.2')
#' @seealso \code{\link{CSW_get_version}}

CSW_set_version <- function (version=NULL) {
	if (is.null(version)) {
		version = CSW_default_version
	}
	.CSW_options$version = version
	invisible(version)
}

#' Gets the version for the catalog structure
#'
#' Determines the version of the  structure catalog information that will be extracted. If the function \code{\link{CSW_set_version}} is not called yet this will be done first with the default \code{2.0.2} (the version for the PDOK/NGR structures).
#' @return Character vector with the active version.
#' @export
#' @examples
#' CSW_get_version()
#' @seealso \code{\link{CSW_set_version}}
CSW_get_version <- function () {
	version = .CSW_options$version
	if (is.null(version)) {
		version = CSW_set_version()
	}
	version
}
