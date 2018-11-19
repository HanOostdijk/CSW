
#' CSW: A package for reading catalog information with the Catalog Service for the Web (CSW) interface.
#'
#' The \href{http://www.opengeospatial.org/standards/cat}{Catalogue Service} page of the \href{http://www.opengeospatial.org/}{Open Geospatial Consortium (OGC)} describes:\cr
#' Catalogue services support the ability to publish and search collections of descriptive information (metadata) for data, services, and related information objects. Metadata in catalogues represent resource characteristics that can be queried and presented for evaluation and further processing by both humans and software. Catalogue services are required to support the discovery and binding to registered information resources within an information community.\cr\cr
#' One of the ways to get information about and from a CSW catalog is the GET method of the HTTP protocol. By specifying a properly formed URL in a internet browser the requested information is shown in the browser window.\cr
#' This package provides functions that use the same GET method to place the result in an R object such  as e.g. a data.frame, xml_document, list  or integer variable.\cr
#' The package has functions for the **read-only** operations of the CSW interface: e.g. the `CSW_GetRecords` function corresponds wiht the `GetRecords` operation. Also included are some utility functions.\cr\cr
#' Until now this package is only tested on the \href{http://nationaalgeoregister.nl/geonetwork/srv/dut/search}{Nationaal GeoRegister(NGR)} catalog with CSW 2.0.2. By using the \code{\link{CSW_set_url}} and  \code{\link{CSW_set_version}} functions these values will be replaced by the ones provided by the user.\cr\cr
#' Let me know if you encounter problems with other catalogs: maybe these can be solved.\cr\cr
#' See the vignette for examples.
#'
#' @importFrom magrittr "%>%"
#'
#' @name CSW-package
NULL

