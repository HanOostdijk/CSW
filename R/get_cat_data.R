
#' Retrieve data from CSW server
#'
#' This function is the only function of the CSW package that actually retrieves data from the CSW Server. Most often this function will not be used directly by the user. For the various CSW operations dedicated functions are provided that generate a string with parameters that is passed to `CSW_GetData`. With the parameter `verbose` one can indicate that this generated string is printed in the console log. These parameters can then specified for this function (e.g. when an option would be needed that is not provided for by the dedicated functions). See example.
#' @param request Character vector indicating the CSW operation that has to be performed. One of `GetCapabilities`, `GetRecords`, `DescribeRecord`, `GetDomain`, `GetRecordById`, `Transaction`, `Harvest`.  Default `GetCapabilities`.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @param ... Optionally one or more `name=value` arguments. See example exp2.
#' @return An `xml` document with the requested information.
#' @export
#' @section Remarks:
#' `CSW_GetData` always returns an xml document where other functions sometimes try to reformat the information when requested. Using this function can help with debugging when there is a problem (e.g. with this formatting).
#' @examples
#' \dontrun{
#' # exp1 and exp2 request the same information from the server:
#' exp1 = CSW_DescribeRecord(typeName = 'gmd:MD_Metadata',
#' 	namespace = 'gmd:http://www.isotc211.org/2005/gmd',verbose='N')
#' exp2 = CSW_GetData(request='DescribeRecord',
#' 	namespace ='gmd:http://www.isotc211.org/2005/gmd',
#' 	typeName ='gmd:MD_Metadata',verbose='N')
#' }

CSW_GetData <- function (
	request = c("GetCapabilities", "GetRecords", "DescribeRecord",
		"GetDomain", "GetRecordById", "Transaction", "Harvest"),
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"),
	...)
{
	request =  match_arg_m(missing(request),request,
		c("GetCapabilities", "GetRecords", "DescribeRecord",
			"GetDomain", "GetRecordById", "Transaction",
			"Harvest") ,several.ok = FALSE)
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	m  = list(...)           # list with parameters
	rqf       <- glue::glue(  # fixed part of url
		baseurl
		, "service=CSW"
		, "&version={version}"
		, "&request={request}"
	)
	nm = setdiff(names(m),'typeNames')
	# handle all parameters except typeName(s)
	rq = glue::glue_collapse(purrr::map_chr(nm,~handle_parm(.,m)))
	# handle parameter typeName(s)
	if (!is.null(m$typeNames) && m$typeNames != 'default'){
		if (request == 'DescribeRecord')
			rq = glue::glue(rq,glue::glue("&typeName={y}",y=encode_parm(m$typeNames)) )
		else
			rq = glue::glue(rq,glue::glue("&typeNames={y}",y=encode_parm(m$typeNames)) )
	}
	rq1 = glue::glue(rqf,rq)
	if (verbose == "F") {
		display_wrapped(glue::glue('{request} request:'))
		display_wrapped(rq1)
		cat('\n')
	} else if (verbose == "Y") {
		display_wrapped(
			glue::glue('variable part of {request} request (decoded):')
		)
		display_wrapped(URLdecode(rq))
		cat('\n')
	}
	xml2::read_xml(rq1, options = "NOWARNING")
}# end CSW_GetData

#' Get Description of CSW Records
#'
#' This function gives a description of the catalog records in the form of an `xml` document. With typeName `csw:Record`  the description follows a table format and that is why I like this format more than the one you get with typeName `gmd:MD_Metadata`. It is possible to get both formats in one `xml` document as shown in example exp12.
#' @param typeName Character vector with one or both of `csw:Record` and `gmd:MD_Metadata`. But see remarks.
#' @param typeNames alternative for `typeName`.
#' @param namespace Character vector with one or both of `csw:http://www.opengis.net/cat/csw/2.0.2` and `gmd:http://www.isotc211.org/2005/gmd`. See Remarks for the relation between `typeName` and `namespace`.
#' @param outputFormat Character vector. Only  `application/xml` is apparently (?) supported by CSW
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return An `xml` document with the record description in the indicated layout.
#' @export
#' @section Remarks:
#' typeName `gfc:FC_FeatureCatalogue` and `dcat` were also mentioned as possible values but can not be combined with the two namespace options that are given here. Also found was the gmi analog for gmd but for the default catalog no results were delivered. Moreover the typeName and namespace must match: the first typeName with the first namespace and the same for the second one.
#' @examples
#' \dontrun{
#' exp1  = CSW_DescribeRecord()
#' exp2  = CSW_DescribeRecord(typeName = 'gmd:MD_Metadata',
#'        namespace = 'gmd:http://www.isotc211.org/2005/gmd')
#' exp12 = CSW_DescribeRecord(
#'     typeName =
#'         c('csw:Record',	'gmd:MD_Metadata') ,
#'     namespace = c(
#'         'csw:http://www.opengis.net/cat/csw/2.0.2',
#'         'gmd:http://www.isotc211.org/2005/gmd') )
#' }

CSW_DescribeRecord <- function(
	typeName  =
		c('csw:Record',	'gmd:MD_Metadata', 'gmi:MI_Metadata') ,
	typeNames =
		c('csw:Record',	'gmd:MD_Metadata', 'gmi:MI_Metadata') ,
	namespace = c(
		'csw:http://www.opengis.net/cat/csw/2.0.2',
		'gmd:http://www.isotc211.org/2005/gmd',
		'gmi:http://www.isotc211.org/2005/gmi'),
	outputFormat = "application/xml",
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	# typeNames and namespace with csw match;
	# same for gmd (with outputSchema=http://www.isotc211.org/2005/gmd ??)
	# other values / combinations ??
	if ( !missing(typeName) )
		typeNames <- typeName
	typeNames =  match_arg_m(missing(typeNames),typeNames,
		c('csw:Record',	'gmd:MD_Metadata', 'gmi:MI_Metadata',
			'gfc:FC_FeatureCatalogue','dcat') ,several.ok = TRUE)
	namespace =  match_arg_m(missing(namespace),namespace,
		c('csw:http://www.opengis.net/cat/csw/2.0.2',
			'gmd:http://www.isotc211.org/2005/gmd',
			'gmi:http://www.isotc211.org/2005/gmi'
		),several.ok = TRUE)
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	outputFormat =  match_arg_m(missing(outputFormat),outputFormat,
		c("default", "application/xml"),several.ok = FALSE, free=TRUE)
	CSW_GetData (
		request = 'DescribeRecord',
		namespace = namespace,
		typeNames = typeNames,
		outputFormat = outputFormat,
		version = version,
		baseurl = baseurl,
		verbose = verbose
	)
} # end CSW_DescribeRecord

#' Get Capabilities of the CSW server
#'
#' The `CSW_GetCapabilities` function gives information about the capabilities that are available to query the catalog. By calling this function without arguments we receive an `xml` document with this information. We can view the contents of this document (e.g.  with the utility function `CSW_display_node`) and study its structure. We see among other things the operations that are available and the parameters with which these operations can be called. Apart of viewing the document we can also select parts of it with functions of the \pkg{xml2} package by using `XPATH` expressions. The vignette shows how one can see which operations are provided by the CSW interface and which parameters are available for the GetRecords operation (i.e. the `CSW_GetRecords` function).
#' @param sections Character vector to restrict the output to one of the `OperationsMetadata`, `ServiceIdentification`, `ServiceProvider` and `Filter_Capabilities` sections. When the argument is not specified all information is provided. NB whatever section is requested, the `Filter_Capabilities` section is always included.
#' @param typeNames Character vector with one of `csw:Record`, `gmd:MD_Metadata`, 	`gfc:FC_FeatureCatalogue` and `dcat`. Default `csw:Record`.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return An `xml` document with the indicated server capabilities.
#' @export
#' @examples
#' \dontrun{
#' exp1  = CSW_GetCapabilities(sections = 'OperationsMetadata',
#'          typeNames = 'csw:Record')
#' # CSW_display_node(exp1)
#' exp2  = CSW_GetCapabilities(sections = 'OperationsMetadata',
#'          typeNames = 'gmd:MD_Metadata')
#' }
CSW_GetCapabilities <- function(
	sections = c('OperationsMetadata','ServiceIdentification',
		'ServiceProvider','Filter_Capabilities'),
	typeNames =  c('csw:Record', 'gmd:MD_Metadata',
		'gfc:FC_FeatureCatalogue', 'dcat'),
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	sections =  match_arg_m(missing(sections),sections,
		c('default', 'OperationsMetadata','ServiceIdentification',
			'ServiceProvider','Filter_Capabilities'
		),several.ok = FALSE)
	typeNames =  match_arg_m(missing(typeNames),typeNames,
		c('csw:Record',	'gmd:MD_Metadata',
			'gfc:FC_FeatureCatalogue', 'dcat'),several.ok = FALSE)
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	CSW_GetData (
		request = 'GetCapabilities',
		sections = sections,
		typeNames = typeNames,
		version = version,
		verbose = verbose,
		baseurl = baseurl
	)
} # end CSW_GetCapabilities

#' Get Domain values of the CSW server
#'
#' With the `GetDomain` operation the CSW server can return information about `ParameterName`s or `PropertyName`s (fields). \cr\cr
#' We can inquire after the parameters of an operation by specifying the `ParameterName` argument. This argument consists of one or more `operation.parameter` pairs as in example `exp1`. The output is a list (or alternatively an xml_document) with the values that can be used for the parameters. In each pair the part before the point should be an operation; the part after the point a parameter.  \code{\link{CSW_GetDomainParameterNames}} gives the same information for all possible combinations (at the cost of some extra run-time). \cr\cr
#' We can also use the `CSW_GetDomain` function to inquire which values a certain field can take in a catalog. We do this by specifying the fieldname in the `PropertyName` argument. This argument consists of one or more fieldnames as e.g. in example `exp2`. The output is (just as in the `ParameterName` case) a list (or alternatively an xml_document) with the values that can be taken by the field. \code{\link{CSW_GetQueryables}} will show which fieldnames (`PropertyNames`) are recognized.
#' @param ParameterName Character vector with one or more CSW parameters. See  \code{\link{CSW_GetDomainParameterNames}} for possible values. When `ParameterName` and `PropertyName` are both used in the same call `ParameterName` will be ignored.
#' @param PropertyName Character vector with one or more CSW parameters. See  \code{\link{CSW_GetQueryables}} for possible values. When `ParameterName` and `PropertyName` are both used in the same call `ParameterName` will be ignored.
#' @param output Character vector with one of `list` and `xml`. With the default `list` the `xml` output is converted to a list.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return An xml_document or list with the values permitted (for `ParameterName`) or to be used (for `PropertyName`)
#' @export
#' @seealso \code{\link{CSW_GetDomainParameterNames}}, \code{\link{CSW_GetQueryables}}
#' @examples
#' \dontrun{
#' exp1 = CSW_GetDomain(
#'    ParameterName = "GetRecords.outputFormat,GetRecords.ElementSetName",
#'    verbose = "Y")
#' exp2 = CSW_GetDomain(
#'    PropertyName = "Language,GeographicDescriptionCode",
#'    verbose = "Y")
#' }
CSW_GetDomain <- function(
	ParameterName='',
	PropertyName='',
	output= c('list','xml'),
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	output =  match_arg_m(missing(output),output,
		c('list','xml'),several.ok = FALSE)
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	res = CSW_GetData (
		request = 'GetDomain',
		ParameterName = ParameterName,
		PropertyName = PropertyName,
		version = version,
		verbose = verbose,
		baseurl = baseurl
	)
	if (output == 'list') {
	  dv = xml2::xml_find_all(res, '//csw:DomainValues')

	  fn  <- function(z) {
	  	# do this for each of PropertyName or ParameterName
			nme = xml2::xml_child(z,1) %>% xml2::xml_text() # name
			val = xml2::xml_find_all(z,'.//csw:Value') %>% xml2::xml_text()
			res = c(nme,val)
	  }
	  res = purrr::map(dv,fn)
	  nm<- purrr::map_chr(res,function(x)x[1]) # extract name
	  res <- purrr::map(res,function(x)x[-1]) # keep values
	  names(res) = nm # assign name to list elements
  }
	res
} # end CSW_GetDomain

#' Get Queryable values of the CSW server
#'
#' The function `CSW_GetQueryables` retrieves the names of properties (fields) that can be used in the `CSW_GetDomain` function or in a query. The output is a list with two sublists: one with the 'SupportedISOQueryables' and one with the 'AdditionalQueryables'.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return A list with two sublists: one with the 'SupportedISOQueryables' and one with the 'AdditionalQueryables'
#' @export
#' @examples
#' \dontrun{
#' exp1 = CSW_GetQueryables()
#' }

CSW_GetQueryables <- function (
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	x = CSW_GetCapabilities(
		version = version,
		verbose = verbose,
		baseurl = baseurl)
	siq = xml2::xml_find_all(
		x, glue::glue('//ows:Operation[@name="GetRecords"]',
			'//ows:Constraint[@name="SupportedISOQueryables"]//ows:Value')
	)
	aq = xml2::xml_find_all(
		x, glue::glue('//ows:Operation[@name="GetRecords"]',
			'//ows:Constraint[@name="AdditionalQueryables"]//ows:Value')
	)
	list(
		SupportedISOQueryables = xml2::xml_text(siq),
		AdditionalQueryables = xml2::xml_text(aq)
	)
} # end CSW_GetQueryables

#' Get elements that can be used in GetDomain as ParameterName
#'
#' `CSW_GetDomainParameterNames` calls CSW_GetDomain for all (im)possible combinations of
#' Operation and Parameter names assembles the result in a `data.frame`. Not all parameters are found in this way: e.g. `DescribeRecord.outputSchema` can be found in the output of the `GetCapabilities` output but not in the output of `CSW_GetDomainParameterNames` !?
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return A `data.frame` with columns `n` (element names) and `v` (possible parameter values). An element name has the format CSW_operation.CSW_parameter.
#' @section Remarks:
#' I have seen examples where CSW_GetCapabilities directly shows
#' which combinations are supported by CSW_GetDomain
#' (see \url{https://docs.oracle.com/cd/E11882_01/appdev.112/e11830/sdo_csw.htm#SPATL963})
#' but for the default baseurl this is apparently not available.
#' @export
#' @examples
#' \dontrun{
#' exp1 = CSW_GetDomainParameterNames()
#' }

CSW_GetDomainParameterNames <- function (
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	x = CSW_GetCapabilities(
		version = version,
		verbose = verbose,
		baseurl = baseurl)
	ns = xml2::xml_ns(x)

	# names of known Operations and Parameters
	# ops <- xml2::xml_find_all(x, "//ows:Operation")
	# op_names <-xml2::xml_attr(ops, "name")
	op_names <- c("GetCapabilities", "DescribeRecord", "GetDomain", "GetRecords",
		"GetRecordById", "Transaction", "Harvest" )
	# parms <- xml2::xml_find_all(x, "//ows:Parameter")
	# parms_names <-xml2::xml_attr(parms, "name")
	parms_names <- c("sections", "outputFormat", "schemaLanguage", "outputSchema",
		"resultType" , "typeNames", "CONSTRAINTLANGUAGE", "ElementSetName",
		"ResourceType", "service", "version" )
	# add potential Parameter names
	parms_names <- c(parms_names,"namespace", "typeName", "ParameterName",
		"PropertyName", "ElementName","outputRecType")
	parms_names <- unique(parms_names)

	# all combinations of Operation and Parameter names
	allcomb = expand.grid(op_names,
		parms_names,
		KEEP.OUT.ATTRS = F,
		stringsAsFactors = F)

	# function to retrieve values for one Operation and Parameter name combination
	# NB most often there is no data for a combination
	fn <-	function(x, ns) {
		element_name = as.character(glue::glue("{x1}.{x2}",
			x1 = allcomb[x, 1], x2 = allcomb[x, 2]))
		x = CSW_GetDomain(ParameterName = element_name, output = 'xml')
		val = xml2::xml_find_all(x, './/csw:Value', ns) %>% xml2::xml_text()
		res = data.frame(
			n = rep(element_name, length(val)),
			v = val,
			stringsAsFactors = F
		)
	}
	# table with parameter values for available elements
	purrr::map_dfr(seq(1, dim(allcomb)[1]), ~ fn(., ns)) %>%
		dplyr::arrange(n, v)
} # end CSW_GetDomainParameterNames

#' Get the number of records that match a constraint
#'
#' CSW_GetHits returns the number of records that satisfy a constraint. The default constraint language and its version are `CQL_TEXT` and `1.1.0` but can be overwritten by arguments of this function.
#' @param constraint Character vector with a constraint written with `` and `constraint_language_version==1.1.0`.
#' @param constraintLanguage Character vector indicating in which language the constraint is written. Default `CQL_TEXT`.
#' @param constraint_language_version Character vector indicating the version of the constraint language. Default `1.1.0`.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return An integer with the number of records that satisfy the constraint or an xml_document with the exception in case of an error
#' @export
#' @examples
#' \dontrun{
#' # total number of records in collection :
#'   exp1 = CSW_GetHits()
#' # total number of records related to 'duin*' (dune*)
#'   exp2 = CSW_GetHits(constraint="AnyText LIKE 'duin%'" )
#' }

CSW_GetHits <- function(
	constraint = '',
	constraintLanguage= 'CQL_TEXT',
	constraint_language_version = '1.1.0',
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	csw_gr_xml = CSW_GetData (
		request = 'GetRecords',
		resultType = 'hits',
		typeNames = 'csw:Record',
		maxRecords = 1,
		startPosition = 1 ,
		constraint = constraint,
		constraintLanguage = constraintLanguage,
		constraint_language_version = constraint_language_version,
		version = version,
		verbose = verbose,
		baseurl = baseurl
	)
	if (is_exception(csw_gr_xml)) {
		return(NA)
	} else {
		ns = xml2::xml_ns(csw_gr_xml)
		n1 = xml2::xml_find_first(csw_gr_xml, './/csw:SearchResults', ns = ns)
	  get_number(n1)
	}
} # end CSW_GetHits

#' Get catalog info for entry with given id
#'
#' The only difference with \code{\link{CSW_GetRecords}} is that the `constraint` and a 'results` arguments of \code{\link{CSW_GetRecords}} are replace by the  `id` argument of \code{\link{CSW_GetRecordById}}. Therefore see \code{\link{CSW_GetRecords}} for details.
#' @param id Character vector with the Identifier of the catalog entry that has to be retrieved.
#' @param typeNames Character vector with one (or both?) of `csw:Record` and `gmd:MD_Metadata`. Also `gfc:FC_FeatureCatalogue` and `dcat` can be used.
#' @param outputSchema Character vector with one (or more?) of `http://www.opengis.net/cat/csw/2.0.2` , `http://www.isotc211.org/2005/gmd`, `http://www.isotc211.org/2005/gfc` and `http://www.w3.org/ns/dcat#`. The first two work and the first of them give a more table like presentation.
#' @param ElementSetName Character vector with one of `summary`, `full` and `brief` indicating how much data is presented. Default `summary`.
#' @param namespace Character vector with one or both of `csw:http://www.opengis.net/cat/csw/2.0.2` and `gmd:http://www.isotc211.org/2005/gmd`. See Remarks for the relation between `typeName` and `namespace`.
#  apparently srsName does not work for CSW as it does for WFS
#  @param srsName  Character vector with indication for coordinate system. Default for the default catalog is EPSG:28992 (Amersfoort / RD New).  An alternative is e.g. EPSG:4326 (WGS84 World Geodetic System 1984). See \code{https://epsg.io/} for more information about coordinate systems.
#' @param output Character vector with one of `table`, `list` and `xml`. With `table` (the default) the results will be put in a (one row) data.frame. With `list` the output is conforming to that of  \code{\link{CSW_GetRecords}} when `list` is requested (the number of retrieved (here 1) and total records (here 1) are gathered in a list). With `xml` the retrieved xml_document is passed through without conversion.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return An `xml` document with the record description in the indicated layout.
#' @seealso \code{\link{CSW_GetRecords}}
#' @section Remarks:
#' We have tried various combinations of typeNames (first two only), outputSchema (same) and namespace.  These have only two possible outcomes and depend only on outputSchema: the csw output is more table like and the gmd output is more verbose (?)
#' @export
#' @examples
#' \dontrun{
#'  exp1  = CSW_GetRecordById(
#'           id = "ecb96a41-19da-4a57-b570-05a1ee5743ca",
#'           ElementSetName = "brief",
#'           output = "xml")
#' }
CSW_GetRecordById  <- function(id='',
	typeNames = c('csw:Record',	'gmd:MD_Metadata',
		'gfc:FC_FeatureCatalogue', 'dcat'),
	outputSchema =	c('http://www.opengis.net/cat/csw/2.0.2'
		, 'http://www.isotc211.org/2005/gmd'
		, 'http://www.isotc211.org/2005/gfc'
		, 'http://www.w3.org/ns/dcat#') ,
	ElementSetName = c('summary', 'full', 'brief'),
	namespace=c('csw:http://www.opengis.net/cat/csw/2.0.2',
		'gmd:http://www.isotc211.org/2005/gmd'),
	# srsName= c('EPSG:28992','EPSG:4326'),
	output = c('table','list','xml'),
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	typeNames =  match_arg_m(missing(typeNames),typeNames,
		c('csw:Record',	'gmd:MD_Metadata',
			'gfc:FC_FeatureCatalogue', 'dcat'),several.ok = TRUE)
	outputSchema =  match_arg_m(missing(outputSchema),outputSchema,
			c('default'
				, 'http://www.opengis.net/cat/csw/2.0.2'
				, 'http://www.isotc211.org/2005/gmd'
				, 'http://www.isotc211.org/2005/gfc'
				, 'http://www.w3.org/ns/dcat#'
			),several.ok = FALSE)
	ElementSetName = match_arg_m(missing(ElementSetName),ElementSetName,
		  c('summary', 'full', 'brief'
		  ),several.ok = FALSE)
	namespace =  match_arg_m(missing(namespace),namespace,
		c('default'
			,'csw:http://www.opengis.net/cat/csw/2.0.2'
			,'gmd:http://www.isotc211.org/2005/gmd'
		),several.ok = TRUE)
	# srsName= match_arg_m(missing(srsName),srsName,
	# 	c('default','EPSG:28992','EPSG:4326'),several.ok = FALSE, free=TRUE)
	output =  match_arg_m(missing(output),output,
		c('table','list','xml'), several.ok = FALSE)
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
		c("N", "F", "Y") ,several.ok = FALSE)
	# retrieve XML load
	request = 'GetRecordById'
	csw_gr_xml = CSW_GetData (
		request = request,
 		resultType = 'results',
		typeNames = typeNames ,
		outputSchema = outputSchema,
		ElementSetName = ElementSetName ,
		namespace = namespace,
		# srsName = srsName,
		id = id,
		version = version,
		verbose = verbose,
		baseurl = baseurl
	)
	if (output == 'xml')
		csw_gr_xml
	else
		CSW_GetEntries 	(csw_gr_xml,request,ElementSetName,output = output)
} # end CSW_GetRecordById

#' Get catalog info for records satisfying constraint
#'
#' The `CSW_GetRecords` function retrieves catalog entries from the CSW server (unless `resultType='validate'` is specified). The default output format is a `data.frame` with as alternatives an `xml_document` or `list` (see `output`). The fields of the `data.frame` are determined by the `ElementSetName` argument that results in few (`brief`), more (`summary`) and most (`full`) fields. The relation between `typeNames`, `outputSchema` and `namespace` is not clear to me, but it seems that the argument values must correspond: taking the first option for `typeNames` one should also take the first one for `outputSchema` and `namespace`. Also see remarks.\cr
#' The default constraint language and its version are `CQL_TEXT` and `1.1.0` but can be overwritten by arguments of this function. See the vignette for examples and references for the query language.
#' @param constraint Character vector with a constraint written with `constraintLanguage=CQL_TEXT` and `constraint_language_version==1.1.0`.
#' @param constraintLanguage Character vector indicating in which language the constraint is written. Default `CQL_TEXT`.
#' @param constraint_language_version Character vector indicating the version of the constraint language. Default `1.1.0`.
#' @param resultType Character vector with one of `results` and `validate`. With `results` actual search results will be returned. With `validate` a Boolean will be returned indicating if the constraint is valid.
#' @param typeNames Character vector with one (or both?) of `csw:Record` and `gmd:MD_Metadata`. Also `gfc:FC_FeatureCatalogue` and `dcat` can be used.
#' @param outputSchema Character vector with one (or more?) of `http://www.opengis.net/cat/csw/2.0.2` , `http://www.isotc211.org/2005/gmd`, `http://www.isotc211.org/2005/gfc` and `http://www.w3.org/ns/dcat#`. The first two work and the first of them give a more table like presentation.
#' @param ElementSetName Character vector with one of `summary`, `full` and `brief` indicating how much data is presented. Default `summary`.
#' @param namespace Character vector with one or both of `csw:http://www.opengis.net/cat/csw/2.0.2` and `gmd:http://www.isotc211.org/2005/gmd`. See Remarks for the relation between `typeName` and `namespace`.
#' @param maxRecords Integer indicating the number of records that will be read from the server. Default is determined by the CSW server (for the default `baseurl` the default `maxRecords` is 10 but values in the order of 100 are accepted )
#' @param startPosition Integer indicating the number of the first record that will be returned. The default is 1. To read e.g. the records of a constraint with 150 hits one can issue two requests: the first one with `maxRecords = 100` and `startPosition = 1` (returning 100 records) and the second with  `maxRecords = 100` and `startPosition = 101` (that will return the last 50 records)
#  apparently srsName does not work for CSW as it does for WFS
#  @param srsName  Character vector with indication for coordinate system. Default for the default catalog is EPSG:28992 (Amersfoort / RD New).  An alternative is e.g. EPSG:4326 (WGS84 World Geodetic System 1984). See \code{https://epsg.io/} for more information about coordinate systems.
#' @param output Character vector with one of `table`, `list` and `xml`. With `table` (the default) the results will be put in a data.frame. With `list` this table is placed in a list with the number of retrieved and total number of records satisfying the constraint. With `xml` the retrieved xml_document is passed through without conversion.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return An `xml` document with the record description in the indicated layout.
#' @section Remarks:
#' We have tried various combinations of typeNames (first two only), outputSchema (same) and namespace.  These have only two possible outcomes and depend only on outputSchema: the csw output is more table like and the gmd output is more verbose (?)
#' @export
#' @examples
#' \dontrun{
#'  exp1 = CSW_GetRecords(constraint="AnyText LIKE 'duin%'" )
#' }
CSW_GetRecords <- function(
	constraint = '',
	constraintLanguage = 'CQL_TEXT',
	constraint_language_version = '1.1.0',
	resultType =c('results','validate'),
	typeNames = c('csw:Record',	'gmd:MD_Metadata',
		'gfc:FC_FeatureCatalogue', 'dcat'),
	outputSchema =	c('http://www.opengis.net/cat/csw/2.0.2'
		, 'http://www.isotc211.org/2005/gmd'
		, 'http://www.isotc211.org/2005/gfc'
		, 'http://www.w3.org/ns/dcat#') ,
	ElementSetName = c('summary', 'full', 'brief'),
	namespace=c('csw:http://www.opengis.net/cat/csw/2.0.2',
		'gmd:http://www.isotc211.org/2005/gmd'),
	maxRecords = '',
	startPosition = '' ,
	# srsName= c('EPSG:28992','EPSG:4326'),
	output = c('table','list','xml'),
	version = CSW_get_version(),
	baseurl = CSW_get_url(),
	verbose = c("N", "F", "Y"))
{
	resultType =  match_arg_m(missing(resultType),resultType,
		c('results','validate'),several.ok = FALSE)
	typeNames =  match_arg_m(missing(typeNames),typeNames,
		c('csw:Record',	'gmd:MD_Metadata',
			'gfc:FC_FeatureCatalogue', 'dcat'),several.ok = TRUE)
	outputSchema =  match_arg_m(missing(outputSchema),outputSchema,
		c('default'
			, 'http://www.opengis.net/cat/csw/2.0.2'
			, 'http://www.isotc211.org/2005/gmd'
			, 'http://www.isotc211.org/2005/gfc'
			, 'http://www.w3.org/ns/dcat#'
		),several.ok = FALSE)
	ElementSetName = match_arg_m(missing(ElementSetName),ElementSetName,
		c('summary', 'full', 'brief'
		),several.ok = FALSE)
	namespace =  match_arg_m(missing(namespace),namespace,
		c('default'
			,'csw:http://www.opengis.net/cat/csw/2.0.2'
			,'gmd:http://www.isotc211.org/2005/gmd'
		),several.ok = TRUE)
	# srsName= match_arg_m(missing(srsName),srsName,
	# 	c('default','EPSG:4326'),several.ok = FALSE, free=TRUE)
	output =  match_arg_m(missing(output),output,
		c('table','list','xml'), several.ok = FALSE)
	verbose =  match_arg_m(missing(verbose),toupper(verbose),
			c("N", "F", "Y") ,several.ok = FALSE)
	# retrieve XML load
	request = 'GetRecords'
	csw_gr_xml = CSW_GetData (
		request = request,
	  resultType =  resultType,
		typeNames = typeNames ,
		outputSchema = outputSchema,
		ElementSetName = ElementSetName ,
		namespace = namespace,
		maxRecords = maxRecords,
		startPosition = startPosition,
		# srsName = srsName,
		constraint = constraint,
		constraintLanguage = constraintLanguage,
		constraint_language_version = constraint_language_version,
		version = version,
		verbose = verbose,
		baseurl = baseurl
	)
	if (resultType == 'validate'){
		if (output == 'xml')
			return(csw_gr_xml)
		else if (is_exception(csw_gr_xml))
			return(FALSE)
		else
			return(TRUE)
	}
	if (output == 'xml')
		csw_gr_xml
	else
		CSW_GetEntries 	(csw_gr_xml,request,ElementSetName,output = output)
} # end CSW_GetRecords

CSW_GetEntries 	<- function (csw_gr_xml,
		request,
		ElementSetName,
		output) {
	if (is_exception(csw_gr_xml)) {
		if (output == 'list') {
			return(
				list(
					'nrm' = 0,
					'nrr' = 0,
					df = data.frame(),
					xml = csw_gr_xml
				)
			)
		} else { # output == 'table'
			return(
					data.frame()
			)
		}
	}

	# search results
	if (request == 'GetRecords') {
		search_res = xml2::xml_child(csw_gr_xml, 2)
		nrm = get_number(search_res, type = 'matched')
		nrr = get_number(search_res, type = 'returned')
	} else { # GetRecordById
		nrm = 1
		nrr = 1
	}

	if (ElementSetName == "full") {
		element    <- ".//csw:Record"
	} else if (ElementSetName == "summary"){
		element    <- ".//csw:SummaryRecord"
	} else
		element    <- ".//csw:BriefRecord"

	# search elements
	ns <- xml2::xml_ns(csw_gr_xml)
	se   <- xml2::xml_find_all(csw_gr_xml, element, ns)

	# function to retrieve data from one entry
	upc <- function(f, i) {
		d1 = purrr::map_dfr(xml2::xml_children(f), function(x)
			tibble::tibble(
				n = xml2::xml_name(x, ns = character()),
				v = xml2::xml_text(x)
			))
		d1 = d1 %>%
			unique() %>%
			dplyr::group_by(n) %>%
			dplyr::summarise(v = paste(v, collapse = ", ")) %>%
			dplyr::ungroup()
		d2 = tibble::tibble(seqnr = rep(i, dim(d1)[1]))
		cbind(d2, d1)
	}

	# collect the data for all retrieved catalog entries
	 cdata <- purrr::imap_dfr(se, upc)

	cdata = cdata %>%
		tidyr::spread(n, v) %>%
		dplyr::select(-seqnr)

	if (output == 'list') {
		return(
			list(
				'nrm' = nrm,
				'nrr' = nrr,
				df = cdata
			)
		)
	} else { # output == 'table'
		return(
			cdata
		)
	}
} # end CSW_GetEntries

#' Get catalog info for all records satisfying constraint
#'
#' CSW_GetRecordsAll retrieves all records that satisfy a constraint by looping over CSW_GetRecords until all records are handled
#' @param constraint Character vector with a constraint written with `constraintLanguage=CQL_TEXT` and `constraint_language_version==1.1.0`.
#' @param typeNames Character vector with one (or both?) of `csw:Record` and `gmd:MD_Metadata`. Also `gfc:FC_FeatureCatalogue` and `dcat` can be used.
#' @param ElementSetName Character vector with one of `summary`, `full` and `brief` indicating how much data is presented. Default `summary`.
#  apparently srsName does not work for CSW as it does for WFS
#  @param srsName  Character vector with indication for coordinate system. Default for the default catalog is EPSG:28992 (Amersfoort / RD New).  An alternative is e.g. EPSG:4326 (WGS84 World Geodetic System 1984). See \code{https://epsg.io/} for more information about coordinate systems.
#' @param output Character vector with one of `table` and `list`. With `table` (the default) the results will be put in a data.frame. With `list` this table is placed in a list with the number of retrieved and total number of records satisfying the constraint.
#' @param batchsize Integer with the number of records that will be retrieved in each iteration of the retrieval loop. Default 100.
#' @param version Character vector with CSW version. Default is `2.0.2` but this can be changed for the remainder of the session with \code{\link{CSW_set_version}} .
#' @param baseurl Character vector with base url of the CSW server. Default is \url{http://nationaalgeoregister.nl/geonetwork/srv/dut/csw?} but this can be changed for the remainder of the session with  \code{\link{CSW_set_url}} .
#' @param verbose Character `N`, `F` or `Y`. When `F` the full generated and encoded url will be displayed, when `Y` the variable part of the url (without baseurl, service indication `CSW` and version) will be displayed in decoded form and when `N` (default) nothing of the generated url will be displayed.
#' @return An `xml` document with the record description in the indicated layout.
#' @seealso \code{\link{CSW_GetRecords}}
#' @export
#' @examples
#' \dontrun{
#'  exp1 = CSW_GetRecordsAll(constraint="AnyText LIKE 'duin%'" )
#' }
CSW_GetRecordsAll <- function(constraint,
	typeNames =
			c('csw:Record', 'gfc:FC_FeatureCatalogue',
				'dcat', 'gmd:MD_Metadata') ,
	ElementSetName = c('summary', 'full', 'brief'),
#	srsName= c('EPSG:28992','EPSG:4326'),
	output = c('table','list'),
	batchsize = 100,
	version = "2.0.2",
	verbose = F,
	baseurl =  CSW_get_url() )
{
	typeNames =  match_arg_m(missing(typeNames),typeNames,
			c('csw:Record',	'gfc:FC_FeatureCatalogue',
				'dcat', 'gmd:MD_Metadata'
			),several.ok = TRUE)
	ElementSetName =  match_arg_m(missing(ElementSetName),ElementSetName,
		c('summary', 'full', 'brief'),several.ok = FALSE)
	# srsName= match_arg_m(missing(srsName),srsName,
	# 	c('default','EPSG:4326'),several.ok = FALSE, free=TRUE)
	output =  match_arg_m(missing(output),output,
		c('table','list'),several.ok = FALSE)
	cs_a <- CSW_GetRecords(
		constraint = constraint,
		resultType =  'results',
		maxRecords = 100,
		startPosition = 1,
		typeNames = typeNames,
		ElementSetName = ElementSetName,
		# srsName = srsName,
		output = 'list',
		version = version,
		verbose = verbose, # only for initial CSW_GetRecords
		baseurl = baseurl
	)
	nrm_a <- cs_a$nrm
	nrr_a <- cs_a$nrr
	df_a <- cs_a$df
	while (nrr_a < nrm_a) {
		cs_a <- CSW_GetRecords(
			constraint = constraint,
			resultType =  'results',
			maxRecords = 100,
			startPosition = nrr_a + 1,
			typeNames = typeNames,
			ElementSetName = ElementSetName,
			# srsName = srsName,
			output = 'list',
			version = version,
			baseurl = baseurl
		)
		nrr_a <- nrr_a + cs_a$nrr
		df_a <- rbind(df_a, cs_a$df)
	}
	if (output == 'list') {
		return(
			list(
				'nrm' = nrr_a,
				'nrr' = nrr_a,
				df = df_a
			)
		)
	} else { # output == 'table'
		return(
			df_a
		)
	}
} # end CSW_GetRecordsAll

is_exception <- function (xml_doc) {
	if ( xml2::xml_name(xml_doc) == "ExceptionReport" )
		TRUE
	else
		FALSE
} # end is_exception

get_number <- function(my_node, type = 'matched') {
	if (type == 'matched')
		as.numeric(xml2::xml_attr(my_node,
			"numberOfRecordsMatched"))
	else
		as.numeric(xml2::xml_attr(my_node,
			"numberOfRecordsReturned"))
} # end get_number
