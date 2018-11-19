# R-package CSW

## Introduction: Catalog Services for the Web (CSW) {#introduction}
I ran into the Catalog Services for the Web (CSW) when I wanted to see which public geographical data was available for the Netherlands: the [Nationaal GeoRegister (NGR)](http://nationaalgeoregister.nl/geonetwork/srv/dut/search) 
makes use of it. Apart from showing available datasets, the page also gives information about the underlying technology: CSW. 

On the [Catalogue Service](http://www.opengeospatial.org/standards/cat) page of the 
[Open Geospatial Consortium (OGC)](http://www.opengeospatial.org/) 
[Standards](http://www.opengeospatial.org/docs/is) page the full
[Specification](http://portal.opengeospatial.org/files/?artifact_id=20555) for  the latest version (2.0.2) can be found. See the [References](#references) section for more links. 

The [Catalogue Service](http://www.opengeospatial.org/standards/cat) page describes: 

> Catalogue services support the ability to publish and search collections of descriptive information (metadata) for data, services, and related information objects. Metadata in catalogues represent resource characteristics that can be queried and presented for evaluation and further processing by both humans and software. Catalogue services are required to support the discovery and binding to registered information resources within an information community.

## R package CSW {#r-package-csw}
One of the ways to get information about and from a CSW catalog is the GET method of the HTTP protocol. By specifying a properly formed URL in a internet browser the requested information is shown in the browser window.  

The `CSW`&nbsp;R&nbsp;package provides functions that use the same GET method to place the result in an&nbsp;R&nbsp;object as a data.frame, xml_document or integer variable.  
`Operations` are recognized by the CSW interface in the URL by the phrase `request=`. The package has functions for the **read-only** `Operations`. Also included are some utility functions.  

This package is until now only tested on the 
[Nationaal GeoRegister(NGR)](http://nationaalgeoregister.nl/geonetwork/srv/dut/search) 
catalog with CSW 2.0.2. By using the `CSW_set_url` and `CSW_set_version` functions these values will be replaced by the ones provided by the user.\cr\cr

Let me know if you encounter problems with other catalogs: maybe these can be solved.

## Install CSW

devtools::install_github("HanOostdijk/CSW",build_vignettes = T)

## References {#references}

- Nationaal GeoRegister (NGR) : hub for location spatial information for the Netherlands (in Dutch)  
[  http://nationaalgeoregister.nl/geonetwork/srv/dut/search](http://nationaalgeoregister.nl/geonetwork/srv/dut/search)  
- Publieke Dienstverlening op de Kaart (PDOK) :   platform for open spatial data (in Dutch)  
[  https://www.pdok.nl/](https://www.pdok.nl/)  
- Description APIs (including CSW) for PDOK environment (in Dutch)   
[  https://pdok-ngr.readthedocs.io/](https://pdok-ngr.readthedocs.io/).   
- Description of GeoServer services (including CSW)  
[  https://docs.geoserver.org/latest/en/user/services/index.html](https://docs.geoserver.org/latest/en/user/services/index.html)
- Catalogue Service page of the Open Geospatial Consortium (OGC)  
[  http://www.opengeospatial.org/standards/cat](http://www.opengeospatial.org/standards/cat)  
- Specification of Catalog Services for the Web (CSW) version 2.0.2  
[  http://portal.opengeospatial.org/files/?artifact_id=20555](http://portal.opengeospatial.org/files/?artifact_id=20555)  
- GeoServer Tutorial section about queries  
[  https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html](https://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html)  
- GeoServer reference for ECQL queries  
[  https://docs.geoserver.org/stable/en/user/filter/ecql_reference.html#filter-ecql-reference](https://docs.geoserver.org/stable/en/user/filter/ecql_reference.html#filter-ecql-reference)  
- Examples for DescribeRecords by [FrieseWoudloper](https://twitter.com/FrieseWoudloper)  
[https://gist.github.com/FrieseWoudloper/b7cad022cb75ba531ebbececf5fc85db](https://gist.github.com/FrieseWoudloper/b7cad022cb75ba531ebbececf5fc85db)  


## See also
To my knowledge no other R packages concerning CSW  exist. 

