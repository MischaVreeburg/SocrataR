#####################################################################################################################################
###
###                                          opendata.socrata interface for R
###
###
###                                           Browse and Download data sets from Socrata
###
###                                           Mischa Vreeburg
###
###
#####################################################################################################################################

#### Search main page, not using api
#' @name  search.Socrata
#' @title search.Socrata
#' search.Socrata is an interface function to search and retrieve a listing of the datasets available on http://opendata.socrata.com/.
#'
#' search.Socrata returns a html page and uses the RCurl get method to retrieve the page.
#'
#' @param search Any search term can be entered. All data sets that have this term will be returned
#' @param topic Lists data sets that are related to the topic. Possible topics are: air, epa, filter, government, water, 2.0, 2009, air, aircraft, airplane, all+deals, apps, archaeology, art, asb, automatic+revocations, aviation, bfads, blackfriday, blog, book, books, bp, budget, business, calories, campaign, campaign+finance, cannon+land, cartridge, census, chronicle+of+philanthropy, church, city, community, congress, contacts, cooking, county, crash, dail, data, datos+abiertos, dc, deals, design, diet, directory, districts, do, doorbusters, drinking, early+bird, ecology, edmonton, education, empresarios, empresas, energy, energystar, enter+keywords, environment, epa, exercise, faa, faculty, fgcu, filter, finance, food, football, for+sale, fun, games, government, grants, green, hacs, health, health+care, history, hospital-acquired+infections, hospitals, house, infections, interests, internal+revenue+service, internet, ireland, irs, japan, lacy+cannon, library, lima, list, lots, management, media, medical+errors, milk, money, most, mousse, movie, movies, municipalidad+metropolitana+de+lima, music, myrtle+beach, national, ncopendata, network, new+york, ning, nonprofits, ny, octo, of, oil, oily+debris, online, open+data, open+government, opendata, parliament, payroll, personal, peru, points, politics, population, precipitation, project, projects, pullups, radiation, radnet, rain, rank, reading, recipes, recursos+educacion+internet, reduced+price+lunch, rugby, safety, salaries, salary, sales, sc, school, science, seattle, sediment, senate, shopping, sleet, snow, social, software, songs, sort, spill, state, statistics, stats, strike, subsistence, tar, td, teachers, tech+integration, technology, test, thanksgiving, title, to, todo, tools, top, trabajo, trabajo+mata, training, transparency, transportation, travel, tv, twitter, us, video, visitor+records, washington, waste, watchers, water, weathered+oil, web, web2.0, websites, weight, white+house, white+house+staff, whitehouse, wildlife, wine, work, world,
#' @param Limit Limits the results to datasets of the types. Possible limits are: datasets, blob, charts, calendars, href, filters, maps, forms
#' @param Category Lists all datasets within the category. Category limits are: Business, Fun, Personal, Education, Government
#' @param page The page to be retrieved.
#' @param sortby Datasets can be returned in a sorted order. Possible values are alphabetical, most relevant, most accessed, newest, highest rating, most comments, oldest.
#' @param sortPeriod Period over which the most accessed are determined. Only revelant when sortby = 'most accessed'
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' getListedDatasets(socrata.html)
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' socrata.html <- search.Socrata(topic = 'airplane')
#' getListedDatasets(socrata.html)
#'
#'
#' @rdname search.Socrata
#' @export search.Socrata

search.Socrata <- function(search = NULL,
                           topic = NULL,
                           Limit = NULL,
                           Category = NULL,
                           page = 1,
                           sortby = NULL,
                           sortPeriod = 'week'
                          ){
  require('RCurl')
  require('XML')
  ## setting curl options

  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           timeout = 100,
                           cainfo = cainfo
  )

  ## capath doesn't work:: NEED cainfo!
  ## test for existing cainfo:
  if (!file.exists(cainfo)){
    download.file('http://curl.haxx.se/ca/cacert.pem', cainfo )
  }
  ## test for age of cainfo, if older than 2 weeks get new.
  if (file.exists(cainfo)){
    file.inf.cainfo <- file.info(cainfo)
    age.cainfo <- Sys.time() - file.inf.cainfo[["mtime"]]
    if(as.numeric(age.cainfo, units="days") > 14 ){
      download.file('http://curl.haxx.se/ca/cacert.pem', cainfo )
    }
  }

  ### Make URL
  baseSocrataUrl <- 'http://opendata.socrata.com/browse'

  if(!is.null(Limit)){
    Limit <- match.arg( Limit, c('datasets', 'blob', 'charts', 'calendars', 'href', 'filters', 'maps', 'forms'))
  }
  if(!is.null(sortby)){
    sortby <- match.arg( sortby, c('alphabetical', 'most relevant', 'most accessed', 'newest', 'highest rating', 'most comments', 'oldest'))
  }
  if(!is.null(sortPeriod)){
    sortPeriod <- match.arg(sortPeriod, c('week', 'year', 'month'))
  }
  if(!is.null(Category)){
    Category <- match.arg( Category, c('Business', 'Fun', 'Personal', 'Education', 'Government'))
  }

  params <- list(
  category = Category,
  tag      = topic,
  limitTo  = Limit,
  q        = search,
  page     = page,
  sortby   = if( !is.null( sortby)){
              if(sortby == 'alphabetical'){
                sortby <- 'alpha'
              } else {
                if(sortby == 'most relevant'){
                  sortby <- 'relevance'
                } else {
                  if(sortby == 'most accessed'){
                    sortby <- 'most_accessed'
                  } else {
                    if(sortby == 'newest'){
                      sortby <- 'newest'
                    } else {
                      if(sortby == 'oldest'){
                        sortby <- 'oldest'
                      } else {
                        if(sortby == 'highest rating'){
                          sortby <- 'rating'
                        } else {
                          if(sortby == 'most comments'){
                            sortby <- 'comments'
                          }
                        }
                      }
                    }
                  }
                }
              }
            },
  sortPeriod = if( !is.null(sortby) ){
                sortPeriod
              } else {
                NULL
              }
  )
  params <- Filter(Negate(is.null), params)

  ### Retrieving html
  SocrataHtml <-  getForm(baseSocrataUrl, .params = params, curl = curl)
  assign('search.Socrata.Call', getCurlInfo(curl)$effective.url, envir=.GlobalEnv)

  rm(curl)
  gc()

  return(SocrataHtml)
}

### get links from search page
getLinks = function() {
  links = character()
  list(a = function(node, ...) {
    links <<- c(links, xmlGetAttr(node, "href"))
    node

  },
  links = function()links)
}

#' @name  getListedDatasets
#' @title getListedDatasets
#' getListedDatasets lists databases in the retrieved html from http://opendata.socrata.com/.
#'
#' getListedDatasets returns a dataframe with the datasets found. It returns max 10 per page
#'
#' @param SocrataHtml The html source retrieved by the \link{search.Socrata} function
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' getListedDatasets(socrata.html)
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' socrata.html <- search.Socrata(topic = 'airplane')
#' getListedDatasets(socrata.html)
#'
#'
#' @rdname getListedDatasets
#' @export getListedDatasets

### get listing of data sets
getListedDatasets <- function(SocrataHtml){

  h1 = getLinks()
  SocrataParsedLinks <- htmlTreeParse(SocrataHtml, useInternalNodes = TRUE, handlers = h1)

  SocrataLinks <- SocrataParsedLinks$links()

  SocrataPages <- SocrataLinks[grep('page', SocrataLinks)]

  ## cleanup links:

  if( length( grep('tags', SocrataLinks) != 0)){
    SocrataLinks <- SocrataLinks[-grep('tags', SocrataLinks)]
  }
  if( length( grep('category', SocrataLinks) != 0)){
    SocrataLinks <- SocrataLinks[-grep('category', SocrataLinks)]
  }
  if( length( grep('limit', SocrataLinks) != 0)){
    SocrataLinks <- SocrataLinks[-grep('limit', SocrataLinks)]
  }
  if( length( grep('page', SocrataLinks) != 0)){
    SocrataLinks <- SocrataLinks[-grep('page', SocrataLinks)]
  }
  if( length( grep('http', SocrataLinks) != 0)){
    SocrataLinks <- SocrataLinks[-grep('http', SocrataLinks)]
  }
  if( length( grep('#', SocrataLinks) != 0)){
    SocrataLinks <- SocrataLinks[-grep('#', SocrataLinks)]
  }
  if( length( grep('nominate', SocrataLinks) != 0)){
    SocrataLinks <- SocrataLinks[-grep('nominate', SocrataLinks)]
  }
  SocrataLinks <- unique(SocrataLinks[nchar(SocrataLinks) != 1])

  ### listing datasets
  datasets <- data.frame(
    t(
      structure(
        unlist(
          strsplit(
            substring(SocrataLinks, first = 2),
                   split= '/')
        ),
        dim = c(3, length(SocrataLinks)))
    ),
    stringsAsFactors = FALSE)
  names(datasets) <- c('Category', 'Dataset Name', 'ID')
  datasets$links <- SocrataLinks

  ## xmlValue a href
  ## div class="description"

  SocrataHtml2 <- gsub('\r','', gsub('\t','', SocrataHtml))
  SocrataHtml2 <- strsplit(SocrataHtml2, split = '\n')[[1]]


  ## div class="description"
  description <- SocrataHtml2[grep('class="description"', SocrataHtml2)]

  description <- substring(description,
                           first = matrix(unlist(lapply(strsplit(description, split = ''),
                                                        FUN = function(x){grep('>', x)}))+1, byrow = T, ncol = 2)[,1],
                           last = matrix(unlist(lapply(strsplit(description, split = ''),
                                                       FUN = function(x){grep('<', x)}))-1, byrow = T, ncol = 2)[,2]
  )

  ## div class="name"
  hrefNames <- SocrataHtml2[grep('class="name"', SocrataHtml2)+1]

  hrefNames <- substring(hrefNames,
                         first = matrix(unlist(lapply(strsplit(hrefNames, split = ''),
                                                      FUN = function(x){grep('>', x)}))+1, byrow = T, ncol = 2)[,1],
                         last = matrix(unlist(lapply(strsplit(hrefNames, split = ''),
                                                     FUN = function(x){grep('<', x)}))-1, byrow = T, ncol = 1)[,1]
  )

  datasets$'Dataset Name' <- gsub('&amp;', '&', curlUnescape( hrefNames))
  datasets$description <- curlUnescape( description)

  datasets <- datasets[c(2,5,1,3,4)]

  x = length(datasets[,1])
  if(x < 10){
    y = x
  } else {
    y = as.numeric(substring(SocrataPages[length(SocrataPages)],
                             first = grep('=',
                                          strsplit( SocrataPages[length(SocrataPages)],
                                                    split = '')[[1]]
                             )+1
    )
    )*10
  }

  message('listing ', x, ' of ', y, ' datasets')

  return(datasets)
}

## download data set

#' @name  getSocrataData
#' @title getSocrataData
#' getSocrataData lists databases in the retrieved html from http://opendata.socrata.com/.
#'
#' getSocrataData returns a dataframe of the selected dataset and optionally downloads the dataset.
#'
#' @param rownr The row in the datasetlist that has the dataset you want to load
#' @param datasetList The dataframe retrieved by the \link{getListedDatasets} function
#' @param filetype The file type of the data that shall be downloaded. Options are: csv, json, pdf, rdf, rss, xls, xlsx, xml
#' @param file The destination file where the data will be written
#' @param load.data Load the data into R?
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' data <- getListedDatasets(socrata.html)
#' getSocrataData(1, data, file = NULL)
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' socrata.html <- search.Socrata(topic = 'airplane')
#' data <- getListedDatasets(socrata.html)
#' getSocrataData(8, data, file = NULL)
#'
#' @rdname getSocrataData
#' @export getSocrataData

getSocrataData <- function(rownr,
                           datasetList,
                           filetype = 'csv',
                           file = paste(datasetList[rownr, 4], '.', filetype, sep=''),
                           load.data = TRUE
                          ){

  filetype <- match.arg(filetype, c('csv', 'json', 'pdf', 'rdf', 'rss', 'xls', 'xlsx', 'xml'))
  ## build URL
  baseURL <- 'http://opendata.socrata.com/views/'
  dataURL <- paste(baseURL, datasetList[rownr, 4], '/rows.', filetype, '?accessType=DOWNLOAD', sep = '')

  ## setting Curl options:
  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           timeout = 100
  )

  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  data = NULL
  if(!is.null(file)){
    if(!is.na(file)){
      if(load.data){
        download.file(dataURL, destfile = file.path(tempdir(), paste(datasetList[rownr, 4],'.', filetype, sep='')))
        data <- read.csv(file.path(tempdir(), paste(datasetList[rownr, 4],'.', filetype, sep='')))
      }
    }
  }

  if(!is.null(file)){
    if(!is.na(file)){
      download.file(dataURL, destfile = file)
      if(load.data){
        data <- read.csv(file)
      }
    }
  }

  rm(curl)
  gc()

  return(data)
}

#' @name  nextSocrataPage
#' @title nextSocrataPage
#' nextSocrataPage retrieves the next page from the current search.
#'
#' nextSocrataPage retrieves the html source page for the next page.
#'
#' @param search.Socrata.Call The URL call of the last html page retrieval.
#' @param Page The page number to retrieve. If not entered it will use next page.
#' @examples
#'
#' ## Not run
#' ## Retrieve basic page
#' socrata.html <- search.Socrata()
#' data <- getListedDatasets(socrata.html)
#' socrata1.html <- nextSocrataPage(search.Socrata.Call)
#' data1 <- getListedDatasets(socrata1.html)
#' print(data)
#' print(data1)
#'
#' @rdname nextSocrataPage
#' @export nextSocrataPage

nextSocrataPage <- function(search.Socrata.Call, Page = NULL){
  require('RCurl')
  require('XML')
  ## set Curl options
  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")
  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie ,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           timeout = 100,
                           cainfo = cainfo
                         )

  ## make new URl
  search.Socrata.Call <- strsplit(search.Socrata.Call, split = '?', fixed = TRUE)[[1]]
  search.Socrata.Call <- unlist(strsplit(search.Socrata.Call, split = '&'))
  pageNo <- gsub('page=', '', search.Socrata.Call[grep('page', search.Socrata.Call)])
  if(is.null(Page)){
    Page <- paste('page=', as.numeric(pageNo) + 1, sep = '')
  } else {
    Page <- paste('page=', Page, sep = '')
  }
  search.Socrata.Call[grep('page', search.Socrata.Call)] <- Page
  params = paste( search.Socrata.Call[2:length(search.Socrata.Call)], collapse = '&')
  search.Socrata.Call <- paste( search.Socrata.Call[1], params, sep = '?')

  SocrataHtml <- getURL(search.Socrata.Call, curl = curl )
  assign('search.Socrata.Call', getCurlInfo(curl)$effective.url, envir=.GlobalEnv)
  return(SocrataHtml)
}

#### Searching data using api.:
### Views Api:: https://opendata.socrata.com/api/docs/views

#' @name  search.Socrata.Views
#' @title search.Socrata.Views
#' search.Socrata.Views is an interface function to search and retrieve a listing of the datasets available on https://opendata.socrata.com/api/.
#'
#' search.Socrata.Views returns either an xml page with type = xml or a dataframe with type = json. and uses the RCurl get method to retrieve the page.
#'
#' @param search Any search term can be entered. All data sets that have this term in their metadata or contend will be returned.
#' @param topic Lists views containing this text in their description
#' @param name Lists views containing this text in their name
#' @param tags Lists data sets that are related to the tags Possible tags are: air, epa, filter, government, water, 2.0, 2009, air, aircraft, airplane, all+deals, apps, archaeology, art, asb, automatic+revocations, aviation, bfads, blackfriday, blog, book, books, bp, budget, business, calories, campaign, campaign+finance, cannon+land, cartridge, census, chronicle+of+philanthropy, church, city, community, congress, contacts, cooking, county, crash, dail, data, datos+abiertos, dc, deals, design, diet, directory, districts, do, doorbusters, drinking, early+bird, ecology, edmonton, education, empresarios, empresas, energy, energystar, enter+keywords, environment, epa, exercise, faa, faculty, fgcu, filter, finance, food, football, for+sale, fun, games, government, grants, green, hacs, health, health+care, history, hospital-acquired+infections, hospitals, house, infections, interests, internal+revenue+service, internet, ireland, irs, japan, lacy+cannon, library, lima, list, lots, management, media, medical+errors, milk, money, most, mousse, movie, movies, municipalidad+metropolitana+de+lima, music, myrtle+beach, national, ncopendata, network, new+york, ning, nonprofits, ny, octo, of, oil, oily+debris, online, open+data, open+government, opendata, parliament, payroll, personal, peru, points, politics, population, precipitation, project, projects, pullups, radiation, radnet, rain, rank, reading, recipes, recursos+educacion+internet, reduced+price+lunch, rugby, safety, salaries, salary, sales, sc, school, science, seattle, sediment, senate, shopping, sleet, snow, social, software, songs, sort, spill, state, statistics, stats, strike, subsistence, tar, td, teachers, tech+integration, technology, test, thanksgiving, title, to, todo, tools, top, trabajo, trabajo+mata, training, transparency, transportation, travel, tv, twitter, us, video, visitor+records, washington, waste, watchers, water, weathered+oil, web, web2.0, websites, weight, white+house, white+house+staff, whitehouse, wildlife, wine, work, world,
#' @param category Lists all datasets within the category. Category limits are: Business, Fun, Personal, Education, Government
#' @param count Boolean. If count is true then it returns the total number of rows, and ignores the limit parameter
#' @param limit Limits the total number of results to return, max 200.
#' @param page The page to be retrieved.
#' @param type The type of response format. Supports json and xml.
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#'
#'
#'
#' @rdname search.Socrata.Views
#' @export search.Socrata.Views

search.Socrata.Views <- function(search = NULL, ## full
                                 topic = NULL, ## description
                                 name = NULL, ## title field search
                                 tags = NULL,
                                 category = NULL,
                                 count = FALSE,
                                 limit = 10, ## max 200
                                 page = 1,
                                 type = "json" ## can also be xml
                                ){

  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  ## capath doesn't work:: NEED cainfo!
  ## test for existing cainfo:
  if (!file.exists(cainfo)){
    download.file('http://curl.haxx.se/ca/cacert.pem', cainfo )
  }
  ## test for age of cainfo, if older than 2 weeks get new.
  if (file.exists(cainfo)){
    file.inf.cainfo <- file.info(cainfo)
    age.cainfo <- Sys.time() - file.inf.cainfo[["mtime"]]
    if(as.numeric(age.cainfo, units="days") > 14 ){
      download.file('http://curl.haxx.se/ca/cacert.pem', cainfo )
    }
  }

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views.'

  if(!is.null(category)){
    category <- match.arg( category, c('Business', 'Fun', 'Personal', 'Education', 'Government'))
  }
  type <- match.arg( type, c('json', 'xml'))

  baseSocrataUrl <- paste( baseSocrataUrl, type, sep = '')

  params <- list(
              category      = category,
              tags          = tags,
              limit         = min(limit, 200),
              full          = search,
              page          = page,
              description   = topic,
              name          = name,
              count         = count
            )
  params <- Filter(Negate(is.null), params)

  ### Retrieving html
  SocrataHtml <-  getForm(baseSocrataUrl, .params = params, curl = curl)
  assign('search.Socrata.Call', getCurlInfo(curl)$effective.url, envir=.GlobalEnv)

  if(type == 'json'){
    SocrataTable <- fromJSON(SocrataHtml)
    SocrataTable <- lapply( SocrataTable, function(x){data.frame( x, stringsAsFactors = FALSE) } )
    SocrataTable.df <- data.frame( matrix( nrow = length( SocrataTable), ncol = max(unlist(lapply(SocrataTable, length) ) ) ) )
    names(SocrataTable.df) <- names( SocrataTable [lapply( SocrataTable, length ) == max( unlist( lapply( SocrataTable, length) ) ) ] [[1]] )
    for( i in 1: length( SocrataTable ) ){
      for( j in 1: length( names( SocrataTable[[i]] ) ) ){
        SocrataTable.df[i, names(SocrataTable[[i]])[j]] <- SocrataTable[[i]][i, names( SocrataTable[[i]] ) [j] ]
      }
    }

    rm(curl)
    gc()

    return(SocrataTable.df)
  } else {

    rm(curl)
    gc()

    return(SocrataHtml)
  }
}

#' @name  get.Metadata.View
#' @title get.Metadata.View
#' get.Metadata.View returns the metadata associated with the view.
#'
#' get.Metadata.View returns the metadata associated with the view.
#'
#' @param viewID The ViewId for the view can be found in the first column of the \link{search.Socrata.Views} output.
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#' \dontrun{airplane1 <- get.Metadata.View(socrata.views[1,1])}
#' \dontrun{print(airplane1)}
#'
#' @rdname get.Metadata.View
#' @export get.Metadata.View

get.Metadata.View <- function(viewID){
  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views/'

  ### Retrieving html
  SocrataUrl <- paste( baseSocrataUrl, viewID, '.json', sep = '')

  SocrataHtml <- getURL(SocrataUrl, curl = curl)

  ### parse json data
  SocrataMetadataList <- fromJSON(SocrataHtml)
  # return(SocrataMetadataList)

  SocrataMetadataData <- data.frame( matrix( nrow = length( SocrataMetadataList ), ncol = length( SocrataMetadataList[["owner"]] ) + 1 + length( SocrataMetadataList[["grants"]] [[1]] ) ), row.names = names( SocrataMetadataList ) )

  names( SocrataMetadataData ) <- c( NA, names( SocrataMetadataList[["owner"]] ), names( SocrataMetadataList[["grants"]] [[1]] ) )

  for( i in 1 : length(  SocrataMetadataList ) ){
    if( length( SocrataMetadataList [[i]] ) == 0 ){
      SocrataMetadataData[ names( SocrataMetadataList ) [[i]], 1] <- NA
    } else {
      if( length( SocrataMetadataList [[i]] ) > 1 ){
        if(names( SocrataMetadataList ) [i] == 'columns'){
          SocrataMetadataData[ names( SocrataMetadataList ) [[i]], 1] <- length(SocrataMetadataList [i])
        } else {
          if(names( SocrataMetadataList ) [i] == 'owner' | names( SocrataMetadataList ) [i] == 'tableAuthor' ){
            Metadata <- data.frame( SocrataMetadataList [[i]], stringsAsFactors = FALSE)
            SocrataMetadataData[ names( SocrataMetadataList ) [[i]], 1] <- Metadata [1, 2]
            for( j in 1 : length( Metadata ) ){
              if( names( SocrataMetadataList [[i]] ) [j] == 'rights' ){
                if ( length( SocrataMetadataList [[i]] [[j]] ) > 1){
                  SocrataMetadataData[ names( SocrataMetadataList ) [[i]], names( Metadata ) [[j]] ] <- paste( Metadata[, j], collapse = ', ')

                } else {
                  SocrataMetadataData[ names( SocrataMetadataList ) [[i]], names( Metadata ) [[j]] ] <- Metadata[1, j]
                }
              } else {
                SocrataMetadataData[ names( SocrataMetadataList ) [[i]], names( Metadata ) [[j]] ] <- Metadata[1, j]
              }
            }
          }
        }
      } else {
        if(names( SocrataMetadataList ) [i] == 'grants' ){
          Metadata <- data.frame( SocrataMetadataList [[i]], stringsAsFactors = FALSE)
          SocrataMetadataData[ names( SocrataMetadataList ) [[i]], 1] <- Metadata [, 1]
          for( j in 1 : length( Metadata ) ){
            SocrataMetadataData[ names( SocrataMetadataList ) [[i]], names( Metadata ) [[j]] ] <- Metadata[, j]
          }
        } else {
          SocrataMetadataData[ names( SocrataMetadataList ) [[i]], 1] <- SocrataMetadataList [[i]]
        }
      }
    }
  }

  rm(curl)
  gc()

  return( SocrataMetadataData )
}

#' @name  get.Column.View
#' @title get.Column.View
#' get.Column.View returns metadata about all of the columns on a given view.
#'
#' get.Column.View returns metadata about all of the columns on a given view.
#'
#' @param viewID The ViewId for the view can be found in the first column of the \link{search.Socrata.Views} output.
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#' \dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
#' \dontrun{print(airplane1)}
#'
#' @rdname get.Column.View
#' @export get.Column.View

get.Column.View <- function(viewID){
  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views/'

  ### Retrieving html
  SocrataUrl <- paste( baseSocrataUrl, viewID, '/columns.json', sep = '')

  SocrataHtml <- getURL(SocrataUrl, curl = curl)

  ### parse json data
  SocrataColumnList <- fromJSON(SocrataHtml)

  SocrataColumndata <- data.frame( matrix( nrow = length( SocrataColumnList ), ncol = max( unlist( lapply( SocrataColumnList, length ) ) ) ) )

  names(SocrataColumndata ) <- names( SocrataColumnList [lapply( SocrataColumnList, length ) == max( unlist( lapply( SocrataColumnList, length) ) ) ] [[1]] )

  for( i in 1 : length( SocrataColumnList ) ){
    for( j in 1 : length( names( SocrataColumnList[[i]] ) ) ){
      if(length(SocrataColumnList [[i]] [[j]] ) == 0 ){
        SocrataColumndata[i, names(SocrataColumnList[[i]])[j]] <- NA
      } else{
        if(length(SocrataColumnList [[i]] [[j]] ) > 1 ){
          col.data <- data.frame( SocrataColumnList [[i]] [[j]] , stringsAsFactors = FALSE )
          if( names( col.data )[1] == names( SocrataColumndata ) [1] ){ ## Catch formatting error!
            col.data <- SocrataColumnList [[i]] [[j]]
            for( l in 1 : length( col.data ) ){
              if(length( SocrataColumnList ) > length( SocrataColumndata[, 1] ) ){
                O <- length( SocrataColumnList ) + 1
              } else {
                O <-  length( SocrataColumndata[, 1] ) + 1
              }
              for( m in 1 : length( names( col.data [[l]] ) ) ){
                if( length( col.data [[l]] [[m]] ) == 0 ){
                  SocrataColumndata[O, names( col.data[[l]] ) [m] ] <- NA
                } else{
                  if( length( col.data [[l]] [[m]] ) > 1 ){
                    col.data2 <- data.frame( col.data [[l]] [[m]] , stringsAsFactors = FALSE)
                    names( col.data2 ) <- paste( names( col.data[[l]] ) [m] , names( col.data2 ), sep = '-')
                    for( n in 1 : length( col.data2 ) ){
                      if( names( col.data2 ) [[n]] %in% names( SocrataColumndata ) ){
                        SocrataColumndata[[ names( col.data2 ) [[n]] ]] <- NA
                        SocrataColumndata[O, names( col.data2 ) [n] ] <- col.data2[, n]
                      } else {
                        SocrataColumndata[O, names( col.data2 ) [n] ] <- col.data2[, n]
                      }
                    }
                  } else {
                    SocrataColumndata[O, names( col.data [[l]] ) [m] ] <- col.data [[l]] [ names( col.data [[l]] ) [m] ]
                  }
                }
              }
            }
          } else {
            names(col.data) <- paste(names(SocrataColumnList[[i]])[j] , names(col.data), sep = '-')
            for( k in 1 : length( col.data ) ){
              if( names( col.data ) [[k]] %in% names( SocrataColumndata ) ){
                SocrataColumndata[[ names( col.data) [[k]] ]] <- NA
                SocrataColumndata[i, names( col.data ) [k] ] <- col.data[, k]
              } else {
                SocrataColumndata[i, names( col.data ) [k] ] <- col.data[, k]
              }
            }
          }
        } else {
          SocrataColumndata[i, names(SocrataColumnList[[i]])[j] ] <- SocrataColumnList[[i]][ names( SocrataColumnList[[i]] ) [j] ]
        }
      }
    }
  }

  rm(curl)
  gc()

  return(SocrataColumndata)
}

#' @name  get.Single.Column.View
#' @title get.Single.Column.View
#' get.Single.Column.View returns metadata about a single column on a given view.
#'
#' get.Single.Column.View returns metadata about a single column on a given view.
#'
#' @param viewID The ViewId for the view can be found in the first column of the \link{search.Socrata.Views} output.
#' @param columnID The columnID for the columns can be found in the first column of the \link{get.Column.View} output.
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#' \dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
#' \dontrun{airplane2 <- get.Single.Column.View(socrata.views[1,1], airplane1[1,1])}
#' \dontrun{print(airplane2)}
#'
#' @rdname get.Single.Column.View
#' @export get.Single.Column.View

get.Single.Column.View <- function(viewID, columnID){
  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views/'

  ### Retrieving html
  SocrataUrl <- paste( baseSocrataUrl, viewID, '/columns/', columnID, '.json', sep = '')

  SocrataHtml <- getURL(SocrataUrl, curl = curl)

  ### parse json data
  SocrataColumnList <- fromJSON(SocrataHtml)

  rm(curl)
  gc()

  return(SocrataColumnList)
}

#' @name  get.Sub.Column.View
#' @title get.Sub.Column.View
#' get.Sub.Column.View returns metadata about the subcolumns of a single column on a given view.
#'
#' get.Sub.Column.View returns metadata about the subcolumns of a single column on a given view.
#'
#' @param viewID The ViewId for the view can be found in the first column of the \link{search.Socrata.Views} output.
#' @param columnID The columnID for the columns can be found in the first column of the \link{get.Column.View} output.
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#' \dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
#' \dontrun{airplane2 <- get.Sub.Column.View(socrata.views[1,1], airplane1[1,1])}
#' \dontrun{print(airplane1)}
#' \dontrun{print(airplane2)}
#'
#' @rdname get.Sub.Column.View
#' @export get.Sub.Column.View

get.Sub.Column.View <- function(viewID, columnID){
  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views/'

  ### Retrieving html
  SocrataUrl <- paste( baseSocrataUrl, viewID, '/columns/', columnID, '/sub_columns.json', sep = '')

  SocrataHtml <- getURL(SocrataUrl, curl = curl)

  ### parse json data
  SocrataColumnList <- fromJSON(SocrataHtml)

  rm(curl)
  gc()

  return(SocrataColumnList)
}

#### for full api:: where to get fileID

#' @name  get.Rows.View
#' @title get.Rows.View
#' get.Rows.View Retrieve multiple rows from a view.
#'
#' get.Rows.View  Retrieve multiple rows from a view as nested arrays instead of in the expanded form. By default this will return all the rows in the view, but you can limit the number of rows returned with max_rows or request only specific rows using ids. You can also request only row IDs by passing row_ids_only. You must have read permissions on the view in order to access this resource.
#'
#' @param viewID The ViewId for the view can be found in the first column of the \link{search.Socrata.Views} output.
#' @param row_ids_only If true, the service will only return row IDs.
#' @param max_rows Limit the number of rows returned.
#' @param start Start retrieving rows from row number...
#' @param length The number of rows to be retrieved.
#' @param ids The row ids to be retrieved. Can be a vector of ids.
#' @param include_ids_after Include this number of rows, after which only row IDs are returned.
#' @param search Run a full text search on the view and only return rows/ids that match.
#' @param meta If set to 'true', will write the view object. Only valid if rendering JSON. Default is true.
#' @param as_hashes If set to 'true', write fields in hash format. Otherwise, write fields in array. Only valid if rendering JSON. Default to false (array).
#' @param exclude_system_fields If set to 'true', do not return system fields like row id. Only valid if rendering JSON. Default to false.
#' @param unwrapped If set to 'true', return just array or rows with no outer object. Only valid if rendering JSON. Default to false.
#' @param most_recent If set to 'true', return only the most recent rows added to the dataset. Only valid if rendering RSS. Default to true.
#' @param access_type Valid values for the channel are PRINT, EMAIL, API, RSS, WIDGET, DOWNLOAD, WEBSITE.
#' @param filetype Can be 'csv', 'json', 'pdf', 'rdf', 'rss', 'xls', 'xlsx', 'xml'.
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#' \dontrun{airplane1 <- get.Column.View(socrata.views[1,1])}
#' \dontrun{airplane_rows <- get.Rows.View(socrata.views[1,1], airplane1[1,1])}
#' \dontrun{print(airplane_rows)}
#'
#' @rdname get.Rows.View
#' @export get.Rows.View

get.Rows.View <- function(viewID,
                          row_ids_only = FALSE, ## If true, the service will only return row IDs
                          max_rows = NULL,
                          include_ids_after = NULL,
                          search = NULL,
                          start = NULL,
                          length = NULL,
                          ids = NULL, ## can be vector of
                          meta = TRUE, ## Only valid if rendering JSON. Default is true.
                          as_hashes = FALSE, ## Only valid if rendering JSON. Default to false (array).
                          exclude_system_fields = FALSE, ## Only valid if rendering JSON. Default to false.
                          unwrapped = FALSE, ## Only valid if rendering JSON. Default to false.
                          most_recent = FALSE, ## Only valid if rendering RSS Default to false.
                          access_type =  NULL,
                          filetype = 'json' ## can be 'csv', 'json', 'pdf', 'rdf', 'rss', 'xls', 'xlsx', 'xml'
                          ){

  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  filetype <- match.arg(filetype, c('csv', 'json', 'pdf', 'rdf', 'rss', 'xls', 'xlsx', 'xml'))

  if(!is.null(access_type)){
    access_type <- match.arg(access_type, c('print', 'email', 'api', 'rss', 'widget', 'download', 'website', 'PRINT', 'EMAIL', 'API', 'RSS', 'WIDGET', 'DOWNLOAD', 'WEBSITE'))
  }

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views/'

  baseSocrataUrl <- paste( baseSocrataUrl, viewID, '/rows.', filetype, sep = '')

  params <- list(
    row_ids_only            = row_ids_only,
    max_rows                = max_rows,
    include_ids_after       = include_ids_after,
    search                  = search,
    start                   = start,
    length                  = length,
    ids                     = ids,
    meta                    = meta,
    as_hashes               = as_hashes,
    exclude_system_fields   = exclude_system_fields,
    unwrapped               = unwrapped,
    most_recent             = most_recent,
    access_type             = access_type
            )
  params <- Filter(Negate(is.null), params)

  ### Retrieving html

  if(filetype %in% c('csv', 'json', 'xml')){
    SocrataRows <- getForm( baseSocrataUrl, .params = params, curl = curl) #getURL(SocrataUrl, curl = curl)
    SocrataRows <- fromJSON( SocrataRows )

    rm(curl)
    gc()

    return(SocrataRows)
  } else {

    rm(curl)
    gc()

    Downloaded.file <- getBinaryURL(baseSocrataUrl, .params = params, curl = curl)
    writeBin(Downloaded.file, paste(viewID, filetype, sep = ''))
  }
}

#' @name  get.Single.Row.View
#' @title get.Single.Row.View
#' get.Single.Row.View Retrieve the expanded representation of a particular row by ID.
#'
#' get.Single.Row.View Retrieve the expanded representation of a particular row by ID. You must have read permissions on the view in order to access this resource.
#'
#' @param viewID The ViewId for the view can be found in the first column of the \link{search.Socrata.Views} output.
#' @param rowID The rowID for the view can be found in the first column of the \link{get.Rows.View} output.
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#' \dontrun{airplane1 <- get.Rows.View(socrata.views[1,1], row_ids_only = TRUE, meta = FALSE)}
#' \dontrun{airplane2 <- get.Rows.View(socrata.views[1,1], ids = data.frame(airplane1)[1, 1] , meta = FALSE)}
#' \dontrun{airplane_row <- get.Single.Row.View(socrata.views[1,1], data.frame(airplane1)[1, 1])}
#' \dontrun{print(t(data.frame(unlist(airplane2 ))))}
#' \dontrun{print(data.frame(airplane_row))}
#'
#' @rdname get.Single.Row.View
#' @export get.Single.Row.View

get.Single.Row.View <- function(viewID, rowID){
  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views/'

  ### Retrieving html
  SocrataUrl <- paste( baseSocrataUrl, viewID, '/rows/', rowID, '.json', sep = '')

  SocrataHtml <- getURL(SocrataUrl, curl = curl)

  ### parse json data
  SocrataRowList <- fromJSON(SocrataHtml)

  rm(curl)
  gc()

  return(SocrataRowList)
}

#' @name  get.Tags.View
#' @title get.Tags.View
#' get.Tags.View Allows you to retrieve tags for a view .
#'
#' get.Tags.View Allows you to retrieve tags for a view .
#'
#' @param viewID The ViewId for the view can be found in the first column of the \link{search.Socrata.Views} output.
#' @param userTags Retrieve the user tags associated with the view .
#' @examples
#'
#'
#' ## Not run
#' ## Retrieve datasets related to airplane
#' \dontrun{socrata.views <- search.Socrata.Views(topic = 'airplane')}
#' \dontrun{airplane1 <- get.Tags.View(socrata.views[1, 1])}
#' \dontrun{airplane_tags <- get.Tags.View(socrata.views[1, 1])}
#' \dontrun{print(airplane_tags)}
#'
#' @rdname get.Tags.View
#' @export get.Tags.View

get.Tags.View <- function(viewID, userTags = FALSE){
  require('RCurl')
  require('XML')
  require('rjson')

  ## setting curl options
  capath = system.file("CurlSSL",package = "RCurl")
  cainfo = system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")

  cookie = 'cookiefile.txt'
  curl  =  getCurlHandle ( cookiefile = cookie,
                           cookiejar = cookie,
                           useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                           header = FALSE,
                           verbose = TRUE,
                           netrc = FALSE,
                           maxredirs = as.integer(20),
                           followlocation = TRUE,
                           ssl.verifypeer = TRUE,
                           cainfo = cainfo,
                           timeout = 100
  )

  ### Make URL
  baseSocrataUrl <- 'https://opendata.socrata.com/api/views/'

  ### Retrieving html
  if(!userTags){
    SocrataUrl <- paste( baseSocrataUrl, viewID, '/tags.json', sep = '')
  } else {
    SocrataUrl <- paste( baseSocrataUrl, viewID, '/user_tags.json', sep = '')
  }

  SocrataHtml <- getURL(SocrataUrl, curl = curl)

  ### parse json data
  SocrataTagsList <- fromJSON(SocrataHtml)

  rm(curl)
  gc()

  return(SocrataTagsList)
}

