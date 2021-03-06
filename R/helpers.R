#' Locate descriptor
#' @description Helper function to locate descriptor.
#' @param descriptor descriptor
#' @rdname locateDescriptor
#' @export
#'

locateDescriptor <- function(descriptor) {
  
  # Infer from path/url
  if (is.character(descriptor) && !isTRUE(jsonlite::validate(descriptor))) {
    
    path <- strsplit(descriptor, '/')
    path <- path[[1]]
    path <- rlist::list.remove(path, length(path) )
    path <- paste(path, collapse = "/")
    if (is.null(path)) {
      basePath <- getwd()
    }
    else{
      basePath <- path
    }
    
  } else{
    basePath <- getwd()
  }
  return(basePath)
}

#' Retrieve descriptor
#' @description Helper function to retrieve descriptor.
#' @param descriptor descriptor
#' @rdname retrieveDescriptor
#' @export
#'

retrieveDescriptor <- function(descriptor) {
  
  if (is.list(descriptor)) {
    return(descriptor)
  }
  
  if (is.json(descriptor)) {
    descriptor <- helpers.from.json.to.list(descriptor)
    return(descriptor)
  }
  
  if (is.character(descriptor)) {
    if (jsonlite::validate(descriptor)) {
      return(helpers.from.json.to.list(descriptor))
    }
    # Remote
    if (isRemotePath(descriptor)) {
      tryCatch({
        response <- httr::GET(descriptor)
        descriptor <- httr::content(response, as = 'text')
        descriptor <- helpers.from.json.to.list(descriptor)
        return(descriptor)
      },
      
      error = function(e) {
        message <- stringr::str_interp('Can not retrieve remote descriptor "${descriptor}"')
        stop(message)
      })
    }
    
    else {
      tryCatch({
        if (endsWith(descriptor,"csv")) descriptor <- as.list(utils::read.csv(descriptor,as.is = TRUE))
        else descriptor <- helpers.from.json.to.list(descriptor)
        return(descriptor)
      },
      
      error = function(e) {
        message <- stringr::str_interp('Can not retrieve local descriptor "${descriptor}"')
        stop(message)
      },
      
      warning = function(e) {
        message <- stringr::str_interp('Can not retrieve local descriptor "${descriptor}"')
        stop(message)
      })
    }
  } else
    stop(message)
  # descriptor$resources <- purrr::flatten(descriptor$resources)
}

#' Dereference package descriptor
#' @description Helper function to dereference package descriptor.
#' @param descriptor descriptor
#' @param basePath basePath
#' @rdname dereferencePackageDescriptor
#' @export
#'

dereferencePackageDescriptor <- function(descriptor, basePath) {
  
  for (i in 1:length(descriptor$resources)) {
    descriptor$resources[[i]] <- dereferenceResourceDescriptor(descriptor = descriptor$resources[[i]], basePath = basePath, baseDescriptor = descriptor)
  }
  return(descriptor)
}

#' Dereference resource descriptor
#' @description Helper function to dereference resource descriptor.
#' @param descriptor descriptor
#' @param basePath basePath
#' @param baseDescriptor baseDescriptor
#' @rdname dereferenceResourceDescriptor
#' @export
#'

dereferenceResourceDescriptor <- function(descriptor, basePath, baseDescriptor = NULL) {
  #conditions
  
  if (isTRUE(is.null(baseDescriptor))){
    baseDescriptor <- descriptor
  }
  
  #set list properties
  PROPERTIES <- list('dialect','schema')
  
  
  for (property in PROPERTIES) {
    value <- descriptor[[property]]    
    # URI -> No
    if (!is.character(value)) {
      next
      
      
      # URI -> Pointer
    } else if (isTRUE(startsWith(unlist(value), '#'))) {
      
      descriptor[[property]]  <- tryCatch({
        descriptor.pointer(value, baseDescriptor)},
        error = function(e) {
          stop(stringr::str_interp(
            'Not resolved Pointer URI "${value}" for resource[[${property}]]'
          ))},
        warning = function(e){
          stop(stringr::str_interp(
            
            'Not resolved Pointer URI "${value}" for resource[[${property}]]'
          ))}
      )
      # URI -> Remote
    } else if (isTRUE(basePath) || isRemotePath(unlist(value))) {
      
      value <- paste0(basePath,'/',value)
      
    } else if (isRemotePath(unlist(value))) {
      tryCatch({
        # response <- httr::GET(value)
        descriptor[[property]] <- helpers.from.json.to.list(value)
        #httr::content(response, as <- 'text')
      },
      error = function(e) {
        message <- DataPackageError$new(
          stringr::str_interp(
            'Not resolved Remote URI "${value}" for descriptor[[${property}]]'
          ))$message
        
        stop(message)
      })
      
      # URI -> Local
    } 
    else {
      if (isTRUE(!isSafePath(unlist(value)))) {
        message <- DataPackageError$new(
          stringr::str_interp(
            'Not safe path in Local URI "${value}" for resource[[${property}]]'
          )
        )$message
        
        stop(message)
      }
      if (isTRUE(is.null(basePath) || basePath == "")) {
        message <- DataPackageError$new(
          stringr::str_interp(
            'Local URI "${value}" requires base path for resource[[${property}]]'
          )
        )$message
        
        stop(message)
      }
      
      tryCatch({
        # TODO: support other that Unix OS
        fullPath <- stringr::str_c(basePath, value, sep = '/')
        # TODO: rebase on promisified fs.readFile (async)
        descriptor[[property]] <- helpers.from.json.to.list(fullPath)
        # contents <- readLines(fullPath, 'utf-8')
        # descriptor[[property]] <- jsonlite::fromJSON(contents)
      },
      error = function(e) {
        message <- DataPackageError$new(
          stringr::str_interp(
            'Not resolved Local URI "${value}" for resource[[${property}]]'
          )
        )$message
        stop(message)
      },  
      warning = function(e)
      {
        message <- DataPackageError$new(
          stringr::str_interp(
            'Not resolved Local URI "${value}" for resource[[${property}]]'
          )
        )$message
        stop(message)
      })
    }
  }
  return(descriptor)
}


#' Expand package descriptor
#' @description Helper function to expand package descriptor.
#' @param descriptor descriptor
#' @rdname expandPackageDescriptor
#' @export
#'

expandPackageDescriptor <- function(descriptor) {
  
  descriptor$profile <- if (is.empty(descriptor$profile)) {
    config::get("DEFAULT_DATA_PACKAGE_PROFILE", file = system.file("config/config.yaml", package = "datapackage.r"))
  } else {
    descriptor$profile
  }
  if (length(descriptor$resources) > 0) {
    # if (length(descriptor$resources)==0) index = 1 else index = length(descriptor$resources)
    for (i in 1:length(descriptor$resources)) {
      descriptor$resources[[i]] <- expandResourceDescriptor(descriptor$resources[[i]])
    }
  }
  
  return(descriptor)
}

#' Expand resource descriptor
#' @description Helper function to expand resource descriptor.
#' @param descriptor descriptor
#' @rdname expandResourceDescriptor
#' @export
#'
expandResourceDescriptor <- function(descriptor) {
  
  # set default for profile and encoding
  
  descriptor$profile <- if (isTRUE(is.null(descriptor$profile))) {
    config::get("DEFAULT_RESOURCE_PROFILE", file = system.file("config/config.yaml", package = "datapackage.r"))
  } else {
    descriptor$profile
  }
  
  descriptor$encoding <- if (isTRUE(is.null(descriptor$encoding))) {
    config::get("DEFAULT_RESOURCE_ENCODING", file = system.file("config/config.yaml", package = "datapackage.r"))
  } else {
    descriptor$encoding
  }
  
  # tabular-data-resource
  if (isTRUE(descriptor$profile == 'tabular-data-resource')) {
    
    # Schema
    
    if (isTRUE(!is.null(descriptor$schema))) {
      
      for (i in 1:length(descriptor$schema$fields)) {
        
        
        
        descriptor$schema$fields[[i]]$type <- if (is.empty(descriptor$schema$fields[[i]]$type))
          config::get("DEFAULT_FIELD_TYPE", file = system.file("config/config.yaml", package = "datapackage.r"))
        else {
          descriptor$schema$fields[[i]]$type
        }
        descriptor$schema$fields[[i]]$format <- if (is.empty(descriptor$schema$fields[[i]]$format)) {
          config::get("DEFAULT_FIELD_FORMAT", file = system.file("config/config.yaml", package = "datapackage.r"))
        } else {
          descriptor$schema$fields[[i]]$format
        }
        
      }
      
      descriptor$schema$missingValues <- if (is.empty(descriptor$schema$missingValues)) {
        as.list(config::get("DEFAULT_MISSING_VALUES", file = system.file("config/config.yaml", package = "datapackage.r")))
      } else {
        descriptor$schema$missingValues
      }
    }
    
    # Dialect
    
    # if (isTRUE(!is.null(descriptor$dialect))) {
    #   #descriptor$dialect <- config::get("DEFAULT_DIALECT", file = system.file("config/config.yaml", package = "datapackage.r"))
    #   # descriptor$dialect$lineTerminator<-"\r\n"
    #   # descriptor$dialect$quoteChar<-"\""
    #   # descriptor$dialect$escapeChar<-"\\"
    #   
    #   for (key in which(!names(config::get("DEFAULT_DIALECT", file = system.file("config/config.yaml", package = "datapackage.r"))) %in% names(descriptor$dialect))) {
    #     # if (!names(config::get("DEFAULT_DIALECT", file = system.file("config/config.yaml", package = "datapackage.r")))[key] %in% names(descriptor$dialect)) {
    #     
    #     descriptor$dialect[[
    #       paste(names(config::get("DEFAULT_DIALECT", file = system.file("config/config.yaml", package = "datapackage.r")))[key])
    #       ]] <- config::get("DEFAULT_DIALECT", file = system.file("config/config.yaml", package = "datapackage.r"))[key]
    #   }
    #   descriptor$dialect <- lapply(descriptor$dialect, unlist, use.names = FALSE)
    #   #}
    # }
    
    
    
    dialect <- descriptor$dialect
    if (isTRUE(!is.null(dialect))) {
      
        for (key in names(filterDefaultDialect(validateDialect(dialect)))) {
          
        if (is.null(dialect[[key]])) {
          
          dialect[key] <- filterDefaultDialect(validateDialect(dialect))[key]
        }
      }
    }
    
    descriptor$dialect <- dialect
    
    
    
    
  }
  return(descriptor)
}

# Miscellaneous

#' Validate dialect
#' @description Helper function to validate dialect.
#' quoteChar and escapeChar are mutually exclusive: https://frictionlessdata.io/specs/csv-dialect/#specification
#' @param dialect list
#' @return dialect list
#' @rdname validateDialect
#' @export
#' 

validateDialect <- function(dialect = NULL) {
  
  if (isTRUE(all(c('quoteChar','escapeChar') %in% names(dialect)))) {
    stop(DataPackageError$new('Resource$table dialect options quoteChar and escapeChar are mutually exclusive.')$message)
  } else return(dialect)
}


#' Filter Default Dialect
#' @description Helper function to filter default dialect
#' quoteChar and escapeChar are mutually exclusive: https://frictionlessdata.io/specs/csv-dialect/#specification
#' @param dialect list
#' @return dialect list
#' @rdname filterDefaultDialect
#' @export
#' 
filterDefaultDialect <- function(dialect = NULL) {
  
  default.dialect <- config::get("DEFAULT_DIALECT", file = system.file("config/config.yaml", package = "datapackage.r"))
  
  if ('escapeChar' %in% names(dialect)) {default.dialect <- default.dialect[names(default.dialect) != 'quoteChar']} else default.dialect
  
  return(default.dialect)
}



#' Is remote path
#' @description Helper function to identify a remote path.
#' @param path string path
#' @return TRUE if path is remote
#' @rdname isRemotePath
#' @export
#' 

isRemotePath <- function(path) {
  if (!is.character(path)) {
    FALSE
  } else {
    path <- as.character(path)
    #if (!is.character(path)) FALSE else
    isTRUE(startsWith("http", unlist(strsplit(path, ":")))[1] |
             startsWith("https", unlist(strsplit(path, ":")))[1] | 
             isTRUE(is.git(path)))
    
  }
}

#' Is safe path
#' @description Helper function to check if a path is safe.
#' @param path string path
#' @return TRUE if path is safe
#' @rdname isSafePath
#' @export
#' 


isSafePath <- function(path) {
  
  if (!isTRUE(is.character(path)))
    FALSE
  else {
    containsWindowsVar <- function(path){
      if (isTRUE(grepl("%.+%", path)))
        TRUE
      else
        FALSE
    }
    containsPosixVar <- function(path){
      if (isTRUE(grepl("\\$.+", path)))
        TRUE
      else
        FALSE
    }
    
    # un Safety checks
    unsafenessConditions <- list(
      R.utils::isAbsolutePath(path),
      grepl("\\|/", path),
      grepl('\\.\\.', path),
      #path.includes(`..${pathModule.sep}`),
      startsWith(path, '~'),
      containsWindowsVar(path),
      containsPosixVar(path)
    )
    response <- any(unlist(unsafenessConditions))
    return(!response)
  }
}


## Extras

#' Check if a variable is undefined or NULL
#' @description Helper function to check if a variable is undefined or NULL.
#' @param x variable
#' @return TRUE if variable is undefined
#' @rdname isUndefined
#' @export
#'

isUndefined <- function(x) {
  
  if (any(isTRUE(!exists(deparse(substitute(
    x
  ))) || is.null(x))))
    TRUE
  else
    FALSE
}

#' Push elements in a list or vector
#' @description Helper function to add components in a list or vector.
#' @param x list or vector
#' @param value object to push in x
#' @rdname push
#' @export
#'

push <- function(x, value) {
  x <- append(x, value) #append rlist::list.
  return(x)
}


#' is git
#' @description Helper function to check if a link is from git.
#' @param x url
#' @rdname is.git
#' @return TRUE if url is git
#' @export
#'

is.git <- function(x) {
  any(grepl("git", x) | grepl("hub", x) | grepl("github", x))
}

#' is compressed
#' @description Helper function to check if a file is compressed..
#' @param x string with the file's path
#' @rdname is.compressed
#' @return TRUE if file is compressed
#' @export
#'

is.compressed <- function(x) {
  if (file.exists(x))
    grepl("^.*(.gz|.bz2|.tar|.zip)[[:space:]]*$", x)
  else
    message("The input file does not exist in:", getwd())
}

#' is json
#' @description Check if an object is json.
#' @param object object to test if it's json
#' @rdname is.json
#' @return TRUE if object is json
#' @export
#'
is.json <- function(object) {
  
  if (class(object) == "json")
    return(TRUE)
  else
    return(FALSE)
}

#' findFiles
#' @description Find a file pattern in a specified directory.
#' @param pattern string pattern
#' @param path string path
#' @rdname findFiles
#' @export
#'

findFiles <- function(pattern, path = getwd()) {
  
  files <- list.files(path, recursive = TRUE)
  #files=filepath(path)#, recursive = TRUE)
  # matched_files = files[grep(path, files, fixed = FALSE, ignore.case = FALSE)]
  
  matched_files <- files[grepl(pattern,
                               files,
                               fixed = FALSE,
                               ignore.case = FALSE)]
  
  matched_files <- matched_files[grepl(stringr::str_c(".","csv"), 
                                       matched_files, 
                                       fixed = TRUE, 
                                       ignore.case = FALSE)]
  
  return(matched_files)
}


#' Is empty
#' @description Is empty list
#' @param list list
#' @rdname is.empty
#' @return TRUE if list is empty
#' @export
#'

is.empty <- function(list) {
  empty <- purrr::every(list, function(x) {
    purrr::is_empty(x)
  })
  return(empty)
}


#' Is Local Descriptor Path
#' @description Helper functoin to check if a descriptor is local
#' @param descriptor descriptor
#' @param directory A character vector of full path name. The default corresponds to the working directory specified by \code{\link[base]{getwd}}
#'
#' @rdname is.local.descriptor.path
#'
#' @export
#'

is.local.descriptor.path <- function(descriptor, directory = ".") {
  #descriptor.path<-path.expand(paste0(basePath,"/datapackage.json"))
  if (!is.character(descriptor)) {
    return(FALSE)
  } else {
    if (isTRUE(startsWith(unlist(descriptor), "#"))) return(FALSE)  else 
    {
      
      
      isTRUE(any(
        descriptor %in% list.files(path = directory, recursive = TRUE) |
          grepl(descriptor , list.files(path = directory, recursive = TRUE)) |
          file.exists(
            normalizePath(
              stringr::str_c('inst/data', basename(descriptor), sep = '/'),
              winslash = "\\",
              mustWork = FALSE
            )
          )
      ))
    }
  }
}

#' Descriptor pointer
#' @description Helper function for specifying locations in a descriptor.
#' @param value value  that specifies location in the descriptor
#' @param baseDescriptor base descriptor
#' @rdname descriptor.pointer
#' @export
#'

descriptor.pointer <- function(value, baseDescriptor) {
  v8 <- V8::v8()
  v8$source("inst/scripts/jsonpointer.js")
  
  v8$call("function(x,y){output = jsonpointer.get(x,y)}", baseDescriptor, substring(value[[1]], 2, stringr::str_length(value[[1]])) )
  property = v8$get("output", simplifyVector = FALSE)
  
  return(property)
}

#' Convert json to list
#' @description Helper function convert json to list.
#' @param lst list object
#' @rdname helpers.from.json.to.list
#' @export
#'

helpers.from.json.to.list <- function(lst) {
  return(jsonlite::fromJSON(lst, simplifyVector = FALSE))
}

#' Convert list to json
#' @description Helper function convert list to json.
#' @param json json string
#' @rdname helpers.from.list.to.json
#' @export
#'

helpers.from.list.to.json <- function(json) {
  return(jsonlite::toJSON(json, auto_unbox = TRUE))
}


#' File basename
#' @description Removes all of the path up to and including the last path separator (if any)  without extensions.
#' @param path character vector with path names
#' @rdname file_basename
#' @export
#'

file_basename <- function(path){
  if (isTRUE(stringr::str_count(path,"[.]") == 2)) {
    tools::file_path_sans_ext(tools::file_path_sans_ext(basename(path)))
  } else tools::file_path_sans_ext(basename(path))
}


#' File extension
#' @description Returns the file extension without the leading dot.
#' @param path string with path names
#' @rdname file_extensions
#' @export
#'

file_extension <- function(path){
  if (isTRUE(stringr::str_count(path,"[.]") == 2)) {
    tools::file_ext(tools::file_path_sans_ext(basename(path)))
  } else tools::file_ext(basename(path))
}

#' Save json file
#' @description Save a list object in json file to disk
#' @param x list object
#' @param file file path
#' @rdname write.json
#' @export
#'

write.json <- function(x, file){
  x <- jsonlite::prettify(helpers.from.list.to.json(x))
  x <- writeLines(x, file)
}
