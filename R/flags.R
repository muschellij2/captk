#' Return CapTK directory information
#'
#' Call this to get the include path for CapTK configuration.
#' this is what needs to be passed to dependent programs.
#'
#' @examples
#'
#' captkDir()
#' @import Rcpp
#' @export
captkDir <- function() {
  itkd = get_captk_dir()
  if ( ! file.exists(itkd) && !dir.exists(itkd) )
    print("captkDir: captk dir does not exist")
  itkd = paste0(itkd, "/")
  cat( itkd )
}

get_captk_dir = function() {
  lib_dir = system.file("libs", "lib", "cmake", package = "captkr")
  dirs = list.dirs(path = lib_dir, recursive = FALSE)
  bn = basename(dirs)
  # Keep CapTK dirs  
  dirs = dirs[ grepl("^CapTK", bn) ]
  if (length(dirs) == 0) {
    stop("No CapTK Directory Found!")
  }
  if (length(dirs) == 1) {
    itkd = dirs
  } else {
    bn = basename(dirs)
    bn = strsplit(bn, split = "-")
    bn = sapply(bn, function(x) x[2])
    bn = numeric_version(bn)
    dirs = dirs[order(bn, decreasing = TRUE)]
    itkd = dirs[1]
  }
  return(itkd)
}

#' Return CapTK installation information
#'
#' Call this to get the include path for CapTK headers and libraries
#'
#' @examples
#'
#' captkIncludes()
#'
#' @export 
captkIncludes <- function() {
  itklocation<- system.file(
    "libs", "include", paste0("CapTK-",itkVersion()),
    package = "captkr")
  if ( ! file.exists(itklocation) && !dir.exists(itklocation) )
    print("captkIncludes: captk includes do not exist")
  itklocation = paste0(itklocation, "/")
  cat( itklocation )
}

#' Return CapTK library location
#'
#' Call this to get the path to CapTK static libaries 
#' to which you will link
#'
#' @examples
#'
#' captkLibs()
#'
#' @export itkLibs
captkLibs <- function() {
  itklibs <- system.file("libs", "lib", package = "captkr")
  if ( ! file.exists(itklibs) && !dir.exists(itklibs) )
    print("captkLibs: captk libs do not exist")
  itklibs = paste0(itklibs, "/")
  cat( itklibs )
}


#' Return CapTK compile flags
#'
#' Call this to get the compilation flags used for CapTK and dependent software
#'
#' @examples
#'
#' captkCompileFlags()
#'
#' @export 
captkCompileFlags <- function() {
  str = " -fPIC -O2  "
  if (.Platform$OS.type == "windows") {
    oflags = c("-lws2_32", "-lgdi32", "-mwindows", "-Wno-c++11-long-long", 
               "-msse4.1", "-mssse3", "-Wa,-mbig-obj")
    oflags = paste(oflags, collapse = " ")
    str = paste(str, oflags)
  }
  cat( str )
}

#' Return CapTK version information
#'
#' Call this to get the installed CapTK version
#'
#' @examples
#'
#' captkVersion()
#'
#' @export 
captkVersion <- function() {
  # should update this as versions change
  ver = get_captk_dir()
  ver = basename(ver)
  ver = sub("CapTK-", "", ver)
  return(ver)
}

