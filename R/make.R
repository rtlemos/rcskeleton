#' Generate basic files to get an RC package to run
#'
#' @param package.name character Name of package
#' @param dir character Folder where package is being created
#'
#' @export make.skeleton
#'
make.skeleton <- function(dir, package.name, short.name) {
  if (missing(dir)) {
    stop('You must provide the folder where you are building the package')
  }
  if (missing(package.name)) stop('You must provide the package name')
  if (missing(short.name)) short.name <- package.name
  
  make.conf(package.name, short.name, dir)
  make.daemon(package.name, short.name, dir)
  make.parameters(package.name, short.name, dir)
  make.strategy(package.name, short.name, dir)
  make.plotter(package.name, short.name, dir)
  print("Done!")
}

make.conf <- function(package.name, short.name, dir) {
  sink(paste0(dir, short.name, "-conf.R"))
  cat("#' Template configuration for this package\n")
  cat("#'\n")
  cat("#' @import rcvirtual\n")
  cat("#' @export rcage.conf\n")
  cat("#' @exportClass rcage.conf\n")
  cat("#'\n")
  cat(paste0(package.name, ".conf <- setRefClass(\n"))
  cat(paste0("  Class = '", package.name, ".conf',\n"))
  cat("  contains = 'rcvirtual.conf',\n")
  cat("  methods = list(\n")
  cat("    construct = function(){\n")
  cat("\n")      
  cat("      callSuper()\n")
  cat("\n")      
  cat(paste0("      .self$package.name <- ", package.name, "\n"))
  cat("      t.posix <- as.POSIXlt(c('1964/01/01', '2004/01/01'), tz = 'GMT')\n")
  cat("      t.bnd <- as.numeric(t.posix)\n")
  cat("\n")      
  cat("      #-------------------------------------------------------------------------\n")
  cat("      # Daemon conf\n")
  cat("      #-------------------------------------------------------------------------\n")
  cat("      .self$daemon <- list(timeb = t.bnd)\n")
  cat("\n")      
  cat("      #-------------------------------------------------------------------------\n")
  cat("      # Strategy conf\n")
  cat("      #-------------------------------------------------------------------------\n")
  cat("      .self$strategy <- list(timeb = t.bnd)\n")
  cat("\n")        
  cat("      #-------------------------------------------------------------------------\n")
  cat("      # Plotter conf\n")
  cat("      #-------------------------------------------------------------------------\n")
  cat("      .self$plotter <- list(timeb = t.bnd)\n")
  cat("\n")      
  cat("      #-------------------------------------------------------------------------\n")
  cat("      # Parameter conf\n")
  cat("      #-------------------------------------------------------------------------\n")
  cat("      df <- .self$get.conf.template()\n")
  cat("      sk <- function(name) {\n")
  cat("        as.numeric(mapply(name, FUN = function(name) which(df$name == name)))\n")
  cat("      }\n")
  cat("      df$type[sk(c('Y', 't'))] <- 'fixed'\n")
  cat("      df$type[sk('K')] <- 'derived'\n")
  cat("      df$type[sk('phi')] <- 'unknown'\n")
  cat("      df$type[sk('theta')] <- 'processes'\n")
  cat("      df$type[sk('chi')] <- 'conjugate normal parameter'\n")
  cat("\n")      
  cat("      df$initial[sk('phi')] <- 0.1\n")
  cat("\n")      
  cat("      df$lbound[sk('phi')] <- 0\n")
  cat("\n")      
  cat("      df$ubound[sk('phi')] <- 1\n")
  cat("\n")      
  cat("      df$long.name[sk('Y')] <- 'catches'\n")
  cat("      df$long.name[sk('t')] <- 'time'\n")
  cat("      df$long.name[sk('K')] <- 'carrying capacity'\n")
  cat("      df$long.name[sk('phi')] <- 'elasticity'\n")
  cat("      df$long.name[sk('theta')] <- 'log-abundance at age'\n")
  cat("      df$long.name[sk('chi')] <- 'log-catchability'\n")
  cat("\n")      
  cat("      df$is.spatial.avg[sk('Y')] <- TRUE\n")
  cat("\n")      
  cat("      df$store.in.ram[sk('Y')] <- TRUE\n")
  cat("\n")      
  cat("      df$units[sk('t')] <- 'year'\n")
  cat("\n")      
  cat("      out.dir <- paste0(tempdir(), '/')\n")
  cat("      df$output.file[sk('Y')] <- paste0(out.dir, 'observations.csv')\n")
  cat("\n")      
  cat("      .self$parameters <- df[df$type != '-', ]\n")
  cat("    }\n")
  cat("  )\n")
  cat(")\n")
  sink()
}

make.daemon <- function(package.name, short.name, dir) {
  
}

make.parameters <- function(package.name, short.name, dir) {
  
}

make.strategy <- function(package.name, short.name, dir) {
  
}

make.plotter <- function(package.name, short.name, dir) {
  
}