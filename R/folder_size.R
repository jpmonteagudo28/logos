#' Calculate the Size of a Folder
#'
#' Internal function to calculate the total size of a folder, the number of files,
#' and the unique file extensions within the folder.
#'
#' @param folder A string specifying the name of the folder whose size is to be calculated.
#' @param units A string specifying the units for the size calculation. Options are:
#' `"B"` (bytes), `"Kb"` (kilobytes), `"Mb"` (megabytes), or `"Gb"` (gigabytes). Defaults to `"Kb"`.
#' @param recursive A logical value indicating whether to include subdirectories when listing files. Defaults to `TRUE`.
#' @param omit_folders A logical value indicating whether to exclude specific folders (e.g., `.git`, `.quarto`, `renv`) from the search. Defaults to `TRUE`.
#' @param ... further arguments to be passed to `is_file` or `omit_folders`
#' @return A data frame containing:
#' \itemize{
#'   \item \code{folder}: The name of the folder.
#'   \item \code{size}: The total size of the folder in the specified units.
#'   \item \code{files}: The total number of files in the folder.
#'   \item \code{extensions}: A list of unique file extensions in the folder.
#' }
#'
#' @keywords internal
#'
#' @details
#' The function allows the user to calculate folder sizes while excluding specific folders
#' such as `renv` or `.git` to optimize performance. If multiple folders with the same name
#' are found, the user is prompted to select one.
#'
get_folder_size <- function(folder,
                            units = "Kb",
                            recursive = TRUE,
                            omit_folders = TRUE,
                            ...){

  units <- match.arg(units, c("B","Kb","Mb","Gb"),
                     several.ok = FALSE)


  root_dir <- getwd()

  folder <- as.character(folder)

  dirs <- omit_folders(root_dir)

  check_for_file <- is_file(name = folder,
                            dir = dirs)
  if(check_for_file){
    stop("You have entered a file name.
         Please enter a project folder instead")
  }

  target_dirs <- grep(paste0("/", folder, "$"), dirs, value = TRUE)

  if(length(target_dirs) == 0){
    stop(paste0("'", folder, "' folder not found in the current directory or subdirectories"))
  }

  if (length(target_dirs) > 1) {
    cat("Multiple '", folder, "' folders found:\n", sep = "")
    for (i in seq_along(target_dirs)) {
      cat(i, ": ", target_dirs[i], "\n", sep = "")
    }
    choice <- as.integer(readline(prompt = paste("Enter the number of the '", folder,
                                                 "' folder you want to use: ", sep = "")))

    if (is.na(choice) || choice < 1 || choice > length(target_dirs)) {
      stop("Invalid selection")
    }
    target_dir <- target_dirs[choice]
  } else {
    target_dir <- target_dirs[1]
  }


  files <- list.files(target_dir,
                      full.names = TRUE,
                      recursive = recursive)

  total_files <- length(files)

  if(total_files > 1000) {
    warning("Large number of files. This may take a while.")
  }

  # Calculate total file size
  file_sizes <- file.info(files)$size

  total_size_bytes <- sum(file_sizes, na.rm = TRUE)

  has_extensions <- sub(".*\\.([a-zA-Z0-9]+)$", "\\1",files)

  no_paths_just_ext <- unique(
    has_extensions[
      grepl("\\.[a-zA-Z0-9]+$", files)
    ]
  )


  # Convert total size to the specified units
  total_size <- switch(units,
                       B = total_size_bytes,
                       Kb = total_size_bytes / 1024,
                       Mb = total_size_bytes / (1024^2),
                       Gb = total_size_bytes / (1024^3))


  # Return a list containing size, total files, and unique extensions
  return(
    data.frame(
      folder = folder,
      size = total_size,
      files = total_files,
      extensions = I(
        list(no_paths_just_ext)
      )
    )
  )
}

#' Check if a Name is a File or a Folder
#'
#' Internal function to verify whether a given name corresponds to a file
#' or a folder in the specified directory.
#'
#' @param name A string specifying the name to check.
#' @param dir A string specifying the directory to search within.
#' @param ignore_dot_folders A logical value indicating whether to ignore
#' hidden folders (e.g., `.git`, `.quarto`) during the check. Defaults to `TRUE`.
#'
#' @return A logical value: \code{TRUE} if the name is a file, \code{FALSE} otherwise.
#'
#' @keywords internal
#'
#' @details
#' This function searches for the specified name in the directory and distinguishes
#' between files and folders. If the name corresponds to a folder, a warning is issued.
#'
is_file <- function(name,
                    dir,
                    ignore_dot_folders = TRUE,
                    ...){

  if(!is.character(name)){
    name <- as.character(name)
  }
  dirs <- if (ignore_dot_folders) {
    omit_folders(dir)
  } else {
    list.dirs(dir, full.names = TRUE, recursive = TRUE)
  }

  # Explicitly set 'recursive' to FALSE so function stops before
  # exploring external directories
  file_check <- list.files(dir,pattern = name, full.names = TRUE,
                           recursive = FALSE)

  if(length(file_check) == 0){
    stop("'",name,"'folder or file not found in the current project")
  }

  if (length(file_check) > 0 && !any(file_check %in% dirs)) {
    # The file exists but is not a folder
    return(TRUE)
  } else if (length(file_check) > 0 && any(file_check %in% dirs)) {
    # The name corresponds to a folder
    warning("'", name, "' is a folder, not a file.")
    return(FALSE)
  }
}

##-------------------------------------------------------------------##
# Exclude renv folder from project directories to speed up path look up
#' Exclude Specified Folders from Directory Search
#'
#' Internal function to exclude specific folders (e.g., `renv`, `.git`, `.quarto`) from
#' the directory search to optimize path lookups.
#'
#' @param dir A string specifying the root directory to search within.
#'
#' @return A character vector of directory paths excluding the specified folders.
#'
#' @keywords internal
#'
#' @details
#' The function removes any directories matching predefined patterns (e.g., `/renv`, `/.git`)
#' from the list of directories in the specified root directory.
#'
omit_folders <- function(dir, exclude_folders = NULL,...){

  excluded_folders <- if(is.null(exclude_folders)){

     c("/renv","/.git", "/.github","/.quarto","/.Rproj.user")
    } else {
      exclude_folders
  }

  # Get all directories within the root, recursively
  dirs <- list.dirs(dir, full.names = TRUE, recursive = TRUE)

  # Remove any directory that matches one of the excluded folders
  dirs <- dirs[!Reduce(`|`, lapply(excluded_folders, grepl, dirs))]

  return(dirs)
}
