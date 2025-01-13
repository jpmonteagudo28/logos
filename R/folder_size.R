get_folder_size <- function(folder,
                            units = "Kb",
                            recursive = TRUE,
                            omit_folders = TRUE){

  units <- match.arg(units, c("B","Kb","Mb","Gb"),
                     several.ok = FALSE)


  root_dir <- getwd()

  folder <- as.character(folder)
  # Exclude 'renv' from project directory
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

is_file <- function(name,
                    dir,
                    ignore_dot_folders = FALSE){

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

  if(length(file_check) > 0 && !(file_check %in% dirs)){
    TRUE
  } else {
    warning("'", name, "' is a folder, not a file.")
    FALSE
  }
}

##-------------------------------------------------------------------##
# Exclude renv folder from project directories to speed up path look up
omit_folders <- function(dir){

  exclude_folders <- c("/renv",
                       "/.git",
                       "/.github",
                       "/.quarto",
                       "/.Rproj.user"
  )

  # Get all directories within the root, recursively
  dirs <- list.dirs(dir, full.names = TRUE, recursive = TRUE)

  # Remove any directory that matches one of the excluded folders
  for (folder in exclude_folders) {
    dirs <- dirs[!grepl(folder, dirs)]
  }
  return(dirs)
}
