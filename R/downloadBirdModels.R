downloadBirdModels <- function(folderUrl,
                               version,
                               birdsList,
                               modelsPath,
                               returnPath = FALSE){
  filesToDownload <- tryCatch({
    drive_ls(path = as_id(folderUrl),
                          pattern = paste0("brt", version, ".R")) |>
      Cache()
  }, error = function(e) {
    ## Ceres: work around for sim$urlModels in shared drive
    drive_ls(path = as_id(folderUrl),
                         pattern = paste0("brt", version, ".R"), ## to make sure only projection file names are listed
                         shared_drive = as_shared_drive(as_id("0ABSERKnd1o3sUk9PVA"))) |>
      Cache()
  })

  modelsForBirdList <- filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"),
                                                  x = filesToDownload$name)]
  if (length(modelsForBirdList) == 0){
    message(crayon::red(paste0("No model available for ", birdsList,
                   " for models V", version)))
    return(NA)
  }
  downloadedModels <- lapply(X = modelsForBirdList, FUN = function(modelFile){
    if (!file.exists(file.path(modelsPath, modelFile))){
      googledrive::drive_download(file = as_id(filesToDownload[filesToDownload$name %in% modelFile, ]$id), #modelFile,
                                  path = file.path(modelsPath, modelFile), overwrite = TRUE)
    }
    if (returnPath){
      return(file.path(modelsPath, modelFile))
    } else {
      return(get(load(file.path(modelsPath, modelFile))))
    }
  })
  names(downloadedModels) <- birdsList

  return(downloadedModels)
}
