fileCheck <- function(archive, url, ...) {
    # Check to see if a set of files "..." are in the working directory
    # If they're not present, check for a zip archive "archive" and unzip it
    # If "archive" isn't present, download it from "url"
    
    # Make list out of files we need
    files = c(...)
    
    # Check if files exist
    if (!(length(files) == sum(file.exists(files)))) {
        
        # Check if archive exists and download if not
        if (!file.exists(archive)) {
            download.file(url, archive, method = "curl")
        }
        
        unzip(archive)
    }
}
