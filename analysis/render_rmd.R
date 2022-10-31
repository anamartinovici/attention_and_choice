args <- commandArgs(TRUE)

if (length(args) == 0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	path_to_RMD_file <- args[1]
}

rmarkdown::render(path_to_RMD_file, output_dir = here::here())
