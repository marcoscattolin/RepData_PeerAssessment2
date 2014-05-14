load_data function(file) {
        data <- read.csv(bzfile(file))        
}





data <- load_data("./data/repdata-data-StormData.csv.bz2")