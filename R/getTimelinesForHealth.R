#' seqlast
#'
#' @param df A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @keywords
#' @export
#' @examples
#' getTimelinesForHealth()

seqlast <- function (from, to, by) {
  vec <- do.call(what = seq.Date, args = list(from, to, by))
  if ( tail(vec, 1) != to ) {
    return(c(vec, to))
  } else {
    return(vec)
  }
}


#' create_time_batches
#'
#' @param df A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @keywords
#' @export
#' @examples
#' getTimelinesForHealth()

create_time_batches <- function(start, end, year_batch){
    s <- seqlast(from=ymd(start), to=ymd(end), by=year_batch)
    l <- list(); ct <- 1
    for(i in 1:(length(s)-1)){
        l[[ct]] <- c(s[i], s[i+1]); ct <- ct + 1
    }
    return(l)
}


#' getTimelineForHealth
#'
#' @param df A dataframe including time as \code{timestamp} and searches for your given geography in one column.
#' @keywords
#' @export
#' @examples
#' getTimelinesForHealth()


getTimelinesForHealth <- function(
    batch_size = 1,
    year_batch = "1 year",
    time.startDate = "2019-06-15",
    time.endDate = "2020-01-01",
    timelineResolutions = c(
        "month"
    ),
    terms = c(
        "summer + winter + fall + spring", 
        "cat + cat food + dog + dog food"
    ),
    names = c(
        "seasons",
        "pets"
    ),
    geoRestriction.regions = c(
        "US-NY", 
        "US-CA"
    ),
    geoRestriction.countries = c(
        "GB",
        "US"
    ),
    geoRestriction.dmas = c(
    ),
    output_directory = "../output"
    ){


    ## ANALYSIS
    key <- Sys.getenv("GOOGLE_TRENDS_KEY")
    match_names <- data.frame(terms=terms, names=names, stringsAsFactors=F)
    alt <- "json"

    time_batches <- create_time_batches(time.startDate, time.endDate, year_batch)
    term_batches <- split(terms, ceiling(seq_along(terms)/batch_size))
    name_batches <- split(names, ceiling(seq_along(terms)/batch_size))

    geoRestrictions <- c(
        geoRestriction.regions,
        geoRestriction.countries,
        geoRestriction.dmas
    )

    geoRestriction.types <- c(
        rep("geoRestriction.region", times=length(geoRestriction.regions)),
        rep("geoRestriction.country", times=length(geoRestriction.countries)),
        rep("geoRestriction.dma", times=length(geoRestriction.dmas))
    )



    # Make sure we're doing a valid request
    if(length(terms) != length(names)) stop("terms and names must be the same length")
    # timelineResolutions
    if(!(timelineResolutions %in% c("year", "month", "week", "day"))) stop("Invalid timelineResolution argument")
    if(!dir.exists(output_directory)) stop("output_directory does not exist")
    if(batch_size >= 30) stop("batch_size must be less than 30")
    if(key == "") stop("GOOGLE_TRENDS_KEY not a system variable")
    if(length(geoRestrictions) == 0) stop("Need at least one geoRestriction")


    dat <- list(); ct <- 1

    for(timelineResolution in timelineResolutions){

        for(time_batch in time_batches){

            batch.startDate <- format(time_batch[1], "%Y-%m-%d")
            batch.endDate <- format(time_batch[2], "%Y-%m-%d")
                
            for(term_batch_idx in 1:length(term_batches)){

                term_batch <- term_batches[[term_batch_idx]]
                # name_batch <- name_batches[[term_batch_idx]]

                print(sprintf("[%s] Retrieving TERMS [%s] over PERIOD [%s to %s]", Sys.time(), paste(term_batch, collapse=", "), batch.startDate, batch.endDate))

                for (geo_idx in 1:length(geoRestriction.types)){

                    q <- list()

                    region <- geoRestrictions[geo_idx]
                    region_type <- geoRestriction.types[geo_idx]

                    # print(region)
                    # print(region_type)
                    
                    for(term_idx in 1:length(term_batch)){
                        q[[term_idx]] <- term_batch[term_idx]
                        names(q)[term_idx] <- "terms"
                    }

                    q[["time.startDate"]] <- batch.startDate
                    q[["time.endDate"]] <- batch.endDate
                    q[["timelineResolution"]] <- timelineResolution
                    q[["key"]] <- key 
                    q[["alt"]] <- alt
                    q[[region_type]] <- region

                    # print(q)


                    prms <- paste(sapply(1:length(q), function(idx) {
                        return(sprintf("%s=%s", names(q)[idx], URLencode(q[[idx]])))
                    }), collapse="&")

                    req <- gargle::request_build(
                        method = "GET",
                        path = sprintf("trends/v1beta/timelinesForHealth?%s", prms),
                        base_url = "https://www.googleapis.com"
                    )

                    resp <- gargle::request_make(req)
                    out <- gargle::response_process(resp)
                    
                    out.dat <- list(); out.ct <- 1
                    for(out.line in out$lines){
                        out.term <- out.line$term
                        out.name <- match_names$name[match(out.term, match_names$term)]
                        for(out.point in out.line$points){


                            if (timelineResolution %in% c("day", "week")){
                                out.date <- as.Date(out.point$date, format="%b %d %Y")
                            } else if (timelineResolution %in% c("month")){
                                d <- out.point$date
                                d <- gsub(" ", " 01 ", trimws(d))
                                out.date <- as.Date(d, format="%b %d %Y")
                            } else if (timelineResolution %in% c("year")){
                                d <- sprintf("Jan 01 %s", out.point$date)
                                out.date <- as.Date(d, format="%b %d %Y")
                            } else {
                                stop("Unrecognized timelineResolution argument")
                            }

                            out.value <- out.point$value

                            # print(out.date)
                            # print(out.value)

                            out.dat[[out.ct]] <- rbind(c(
                                "timelineResolution" = timelineResolution, 
                                "region" = region, 
                                "term" = out.term, 
                                "date" = as.character(as.Date(out.date, origin="1970-01-01")), 
                                "value" = out.value,
                                "name_" = out.name
                                ))

                            out.ct <- out.ct + 1
                        }
                    }

                    out.df <- do.call(rbind.data.frame, out.dat)
                    dat[[ct]] <- out.df; ct <- ct + 1

                    # print(out.df)
                    # exit(0)

                }

            }

        }

    }

    df <- do.call(rbind.data.frame, dat)
    df <- df %>% mutate_all(as.character) %>% mutate(value = as.numeric(value))
    # df$name_ <- as.character(df$name_)
    # df$date <- as.character(df$date)

    # print(df %>% head())

    mean0_ <- function(x){
        x <- as.numeric(as.character(x))
        x <- na.omit(x)
        x <- x[x!=0]
        return(mean(x, na.rm = T))
    }

    df <- df %>% 
            dplyr::group_by(
                timelineResolution, 
                region, 
                term, 
                name_,
                date
            ) %>% 
            dplyr::summarise(
                value = mean0_(value)
            ) %>% 
            dplyr::ungroup()

    # print(df %>% arrange(date) %>% head(10) )
    # print(df %>% filter(grepl("2019-08", date), as.character(name_) == "pets"))

    df %>% 
        dplyr::group_by(
            timelineResolution, 
            name_
        ) %>% 
        dplyr::group_walk(
            ~write.csv(
                .x %>% 
                    select(date, region, value) %>% 
                    tidyr::spread(region, value) %>% 
                    rename(timestamp = date) 
                    # %>% mutate(timestamp = as.Date(timestamp, format="%b %d %Y"))
                    , 
                file=sprintf("%s/%s_%s.csv", output_directory, .y$name_, .y$timelineResolution), 
                row.names = F
            )
        )

}


