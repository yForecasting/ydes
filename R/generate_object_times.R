#' Generate Object Times
#'
#' Generate the draws for the inter-arrival time, the process times for the
#' simulation
#'
#' Draw different times for all the distributions.
#'
#' @param n number of items to generate.
#'
#' @param distr A list of info on the distributions used for each time.
#'
#' @param time_names A vector of names for the different time values.
#'
#' @param export_file Filename to export dataframe in csv format. When this is
#' set to the default value NA, no csv file will be exported.
#'
#' @author Yves R. Sagaert
#'
#' @return A dataframe of draws of times.
#'
#' @export
#'
#' @examples
#' generate_object_times(n=30, distr=list(c(10,2),c(15,1),c(20,2),c(10,0.5)),
#'                time_names=c("inter_arrival_time", "process_time1",
#'                "process_time2", "process_time3"))

generate_object_times <- function(n=30, distr, time_names, export_file=NA){
  # Set the seed for reproducibility
  set.seed(1010)

  df <- NA
  # Generate inter-arrival times and process times
  for (i in 1:length(distr)){
    t <- rnorm(n, mean = distr[[i]][1], sd = distr[[i]][2])
    # Create a data frame with the generated times
    if ("logical" %in% class(df)){
      df <- data.frame(t)
    } else {
      df <- cbind(df,t)
    }
  }

  # Names of the data frame
  colnames(df) <- time_names

  # Export the data frame to a CSV file
  if (!("logical" %in% class(export_file))){
    write.csv(df, file = "simulation_data.csv", row.names = FALSE)
  }


}
