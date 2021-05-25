#' Filter the rss to the first third of the rss confidence interval, removing any duplicates in the parameters
#'
#' \code{rss_filter} Computes the summary distribution for the residual sum of squares and then filters out the unique values.  Has to work on a nested data frame
#'
#' @param input_results A nested data frame
#'
#' @return A filtered nested data frame
#' @examples
#'
#' # To be filled in later

#' @export


rss_filter <- function(input_results) {

  rss_iterations <- input_results %>%
    group_by(Year,depth,model) %>%
    nest() %>%
    mutate(fivenum = map(.x=data,.f=~(summary(.x$rss) %>% enframe()) )   ) %>%  # Now we compute the summary stats
    mutate(rss_filter = map2(.x=data,
                             .y=fivenum,
                             .f=~(filter(.x,between(rss,.y$value[2],.y$value[5])) ) ) )


  rss_unique <- rss_iterations %>%
    mutate(rss_filter = map(rss_filter,.f=~(unnest(.x,cols=params)  %>%
                                              pivot_wider(names_from="name",values_from="value") %>%
                                              mutate(across(.cols=!c("rss","iteration"),.fns=~round(.x,5))) %>%
                                              distinct(across(.cols=!c("rss","iteration")),.keep_all=TRUE) %>%
                                              pivot_longer(cols = !c("rss","iteration"),names_to="name",values_to="value")  %>%
                                              nest(params=c(name,value))) ) )

  return(rss_unique)


}
