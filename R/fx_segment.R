# Experis Business Demo ---------------------------------------------------

# Function: Segment

demo_segment <- function(data, k = 20, estimate_k = TRUE, cols_remove = NULL) {

  library(h2o)
  h2o.init()


  data_h2o <-
    if ("H2OFrame" %in% class(data)) {
      data
    } else {
      data %>% mutate_if(is_character, as_factor) %>% as.h2o()
    }


  # Remove Columns
  column_names <- h2o.colnames(data_h2o)
  try(silent = TRUE,
      column_names <- column_names %>% setdiff(c(cols_remove, "segment"))
  )


  # Create Segments
  segment_model <-
    h2o.kmeans(data_h2o,
               k = k,
               seed = 42,
               # nfolds = 5,
               x = column_names,
               estimate_k = estimate_k)


  segment_summary <- segment_model %>% h2o.centers() %>% as_tibble()
  segment_assignment <- h2o.predict(segment_model, data_h2o) %>% as.vector()


  lst(
    model = segment_model,
    summary = segment_summary,
    assignment = segment_assignment
    )


}

