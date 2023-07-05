to_row_percent <- function(data){
  data |>
    dplyr::rowwise() |> # make calculations rowwise
    dplyr::mutate(sm = sum(dplyr::c_across(where(is.numeric)))) |> #make row sums...
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.numeric),
                    function(x) (x/sm) * 100)) |>  #...apply in function...
    dplyr::select(-sm)       # ...then delete
}
