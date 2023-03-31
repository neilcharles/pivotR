devtools::load_all(".")
pivotR(mtcars |> 
         dplyr::mutate(name = rownames(mtcars)))
