# Interesting projection
randProject = function(data_img, num_img, dim_1 = c(10,10), dim_2 = 10,
                        seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  
  img_avg_org = imgAvg(data_img, num_img, dim_1[1], dim_1[2])
  img_mat = matrix(unlist(img_avg_org), byrow = T, nrow = num_img)
  img_dataframe = as.data.frame(img_mat)
  
  # Projection pursuit guided tour by Holes index
  R = matrix(save_history(img_dataframe,
                            guided_tour(d=dim_2, holes(),
                                        search_f = search_better_random),
                            max = 10)[,,10], ncol = dim_2)
  projection = img_mat %*% R
  img_avg = list()
  for (i in 1:num_img) img_avg[[i]] = projection[i,]
  
  return(img_avg)
}