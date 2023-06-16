# A greedy algorithm based on the random projection grand tour
tourRP = function(data_img, num_img, dim_1 = c(10,10), dim_2 = 10, target = 10,
                  seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  if (target > num_img) target = num_img
  tour_guide_ind = sample(num_img, 1)
  
  
  img_avg_org = imgAvg(data_img, num_img, dim_1[1], dim_1[2])
  img_mat = matrix(unlist(img_avg_org), byrow = T, nrow = num_img)
  img_dataframe = as.data.frame(img_mat)
  
  h = save_history(img_dataframe, grand_tour(d = dim_2),
                    max = ceiling(target/10) + 2)
  
  count = 1
  loop = 2
  while(count < target)
  {
    h_sub = interpolate(h[,,loop:(loop+1)], 0.3)
    loop = loop + 1
    # Find at most 10 images between 2 random projections by interpolation
    for (i in 1:min(dim(h_sub)[3], 10))
    {
      R = matrix(h_sub[,,i], ncol = dim_2)
      # Random projection
      projection = img_mat %*% R
      # Find the nearest neighbor
      candidate = order(apply(sweep(projection, 2,
                                       projection[tour_guide_ind[count],]), 1,
                                 function(i) sum(i^2)))
      candidate = candidate[!(candidate %in% tour_guide_ind)]
      tour_guide_ind = c(tour_guide_ind, candidate[1])
      count = count + 1
      if (count == target) break
    }
  }
  tour_guide_name = name_img[tour_guide_ind]
  return(list(tour_guide_ind, tour_guide_name))
}