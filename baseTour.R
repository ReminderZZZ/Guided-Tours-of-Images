# Greedy algorithm

# Output: sorted names and labels of the images in tour sequence
baseTour = function(img_dist, num_img, name_img, target = 10, seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  if (target > num_img) target = num_img
  
  # Random start
  tour_guide = start_img = sample(num_img, 1)
  
  img_dist = img_dist[-start_img,]
  img_dist = rbind(img_dist, Inf)
  
  # Find the nearest neighbor for the current image each time
  for (i in 1:(target-1))
  {
    next_index = order(img_dist[,start_img])[1]
    next_img = rownames(img_dist)[next_index]
    img_dist = img_dist[-next_index,]
    tour_guide = c(tour_guide, next_img)
    start_img = next_img
  }
  
  tour_guide_ind = as.numeric(tour_guide)
  tour_guide_name = name_img[tour_guide_ind]
  return(list(tour_guide_ind, tour_guide_name))
}