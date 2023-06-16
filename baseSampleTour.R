# Apply sampling image candidate set to greedy algorithm
baseSampleTour = function(img_avg, name_img, num_img, target = 10,
                          sample_size = 10, seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  if (target > num_img) target = num_img
  
  # Avoid over the range
  target = min(num_img, target)
  sample_size = min(num_img, sample_size)
  
  # Initialization
  avail_img = 1:num_img
  tour_guide_ind = s = sample(num_img, 1)
  avail_img = avail_img[-s]
  num_img = num_img - 1
  
  for (i in 1:(target-1))
  {
    # Get a sample of images
    sample_space = sample(avail_img, min(num_img, sample_size))
    # Calculate distances for the current last image
    dist_current = sapply(img_avg[sample_space], function(e){
      sum((e - img_avg[[s]])^2)
    })
    s = sample_space[which.min(dist_current)]
    tour_guide_ind = c(tour_guide_ind, s)
    avail_img = avail_img[-which(avail_img == s)]
    num_img = num_img - 1
  }
  
  tour_guide_name = name_img[tour_guide_ind]
  return(list(tour_guide_ind, tour_guide_name))
}
