# Preprocessing

# Read images data and convert RGB format to Luv
# Input: file directory
# Output: list contains Luv data and size of images
getLuv = function(dir, size = 100, seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  
  # Get file paths
  file_names = list.files(dir)
  size = min(size, length(file_names))
  file_names = sample(file_names, size)
  path = file.path(dir, file_names)
  
  len = length(path)
  
  luv_list = list()
  img_dim = list()
  
  # Read data
  for (i in 1:len)
  {
    img = readImage(path[i])
    img_mat = matrix(img, ncol=3)
    img_luv = convertColor(img_mat, from="sRGB", to="Luv")
    img_dim[[i]] = dim(img)[1:2]
    luv_list[[i]] = img_luv
  }
  
  return(list(img_dim, luv_list, len, file_names))
}

# Average the image
# Input: images data, number of images, target compressed height and width
# Output: Averaged images data
imgAvg = function(data_img, num_img, h_avg = 3, w_avg = 3)
{
  # Get the size of the image
  data_size = data_img[[1]]
  data_luv = data_img[[2]]
  
  # Save averaged images data
  img_avg = list()
  
  for (i in 1:num_img)
  {
    h = data_size[[i]][1]
    w = data_size[[i]][2]
    
    arr_old = array(data_luv[[i]], dim=c(h,w,3))
    arr_new = array(0, dim=c(h_avg,w_avg,3))
    
    # Sub size we need to average
    h_move = round(h/h_avg)
    w_move = round(w/w_avg)
    
    h_start = w_start =  1
    # For each compressed pixel
    for (j in 1:h_avg)
    {
      h_end = ifelse(h_start + h_move - 1 >= h, h, h_start + h_move - 1)
      for (k in 1:w_avg)
      {
        w_end = ifelse(w_start + w_move - 1 >= w, w, w_start + w_move - 1)
        for (l in 1:3)
        {
          # Average the value
          arr_new[j,k,l] = mean(arr_old[h_start:h_end,w_start:w_end,l])
        }
        w_start = w_end + 1
      }
      h_start = h_end + 1
      w_start =  1
    }
    
    img_avg[[i]] = arr_new
  }
  
  return(img_avg)
}

# Calculate the square of the distance between all the images
allDist = function(img_avg, num_img)
{
  mat_dist = matrix(Inf, nrow = num_img, ncol = num_img)
  rownames(mat_dist) = colnames(mat_dist) = 1:num_img
  for (i in 1:(num_img-1))
  {
    for (j in (i+1):num_img)
    {
      # Square of Euclidean distance
      mat_dist[i,j] = mat_dist[j,i] = sum((img_avg[[i]] - img_avg[[j]])^2)
    }
  }
  
  return(mat_dist)
}

# Save images to a file fold according the order of tour guide
saveResult = function(dir, target, tour_guide_name)
{
  file.remove(file.path(target, list.files(target)))
  i = 1
  for (n in tour_guide_name)
  {
    from = file.path(dir, n)
    to = file.path(target, paste0(i, ".jpg"))
    file.copy(from, to)
    i = i + 1
  }
}

# Make a video
makeVideo = function(dir, target, tour_guide_name)
{
  from = file.path(dir, tour_guide_name)
  to = file.path(target, "Image Tour.mp4")
  av_encode_video(from, to, framerate = 1)
}