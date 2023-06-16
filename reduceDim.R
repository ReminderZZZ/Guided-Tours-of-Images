# Reduce colour dimensions
# Only keep the Luv data on the top left
reduceDim = function(img_avg, num_img)
{
  for (i in 1:num_img)
  {
    img_avg[[i]] = array(img_avg[[i]][1,1,], dim = c(1,1,3))
  }
  
  return (img_avg)
}