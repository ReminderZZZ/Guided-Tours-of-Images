# Image tour cycle

# Get the initial movement information
getDirection = function(img_1, img_2, img_3)
{
  l_direct = diff(c(mean(img_1[,,1]), mean(img_2[,,1]), mean(img_3[,,1])))
  u_direct = diff(c(mean(img_1[,,2]), mean(img_2[,,2]), mean(img_3[,,2])))
  v_direct = diff(c(mean(img_1[,,3]), mean(img_2[,,3]), mean(img_3[,,3])))
  direction = matrix(c(l_direct, u_direct, v_direct), ncol = 3)
  
  return(direction)
}

# cross product
cross <- function(a, b) {
  normal_vector = c(c(a[2]*b[3] - a[3]*b[2], a[3]*b[1] - a[1]*b[3],
                      a[1]*b[2] - a[2]*b[1]))
  if (all(normal_vector == 0))  normal_vector = c(0,0,1)
  
  return(normal_vector)
}

# Get the loop of the image tour
getCircle = function(img_1, img_2, img_3, direction)
{
  direction = direction[,c(2,3,1)]
  l_mid = mean(c(mean(img_1[,,1]), mean(img_2[,,1]), mean(img_3[,,1])))
  u_mid = mean(c(mean(img_1[,,2]), mean(img_2[,,2]), mean(img_3[,,2])))
  v_mid = mean(c(mean(img_1[,,3]), mean(img_2[,,3]), mean(img_3[,,3])))
  mid_point = c(u_mid, v_mid, l_mid)
  # Normal vector of the flat
  normal_vector = cross(direction[1,], direction[2,])
  # The vector to move the center of the circle
  direction_avg = apply(direction, 2, mean)
  direction_circle = cross(normal_vector, direction_avg)
  
  # Initialization
  circle = T
  center = mid_point
  len_direct = sqrt(sum(direction_avg^2))
  len_circle = sqrt(sum(direction_circle^2))
  first_point = c(mean(img_1[,,2]), mean(img_1[,,3]), mean(img_1[,,1]))
  second_point = c(mean(img_2[,,2]), mean(img_2[,,3]), mean(img_2[,,1]))
  third_point = c(mean(img_3[,,2]), mean(img_3[,,3]), mean(img_3[,,1]))
  data_circle = matrix(c(first_point, second_point, third_point),
                       ncol = 3, byrow = T)
  # The speed and direction of the movement of the center of the circle
  move_circle = direction_circle/len_circle
  if (sum((center+move_circle-second_point)^2) <
                         sum((center-move_circle-second_point)^2))
                    move_circle = -move_circle
  avg_point_1 = mid_point + direction_avg/2
  # Find the largest circle
  while (circle)
  {
    center = center + move_circle
    r = sqrt(sum((center - avg_point_1)^2))
    angle = acos((2*r^2 - len_direct^2) / (2*r^2))
    phi = seq(0, 2*pi, length = (2*pi)/angle)
    theta = atan(-(normal_vector[1]*cos(phi) + normal_vector[2]*sin(phi))
                 /normal_vector[3])
    u_c = center[1] + r*cos(theta)*cos(phi)
    v_c = center[2] + r*cos(theta)*sin(phi)
    l_c = center[3] + r*sin(theta)
    
    # If the circle is out of boundary
    if (any(l_c < 0 | l_c > 100) | any(abs(u_c) > 100) | any(abs(v_c) > 100))
    {
      circle = F
    }
    else if (length(l_c) > 2) data_circle = matrix(c(u_c, v_c, l_c), ncol = 3)
  }

  return(data_circle[-1,])
}

# Image tour according first three images (speed and direction)
sameDirection = function(img_avg, name_img, num_img, names, target = 10, precision = 3)
{
  if (target > num_img) target = num_img
  
  # Initialization
  name_img_copy = name_img
  img_1_label = which(name_img == names[1])
  img_2_label = which(name_img == names[2])
  img_3_label = which(name_img == names[3])
  if (img_1_label == img_3_label) stop("Image 1 and image 3 must be different!")
  img_1 = img_avg[[img_1_label]]
  img_2 = img_avg[[img_2_label]]
  img_3 = img_avg[[img_3_label]]
  
  # Get the loop of the image tour
  direction = getDirection(img_1, img_2, img_3)
  circle = getCircle(img_1, img_2, img_3, direction)
  
  # Find the positions of first 2 images in the circle
  start = matrix(c(mean(img_1[,,2]), mean(img_1[,,3]), mean(img_1[,,1])),
                 byrow = T, nrow = 1)
  then = matrix(c(mean(img_3[,,2]), mean(img_3[,,3]), mean(img_3[,,1])),
                byrow = T, nrow = 1)
  point_1 = which.min(apply(sweep(circle, 2, start), 1, function(i) sum(i^2)))
  point_3 = which.min(apply(sweep(circle, 2, then), 1, function(i) sum(i^2)))
  
  # Confirm the direction
  move = ifelse(point_3 < point_1, -1, 1)
  point_next = ifelse((point_3 + move) %% nrow(circle) != 0,
                      (point_3 + move) %% nrow(circle), nrow(circle))
  
  # Initialization
  find_img = T
  name_img = name_img[-c(img_1_label, img_2_label, img_3_label)]
  img_avg = img_avg[-c(img_1_label, img_2_label, img_3_label)]
  search_range = sum(apply(direction, 2, mean)^2) * precision
  
  # Stop when can not find suitable images
  while(find_img & length(names) < target)
  {
    find_img = F
    if (length(img_avg) == 0) break
    img_diff = numeric(length(img_avg))
    for (i in 1:(num_img-3))
    {
      img_diff[i] = sum((c(mean(img_avg[[i]][,,2]), mean(img_avg[[i]][,,3]),
                   mean(img_avg[[i]][,,1])) - circle[point_next,])^2)
    }
    # The image closest to our request
    nearest = which.min(img_diff)
    # If find suitable images
    if (img_diff[nearest] <= search_range)
    {
      names = c(names, name_img[nearest])
      find_img = T
      num_img = num_img - 1
      name_img = name_img[-nearest]
      img_avg = img_avg[-nearest]
    }
    point_next = ifelse((point_next + move) %% nrow(circle) != 0,
                        (point_next + move) %% nrow(circle), nrow(circle))
  }
  
  labels = numeric(length(names))
  for (i in 1:length(names)) labels[i] = which(name_img_copy == names[i])
  return(list(labels, names))
}