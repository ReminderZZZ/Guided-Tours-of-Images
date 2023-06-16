# Approximate solution to Travelling salesman problem

# Get the adjacency distance matrix
getDistList = function(img_dist, num_img)
{
  adj_mat = matrix(NA, 0, 3)
  
  for (i in 1:(num_img-1))
  {
    for (j in (i+1):num_img)
    {
      adj_mat = rbind(adj_mat, c(i,j,img_dist[i,j]))
    }
  }
  
  return(adj_mat)
}

# Find minimal spanning tree (Prim algorithm)
Prim = function(img_dist, num_img)
{
  # Adjacency distance matrix
  adj_mat = getDistList(img_dist, num_img)
  
  tree_main = 1
  MST = matrix(NA, 0, 3)
  while (length(tree_main) < num_img)
  {
    adj_sub = adj_mat[which(adj_mat[,1] %in% tree_main |
                              adj_mat[,2] %in% tree_main),]
    # Search from main tree
    for (i in order(adj_sub[,3]))
    {
      pair = adj_sub[i,]
      # Avoid forming cycle
      if (!all(pair[1:2] %in% tree_main))
      {
        MST = rbind(MST, pair)
        tree_main = c(tree_main, pair[1:2])
        tree_main = unique(tree_main)
        break
      }
    }
  }
  
  return(MST)
}

# Depth first search
DFS = function(MST, s, target)
{
  MST_copy = MST
  MST_copy = cbind(MST_copy, 1:nrow(MST_copy))
  MST_copy = rbind(MST_copy, Inf, Inf)
  search = s
  seq = s
  while (length(search) > 0 & length(seq) < target)
  {
    adj_sub = MST_copy[which(MST_copy[,1] %in% search[length(search)] |
                          MST_copy[,2] %in% search[length(search)]),]
    if (length(adj_sub) != 0)
    {
      adj_sub = rbind(adj_sub, Inf)
      leftChild = adj_sub[order(adj_sub[,3])[1],]
      MST_copy = MST_copy[-which(MST_copy[,4]==leftChild[4]),]
      if (leftChild[1] %in% search) 
      {
        search = c(search, leftChild[2])
        seq = c(seq, leftChild[2])
      }
      else {search = c(search, leftChild[1]); seq = c(seq, leftChild[1])}
      
    }
    else search = search[-length(search)]
  }
  
  return(seq)
}

# Main function
apprTSP = function(img_dist, num_img, name_img, target = 10, seed = NA)
{
  if (!is.na(seed)) set.seed(seed)
  if (target > num_img) target = num_img
  
  # Random start
  s = sample(num_img, 1)
  # Prim algorithm
  MST = Prim(img_dist, num_img)
  # Depth first search
  tour_guide_ind = DFS(MST, s, target)
  tour_guide_name = name_img[tour_guide_ind]
  
  return(list(tour_guide_ind, tour_guide_name))
}