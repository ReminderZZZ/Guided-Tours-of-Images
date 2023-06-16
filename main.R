# Packages needed
library(colorspace)
library(recolorize)
library(abind)
library(tourr)
library(av)

# Load functions
source("processData.R")
source("baseTour.R")
source("apprTSP.R")
source("tourCycle.R")
source("reduceDim.R")
source("randProject.R")
source("baseSampleTour.R")
source("tourRP.R")

# Image data file fold
input = "Data/landscape2"
# Output directory
output_img = "Data/test1_result" # Sorted images
output_video = "Data/video" # Video

# Preprocessing
# Get data
data_img = getLuv(input, size = 200)
# Number of images
num_img = data_img[[3]]
# Image names
name_img = data_img[[4]]
# Average the images
img_avg = imgAvg(data_img, num_img)
# Interesting projection
img_avg_RP = randProject(data_img, num_img)
# Reduce colour dimensions
#img_avg = reduceDim(img_avg, num_img)
# Calculate the image distances
img_dist = allDist(img_avg, num_img)
# Image distances with random projection
img_dist_RP = allDist(img_avg_RP, num_img)


# Baseline method
tour_guide_ind = sample(num_img, 15)
tour_guide_name = name_img[tour_guide_ind]

# Greedy algorithm
tour_guide = baseTour(img_dist, num_img, name_img, target = 15, seed = 1)
# With random projection
tour_guide_RP = baseTour(img_dist_RP, num_img, name_img, target = 15, seed = 1)

# Approximate solution to Travelling salesman problem
tour_guide = apprTSP(img_dist, num_img, name_img, target = 15, seed = 1)
# With random projection
tour_guide_RP = apprTSP(img_dist_RP, num_img, name_img, seed = 2)

# Image tour cycle
first_3_img = sample(name_img, 3) # Pick first three images
tour_guide = sameDirection(img_avg, name_img, num_img, first_3_img,
                           precision = 20, target = 15)

# Apply sampling image candidate set to greedy algorithm
tour_guide = baseSampleTour(img_avg, name_img, num_img, sample_size = 10, target = 15, seed = 1)

# A greedy algorithm based on the random projection grand tour
tour_guide = tourRP(data_img, num_img, target = 15, seed = 1)

# Save results
tour_guide_ind = tour_guide[[1]]
tour_guide_name = tour_guide[[2]]
saveResult(input, output_img, tour_guide_name)

# Save results with random projection
tour_guide_ind = tour_guide_RP[[1]]
tour_guide_name = tour_guide_RP[[2]]
saveResult(input, output_img, tour_guide_name)

# Make video and save
makeVideo(input, output_video, tour_guide_name)
