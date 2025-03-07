# Intel Smartwatch

# When you open RStudio:
# Source Editor (top-left): where you write your R code in files (scripts).
# Console (bottom-left): where code is executed.
# Environment/History (top-right): to inspect variables or your command history.
# Files/Plots/Packages/Help (bottom-right): for navigating files, viewing plots, etc.

# Create or Open RScript:
# File > New File > R Script.
# Type your code in the new script window.

# Run the code:
# Highlight the line(s) of code and press Ctrl + Enter on Windows (Command + Enter on Mac)
# Click the Run button (usually top-right in the script panel).
# The code will execute, and you’ll see results in the Console pane.

################## Intel CASE ###################

# Note:
# - Replace the blanks with appropriate functions, objects, or parameters.
# - Try to reason through each step of the script to understand what it is doing.


# Install necessary packages if not already installed
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")
install.packages("factoextra")
install.packages("dendextend")

# Load the required libraries
library("readxl")      # For reading Excel files
library("tidyverse")      # For data manipulation and visualisation
library("cluster")    # For clustering methods
library("openxlsx")      # For exporting data to Excel
library(factoextra)
library(dendextend)      # For colored dendrogram

################## PREPARATION ###################

# IMPORTING DATA FROM EXCEL 
# "file.choose()" lets the user select a file interactively.
SmartWatch <- read_excel(file.choose())
# View the imported dataset (opens in a separate viewer)
View(SmartWatch)      # Name of the dataset


# INITIAL DATA EXPLORATION 
# Display column names of the dataset
names(SmartWatch)    # object that holds data
# Display basic summary statistics (e.g., min, max, mean, etc.) for each variable
summary(SmartWatch)  
# Explore the structure of the dataset (e.g., column types, number of rows)
str(SmartWatch)      

# Create a new dataframe
df <- SmartWatch   
names(df)

# STANDARDISE DATA
# "scale()" standardises the data to have a mean of 0 and standard deviation of 1.
dfz <- scale(df)       # data should be scaled as there are significant differences and to calculate the euclidean distance
# View the standardised data
View(dfz) 


################### SEGMENTATION STEP ###################


# CALCULATE EUCLIDEAN DISTANCE
# "dist(dfz)" calculates pairwise distances between rows of a dataset.
distance <- dist(dfz, method = 'euclidean')


#1. CLUSTER DENDROGRAM 
# Perform hierarchical clustering
?hclust # if we want to know about the function hclust
hc.w <- hclust(distance, method = 'ward.D2') 
# Using Ward.D2 linkage
# Plot the dendrogram to visualise the clustering
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")


# DETERMINE THE OPTIMAL NUMBER OF CLUSTERS
# Use the elbow method to decide the number of clusters.
x <- c(1:10)
sort_height <- sort(hc.w$height, decreasing = TRUE)    # we are trying to sort height
y <- sort_height[1:10]

#2. Plot elbow plot
plot(x, y, type = "b", main = "Elbow plot", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares")     # Hint: "Elbow Plot"
# Identify the optimal number of clusters visually
optimal <- 3  # Change based on where the "elbow" is in your dataset
points(optimal, y[optimal], col = "blue", pch = 5, cex = .25)
text(optimal, y[optimal], labels = "Optimal Cluster", pos = 1, col = "blue", cex=.4)
# After cluster=3, adding more clusters isn’t significantly improving the fit within the clusters.
# We can consider 3 as the optimal number of clusters.

# CUT DENDROGRAM INTO 3 CLUSTERS
cluster <- cutree(hc.w, k = 3)    # Number of clusters was chosen as 3
# Display cluster assignments
print(cluster)
# Create a frequency table to see the size of each cluster
table(cluster)
# Add cluster assignments back to the original data
df_final <- cbind(df, cluster)    #Combining the original data with clusters.
# Check the updated dataset
View(df_final)

# Convert hclust object to color dendrogram
dend <- as.dendrogram(hc.w)
# Color branches for 3 clusters
dend <- dend %>%
  set("branches_k_color", k = 3) %>%  # Color branches for 3 clusters
  set("branches_lwd", 2)  # Make branches thicker for better visibility
# Plot the dendrogram with colored branches
plot(dend, main = "Cluster Dendrogram with Colored Branches", 
     xlab = "Observations", ylab = "Height")


################### DESCRIPTION STEP ###################

# CALCULATE SEGMENT SIZES
proportions <- table(df_final$cluster) / nrow(df_final)     #Comparing with final dataset to get the proportions of segments
percentages <- proportions * 100
# Display segment sizes in percentages
print(percentages)     # variable that holds the percentages
SegmentSize <- percentages

# EXPLORE MEAN VALUES OF VARIABLES IN EACH CLUSTER
segments<-
  df_final %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))
# Display the calculated means
segments

# SAVE MEAN TABLE TO EXCEL 
write.xlsx(segments, 'watchsegment2.xlsx')     #variable that holds the summarised data
# Please go to your working directory to open the Excel file and explore.

# To check the current working directory
getwd()  # Prints the path of the current working directory
# To set a new working directory (replace 'your/path/here' with the desired path)
setwd("/Users/pritid/Downloads/BEM463")
# Example: If the working directory is set to "C:/Users/Documents"
# setwd("/Users/pritid/Downloads")


# IMPORTING DATA FROM EXCEL 
# "file.choose()" lets the user select a file interactively.
watchsegment2 <- read_excel(file.choose())

# View the imported dataset (opens in a separate viewer)
View(watchsegment2)      # name of the new dataset

# View summary statistics
summary(watchsegment2)

