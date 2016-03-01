# Facial Recognition for Yale Faces B Data Set
I did a facial recognition for Yale Faces B data set which contains 38 subjects, each photographed in a variety of lighting conditions. 

I first loaded in all of the images and stored them as a matrix where each row is a vector converted from an image. 
Therefore, I could plot an average face, which came from the average value for each pixel across all of the faces. 

Then I did Principle Component Analysis on the whole image matrix and successfully reconstructed a recognizable face using 25 principle components. In other words, this step can save storage space while we can still easily recognize that it’s the same person. 

Next, I want computers to identify the person of the reconstructed photo. I randomly divided the whole data set into training and testing data sets. After doing PCA on the training set, I projected the testing data onto the first 25 loadings so that they are also represented by the first 25 scores. Then I used 1-Nearest Neighbor classification in the space of the first 25 scores to identify the subject for each testing observation.
