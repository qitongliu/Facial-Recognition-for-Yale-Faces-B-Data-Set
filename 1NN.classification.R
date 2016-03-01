#### Compare PCA performance on data of different lighting ranges. 
#### The second set of photos have a much wider lighting ranges and PCA works much worse in the second set. 
#### It suggests that PCA works better on data that gather tightly in the feature space

################## First set of photos of narrow lighting ranges #####################
## Load the views P00A+000E+00, P00A+005E+10, P00A+005E-10, and P00A+010E+00
library(pixmap)

# get directory structure
dir_list = dir(path="CroppedYale/",all.files=FALSE)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
view_list = c(  'P00A+000E+00' , 'P00A+005E+10' , 'P00A+005E-10', 'P00A+010E+00')

# initialize an empty matrix of faces data
face_matrix_6a = vector()
# outer loop through the pictures 
for ( i in 1:length(dir_list) ){
  # inner loop over views
  for ( j in 1:length(view_list) ){
    # compile the correct file name
    this_filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list[i] , dir_list[i] , view_list[j])
    # you can print out each name to help debug the code
    # print(this_filename)
    # load the data
    this_face = read.pnm(file = this_filename)
    this_face_matrix = getChannels(this_face)
    this_face_vector = as.vector(this_face_matrix)
    # append the view to the row for this face
    face_matrix_6a = rbind( face_matrix_6a , this_face_vector )
  }
}

subject = rep(1:length(dir_list), each = length(view_list))
view = rep(1:length(view_list), length(dir_list))
labels_a = data.frame(cbind(subject, view))

## Divide the data into training and testing sets
fm_6a_size = dim(face_matrix_6a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6a = floor(fm_6a_size[1]*4/5)
ntest_6a = fm_6a_size[1]-ntrain_6a
set.seed(1)
ind_train_6a = sample(1:fm_6a_size[1],ntrain_6a)
ind_test_6a = c(1:fm_6a_size[1])[-ind_train_6a]

labels_a[ind_train_6a[1:5],]   # subject and view indices of the first 5 rows in the training set
labels_a[ind_test_6a[1:5],]    # subject and view indices of the first 5 rows in the testing set

## Do PCA on the training set and use the firrst 25 scores to represent the data.
## Project testing data onto the first 25 loadings so that it is also represented by the first 25 scores.
## Use 1NN classification in the space of the first 25 scores to identify the subject for each testing observation.
train.faces = face_matrix_6a[ind_train_6a,]
test.faces = face_matrix_6a[ind_test_6a,]
column.mean = colMeans(train.faces)
train.centered = train.faces - matrix(rep(column.mean,nrow(train.faces)), ncol = ncol(train.faces), byrow = T)
test.centered = test.faces - matrix(rep(column.mean,nrow(test.faces)), ncol = ncol(test.faces), byrow = T)
pca = prcomp(train.centered)
train.scores = train.centered %*% pca$rotation[,1:25]
test.scores = test.centered %*% pca$rotation[,1:25]

distance = as.matrix(dist(rbind(test.scores, train.scores), diag = T, upper = T))[1:ntest_6a, (ntest_6a+1):(ntest_6a+ntrain_6a)]
test.subject.pred = labels_a[ind_train_6a[apply(distance, 1, which.min)], 1]
test.subject.real = labels_a[ind_test_6a, 1]
correct = sum(test.subject.real == test.subject.pred)
incorrect = ntest_6a - correct
test.subject.real
test.subject.pred
correct
incorrect


################### Second set of photos of wide lighting ranges ########################
## load the views P00A-035E+15, P00A-050E+00, P00A+035E+15, and P00A+050E+00
# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
view_list = c(  'P00A-035E+15' , 'P00A-050E+00' , 'P00A+035E+15', 'P00A+050E+00')

# initialize an empty matrix of faces data
face_matrix_6c = vector()
# outer loop through the pictures 
for ( i in 1:length(dir_list) ){
  # inner loop over views
  for ( j in 1:length(view_list) ){
    # compile the correct file name
    this_filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list[i] , dir_list[i] , view_list[j])
    # you can print out each name to help debug the code
    # print(this_filename)
    # load the data
    this_face = read.pnm(file = this_filename)
    this_face_matrix = getChannels(this_face)
    this_face_vector = as.vector(this_face_matrix)
    # append the view to the row for this face
    face_matrix_6c = rbind( face_matrix_6c , this_face_vector )
  }
}

subject = rep(1:length(dir_list), each = length(view_list))
view = rep(1:length(view_list), length(dir_list))
labels_c = data.frame(cbind(subject, view))

## Divide the data into training and testing sets
fm_6c_size = dim(face_matrix_6c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_6c = floor(fm_6c_size[1]*4/5)
ntest_6c = fm_6c_size[1]-ntrain_6c
set.seed(2)
ind_train_6c = sample(1:fm_6c_size[1],ntrain_6c)
ind_test_6c = c(1:fm_6c_size[1])[-ind_train_6c]

## Do PCA on the training set and use the firrst 25 scores to represent the data.
## Project testing data onto the first 25 loadings so that it is also represented by the first 25 scores.
## Use 1NN classification in the space of the first 25 scores to identify the subject for each testing observation.
train.faces = face_matrix_6c[ind_train_6c,]
test.faces = face_matrix_6c[ind_test_6c,]
column.mean = colMeans(train.faces)
train.centered = train.faces - matrix(rep(column.mean,nrow(train.faces)), ncol = ncol(train.faces), byrow = T)
test.centered = test.faces - matrix(rep(column.mean,nrow(test.faces)), ncol = ncol(test.faces), byrow = T)
pca = prcomp(train.centered)
train.scores = train.centered %*% pca$rotation[,1:25]
test.scores = test.centered %*% pca$rotation[,1:25]

distance = as.matrix(dist(rbind(test.scores, train.scores), diag = T, upper = T))[1:ntest_6c, (ntest_6c+1):(ntest_6c+ntrain_6c)]
test.subject.pred = labels_c[ind_train_6c[apply(distance, 1, which.min)], 1]
test.subject.real = labels_c[ind_test_6c, 1]
correct = sum(test.subject.real == test.subject.pred)
incorrect = ntest_6c - correct
test.subject.real
test.subject.pred
correct
incorrect

## Plot any subject photos that are misidentified next to the 1NN photo prediction.
test.view.pred = labels_c[ind_train_6c[apply(distance, 1, which.min)], 2]
test.view.real = labels_c[ind_test_6c, 2]
compare.row = NULL
compare.matrix = NULL
k = 0
for(i in 1:ntest_6c){
  if(test.subject.real[i] != test.subject.pred[i]){
    k = k+1
    real.matrix = matrix(face_matrix_6c[4*(test.subject.real[i]-1)+test.view.real[i],], nrow = 192)
    pred.matrix = matrix(face_matrix_6c[4*(test.subject.pred[i]-1)+test.view.pred[i],], nrow = 192)
    compare.row = cbind(compare.row, real.matrix, pred.matrix)
    if(k %% 3 == 0){
      compare.matrix = rbind(compare.matrix, compare.row)
      compare.row = NULL
    }
  }
}
compare = pixmapGrey(compare.matrix)
plot(compare)
title('subject photos next to misidentified predictions')
filename = 'subject photos next to misidentified predictions.png'
dev.copy(device=png, file=filename, height=600, width=500)
dev.off()

## Redo the second set of photos with 10 different training and testing divides.
correct = NULL
incorrect = NULL
for(i in 1:10){
  set.seed(i)        ## set the random seed to 1:10
  ind_train_6c = sample(1:fm_6c_size[1],ntrain_6c)
  ind_test_6c = c(1:fm_6c_size[1])[-ind_train_6c]
  
  train.faces = face_matrix_6c[ind_train_6c,]
  test.faces = face_matrix_6c[ind_test_6c,]
  column.mean = colMeans(train.faces)
  train.centered = train.faces - matrix(rep(column.mean,nrow(train.faces)), ncol = ncol(train.faces), byrow = T)
  test.centered = test.faces - matrix(rep(column.mean,nrow(test.faces)), ncol = ncol(test.faces), byrow = T)
  pca = prcomp(train.centered)
  train.scores = train.centered %*% pca$rotation[,1:25]
  test.scores = test.centered %*% pca$rotation[,1:25]
  
  distance = as.matrix(dist(rbind(test.scores, train.scores), diag = T, upper = T))[1:ntest_6c, (ntest_6c+1):(ntest_6c+ntrain_6c)]
  test.subject.pred = labels_c[ind_train_6c[apply(distance, 1, which.min)], 1]
  test.subject.real = labels_c[ind_test_6c, 1]
  correct[i] = sum(test.subject.real == test.subject.pred)
  incorrect[i] = ntest_6c - correct[i]
}
correct
incorrect
