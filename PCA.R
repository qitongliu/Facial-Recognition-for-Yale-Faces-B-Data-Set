## Load the views P00A+000E+00, P00A+005E+10, P00A+005E-10, and P00A+010E+00 for all subjects. 
## Convert each photo to a matrix (using getChannels) and then to a vector 
## store the collection as a matrix where each row is a photo
library(pixmap)

# get directory structure
dir_list = dir(path="CroppedYale/",all.files=FALSE)

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
view_list = c(  'P00A+000E+00' , 'P00A+005E+10' , 'P00A+005E-10', 'P00A+010E+00')

# initialize an empty matrix of faces data
faces_matrix = vector()
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
    faces_matrix = rbind( faces_matrix , this_face_vector )
  }
}

## Compute a mean face, which is the average for each pixel across all of the faces. 
column.mean = colMeans(faces_matrix)
mean.face.matrix = matrix(column.mean, nrow = nrow(this_face_matrix))
mean.face = pixmapGrey(mean.face.matrix)
plot(mean.face)
title('mean face')
filename = 'mean face.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

## Subtract mean face off each of the faces, then find the principal components of the image matrix.
faces.centered = faces_matrix - matrix(rep(column.mean,nrow(faces_matrix)), ncol = ncol(faces_matrix), byrow = T)
pca = prcomp(faces.centered)
screeplot(pca, type = "l", main = "Proportion of the variance explained against number of components")

## Display each principal component as an eigenface
eigenface.matrix = vector()
k = 1
for (i in 1:3){
  eigenface.row = vector()
  for(j in 1:3){
    this.eigenface = matrix(pca$rotation[,k],nrow = nrow(this_face_matrix))
    eigenface.row = cbind(eigenface.row, this.eigenface)
    k = k+1
  }
  eigenface.matrix = rbind(eigenface.matrix, eigenface.row)
}
eigenfaces = pixmapGrey(eigenface.matrix)
plot(eigenfaces)
title('3x3 grid of eigenfaces')
filename = '3x3 grid of eigenfaces.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

## Use the eigenfaces to reconstruct yaleB05 P00A+010E+00.pgm
newface.centered = faces.centered[20,]
newface.matrix = vector()
k = 0
for (i in 1:5){
  newface.row = vector()
  for (j in 1:5){
    if (k == 0){
      this.newface = mean.face.matrix
    } else {
      this.newface.vector = newface.centered %*% pca$rotation[,1:k] %*% t(pca$rotation[,1:k])
      this.newface = mean.face.matrix + matrix(this.newface.vector, nrow = nrow(this_face_matrix))
    }
    k = k+1
    newface.row = cbind(newface.row, this.newface)
  }
  newface.matrix = rbind(newface.matrix, newface.row)
}
newfaces = pixmapGrey(newface.matrix)
plot(newfaces)
title('5x5 grid of reconstructing adding one eigenface')
filename = '5x5 grid of reconstructing adding one eigenface.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

newface5.matrix = vector()
k = 0
for (i in 1:5){
  newface5.row = vector()
  for (j in 1:5){
    if (k == 0){
      this.newface5 = mean.face.matrix
    } else {
      this.newface5.vector = newface.centered %*% pca$rotation[,1:(5*k)] %*% t(pca$rotation[,1:(5*k)])
      this.newface5 = mean.face.matrix + matrix(this.newface5.vector, nrow = nrow(this_face_matrix))
    }
    k = k+1
    newface5.row = cbind(newface5.row, this.newface5)
  }
  newface5.matrix = rbind(newface5.matrix, newface5.row)
}
newfaces5 = pixmapGrey(newface5.matrix)
plot(newfaces5)
title('5x5 grid of reconstructing adding five eigenfaces')
filename = '5x5 grid of reconstructing adding five eigenfaces.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

## Remove the pictures of subject 01 from the image matrix and recenter the data. 
## Use new principal components to reconstruct yaleB01 P00A+010E+00.pgm
removed.faces.matrix = faces_matrix[-(1:4),]
removed.column.mean = colMeans(removed.faces.matrix)
removed.meanface = matrix(removed.column.mean, nrow = nrow(this_face_matrix))
removed.faces.centered = removed.faces.matrix - matrix(rep(removed.column.mean,nrow(removed.faces.matrix)), ncol = ncol(faces_matrix), byrow = T)
removed.pca = prcomp(removed.faces.centered)
remaining.image = faces_matrix[4,] - removed.column.mean
reconstruct.vector = remaining.image %*% removed.pca$rotation %*% t(removed.pca$rotation)
reconstruct.matrix = removed.meanface + matrix(reconstruct.vector, nrow = nrow(this_face_matrix))
reconstruct.image = pixmapGrey(reconstruct.matrix)
plot(reconstruct.image)
title('Reconstructed Image')
filename = 'Reconstructed Image.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
