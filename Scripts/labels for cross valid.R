#labelling data for the confusion matrix/external cluster validation

labels <- veri

Z1 <- veri[c(9,24,31,39,41,43,45,50,53,60)]
Z2 <- veri[c(3,12,15,21,23,28,32,35,49,57)]
Z3 <- veri[c(1,8,10,16,17,22,27,34,40,47)]
Z4 <- veri[c(7,33,36,38,52,54,55,58,59)]
Z5 <- veri[c(2,4,11,14,18,19,26,30,37,44)]
Z6 <- veri[c(6,13,20,25,29,42,46,48,51,56)]


#Compute the dimension score for each obs

         labels$Z1 <- rowSums(Z1)
         labels$Z2 <- rowSums(Z2)
         labels$Z3 <- rowSums(Z3)
         labels$Z4 <- rowSums(Z4)
         labels$Z5 <- rowSums(Z5)
         labels$Z6 <- rowSums(Z6)
         
         z_scores <- labels[c(61,62,63,64,65,66)]
         
         cluster_labels <- colnames(z_scores)[max.col(z_scores,ties.method="random")]
         