library(sf);library(tree)

##Task 1.a
accident_data = sf::st_read('data_accident\\Accident_data.shp')

##Task 1.b
cols_to_factor <- c("accident", "intersect", "F_SYSTEM","BelowFreez", "Fog", "Thunder","FrozenPrec")

# cols_to_factor <- c("accident", "intersect", "BelowFreez", "Fog", "Thunder","FrozenPrec")

# Convert each column to factor
accident_data <- within(accident_data, {
  for (col in cols_to_factor) {
    assign(col, as.factor(get(col)))
  }
})

## Task 2.a
# split data into 70% training and 30% testing
sample_idx <- sample(nrow(accident_data), size = 0.7 * nrow(accident_data))
train <- accident_data[sample_idx, ]
test <- accident_data[-sample_idx, ]

table(train$accident)
table(test$accident)

##Task 2.b
# Build a decision tree
tree.acc = tree(accident~hour + F_SYSTEM + intersect+NACH_LEN_r + NAIN_LEN_r +NUM_LANES + lane_width+BelowFreez + Fog + Thunder + FrozenPrec, data = train)
summary(tree.acc)

##Task2.c
##plot tree
plot(tree.acc)
text(tree.acc ,pretty =0)

##Task 3.a
## cv with k = 5
cv.carseats =cv.tree(tree.acc,FUN = prune.misclass, K = 5)
names(cv.carseats)
cv.carseats

##Task 4
## prune the tree
prune.acc = prune.misclass(tree.acc, best =6)
prune.acc
plot( prune.acc)
text( prune.acc , pretty =0)

##Task 5
## predict the result
tree.pred = predict(prune.acc, test ,type ="class")
table (tree.pred ,test$accident)

