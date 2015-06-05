# ozone-loop-test.R
#
# Test a function and "loop" (with sapply) to refactor a large section of code.
#
# Problem: Given a "site", find 4th highest ozone value for matching rows,
#          and process many data frames with many site strings this way.

# Create a function to find "nth" highest "j" value for matching "i" (id)
highest <- function (df, id, n) {
    with(df, sort(j[i==id], decr=T)[n])
}

# Create data frames with "i"=A-E, repeated 10 times, and random values for "j"
oz1 <- data.frame(i=rep(LETTERS[1:5], times=10), j=rnorm(50))
oz2 <- data.frame(i=rep(LETTERS[1:5], times=10), j=rnorm(50))
oz3 <- data.frame(i=rep(LETTERS[1:5], times=10), j=rnorm(50))

# Create a list of these data frames to loop through
dflist <- list(oz1, oz2, oz3)

# Loop through list of data frames with sapply and run sort function on each.
# We use sapply() twice, but this could also be a nested pair of for-loops.
ids <- c("A", "B", "C", "D", "E")
res <- sapply(ids, function (y) {sapply(dflist, function(x) highest(x, y, 4))})

# View the results
rownames(res) <- c("oz1", "oz2", "oz3")
res

# Spot check some values as a test of "res". Make sure you get correct results.
with(oz1, sort(j[i=="A"], decr=T))
with(oz1, sort(j[i=="A"], decr=T)[4])
res[rownames(res)=="oz1","A"]
with(oz1, sort(j[i=="A"], decr=T)[4]) == res[rownames(res)=="oz1","A"]
# You can do the same checks with "B", etc. and oz2, etc. until convinced.
