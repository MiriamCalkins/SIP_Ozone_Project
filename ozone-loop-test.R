# ozone-loop-test.R
#
# Test a function and "loop" (with sapply) to refactor a large section of code.
#
# Problem: Given a "site", find 4th highest ozone value for matching rows,
#          and process many data frames with many site strings this way.

# Create a function to find 4th highest "j" (oz_num) for matching "i" (site_id)
sort_oz <- function (oz_num, site_id) {
    with(oz_num, sort(j[i==site_id], decr=T)[4])
}

# Create data frames with "i"=A-E, repeated 10 times, and random values for "j"
oz1 <- data.frame(i=rep(LETTERS[1:5], times=10), j=rnorm(50))
oz2 <- data.frame(i=rep(LETTERS[1:5], times=10), j=rnorm(50))
oz3 <- data.frame(i=rep(LETTERS[1:5], times=10), j=rnorm(50))

# Create a list of these data frames to loop through
dflist <- list(oz1, oz2, oz3)

# Loop through list of data frames with sapply and run sort function on each.
# We use sapply() twice, but this could also be a nested pair of for-loops.
sites <- c("A", "B", "C", "D", "E")
res <- sapply(sites, function (y) {sapply(dflist, function(x) sort_oz(x, y))})

# View the results
rownames(res) <- c("oz1", "oz2", "oz3")
res

# Spot check some values as a test of "res". Make sure you get correct results.
with(oz1, sort(j[i=="A"], decr=T))
with(oz1, sort(j[i=="A"], decr=T)[4])
res[rownames(res)=="oz1","A"]
with(oz1, sort(j[i=="A"], decr=T)[4]) == res[rownames(res)=="oz1","A"]
# You can do the same checks with "B", etc. and oz2, etc. until convinced.
