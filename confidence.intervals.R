#
#  These functions require a vector, x, of positive real numbers

# R code for 95% confidence interval of a median
# Function definition
"conf.med"<-function(x)
{
        v <- sort(x, na.last = NA)
        n <- length(x)
        if(n > 0) {
                m <- median(x)
                i <- qbinom(0.025, n, 0.5)
                if(i > 0)
                        r <- c(m, v[i], v[n - i + 1])
                else r <- c(m, NA, NA)
        }
        else r <- c(NA, NA, NA)
        r <- as.data.frame(list(median = r[1], lower = r[2], upper = r[3]))
        class(r) <- "table"
        r
}

# R code for 95% confidence interval of a mean
# Function definition
"ci95"<-function(x){
     t.value<-qt(0.975,length(x)-1)
     standard.error<-sqrt(var(x)/length(x))
     ci<-t.value*standard.error
     cat("95 % confidence interval for ",mean(x), " = ", mean(x) - ci, "to", mean(x)+ci,"\n")
     t<-c(mean(x),mean(x) - ci,mean(x)+ci)
     t <- as.data.frame(list(mean=t[1], upper = t[2], lower = t[3]))
     class(t) <- "table"
     t
     }   

