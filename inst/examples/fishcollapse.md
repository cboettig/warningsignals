

#  Warning Signals in Fish Collapse

 * Author Carl Boettiger <cboettig@gmail.com>
 * Date: 10 Nov 2011
 * Description: an example trying to detect early warning signals
  in data from fisheries collapses

Set up markdown format, image uploads, and 


```r
render_gfm()
opts_knit$set(upload = TRUE)
opts_knit$set(imgur.key = getOption("imgur"))

require(warningsignals)
```



```
## Loading required package: warningsignals
```



```
## Loading required package: snowfall
```



```
## Loading required package: snow
```



```
## Loading required package: mcmcTools
```



```r
require(ggplot2)
```



```
## Loading required package: ggplot2
```



```r
require(reshape2)
```



```
## Loading required package: reshape2
```





## Load the data 


```r
scotia <- read.csv("../../data/rawdata/sau_scotia.csv")
```




## Visualize data 



```r
dat_scotia <- melt(scotia, id = "Year")
p_scotia <- ggplot(dat_scotia, aes(Year, value, 
    fill = variable)) + geom_area()
print(p_scotia)
```

![plot of chunk unnamed-chunk-3](http://i.imgur.com/Tpjp0.png) 


## Compute some indicators

Define some indicators


```r
window_var <- function(X, windowsize = (length(X)/2)) {
    out <- sapply(0:(length(X) - windowsize), function(i) {
        var(X[(i + 1):(i + windowsize)])
    })
    c(rep(NA, length(X) - length(out)), out)
}

window_autocorr <- function(X, windowsize = (length(X)/2)) {
    out <- sapply(0:(length(X) - windowsize), function(i) acf(X[(i + 
        1):(i + windowsize)], lag.max = 1, plot = F)$acf[2])
    c(rep(NA, length(X) - length(out)), out)
}
```




Reformat the data, uses data.table to perform computations over species


```r
require(data.table)
```



```
## Loading required package: data.table
```



```
## data.table 1.7.10  For help type: help("data.table")
```



```r
fish <- data.table(subset(dat_scotia, Year < 1992))
tmp <- data.frame(species = fish$variable, Year = fish$Year, 
    Stock = fish$value, variance = fish[, window_var(value), 
        by = "variable"]$V1, acor = fish[, window_autocorr(value), 
        by = "variable"]$V1)

dat <- melt(tmp, id = c("Year", "species"))
```




Cod are approaching a crash, but lobster are going strong, but both seem to show the same pattern.  


```r
ggplot(subset(dat, species %in% c("Atlantic.cod"))) + 
    geom_point(aes(Year, value)) + facet_grid(variable ~ 
    species, scales = "free_y")
```



```
## Warning message: Removed 20 rows containing missing values (geom_point).
```



```
## Warning message: Removed 20 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-6](http://i.imgur.com/czslO.png) 

```r
ggplot(subset(dat, species %in% c("American.lobster"))) + 
    geom_point(aes(Year, value)) + facet_grid(variable ~ 
    species, scales = "free_y")
```



```
## Warning message: Removed 20 rows containing missing values (geom_point).
```



```
## Warning message: Removed 20 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-6](http://i.imgur.com/BXcFG.png) 


Note the indicator patterns vary widly and rather arbitrarily among species 


```r
dt <- data.table(dat_scotia)
indicator <- data.frame(dt[, window_var(value), 
    by = "variable"], Year = dat_scotia$Year)
ggplot(indicator) + geom_line(aes(Year, V1)) + 
    facet_wrap(~variable, scales = "free_y")
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```



```
## Warning message: Removed 28 rows containing missing values (geom_path).
```

![plot of chunk unnamed-chunk-7](http://i.imgur.com/vcYzx.png) 







