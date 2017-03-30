install.packages("pangaear")

library("pangaear")
library("tibble")
library("ggplot2")
library("mgcv")

#search PANGAEA for datasets from Taricco and store in recs 
recs <- pg_search(query = "citation:Taricco")

#view the datasets available - optional
recs$citation

#select dataset
res <- pg_data(doi = recs$doi[2])       # doi = "10.1594/PANGAEA.857573"

#check the structure of meta, here we find where latitude and longtitude are  
res[[1]]$meta

#next we're gonna need to extract these values using a regular expression pattern
#and the following matching command. 
coord <- stringr::str_extract_all(res[[1]]$meta$meta[11],"([0-9.-]+)")

#turn the values to numeric and assign them as lat and lon ,
#we can see in the structure of the meta which one is the first value
lon <- as.numeric(coord[[1]][2])
lat <- as.numeric(coord[[1]][1])

#now we have the lat and lon clear. All we need to do is point it on a map.
#there are many kinds of maps. A simple map plot with our site pointed is created using the following.

#using the maps package
library(maptools)
library(maps)

#take the world map and draw it
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
#then point out our lons and lats (these can be vectors containing multiple points) , and give them colour
points(lon,lat, col="red", pch=12)


#######NOW We want to see some visualizations on our data.
#first of all it's better to extract the data object , this makes it easier to tidy up var/col names also.
foram <- res[[1]]$data
#assign var names
names(foram) <- c("Depth", "Age_AD", "Age_kaBP", "d18O")
#and check
foram

#next let's see what the dataset looks like
#assign names on axis
ylabel <- expression(delta^{18} * O ~ "[‰ VPDB]")
xlabel <- "Age [ka BP]"
#and plot. Note that we have to reverse the scale due to the type of data.
#the lower the  ä^18 *Ï the higher the temperature so in the plot , warmer and down , is colder. 
ggplot(foram, aes(y = d18O, x = Age_kaBP)) +
  geom_path() +
  scale_x_reverse(sec.axis = sec_axis( ~ 1950 - (. * 1000), name = "Age [AD]")) +
  scale_y_reverse() +
  labs(y = ylabel, x = xlabel)

#to model the data in the same time ordered way as before, we have to add a variable that is -Age_kaBP
#not using the AD , because of discontinuity at 0AD (0AD doesn't exist)
foram <- with(foram, add_column(foram, Age = - Age_kaBP))

#after that we can fit a generalized additive model to the ä^18 *O record
m <- gam(d18O ~ s(Age, k = 100, bs = "ad"), data = foram, method = "REML", select = TRUE)
#bs="ad" is an adaptive smoother, Adaptive smoothers tend to use a lot more computing resources but can provide a better fit
#note that adaptive smoothers won't work well in short time series 


gam.check(m)
## RStudio users might need
## layout(matrix(1:4, ncol = 2, byrow = TRUE))
## gam.check(m)
## layout(1)
## to see all the plots on one device

#we can now get a summary over m
summary(m)

#and prepare the data for plotting with ggplot()
pred <- with(foram, data.frame(Age = -seq(min(Age_kaBP), max(Age_kaBP), length = 200)))
pred <- cbind(pred, as.data.frame(predict(m, pred, se.fit = TRUE, unconditional = TRUE)))
pred <- transform(pred,
                  Fitted = fit,
                  Upper = fit + (2 * se.fit),
                  Lower = fit - (2 * se.fit),
                  Age_kaBP = - Age)

#Finally we get to plot the Observed ä18O values with the fitted trend 
ggplot(foram, aes(y = d18O, x = Age_kaBP)) +
  geom_point() +
  geom_ribbon(data = pred, mapping = aes(x = Age_kaBP, ymin = Lower, ymax = Upper),
              fill = "grey", colour = NA, alpha = 0.7, inherit.aes  = FALSE) +
  geom_path(data = pred, mapping = aes(x = Age_kaBP, y = Fitted), inherit.aes = FALSE,
            size = 1) +
  scale_x_reverse(sec.axis = sec_axis( ~ 1950 - (. * 1000), name = "Age [AD]")) +
  scale_y_reverse() +
  labs(y = ylabel, x = xlabel)
