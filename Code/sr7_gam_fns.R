## -----------------------------------------------------------------------------
##
## Project: Summer Research Project
##
## Script purpose: Functions for  GAM and heatmap base 
##
## Date: 04/06/2024
## Author: BOD
##
## -----------------------------------------------------------------------------


## Code to create a heatmap base 
ngrid <- 1500
# create a grid of long and lat values 
#grid has size ngrid x ngrid
grid_long <-rep(seq(min(hd_all$Longitude_Original)-.05,max(hd_all$Longitude_Original)+.05, length.out=ngrid),each=ngrid)
grid_lat <-rep(seq(min(hd_all$Latitude_Original)-.01,max(hd_all$Latitude_Original)+.01, length.out=ngrid),ngrid)

# df of new long and lat 
newd <- data.frame(Longitude= grid_long, Latitude = grid_lat)

# project coords to km setting to line up with the data form the GAM 
coords.new <- newd[,c("Longitude", "Latitude")]
sp::coordinates(coords.new) <- ~ Longitude+Latitude
sp::proj4string(coords.new) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
coords.new.km <- as.matrix(as.data.frame(sp::spTransform(coords.new, sp::CRS(utm))))/1000 #convert to km and make 0,0 origin




## plot_gam_resid
#' Create map of residuals for GAM
#' 
#' @param model the mgcv::gam 
#' @param data the dataset
#' @param map base map
#' @param title string title of map
#' 
#' @return map of reisiduals 
#' 
plot_gam_resid <- function(model, data, map = map_roi_counties, title = 'map'){
  
  # data frame of the coordinates and residuals 
  resid.df <- as.data.frame(cbind(Longitude = data$Longitude_Original, 
                                  Latitude = data$Latitude_Original, 
                                  residuals = model$residuals))
  # create shapefile 
  resid.df_sf <- st_as_sf(resid.df, coords = c("Longitude", "Latitude"))
  st_crs(resid.df_sf) <- st_crs(st_crs(roi_counties_sf))
  
  map_resid <-  map +
    geom_sf(data = resid.df_sf, aes(color = residuals), size = 1) +
    scale_color_gradient(name = title, low = "red", high = "green") + 
    theme_map()
  return(map_resid)
}


## create_heatmap
#' Create heatmaps for GAM
#'
#' @param model the mgcv::gam
#' @param data the dataset
#' @param ngrid number of grid poitns
#'
#' @return list of maps, one with obseraions and one wihtout.
#'
create_heatmap <- function(model, pred_cord, newd = newd, in_data = hd_all, in_ngrid = 500){
  pred_cord <- Pred.coord
  model <-  gam.sub.MRF
  # make prediction
  pred <- predict.gam(model, newdata = pred_cord, se.fit = T)
  price.m2 <- exp(pred$fit)
  lnprice.m2 <- pred$fit

  # align coordinates and price and create a raster
  pred.xyz <- cbind(newd, lnprice.m2)
  pred.r <- rasterFromXYZ(pred.xyz)
  proj4string(pred.r) <- utm
  crs(pred.r) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  #now clip to Ireland
  clip.shp <- roi_counties_sp #dublin_sp
  proj4string(clip.shp)
  pred.r.clipped <- mask(pred.r, clip.shp)
  crs(pred.r.clipped) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Made the map values and pallette
  val = range(as.numeric(pred.r.clipped@data@values), na.rm = T)
  pal = colorNumeric(palette = c("blue","#acdf87", "yellow", "orange", "#FF0800", "red"), domain=val)

  # create a heatmap with observation points
  heatmap_obs <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo")   %>%
    addRasterImage(pred.r.clipped, colors = pal, opacity = 0.5)%>%
    addCircleMarkers(~Longitude_Original, ~Latitude_Original, data=in_data,
                     radius=1, stroke = FALSE,color="black",  fillOpacity=.5) %>%
    addLegend(pal = pal, values = val, title = "Location Value", labFormat = labelFormat(prefix = "€"), position = "topleft")

  # create a heatmap with no observation points
  heatmap_no_obs <- leaflet() %>%
    addProviderTiles("CartoDB.Positron")   %>%
    addRasterImage(pred.r.clipped, colors = pal, opacity = 0.5)%>%
    addLegend(pal = pal, values = val, title = "Location Value", labFormat = labelFormat(prefix = "€"), position = "topleft")

  gp_headmap_no_obs <- leaflet() %>%
    addProviderTiles("CartoDB.Positron")   %>%
    addRasterImage(pred.r.clipped, colors = pal, opacity = 0.5)%>%
    addLegend(pal = pal, values = val, title = "Location Scaling", position = "topleft", bins = c(5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5))
  gp_headmap_no_obs
  return(list(heatmap_obs, heatmap_no_obs))
}








#' ## create_heatmap
#' #' Create heatmaps for GAM
#' #' 
#' #' @param model the mgcv::gam 
#' #' @param data the dataset
#' #' @param ngrid number of grid poitns 
#' #' 
#' #' @return list of maps, one with obseraions and one wihtout. 
#' #'
#' create_heatmap <- function(model, in_data, in_ngrid = 500){
#'   ngrid <- in_ngrid
#'   # create a grid of long and lat values 
#'   #grid has size ngrid x ngrid
#'   grid_long <-rep(seq(min(in_data$Longitude_Original)-.05,max(in_data$Longitude_Original)+.05, length.out=ngrid),each=ngrid)
#'   grid_lat <-rep(seq(min(in_data$Latitude_Original)-.01,max(in_data$Latitude_Original)+.01, length.out=ngrid),ngrid)
#'   
#'   # df of new long and lat 
#'   newd <- data.frame(Longitude= grid_long, Latitude = grid_lat)
#'   
#'   # project coords to km setting to line up with the data form the GAM 
#'   coords.new <- newd[,c("Longitude", "Latitude")]
#'   sp::coordinates(coords.new) <- ~ Longitude+Latitude
#'   sp::proj4string(coords.new) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#'   coords.new.km <- as.matrix(as.data.frame(sp::spTransform(coords.new, sp::CRS(utm))))/1000 #convert to km and make 0,0 origin
#'   
#'   # Create predicition data - new coordinates, size, 0 bed and 0 bath 
#'   Pred.coord <- data.frame(Longitude= coords.new.km[,1] ,Latitude = coords.new.km[,2],
#'                            Size= rep(in_data$Size[1],ngrid*ngrid),
#'                            PropertyType.c = rep(in_data$PropertyType[1],ngrid*ngrid),
#'                            Beds=rep(0,ngrid*ngrid),
#'                            Baths=rep(0,ngrid*ngrid))
#'   
#'   # make prediction
#'   pred <- predict.gam(model, newdata = Pred.coord, se.fit = T)
#'   price.m2 <- exp(pred$fit)
#'   
#'   # align coordinates and price and create a raster 
#'   pred.xyz <- cbind(newd, price.m2)
#'   pred.r <- rasterFromXYZ(pred.xyz)
#'   proj4string(pred.r) <- utm
#'   crs(pred.r) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#'   
#'   #now clip to Ireland
#'   clip.shp <- roi_counties_sp
#'   proj4string(clip.shp)
#'   pred.r.clipped <- mask(pred.r, clip.shp)
#'   crs(pred.r.clipped) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#'   
#'   # Made the map values and pallette 
#'   val = range(as.numeric(pred.r.clipped@data@values), na.rm = T) 
#'   pal = colorNumeric(palette = c("blue","#acdf87", "yellow", "orange", "#FF0800", "red"), domain=val)
#'   
#'   # create a heatmap with observation points 
#'   heatmap_obs <- leaflet() %>%
#'     addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo")   %>%
#'     addRasterImage(pred.r.clipped, colors = pal, opacity = 0.5)%>%
#'     addCircleMarkers(~Longitude_Original, ~Latitude_Original, data=in_data,
#'                      radius=1, stroke = FALSE,color="black",  fillOpacity=.5) %>%
#'     addLegend(pal = pal, values = val, title = "Location Value", labFormat = labelFormat(prefix = "€"), position = "topleft")
#'   
#'   # create a heatmap with no observation points 
#'   heatmap_no_obs <- leaflet() %>%
#'     addProviderTiles("CartoDB.Positron")   %>%
#'     addRasterImage(pred.r.clipped, colors = pal, opacity = 0.5)%>%
#'     addLegend(pal = pal, values = val, title = "Location Value", labFormat = labelFormat(prefix = "€"), position = "topleft")
#'   
#'   return(list(heatmap_obs, heatmap_no_obs))
#' }
#' 




## create_heatmap
#' Create heatmaps for GAM
#' 
#' @param model the mgcv::gam 
#' @param data the dataset
#' @param ngrid number of grid poitns 
#' 
#' @return list of maps, one with obseraions and one wihtout. 
#'
create_var_heatmap <- function(model, in_data, in_ngrid = 500){
  
  ngrid <- in_ngrid
  # create a grid of long and lat values 
  #grid has size ngrid x ngrid
  grid_long <-rep(seq(min(in_data$Longitude_Original)-.05,max(in_data$Longitude_Original)+.05, length.out=ngrid),each=ngrid)
  grid_lat <-rep(seq(min(in_data$Latitude_Original)-.01,max(in_data$Latitude_Original)+.01, length.out=ngrid),ngrid)
  
  # df of new long and lat 
  newd <- data.frame(Longitude= grid_long ,Latitude = grid_lat )
  
  # project coords to km setting to line up with the data  form the GAM 
  coords.new <- newd[,c("Longitude", "Latitude")]
  sp::coordinates(coords.new) <- ~ Longitude+Latitude
  sp::proj4string(coords.new) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  coords.new.km <- as.matrix(as.data.frame(sp::spTransform(coords.new, sp::CRS(utm))))/1000 #convert to km and make 0,0 origin
  
  # Create predicition data - new coordinates, size, 0 bed and 0 bath 
  Pred.coord <- data.frame(Longitude= coords.new.km[,1] ,Latitude = coords.new.km[,2],
                           Size= rep(in_data$Size[1],ngrid*ngrid),
                           PropertyType.c = rep(in_data$PropertyType[1],ngrid*ngrid),
                           Beds=rep(0,ngrid*ngrid),
                           Baths=rep(0,ngrid*ngrid))
  # make prediction
  pred <- predict.gam(gam.irl2, newdata = Pred.coord, se.fit = T)
  
  ## Varince Map 
  var <- exp(pred$se.fit)
  var.xyz <- cbind(newd, var)
  var.r <- rasterFromXYZ(var.xyz)
  proj4string(var.r) <- utm
  crs(var.r) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #now clip to Ireland
  clip.shp <- roi_counties_sp
  proj4string(clip.shp)
  var.r.clipped <- mask(var.r, clip.shp)
  crs(var.r.clipped) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  # Made the map 
  val = range(as.numeric(var.r.clipped@data@values), na.rm = T) 
  pal = colorNumeric(palette = c("blue","#acdf87", "yellow", "orange", "#FF0800", "red"), domain=val)
  
  var_map <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo")   %>%
    addRasterImage(var.r.clipped, colors = pal, opacity = 0.5)%>%
    addCircleMarkers(~Longitude_Original, ~Latitude_Original, data=hd_rural_all,
                     radius=1, stroke = FALSE,color="black",  fillOpacity=.5) %>%
    addLegend(pal = pal, values = val, title = "Location Variance Value", labFormat = labelFormat(prefix = "€"), position = "topleft")
  
  return(var_map)
}




## gam_model_metrics 
#' Calculate the model metrics for GAM
#' 
#' @param model model of mgcv GAM
#' @param testing.df the test data
#' 
#' @return dataframe of model metrics 
#' 
gam_model_metrics <- function(model, testing.df){

    # true price and size 
    size <- testing.df$Size
    act_price <- testing.df$Price
  
    ## Create Prediction Intervals 
    
      # if this is not a random forest - calculate the prediction intervals
      beta <- coef(model)
      V.mod <- vcov(model)
      
      # simulate replicate beta vectors from posterior...
      Cv <- chol(V.mod)
      n.rep <- 10000
      nb <- length(beta)
      br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta
  
      ## turn these into replicate linear predictors...
      Xp <- predict(model, newdata = testing.df, type="lpmatrix")
      
      lp <- Xp%*%br
      fv <- exp(lp) ## ... finally, replicate expected value vectors
      size1 <- t(size)
      size1 <- size1[rep(seq_len(nrow(size1)), each = n.rep), ]
      fv.s <- as.data.frame(fv)*size1
    
      # Calculate the prediction Intervals 
      pi.50 <- apply(fv.s, 1, quantile, prob=c(0.25,0.75))
      pi.80 <- apply(fv.s, 1, quantile, prob=c(0.1,0.9))
      pi.90 <- apply(fv.s, 1, quantile, prob=c(0.05,0.95))
      pi.95 <- apply(fv.s, 1, quantile, prob=c(0.025,0.975))
      pi.99 <- apply(fv.s, 1, quantile, prob=c(0.005,0.995))
      
      # Calculate the proportion of values in each PI 
      in50.pi <- 100*(mean(pi.50[1,] < act_price & pi.50[2,] > act_price))
      in80.pi <- 100*(mean(pi.80[1,] < act_price & pi.80[2,] > act_price))
      in90.pi <- 100*(mean(pi.90[1,] < act_price & pi.90[2,] > act_price))
      in95.pi <- 100*(mean(pi.95[1,] < act_price & pi.95[2,] > act_price))
      in99.pi <- 100*(mean(pi.99[1,] < act_price & pi.99[2,] > act_price))

    
  # predictions from the model 
  predictions.df <- predict(model, newdata = testing.df)
  
  # get price values 
  pred_price <- exp( predictions.df )*size

  # calculate standard model metrics 
  rsq <- cor(pred_price, act_price)^2
  rmse <- Metrics::rmse(act_price, pred_price)
  mape <- Metrics::mape(act_price, pred_price)
  in5 <- sum(abs(pred_price - act_price) / act_price <= 0.01) / length(act_price)*100
  in10 <- sum(abs(pred_price - act_price) / act_price <= 0.1) / length(act_price)*100
  in20 <- sum(abs(pred_price - act_price) / act_price <= 0.2) / length(act_price)*100
  
  model_metrics <- cbind.data.frame("rsq" = rsq,
                                    "rmse" = rmse,
                                    "mape" = mape,
                                    "within5" = in5, 
                                    "within10" = in10, 
                                    "within20" = in20,
                                    "pi50" = in50.pi, 
                                    "pi80" = in80.pi, 
                                    "pi90" = in90.pi, 
                                    "pi95" = in95.pi,
                                    "pi99" = in99.pi)
  
  model_metrics <- model_metrics
  
  return(model_metrics)
}




## gam_model_metrics_df
#' Calculate the model metrics for GAM
#' 
#' @param model model of mgcv GAM
#' @param testing.df the test data
#' 
#' @return dataframe of model metrics 
#' 
gam_model_metrics_df <- function(model, testing.df){
  
  # true price and size 
  size <- testing.df$Size
  act_price <- testing.df$Price
  
  ## Create Prediction Intervals 
  
  # if this is not a random forest - calculate the prediction intervals
  beta <- coef(model)
  V.mod <- vcov(model)
  
  # simulate replicate beta vectors from posterior...
  Cv <- chol(V.mod)
  n.rep <- 10000
  nb <- length(beta)
  br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta
  
  ## turn these into replicate linear predictors...
  Xp <- predict(model, newdata = testing.df, type="lpmatrix")
  
  lp <- Xp%*%br
  fv <- exp(lp) ## ... finally, replicate expected value vectors
  size1 <- t(size)
  size1 <- size1[rep(seq_len(nrow(size1)), each = n.rep), ]
  fv.s <- as.data.frame(fv)*size1
  
  # Calculate the prediction Intervals 
  pi.50 <- apply(fv.s, 1, quantile, prob=c(0.25,0.75))
  pi.80 <- apply(fv.s, 1, quantile, prob=c(0.1,0.9))
  pi.90 <- apply(fv.s, 1, quantile, prob=c(0.05,0.95))
  pi.95 <- apply(fv.s, 1, quantile, prob=c(0.025,0.975))
  pi.99 <- apply(fv.s, 1, quantile, prob=c(0.005,0.995))
  
  pi.50.l <- pi.50[1,]
  pi.50.u <- pi.50[2,]
  pi.80.l <- pi.80[1,]
  pi.80.u <- pi.80[2,]
  pi.90.l <- pi.90[1,] 
  pi.90.u <- pi.90[2,] 
  pi.95.l <- pi.95[1,]
  pi.95.u <- pi.95[2,]
  pi.99.l <- pi.99[1,]
  pi.99.u <- pi.99[2,]
  

  # predictions from the model 
  predictions.df <- predict(model, newdata = testing.df)
  
  # get price values 
  pred_price <- exp( predictions.df )*size
  
  df <- data.frame(Predicted = pred_price, 
                   pi.50.l,
                   pi.50.u,
                   pi.80.l,
                   pi.80.u,
                   pi.90.l,
                   pi.90.u,
                   pi.95.l,
                   pi.95.u,
                   pi.99.l,
                   pi.99.u)
  
  return(df)
}






## model_metrics_subareas 
#' Calculate the model metrics within each subarea 
#' 
#' @param model any model 
#' @param testing.df the test data
#' 
#' @return list, each entry is a df of metrics per subarea 
model_metrics_subareas <- function(model, testing.df, model_metrics){
  
  # the number of areas 
  unique_areas <- unique(hd_all$SubArea)
  # initialise list 
  area_metrics <- list()
  
  for (area in unique_areas){
    area_data <- testing.df[testing.df$SubArea == area,]
    area_metrics[[area]] <- model_metrics(model, area_data)
  }
  return(area_metrics)
}


## rf_model_metrics_df
#' Calculate model metrics of random forest model return df 
#' 
#' @param rf_model Random Forest model type quantrandForest
#' @param testing.df test data
#' 
#' @return dataframe of model metrics 
#' 
rf_model_metrics_df <- function(rf_model, testing.df){
  
  # create predictions 
  rf_pred <- predict(rf_model, testing.df, what = 0.5)
  rf_quantiles <- predict(rf_model, testing.df, what = c(0.25, 0.75, 0.1 ,0.9, 0.05, 0.95, 0.025, 0.975, 0.005, 0.995))
  act_price <- testing.df$Price
  
  # Calculate metrics 
  pi.50.l <- rf_quantiles[,1]
  pi.50.u <- rf_quantiles[,2]
  pi.80.l <- rf_quantiles[,3]
  pi.80.u <- rf_quantiles[,4]
  pi.90.l <- rf_quantiles[,5]
  pi.90.u <- rf_quantiles[,6]
  pi.95.l <- rf_quantiles[,7]
  pi.95.u <- rf_quantiles[,8]
  pi.99.l <- rf_quantiles[,9]
  pi.99.u <- rf_quantiles[,10]

  df <- data.frame(Predicted = rf_pred, 
                   pi.50.l,
                   pi.50.u,
                   pi.80.l,
                   pi.80.u,
                   pi.90.l,
                   pi.90.u,
                   pi.95.l,
                   pi.95.u,
                   pi.99.l,
                   pi.99.u)
  
  return(df)
}


## rf_model_metrics 
#' Calculate model metrics of random forest model 
#' 
#' @param rf_model Random Forest model type quantrandForest
#' @param testing.df test data
#' 
#' @return dataframe of model metrics 
#' 
rf_model_metrics <- function(rf_model, testing.df){
  
  
  # create predictions 
  rf_pred <- predict(rf_model, testing.df, what = 0.5)
  rf_quantiles <- predict(rf_model, testing.df, what = c(0.25, 0.75, 0.1 ,0.9, 0.05, 0.95, 0.025, 0.975, 0.005, 0.995))
  act_price <- testing.df$Price
  
  # # Calculate metrics 
  # rsq <- cor(rf_pred, act_price)^2
  # rmse <- Metrics::rmse(act_price, rf_pred)
  # mape <- Metrics::mape(act_price, rf_pred)
  # in5 <- sum(abs(rf_pred - act_price) / act_price <= 0.01) / length(act_price)*100
  # in10 <- sum(abs(rf_pred - act_price) / act_price <= 0.1) / length(act_price)*100
  # in20 <- sum(abs(rf_pred - act_price) / act_price <= 0.2) / length(act_price)*100
  # in50.pi <- 100*(mean(rf_quantiles[,1] < act_price & rf_quantiles[,2] > act_price))
  # in80.pi <- 100*(mean(rf_quantiles[,3] < act_price & rf_quantiles[,4] > act_price))
  # in90.pi <- 100*(mean(rf_quantiles[,5] < act_price & rf_quantiles[,6] > act_price))
  # in95.pi <- 100*(mean(rf_quantiles[,7] < act_price & rf_quantiles[,8] > act_price))
  # in99.pi <- 100*(mean(rf_quantiles[,9] < act_price & rf_quantiles[,10] > act_price))
  # 
  # model_metrics <- data.frame("RSq" = rsq,
  #                                   "RMSE" = rmse,
  #                                   "MAPE" = mape,
  #                                   "Within-5" = in5, 
  #                                   "Within-10" = in10, 
  #                                   "Within-20" = in20,
  #                                   "PI-50" = in50.pi, 
  #                                   "PI-80" = in80.pi, 
  #                                   "PI-90" = in90.pi, 
  #                                   "PI-95" = in95.pi,
  #                                   "PI-99" = in99.pi)
  # return(model_metrics)
  
  
  
  # Calculate metrics 
  pi.50.l <- rf_quantiles[,1]
  pi.50.u <- rf_quantiles[,2]
  pi.80.l <- rf_quantiles[,3]
  pi.80.u <- rf_quantiles[,4]
  pi.90.l <- rf_quantiles[,5]
  pi.90.u <- rf_quantiles[,6]
  pi.95.l <- rf_quantiles[,7]
  pi.95.u <- rf_quantiles[,8]
  pi.99.l <- rf_quantiles[,9]
  pi.99.u <- rf_quantiles[,10]
  
  df <- data.frame(Predicted = rf_pred, 
                   pi.50.l,
                   pi.50.u,
                   pi.80.l,
                   pi.80.u,
                   pi.90.l,
                   pi.90.u,
                   pi.95.l,
                   pi.95.u,
                   pi.99.l,
                   pi.99.u)
}



## lm_model_metrics 
#' Calculate model metrics of linear regression model 
#' 
#' @param lm_model Linear regression model type lm
#' @param testing.df test data
#' 
#' @return dataframe of model metrics 
#' 
lm_model_metrics <- function(lm_model, testing.df){
  
  # extract the true values 
  act_price <- testing.df$Price
  size <- testing.df$Size
  
  # precited values 
  pred_price <- exp(predict(lm_model, testing.df))*size
  
  # calcualte the prediction intervals of price 
  pi.50 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.50)[,2:3])*size
  pi.80 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.80)[,2:3])*size  
  pi.90 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.90)[,2:3])*size
  pi.95 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.95)[,2:3])*size
  pi.99 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.99)[,2:3])*size  
  
  # Calculate the proportions within prediction interval 
  in50.pi <- 100*(mean(pi.50[,1] < act_price & pi.50[,2] > act_price))
  in80.pi <- 100*(mean(pi.80[,1] < act_price & pi.80[,2] > act_price))
  in90.pi <- 100*(mean(pi.90[,1] < act_price & pi.90[,2] > act_price))
  in95.pi <- 100*(mean(pi.95[,1] < act_price & pi.95[,2] > act_price))
  in99.pi <- 100*(mean(pi.99[,1] < act_price & pi.99[,2] > act_price))
  
  # calculate standard model metrics 
  rsq <- cor(pred_price, act_price)^2
  rmse <- Metrics::rmse(act_price, pred_price)
  mape <- Metrics::mape(act_price, pred_price)
  in5 <- sum(abs(pred_price - act_price) / act_price <= 0.01) / length(act_price)*100
  in10 <- sum(abs(pred_price - act_price) / act_price <= 0.1) / length(act_price)*100
  in20 <- sum(abs(pred_price - act_price) / act_price <= 0.2) / length(act_price)*100
  
  model_metrics <- cbind.data.frame("RSq" = rsq,
                                    "RMSE" = rmse,
                                    "MAPE" = mape,
                                    "Within-5" = in5, 
                                    "Within-10" = in10, 
                                    "Within-20" = in20,
                                    "PI-50" = in50.pi, 
                                    "PI-80" = in80.pi, 
                                    "PI-90" = in90.pi, 
                                    "PI-95" = in95.pi,
                                    "PI-99" = in99.pi)
  
  model_metrics <- model_metrics
  return(model_metrics)
}







## lm_model_metrics_df
#' Calculate model metrics of linear regression model 
#' 
#' @param lm_model Linear regression model type lm
#' @param testing.df test data
#' 
#' @return dataframe of model metrics 
#' 
lm_model_metrics_df <- function(lm_model, testing.df){

  size <- testing.df$Size
  
  # precited values 
  pred_price <- exp(predict(lm_model, testing.df))*size
  
  # calcualte the prediction intervals of price 
  pi.50 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.50)[,2:3])*size
  pi.80 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.80)[,2:3])*size  
  pi.90 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.90)[,2:3])*size
  pi.95 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.95)[,2:3])*size
  pi.99 <- exp(predict(lm_model, testing.df, interval = 'predict', level = 0.99)[,2:3])*size  
  

  pi.50.l <- pi.50[,1] 
  pi.50.u <- pi.50[,2]
  pi.80.l <- pi.80[,1] 
  pi.80.u <- pi.80[,2] 
  pi.90.l <- pi.90[,1]
  pi.90.u <- pi.90[,2] 
  pi.95.l <- pi.95[,1]
  pi.95.u <- pi.95[,2]
  pi.99.l <- pi.99[,1]
  pi.99.u <- pi.99[,2] 
  
  
  df <- data.frame(Predicted = pred_price, 
                   pi.50.l,
                   pi.50.u,
                   pi.80.l,
                   pi.80.u,
                   pi.90.l,
                   pi.90.u,
                   pi.95.l,
                   pi.95.u,
                   pi.99.l,
                   pi.99.u)
  
  return(df)
}







## analyse_cv_results 
#' Calculate model metrics from cv_df
#' 
#' @param cv_df dataframe of results from cross validation 
#' 
#' @return dataframe of model metrics 
#' 
analyse_cv_results <- function(cv_df){
  
    # extract the true values and pred_price
    act_price <- cv_df$Actual
    size <- cv_df$Size
    pred_price <- cv_df$Predicted
    
    # Calculate the proportions within prediction interval 
    in50.pi <- 100*(mean(cv_df$pi.50.l < act_price & cv_df$pi.50.u > act_price))
    in90.pi <- 100*(mean(cv_df$pi.90.l < act_price & cv_df$pi.90.u > act_price))
    in95.pi <- 100*(mean(cv_df$pi.95.l < act_price & cv_df$pi.95.u > act_price))
    in99.pi <- 100*(mean(cv_df$pi.99.l < act_price & cv_df$pi.99.u > act_price))
    in80.pi <- 100*(mean(cv_df$pi.80.l < act_price & cv_df$pi.80.u > act_price))
    
    # calculate standard model metrics 
    rsq <- cor(pred_price, act_price)^2
    rmse <- Metrics::rmse(act_price, pred_price)
    mape <- Metrics::mape(act_price, pred_price)
    in5 <- sum(abs(pred_price - act_price) / act_price <= 0.01) / length(act_price)*100
    in10 <- sum(abs(pred_price - act_price) / act_price <= 0.1) / length(act_price)*100
    in20 <- sum(abs(pred_price - act_price) / act_price <= 0.2) / length(act_price)*100
    
    model_metrics <- cbind.data.frame("RSq" = rsq,
                                      "RMSE" = rmse,
                                      "MAPE" = mape,
                                      "Within-5" = in5, 
                                      "Within-10" = in10, 
                                      "Within-20" = in20,
                                      "PI-50" = in50.pi, 
                                      "PI-80" = in80.pi, 
                                      "PI-90" = in90.pi, 
                                      "PI-95" = in95.pi,
                                      "PI-99" = in99.pi)
    return(model_metrics)
}





## analyse_cv_results_areas
#' Calculate model metrics from cv_df
#' 
#' @param cv_df dataframe of results from cross validation 
#' 
#' @return dataframe of model metrics in areas 
#' 
analyse_cv_results_areas <- function(cv_df){
  
  unique_areas <- unique(cv_df$SubArea)
  area_metrics <- c()
  
  for (area in unique_areas){
    area_data <- cv_df[cv_df$SubArea == area,]
    area_metrics <- rbind(area_metrics, c(as.character(area), analyse_cv_results(area_data)))
  }
  return(area_metrics)
}



## analyse_cv_results_strata
#' Calculate model metrics from cv_df
#' 
#' @param cv_df dataframe of results from cross validation 
#' 
#' @return dataframe of model metrics in stratas
#' 
analyse_cv_results_strata <- function(cv_df){
  
  cv_df$PriceStrata <- factor(cv_df$PriceStrata, ordered = T, levels = 
                                 c('0-100','100-200','200-300','300-400','400-500',
                                   '500-600','600-700','700-800','800-900','900-1000',
                                   '1000-1500','1500-2000','2000-3000', '3000+'))
  
  unique_strata <- levels(cv_df$PriceStrata)
  strata_metrics <- c()
  
  for (strata in unique_strata){
    strata_data <- cv_df[cv_df$PriceStrata == strata,]
    strata_metrics <- rbind(strata_metrics, c(as.character(strata), analyse_cv_results(strata_data)))
  }
  return(strata_metrics)
}


  



