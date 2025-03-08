eircodes_sf
dim(eircodes_sf)

ecodessf <- sf::read_sf(here("Data/Map Data/eircodes"))

sf_use_s2(FALSE)
plot(eircodes_sf)
adj_list <- st_touches(eircodes_sf, eircodes_sf)
names(adj_list) <- eircodes_sf[[1]]
hd_all$Eircode <- as.factor(hd_all$Eircode)
droplevels(hd_all$Eircode)

aapcode <- names(table(hd_all$Eircode))
levels(hd_all$Eircode) <- names(adj_list)

df <- data.frame(Eircode = rep(eircodes_sf$Eircode, 10))


df$Price <- rnorm(n = dim(df)[1], mean = 3000, sd = 500)
summary(df$Price)

model <- gam(Price ~ s(Eircode, bs = 'mrf', k = 20, xt = list(nb = adj_list)), data = df)

