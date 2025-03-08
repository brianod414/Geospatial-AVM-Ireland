
set.seed(2)



n <- 9 
B <- c(1, 2, -3) + 2
# oldX <- cbind(1, runif(n, 0, 10), runif(n, 0, 10))


X <- matrix(0, nrow = n, ncol = 3)
X[1:(n/3),1] <- 1
X[(n/3) + 1:(n/3),2] <- 1
X[2*(n/3) + 1:(n/3),3] <- 1

X <- X[, 1:2]

X%*%B 

Y <- rnorm(n, X%*%B, 1)

lmY <- lm(Y ~ X)
summary(lmY)


# intercept should be 2
# Beta1 = 1, Beta2 = 2, Beta3 = -3



# ------------------    ------------------    ------------------    ------------
library(emmeans)
library(MASS)

data("PlantGrowth")

## -- Dummy Coding -- ##

PlantGrowth$group <- as.factor(PlantGrowth$group)
contrasts(PlantGrowth$group) 
# control group is the reference level 

lm <- lm(weight ~ group, data = PlantGrowth)
model.matrix(lm)
summary(lm)

emmeans(lm, 'group')
# intercept is th e mean of control group 
# coeffs 1 and 2 are the differences in means between ceontrol group and groups 1 and 2 


## -- Contrast Coding -- ##
contrasts(PlantGrowth$group) <- contr.sum(3)
Cmat <- contrasts(PlantGrowth$group)
# var 1 is comparing control group to the grand mean 
# var 2 is comparing the trt1 to the grand mean 

lm <- lm(weight ~ group, data = PlantGrowth)
model.matrix(lm)
summary(lm)

emmeans(lm, 'group' )

(5.03+4.66+5.53)/3 # intercept is the grand mean 
(5.03+4.66+5.53)/3 - 0.0410 # coeff 2 is comparing the grand mean to control group 
(5.03+4.66+5.53)/3 - 0.4120 # coeff 2 is comparing the grand mean to trt1

# to compare tr2 to the control group - get -B1-B2
B3 <- 0.0410 + 0.4120
(5.03+4.66+5.53)/3 + B3 # B3 compares the grand mena to the mean of trt2 


# coeff after contrasts
coef_ac <- coef(lm)[2:3]
coef_bc <- (Cmat %*% coef_ac)[, 1]


# No intercept 
lm <- lm(weight ~ group-1, data = PlantGrowth)
model.matrix(lm)
summary(lm)
# the mean of each group are the coeff 

## -- Experiment with multiple factprs -- ##
Cmat <- matrix(c(1/2, 1/2, 0, 0, 1/2, -1/2, 0, 0, 0, 0, 1/2, 1/2, 0, 0, 1/2, -1/2), nrow = 4)
Cmat

ginv(Cmat)


## -- Add a second factor to PlantGrowth -- ##

# even are B, odd are A 
index <- seq(1, 30)
even_inx <- index[index%% 2 == 0]
PlantGrowth$factor <- "A"
PlantGrowth$factor[even_inx] <- "B"
table(PlantGrowth$factor, PlantGrowth$group)

PlantGrowth$factor <- as.factor(PlantGrowth$factor)
contrasts(PlantGrowth$factor ) <- contr.sum(2)


# create an interaction term 
PlantGrowth$factorgroup <- interaction(PlantGrowth$group, PlantGrowth$factor)

attach(PlantGrowth)
factor <- as.factor(factor)
group <- as.factor(group)
PlantGrowth$factorgroup <- as.factor(PlantGrowth$factorgroup)

contrasts(PlantGrowth$factorgroup) <- contr.treatment(6)

lm2 <- lm(weight ~ factorgroup, dat = PlantGrowth)
summary(lm2)
# intercept is the mean of Acontrl, 
# B1 compares Bctrl to Acontrol, etc. 

emmeans(lm2, 'factorgroup')

PlantGrowth %>% group_by(group) %>% summarise(mean = mean(weight))
PlantGrowth %>% group_by(factor) %>% summarise(mean = mean(weight))
PlantGrowth %>% group_by(factorgroup) %>% summarise(mean = mean(weight)) %>% summarise(gmean = mean(mean))

contrasts(PlantGrowth$group)

contr.sum(3)

## Sum coding for interaction 
Cmat <- matrix(c(1, 1, 0, 0, 0, 0,
               1, 0, 1, 0, 0, 0,
               1, -1, -1,0,  0, 0,
               0, 0,0, 1, 1,  0,
               0, 0,0, 1, 0, 1,
                0, 0, 0, 1, -1, -1), nrow = 6, byrow = T)
fractions(Cmat)
Cmat2 <- ginv(Cmat)
fractions(Cmat2)

contrasts(PlantGrowth$factorgroup, how.many = 6) <- Cmat
contrasts(PlantGrowth$factorgroup)

lm3 <- lm(weight ~ factorgroup-1, data = PlantGrowth)
model.matrix(lm3)
lm3          
emmeans(lm3, 'factorgroup')
# no intercept, the mean of each group are the coefficients 


contrasts(PlantGrowth$factor)

lm <- lm(weight ~ factor + group + factor*group, data = PlantGrowth)
summary(lm)
emmeans(lm, 'group')
emmeans(lm, c('factor', 'group'))
ref_grid(lm)

contrast(eg, by = 'factor')



PlantGrowth$value <- runif(30)

### try with GAM

gammy <- gam(weight ~ factor*group, data = PlantGrowth)
plot(gammy)
summary(gammy)


emmeans(gammy, c('factor', 'group'))
grid <- ref_grid(gammy)

contrast(grid,interaction = T)
contrast(grid, by = 'factor')





# emmeans works well but what do these compare to the interpretations of the linear model? 

data('PlantGrowth')
dat <- PlantGrowth

# adding the new factor 
index <- seq(1, 30)
even_inx <- index[index%% 2 == 0]
dat$factor <- "A"
dat$factor[even_inx] <- "B"
table(dat$factor, dat$group)

dat$factor <- as.factor(dat$factor)
dat$group <- as.factor(dat$group)


library(mgcv)

# checking with 1 varbiable 
dat %>% group_by(group) %>% summarise(mean = mean(weight))
dat %>% group_by(group) %>% summarise(mean = mean(weight)) %>% summarise(gmean = mean(mean))

contrasts(dat$group) <- contr.sum(3)
gam1 <- gam(weight ~ group, data = dat)
summary(gam1)

grid <- emmeans(gam1, 'group')
coeff <- summary(contrast(grid, method= 'eff'))
# the 3 coeff are B1, B2 and -B1-B2. The relative scalings of the levels 



# checking with 2 variables 
dat$interaction <- interaction(dat$factor, dat$group)
dat %>% group_by(factor) %>% summarise(mean = mean(weight))
dat %>% group_by(factor, group) %>% summarise(mean = mean(weight))
dat %>% group_by(interaction) %>% summarise(mean = mean(weight)) %>% summarise(mean = mean(mean))

gam2 <- gam(weight ~ group*factor, data = dat)
summary(gam2)

contrasts(dat$factor) <- contr.treatment(2)
contrasts(dat$group) <- contr.treatment(3)

grid <- emmeans(gam2, c('group', 'factor'), by = 'factor')
coeff <- summary(contrast(grid, method= 'eff'))

dat %>% group_by(factor) %>% summarise(mean = mean(weight))
dat %>% filter(factor == 'A') %>% group_by(group) %>% summarise(mean = mean(weight) - 5.18) # A factor comparison to grand mean of A 
dat %>% filter(factor == 'B') %>% group_by(group) %>% summarise(mean = mean(weight) - 4.96) # B factor comparison to grand mean of B 






##### ------- Simulating Data ------------- 
library(ggplot2)
library(mgcv)
library(emmeans)
library(dplyr)
library(cowplot)
library(tidyr)

set.seed(2)

N <- 10000;


sub_areas <- c("Dublin", "Cork", "Galway", "Limerick", "Towns", "Rural")
sub_area_spl <- sample(x = sub_areas, size = N, replace = TRUE, prob = c(0.25, 0.15, 0.15, 0.1, 0.15, 0.2))

p_types <- c("Detached", "Semi-Detached", "Terraced", "End of Terrace", "Townhouse", "Apartment", "Duplex")
p_type_probs <- c(0.25, 0.35, 0.1, 0.05, 0.05, 0.15, 0.05)

property_samples <- list()
for (sub_area in unique(sub_area_spl)) {
  size <- sum(sub_area_spl == sub_area)
  property_samples[[sub_area]] <- sample(x = p_types, size = size, replace = TRUE, prob = p_type_probs)
}

# Optionally, convert the list to a data frame for easier viewing
data <- data.frame(
  SubArea = rep(names(property_samples), times = sapply(property_samples, length)),
  PropertyType = unlist(property_samples)
)
rownames(data) <- NULL
table(data$PropertyType, data$SubArea)

# create. a size variable 
p_types <- c("Detached", "Semi-Detached", "Terraced", "End of Terrace", "Townhouse", "Apartment", "Duplex")
p_types_mu_size <- c(157, 115, 96.7, 99.6, 90.2, 70, 99.8)
p_types_sd_size <- c(63, 30, 40, 36, 24, 22, 24)

property_sizes <- list()

# Loop through each property type and generate sizes using rnorm
for (i in seq_along(p_types)) {
  type <- p_types[i]
  mu <- p_types_mu_size[i]
  sd <- p_types_sd_size[i]
  size <- sum(data$PropertyType == type)
  property_sizes[[type]] <- rnorm(n = size, mean = mu, sd = sd)
}

property_sizes_vector <- unlist(property_sizes)

# Add the property sizes to the data frame
data$PropertySize <- property_sizes_vector



# Initialize lists to store means and standard deviations for each property type in each subarea
mean_prices <- list(
  Dublin = numeric(length(p_types)),
  Cork = numeric(length(p_types)),
  Galway = numeric(length(p_types)),
  Limerick = numeric(length(p_types)),
  Towns = numeric(length(p_types)),
  Rural = numeric(length(p_types))
)

sd_prices <- list(
  Dublin = numeric(length(p_types)),
  Cork = numeric(length(p_types)),
  Galway = numeric(length(p_types)),
  Limerick = numeric(length(p_types)),
  Towns = numeric(length(p_types)),
  Rural = numeric(length(p_types))
)

## p_types <- c("Detached", "Semi-Detached", "Terraced", "End of Terrace", "Townhouse", "Apartment", "Duplex")
mean_prices$Dublin <- c(700000, 500000, 250000, 275000, 220000, 300000, 350000)
sd_prices$Dublin <- c(350000, 200000, 280000, 250000, 100000, 150000, 100000)
mean_prices$Cork <- c(500000, 350000, 100000, 150000, 150000, 170000, 200000, 230000)
sd_prices$Cork <- c(200000, 50000, 50000, 100000, 50000, 75000, 50000, 30000)
mean_prices$Galway <- c(500000, 330000, 200000, 220000, 210000, 190000, 250000, 270000)
sd_prices$Galway <- c(250000, 100000, 50000, 120000, 100000, 20000, 100000, 30000)
mean_prices$Limerick <-  c(400000, 300000, 200000, 250000, 220000, 100000, 150000, 170000)
sd_prices$Limerick <- c(100000, 50000, 30000, 100000, 50000, 50000, 20000, 20000)
mean_prices$Towns <-  c(500000, 300000, 100000, 120000, 120000, 130000, 150000, 160000)
sd_prices$Towns <- c(150000, 130000, 20000, 10000, 10000, 20000, 40000, 50000)
mean_prices$Rural <-  c(400000, 200000, 70000, 70000, 80000, 100000, 100000, 100000)
sd_prices$Rural <- c(300000, 20000, 10000, 10000, 10000, 20000, 20000, 10000)

# Initialize a list to store property prices
property_prices <- list()

# Loop through each subarea and each property type to generate prices using rnorm
for (sub_area in unique(data$SubArea)) {
  for (i in seq_along(p_types)) {
    type <- p_types[i]
    mu <- mean_prices[[sub_area]][i]
    sd <- sd_prices[[sub_area]][i]
    size <- sum(data$SubArea == sub_area & data$PropertyType == type)
    property_prices[[paste(sub_area, type, sep = "_")]] <- rnorm(n = size, mean = mu, sd = sd)
  }
}

# Combine the property prices into a single vector
property_prices_vector <- unlist(property_prices)

# Add the property prices to the data frame
data$Price <- property_prices_vector

# Display the data frame
print(data)

data$Price[data$Price <0] <- data$Price[data$Price<0] *-1
data$PropertySize[data$PropertySize <0] <- data$PropertySize[data$PropertySize<0] *-1

g1 <- gam(log(Price) ~ PropertyType*SubArea + s(PropertySize), data = data)
summary(g1)


grid <- emmeans(g1, c('PropertyType', 'SubArea'), by = 'SubArea')
coeff <- summary(contrast(grid, method= 'eff'))

condition_coeff_df <- data.frame(area = coeff$SubArea, contrast = coeff$contrast, coeff = exp(coeff$estimate), lower = exp(coeff$estimate - 1.96*coeff$SE), upper = exp(coeff$estimate + 1.96*coeff$SE)) %>% arrange(coeff)
condition_coeff_df$contrast <- gsub(" effect", "", condition_coeff_df$contrast)

# for Dublin 
condition_coeff_mini <- condition_coeff_df[condition_coeff_df$area=='Dublin', ]
ggplot(condition_coeff_mini, aes(reorder(contrast, coeff), coeff)) + 
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper)) + 
  geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Changing linetype to dotted
  theme_cowplot() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) + # Adjusting y-axis ticks
  labs(title = "Relative Scalings of Property Condition", x = "Property Condition", y = "Multiplicative Scaling")
data %>% filter(SubArea == 'Dublin') %>% group_by(PropertyType) %>% summarise(mean = mean(log(Price)), sd = sd(log(Price)), n = n()) %>% arrange(mean)

# for Rural
condition_coeff_mini <- condition_coeff_df[condition_coeff_df$area=='Cork', ]
ggplot(condition_coeff_mini, aes(reorder(contrast, coeff), coeff)) + 
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper)) + 
  geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Changing linetype to dotted
  theme_cowplot() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) + # Adjusting y-axis ticks
  labs(title = "Relative Scalings of Property Condition", x = "Property Condition", y = "Multiplicative Scaling")
data %>% filter(SubArea == 'Galway') %>% group_by(PropertyType) %>% summarise(mean = mean(log(Price)), sd = sd(log(Price)), n = n()) %>% arrange(mean)

table(data$SubArea, data$PropertyType)

unique_areas <- unique(data$SubArea)
rel_scalings_plots <- list()
# Loop through each area to create and store the plots
for (area in unique_areas) {
  condition_coeff_mini <- condition_coeff_df[condition_coeff_df$area == area, ]
  
  plot <- ggplot(condition_coeff_mini, aes(reorder(contrast, coeff), coeff)) + 
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    geom_abline(intercept = 1, slope = 0, color = "grey", linetype = "dotted") + # Line representing the grand mean 
    theme_cowplot() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) + # Adjusting y-axis ticks
    labs(title = paste("Relative Scalings of Property Condition in", area), x = "Property Condition", y = "Multiplicative Scaling")
  
  rel_scalings_plots[[area]] <- plot
}

# Combine the plots into a grid
plot_grid(plotlist = rel_scalings_plots)


data %>% group_by(SubArea) %>% group_by(PropertyType) %>% summarise(mean = mean(log(Price)), sd = sd(log(Price)), n = n()) %>% arrange(mean)

# comparing the scalings to the mean values 
mean_values <- data %>%
  group_by(SubArea, PropertyType) %>%
  summarise(mean_value = mean(Price, na.rm = TRUE)) %>%
  ungroup()

# Reshape the data to have PropertyType as rows and SubArea as columns
mean_values_wide <- mean_values %>%
  pivot_wider(names_from = SubArea, values_from = mean_value)

# Print the reshaped table
print(mean_values_wide)




############# CODE DUMP 

length(unique(hd_all$PropertyType.SubArea.f))

# 6 unique sub areas, n_columns of submatrices 
length(unique(hd_all$SubArea.f))

# create contrasts for the interaction term 
ptype_contr <- contrasts(hd_all$PropertyType.f) # maybe need to cbind(1, ..) to this for intercept? 
null_matrix <- matrix(0, 7, 6) 

fractions(ginv(ptype_contr))

# we need 6 submatrices in each row (one per sub area) 
sub_mat1 <- matrix(c(ptype_contr, null_matrix, null_matrix, null_matrix, null_matrix, null_matrix), nrow = 7) 
sub_mat2 <- matrix(c(null_matrix, ptype_contr, null_matrix, null_matrix, null_matrix, null_matrix), nrow = 7) 
sub_mat3 <- matrix(c(null_matrix, null_matrix, ptype_contr, null_matrix, null_matrix, null_matrix), nrow = 7) 
sub_mat4 <- matrix(c(null_matrix, null_matrix, null_matrix, ptype_contr, null_matrix, null_matrix), nrow = 7) 
sub_mat5 <- matrix(c(null_matrix, null_matrix, null_matrix, null_matrix, ptype_contr, null_matrix), nrow = 7) 
sub_mat6 <- matrix(c(null_matrix, null_matrix, null_matrix, null_matrix, null_matrix, ptype_contr), nrow = 7) 
cont_matrix <- rbind(sub_mat1, sub_mat2, sub_mat3, sub_mat4, sub_mat5, sub_mat6)
View(cont_matrix)
rownames(cont_matrix) <- levels(hd_all$PropertyType.SubArea.f)
colnames(cont_matrix) <- levels(hd_all$PropertyType.SubArea.f)


# nrow = 42, one per level for PropertyType.SubArea.f
contrasts(hd_all$PropertyType.SubArea.f, how.many = 36) <- cont_matrix

S = contrasts(hd_all$PropertyType.SubArea.f)
View(S)

L = as.fractions(ginv(S))
View(L )

contrasts(hd_all$PropertyType.SubArea.f)

#### Run a linear regression 
XX <- cbind(1, contrasts(hd_all$PropertyType.SubArea.f))

XY <- matrix(-1/7, nrow = dim(hd_all)[1], ncol = 7)
XY[hd_all$PropertyType == 'Apartment',1] <- 6/7
XY[hd_all$PropertyType == 'Detached', 2] <- 6/7
XY[hd_all$PropertyType == 'Duplex', 3] <- 6/7
XY[hd_all$PropertyType == 'End of Terrace', 4] <- 6/7
XY[hd_all$PropertyType == 'Semi-Detached', 5] <- 6/7
XY[hd_all$PropertyType == 'Terraced', 6] <- 6/7
XY[hd_all$PropertyType == 'Townhouse', 7] <- 6/7

rowSums(XY)

table(hd_all$PropertyType)
colSums(XY)

XY2 <- cbind(1, XY)

lm1 <- lm(LogPricem2 ~ XY - 1, data = hd_all)
summary(lm1)
plot(lm1)

dim(hd_all)
dim(XX)

lm1 <- lm(LogPricem2 ~ PropertyType.f*SubArea.f, data = hd_all)
model.matrix(lm1) 



### Check the temporal element 

# Set seed for reproducibility
set.seed(123)

# Define the number of months and observations per month
num_months <- 12
obs_per_month <- 100

# Generate the data
data <- data.frame(
  Month = rep(1:num_months, each = obs_per_month),
  Price = unlist(lapply(1:num_months, function(x) rnorm(obs_per_month, mean = 1/2*x^2, sd = x*100)))
)

# Display the first few rows of the dataset
head(data)

# Display the structure of the dataset
str(data)

ggplot(data, aes(x = factor(Month), y = Price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Price per Month",
       x = "Month",
       y = "Price") +
  theme_minimal()


model <- gam(Price ~ s(Month), data = data)
draw(model)



