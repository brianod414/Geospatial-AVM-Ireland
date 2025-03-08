
rw_penalty2 <- mrf_penalty(object = rep(1:12),  type = 'linear')
# View(rw_penalty)

I = rep(0, 12)
for(i in 2:12){I[i] <- I[i-1] + rnorm(1,0, 2)}
#I <- 0 + 1*(1:12)


plot(I, type = 'l')

y <- rnorm(100*12, rep(I, each = 100), 1)

plot(rep(1:12, each = 100), y)
lines(I, type = 'l')

dat <- data.frame(y = y, month = rep(1:12, each = 100))
dat$month <- factor(dat$month, ordered = TRUE, 
                                  levels = c(1:12))

contrasts(dat$month) <- contr.sum(12)

# model <- mgcv::gam(y ~ -1 + s(month, bs = 'mrf', k =12, xt = list(penalty = rw_penalty)), data = dat)
model <- mgcv::gam(y ~  -1 + s(month, bs = 'mrf', xt = list(penalty = rw_penalty)), data = dat)
summary(model)
names(model)

model$smooth

draw(model)

model$coefficients


lines(rep(2:12), c(model$coefficients[4:11], model$coefficients[1:3])-model$coefficients[4], col = 'red')






















library(mgcv)

# number of months
N<-12

# generate linear data
t<-rep(1:N,each=10)
f<-0 + 1*t
y<- f + rnorm(length(t))
plot(t,y)

# generate nonlinear data
set.seed(1)
t<-rep(1:N,each=10)
f<-cumsum(rnorm(N))
y<- f[t] + 1*rnorm(length(t))
plot(t,y)
lines(f,col="red",lwd=2)

##### model p  spline order 1 penalty 1
mod <- gam(y ~ s(t,bs="ps",m=c(1,1)), 
           #data = dat,
           drop.unused.levels = FALSE,
           method = "REML")
summary(mod)
mod$coefficients
plot(mod)
lines(1:N,f - mean(f),col="red")

#based on link
m_bs <- gam(y ~ s(t, bs = "bs", m = 1), method = "REML")
plot(m_bs)
lines(1:N,f - mean(f),col="red")

m_ps <- gam(y ~ s(t, bs = "ps", m = c(0, 1)), method = "REML")
plot(m_ps)
lines(1:N,f - mean(f),col="red")




