### Basic-Time-Series

### Check time series pattern
plot.ts(energy.data, type='b')

![quadratic pattern](https://cloud.githubusercontent.com/assets/20606137/26661149/9d61f766-4641-11e7-9216-d431649d6484.JPG)

Quadratic pattern deteced, and growth at an increasing rate

### Construct and Build Time Series Model
y<-ts((energy.data))
t<-time(y)
L<-4   
m<-10
T=L*m
Q<-factor(rep(c(2:L,1),m))
energy.model<-lm(y~t+I(t^2)+Q)
summary(energy.model)

### Forecast and Trend Model

t<-time(ts(dt))
trend.model<-lm(dt~t)
After calculate seasonal factor, compute and forecast using the formula:![formula](https://cloud.githubusercontent.com/assets/20606137/26661604/58142438-4644-11e7-9a42-74d70116b372.png)

#### Require Package 
The R package TTR allows to perform technical analysis
You can install the stable version on R CRAN.

install.packages('TTR')

To calculate simple Moving Average, use SMA(). The alternate way to perform is to use Forecast package 
