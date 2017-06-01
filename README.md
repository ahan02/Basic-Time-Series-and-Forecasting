### Basic-Time-Series

### Check time series pattern
plot.ts(energy.data, type='b')

![quadratic pattern](https://cloud.githubusercontent.com/assets/20606137/26661149/9d61f766-4641-11e7-9216-d431649d6484.JPG)

Example plot, Quadratic pattern detected, and growth at an increasing rate

### Construct and Build Time Series Model
y<-ts((energy.data)) <br />
t<-time(y) <br />
L<-4   <br />
m<-10 <br />
T=L*m <br />
Q<-factor(rep(c(2:L,1),m)) <br />
energy.model<-lm(y~t+I(t^2)+Q) <br />
summary(energy.model)   

### Forecast and Trend Model

t<-time(ts(dt)) <br />
trend.model<-lm(dt~t) <br />
After calculate seasonal factor, compute and forecast using the formula: <br />
![formula](https://cloud.githubusercontent.com/assets/20606137/26661604/58142438-4644-11e7-9a42-74d70116b372.png)

#### Require Package 
The R package TTR allows to perform technical analysis <br />
You can install the stable version on R CRAN.  <br />
install.packages('TTR')   <br />
To calculate simple Moving Average, use SMA(). <br />
<br />
The alternate way to perform is to use Forecast package 
