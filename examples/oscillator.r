# oscillator.r
# using GP to find the time-position-equation of a simple damped oscillator
#

require(rgp)


# a simple simulator for damped oscillators...
#

new.dampedOscillator <- function(m = 1, R = 1, x0 = 1, omega = pi, phi0 = pi, noiseSd = 0) {
  delta <- R / 2 * m
  function(t) x0 * exp(-delta * t) * sin(omega * t + phi0) + rnorm(length(t), sd=noiseSd)
}


# create simulated test data...
#

simulateDampedOscillator <- function(samples = 512, maxt = 4 * pi, ...) {
  xs <- seq(from=1, to=maxt, length.out=samples)
  dampedOscillator <- new.dampedOscillator(...)
  data.frame(time=xs, amplitude=dampedOscillator(xs))
}

dampedOscillatorData <- simulateDampedOscillator(noiseSd=0.001)

plot(dampedOscillatorData, type = "l")


# apply symbolic regression via GP to find a model for the damped oscillator data...
#

models <- symbolicRegression(amplitude ~ time, data=dampedOscillatorData,
                             stopCondition=makeTimeStopCondition(5 * 60))
modelRMSEs <- Map(models$fitnessFunction, models$population)
bestModelIndex <- which.min(modelRMSEs)
bestModel <- models$population[[bestModelIndex]]
bestModelRMSE <- modelRMSEs[[bestModelIndex]]

# use the model for predictions...
#

detailedPrediction <- predict(models, newdata = dampedOscillatorData, detailed = TRUE)

lines(detailedPrediction$response[,"predicted"], col = 2)
