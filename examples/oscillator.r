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

dampedOscillatorData1 <- simulateDampedOscillator(noiseSd=0.005)


# apply symbolic regression via GP to find a model for the damped oscillator data...
#

models <- symbolicRegression(amplitude ~ time, data=dampedOscillatorData1,
                             stopCondition=makeTimeStopCondition(120))
modelRMSEs <- Map(models1$fitnessFunction, model1$population)
bestModelIndex <- which.min(modelRMSEs)
bestModel <- models1$population[[bestModelIndex]]
bestModelRMSE <- modelRMSEs[[bestModelIndex]]

