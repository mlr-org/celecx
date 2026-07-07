# Empty lists that are populated by class definitions and registered in .onLoad
optimizers = list()
learners = list()
resamplings = list()
measures = list()
acq_functions = list()

# Defaults for the distributional predict-type parameters of a LearnerLCE.
lce_default_probs = c(0.05, 0.25, 0.5, 0.75, 0.95)
lce_default_n_samples = 100L
