# Class for simulated depmix model

setClass("depmix.sim",
  contains="depmix",
  representation(
    states="matrix"
  )
)

setClass("mix.sim",
  contains="mix",
  representation(
	states="matrix"
  )
)