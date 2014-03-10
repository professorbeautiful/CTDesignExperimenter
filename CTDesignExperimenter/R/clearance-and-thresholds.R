cat("======== clearance-and-thresholds.R  ================\n")



printNetworkOutputs = function(networkEnv) {
  for(v in ls(env=networkEnv)) {
    vv = get(v, env=networkEnv)
    catn(paste(v, vv))
  }   ### Includes parameters.
}

# To Examples: evaluateOutput(vg_clearanceRate)

