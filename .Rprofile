options(renv.snapshot.filter = function(project, ...) {
  lockfile <- renv::snapshot(project=project, lockfile=NULL, type="explicit")
  packages <- names(lockfile$Packages)
  setdiff(packages, c("tdmore", "shinytdmore"))
})
