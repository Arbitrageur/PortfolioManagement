library(R6)
library(lazyeval)

Task <- R6Class("Task",
                public = list(
                  name = NULL,
                  task = NULL,
                  dependency = list(),
                  completed = F,
                  initialize = function(name, expr) {
                    self$name <- name
                    self$task <- lazy(expr)
                  },
                  run = function() {
                    tryCatch({
                      if (!is.null(self$dependency) &&
                          length(self$dependency) > 0) {
                        for (i in self$dependency) {
                          if (!i$completed) {
                            stop(simpleError(i$name, " has not been compledted.\nTry runAll()."))
                          }
                        }
                      }
                      lazy_eval(self$task)
                      self$completed <- T
                      invisible(self)
                    },
                    error = function(e) {
                      print(e)
                      self$completed <- F
                      invisible(self)
                    })
                  },
                  runAll = function() {
                    tryCatch({
                      if (!is.null(self$dependency) &&
                          length(self$dependency) > 0) {
                        for (i in self$dependency) {
                          i$run()
                        }
                      }
                      lazy_eval(self$task)
                      self$completed <- T
                      invisible(self)
                    },
                    error = function(e) {
                      self$completed <- F
                      invisible(self)
                    })
                  },
                  runOnce = function() {
                    if (self$completed) {
                      message(self$name, " skipped.")
                      invisible(self)
                    } else {
                      tryCatch({
                        if (!is.null(self$dependency) &&
                            length(self$dependency) > 0) {

                          for (i in self$dependency) {
                            i$runOnce()
                          }
                        }
                        lazy_eval(self$task)
                        self$completed <- T
                        invisible(self)
                      },
                      error = function(e) {
                        self$completed <- F
                        stop(simpleError(self$name))
                      })
                    }
                  },
                  then = function() {
                    tryCatch({
                      if (self$completed) {
                        t$runOnce()
                      } else {
                        stop(self$name, " not completed")
                      }
                      invisible(t)
                    },
                    error = function(e) {
                      print(e)
                      invisible(t)
                    })
                  },
                  dependsOn = function(e1, ...) {
                    extra <- list(...)
                    self$dependency <- c(self$dependency, e1, extra)
                  }
                ))
