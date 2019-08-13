####
## This is a simplified version of with.flogging from Proc4

flog.try <- function(expr,context=deparse(substitute(expr)),
                         loggername=flog.namespace(),
                         tracelevel=c("WARN","ERROR","FATAL")) {
  tracelevel <- toupper(tracelevel)
  handler <- function(obj) {
    ## Change behaviour based on type of message
    level <- sapply(class(obj), switch,
                    trace="TRACE",
                    debug="DEBUG",
                    message="INFO",
                    warning="WARN",
                    caughtError = "ERROR",
                    error="FATAL", "")
    ## Fixes multiple classes on message.
    level <- c(level[level != ""], "ERROR")[1]
    simpleMessage <- switch(level, DEBUG=,INFO=TRUE, FALSE)

    ## Format message
    txt   <- conditionMessage(obj)
    if (!simpleMessage) txt <- paste(txt, "\n", sep="")
    msg <- paste("While ", context, ", ", level,
                 ifelse(level=="FATAL"," ERROR:  ",":  "),txt, sep="")
    logger <- switch(level,
                     TRACE=flog.trace,
                     DEBUG=flog.debug,
                     INFO=flog.info,
                     WARN=flog.warn,
                     ERROR=flog.error,
                     FATAL=flog.fatal,flog.error)
    logger(msg,name=loggername)
    if (level %in% tracelevel) {
        calls <- sys.calls()
        calls <- calls[1:length(calls)-1]
        trace <- limitedLabels(c(calls, attr(obj, "calls")))
        if (length(trace) > 0L) {
          trace <- trace[length(trace):1L]
        }
        flog.debug("Traceback:",trace,
                   name=loggername,capture=TRUE)
    }

    ## Muffle any redundant output of the same message
    optionalRestart <- function(r) {
      res <- findRestart(r)
      if (!is.null(res)) invokeRestart(res)
    }
    optionalRestart("muffleMessage")
    optionalRestart("muffleWarning")
    if (level %in% c("ERROR","FATAL"))
      invokeRestart("tryError",msg,obj)
  }
  withRestarts(
      withCallingHandlers(expr,
                          debug=handler, message=handler, warning=handler,
                          caughtError=handler, error=handler),
      tryError=
        function(msg,obj)
          invisible(structure(msg, class = "try-error", condition = obj)))
}
