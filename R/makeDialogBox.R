#' makeDialogBox
#' @description
#' Creates a dialog box to ask if the user wants to input more points.
#'
#'
#' @param vars text appearing in popup box
#' @param title NULL
#' @param prompt NULL
#'
#' @returns value input by the user into the dialog box.
#' @export
#'
#' @examples makeDialogBox
makeDialogBox <- function(vars    = c('How many additional unkown data points are needed? Enter `0` if you are done.'),
                           title  = 'Variable Entry',
                           prompt = NULL) {
  labs <- vars
  show   <- rep(NA, length(vars))
  fun    <- rep(list(as.character), length(vars))

  stopifnot(length(vars) == length(labs), length(labs) == length(fun))

  # Create a variable to keep track of the state of the dialog window:
  # done = 0; If the window is active
  # done = 1; If the window has been closed using the OK button
  # done = 2; If the window has been closed using the Cancel button or destroyed
  done <- tcltk::tclVar(0)

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, title)
  entries <- list()
  tclvars <- list()

  # Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens,
  # assign 2 to done.
  tcltk::tkbind(tt,"<Destroy>",function() tcltk::tclvalue(done)<-2)

  for(i in seq_along(vars)) {
    tclvars[[i]] <- tcltk::tclVar("")
    if(is.na(show[i]) | is.null(show[i])) {
      entries[[i]] <- tcltk::tkentry(tt, textvariable=tclvars[[i]])
    } else {
      entries[[i]] <- tcltk::tkentry(tt, textvariable=tclvars[[i]], show=show[i])
    }
  }

  doneVal <- as.integer(tcltk::tclvalue(done))
  results <- list()

  reset <- function() {
    for(i in seq_along(entries)) {
      tcltk::tclvalue(tclvars[[i]]) <<- ""
    }
  }
  reset.but <- tcltk::tkbutton(tt, text="Reset", command=reset)

  submit <- function() {
    for(i in seq_along(vars)) {
      tryCatch( {
        results[[vars[[i]]]] <<- fun[[i]](tcltk::tclvalue(tclvars[[i]]))
        tcltk::tclvalue(done) <- 1
      },
      error = function(e) { tcltk::tkmessageBox(message=geterrmessage()) },
      finally = { }
      )
    }
  }
  submit.but <- tcltk::tkbutton(tt, text="Submit", command=submit)

  if(!is.null(prompt)) {
    tcltk::tkgrid(tcltk::tklabel(tt,text=prompt), columnspan=3, pady=10)
  }

  for(i in seq_along(vars)) {
    tcltk::tkgrid(tcltk::tklabel(tt, text=labs[i]), entries[[i]], pady=10, padx=10, columnspan=4)
  }

  tcltk::tkgrid(submit.but, reset.but, pady=10, padx=10, columnspan=3)
  tcltk::tkfocus(tt)
  tcltk::tkwait.variable(done)

  if(tcltk::tclvalue(done) != 1) {
    results <- NULL
  }

  tcltk::tkdestroy(tt)
  return(results)
}
