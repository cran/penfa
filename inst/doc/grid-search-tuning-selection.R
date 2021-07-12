## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----dataset, R.options = list(width = 100)-------------------------------------------------------
library(penfa)
data(ccdata)

summary(ccdata)

## ----syntax-------------------------------------------------------------------
syntax = 'help  =~   h1 + h2 + h3 + h4 + h5 + h6 + h7 + 0*v1 + v2 + v3 + v4 + v5
          voice =~ 0*h1 + h2 + h3 + h4 + h5 + h6 + h7 +   v1 + v2 + v3 + v4 + v5'

## ----scad---------------------------------------------------------------------
scad.fit <- penfa(## factor model
                  model  = syntax,
                  data   = ccdata,
                  std.lv = TRUE,
                  ## penalization
                  pen.shrink = "scad",
                  eta = list(shrink = c("lambda" = 0.05), diff = c("none" = 0)),
                  ## fixed tuning
                  strategy = "fixed")

## ----scad_grid_search---------------------------------------------------------
# Grid of values for tuning parameter
eta.grid <- seq(0, 0.15, length.out = 51)

# Return GBIC from a converged and admissible penfa model with fixed tuning
penfa.fixedTun <- function(eta, penalty, ...){
  
  fitted <- penfa(model = syntax, data = ccdata, 
                  std.lv = TRUE, pen.shrink = penalty, 
                  eta = list(shrink = c("lambda" = eta), diff = c("none" = 0)),
                  strategy = "fixed", verbose = FALSE, ...)
  
  if(all(fitted@Vcov$solution) & fitted@Vcov$admissibility)
    return(BIC(fitted))
}

# additional penfaOptions can be passed
GBIC.scad <- sapply(eta.grid, penfa.fixedTun, penalty = "scad") 

## ----scad_plot, fig.height=4, fig.width=6, message=FALSE, warning=FALSE-------
optimtun.scad <- eta.grid[[which.min(GBIC.scad)]]

# To plot GBIC across tuning values
# p <- plotly::plot_ly(x = eta.grid, y = GBIC.scad, type = 'scatter', mode = 'lines')
# plotly::layout(p, xaxis = list(showline = TRUE),
#               title = list(text = "GBIC values across tuning parameters"))

## ----scad_fit-----------------------------------------------------------------
scad.fit <- penfa(## factor model 
                  model = syntax, 
                  data = ccdata, 
                  std.lv = TRUE, 
                  ## penalization
                  pen.shrink = "scad", 
                  # optimal tuning
                  eta = list(shrink = c("lambda" = optimtun.scad), diff = c("none" = 0)),
                  strategy = "fixed", 
                  verbose = FALSE)
summary(scad.fit)

## ----scad_implied-------------------------------------------------------------
implied <- fitted(scad.fit)
implied

## ----fscores------------------------------------------------------------------
fscores <- penfaPredict(scad.fit)
head(fscores)

## ----mcp_grid_search, fig.height=4, fig.width=6, message=FALSE, warning=FALSE----
GBIC.mcp <- sapply(eta.grid, penfa.fixedTun, penalty = "mcp")

optimtun.mcp <- eta.grid[[which.min(GBIC.mcp)]]
optimtun.mcp

## -----------------------------------------------------------------------------
mcp.fit <- penfa(## factor model 
                 model = syntax, 
                 data = ccdata, 
                 std.lv = TRUE, 
                 ## penalization
                 pen.shrink = "mcp", 
                 # optimal tuning
                 eta = list(shrink = c("lambda" = optimtun.mcp), diff = c("none" = 0)),
                 strategy = "fixed", 
                 verbose = FALSE)
summary(mcp.fit)

## -----------------------------------------------------------------------------
sessionInfo()

