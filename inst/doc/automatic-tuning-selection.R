## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----dataset, R.options = list(width = 100)-------------------------------------------------------
library(penfa)
data(ccdata)

summary(ccdata)

## ----syntax-------------------------------------------------------------------
syntax = 'help  =~   h1 + h2 + h3 + h4 + h5 + h6 + h7 + 0*v1 + v2 + v3 + v4 + v5
          voice =~ 0*h1 + h2 + h3 + h4 + h5 + h6 + h7 +   v1 + v2 + v3 + v4 + v5'

## ----mle.fit------------------------------------------------------------------
mle.fit <- penfa(## factor model 
                 model = syntax, 
                 data  = ccdata,
                 std.lv = TRUE,
                 ## (no) penalization
                 pen.shrink = "none",
                 eta = list(shrink = c("none" = 0), diff = c("none" = 0)),
                 strategy = "fixed")

## ----weights------------------------------------------------------------------
mle.weights <- coef(mle.fit)

## ----penfaOut-----------------------------------------------------------------
penfaOut(mle.fit)

## ----lasso--------------------------------------------------------------------
lasso.fit <- penfa(## factor model
                   model  = syntax,
                   data   = ccdata,
                   std.lv = TRUE,
                   ## penalization
                   pen.shrink = "lasso",
                   eta = list(shrink = c("lambda" = 0.01), diff = c("none" = 0)),
                   ## automatic procedure
                   strategy = "auto")

## ----summary_lasso------------------------------------------------------------
summary(lasso.fit)

## ----alasso-------------------------------------------------------------------
alasso.fit <- penfa(## factor model
                    model  = syntax,
                    data   = ccdata,
                    std.lv = TRUE,
                    ## penalization
                    pen.shrink = "alasso",
                    eta = list(shrink = c("lambda" = 0.01), diff = c("none" = 0)),
                    ## automatic procedure
                    strategy = "auto",
                    gamma = 4,
                    ## alasso
                    weights = mle.weights,
                    verbose = FALSE)
alasso.fit

## -----------------------------------------------------------------------------
alasso.fit@Inference$edf.single

## ----alasso.summary-----------------------------------------------------------
summary(alasso.fit)

## ----IC-----------------------------------------------------------------------
BIC(alasso.fit)

## -----------------------------------------------------------------------------
implied <- fitted(alasso.fit)
implied

## -----------------------------------------------------------------------------
alasso_penmat <- penmat(alasso.fit)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  diag(alasso_penmat)

## ----echo=FALSE---------------------------------------------------------------
print(formatC(diag(alasso_penmat), digits = 2, format = "f"), quote = FALSE)

## ----fscores------------------------------------------------------------------
fscores <- penfaPredict(alasso.fit)
head(fscores)

## -----------------------------------------------------------------------------
sessionInfo()

