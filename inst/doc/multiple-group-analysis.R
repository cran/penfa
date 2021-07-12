## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## ----dataset, R.options = list(width = 100)-------------------------------------------------------
library(penfa)
data(ccdata)

summary(ccdata)

## ----syntax_mg----------------------------------------------------------------
syntax.mg = 'help  =~ 1*h1 +          h2 +          h3 + h4 + h5 + h6 + h7 + 0*v1 + v2 + v3 + v4 + v5
             voice =~ 0*h1 + start(0)*h2 + start(0)*h3 + h4 + h5 + h6 + h7 + 1*v1 + v2 + v3 + v4 + v5
             
             h2 + h3 + h4 + h5 + h6 + h7 + v2 + v3 + v4 + v5 ~ 1 
             help  ~ NA*1
             voice ~ NA*1 '

## ----mle.mgfit----------------------------------------------------------------
mle.fitMG <- penfa(## factor model
                   model = syntax.mg,
                   data  = ccdata,
                   group = "country",
                   ## (no) penalization
                   pen.shrink = "none",
                   pen.diff = "none",
                   eta = list(shrink = c("lambda" = 0), diff = c("none" = 0)),
                   strategy = "fixed",
                   verbose = FALSE) 

## ----weights.mg---------------------------------------------------------------
mle.weightsMG <- coef(mle.fitMG)

## ----alasso_mg_fit------------------------------------------------------------
alasso.fitMG <- penfa(## factor model
                      model = syntax.mg,
                      data = ccdata,
                      group = "country",
                      int.lv.free = TRUE,
                      ## penalization
                      pen.shrink = "alasso",
                      pen.diff = "alasso",
                      eta = list(shrink = c("lambda" = 0.01),
                                 diff   = c("lambda" = 0.1, "tau" = 0.01)),
                      ## automatic procedure
                      strategy = "auto",
                      gamma = 4,
                      ## alasso
                      weights = mle.weightsMG,
                      verbose = TRUE)

## ----alasso_mg_show-----------------------------------------------------------
alasso.fitMG

## -----------------------------------------------------------------------------
summary(alasso.fitMG)

## ----penfaOutMG---------------------------------------------------------------
penfaOut(alasso.fitMG, which = c("lambda", "tau"))

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  alasso.fitMG@Inference$edf.single

## ----echo=FALSE---------------------------------------------------------------
round(alasso.fitMG@Inference$edf.single, 3)

## ----R.options = list(width = 100)----------------------------------------------------------------
implied <- fitted(alasso.fitMG)
implied

## ----message=FALSE, warning=FALSE---------------------------------------------
full.penmat <- penmat(alasso.fitMG)

## ----message=FALSE, warning=FALSE---------------------------------------------
sparsity.penmat <- penmat(alasso.fitMG, type = "shrink", which = "lambda")

## ----message=FALSE, warning=FALSE---------------------------------------------
loadinvariance.penmat <- penmat(alasso.fitMG, type = "diff", which = "lambda")

## ----fig.height=6, fig.width=8, message=FALSE, warning=FALSE------------------
intinvariance.penmat <- penmat(alasso.fitMG, type = "diff", which = "tau")

## ----fscores_mg---------------------------------------------------------------
fscoresMG <- penfaPredict(alasso.fitMG, assemble = TRUE)
head(fscoresMG)

## -----------------------------------------------------------------------------
sessionInfo()

