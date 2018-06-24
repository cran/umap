## ---- echo=FALSE---------------------------------------------------------
## block with some startup/background objects functions
library(umap)

iris.colors = c("#1f77b4", "#d62728", "#7f7f7f")
iris.colors = c("#ff7f00", "#e377c2", "#17becf")
plot.iris = function(x, labels,
         main="A UMAP visualization of the Iris dataset",
         pad=0.02, cex=0.65, pch=19,
         cex.main=1, cex.legend=1) {

  layout = x$layout
  par(mar=c(0.2,0.7,1.2,0.7), ps=10)
  xylim = range(layout)
  xylim = xylim + ((xylim[2]-xylim[1])*pad)*c(-0.5, 0.5)
  plot(xylim, xylim, type="n", axes=F, frame=F)
  xylim = par()$usr
  rect(xylim[1], xylim[1], xylim[2], xylim[2], border="#aaaaaa", lwd=0.2)
  points(layout[,1], layout[,2], col=iris.colors[as.integer(labels)],
         cex=cex, pch=pch)
  mtext(side=3, main, cex=cex.main)

  labels.u = unique(labels)
  legend("topright", legend=as.character(labels.u),
         col=iris.colors[as.integer(labels.u)],
         bty="n", pch=pch, cex=cex.legend)
}

set.seed(123456)

## ------------------------------------------------------------------------
head(iris, 3)

## ------------------------------------------------------------------------
iris.data = iris[, grep("Sepal|Petal", colnames(iris))]
iris.labels = iris[, "Species"]

## ----iris.umap, cache=TRUE-----------------------------------------------
library(umap)
iris.umap = umap(iris.data)

## ----umap.print----------------------------------------------------------
iris.umap

## ----umap.layout---------------------------------------------------------
head(iris.umap$layout, 3)

## ---- fig.width=3.6, fig.height=3.6, dpi=150-----------------------------
plot.iris(iris.umap, iris.labels)

## ----defaults, eval=FALSE------------------------------------------------
#  umap.defaults

## ----defaults2, eval=TRUE, echo=FALSE, collapse=TRUE---------------------
umap.defaults

## ----custom.config, eval=TRUE--------------------------------------------
custom.config = umap.defaults
custom.config$seed = 123

## ----custom2, cache=TRUE, fig.width=3.6, fig.height=3.6, dpi=150---------
iris.umap.2 = umap(iris.data, custom.config)
plot.iris(iris.umap.2, iris.labels,
          main="Another UMAP visualization of the Iris dataset (different seed)")

## ----custom3, eval=FALSE-------------------------------------------------
#  iris.umap.3 = umap(iris.data, seed=123)

## ---- eval=FALSE---------------------------------------------------------
#  iris.umap.4 = umap(iris.data, method="python")

## ----show.plot.iris------------------------------------------------------
plot.iris

## ------------------------------------------------------------------------
sessionInfo()

