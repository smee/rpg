testTreePlot <- function(depth, circular = FALSE) {
  ig1 <- exprToIgraph(randexprFull(conset = numericConstantSet,
                                   funcset = mathFunctionSet,
                                   inset = inputVariableSet("x1", "x2"),
                                   maxdepth = depth))
  vertexDepths <- shortest.paths(ig1)[1,] + 1
  edgeDepths <- vertexDepths[get.edges(ig1, E(ig1))[,2] + 1]
  edgeWiths <- 20 / edgeDepths
  edgeColors <- rainbow(max(edgeDepths))[edgeDepths] # TODO
  plot(ig1, layout = layout.reingold.tilford(ig1, circular = circular),
       vertex.shape = "none", vertex.size = 0, vertex.label = NA,
       edge.color = edgeColors, edge.width = edgeWiths, edge.arrow.mode = "-")
}
