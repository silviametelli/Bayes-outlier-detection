#### R function to produce network plots 
# t1,t2 vectors of treatments (mandatory)
# w.n, w.e weight factor for re-scaling of edge/node size (optional)
# igraph arguments: layout_in_circle default, cex=1.0 default labels size etc.. (optional)

networkplot = function(t1,
                       t2, 
                       w.n=1.2, 
                       w.e=2,
                       cexnodes=1.0,
                       vertex.label.font=2,
                       cexedges=1.4,
                       vertex.label.dist=3,
                       layout=layout_in_circle,
                       coloredges="gray60",
                       colornodes="blue",
                       nodesize=8,
                       radscale=FALSE, 
                       weightnodes=FALSE,
                       edgelabel=TRUE,
                       totnnodes=n.vec, 
                       vnames=sort(V(g)$name)
){
  
  edges = data.frame(t1,t2) 
  colnames(edges) = c("Source","Target")
  nodes = unique(c(t1,t2))
  g.u = igraph::simplify(igraph::graph.data.frame(edges,directed=FALSE)) # compute graph
  unique_edges = data.frame(get.edgelist(g.u))
  
  getNodeID = function(x){
    which(x == igraph::V(g.u)$name) - 1} # ensure IDs start at 0
  
  sorted_edges = arrange(unique_edges,unique_edges[,1],unique_edges[,2])
  colnames(sorted_edges)=c("Source","Target")
  edges_= data.frame(ddply(sorted_edges,.(sorted_edges$Source,sorted_edges$Target),nrow))
  colnames(edges_) = c("Source","Target","Weight")
  
  nodeList = data.frame(ID = c(0:(igraph::vcount(g.u) - 1)),nName = c(paste0(V(g.u)$name)))
  edgeList = plyr::ddply(edges_, .variables = c("Source", "Target", "Weight"), 
                         function (x) data.frame(SourceID = getNodeID(x$Source), 
                                                 TargetID = getNodeID(x$Target)))
  nodeList = cbind(nodeList, nodeDegree=igraph::degree(g.u, v = igraph::V(g.u), mode = "all"))
  
  sorted_edges_full = arrange(edges,edges$Source,edges$Target)
  colnames(sorted_edges_full) = c("Source","Target")
  edges_full = data.frame(ddply(sorted_edges_full,.(sorted_edges_full$Source,sorted_edges_full$Target),nrow))
  colnames(edges_full) = c("Source","Targets","Weight")
  
  adj.matrix = get.adjacency(graph.edgelist(as.matrix(sorted_edges_full), directed=FALSE),type="lower")
  adj.matrix = adj.matrix[order(rownames(adj.matrix)), order(colnames(adj.matrix))]
  g = graph_from_adjacency_matrix(adj.matrix, weighted=TRUE, mode="plus", diag=FALSE) ## direction doesn't count
  V(g)$name=sort(V(g)$name)
  
  if(weightnodes){
    #deg = degree(g, mode="in") ## prop.to node degree 
    V(g)$size = n.vec$nvec} else{ V(g)$size= rep(nodesize,length(nodes))} 
  
  if(edgelabel){
    edgeweights=E(g)$weight
  }else{edgeweights=NULL}
  
  if(radscale){
    radian.rescale = function(x, start=0, direction=1) {
      c.rotate = function(x) (x + start) %% (2 * pi) * direction
      c.rotate(scales::rescale(x, c(0, 2 * pi), range(x))) }
    lab.locs = radian.rescale(x=1:length(nodes), direction=-1, start=0)
  } else{ lab.locs = NULL }
  
  if(is.null(vnames)){
    vnames=sort(V(g)$name)
  }else{vnames=vnames}
  
  wrap_strings = function(v.strings,width){
    as.character(sapply(v.strings, FUN=function(x){
      paste(strwrap(x,width=width,simplify = TRUE), collapse="\n")
    }))
  }
  
  plot.g =  plot(g, 
                 vertex.label=c(paste0(wrap_strings(vnames,6))),
                 vertex.size=(sqrt(V(g)$size)*1.2), 
                 edge.width=E(g)$weight*w.e,
                 vertex.label.font=vertex.label.font,
                 edge.label.font=2,
                 vertex.frame.color="white",
                 edge.frame.color="black",
                 vertex.label.family="Times", 
                 edge.label.family = "Times",
                 vertex.label.dist=vertex.label.dist,    
                 vertex.label.degree=lab.locs,
                 edge.label.color="skyblue2",
                 vertex.label.color="black",
                 vertex.color=colornodes,
                 edge.color=coloredges,
                 edge.label=c(paste0(edgeweights)),
                 edge.label.degree=lab.locs,
                 vertex.label.cex=cexnodes,
                 edges.label.font=4,
                 edge.label.cex = cexedges,
                 layout=layout,
                 asp = 1,
                 loop.angle=20,
                 keep_aspect_ratio=T,
                 margin =0.25)
  
  return(plot.g)
}


