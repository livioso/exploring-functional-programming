graph = [('a','b',5), ('a','c',2), ('c','b',1)]

doSpanningTreeUsingPrim [(a, b, cost)] start = (a, b, cost)

let ST = doSpanningTreeUsingPrim(graph)

main = do
