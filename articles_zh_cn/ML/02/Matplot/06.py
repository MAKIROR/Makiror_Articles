import graphviz as pgv

G = pgv.AGraph(strict=False, directed=True)

G.node_attr['shape'] = 'circle'

G.add_node("C", label="C")
G.add_node("x1", label="x_1")
G.add_node("x2", label="x_2")

G.draw("graph.png", format="png")
