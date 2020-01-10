package dsa_assignment5;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * An Undirected Graph class implemented in an Adjacency List style
 * 
 * This is an adjacency list representation of a graph. Nodes are integers
 * starting at 0. The textbooks usually describe an adjacency list as a list (or
 * array), indexed by node numbers, of connection lists. If we did that in Java
 * we would continually end up checking for our node numbers being within range.
 * Further, we normally want a whole object for the node rather than just an
 * integer, so a {@code Map} instead of an array is a good choice.
 * 
 * The graph is represented by a {@code Map} which maps the node number to a
 * LinkedList of Connection objects, which represent the nodes that are directly
 * connected to this node.
 *
 */
public class Graph {
    // The underlying map representing the graph
    private Map<Integer, List<Connection>> graph = new HashMap<>();

    /**
     * For marking purposes
     * 
     * @return Your student id
     */
    public static String getStudentID() {
	// Change this return value to return your student id number, e.g.
	// return "1234567";
	return "1943871";
    }

    /**
     * For marking purposes
     * 
     * @return Your name
     */
    public static String getStudentName() {
	// Change this return value to return your name, e.g.
	// return "John Smith";
	return "Phan Minh Cuong";
    }

    /**
     * It is okay to create an empty graph, as we can add edges to it
     */
    public Graph() {
    }

    /**
     * Create a graph from an array of edges. Each edge is itself an array of 3
     * integers, the source node, the destination node and the distance between
     * them.
     * 
     * Each edge in the input array will be added using
     * {@code addEdge(node1,node2,distance)}, implying that the reverse of each edge
     * is also added and should NOT be explicitly in the input array.
     * 
     * @param connections the array of edges to add
     * @throws GraphException if there are not exactly 3 integers in each edge array
     */
    public Graph(int[][] connections) throws GraphException {
	for (int[] connection : connections) {
	    if (connection.length != 3)
		throw new GraphException(
			"Connections in Graphs must have 3 integers: node 1, node 2 and the distance between them. This connection did not: "
				+ Arrays.toString(connection));
	    addEdge(connection[0], connection[1], connection[2]);
	}
    }

    /**
     * Get an array of edges in the same form as the array of edges Graph
     * constructor. One important difference is that this returns ALL the individual
     * edges, thus one for the forward and one for the reverse direction of each
     * true edge.
     * 
     * @return the array of edges in the graph
     */
    public int[][] getConnections() {
	ArrayList<int[]> connections = new ArrayList<>();
	for (Map.Entry<Integer, List<Connection>> entry : graph.entrySet()) {
	    int node = entry.getKey();
	    for (Connection edge : entry.getValue())
		connections.add(new int[] { node, edge.getNode(), edge.getDistance() });
	}

	return connections.toArray(new int[0][0]);
    }

    /**
     * Add an edge to the graph
     * 
     * This graph is UNDIRECTED, so any time we add an edge we must add the reverse
     * edge as well.
     * 
     * @param node1    The source node
     * @param node2    The destination node
     * @param distance The distance or weight on the edge
     * @throws GraphException if the distance is negative
     */
    public void addEdge(int node1, int node2, int distance) throws GraphException {
	if (distance < 0)
	    throw new GraphException(String.format(
		    "All distances must be greater than or equal to 0: attempted to add node %d to node %d with distance %d",
		    node1, node2, distance));
	List<Connection> edgeList = graph.get(node1);
	if (edgeList == null) {
	    edgeList = new LinkedList<>();
	    graph.put(node1, edgeList);
	}
	edgeList.add(new Connection(node2, distance));

	// now add the reverse connection
	edgeList = graph.get(node2);
	if (edgeList == null) {
	    edgeList = new LinkedList<>();
	    graph.put(node2, edgeList);
	}
	edgeList.add(new Connection(node1, distance));
    }

    /**
     * Contract a node that has exactly two connecting edges
     * <p>
     * This is the simplest case of contracting nodes. If node X is connected to
     * node A with distance a and node B with distance b, then this should remove
     * both the X-A and the X-B edge (essentially removing X from the graph), and
     * add a new edge (A-B) with distance a+b
     * </p>
     * <p>
     * Note that there may already be a pre-existing edge between A and B. This
     * should not be changed or removed.
     * </p>
     * 
     * @param node the node to be contracted
     * @throws GraphException if the node to be contracted does not exist in the
     *                        graph or does not have precisely two edges
     */
    public void contractNodeWithTwoEdges(int node) throws GraphException {
	List<Connection> nodeContract = graph.get(node);

	// ensures node recieved has only an A and a B
	if (nodeContract.size() != 2) {
	    throw new GraphException();
	}

	// get A and B node, extract distance and node value itself
	Connection nodeA = nodeContract.get(0);
	Connection nodeB = nodeContract.get(1);

	int nodeADist = nodeA.getDistance();
	int nodeBDist = nodeB.getDistance();
	// get A + B
	int sumDistance = nodeADist + nodeBDist;

	int nodeAVal = nodeA.getNode();
	int nodeBVal = nodeB.getNode();

	// contract node
	addEdge(nodeAVal, nodeBVal, sumDistance);
	removeEdge(nodeAVal, node);
	removeEdge(node, nodeBVal);
	removeNode(node);
    }

    private void removeEdge(int node1, int node2) {
	for (Integer from : graph.keySet()) {
	    List<Connection> connections = graph.get(from);
	    for (int i = 0; i < connections.size(); i++) {
		if ((connections.get(i).getNode() == node1 && from == node2)
			|| (connections.get(i).getNode() == node2 && from == node1)) {
		    connections.remove(i);
		    i--;
		}
	    }
	}
    }

    private void removeNode(int node) {
	for (Integer from : graph.keySet()) {
	    List<Connection> connections = graph.get(from);
	    for (int i = 0; i < connections.size(); i++) {
		if (connections.get(i).getNode() == node) {
		    connections.remove(i);
		    i--;
		}
	    }
	}
	graph.remove(node);
    }

    /**
     * Apply Dijkstra's algorithm to find the distance between 2 nodes in the graph
     * <p>
     * The version of the algorithm to implement is Version 1 from the Theory part
     * handout of the module. Do not try to use Java's PriorityQueue to implement
     * Version 2 as that cannot cope with the priority of an object in the queue
     * changing while the object is in the queue.
     * </p>
     * <p>
     * Rather than using the arrays as described in the handout, I recommend you use
     * <ul>
     * <li>{@code Integer.MAX_VALUE} for infinity</li>
     * <li>A {@code Map<Integer, Integer>} for the {@code D} array in the notes, to
     * record the minimum distance found so far from the start node for each node in
     * the graph</li>
     * <li>A {@code Set<Integer>} called {@code nonTight} of nodes to represent the
     * set of nodes for which you have not yet found tight distance values for. This
     * is equivalent to the {@code tight} array of booleans in the handout but it is
     * much easier and more efficient to iterate over the {@code nonTight} set than
     * iterate over the nodes whose boolean values in the {@code tight} array are
     * true</li>
     * </ul>
     * </p>
     * There is no need to have a {@code pred} variable as there is no need to
     * calculate or return the route of the shortest path
     * <p>
     * </p>
     * 
     * @param node1 the start node in the pair between which the distance is to be
     *              found
     * @param node2 the final node in the pair between which the distance is to be
     *              found
     * @return the distance between the pair of nodes
     * @throws GraphException if either of the nodes are not in the graph or there
     *                        is no path between them
     */
    public int dijkstra(int node1, int node2) throws GraphException {
	if (!graph.containsKey(node1) || !graph.containsKey(node2))
	    throw new GraphException();

	// create an array of distances
	Map<Integer, Integer> D = new HashMap<Integer, Integer>();
	for (Integer node : graph.keySet()) {
	    D.put(node, Integer.MAX_VALUE);
	}
	// set shortest distance from node1 to itself as 0
	D.put(node1, 0);

	// creates a auxiliary array tight
	Set<Integer> nonTight = new HashSet<Integer>();
	for (Integer node : graph.keySet()) {
	    nonTight.add(node);
	}

	while (!nonTight.isEmpty()) {
	    int minEsti = Integer.MAX_VALUE;
	    int u = 0;

	    // find the smallest estimate of map D which is also within the
	    // nonTight Set, each successful if statement attemps to find a smaller node
	    // until traversed through map from the maximum possilbe value
	    for (Integer node : D.keySet()) {
		if (D.get(node) < minEsti && nonTight.contains(node)) {
		    minEsti = D.get(node);
		    u = node;
		}
	    }
	    // remove element from auxillary set, will not be called on subsequent
	    // iterations of the while loop as processed
	    nonTight.remove(u);

	    // return node found
	    List<Connection> connections = graph.get(u);
	    for (Connection connection : connections) {
		int z = connection.getNode();
		int weight = connection.getDistance();
		if (D.get(u) + weight < D.get(z)) {
		    //improving estimates
		    D.put(z, D.get(u) + weight);
		}
	    }

	}

	return D.get(node2);
    }

}
