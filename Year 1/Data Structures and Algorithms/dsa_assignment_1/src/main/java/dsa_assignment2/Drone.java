package dsa_assignment2;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;

/**
 * A Drone class to simulate the decisions and information collected by a drone
 * on exploring an underground maze.
 * 
 */
public class Drone implements DroneInterface
{
	private static final Logger logger     = Logger.getLogger(Drone.class);
	
	public String getStudentID()
	{
		//change this return value to return your student id number
		return "1943871";
	}

	public String getStudentName()
	{
		//change this return value to return your name
		return "Cuong Phan";
	}

	/**
	 * The Maze that the Drone is in
	 */
	private Maze                maze;
	
	//Visited is the first time you reach each room, 
	//visit queue is literally everytime you visit a room, 
	//visitstack is a stack for you to keep track of where to backtrack
	

	/**
	 * The stack containing the portals to backtrack through when all other
	 * doorways of the current chamber have been explored (see assignment
	 * handout). Note that in Java, the standard collection class for both
	 * Stacks and Queues are Deques
	 */
	private Deque<Portal>       visitStack = new ArrayDeque<>();
	
	
	//used to initialize the seachStep method, is only used once.
	private boolean init = false;

	/**
	 * The set of portals that have been explored so far.
	 */
	private Set<Portal>         visited    = new HashSet<>();

	/**
	 * The Queue that contains the sequence of portals that the Drone has
	 * followed from the start
	 */
	private Deque<Portal>       visitQueue = new ArrayDeque<>();

	/**
	 * This constructor should never be used. It is private to make it
	 * uncallable by any other class and has the assert(false) to ensure that if
	 * it is ever called it will throw an exception.
	 */
	@SuppressWarnings("unused")
	private Drone()
	{
		assert (false);
	}

	/**
	 * Create a new Drone object and place it in chamber 0 of the given Maze
	 * 
	 * @param maze
	 *            the maze to put the Drone in.
	 */
	public Drone(Maze maze)
	{
		this.maze = maze;
	}

	/* 
	 * @see dsa_assignment2.DroneInterface#searchStep()
	 */
	@Override
	public Portal searchStep()
	{
		if (!init) {
			Portal firstPortal = new Portal (maze.getCurrentChamber(), maze.getNumDoors() - 1);//initialize portal object
			visitStack.addFirst(firstPortal = new Portal(maze.getCurrentChamber(), 0));
			visited.add(firstPortal);
			init = true;
		}
		
		if (visitStack.isEmpty()) {
			return null;
		}

		Portal testPortal = visitStack.peek(); 
	
		int itr = 0;
		while (itr < maze.getNumDoors()) {//iterate through doors of currentPortal
			Portal reference = new Portal(testPortal.getChamber(), itr);
			
			testPortal = maze.traverse(itr);//test going through a door
			
			if(testPortal != null && !visited.contains(testPortal)) {//if a chamber exist and isn't visited 
				if (!reference.equals(visitQueue.peekFirst())) {
					visitQueue.addFirst(reference);
					visitQueue.addFirst(testPortal);  
				} else {
					Portal tempRemove = visitQueue.peek();
					visitQueue.pop();
					Portal checkIfReturn = visitQueue.peek();
					if (testPortal.equals(checkIfReturn)) {
						visitQueue.addFirst(tempRemove);
						visitQueue.addFirst(reference);
						visitQueue.addFirst(testPortal); 
					} else {
						visitQueue.addFirst(tempRemove);
					} 
				}
			
				visited.add(testPortal);
				visitStack.addFirst(testPortal);
				
				return testPortal;
			}
			else {
				testPortal = maze.traverse(testPortal.getDoor()); //go back to previous door and iterate
			
				itr++;
				}
		}
		if(itr == maze.getNumDoors()) {
			
			Portal beforePop = visitStack.peek();
			visitStack.pop();
			Portal popped = visitStack.peek();
			
			if (visitStack.isEmpty()) {				
				return null;
			}
			
			int itr2 = 0;
			while (itr2 < maze.getNumDoors()) {//iterate through doors of currentPortal
				testPortal = maze.traverse(itr2);
			
				if (maze.getCurrentChamber() == popped.getChamber()) {
					visitQueue.addFirst(beforePop);
					visitQueue.addFirst(testPortal);

					return testPortal;
				} else {
					testPortal = maze.traverse(testPortal.getDoor()); //go back to previous door and iterate
					itr2++;
				}
			}
			
		}

		return testPortal;
	}

	/* 
	 * @see dsa_assignment2.DroneInterface#getVisitOrder()
	 */
	@Override
	public Portal[] getVisitOrder()
	{
		List<Portal> visitOrder = new ArrayList<Portal>();
		List<Portal> visitQueueStep = new ArrayList<Portal>();
		visitQueueStep.addAll(visitQueue);
		
		
		for (int i = 0; i < visitQueueStep.size(); i++) {
			if (visitQueueStep.get(i) != null){
				visitOrder.add(visitQueueStep.get(i));
			}
		}
		Portal[] visitOrdered = new Portal[visitOrder.size()];
		
		for (int i = 0; i < visitOrder.size(); i++) {
			visitOrdered[i] = visitOrder.get(i);
		}
		return visitOrdered;
	}

	/*
	 * @see dsa_assignment2.DroneInterface#findPathBack()
	 */
	@Override
	public Portal[] findPathBack()
	{
		Portal[] pathBack = getVisitOrder();
		for (int i = 0; i < pathBack.length/2;i++) {
			Portal reverse = pathBack[i];
			pathBack[i] = pathBack[pathBack.length - i - 1];
			pathBack[pathBack.length - i - 1] = reverse;
		}
		
		/*Portal[] pathBacknoFirstChamber = new Portal[pathBack.length - 1];
		for (int i = pathBack.length - 1; i > 0; i--) {
			pathBacknoFirstChamber[i - 1] = pathBack[i];
		}
		System.out.println(pathBacknoFirstChamber[0]);*/
		
		return pathBack;
	}

	
}
