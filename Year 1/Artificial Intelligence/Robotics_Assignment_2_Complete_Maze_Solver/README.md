## Robotics Assignment 2: Maze Navigation and Mapping

This assignment builds on the basic knowledge gathered within the first assignment, and challenges our team to create a robot that navigates through a maze, mapping it as it goes along. 

The goal is to reach the end square, and then returning to the starting square. To do so, the tasks our team required to do included:

1. Program a robot that is capable of reliably navigating a maze to reach the end square. 

2. Map the maze. It must keep track of its position, heading and previous locations.

3. Find the shortest way home after mapping the whole maze.

4. View the map through communication with a client running on a separate device, updating a GUI containing an occupancy grid. 

For this task, we used the ultrasonic sensor mounted on a rotor to check if a wall is front, left or right of it. The 2 colour sensor manages movement between squares. 

This design came about as changes to our robot from the 1st assignment was nessesary as the requirements of the task changed.

As the robot moves, it updates a 6x9 square matrix with readings from the ultrasonic sensor and displays the output of the map. 

It is displayed as a grid matrix GUI onto my laptop through bluetooth where different values indicate if theres a wall, an avaiable path, an unchecked path, a path with a dead-end, etc...  

