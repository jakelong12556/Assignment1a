import java.io.Serializable;
import lejos.hardware.lcd.LCD;
import lejos.utility.Delay;

/**
 * internal Mapping class of robot, handles creating original map and updating
 * status of map. parameters: orientaion of robot from Movement SonicSensor
 * orientation and if the vision returns a wall or an avaiable path at said
 * angle
 * 
 */
public class DiscLineMap extends Thread implements Serializable {

    private static final long serialVersionUID = -1456725987576828686L;
    public static final String AHEADG = "AHEAD";
    public static final String LEFTG = "LEFT";
    public static final String RIGHTG = "RIGHT";
    public static final String BEHINDG = "BEHIND";

    private static final int unknown = 0;
    private static final int unoccupied = 1;
    private static final int occupied = 2;
    private static final int green = 3;
    private static final int blocked = 4;
    private static final int robot = 5;
    private static final int wall = 8;
    private static final int goal = 9;

    private static final int left = 0;
    private static final int ahead = 1;
    private static final int right = 2;
    private static final int behind = 3;

    private int mapWidth = 13;// width 6 + 7 include wall
    private int mapLength = 19;// length 9 + 10 w/wall

    private int x;// x coord of robot
    private int y;// y coord of robot

    private int tempx;
    private int tempy;

    private int[][] map;

    private ExchangeInfo EIObject;

    public DiscLineMap(ExchangeInfo EI) {
	EIObject = EI;
	map = new int[mapWidth][mapLength];

	for (int m = 0; m < mapWidth; m++) {
	    for (int n = 0; n < mapLength; n++) {
		map[m][n] = unknown;
	    }
	}

	for (int m = 0; m < mapWidth; m++) {
	    map[m][0] = wall;
	    map[m][mapLength - 1] = wall;
	}

	for (int m = 0; m < mapLength; m++) {
	    map[0][m] = wall;
	    map[mapWidth - 1][m] = wall;
	}

	for (int n = 0; n < mapWidth; n += 2) {
	    for (int m = 0; m < mapLength; m += 2) {
		map[n][m] = wall;
	    }
	}

	x = mapWidth - 2;
	y = 1;

	tempx = x;
	tempy = y;

	EIObject.setMap(map);

	map[x][y] = robot;
    }

    public void run() {
	while (true) {
	    if (EIObject.getColor() == "WHITE") {
		tempx = x;
		tempy = y;
		LCD.drawString("xy at: " + tempx + " | " + tempy, 0, 6);
		if (isKnown() == true) {
		    updateMap();
		    EIObject.setMap(map);
		} else {
		    LCD.refresh();
		}

		setMovement();
		EIObject.setMap(map);
		Delay.msDelay(500);

		LCD.drawString("xy at: " + tempx + " | " + tempy, 0, 6);
		while (EIObject.getCommandDirection() != 4) {
		    // waiting loop to finish movement
		    Delay.msDelay(10);
		}

		EIObject.setMap(map);
		Delay.msDelay(25);
	    } else if (EIObject.getColor() == "RED" || EIObject.getColor() == "GREEN") {
		// set immediate change
		if (EIObject.getColor() == "GREEN") {
		    map[x][y] = green;
		    map[x][y - 1] = wall;
		    map[x - 1][y] = wall;
		    map[x][y + 1] = wall;
		    map[x + 1][y] = wall;
		    EIObject.setDirection(5);
		    while (EIObject.getCommandDirection() != 4) {
			// waiting loop to finish movement
			Delay.msDelay(10);
		    }
		    map[tempx][tempy] = robot;
		    x = tempx;
		    y = tempy;
		} else {
		    map[x][y] = goal;
		    map[tempx][tempy] = robot;
		    EIObject.setDirection(6);
		    while (EIObject.getCommandDirection() != 4) {
			// waiting loop to finish movement
			Delay.msDelay(10);
		    }
		    x = tempx;
		    y = tempy;
		    while (true) {

			if (x == mapWidth - 2 && y == 1) {
			    break;
			}

			tempx = x;
			tempy = y;

			LCD.drawString("xy at: " + tempx + " | " + tempy, 0, 6);
			getHome();
			EIObject.setMap(map);
			Delay.msDelay(500);

			LCD.drawString("xy at: " + tempx + " | " + tempy, 0, 6);

			while (EIObject.getCommandDirection() != 4) {
			    // waiting loop to finish movement
			    Delay.msDelay(10);
			}

			EIObject.setMap(map);

		    }
		    EIObject.setDirection(7);
		    break;
		}
	    }
	}
    }

    public void getHome() {
	int lBot = map[x][y - 1];
	int aBot = map[x - 1][y];
	int rBot = map[x][y + 1];
	int bBot = map[x + 1][y];

	LCD.drawString("going Home", 0, 7);

	if (lBot == occupied) {
	    // go left, set current x y as wall
	    map[x][y - 2] = robot;
	    map[x][y] = blocked;
	    map[x][y - 1] = blocked;
	    y = y - 2;
	    EIObject.setMap(map);
	    EIObject.setDirection(left);
	} else if (aBot == occupied) {
	    // go ahead, set current x y as wall
	    map[x - 2][y] = robot;
	    map[x][y] = blocked;
	    map[x - 1][y] = blocked;
	    x = x - 2;
	    EIObject.setMap(map);
	    EIObject.setDirection(ahead);
	} else if (rBot == occupied) {
	    // go right, set current x y as wall
	    map[x][y + 2] = robot;
	    map[x][y] = blocked;
	    map[x][y + 1] = blocked;
	    y = y + 2;
	    EIObject.setMap(map);
	    EIObject.setDirection(right);
	} else if (bBot == occupied) {
	    // go behind, set current x y as wall
	    map[x + 2][y] = robot;
	    map[x][y] = blocked;
	    map[x + 1][y] = blocked;
	    x = x + 2;
	    EIObject.setMap(map);
	    EIObject.setDirection(behind);
	}
    }

    public void setMovement() {
	int lBot = map[x][y - 1];
	int aBot = map[x - 1][y];
	int rBot = map[x][y + 1];
	int bBot = map[x + 1][y];
	tempx = x;
	tempy = y;

	if (lBot == unoccupied || rBot == unoccupied || bBot == unoccupied || aBot == unoccupied) {
	    LCD.drawString("exist Occ", 0, 7);
	    if (lBot == unoccupied) {
		// go left
		map[x][y - 2] = robot;
		map[x][y] = occupied;
		map[x][y - 1] = occupied;
		y = y - 2;
		EIObject.setMap(map);
		EIObject.setDirection(left);
	    } else if (aBot == unoccupied) {
		// go ahead
		map[x - 2][y] = robot;
		map[x][y] = occupied;
		map[x - 1][y] = occupied;
		x = x - 2;
		EIObject.setMap(map);
		EIObject.setDirection(ahead);
	    } else if (rBot == unoccupied) {
		// go right
		map[x][y + 2] = robot;
		map[x][y] = occupied;
		map[x][y + 1] = occupied;
		y = y + 2;
		EIObject.setMap(map);
		EIObject.setDirection(right);
	    } else if (bBot == unoccupied) {
		// go behind
		map[x + 2][y] = robot;
		map[x][y] = occupied;
		map[x + 1][y] = occupied;
		x = x + 2;
		EIObject.setMap(map);
		EIObject.setDirection(behind);
	    }
	} else {
	    LCD.drawString("backtracking", 0, 7);
	    if (lBot == occupied) {
		// go left, set current x y as wall
		map[x][y - 2] = robot;
		map[x][y] = blocked;
		map[x][y - 1] = blocked;
		y = y - 2;
		EIObject.setMap(map);
		EIObject.setDirection(left);
	    } else if (aBot == occupied) {
		// go ahead, set current x y as wall
		map[x - 2][y] = robot;
		map[x][y] = blocked;
		map[x - 1][y] = blocked;
		x = x - 2;
		EIObject.setMap(map);
		EIObject.setDirection(ahead);
	    } else if (rBot == occupied) {
		// go right, set current x y as wall
		map[x][y + 2] = robot;
		map[x][y] = blocked;
		map[x][y + 1] = blocked;
		y = y + 2;
		EIObject.setMap(map);
		EIObject.setDirection(right);
	    } else if (bBot == occupied) {
		// go behind, set current x y as wall
		map[x + 2][y] = robot;
		map[x][y] = blocked;
		map[x + 1][y] = blocked;
		x = x + 2;
		EIObject.setMap(map);
		EIObject.setDirection(behind);
	    }
	}
    }

    public void updateMap() {
	EIObject.callSonic(true);
	while (EIObject.getSonic() == true) {
	    // waiting loop to finish scanning
	    Delay.msDelay(10);
	}
	int[] sonicInfo = EIObject.getUltraInfo();
	String robotPose = EIObject.getOriRobot();
	for (int i = 0; i < sonicInfo.length; i++) {
	    String placement = globalHeading(robotPose, i);
	    if (sonicInfo[i] == 1) {
		if (placement == AHEADG) {
		    map[x - 1][y] = unoccupied;
		    map[x - 2][y] = unoccupied;
		} else if (placement == LEFTG) {
		    map[x][y - 1] = unoccupied;
		    map[x][y - 2] = unoccupied;
		} else if (placement == RIGHTG) {
		    map[x][y + 1] = unoccupied;
		    map[x][y + 2] = unoccupied;
		} else if (placement == BEHINDG) {
		    map[x + 1][y] = unoccupied;
		    map[x + 2][y] = unoccupied;
		}
	    } else {
		if (placement == AHEADG) {
		    map[x - 1][y] = wall;
		} else if (placement == LEFTG) {
		    map[x][y - 1] = wall;
		} else if (placement == RIGHTG) {
		    map[x][y + 1] = wall;
		} else if (placement == BEHINDG) {
		    map[x + 1][y] = wall;
		}
	    }
	}

    }

    public boolean isKnown() {
	int lBot = map[x][y - 1];
	int rBot = map[x][y + 1];
	int bBot = map[x + 1][y];
	int aBot = map[x - 1][y];
	if (lBot == unknown || rBot == unknown || bBot == unknown || aBot == unknown) {
	    return true;
	} else {
	    return false;
	}
    }

    public String globalHeading(String robotHeading, int sensorHeading) {
	// sensor heading points left

	if (sensorHeading == 0) {
	    if (robotHeading == AHEADG) {
		return LEFTG;
	    }
	    if (robotHeading == LEFTG) {
		return BEHINDG;
	    }
	    if (robotHeading == RIGHTG) {
		return AHEADG;
	    }
	    if (robotHeading == BEHINDG) {
		return RIGHTG;
	    }
	}
	// sensor heading points ahead
	if (sensorHeading == 1) {
	    if (robotHeading == AHEADG) {
		return AHEADG;
	    }
	    if (robotHeading == LEFTG) {
		return LEFTG;
	    }
	    if (robotHeading == RIGHTG) {
		return RIGHTG;
	    }
	    if (robotHeading == BEHINDG) {
		return BEHINDG;
	    }
	}
	// sensor heading poitns right
	if (sensorHeading == 2) {
	    if (robotHeading == AHEADG) {
		return RIGHTG;
	    }
	    if (robotHeading == LEFTG) {
		return AHEADG;
	    }
	    if (robotHeading == RIGHTG) {
		return BEHINDG;
	    }
	    if (robotHeading == BEHINDG) {
		return LEFTG;
	    }
	}
	return null;
    }
}
