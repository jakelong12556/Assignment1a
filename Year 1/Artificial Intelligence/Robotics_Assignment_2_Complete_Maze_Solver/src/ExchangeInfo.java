
public class ExchangeInfo {
    private String PoseRobot = "AHEAD";
    private int goDirection = 4;
    private float distance = 0.0f;
    private String Color = "WHITE";
    private int[][] map;
    private int[] wallState;
    private boolean callSonic = false;
    private boolean isWall = false;
    private boolean complete = false;

    public ExchangeInfo() {

    }

    // Methods for Sonic sensor

    public void setSonicWall(boolean command) {
	isWall = command;
    }

    public boolean getSonicWall() {
	return isWall;
    }

    public void callSonic(boolean command) {
	callSonic = command;
    }

    public boolean getSonic() {
	return callSonic;
    }

    public void setUltraInfo(int[] info) {
	wallState = info;
    }

    public int[] getUltraInfo() {
	return wallState;
    }
    
    public void setDistance(float info) {
	distance = info;
    }

    public float getDist() {
	return distance;
    }

    // Methods for color sensor

    public void setColor(String color) {
	Color = color;
    }

    public String getColor() {
	return Color;
    }

    // Methods for Movement

    public void setOriRobot(String pose) {
	PoseRobot = pose;
    }

    public String getOriRobot() {
	return PoseRobot;
    }

    public void setDirection(int direction) {
	goDirection = direction;
    }

    public int getCommandDirection() {
	return goDirection;
    }

    // Methods for Mapping

    public void setMap(int[][] maps) {
	map = maps;
    }

    public int[][] getMap() {
	return map;
    }
    
    public void setEnd(boolean x) {
 	complete = x;
     }

     public boolean getEnd() {
 	return complete;
     }
}
