public class ExchangeInfo
{
	private boolean obstactleDetection = false;
	private int CMD = 1;
	private int CMDIR = 1;
	private String ColorLeft = "";
	private String ColorRight = "";
	
	public ExchangeInfo() {
		
	}
	
	public void setObstacleDectected(boolean status) {
		obstactleDetection = status;
	}
	public boolean getObstacleInfo() {
		return obstactleDetection;
	}
	
	public void setCMD(int command) {
		CMD = command;
	}
	public int getCMD(){
		return CMD;
	}
	
	public void setCMDIR(int command) {
		CMDIR = command;
	}
	public int getCMDIR(){
		return CMDIR;
	}
	
	
	public void setColorLeft(String color) {
		ColorLeft = color;
	}
	public void setColorRight(String color) {
		ColorRight = color;
	}
	public String getColorLeft() {
		return ColorLeft;
	}
	public String getColorRight() {
		return ColorRight;
	}
}
