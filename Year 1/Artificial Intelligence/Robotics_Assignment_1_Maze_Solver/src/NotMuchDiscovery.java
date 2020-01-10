import lejos.hardware.Button;
import lejos.hardware.lcd.LCD;

public class NotMuchDiscovery {

    public static ExchangeInfo EI;
    public static MazeLineFollow MLF;
    public static FindObject FO;
    public static ReadColor RC;
    public static ReadInfared RI;
    
	 public static void main(String[] args) {
		 
		 EI = new ExchangeInfo();
		 MLF = new MazeLineFollow(EI);
		 RC = new ReadColor(EI);
		 FO = new FindObject(EI);
		 RI = new ReadInfared(EI);
		 
		 MLF.start();
		 FO.start();
		 RC.start();
		 RI.start();
		 
		 while(!Button.ESCAPE.isUp()) {
		 }
		 LCD.refresh();
	 }
}
