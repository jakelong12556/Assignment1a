import java.io.IOException;

import lejos.hardware.BrickFinder;
import lejos.hardware.Button;
import lejos.hardware.Keys;
import lejos.hardware.ev3.EV3;
import lejos.hardware.lcd.LCD;

public class NotMuchDiscovery {

    public static ExchangeInfo EI;
    public static Movement MLF;
    public static SonicSensor SS;
    public static ReadColor RC;
    public static DiscLineMap DLM;
    public static EV3Server E3S;

    public static void main(String[] args) throws IOException {

	EI = new ExchangeInfo();
	DLM = new DiscLineMap(EI);
	SS = new SonicSensor(EI);
	MLF = new Movement(EI);
	RC = new ReadColor(EI);
	E3S = new EV3Server(EI);
		
	EV3 ev3brick = (EV3) BrickFinder.getLocal();
	Keys buttons = ev3brick.getKeys();

	LCD.drawString("Press anything", 0, 0);

	buttons.waitForAnyPress();
	LCD.clearDisplay();
	
	E3S.start();
	SS.start();	
	DLM.start();	
	MLF.start();	
	RC.start();

	while (!Button.ESCAPE.isUp()) {
	}
	LCD.refresh();
    }
}
