import lejos.hardware.lcd.LCD;
import lejos.hardware.port.SensorPort;
import lejos.hardware.sensor.EV3ColorSensor;
import lejos.utility.Delay;

public class ReadColor extends Thread
{
	public static final String BLACK = "BLACK";
	public static final String YELLOW = "YELLOW";
	public static final String GREEN = "GREEN";
	public static final String RED = "RED";
	public static final String WHITE = "WHITE";
	
    private float[] samples1;
    private float[] samples2;

	private EV3ColorSensor LEFT_ColorSensor;
	private EV3ColorSensor RIGHT_ColorSensor;
	ExchangeInfo EIObject;
	
	public ReadColor(ExchangeInfo EI) {
		EIObject = EI;

		LEFT_ColorSensor = new EV3ColorSensor(SensorPort.S3);
        RIGHT_ColorSensor = new EV3ColorSensor(SensorPort.S4);

        samples1 = new float[3];
        samples2 = new float[3];
        
        LEFT_ColorSensor.setCurrentMode("RGB");
        RIGHT_ColorSensor.setCurrentMode("RGB");
        LEFT_ColorSensor.getRedMode();
        RIGHT_ColorSensor.getRedMode();
        
        LEFT_ColorSensor.fetchSample(samples1, 0);
        RIGHT_ColorSensor.fetchSample(samples2, 0);
	}
	
	public void run() {

		while(true) {        
            String Color1 = getStringColor(LEFT_ColorSensor, samples1);
            String Color2 = getStringColor(RIGHT_ColorSensor, samples2);
            
            LCD.drawString("LColor: " + Color1, 0, 2);
	        LCD.drawString("RColor: " + Color2, 0, 3);
            
            EIObject.setColorLeft(Color1);
            EIObject.setColorRight(Color2);
            
            Delay.msDelay(20);
		}
	}
	
	static String getColor (float R, float G, float B){
		
		 String color = WHITE;
		 if (R > 40 && G > 40 && B > 40) {
			 return WHITE;
		 } else if (R > 35 && G < 10 && B < 10) {
			 return RED;
		 } else  if (R < 10 && G > 15 && B < 15) {
           return GREEN;
       } if (R < 10 && G < 10 && B < 10) {
           return BLACK;
       }
		return color; 	 
	 }
	
	public String getStringColor(EV3ColorSensor sensor, float[] samples) {
		sensor.fetchSample(samples, 0);
		
	    float r = samples[0]*255;
        float g = samples[1]*255;
        float b = samples[2]*255;
        
        String color = getColor(r,g,b);
        return color;
	}
}
