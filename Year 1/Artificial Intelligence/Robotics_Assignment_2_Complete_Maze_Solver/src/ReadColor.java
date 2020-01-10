import lejos.hardware.lcd.LCD;
import lejos.hardware.port.SensorPort;
import lejos.hardware.sensor.EV3ColorSensor;
import lejos.utility.Delay;

public class ReadColor extends Thread {
    public static final String WHITE = "WHITE";
    public static final String GREEN = "GREEN";
    public static final String RED = "RED";

    private float[] samples1;

    private EV3ColorSensor ColorSensor;
    ExchangeInfo EIObject;

    public ReadColor(ExchangeInfo EI) {
	EIObject = EI;

	ColorSensor = new EV3ColorSensor(SensorPort.S3);

	samples1 = new float[3];

	ColorSensor.setCurrentMode("RGB");
	ColorSensor.getRedMode();

	ColorSensor.fetchSample(samples1, 0);
    }

    public void run() {

	while (true) {
	    String Color1 = getStringColor(ColorSensor, samples1);
	    EIObject.setColor(Color1);
	    LCD.drawString(Color1, 0, 1);
	    Delay.msDelay(20);
	}
    }

    static String getColor(float R, float G, float B) {

	String color = WHITE;
	if (R > 45 && G < 20 && B < 15) {
	    return RED;
	} else if (R < 15 && G > 20 && B < 20) {
	    return GREEN;
	}
	return color;
    }

    public String getStringColor(EV3ColorSensor sensor, float[] samples) {
	sensor.fetchSample(samples, 0);

	float r = samples[0] * 255;
	float g = samples[1] * 255;
	float b = samples[2] * 255;
	
	LCD.drawString("r : " + r + " " + g + " " + b, 0, 2);

	String color = getColor(r, g, b);
	return color;
    }
}
