import lejos.hardware.lcd.LCD;
import lejos.hardware.port.SensorPort;
import lejos.hardware.sensor.EV3IRSensor;
import lejos.robotics.SampleProvider;

public class ReadInfared extends Thread 
{
	private ExchangeInfo EIObject;
	private static EV3IRSensor ir1;
	private SampleProvider infaredDist;
    final float distanceValue = 35;
	
	public ReadInfared(ExchangeInfo EI) {
		EIObject = EI;
		ir1 = new EV3IRSensor(SensorPort.S1);
		infaredDist = ir1.getDistanceMode();
	}
	
	public void run() {
		while(true) {
			float [] sample = new float[infaredDist.sampleSize()];
			infaredDist.fetchSample(sample, 0);
		    float distanceValue2 = sample[0];

			LCD.drawString("DistIR: " + distanceValue2, 0, 6);

			
			if(distanceValue2 < distanceValue) {
				EIObject.setCMDIR(1);
			} else {
				EIObject.setCMDIR(0);
			}
			LCD.refresh();
		}
	}
}
