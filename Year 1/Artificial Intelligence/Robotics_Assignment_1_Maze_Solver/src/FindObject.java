import lejos.hardware.lcd.LCD;
import lejos.hardware.port.SensorPort;
import lejos.hardware.sensor.EV3UltrasonicSensor;
import lejos.robotics.SampleProvider;

public class FindObject extends Thread
{
	private ExchangeInfo EIObject;
	
    private float[] distanceSamples;
	private	EV3UltrasonicSensor sonicSensor;
	private SampleProvider sonicDistance;
    final float distanceValue = 0.1f;
	
	public FindObject(ExchangeInfo EI) {
		EIObject = EI;
        sonicSensor = new EV3UltrasonicSensor(SensorPort.S2);
        sonicDistance = sonicSensor.getDistanceMode();

	}
	public void run() {
		while(true) {	        
			distanceSamples = new float[sonicDistance.sampleSize()];
			sonicDistance.fetchSample(distanceSamples, 0);
			float distanceValue2 = distanceSamples[0];
						
			LCD.drawString("Dist: " + distanceValue2, 0, 5);

			if(distanceValue2 < distanceValue) {
				EIObject.setCMD(0);
			} else {
				EIObject.setCMD(1);
			}
			LCD.refresh();
		}
	}
}
