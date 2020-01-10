import lejos.hardware.lcd.LCD;
import lejos.hardware.motor.EV3MediumRegulatedMotor;
import lejos.hardware.port.MotorPort;
import lejos.hardware.port.SensorPort;
import lejos.hardware.sensor.EV3IRSensor;
import lejos.robotics.SampleProvider;
import lejos.utility.Delay;

/**
 * The sonic sensor object is mounted on a medium rotor, rotated in 90 degs
 * increments parameter: recieves callSonic to begin scan from left to right
 * returns: if wall is detected, and orientation of sensor return to left
 * orientation and return callsonic to false
 */
public class SonicSensor extends Thread {

    private ExchangeInfo EIObject;

    private float[] distanceSamples;
    private EV3MediumRegulatedMotor MEDIUM_MOTOR;

    final float distanceValue = 33.0f;
    public static final String AHEAD = "AHEAD";
    public static final String LEFT = "LEFT";
    public static final String RIGHT = "RIGHT";

    public int headingValue = 0;
    private int[] wallState;

    private static EV3IRSensor ir1;
    private SampleProvider infaredDist;

    public SonicSensor(ExchangeInfo EI) {
	EIObject = EI;

	ir1 = new EV3IRSensor(SensorPort.S4);
	infaredDist = ir1.getDistanceMode();
	MEDIUM_MOTOR = new EV3MediumRegulatedMotor(MotorPort.D);
	MEDIUM_MOTOR.setSpeed(500);

	wallState = new int[3];

	for (int i = 0; i < wallState.length; i++) {
	    wallState[i] = 0;
	}
    }

    public void run() {
	while (true) {
	    if (EIObject.getSonic() == true) {
		for (int i = 0; i < wallState.length; i++) {
		    headingValue = i;

		    distanceSamples = new float[infaredDist.sampleSize()];
		    for (int n = 0; n < 5; n++) {
			infaredDist.fetchSample(distanceSamples, 0);
			Delay.msDelay(100);
		    }
		    float distanceValue2 = distanceSamples[0];
		    LCD.drawString("dist at :" + distanceValue2, 0, 0);
		    LCD.refresh();
		    EIObject.setDistance(distanceValue2);

		    if (distanceValue2 < distanceValue) {
			wallState[i] = 0;
		    } else {
			wallState[i] = 1;
		    }

		    if (i < 2) {
			MEDIUM_MOTOR.rotate(90);
			Delay.msDelay(200);
		    }
		    Delay.msDelay(10);
		}

		MEDIUM_MOTOR.rotate(-180);
		EIObject.setUltraInfo(wallState);
		EIObject.callSonic(false);
	    }
	    Delay.msDelay(25);
	}
    }
}
