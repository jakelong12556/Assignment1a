import lejos.hardware.BrickFinder;
import lejos.hardware.ev3.EV3;
import lejos.hardware.lcd.LCD;
import lejos.hardware.motor.EV3LargeRegulatedMotor;
import lejos.hardware.port.MotorPort;
import lejos.robotics.chassis.Chassis;
import lejos.robotics.chassis.Wheel;
import lejos.robotics.chassis.WheeledChassis;
import lejos.robotics.navigation.MovePilot;
import lejos.utility.Delay;

/**
 * The Movement sensor controls movements of robot handles movement between
 * boxes in set distances parameter: recieves information from Map to move,
 * movement instructions x1,y1 current and x2,y2, and how to get there return:
 * orientation of robot and x,y coordinate of where it is to map
 */
public class Movement extends Thread {

    public static final String AHEAD = "AHEAD";
    public static final String LEFT = "LEFT";
    public static final String RIGHT = "RIGHT";
    public static final String BEHIND = "BEHIND";

    public static final int Rotate = 90;
    public static final int Entry = 40;
    public static final int Exit = 20;
    public static final int CheckAngle = 2;

    public int headingValue = 1;

    ExchangeInfo EIObject;

    private EV3LargeRegulatedMotor LEFT_MOTOR;
    private EV3LargeRegulatedMotor RIGHT_MOTOR;
    private EV3 ev3brick;
    private Wheel wheel1;
    private Wheel wheel2;
    private MovePilot pilot;
    private Chassis chassis;
    private String headingState;

    public Movement(ExchangeInfo EI) {
	EIObject = EI;

	ev3brick = (EV3) BrickFinder.getLocal();

	LEFT_MOTOR = new EV3LargeRegulatedMotor(MotorPort.A);
	RIGHT_MOTOR = new EV3LargeRegulatedMotor(MotorPort.B);

	wheel1 = WheeledChassis.modelWheel(LEFT_MOTOR, 5.6f).offset(-6.0f);
	wheel2 = WheeledChassis.modelWheel(RIGHT_MOTOR, 5.6f).offset(6.0f);

	chassis = new WheeledChassis(new Wheel[] { wheel1, wheel2 }, WheeledChassis.TYPE_DIFFERENTIAL);

	pilot = new MovePilot(chassis);
	pilot.setLinearSpeed(10);
	pilot.setAngularSpeed(15);

	setHeading();
    }

    public void run() {
	while (true) {
	    LCD.drawString("heading: " + headingState, 0, 3);
	    if (EIObject.getCommandDirection() != 4) {
		int commandMove = EIObject.getCommandDirection();
		LCD.drawString("move: " + commandMove, 0, 4);
		if (commandMove == 6) {
		    rotateLeft();
		    rotateLeft();
		} else {
		    setMovement(commandMove);
		    LCD.drawString("move: " + commandMove, 0, 4);
		}
		EIObject.setDirection(4);
	    }
	    Delay.msDelay(25);
	}
    }

    public void setMovement(int commandMove) {
	if (commandMove == 0) {// move left
	    if (headingState == AHEAD) {
		LCD.drawString("g:left / h:ahead", 0, 5);
		rotateLeft();
		Delay.msDelay(10);
	    }
	    if (headingState == RIGHT) {
		LCD.drawString("g:left / h:right", 0, 5);
		rotateLeft();
		rotateLeft();
		Delay.msDelay(10);
	    }
	    if (headingState == BEHIND) {
		LCD.drawString("g:left / h:behind", 0, 5);
		rotateRight();
		Delay.msDelay(10);
	    }
	    travelAndDelay();
	}
	if (commandMove == 1) {// move ahead
	    if (headingState == LEFT) {
		LCD.drawString("g:ahead / h:left", 0, 5);
		rotateRight();
		Delay.msDelay(10);
	    }
	    if (headingState == RIGHT) {
		LCD.drawString("g:ahead / h:right", 0, 5);
		rotateLeft();
		Delay.msDelay(10);
	    }
	    if (headingState == BEHIND) {
		LCD.drawString("g:ahead / h:behind", 0, 5);
		rotateLeft();
		rotateLeft();
		Delay.msDelay(10);
	    }
	    travelAndDelay();
	    Delay.msDelay(10);
	}
	if (commandMove == 2) {// move right
	    if (headingState == AHEAD) {
		LCD.drawString("g:right / h:ahead", 0, 5);
		rotateRight();
		Delay.msDelay(10);
	    }
	    if (headingState == LEFT) {
		LCD.drawString("g:right / h:left", 0, 5);
		rotateRight();
		rotateRight();
		Delay.msDelay(10);
	    }
	    if (headingState == BEHIND) {
		LCD.drawString("g:right / h:behind", 0, 5);
		rotateLeft();
		Delay.msDelay(10);
	    }
	    travelAndDelay();
	    Delay.msDelay(10);

	}
	if (commandMove == 3) {// move behind
	    if (headingState == AHEAD) {
		LCD.drawString("g:behind / h:ahead", 0, 5);
		rotateLeft();
		rotateLeft();
		Delay.msDelay(10);
	    }
	    if (headingState == RIGHT) {
		LCD.drawString("g:behind / h:right", 0, 5);
		rotateRight();
		Delay.msDelay(10);
	    }
	    if (headingState == LEFT) {
		LCD.drawString("g:behind / h:left", 0, 5);
		rotateLeft();
		Delay.msDelay(10);
	    }
	    travelAndDelay();
	    Delay.msDelay(10);

	}

	if (commandMove == 5) {
	    LCD.drawString("g:green / h:green", 0, 5);
	    detectGreen();
	    Delay.msDelay(10);
	}
	if (commandMove == 6) {
	    LCD.drawString("g:red / h:red", 0, 5);
	    detectRed();
	    Delay.msDelay(10);
	}
	if (commandMove == 7) {
	    pilot.stop();
	    Delay.msDelay(10);
	}

    }

    public void rotateLeft() {
	pilot.rotate(-Rotate);
	Delay.msDelay(25);
	if (headingValue != 0) {
	    headingValue--;
	} else {
	    headingValue = 3;
	}
	setHeading();
	EIObject.setOriRobot(headingState);
    }

    public void rotateRight() {
	pilot.rotate(Rotate);
	Delay.msDelay(25);
	if (headingValue != 3) {
	    headingValue++;
	} else {
	    headingValue = 0;
	}
	setHeading();
	EIObject.setOriRobot(headingState);
    }

    public void setHeading() {
	if (headingValue == 0) {
	    headingState = LEFT;
	}
	if (headingValue == 1) {
	    headingState = AHEAD;
	}
	if (headingValue == 2) {
	    headingState = RIGHT;
	}
	if (headingValue == 3) {
	    headingState = BEHIND;
	}
    }

    public void travelAndDelay() {
	pilot.travel(Entry);
	Delay.msDelay(25);
    }

    public void detectGreen() {
	pilot.travel(-Entry);
	rotateLeft();
	rotateLeft();
	Delay.msDelay(25);
    }
    public void detectRed() {
	pilot.travel(-Entry);
	rotateLeft();
	rotateLeft();
	Delay.msDelay(25);
    }
}