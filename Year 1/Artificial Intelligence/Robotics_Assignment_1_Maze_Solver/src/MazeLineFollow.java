import lejos.hardware.BrickFinder;
import lejos.hardware.Keys;
import lejos.hardware.ev3.EV3;
import lejos.hardware.lcd.LCD;
import lejos.hardware.lcd.TextLCD;
import lejos.hardware.motor.EV3LargeRegulatedMotor;
import lejos.hardware.port.MotorPort;
import lejos.robotics.chassis.Chassis;
import lejos.robotics.chassis.Wheel;
import lejos.robotics.chassis.WheeledChassis;
import lejos.robotics.navigation.MovePilot;
import lejos.utility.Delay;

public class MazeLineFollow extends Thread
{
	public static final String		BLACK		= "BLACK";
	public static final String		YELLOW		= "YELLOW";
	public static final String		GREEN		= "GREEN";
	public static final String		RED			= "RED";
	public static final String		WHITE		= "WHITE";

	public static final int			Rotate		= 90;
	public static final int			Entry		= 9;
	public static final int			Exit		= 5;
	public static final int			CheckAngle	= 2;

	ExchangeInfo					EIObject;

	private EV3LargeRegulatedMotor	LEFT_MOTOR;
	private EV3LargeRegulatedMotor	RIGHT_MOTOR;
	private EV3						ev3brick;
	private Keys					buttons;
	private Wheel					wheel1;
	private Wheel					wheel2;
	private MovePilot				pilot;
	private Chassis					chassis;

	public MazeLineFollow(ExchangeInfo EI)
	{
		EIObject = EI;

		ev3brick = (EV3) BrickFinder.getLocal();

		LEFT_MOTOR = new EV3LargeRegulatedMotor(MotorPort.A);
		RIGHT_MOTOR = new EV3LargeRegulatedMotor(MotorPort.B);

		buttons = ev3brick.getKeys();

		wheel1 = WheeledChassis.modelWheel(LEFT_MOTOR, 5.6f).offset(-6.35f);
		wheel2 = WheeledChassis.modelWheel(RIGHT_MOTOR, 5.6f).offset(6.35f);

		chassis = new WheeledChassis(new Wheel[] { wheel1, wheel2 }, WheeledChassis.TYPE_DIFFERENTIAL);

		pilot = new MovePilot(chassis);
		pilot.setLinearSpeed(5);
		pilot.setAngularSpeed(15);
	}

	public void run()
	{

		LCD.drawString("Press anything", 0, 0);
		buttons.waitForAnyPress();
		LCD.clearDisplay();

		TextLCD lcdDisplay = ev3brick.getTextLCD();
		int counter = 0;

		while (true)
		{

			moveForward();

			if (EIObject.getCMD() == 1)
			{
				if (EIObject.getColorLeft() == WHITE && EIObject.getColorRight() == WHITE)
				{
					counter++;
					lcdDisplay.drawString("Count: " + counter, 0, 0);

					Delay.msDelay(25);
				}
				if (counter > 250)
				{
					regainLine();
					counter = 0;
				}

				if (EIObject.getColorLeft() != WHITE || EIObject.getColorRight() != WHITE)
				{
					counter = 0;
				}

				if (EIObject.getColorLeft() == BLACK || EIObject.getColorRight() == BLACK)
				{
					if (EIObject.getColorLeft() == BLACK && EIObject.getColorRight() == BLACK)
					{
						travelAndDelay(10);
					}
					else
					{
						if (EIObject.getColorLeft() == BLACK)
						{
							pilot.rotate(-10);
							Delay.msDelay(25);
						}
						else
						{
							pilot.rotate(10);
							Delay.msDelay(25);
						}
					}
				}

				while ((EIObject.getColorLeft() == GREEN || EIObject.getColorRight() == GREEN))
				{
					pilot.travel(1);
					if (EIObject.getColorLeft() == GREEN && EIObject.getColorRight() == GREEN)
					{
						pilot.rotate(185);
					}
					else
					{
						if (EIObject.getColorLeft() == GREEN)
						{
							if (wiggleColorCheckLeft(CheckAngle, GREEN) == true)
							{
								continue;
							}
							rotateAndMove(Entry, -Rotate, Exit);

						}
						else
						{
							if (wiggleColorCheckRight(CheckAngle, GREEN) == true)
							{
								continue;
							}
							rotateAndMove(Entry, Rotate, Exit);
						}
					}
				}

				if ((EIObject.getColorLeft() == RED || EIObject.getColorRight() == RED))
				{
					pilot.travel(1);
					if (EIObject.getColorLeft() == RED && EIObject.getColorRight() == RED)
					{
						pilot.stop();
						break;
					}
					else
					{
						if (EIObject.getColorLeft() == RED)
						{
							if (wiggleColorCheckLeft(CheckAngle, RED) == true)
							{
								continue;
							}
							travelAndDelay(7);
						}
						else
						{
							if (wiggleColorCheckRight(CheckAngle, RED) == true)
							{
								continue;
							}
							travelAndDelay(7);
						}
					}
				}
				lcdDisplay.clear();
				Delay.msDelay(10);
			}
			else
			{

				lcdDisplay.drawString("Object detected", 0, 6);
				avoidObject();
			}
		}
	}

	public void avoidObject()
	{
		pilot.rotate(90);

		while (EIObject.getCMDIR() == 1)
		{
			LCD.drawString("Object IR Found", 0, 7);
			moveForward();
			LCD.refresh();
		}

		LCD.drawString("Object IR Lost", 0, 7);

		pilot.travel(15);
		pilot.rotate(-90);

		while (EIObject.getCMDIR() == 0)
		{
			LEFT_MOTOR.forward();
			RIGHT_MOTOR.forward();
			LCD.refresh();
		}

		LCD.drawString("Object IR regained", 0, 7);

		while (EIObject.getCMDIR() == 1)
		{
			moveForward();
			LCD.refresh();
		}

		LCD.drawString("Object IR crossed", 0, 7);

		pilot.travel(18);
		pilot.rotate(-90);

		while (EIObject.getColorLeft() != BLACK || EIObject.getColorRight() != BLACK)
		{
			moveForward();
		}

		pilot.travel(7);
		pilot.rotate(90);

	}

	public void regainLine()
	{
		int i = 0;
		int angle = 5;
		while (EIObject.getColorLeft() == WHITE && EIObject.getColorRight() == WHITE)
		{
			if (i % 2 == 0)
			{
				pilot.rotate(-angle);
				pilot.travel(3);
				angle = 10;
				i++;
			}
			else
			{
				pilot.rotate(angle);
				pilot.travel(3);
				i++;
			}
		}
	}

	public void moveForward()
	{
		LEFT_MOTOR.setSpeed(120);
		RIGHT_MOTOR.setSpeed(120);
		LEFT_MOTOR.forward();
		RIGHT_MOTOR.forward();
	}

	public void rotateAndMove(int travelEnter, int rotateDegs, int travelExit)
	{
		pilot.travel(travelEnter);
		pilot.rotate(rotateDegs);
		pilot.travel(travelExit);
		Delay.msDelay(25);
	}

	public void travelAndDelay(int travel)
	{
		pilot.travel(travel);
		Delay.msDelay(25);

	}

	public boolean wiggleColorCheckRight(int wiggleAngle, String colorName)
	{
		for (int i = 0; i < 5; i++)
		{
			pilot.rotate(wiggleAngle);
			Delay.msDelay(50);
			if (EIObject.getColorLeft() == colorName)
			{
				return true;
			}
		}
		pilot.rotate(-wiggleAngle * 5);
		return false;
	}
	

	public boolean wiggleColorCheckLeft(int wiggleAngle, String colorName)
	{
		for (int i = 0; i < 5; i++)
		{
			pilot.rotate(-wiggleAngle);
			Delay.msDelay(50);
			if (EIObject.getColorRight() == colorName)
			{
				return true;
			}
		}
		pilot.rotate(wiggleAngle * 5);
		return false;
	}
}
