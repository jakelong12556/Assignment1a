import java.io.*;
import java.net.*;

import lejos.hardware.lcd.LCD;
import lejos.utility.Delay;

/**
 * Maximum LEGO EV3: Building Robots with Java Brains ISBN-13: 9780986832291
 * Variant Press (C) 2014 Chapter 14 - Client-Server Robotics Robot: EV3 Brick
 * Platform: LEGO EV3
 * 
 * @author Brian Bagnall
 * @version July 20, 2014
 */
public class EV3Server extends Thread implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = -4098320408056139688L;
    public static final int port = 1234;
    private ExchangeInfo EIObject;
    private ServerSocket server;
    private ObjectOutputStream oos;

    public EV3Server(ExchangeInfo EI) throws IOException {
	EIObject = EI;
	server = new ServerSocket(port);
	LCD.drawString("waiting client", 0, 1);
	Socket client = server.accept();
	LCD.drawString("Connected client", 0, 1);
	OutputStream out = client.getOutputStream();
	oos = new ObjectOutputStream(out); 
    }

    public void run() {
	while (true) {
	    try {
		if(EIObject.getEnd() == true) {
		    oos.writeObject(null);
		    server.close();
		    break;
		}
		oos.writeObject(EIObject.getMap());
		oos.reset();
		oos.flush();
		Delay.msDelay(25);
	    } catch (IOException e) {
		e.printStackTrace();
	    }
	}
    }
}
