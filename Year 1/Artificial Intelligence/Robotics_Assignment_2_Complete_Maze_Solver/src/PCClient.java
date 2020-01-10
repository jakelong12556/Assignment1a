import java.io.*;
import java.net.*;
import java.util.Arrays;

/**
 * Maximum LEGO EV3: Building Robots with Java Brains ISBN-13: 9780986832291
 * Variant Press (C) 2014 Chapter 14 - Client-Server Robotics Robot: EV3 Brick
 * Platform: LEGO EV3
 * 
 * @author Brian Bagnall
 * @version July 20, 2014
 */
public class PCClient {

    public static void main(String[] args) throws IOException, ClassNotFoundException {
	String ip = "10.0.1.1"; // BT
	if (args.length > 0)
	    ip = args[0];
	Socket sock = new Socket(ip, EV3Server.port);
	System.out.println("Connected");
	InputStream is = sock.getInputStream();
	ObjectInputStream ois = new ObjectInputStream(is);

	int[][] map;

	while (true) {
	    if ((map = (int[][]) ois.readObject()) != null) {
		map = (int[][]) ois.readObject();
		System.out.println("\n\n");
		System.out.println(Arrays.deepToString(map).replace("], ", "]\n"));
	    }
	}
    }
}
