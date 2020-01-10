import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.swing.JApplet;


public class testcolor extends JApplet {
    /**
	 * 
	 */
	private Color white = Color.WHITE;
	private static final long serialVersionUID = 1L;
	Graphics g;

	public void paint() {
        Image i;
		try
		{
			i = ImageIO.read(new URL(getCodeBase(), "test.bmp"));
	        g.drawImage(i,0,0,WIDTH,HEIGHT,white,null);

		}
		catch (MalformedURLException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }
	
	public static void main(String[] args)
	{
		testcolor m = new testcolor();
		m.paint();
	}
}