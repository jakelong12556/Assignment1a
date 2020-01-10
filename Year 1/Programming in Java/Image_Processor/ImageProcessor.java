//package com.bham.pij.exercises.e2a;

import javafx.application.Application;
import javafx.embed.swing.SwingFXUtils;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.image.PixelReader;
import javafx.scene.image.PixelWriter;
import javafx.scene.image.WritableImage;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import java.io.File;
import java.util.ArrayList;
import javax.imageio.ImageIO;


/*
 * DO NOT IMPORT ANYTHING ELSE.
 */

public class ImageProcessor extends Application {

    // You can change these values if you want, to get a smaller or larger Window.
    private static final int STAGE_WIDTH = 400;
    private static final int STAGE_HEIGHT = 400;

    // These are the filters you must implement.
    private static final String[] filterTypes = {"IDENTITY","BLUR", "SHARPEN", "EMBOSS", "EDGE"};

    private Image image;
    private ImageView imgv;
    private VBox vbox;
    private Scene scene;
    private ArrayList<MenuItem> menuItems;
    private String currentFilename;

    public ImageProcessor() {

    }

    /*
     * You must complete the next four methods. You do not need to change
     * any other methods.
     */

    // You must complete this method.
    public Color[][] applyFilter(Color[][] pixels, float[][] filter) {
        final int filterSize = 3;

        Color[][] trimmedImage = new Color [pixels.length - 2][pixels.length - 2];

        float[][] placeHolderRed = new float[filterSize][filterSize];//3x3 color filter for multiplication and addition
        float[][] placeHolderGreen = new float[filterSize][filterSize];
        float[][] placeHolderBlue = new float[filterSize][filterSize];

        float[][] redFilterImage = new float[pixels.length][pixels.length];//float array containing red,green and blue values separately
        float[][] greenFilterImage = new float[pixels.length][pixels.length];
        float[][] blueFilterImage = new float[pixels.length][pixels.length];


        for (int i = 0; i < pixels.length; i++) { //cycle through the whole image
            for (int j = 0; j < pixels.length; j++) {
                Color col = pixels[i][j];//get color values from the pixels

                double red = col.getRed();//separate the red, greeen and blues from it
                double green = col.getGreen();
                double blue = col.getBlue();

                float redF = (float)red;//convert them to floats to avoid incompatibility and lossy compression
                float greenF = (float)green;
                float blueF = (float)blue;

                redFilterImage[i][j] = redF;//construct separate r,g,b array contianing their values
                greenFilterImage[i][j] = greenF;
                blueFilterImage[i][j] = blueF;
            }
        }

        for (int c = 0; c < pixels.length - 2; c++) { //goes through the original size of the image
            for (int d = 0; d < pixels.length - 2; d++) {

                for (int e = 0; e < filterSize; e++) { //goes through the 3x3 matrix of filters, each loop indenting the next element
                    for (int f = 0; f < filterSize; f++) {
                        placeHolderRed[e][f] = redFilterImage[c + e][d + f];//carve a 3x3 matrix of a single color to apply algebra too at the defined point
                        placeHolderGreen[e][f] = greenFilterImage[c + e][d + f];//c and d would be the original point of reference
                        placeHolderBlue[e][f] = blueFilterImage[c + e][d + f];//of which e and f carves the 3x3 matrix to starting at the left corner
                    }
                }
                //3x3 matrix multiplication through a method
                placeHolderRed = timesArrayTogether(placeHolderRed,filter);
                placeHolderGreen = timesArrayTogether(placeHolderGreen,filter);
                placeHolderBlue = timesArrayTogether(placeHolderBlue,filter);

                //adding in all the elements in the array through a method
                float redSum = findSumOfArray(placeHolderRed);
                float greenSum = findSumOfArray(placeHolderGreen);
                float blueSum = findSumOfArray(placeHolderBlue);

                //checking if the floats are under or over 1 and round them to 1 or 0 if needed
                redSum = overOne(redSum);
                greenSum = overOne(greenSum);
                blueSum = overOne(blueSum);

                redSum = underZero(redSum);
                greenSum = underZero(greenSum);
                blueSum = underZero(blueSum);

                Color color = new Color(redSum,greenSum,blueSum,1.0);//construct the new color;
                trimmedImage[c][d] = color;//construct the color array
            }
        }
        return trimmedImage;
    }
    public float findSumOfArray(float[][] filter) {
        float sum = 0.0f;
        final int filterSize = 3;

        for (int c = 0; c < filterSize; c++) {
            for (int d = 0; d < filterSize; d++) {
                sum += filter[c][d];
            }
        }
        return sum;
    }

    public float[][] timesArrayTogether(float[][] singleColor, float[][] filter) {
        final int filterSize = 3;
        float[][] placeHolder = new float[filterSize][filterSize];
        for (int a = 0; a < filterSize; a++) {
            for (int b = 0; b < filterSize; b++) {
                placeHolder[a][b] = singleColor[a][b] * filter[a][b];
            }
        }
        return placeHolder;
    }

    public float overOne(float number) {
        float num = number;
        if (number > 1) {
            num = 1.0f;
        }
        return num;
    }

    public float underZero(float number) {
        float num = number;
        if (number < 0) {
            num = 0.0f;
        }
        return num;
    }
    // You must complete this method.
    public float[][] createFilter(String filterType) {
        if (filterType.equals("IDENTITY")) {
            float[][] filter = {{0,0,0},{0,1,0},{0,0,0}};
            return filter;

        }
        if (filterType.equals("BLUR")) {
            float[][] filter = {{0.0625f,0.125f,0.0625f},{0.125f,0.25f,0.125f},{0.0625f,0.125f,0.0625f}};
            return filter;

        }
        if (filterType.equals("SHARPEN")) {
            float[][] filter = {{0,-1,0},{-1,5,-1},{0,-1,0}};
            return filter;

        }
        if (filterType.equals("EMBOSS")) {
            float[][] filter = {{-2,-1,0},{-1,0,1},{0,1,2}};
            return filter;

        }
        if (filterType.equals("EDGE")) {
            float[][] filter = {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}};
            return filter;

        }
        else {
            return null;
        }
    }

    // You must complete this method.
    public Color[][] applySepia(Color[][] pixels) {
        Color[][] filteredImage = new Color[pixels.length][pixels.length];

        for (int c = 0; c < pixels.length; c++) {
            for (int d = 0; d < pixels.length; d++) {
                Color col = pixels[c][d];
                double red = col.getRed();
                double green = col.getGreen();
                double blue = col.getBlue();

                double sepRed = red * 0.393f + green * 0.769f + blue * 0.189f;
                double sepGreen = red * 0.349f + green * 0.686 + blue * 0.168f;
                double sepBlue = red * 0.272f + green * 0.534f + blue * 0.131f;

                float sepRedF = (float)sepRed;//convert them to floats to avoid incompatibility and lossy compression
                float sepGreenF = (float)sepGreen;
                float sepBlueF = (float)sepBlue;

                sepRedF = overOne(sepRedF);//check if floats is over 1 or under 0
                sepGreenF = overOne(sepGreenF);
                sepBlueF = overOne(sepBlueF);

                sepRedF = underZero(sepRedF);
                sepGreenF = underZero(sepGreenF);
                sepBlueF = underZero(sepBlueF);

                Color sepia = new Color(sepRedF,sepGreenF,sepBlueF,1.0);
                filteredImage[c][d] = sepia;
            }
        }
        return filteredImage;
    }

    // You must complete this method.
    public Color[][] applyGreyscale(Color[][] pixels) {
        Color[][] filteredImage = new Color[pixels.length][pixels.length];

        for (int c = 0; c < pixels.length; c++) {
            for (int d = 0; d < pixels.length; d++) {
                Color col = pixels[c][d];
                double red = col.getRed();
                double green = col.getGreen();
                double blue = col.getBlue();

                double gray = (red + green + blue) / 3;

                Color grayColor = new Color(gray,gray,gray,1.0);
                filteredImage[c][d] = grayColor;
            }
        }
        return filteredImage;

    }

    /*
     *
     * You can ignore the methods below.
     *
     */

    public void filterImage(String filterType) {

        Color[][] pixels = getPixelDataExtended();

        float[][] filter = createFilter(filterType);

        Color[][] filteredImage = applyFilter(pixels, filter);

        WritableImage wimg = new WritableImage(image.getPixelReader(), (int) image.getWidth(), (int) image.getHeight());

        PixelWriter pw = wimg.getPixelWriter();

        for (int i = 0; i < wimg.getHeight(); i++) {
            for (int j = 0; j < wimg.getWidth(); j++) {
                pw.setColor(i, j, filteredImage[i][j]);
            }
        }

        File newFile = new File("filtered_" + filterType + "_" + this.currentFilename);

        try {
            ImageIO.write(SwingFXUtils.fromFXImage(wimg, null), "png", newFile);
        } catch (Exception s) {
        }

        initialiseVBox(false);

        image = wimg;
        imgv = new ImageView(wimg);
        vbox.getChildren().add(imgv);
    }

    private void sepia() {

        Color[][] pixels = getPixelData();

        Color[][] newPixels = applySepia(pixels);

        WritableImage wimg = new WritableImage(image.getPixelReader(), (int) image.getWidth(), (int) image.getHeight());

        PixelWriter pw = wimg.getPixelWriter();

        for (int i = 0; i < wimg.getHeight(); i++) {
            for (int j = 0; j < wimg.getWidth(); j++) {
                pw.setColor(i, j, newPixels[i][j]);
            }
        }

        File newFile = new File("filtered_SEPIA_" + this.currentFilename);

        try {
            ImageIO.write(SwingFXUtils.fromFXImage(wimg, null), "png", newFile);
        } catch (Exception s) {
        }

        initialiseVBox(false);

        image = wimg;
        imgv = new ImageView(wimg);
        vbox.getChildren().add(imgv);
    }

    private void greyscale() {
        Color[][] pixels = getPixelData();

        Color[][] newPixels = applyGreyscale(pixels);

        WritableImage wimg = new WritableImage(image.getPixelReader(), (int) image.getWidth(), (int) image.getHeight());

        PixelWriter pw = wimg.getPixelWriter();

        for (int i = 0; i < wimg.getHeight(); i++) {
            for (int j = 0; j < wimg.getWidth(); j++) {
                pw.setColor(i, j, newPixels[i][j]);
            }
        }

        File newFile = new File("filtered_GREYSCALE_" + this.currentFilename);

        try {
            ImageIO.write(SwingFXUtils.fromFXImage(wimg, null), "png", newFile);
        } catch (Exception s) {
        }

        initialiseVBox(false);

        image = wimg;
        imgv = new ImageView(wimg);
        vbox.getChildren().add(imgv);

    }

    private Color[][] getPixelData() {
        PixelReader pr = image.getPixelReader();
        Color[][] pixels = new Color[(int) image.getWidth()][(int) image.getHeight()];
        for (int i = 0; i < image.getWidth(); i++) {
            for (int j = 0; j < image.getHeight(); j++) {
                pixels[i][j] = pr.getColor(i, j);
            }
        }

        return pixels;
    }

    private Color[][] getPixelDataExtended() {
        PixelReader pr = image.getPixelReader();
        Color[][] pixels = new Color[(int) image.getWidth() + 2][(int) image.getHeight() + 2];

        for (int i = 0; i < pixels.length; i++) {
            for (int j = 0; j < pixels.length; j++) {
                pixels[i][j] = new Color(1.0, 1.0, 1.0, 1.0);
            }
        }

        for (int i = 0; i < image.getWidth(); i++) {
            for (int j = 0; j < image.getHeight(); j++) {
                pixels[i + 1][j + 1] = pr.getColor(i, j);
            }
        }

        return pixels;
    }

    private void initialiseStage(Stage stage) {
        stage.setTitle("Image Processor");
        scene = new Scene(new VBox(), STAGE_WIDTH, STAGE_HEIGHT);
        scene.setFill(Color.OLDLACE);
    }

    @Override
    public void start(Stage stage) {

        initialiseStage(stage);

        initialiseVBox(true);

        createMenuItems();

        enableMenuItem("open");

        createStage(stage);
    }

    private void createStage(Stage stage) {

        Menu menuFile = new Menu("File");

        MenuItem open = getMenuItem("open");

        open.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                FileChooser fileChooser = new FileChooser();
                fileChooser.setTitle("Open Image File");
                File file = fileChooser.showOpenDialog(stage);
                if (file != null) {
                    enableAllMenuItems();
                    disableMenuItem("open");
                    openFile(file);
                }
            }
        });

        menuFile.getItems().add(open);

        MenuItem close = getMenuItem("close");

        close.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                disableMenuItem("close");
                closeFile();
            }
        });

        menuFile.getItems().add(close);

        Menu menuTools = new Menu("Tools");

        MenuItem greyscale = getMenuItem("greyscale");

        greyscale.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                greyscale();
            }
        });

        menuTools.getItems().add(greyscale);

        MenuItem blur = getMenuItem("blur");

        blur.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                filterImage("BLUR");
            }
        });

        menuTools.getItems().add(blur);

        MenuItem sharpen = getMenuItem("sharpen");

        sharpen.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                filterImage("SHARPEN");
            }
        });

        menuTools.getItems().add(sharpen);

        MenuItem edge = getMenuItem("edge");

        edge.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                filterImage("EDGE");
            }
        });

        menuTools.getItems().add(edge);

        MenuItem sepia = getMenuItem("sepia");

        sepia.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                sepia();
            }
        });

        menuTools.getItems().add(sepia);

        MenuItem emboss = getMenuItem("emboss");

        emboss.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                filterImage("EMBOSS");
            }
        });

        menuTools.getItems().add(emboss);

        MenuItem identity = getMenuItem("identity");

        identity.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                filterImage("IDENTITY");
            }
        });

        menuTools.getItems().add(identity);

        MenuItem reset = getMenuItem("reset");

        reset.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent t) {
                reset();
            }
        });

        menuTools.getItems().add(reset);

        MenuBar menuBar = new MenuBar();

        menuBar.getMenus().addAll(menuFile, menuTools);

        ((VBox) scene.getRoot()).getChildren().addAll(menuBar, vbox);

        stage.setScene(scene);

        stage.show();
    }

    protected void reset() {
        initialiseVBox(false);
        openFile(new File(currentFilename));
    }

    private void initialiseVBox(boolean create) {

        final int LEFT = 10;
        final int RIGHT = 10;
        final int TOP = 10;
        final int BOTTOM = 10;


        if (create) {
            vbox = new VBox();
        }
        vbox.getChildren().clear();
        vbox.setAlignment(Pos.CENTER);
        vbox.setPadding(new Insets(LEFT,TOP,RIGHT,BOTTOM));
    }

    private void createMenuItems() {
        menuItems = new ArrayList<MenuItem>();
        menuItems.add(new MenuItem("Open"));
        menuItems.add(new MenuItem("Close"));
        menuItems.add(new MenuItem("Greyscale"));
        menuItems.add(new MenuItem("Blur"));
        menuItems.add(new MenuItem("Sharpen"));
        menuItems.add(new MenuItem("Sepia"));
        menuItems.add(new MenuItem("Emboss"));
        menuItems.add(new MenuItem("Edge"));
        menuItems.add(new MenuItem("Identity"));
        menuItems.add(new MenuItem("Reset"));
        disableAllMenuItems();
    }

    private void disableAllMenuItems() {
        for (MenuItem m: menuItems) {
            m.setDisable(true);
        }
    }

    private void enableAllMenuItems() {
        for (MenuItem m: menuItems) {
            m.setDisable(false);
        }
    }

    private void disableMenuItem(String item) {
        for (MenuItem m: menuItems) {
            if (m.getText().equalsIgnoreCase(item)) {
                m.setDisable(true);
            }
        }
    }

    private void enableMenuItem(String item) {
        for (MenuItem m: menuItems) {
            if (m.getText().equalsIgnoreCase(item)) {
                m.setDisable(false);
            }
        }
    }

    private MenuItem getMenuItem(String name) {
        for (MenuItem m: menuItems) {
            if (m.getText().equalsIgnoreCase(name)) {
                return m;
            }
        }

        return null;
    }

    private void closeFile() {
        enableMenuItem("open");
        initialiseVBox(false);
    }

    private void openFile(File file) {

        image = new Image("file:" + file.getPath());

        if (image.getWidth() != image.getHeight()) {
            Alert alert = new Alert(AlertType.ERROR, "Image is not square.", ButtonType.OK);
            alert.showAndWait();
            return;
        }

        imgv = new ImageView();
        imgv.setImage(image);
        vbox.getChildren().add(imgv);
        currentFilename = file.getName();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
