package gotcha_pkg;

import java.util.ArrayList;

import processing.core.PApplet;
import processing.core.PImage;   // For background image


public class Gotcha_Multi extends PApplet {

    // Keep track of current score
    int score = 0;

    // Canvas size
    final int canvasWidth  = 500;
    final int canvasHeight = 500;

    // Declare disk
    static Disk d1;
    static Disk d2;
    static Disk d3;
    static Disk d4;
    static ArrayList<Disk> alldisks = new ArrayList<Disk>();
    
    int totalTime = 20;
    int savedTime;
    
    public static void main(String[] args){
        PApplet.main("gotcha_pkg.Gotcha_Multi");
        alldisks.add(d1);
        alldisks.add(d2);
        alldisks.add(d3);
        alldisks.add(d4);
    }
    
    public void settings() {
        size(canvasWidth, canvasHeight);
        smooth();
    }

    // setup() runs one time at the beginning of your program
    public void setup() {
        // Create a disk
        d1 = new Disk(0, 0, 255, 0, 100, 6, 30, 30);
        d2 = new Disk(255, 0, 0, 0, 200, 8, 60, 60);
        d3 = new Disk(0, 255, 0, 0, 300, 10, 40, 40);
        d4 = new Disk(0, 0, 200, 0, 400, 7, 70, 70);
        
        savedTime = second();
    }

    // draw() is called repeatedly in an infinite loop.
    // By default, it is called about 60 times per second.
    public void draw() {
        // Erase the background, if you don't, the previous shape(s) will 
        // still be displayed
        eraseBackground();

        // Move the shape, i.e. calculate the next x and y position
        // where the shape will be drawn.
        d1.calcCoords(); 
        d2.calcCoords(); 
        d3.calcCoords(); 
        d4.calcCoords(); 

        // Draw the shape
        d1.drawShape();
        d2.drawShape(); 
        d3.drawShape(); 
        d4.drawShape(); 

        // Display point value on the shape
        d1.displayPointValue();
        d2.displayPointValue(); 
        d3.displayPointValue();
        d4.displayPointValue();


        // Display player's score 
        fill(0, 102, 154);
        text("Score: " + this.score, 420, 50);
        
        //timer
        int passedTime = second()-savedTime;
        fill(0, 102, 154);
        text("Time: " + passedTime, 420, 100);

        if (passedTime > totalTime) {
        		background(255);
        		fill(0, 102, 154);
            text("Thanks for playing!", 150, 200);
            text("Score: " + this.score, 420, 50);

        }
    }

    public void eraseBackground() {      
        // White background:
        background(255);
        
        /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           | Use the following lines to display an image in the background. |
           | You will need to bring a .png file into your package. The path |
           | given below should be replaced by your path and png file name. |
          */                                                                
           PImage bg = loadImage("img/dogingrasscopy.png");
           background(bg);                                              
    }

    // mousePressed() is a PApplet method that you will override.
    // This method is called from PApplet one time when the mouse is pressed.
    public void mousePressed() {
        // Draw a circle wherever the mouse is
        int mouseWidth  = 20;
        int mouseHeight = 20;
        fill(0, 255, 0);
        ellipse(mouseX, mouseY, mouseWidth, mouseHeight);
        
        // Check whether the click occurred within range of the shape
        	if ((this.mouseX >= d1.x - d1.targetRange) && (this.mouseX <= d1.x + d1.targetRange) && 
        		(this.mouseY >= d1.y - d1.targetRange) && (this.mouseY <= d1.y + d1.targetRange)) {
            
        			// Update score:
        		score = score + d1.pointValue;
        		System.out.println("DBG:  HIT!");
        }
        	if ((this.mouseX >= d2.x - d2.targetRange) && (this.mouseX <= d2.x + d2.targetRange) && 
            		(this.mouseY >= d2.y - d2.targetRange) && (this.mouseY <= d2.y + d2.targetRange)) {
                
            			// Update score:
            		score = score + d2.pointValue;
            		System.out.println("DBG:  HIT!");
            }
    
        	if ((this.mouseX >= d3.x - d3.targetRange) && (this.mouseX <= d3.x + d3.targetRange) && 
            		(this.mouseY >= d3.y - d3.targetRange) && (this.mouseY <= d3.y + d3.targetRange)) {
                
            			// Update score:
            		score = score + d3.pointValue;
            		System.out.println("DBG:  HIT!");
            }
        	if ((this.mouseX >= d4.x - d4.targetRange) && (this.mouseX <= d4.x + d4.targetRange) && 
            		(this.mouseY >= d4.y - d4.targetRange) && (this.mouseY <= d4.y + d4.targetRange)) {
                
            			// Update score:
            		score = score + d3.pointValue;
            		System.out.println("DBG:  HIT!");
            }
    }

    // Create a Disk class that you will use to create one or more disks with each
    // disk having a color, speed, position, etc.
    class Disk {
        // Size of disk
    		final int shapeWidth;
        final int shapeHeight;

        // Point value of disk
        int pointValue;

        // Position of disk - keep track of x and y position of disk
        float x;
        float y;

        // Horizontal speed of disk
        float xSpeed;

        // It's hard to click a precise pixel on a disk, to make it easier we can
        // allow the user to click somewhere on the disk.
        // You might want to make the scoring space be a rectangle fitted tightly
        // to the disk - it's easier than calculating a rounded boundary.
        int targetRange = 20;

        float red;
        float green;
        float blue;

        // The constructor could be extended to accept other disk characteristics
        Disk(float red, float green, float blue, float x, float y, float xSpeed, int shapeWidth, int shapeHeight) {
            this.red   = red;
            this.green = green;
            this.blue  = blue;
            pointValue = (int) (Math.random()*100);
            this.x = x;
            this.y=y;
            this.xSpeed = xSpeed;
            this.shapeHeight = shapeHeight;
            this.shapeWidth = shapeWidth;
            
        }

        public void calcCoords() {      
            // Compute the x position where the shape will be drawn
            this.x += xSpeed;

            // If the x position is off right side of the canvas, reverse direction of 
            // movement:
            if (this.x > 500) {
                // Log a debug message:
                System.out.println("DBG: <--- Change direction, go left because x = " + this.x);

                // Recalculate:
                this.xSpeed = -1 * this.xSpeed;
            }

            // If the x position is off left side of the canvas, reverse direction of 
            // movement:
            if (this.x < 0) {
                // Log a debug message:
                System.out.println("DBG: ---> Change direction, go right because x = " + this.x + "\n");
               
                // Recalculate:
                this.xSpeed = -1 * this.xSpeed;
            } 
        }

        public void drawShape() {
            // Select color, then draw the shape at computed x, y location
            fill(red, green, blue);
            ellipse(x, y, shapeWidth, shapeHeight);
        }

        public void displayPointValue() {
            // Draw the text at computed x, y location
            this.pointValue = pointValue;
            textSize(20);
            fill(255, 255, 255);
            textAlign(CENTER);
            text(pointValue, x, y);
        }
    }
}
