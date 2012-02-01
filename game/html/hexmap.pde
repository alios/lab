
Map m;
PVector dragvector = null;

void setup() {
    size(800, 600, P3D);
    println("h = " + Hexagon.h);
    println("r = " + Hexagon.r);
    println("a = " + Hexagon.a);
    println("b = " + Hexagon.b);

    m = new Map(100,100);
    noLoop();
    redraw();
}


void mouseClicked() {
    switch (mouseButton) {
    case 37:
	println(m.point2coord(mouseX, mouseY));
	break;
    default:	
	println("unhandled mousePressed(): " + mouseButton);
	break;
    }

}

void mousePressed() {
    switch (mouseButton) {
    case 37: // left click
	dragvector = new PVector(mouseX, mouseY);
	break;
	
    default:	
	println("unhandled mousePressed(): " + mouseButton);
	break;
    }
}

void mouseReleased() {
    switch (mouseButton) {
    case 37: // left click
	if (dragvector != null) {
	    PVector v = new PVector(mouseX, mouseY);
	    v.sub(dragvector);
	    v.div(m.ms);
	    m.scroll(v.x, v.y);
	}
	dragvector = null;
	break;

    default:	
	println("unhandled mouseReleased(): " + mouseButton);
	break;
    }
}


void keyTyped() {
    switch (int(key)) {
    case 43: // +
	m.incZoom();
	break;
    case 45: // -
	m.decZoom();
	break;
    case 97: // a
	m.scrollLeft();
	break;
    case 100: // d
	m.scrollRight();
	break;
    case 119: // w
	m.scrollUp();
	break;
    case 115: // s
	m.scrollDown();
	break;
    default:
	println("unhandled keyTyped(): " + int(key));
	break;
    }
}

void draw() {
    background(153);
    m.display();
}

class Map
{
    ArrayList hexagons;
    double mx = 0;
    double my = 0;
    double ms = 16;
    

    Map(int nx, int ny) {
	hexagons = new ArrayList();
	for (int x = 0; x < nx; ++x) {
	    for (int y = 0; y < ny; ++y) {
		hexagons.add(new Hexagon(x,y));
	    }
	}   
    }

    
    PVector point2coord (float x, float y) {
		
	double xpixel = (x/ms) - mx;
	double ypixel = (y/ms) - my;
	
	int xsection = int(xpixel / ( 2 * Hexagon.r ));
	int ysection = int(ypixel / ( Hexagon.h + 1 ));
       
	double xsectpxl = xpixel - xsection * ( 2 * Hexagon.r );
	double ysectpxl = ypixel - ysection * ( Hexagon.h + 1 );
	
	PVector retVal = new PVector(xsection, ysection);
	double m = Hexagon.h / Hexagon.r;

	if ((ysection % 2) == 0) {
	    if (ysectpxl < (Hexagon.h - xsectpxl * m)) {
		retVal = new PVector(retVal.x -1 , retVal.y - 1);
	    }
	    
	    if (ysectpxl < (- Hexagon.h + xsectpxl * m)) {
		retVal = new PVector(retVal.x , retVal.y - 1);
	    }
	} else {
	    if (xsectpxl >=  Hexagon.r) {
		if (ysectpxl < (2 * Hexagon.h - xsectpxl * m)) {
		    retVal = new PVector(retVal.x , retVal.y - 1);
		} 
	    }

	    if(xsectpxl < Hexagon.r) {
		if (ysectpxl < (xsectpxl * m)) {
		    retVal = new PVector(retVal.x, retVal.y - 1);
		} else {
		    retVal = new PVector(retVal.x -1 , retVal.y);
		}
	    }
	}
	
	return retVal;
    }
    
    
    void scroll(double x, double y) {
	mx += x;
	my += y;
	redraw();
    }

    void incZoom() {
	ms *= 2;
	redraw();
    }

    void decZoom() {
	ms /= 2;
	redraw();
    }

    void scrollLeft() {
	mx -= 4 * Hexagon.a;
	redraw();
    }

    void scrollRight() {
	mx += 4 * Hexagon.a;
	redraw();
    }

    void scrollUp() {
	my -= 4 * Hexagon.b;
	redraw();
    }

    void scrollDown() {
	my += 4 * Hexagon.b;
	redraw();
    }
    
    
    void display() {
	pushMatrix();	
	// do zoom
	scale(ms);

	// translate
	translate(mx, my);
	
       	mmin = point2coord(0,0);
	mmax = point2coord(width, height);
	
	// draw hexagon
	int c = 0;
       	for (int i = 0; i <  hexagons.size(); ++i) {
	    Hexagon h = (Hexagon) hexagons.get(i);	    
	    if ((h.hx >= mmin.x) && (h.hx <= mmax.x) &&
		(h.hy >= mmin.y) && (h.hy <= mmax.y)) {
		h.display();		
		c++
	    } 
 	}
	popMatrix();
    }
}

class Hexagon
{
    static int DIR_NW = 1;
    static int DIR_NE = 2;
    static int DIR_E  = 3;
    static int DIR_SE = 4;
    static int DIR_SW = 5;
    static int DIR_W  = 6;

    static double h = sin (radians(30));
    static double r = cos (radians(30));
    static double a = 2 * r;
    static double b = 2 * h + 1;


    static PVector dirCoord(PVector v, int d)
    {
	switch (d) {
	case DIR_NW:
	    return new PVector(v.x, v.y -1);
	case DIR_NE:
	    return new PVector(v.x + 1, v.y -1);
	case DIR_E:
	    return new PVector(v.x + 1, v.y);
	case DIR_SE:
	    return new PVector(v.x, v.y + 1);
	case DIR_SW:
	    return new PVector(v.x - 1, v.y + 1);
	case DIR_W:
	    return new PVector(v.x - 1, v.y);
	default:
	    return null;
	}
    }

    int hx, hy;
    double dx, dy;

    Hexagon(int x, int y) {
       	hx = x;
	hy = y;

	dx = hx * a;
	dy = hy * (h + 1);    

	if (hy % 2 != 0) {
	    dx += r;
	}
    }

    void display() {
	pushMatrix();
	translate(dx, dy);
	stroke(1);
	smooth();
	beginShape();
	vertex(0, h);
	vertex(r, 0);
	vertex(a, h);
	vertex(a, h + 1);
	vertex(r, b);
	vertex(0, h + 1);
	endShape();
	popMatrix();	
    }
}

