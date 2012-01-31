

double h = sin (radians(30));
double r = cos (radians(30));

double a = 2 * r;
double b = 2 * h + 1;


Map m;

void setup() {
    //    size(screen.width, screen.height, P3D);
    size(800, 600, P3D);
    m = new Map(26,26);   
    noLoop();
    redraw();
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
    double ms = 1;

    Map(int nx, int ny) {
	mx = width / 2 -  nx / 2;
	my = height / 2 - ny / 2;
	hexagons = new ArrayList();
	for (int x = 0; x < nx; ++x) {
	    for (int y = 0; y < ny; ++y) {
		hexagons.add(new Hexagon(x,y));
	    }
	}   
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
	mx += 4 * a / ms;
	redraw();
    }

    void scrollRight() {
	mx -= 4 * a / ms;
	redraw();
    }

    void scrollUp() {
	my += 4 * b / ms;
	redraw();
    }

    void scrollDown() {
	my -= 4 * b / ms;
	redraw();
    }
    
    
    void display() {
	pushMatrix();	

	// do zoom
	translate(width/2, height/2);
	scale(ms);
	translate(width/-2, height/-2);

	// translate
	translate(mx, my);

	// draw hexagon
       	for (int i = 0; i <  hexagons.size(); ++i) {
	    Hexagon h = (Hexagon) hexagons.get(i);
	    h.display();
	}

	popMatrix();
    }
}

class Hexagon
{
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

