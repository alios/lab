/* Author: alios */

(function(window) {
    function Hexagon(g, hx, hy) {
	this.initialize(g, hx, hy);
    }

    Hexagon.prototype = new Shape();

    Hexagon.R30 = (30 * Math.PI) / 180;
    
    // public properties:
    Hexagon.prototype.hx = 0;
    Hexagon.prototype.hy = 0;
    Hexagon.prototype.selected = false;
    Hexagon.prototype.grid = null;

    // constructor:
    Hexagon.prototype.Shape_initialize = Hexagon.prototype.initialize;	//unique to avoid overiding base class
	
    Hexagon.prototype.initialize = function(g, hx, hy) {
	this.Shape_initialize(); // super call
	this.grid = g;
	this.hx = hx;
	this.hy = hy;
	this.name = 'hexagon (' + hx + '/' + hy + ')';
	this.getShape();	
    }

    Hexagon.prototype.s = function() {
	return 30;
    }

    Hexagon.prototype.h = function () {
	return Math.sin(Hexagon.R30) * this.s();
    }

    Hexagon.prototype.r = function () {
	return Math.cos(Hexagon.R30) * this.s();
    }

    Hexagon.prototype.dy = function () {
	return this.hy * (this.h() + this.s());
    }

    Hexagon.prototype.dx = function () {
	var _r = this.hx * 2 * this.r(); 
	return (this.hy % 2 == 0) ? _r :  this.r() + _r;
    }

    Hexagon.prototype.onPress = function(e) {
	this.selected = ! this.selected
	this.getShape();
    }
	    
    // public methods:
    Hexagon.prototype.getShape = function() {
	var g = this.graphics;

	g.clear();

	g.beginFill('#000000');
	g.beginStroke(this.selected ? '#FF0000' : '#0000FF');
	g.moveTo(0, this.h());
	g.lineTo(this.r(), 0);
	g.lineTo(2 * this.r(), this.h());
	g.lineTo(2 * this.r(), this.h() + this.s());
	g.lineTo(this.r(), 2*this.h() + this.s());
	g.lineTo(0, this.h() + this.s());
	g.closePath();		

	// translate
	this.x = this.dx();
	this.y = this.dy();
	this.regX = this.r();
	this.regY = this.h() + (this.s() / 2);

	// grid need update
	this.grid.needsUpdate = true;
    }
    window.Hexagon = Hexagon;
}(window));

var mkGrid = function(canvas, hx, hy) {
    var that = {};
    that.needsUpdate = false;
    that.stage = new Stage(canvas);
    that.hx = hx;
    that.hy = hy;
    that.gridContainer = new Container();

    that.selected = function() {
	return that.gridContainer.children.filter(function(element, index, array) {
	    return element.selected;
	});
    }

    that.tick = function() {
	// this set makes it so the stage only re-renders when an event handler indicates a change has happened.
	if (this.needsUpdate) {
	    this.needsUpdate = false; // only update once

	    var sortSelected = function (a,b)
	    {
		var _a = a.selected;
		var _b = b.selected;
		if (_a == _b) return 0;
		return (_a && !_b) ? 1 : -1;
	    }
	    this.gridContainer.sortChildren(sortSelected);
	    this.stage.update();
	}
    };
    
    for (var _x = 0; _x < hx; ++_x)
    {
	for (var _y = 0; _y < hy; ++_y)
	{
	    var h = new Hexagon(that, _x, _y);
	    that.gridContainer.addChild(h);
	}
    }

    // arrange the stage
    that.stage.clear();
    that.stage.addChild(that.gridContainer);

    // enabled mouse over / out events
    // that.stage.enableMouseOver(10);

    // enable Ticks
    Ticker.addListener(that);

    return that;
}

function init() {
    log("starting up application");
    // enable touch interactions if supported on the current device:
    if (Touch.isSupported()) { Touch.enable(stage); }

    //associate the canvas with the stage
    canvas = document.getElementById("map");

    var hexgrid = mkGrid(canvas, 15, 15);   
}











