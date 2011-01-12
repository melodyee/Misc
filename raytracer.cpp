/* 
	Remember to compile try:
		1) gcc hi.c -o hi -lX11
		2) gcc hi.c -I /usr/include/X11 -L /usr/X11/lib -lX11
		3) gcc hi.c -I /where/ever -L /who/knows/where -l X11

	Brian Hammond 2/9/96.    Feel free to do with this as you will!
*/


/* include the X library headers */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

/* include some silly stuff */
#include <stdio.h>
#include <stdlib.h>
#include  <signal.h>
#include <time.h>

#include <list>
#include <iostream>
#include <limits>
#include <cmath>
#include <cstdlib>

using namespace std;

numeric_limits<double> real;
double delta = sqrt(real.epsilon()), infinity = real.infinity();

struct Vec {
  double x, y, z;
  Vec(double x2, double y2, double z2) : x(x2), y(y2), z(z2) {}
};
Vec operator+(const Vec &a, const Vec &b)
{ return Vec(a.x+b.x, a.y+b.y, a.z+b.z); }
Vec operator-(const Vec &a, const Vec &b)
{ return Vec(a.x-b.x, a.y-b.y, a.z-b.z); }
Vec operator*(double a, const Vec &b) { return Vec(a*b.x, a*b.y, a*b.z); }
double dot(const Vec &a, const Vec &b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
Vec unitise(const Vec &a) { return (1 / sqrt(dot(a, a))) * a; }

struct Hit {
	double first;
	Vec second;
	Vec color;
	Hit(double f, Vec s, Vec cr) : first(f), second(s), color(cr) {}
	~Hit() {}
};

struct Ray {
  Vec orig, dir;
  Ray(const Vec &o, const Vec &d) : orig(o), dir(d) {}
};

struct Scene {
  virtual ~Scene() {};
  virtual Hit intersect(const Hit &, const Ray &) const = 0;
};

Scene *s;

struct Sphere : public Scene {
  Vec center;
  double radius;
  Vec color;

  Sphere(Vec c, double r, Vec cr) : center(c), radius(r), color(cr) {}
  ~Sphere() {}

  double ray_sphere(const Ray &ray) const {
    Vec v = center - ray.orig;
    double b = dot(v, ray.dir), disc = b*b - dot(v, v) + radius * radius;
    if (disc < 0) return infinity;
    double d = sqrt(disc), t2 = b + d;
    if (t2 < 0) return infinity;
    double t1 = b - d;
    return (t1 > 0 ? t1 : t2);
  }

  Hit intersect(const Hit &hit, const Ray &ray) const {
    double lambda = ray_sphere(ray);
    if (lambda >= hit.first) return hit;
    return Hit(lambda, unitise(ray.orig + lambda*ray.dir - center), color);
  }
};

typedef list<Scene *> Scenes;
struct Group : public Scene {
  Sphere bound;
  Scenes child;

  Group(Sphere b, Scenes c) : bound(b), child(c) {}
  ~Group() {
    for (Scenes::const_iterator it=child.begin(); it!=child.end(); ++it)
      delete *it;
  }

  Hit intersect(const Hit &hit, const Ray &ray) const {
    Hit hit2=hit;
    double l = bound.ray_sphere(ray);
    if (l >= hit.first) return hit;
    for (Scenes::const_iterator it=child.begin(); it!=child.end(); ++it)
      hit2 = (*it)->intersect(hit2, ray);
    return hit2;
  }
};

Hit intersect(const Ray &ray, const Scene &s)
{ return s.intersect(Hit(infinity, Vec(0, 0, 0), Vec(1,1,1)), ray); }

Vec ray_trace(const Vec &light, const Ray &ray, const Scene &s, int depth) {
  Hit hit = intersect(ray, s);
  if (hit.first == infinity) return Vec(0,0,0);
  double g = dot(hit.second, light);
  if (g >= 0) return Vec(0,0,0);
  Vec p = ray.orig + hit.first*ray.dir + delta*hit.second;
  Vec acc = (intersect(Ray(p, -1. * light), s).first < infinity ? Vec(0,0,0) : -g*hit.color);
  if (depth == 0) return acc;

  Vec a = Vec (-ray.dir.x,-ray.dir.y,-ray.dir.z);
  Vec by = dot(a,hit.second) *  hit.second;
  Vec out = 2 * by - a;
  Ray outray = Ray (p, unitise(out));
  Vec res = ray_trace(light, outray, s, depth-1);
  acc = acc + 0.9 * res;
  return acc;
}

#define n_colors 7
Vec colors[n_colors] = {
		(Vec(1,1,1)),
		(Vec(1,0.5,0.5)),(Vec(0.5,1,0.5)),(Vec(0.5,0.5,1)),
		(Vec(1,1,0.5)),(Vec(0.5,1,1)),(Vec(1,0.5,1)),
//		(Vec(0.5,0.5,0.5)),(Vec(0,0,0.5)),(Vec(0,0.5,0)),
//		(Vec(0.5,0,0))
};

Scene *create(int level, const Vec &c, double r, int nc) {
  Scene *s = new Sphere(c, r, colors[nc%n_colors]);
  if (level == 1) return s;
  Scenes child;
  child.push_back(s);
  double beta = 3./sqrt(12.);
  double alpha = 2;
  double r2 = r*0.6;
  /*
  for (int dz=-1; dz<=1; dz+=2)
    for (int dx=-1; dx<=1; dx+=2)
      child.push_back(create(level-1, c + rn*Vec(dx, 1, dz), r/2));
      */
  child.push_back(create(level-1, c + alpha *r*Vec(0, 1, 1), r2, nc+1));
  child.push_back(create(level-1, c + alpha *r*Vec(beta, 0.5, 0.5), r2, nc+2));
  child.push_back(create(level-1, c + alpha *r*Vec(-beta, 0.5, 0.5), r2, nc+3));
  child.push_back(create(level-1, c + alpha *r*Vec(0, -1, -1), r2, nc+4));
  child.push_back(create(level-1, c + alpha *r*Vec(-beta, -0.5, -0.5), r2, nc+5));
  child.push_back(create(level-1, c + alpha *r*Vec(beta, -0.5, -0.5), r2, nc+6));

  return new Group(Sphere(c, 4*r, colors[0]), child);
}

/* here are our X variables */
Display *dis;
int screen;
Window win;
GC gc;

/* here are our X routines declared! */
void init_x();
void close_x();
void redraw();

int main () {
	XEvent event;		/* the XEvent declaration !!! */
	KeySym key;		/* a dealie-bob to handle KeyPress Events */	
	char text[255];		/* a char buffer for KeyPress Events */

	init_x();

	/* look for events forever... */
	while(1) {		
		/* get the next event and stuff it into our event variable.
		   Note:  only events we set the mask for are detected!
		*/
		XNextEvent(dis, &event);
	
		if (event.type==Expose && event.xexpose.count==0) {
		/* the window was exposed redraw it! */
			redraw();
		}
	}
}

void init_x() {
/* get the colors black and white (see section for details) */        
	unsigned long black,white;

	dis=XOpenDisplay((char *)0);
   	screen=DefaultScreen(dis);
	black=BlackPixel(dis,screen),
	white=WhitePixel(dis, screen);
   	win=XCreateSimpleWindow(dis,DefaultRootWindow(dis),0,0,	
		512, 512, 5,white, black);
   	char buf[100];
#ifdef ISGCC
   	if (ISGCC == 0)
   		sprintf(buf,"%s","Loongcc peak");
   	else
#endif
   		sprintf(buf,"%s","GCC peak");
	XSetStandardProperties(dis,win,buf,"Hi",None,NULL,0,NULL);
	XSelectInput(dis, win, ExposureMask|ButtonPressMask|KeyPressMask);
        gc=XCreateGC(dis, win, 0,0);        
	XSetBackground(dis,gc,white);
	XSetForeground(dis,gc,black);
	XClearWindow(dis, win);
	XMapRaised(dis, win);

	int level = 3;
	s = create(level, Vec(0, 0.7, 0), 0.6, 5);

};

void close_x() {
	XFreeGC(dis, gc);
	XDestroyWindow(dis,win);
	XCloseDisplay(dis);	
	exit(1);				
};

void redraw() {
	while(1) {
#ifndef TEST
		while (time(0)%30!=15)
			;
#endif

	XClearWindow(dis, win);

	  int n = 512, ss = 2;
	  int depth = 3;
	  double eps = 1/ss;

	  Vec light = unitise(Vec(-1, -3, 2));
	  Vec light2 = unitise(Vec(-1, 3, -2));

	  for (int y=0; y<n; y++)
	    for (int x=0; x<n; ++x) {
	      Vec g=Vec(0,0,0);
	      for (int dx=0; dx<ss; ++dx)
	        for (int dy=0; dy<ss; ++dy) {
	          Vec dir(unitise(Vec(x+dx*eps-n/2., y+dy*eps-n/2., n)));
	          g = g + ray_trace(light, Ray(Vec(0, 0, -4), dir), *s, depth);
	          g = g + ray_trace(light2, Ray(Vec(0, 0, -4), dir), *s, depth);
	        }
	     double scale = 220;
	     char offset = 0;
	    unsigned char red = min(255,int(.5 + scale * g.x / (ss*ss))) + offset;
	    unsigned char green = min(255,int(.5 + scale * g.y / (ss*ss)))+ offset;
	    unsigned char blue = min(255,int(.5 + scale * g.z / (ss*ss)))+ offset;
	    XSetForeground(dis,gc, (red<<16) | (green<<8) | blue);
	    XDrawPoint(dis, win, gc, x, n-1-y);
	    }
#ifdef TEST
	exit(0);
#endif
	}
};
