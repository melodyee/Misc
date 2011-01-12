#include <X11/Xlib.h>

#include <X11/Xutil.h>

#include <iostream>

#include <stdio.h>

#include <unistd.h>

#include <math.h>

using namespace std;

#define N 20

int bestRoute[N];
int profitMatrix[N][N];
int currentRoute[N];


// creates a random profit matrix
void createProfit(int profitMatrix[N][N]);

// creates a random route
void createRoute(int currentRoute[N]);

// evaluates a route
int evaluateRoute(int currentRoute[N], const int profitMatrix[N][N]);

// trys a new route (you get to play with this function
void tryRoute(int currentRoute[N], int bestRoute[N],
		const int profitMatrix[N][N]);

// swap swap two items
void swap(int &item1, int &item2);

unsigned short myrand() {
//	static unsigned int a = 10032;
//	a = ((a*a) >> 5) + 324217;
//	return a;

	static unsigned short lfsr = 0xACE1u;
	unsigned bit;
	  /* taps: 16 14 13 11; characteristic polynomial: x^16 + x^14 + x^13 + x^11 + 1 */
	  bit  = ((lfsr >> 0) ^ (lfsr >> 2) ^ (lfsr >> 3) ^ (lfsr >> 5) ) & 1;
	  lfsr =  (lfsr >> 1) | (bit << 15);
	  //printf("lfsr = %u\n",lfsr%20);
	  //printf("lfsr = %u\n",rand()%20);
	  return lfsr;
//	return rand();
}

int work() {
	// variables used
	int value = 0;
	int max = 0;
	int i = 0;
	long int start;
	int kount = 0;

	// create a random environment
	createRoute(currentRoute);
	createRoute(bestRoute);
	createProfit(profitMatrix);

	// seed the rand number generator
	//srand(time(0));

	// loop for 60 CPU seconds
	//start = time(0);
	while (kount < 10000000) {
		value = evaluateRoute(currentRoute, profitMatrix);
		tryRoute(currentRoute, bestRoute, profitMatrix);

		// display every 10000 cycles
		kount++;
		if (kount % 100000==0) {
			cout << "Current = " << evaluateRoute(currentRoute, profitMatrix)
					<< "\t" << " Best = " << evaluateRoute(bestRoute,
					profitMatrix)
//					<< "\t" << " Time = " << (time(0) - start)
//					/ 1000 << "                 "
					<< "\r";
		}
	}

	// output the best route found in the 60 seconds alloted
	cout << "\n\n";
#if 0
	cout << "Profit is: " << evaluateRoute(bestRoute, profitMatrix) << "\n";
	for (i = 0; i <= N - 1; i++) {
		cout << bestRoute[i] << "\n";
	}
	cout << "\n\n";
#endif
	// Grade the route - Hopefully you did great
	cout << "Grade is: "
			<< int((evaluateRoute(bestRoute, profitMatrix) - 14000) * .025
					+ 60.00) << "\n";

	return 0;
}

// tryRoute - tries a route.  You get to pick what route to try next.
//
//    inputs - currentRoute - the current route plan
//           - bestRoute    - the best route so far
//           - profitMatrix - the matrix used to calculate the profit for a route
//
//   outputs - currentRoute - update this plan for you current route
//           - bestRoute    - Update this plan for the best route you have seen.
//           - profitMatrix - Changes to profitMatrix ARE NOT ALLOWED

int planRoute[N];
void tryRoute(int currentRoute[N], int bestRoute[N],
		const int profitMatrix[N][N]) {

	// variables

	int i;
	int first;
	int second;

	static long int tries = 0; // inializes to zero the first time only. (static)

	// increments the number of tries.
	tries++;

	// copy the current route.
	for (i = 0; i <= N - 1; i++) {
		planRoute[i] = currentRoute[i];
	}

	// HINT: When is it best to start over?
	//       When is it best to try small corrections to a known good route?
	//       Maybe you could use the tries counter to see if you have spent
	//         to much time on a specific route?


	// 90% of the time start over, otherwise see if we can plan a better route
	// based on the current route.
	if ((myrand() % 100) <3) {
		// random route
		createRoute(planRoute);
		tries = 0;
	} else {

		// HINT:  Do I want to try small changes or massive changes?
		//        To change more than two cities put the following code in a loop!

		int i;
		// flip two cities
		for(i=0;i<1;i++) {
			first = myrand() % N ;
			second = myrand() % N ;
			int n = (max(first,second) - min(first,second));
			switch(myrand()%2) {
				case 0:
					swap(planRoute[first], planRoute[second]);
					break;
				case 1:
					for(int j=0;j<n/2;j++) {
						swap(planRoute[min(first,second)+j], planRoute[max(first,second)-j]);
					}
					break;
				case 2:
					int k = planRoute[min(first,second)];
					int j;
					for(j=min(first,second);j<max(first,second);j++) {
						planRoute[j]=planRoute[j+1];
					}
					planRoute[j] = k;
					break;
			}
		}
	}

	// HINT:   Do I really want to save a bad route for further analysis?
	//         Maybe sometimes, maybe never?

	// save the best one.
	if (evaluateRoute(planRoute, profitMatrix) > evaluateRoute(currentRoute,
			profitMatrix)) {
		// reset the tries counter
		tries = 0;

		// save current route to best route
		for (i = 0; i <= N - 1; i++) {
			currentRoute[i] = planRoute[i];
		}

		// save the best one.
		if (evaluateRoute(currentRoute, profitMatrix) > evaluateRoute(bestRoute,
				profitMatrix)) {
			// reset the tries counter
			tries = 0;

			// save current route to best route
			for (i = 0; i <= N - 1; i++) {
				bestRoute[i] = currentRoute[i];
			}
		}
	} else {
		int diff = evaluateRoute(planRoute, profitMatrix) - evaluateRoute(currentRoute,
					profitMatrix);
		//printf ("%lf,%lf\n",(myrand()/32768.) * 100,exp(diff/20000.)*100);
		if ((myrand()/32768.) * 100 < exp(diff/2000.)*100) {
//		if (1) {
			// save current route to best route
			for (i = 0; i <= N - 1; i++) {
				currentRoute[i] = planRoute[i];
			}
		}
	}

}

// evaluateRoute - evaluates a route.
//
//    inputs - route        - the current route plan
//           - profitMatrix - the matrix used to calculate the profit for a route
//
//   outputs - the profit for the route provided.
//
int evaluateRoute(int route[N], const int profitMatrix[N][N]) {
	int total = 0;

	for (int i = 0; i <= N - 2; i++) {
		total = total + profitMatrix[route[i]][route[i + 1]];
	}
	total = total + profitMatrix[N - 1][0];
	return total;
}

// createRoute - creates a route.
//
//    inputs - route        - the current route plan
//   outputs - route        - a random route.
void createRoute(int route[N]) {
	int first;
	int second;
	int numb;
	int i;

	for (i = 0; i <= N-1; i++) {
		route[i] = i;
	}

	numb = myrand() % 10 + 10;
	for (i = 1; i <= numb; i++) {
		first = myrand() % N;
		second = myrand() % N;
		swap(route[first], route[second]);
	}
}

// createProfit - creates a random profit matrix.
//
//    inputs - profitMatrix - the current profit matrix
//
//   outputs - profitMatrix - a random profit matrix.
//
int distance(int x,int y, int x2, int y2) {
	return 10*sqrt((x-x2)*(x-x2)+(y-y2)*(y-y2));
}

int cities[N*2] = {442, 147, 98, 16, 452, 293, 288, 245, 17, 213, 468, 320, 224, 370, 416, 88,
		  75, 259, 96, 160, 354, 110, 92, 506, 92, 274, 252, 386, 64, 457, 388, 312,
		  273, 326, 219, 492, 137, 74, 50, 200};
void createProfit(int profitMatrix[N][N]) {
	for (int i = 0; i <= N-1; i++) {
		for (int j = 0; j <= i; j++) {
			int cost = distance(cities[2*i],cities[2*i+1],cities[2*j],cities[2*j+1]);
			profitMatrix[i][j] = -cost;
			profitMatrix[j][i] = -cost;
		}
	}
}

// swap - exchanges two items
void swap(int &item1, int &item2) {
	int temp = item1;
	item1 = item2;
	item2 = temp;
}

#define WIN_WIDTH   512

#define WIN_HEIGHT  512



unsigned long GetColor( Display* dis, char* color_name )

{

    Colormap cmap;

    XColor near_color, true_color;

 

    cmap = DefaultColormap( dis, 0 );

    XAllocNamedColor( dis, cmap, color_name, &near_color, &true_color );

    return( near_color.pixel );

}

 

int main( void )

{

    Display* dis;

    Window win;

    XSetWindowAttributes att;

    GC gc;

    XEvent ev;

    int kount = 0;

    int t;

    int i;

 

    dis = XOpenDisplay( NULL );

    win = XCreateSimpleWindow( dis, RootWindow(dis,0), 100, 100,

      WIN_WIDTH, WIN_HEIGHT, 5, WhitePixel(dis,0), BlackPixel(dis,0) );

 

    att.backing_store = WhenMapped;

    XChangeWindowAttributes( dis, win, CWBackingStore, &att );

 

    XSelectInput( dis, win, ExposureMask );

    XMapWindow( dis, win );

 

    do{

        XNextEvent( dis, &ev);

    }while( ev.type != Expose );

 

    gc = XCreateGC( dis, DefaultRootWindow(dis), 0, 0 );

    XSetFunction( dis, gc, GXxor );

	createRoute(currentRoute);
	createRoute(bestRoute);
	createProfit(profitMatrix);

    for (int t = 0; t < 10000; t++)

    {

//           XSetForeground( dis, gc, BlackPixel(dis, 0)^GetColor( dis, "red"));
           XSetForeground( dis, gc, GetColor( dis, "red"));

       	while (kount < 1) {
//           while (kount < 10) {
       		//int value = evaluateRoute(currentRoute, profitMatrix);
       		tryRoute(currentRoute, bestRoute, profitMatrix);

       		// display every 10000 cycles
       		kount++;
       		if (kount % 100000==0) {
       			cout << "Current = " << evaluateRoute(currentRoute, profitMatrix)
       					<< "\t" << " Best = " << evaluateRoute(bestRoute,
       					profitMatrix)
       //					<< "\t" << " Time = " << (time(0) - start)
       //					/ 1000 << "                 "
       					<< "\r";
       		}
       	}

       	XClearWindow(dis, win);
           //XFillArc( dis, win, gc, t*5+80, t*3+40, 80, 40, 0, 360*64);
#if 1
       	for(i=0;i<N-1;i++){
       		XFillArc( dis, win, gc, cities[2*i], cities[2*i+1], 20, 20, 0, 360*64);
       		XDrawLine(dis,win,gc,cities[2*bestRoute[i]],cities[2*bestRoute[i]+1],
       				cities[2*bestRoute[i+1]],cities[2*bestRoute[i+1]+1]);
			}
       	XFillArc( dis, win, gc, cities[2*i], cities[2*i+1], 20, 20, 0, 360*64);
       	XDrawLine(dis,win,gc,cities[2*bestRoute[N-1]],cities[2*bestRoute[N-1]+1],
       					cities[2*bestRoute[0]],cities[2*bestRoute[0]+1]);
#else
       	for(i=0;i<N-1;i++){
       		XFillArc( dis, win, gc, cities[2*i], cities[2*i+1], 20, 20, 0, 360*64);
       		XDrawLine(dis,win,gc,cities[2*currentRoute[i]],cities[2*currentRoute[i]+1],
       				cities[2*currentRoute[i+1]],cities[2*currentRoute[i+1]+1]);
			}
       	XDrawLine(dis,win,gc,cities[2*currentRoute[N-1]],cities[2*currentRoute[N-1]+1],
       					cities[2*currentRoute[0]],cities[2*currentRoute[0]+1]);
#endif
           //XSetForeground( dis, gc, BlackPixel(dis, 0)^GetColor( dis, "red"));
           kount = 0;
  			cout << "Current = " << evaluateRoute(currentRoute, profitMatrix)
  					<< "\t" << " Best = " << evaluateRoute(bestRoute,
  					profitMatrix)
  //					<< "\t" << " Time = " << (time(0) - start)
  //					/ 1000 << "                 "
  					<< "\r";
        usleep(100000);


           //XFillArc( dis, win, gc, t*5+80, t*3+40, 80, 40, 0, 360*64);

    }

      

    XDestroyWindow( dis , win );

    XCloseDisplay( dis );

    return(0);

}
