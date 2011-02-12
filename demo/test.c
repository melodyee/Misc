/***************************************************************************
 *   Copyright (C) 2011 by zsc   *
 *   zsc@zsc   *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

/* Copyright (c) Mark J. Kilgard, 1995. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

/* molehill uses the GLU NURBS routines to draw some nice surfaces. */

#include <math.h>
#include <GL/glut.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N 200
char depth[N][N];
// GLfloat mat_red_diffuse[] = { 0.7, 0.0, 0.1, 1.0 };
// GLfloat mat_green_diffuse[] = { 0.0, 0.7, 0.1, 1.0 };
// GLfloat mat_blue_diffuse[] = { 0.0, 0.1, 0.7, 1.0 };
// GLfloat mat_yellow_diffuse[] = { 0.7, 0.8, 0.1, 1.0 };
// GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 1.0 };
// GLfloat mat_shininess[] = { 40.0 };
const int milliseconds_per_frame = 500;  // Time you would LIKE per frame
                                        // The actual time can be longer.
int animating = 1;  // The value is toggled when user hits the "A" key. 
int newsockfd;

void draw_strip(char arr[N][N],double scale);

char buffer[N*N*2];
int pos = 0;
void timer(int id) {
  while (1) {
    int n = read(newsockfd,buffer+pos,N*N);
    if (n < 0) error("ERROR reading from socket");
    pos +=n;
    if(pos>=N*N) 
      break;
  }
  int i,j;
  memcpy(depth,buffer,N*N);
  pos-=N*N;
  memcpy(buffer,buffer+N*N+pos,pos);
  glutPostRedisplay();
}

static void 
    display(void)
{
  if (animating) {
         // Cause display to be called again after milliseconds_per_frame.
    glutTimerFunc(milliseconds_per_frame,timer,1);  
  }
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
//   glCallList(1);
  draw_strip(depth,10./N);
//   glFlush();
  glutSwapBuffers();  // Makes the drawing appear on the screen!
}

void initlights(void)
{
  GLfloat ambient[] =
  {0.2, 0.2, 0.2, 1.0};
  GLfloat position[] =
  {0.0, 14.0, 2.0, 1.0};
  GLfloat mat_diffuse[] =
  {0.3, 0.3, 0.6, 1.0};
  GLfloat mat_specular[] =
  {1.0, 1.0, 1.0, 1.0};
  GLfloat mat_shininess[] =
  {70.0};

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT0, GL_POSITION, position);
//   glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 0.2);
  
  glShadeModel(GL_SMOOTH);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
  glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
  glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);
}

void init_depth(char depth[N][N]) {
  int i,j;
  for(i=0;i<N;i++)
    for(j=0;j<N;j++)
      depth[i][j]=0;//80*sin(3.14*((float)i*2/N)*((float)j*2/N));
}
    
void draw_strip(char arr[N][N],double scale) {
  int i,j;
#if 1
  for(i=0;i<N-1;i++) {
    glBegin(GL_TRIANGLE_STRIP);  
//     glBegin(GL_LINES);
//     glBegin(GL_POINTS);
//     glBegin(GL_LINE_STRIP); 
    for(j=0;j<N-1;j++) {
      glVertex3f(scale*(i-N/2),scale*(j-N/2),scale*arr[i][j]/4.);
      glVertex3f(scale*(i+1-N/2),scale*(j-N/2),scale*arr[i+1][j]/4.);
      GLfloat vx = 1; GLfloat vy = 0; GLfloat vz = (-arr[i][j]+arr[i][j+1])/4.;
      GLfloat wx = 0; GLfloat wy = 1; GLfloat wz = (-arr[i][j]+arr[i+1][j])/4.;
      glNormal3f(vy*wz-vz*wy,vz*wx-vx*wz,vx*wy-vy*wx);
    }
    glEnd();
  }
#else
//   for(j=0;j<N;j++) {
//     glBegin(GL_LINE_STRIP); 
//     for(i=0;i<N;i++) {
//       glVertex3f(scale*(i-N/2),scale*(j-N/2),scale*arr[i][j]/10.);
//     }
//     glEnd();
//   }
  for(j=0;j<N-1;j++) {
    glBegin(GL_TRIANGLE_STRIP);  
//     glBegin(GL_LINES);
//     glBegin(GL_POINTS);
//     glBegin(GL_LINE_STRIP); 
    for(i=0;i<N-1;i++) {
      glVertex3f(scale*(i-N/2),scale*(j-N/2),scale*arr[i][j]/4.);
      glVertex3f(scale*(i-N/2),scale*(j+1-N/2),scale*arr[i][j+1]/4.);
      GLfloat vx = 1; GLfloat vy = 0; GLfloat vz = (-arr[i][j]+arr[i][j+1])/4.;
      GLfloat wx = 0; GLfloat wy = 1; GLfloat wz = (-arr[i][j]+arr[i+1][j])/4.;
      glNormal3f(vy*wz-vz*wy,vz*wx-vx*wz,vx*wy-vy*wx);
    }
    glEnd();
  }
#endif
}

int 
    work(char* id)
{
  init_depth(depth);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(512,512);
  char title[100];
  sprintf(title,"%s: Shallow water equation",id);
  glutCreateWindow(title);

//   glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
//   glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);

/*  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);*/
  glEnable(GL_AUTO_NORMAL);
  glEnable(GL_NORMALIZE);
  glEnable(GL_DEPTH_TEST);
  initlights();       /* for lighted version only */

  glMatrixMode(GL_PROJECTION);
  gluPerspective(55.0, 1.0, 2.0, 24.0);
  glMatrixMode(GL_MODELVIEW);
  glTranslatef(0.0, 0.0, -15.0);
  glRotatef(330.0, 1.0, 0.0, 0.0);
  glRotatef(30.0, 0.0, 0.0, 1.0);

//   glNewList(1, GL_COMPILE);
  
//   draw_strip(depth,10./N);

//   glEndList();

  glutDisplayFunc(display);
  glutMainLoop();
  return 0;             /* ANSI C requires main to return int. */
}

int main(int argc, char *argv[])
{
  int sockfd, portno, clilen;
  char buffer[256];
  struct sockaddr_in serv_addr, cli_addr;
  int n;
  if (argc < 2) {
    fprintf(stderr,"ERROR, no port provided\n");
    exit(1);
  }
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) 
    error("ERROR opening socket");
  bzero((char *) &serv_addr, sizeof(serv_addr));
  portno = atoi(argv[1]);
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portno);
  if (bind(sockfd, (struct sockaddr *) &serv_addr,
      sizeof(serv_addr)) < 0) 
    error("ERROR on binding");
  listen(sockfd,5);
  clilen = sizeof(cli_addr);
  newsockfd = accept(sockfd, 
                     (struct sockaddr *) &cli_addr, 
                      &clilen);
  if (newsockfd < 0) 
    error("ERROR on accept");
  
  glutInit(&argc, argv);
 
  work(argv[2]);
  return 0; 
}
