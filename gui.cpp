#include <X11/Xlib.h>
#include <GL/glx.h>
#include <GL/glut.h>
#include <iostream>

using namespace std;

#define SIZE_OF_UNIVERSE 2

GLfloat field_of_view = 40.0;

class material{
public:
	GLfloat* ambient;
	GLfloat* diffuse;
	GLfloat* specular;
	GLfloat  shininess;
	GLfloat* emission;
	void set(GLenum face);
};

void material::set(GLenum face){
	glMaterialfv(face, GL_AMBIENT,        ambient);
	glMaterialfv(face, GL_DIFFUSE,        diffuse);
	glMaterialfv(face, GL_SPECULAR,       specular);
	glMaterialf (face, GL_SHININESS,      shininess);
	glMaterialfv(face, GL_EMISSION,       emission);
}

class space_object{
public:
	GLfloat* pos;
	GLfloat radius;
	material mat;
	void draw();
};

void space_object::draw(){
	glPushMatrix();
		mat.set(GL_FRONT_AND_BACK);
		glTranslatef(pos[0], pos[1], pos[2]);
		glutSolidSphere(radius, 16, 16);
	glPopMatrix();
}

space_object universe[SIZE_OF_UNIVERSE];

void initialize_window(int argc, char **argv){
	glutInitWindowPosition(0, 0);
	glutInitWindowSize(1000, 1000);
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
	glutCreateWindow("YINO");
}

void initialize_gl_state(){
	glClearColor(0.0, 0.0, 0.0, 0.0);
	glShadeModel(GL_SMOOTH);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
}

void reshape(int width, int height){
	glViewport(0, 0, (GLsizei) width, (GLsizei) height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(field_of_view, ((float) width / (float) height), .1, 1000);
}

void display(){
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(
		10.0, 5.0, 5.0,
		0.0, 0.0, 0.0,
		0.0, 1.0, 0.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	for(int i = 0; i < SIZE_OF_UNIVERSE; i++){
		universe[i].draw();
	}

	glutSwapBuffers();
}

void keyboard(unsigned char key, int x, int y){
}

void mouse(int button, int state, int x, int y){
}

int main (int argc, char **argv){
	universe[0].radius = 1.0;
	universe[0].pos = new GLfloat[3];
	universe[0].pos[0] = 0.0;
	universe[0].pos[1] = 0.0;
	universe[0].pos[2] = 0.0;
	universe[0].mat.ambient = new GLfloat[4];
	universe[0].mat.ambient[0] = 0.2;
	universe[0].mat.ambient[1] = 0.2;
	universe[0].mat.ambient[2] = 0.2;
	universe[0].mat.ambient[3] = 1.0;
	universe[0].mat.diffuse = new GLfloat[4];
	universe[0].mat.diffuse[0] = 0.8;
	universe[0].mat.diffuse[1] = 0.8;
	universe[0].mat.diffuse[2] = 0.8;
	universe[0].mat.diffuse[3] = 1.0;
	universe[0].mat.specular = new GLfloat[4];
	universe[0].mat.specular[0] = 0.0;
	universe[0].mat.specular[1] = 0.0;
	universe[0].mat.specular[2] = 0.0;
	universe[0].mat.specular[3] = 1.0;
	universe[0].mat.shininess = 0.0;
	universe[0].mat.emission = new GLfloat[4];
	universe[0].mat.emission[0] = 0.0;
	universe[0].mat.emission[1] = 0.0;
	universe[0].mat.emission[2] = 0.0;
	universe[0].mat.emission[3] = 1.0;

	universe[1].radius = 1.0;
	universe[1].pos = new GLfloat[3];
	universe[1].pos[0] = 3.0;
	universe[1].pos[1] = 0.0;
	universe[1].pos[2] = 0.0;
	universe[1].mat.ambient = new GLfloat[4];
	universe[1].mat.ambient[0] = 0.2;
	universe[1].mat.ambient[1] = 0.2;
	universe[1].mat.ambient[2] = 0.2;
	universe[1].mat.ambient[3] = 1.0;
	universe[1].mat.diffuse = new GLfloat[4];
	universe[1].mat.diffuse[0] = 0.8;
	universe[1].mat.diffuse[1] = 0.8;
	universe[1].mat.diffuse[2] = 0.8;
	universe[1].mat.diffuse[3] = 1.0;
	universe[1].mat.specular = new GLfloat[4];
	universe[1].mat.specular[0] = 0.0;
	universe[1].mat.specular[1] = 0.0;
	universe[1].mat.specular[2] = 0.0;
	universe[1].mat.specular[3] = 1.0;
	universe[1].mat.shininess = 0.0;
	universe[1].mat.emission = new GLfloat[4];
	universe[1].mat.emission[0] = 0.0;
	universe[1].mat.emission[1] = 0.0;
	universe[1].mat.emission[2] = 0.0;
	universe[1].mat.emission[3] = 1.0;

	initialize_window(argc, argv);
	initialize_gl_state();

	glutDisplayFunc(display);
	glutReshapeFunc(reshape);
	glutKeyboardFunc(keyboard);
	glutMouseFunc(mouse);

	glutMainLoop();

	return 0;
}
