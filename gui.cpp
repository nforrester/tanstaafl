#include <X11/Xlib.h>
#include <GL/glx.h>
#include <GL/freeglut.h>
#include <pthread.h>
#include <iostream>

using namespace std;

GLfloat field_of_view = 40.0;

class material{
public:
	GLfloat* ambient;
	GLfloat* diffuse;
	GLfloat* specular;
	GLfloat  shininess;
	GLfloat* emission;
	void set(GLenum face);
	material();
	~material();
};

void material::set(GLenum face){
	glMaterialfv(face, GL_AMBIENT,        ambient);
	glMaterialfv(face, GL_DIFFUSE,        diffuse);
	glMaterialfv(face, GL_SPECULAR,       specular);
	glMaterialf (face, GL_SHININESS,      shininess);
	glMaterialfv(face, GL_EMISSION,       emission);
}

material::material(){
	shininess = 0.0;
	ambient = new GLfloat[4];
	diffuse = new GLfloat[4];
	specular = new GLfloat[4];
	emission = new GLfloat[4];

	ambient[0] = 0.2;
	ambient[1] = 0.2;
	ambient[2] = 0.2;
	ambient[3] = 1.0;

	diffuse[0] = 0.8;
	diffuse[1] = 0.8;
	diffuse[2] = 0.8;
	diffuse[3] = 1.0;

	specular[0] = 0.0;
	specular[1] = 0.0;
	specular[2] = 0.0;
	specular[3] = 1.0;

	emission[0] = 0.0;
	emission[1] = 0.0;
	emission[2] = 0.0;
	emission[3] = 1.0;
}

material::~material(){
	delete[] ambient;
	delete[] diffuse;
	delete[] specular;
	delete[] emission;
}

class space_object{
public:
	space_object* next; // Arrange them as a singly linked list.
	GLfloat* pos;
	GLfloat* ang_pos;
	GLfloat radius;
	material mat;
	space_object();
	~space_object();
	void draw();
};

space_object::space_object(){
	next = NULL;
	radius = 1.0;
	pos = new GLfloat[3];
	pos[0] = 0.0;
	pos[1] = 0.0;
	pos[2] = 0.0;
	ang_pos = new GLfloat[4];
	ang_pos[0] = 0.0;
	ang_pos[1] = 0.0;
	ang_pos[2] = 0.0;
	ang_pos[3] = 0.0;
}

space_object::~space_object(){
	delete[] pos;
	delete[] ang_pos;
	delete next;
}

void space_object::draw(){
	glPushMatrix();
		mat.set(GL_FRONT_AND_BACK);
		glTranslatef(pos[0], pos[1], pos[2]);
		glRotatef(ang_pos[0], ang_pos[1], ang_pos[2], ang_pos[3]);
		glutSolidTeapot(radius);
	glPopMatrix();
}

class universe{
public:
	universe();
	~universe();

	universe* next; // a linked list of previous universes, waiting to be garbage collected.

	space_object* first_object; // linked list of the objects in the universe

	pthread_mutex_t mutex;
	void lock();
	void unlock();
	int trylock();
};

universe::universe(){
	next = NULL;
	first_object = NULL;
	pthread_mutex_init(&mutex, NULL);
}

universe::~universe(){
	delete first_object;
	pthread_mutex_destroy(&mutex);
}

void universe::lock(){
cout << "universal lock?\n";
	pthread_mutex_lock(&mutex);
cout << "universal lock!\n";
}

void universe::unlock(){
cout << "universal unlock?\n";
	pthread_mutex_unlock(&mutex);
cout << "universal unlock!\n";
}

int universe::trylock(){
	return pthread_mutex_trylock(&mutex);
}

universe* now; // singly linked list
pthread_mutex_t now_mutex; // to prevent accidents with garbage collection, makes sure that the definition of now doesn't change at a bad moment.

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
cout << "reshape1\n";
	glViewport(0, 0, (GLsizei) width, (GLsizei) height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(field_of_view, ((float) width / (float) height), .1, 1000);
cout << "reshape2\n";
	glutPostRedisplay();
cout << "reshape3\n";
}

void display(){
cout << "sup!\n";
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(
		0.0, 10.0, 0.0,
		0.0, 0.0, 0.0,
		1.0, 0.0, 0.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

cout << "lock?\n";
	pthread_mutex_lock(&now_mutex);
cout << "lock!\n";
	if(now == NULL){
cout << "unlocka?\n";
		pthread_mutex_unlock(&now_mutex);
cout << "unlocka!\n";
	}else{
cout << "...\n";
		universe* universe_to_draw = now;
		universe_to_draw->lock();
cout << "unlocker?\n";
		pthread_mutex_unlock(&now_mutex);
cout << "unlocker!\n";

		space_object *obj = universe_to_draw->first_object;
		while(obj != NULL){
			obj->draw();
			obj = obj->next;
cout << "loopy!\n";
		}
		universe_to_draw->unlock();
	}

cout << "display update!\n";

	glutSwapBuffers();
}

void keyboard(unsigned char key, int x, int y){
}

void mouse(int button, int state, int x, int y){
}

pthread_cond_t collect_garbage_now;
pthread_mutex_t collect_garbage_now_mutex;

bool new_timestep;

void *physics_io(void *no_arg){
	basic_string<char> str;

	universe* next_universe;
	space_object* new_object;

	/*****************************************************************
	 * Example timestep:
	 *
	 * begin-timestep
	 * begin-object
	 * pos 0 1 5.3
	 * ang-pos 10 2 3 4.3
	 * radius 4.1
	 * end-object
	 * end-timestep
	 *****************************************************************/

	while(!cin.eof()){
		getline(cin, str, '\n');
		if(str == "begin-timestep"){
			next_universe = new universe();
cout << "recognized bt\n";
			while(str != "end-timestep"){
				getline(cin, str, '\n');
				if(str == "begin-object"){
cout << "recognized bo\n";
					new_object = new space_object();
					new_object->next = next_universe->first_object;
					next_universe->first_object = new_object;
					while(str != "end-object"){
						cin >> str;
						if(str == "pos"){
cout << "recognized pos\n";
							cin >> new_object->pos[0];
cout << new_object->pos[0] << endl;
							cin >> new_object->pos[1];
cout << new_object->pos[1] << endl;
							cin >> new_object->pos[2];
cout << new_object->pos[2] << endl;
						}
						if(str == "ang-pos"){
cout << "recognized ang-pos\n";
							cin >> new_object->ang_pos[0];
cout << new_object->ang_pos[0] << endl;
							cin >> new_object->ang_pos[1];
cout << new_object->ang_pos[1] << endl;
							cin >> new_object->ang_pos[2];
cout << new_object->ang_pos[2] << endl;
							cin >> new_object->ang_pos[3];
cout << new_object->ang_pos[3] << endl;
						}
						if(str == "radius"){
cout << "recognized radius\n";
							cin >> new_object->radius;
cout << new_object->radius << endl;
						}
					}
cout << "recognized eo\n";
				}
			}
cout << "recognized et\n";

			next_universe->next = now;
			pthread_mutex_lock(&now_mutex);
			now = next_universe;
			pthread_mutex_unlock(&now_mutex);

			new_timestep = 1;

			pthread_mutex_lock(&collect_garbage_now_mutex);
			pthread_cond_signal(&collect_garbage_now);
			pthread_mutex_unlock(&collect_garbage_now_mutex);
		}
	}
}

void *garbage_collector(void *no_arg){
	universe* condemned;

	while(1){
cout << "it's monday\n";
		pthread_mutex_lock(&collect_garbage_now_mutex);
		pthread_cond_wait(&collect_garbage_now, &collect_garbage_now_mutex);
		pthread_mutex_unlock(&collect_garbage_now_mutex);

cout << "finally\n";
		while(now->next != NULL){
cout << "here's the garbage\n";
			pthread_mutex_lock(&now_mutex);
			condemned = now->next;
			condemned->lock();
			now->next = now->next->next;
			pthread_mutex_unlock(&now_mutex);
			delete condemned;
cout << "got it\n";
		}
	}
}

int main (int argc, char **argv){
	pthread_t physics_io_thread;
	pthread_t garbage_collector_thread;

	now = NULL;

	new_timestep = 0;

	pthread_cond_init(&collect_garbage_now, NULL);
	pthread_mutex_init(&collect_garbage_now_mutex, NULL);
	pthread_mutex_init(&now_mutex, NULL);

	pthread_create(&physics_io_thread, NULL, physics_io, NULL);
	pthread_create(&garbage_collector_thread, NULL, garbage_collector, NULL);

	initialize_window(argc, argv);
	initialize_gl_state();

	glutDisplayFunc(display);
	glutReshapeFunc(reshape);
	glutKeyboardFunc(keyboard);
	glutMouseFunc(mouse);

	while(1){
		glutMainLoopEvent();

		if(new_timestep){
			new_timestep = 0;
			glutPostRedisplay();
		}
	}

	return 0;
}
