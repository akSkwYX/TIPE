#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>

/* Structures et Implémentation de listes */


// Structures

struct coord
{
    int i;
    int j;
};

typedef struct coord coord;

struct cell
{
    float T; // en degré Celsius
    char* type;
	 coord co;
	 float CTherVol;
	 float lambda; //conductivité thermique en W.K-1.m-1
	 float surface; // en m2
	 float epaisseur; // en m
};

typedef struct cell cell;

struct structure
{
	float T;
	char* type;
	coord begining;
	coord ending;
	float CTherVol;
	float lambda;
	float surface;
	float epaisseur;
};

typedef struct structure structure;

// Listes

struct list
{
    int cap;
    int taille;
    coord* t;
};

typedef struct list list;

list *create_list(int capacity)
{
    list *l = (list *)malloc(sizeof(list));
    coord *t = (coord *)malloc(sizeof(coord) * capacity);
    l->cap = capacity;
    l->taille = 0;
    l->t = t;
    return l;
}

void add_list(list *l, int i, int j)
{
    if (l->taille > l->cap)
    {
        printf("Erreur de capacité");
    }
    else
    {
        l->t[l->taille].i = i;
        l->t[l->taille].j = j;
        l->taille += 1;
    }
}

/* Fonctions qui servent à mettre à jour la carte des cellules */


// Mise à jour des murs

void updateWalls(cell **map, list *walls)
{
    for (int k = 0; k < walls->taille; k++)
    {
        for (int x = -1; x < 2; x++)
        {
            for (int y = -1; y < 2; y++)
            {
                if (strcmp(map[walls->t[k].i + x][walls->t[k].j + y].type, "airExt") == 0)
                {
                    map[walls->t[k].i][walls->t[k].j].T = map[walls->t[k].i + x][walls->t[k].j + y].T;
                    goto end;
                }
            }
        }
    end:;
    }
}


// Mise à jour de l'air intérieur

void updateIntAirs(cell **map, list* intAirs){
	int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
	for (int k = 0; k < intAirs->taille; k++)
	{
		cell actCell = map[intAirs->t[k].i][intAirs->t[k].j];
		float total_cond = 0;
        float total_conv = 0;
		float total_chaleur = 0;
		for (int l = 0; l < 4; l++){
			cell c = map[actCell.co.i + toAdd[l][0]][actCell.co.j + toAdd[l][1]];
			total_cond += ((c.T - actCell.T) * 1 * c.lambda * c.surface) / ((actCell.CTherVol * actCell.surface * actCell.epaisseur) * c.epaisseur);
        	total_conv += c.T - actCell.T;
			//if (strcmp(c.type, "radiateur")==0){
			//	total_chaleur += (3500*1)/(actCell.CTherVol * actCell.surface * actCell.epaisseur);
			//}
        }
		map[intAirs->t[k].i][intAirs->t[k].j].T += total_cond + (total_conv/(4*300));
	}
}

// Mise à jour de l'air extérieur

void updateExtAirs(cell **map, list *extAirs)
{
}

// Mise à jour des fenêtres

void updateStructures(cell **map, list *structures)
{
    for (int k = 0; k < structures->taille; k++)
    {
        for (int x = -1; x < 2; x++)
        {
            for (int y = -1; y < 2; y++)
            {
            if (strcmp(map[structures->t[k].i + x][structures->t[k].j + y].type, "airExt") == 0)
                {
                    map[structures->t[k].i][structures->t[k].j].T = map[structures->t[k].i + x][structures->t[k].j + y].T;
                    goto end;
                }
            }
        }
    end:;
    }
}


/* Fonction d'affichage de la carte */

void printMap(cell *map, int height, int width)
{
    for (int i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            if (strcmp(map[i*width + j].type, "airExt") == 0)
            {
                printf("\x1b[46m");
            }
            else if (strcmp(map[i*width + j].type, "airInt") == 0)
            {
                printf("\x1b[30;47m");
            }
            else if (strcmp(map[i*width + j].type, "window") == 0)
            {
                printf("\x1b[104m");
            }
            else if (strcmp(map[i*width + j].type, "wall") == 0)
            {
                printf("\x1b[100m");
            }
			else if (strcmp(map[i*width + j].type, "door") == 0)
			{
				printf("\x1b[43m");
			}
			else if (strcmp(map[i*width + j].type, "radiateur") == 0)
			{
				printf("\x1b[41m");
			}
            printf("%.1f ", map[i*width + j].T);
            printf("\x1b[0m");
        }
        printf("\n");
    }
}

/* Appelle toutes les fonctions de mise à jours */

void nextStep(cell *map, int height, int width)
{
	for (int i=0; i<height; i++){
		for (int j=0; j<width; j++){
			cell actCell = map[i*width + j];
			if (strcmp(actCell.type, "airExt") == 0){

			}
			else if (strcmp(actCell.type, "airInt") == 0){
				int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
				float total_cond = 0;
				float total_conv = 0;
				for (int l = 0; l < 4; l++){
					cell c = map[actCell.co.i + toAdd[l][0]][actCell.co.j + toAdd[l][1]];
					if (strcmp(actCell.type, "wall") == 0){
						total_cond += ((map[actCell.co.i + 2*toAdd[l][0]][actCell.co.j + 2*toAdd[l][1]];.T - actCell.T) * 1 * c.lambda * c.surface) / ((actCell.CTherVol * actCell.surface * actCell.epaisseur) * c.epaisseur);
						total_conv += c.T - actCell.T;
					}
					else{
						total_cond += ((c.T - actCell.T) * 1 * c.lambda * c.surface) / ((actCell.CTherVol * actCell.surface * actCell.epaisseur) * c.epaisseur);
						total_conv += c.T - actCell.T;
					}
				}
				map[intAirs->t[k].i][intAirs->t[k].j].T += total_cond + (total_conv/(4*300));
			}
			else if (strcmp(actCell.type, "window") == 0){

			}
			else if (strcmp(actCell.type, "wall") == 0){

			}
			else if (strcmp(actCell.type, "door") == 0){

			}
		}
	}
}

/* Initialise la carte des cellules avec leur différents paramètres */

cell* initialize(int height, int width, structure* structures, int nbrStructure)
{
    cell* map = (cell*)malloc(sizeof(cell)*height*width);
	for (int k = 0; k < nbrStructure; k++){
		for (int i = structures[k].begining.i; i <= structures[k].ending.i; i++){
			for (int j = structures[k].begining.j; j <= structures[k].ending.j; j++){
				map[i*width + j].T = structures[k].T;
				map[i*width + j].type = structures[k].type;
				map[i*width + j].co.i = i;
				map[i*width + j].co.j = j;
				map[i*width + j].CTherVol = structures[k].CTherVol;
				map[i*width + j].lambda = structures[k].lambda;
				map[i*width + j].surface = structures[k].surface;
				map[i*width + j].epaisseur = structures[k].epaisseur;
			}
		}
	}
    return map;
}

int main()
{
   	int height = 8;
   	int width = 11;
	// Define the house
	structure window1 = {
		.T = 20.0,
		.type = "window",
		.begining = {.i = 1, .j = 4},
		.ending = {.i = 1, .j = 6},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 1,
		.epaisseur = 0.1,
	};
	structure window2 = {
		.T = 20.0,
		.type = "window",
		.begining = {.i = 3, .j = 1},
		.ending = {.i = 4, .j = 1},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 1,
		.epaisseur = 0.1,
	};
	structure door = {
		.T = 20.0,
		.type = "door",
		.begining = {.i = 6, .j = 5},
		.ending = {.i = 6, .j = 5},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 1,
		.epaisseur = 0.1,
	};
	structure wall_top = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 1, .j = 1},
		.ending = {.i = 1, .j = 9},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 1,
		.epaisseur = 0.394,
	};
	structure wall_right = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 1, .j = 9},
		.ending = {.i = 6, .j = 9},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 1,
		.epaisseur = 0.394,
	};
	structure wall_bottom = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 6, .j = 1},
		.ending = {.i = 6, .j = 9},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 1,
		.epaisseur = 0.394,
	};
	structure wall_left = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 1, .j = 1},
		.ending = {.i = 6, .j = 1},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 1,
		.epaisseur = 0.394,
	};
	structure intAirs = {
		.T = 25.0,
		.type = "airInt",
		.begining = {.i = 2, .j = 2},
		.ending = {.i = 5, .j = 8},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 1,
		.epaisseur = 1,
	};
	structure topExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 0, .j = 0},
		.ending = {.i = 0, .j = 10},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 1,
		.epaisseur = 1,
	};
	structure rightExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 0, .j = 10},
		.ending = {.i = 7, .j = 10},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 1,
		.epaisseur = 1,
	};
	structure bottomExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 7, .j = 0},
		.ending = {.i = 7, .j = 10},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 1,
		.epaisseur = 1,
	};
	structure leftExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 0, .j = 0},
		.ending = {.i = 7, .j = 0},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 1,
		.epaisseur = 1,
	};
    int nbrStructure = 12;
	structure init_structure[12] = {topExt, rightExt, bottomExt, leftExt, wall_top, wall_right, wall_bottom, wall_left, intAirs, window1, window2, door};
    cell* map = initialize(height, width, init_structure, nbrStructure);
	printMap(map, height, width);
	FILE* file = fopen("data.csv", "w");
	int lapsTime = 86400;
    for (int t = 0; t < lapsTime; t++)
    {
        fprintf(file, "%f,", map[house[0]+2][house[1]+1].T);
        nextStep(map, height, width);
        if (t % 3600 == 0)
        {
            printf("%dh :\n", t/3600);
            printMap(map, height, width);
            printf("\n");
        }
        
    }
	free(map);
    return 0;
}
