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
	float conductivite; // en W.K-1.
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
	float conductivite;
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
	int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
    for (int k = 0; k < walls->taille; k++)
    {
		cell actCell = map[walls->t[k].i][walls->t[k].j];
		float flux_tot = 0;
        for (int l = 0; l<4; l++)
		{
			cell c = map[actCell.co.i + toAdd[l][0]][actCell.co.j + toAdd[l][1]];
            if (strcmp(c.type, "airExt") == 0)
            {
                flux_tot += c.conductivite * actCell.surface * (actCell.T - c.T);
				map[walls->t[k].i + toAdd[l][0]][walls->t[k].j + toAdd[l][1]].T += (1/(actCell.conductivite * actCell.surface))*(c.conductivite * actCell.surface * (actCell.T - c.T));

            }
			else if (strcmp(c.type, "airInt") == 0)
			{
				flux_tot += c.conductivite * actCell.surface * (actCell.T - c.T);
				map[walls->t[k].i + toAdd[l][0]][walls->t[k].j + toAdd[l][1]].T += (1/(actCell.conductivite * actCell.surface))*(c.conductivite * actCell.surface * (actCell.T - c.T));
				
			}
        }
		map[walls->t[k].i][walls->t[k].j].T -= (1/(actCell.conductivite * actCell.surface))*flux_tot;
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
		for (int l = 0; l < 4; l++){
			cell c = map[actCell.co.i + toAdd[l][0]][actCell.co.j + toAdd[l][1]];
			total_cond += ((c.T - actCell.T) * 1 * c.lambda * c.surface) / ((actCell.CTherVol * actCell.surface * actCell.epaisseur) * c.epaisseur);
        	total_conv += c.T - actCell.T;
        }
		map[intAirs->t[k].i][intAirs->t[k].j].T += total_cond + (total_conv/(4*150));
	}
}

// Mise à jour de l'air extérieur

void updateExtAirs(cell **map, list *extAirs, int height, int width, float Text){
	for (int k = 0; k < extAirs->taille; k++)
	{
		map[extAirs->t[k].i][extAirs->t[k].j].T = Text;
	}
}

// Mise à jour des structures

void updateStructures(cell **map, list *structures)
{
	int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
    for (int k = 0; k < structures->taille; k++)
    {
		cell actCell = map[structures->t[k].i][structures->t[k].j];
		for (int l=0; l < 4; l++)
        {
				cell c = map[actCell.co.i + toAdd[l][0]][actCell.co.j + toAdd[l][1]];
				if ((strcmp(actCell.type, "radiateur") == 0) && (strcmp(c.type, "airInt") == 0)){
					map[actCell.co.i + toAdd[l][0]][actCell.co.j + toAdd[l][1]].T += (2000*1)/(actCell.CTherVol * actCell.surface * actCell.epaisseur);
				}
		}
	}
}


/* Fonction d'affichage de la carte */

void printMap(cell **map, int height, int width)
{
    for (int i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            if (strcmp(map[i][j].type, "airExt") == 0)
            {
                printf("\x1b[46m");
            }
            else if (strcmp(map[i][j].type, "airInt") == 0)
            {
                printf("\x1b[30;47m");
            }
            else if (strcmp(map[i][j].type, "window") == 0)
            {
                printf("\x1b[104m");
            }
            else if (strcmp(map[i][j].type, "wall") == 0)
            {
                printf("\x1b[100m");
            }
			else if (strcmp(map[i][j].type, "door") == 0)
			{
				printf("\x1b[43m");
			}
			else if (strcmp(map[i][j].type, "radiateur") == 0)
			{
				printf("\x1b[41m");
			}
            printf("%.1f ", map[i][j].T);
            printf("\x1b[0m");
        }
        printf("\n");
    }
}


/* Appelle toutes les fonctions de mise à jours */

void nextStep(cell **map, list** lists, int height, int width, float Text)
{
	updateStructures(map, lists[3]);
	updateIntAirs(map, lists[1]);
	updateWalls(map, lists[0]);
	updateExtAirs(map, lists[2], height, width, Text);
}


/* Initialise la carte des cellules avec leur différents paramètres */

cell** initialize(int height, int width, int* house, structure* structures, int nbrStructure, float* temps, float* CTherVol, float* lambdas, float* surfaces, float* epaisseurs, float* conductivites, list** lists)
{
    cell** map = ( cell** )malloc( sizeof(cell*)*(height) );
    for (int i = 0; i < height; i++)
    {
        cell* new = ( cell* )malloc(sizeof(cell)*(width));
        for (int j = 0; j < width; j++)
        {
		bool found = false;
			for (int k = 0; k < nbrStructure; k++){
				if ((i >= structures[k].begining.i) && (i <= structures[k].ending.i) && (j >= structures[k].begining.j) && (j <= structures[k].ending.j)){
					new[j].type = structures[k].type;
               		new[j].T = structures[k].T;
               		new[j].co.i = i;
               		new[j].co.j = j;
					new[j].CTherVol = structures[k].CTherVol;
					new[j].lambda = structures[k].lambda;
					new[j].surface = structures[k].surface;
					new[j].epaisseur = structures[k].epaisseur;
					new[j].conductivite = structures[k].conductivite;
         			add_list(lists[3], i, j);
					found = true;
				}
			}
		if (!found){
			if (((i == house[0] || i == house[2]) && j <= house[3] && j >= house[1]) || ((j == house[1] || j == house[3]) && i <= house[2] && i >= house[0]))
            {
               	new[j].type = "wall";
               	new[j].T = temps[0];
				new[j].co.i = i;
				new[j].co.j = j;
				new[j].CTherVol = CTherVol[0];
				new[j].lambda = lambdas[0];
				new[j].surface = surfaces[0];
				new[j].epaisseur = epaisseurs[0];
				new[j].conductivite = conductivites[0];
            	add_list(lists[0], i, j);
			}
            else if (i > house[0] && i < house[2] && j < house[3] && j > house[1])
            {
                new[j].type = "airInt";
                new[j].T = temps[1];
				new[j].co.i = i;
				new[j].co.j = j;
				new[j].CTherVol = CTherVol[1];
				new[j].lambda = lambdas[1];
				new[j].surface = surfaces[1];
				new[j].epaisseur = epaisseurs[1];
				new[j].conductivite = conductivites[1];
            	add_list(lists[1], i, j);
            }
            else
            {
                new[j].type = "airExt";
                new[j].T = temps[2];
				new[j].co.i = i;
				new[j].co.j = j;
				new[j].CTherVol = CTherVol[2];
				new[j].lambda = lambdas[2];
				new[j].surface = surfaces[2];
				new[j].epaisseur = epaisseurs[2];
				new[j].conductivite = conductivites[2];
                add_list(lists[2], i, j);
            }
        }
		}
        map[i] = new;
    }
    return map;
}

int main()
{
   	int height = 18;
   	int width = 18;
	int house[4] = {/* Corner top i */ 2, /* Corner top j */ 2, /* Corner bot i */ 15, /* Corner bot j */ 15};
	structure window1 = {
		.T = 15.0,
		.type = "window",
		.begining = {.i = 5, .j = 2},
		.ending = {.i = 7, .j = 2},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 1,
		.epaisseur = 0.1,
		.conductivite = 4.5,
	};
	structure window2 = {
		.T = 15.0,
		.type = "window",
		.begining = {.i = 2, .j = 7},
		.ending = {.i = 2, .j = 11},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 1,
		.epaisseur = 0.1,
		.conductivite = 4.5,
	};
	structure door = {
		.T = 15.0,
		.type = "door",
		.begining = {.i = 15, .j = 7},
		.ending = {.i = 15, .j = 7},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 1,
		.epaisseur = 0.1,
		.conductivite = 1.0,
	};
	structure radiateur = {
		.T = 25.0,
		.type = "radiateur",
		.begining = {.i = 8, .j = 14},
		.ending = {.i = 8, .j = 14},
		.CTherVol = 3200000,
		.lambda = 55,
		.surface = 1,
		.epaisseur = 0.2,
		.conductivite = 1,
	};
	structure wall1 = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 10, .j = 6},
		.ending = {.i = 14, .j = 6},
		.CTherVol = 2400000,
		.lambda = 1.4,
		.surface = 1,
		.epaisseur = 0.15,
		.conductivite = 4.4,
	};
	structure wall2 = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 10, .j = 3},
		.ending = {.i = 10, .j = 4},
		.CTherVol = 2400000,
		.lambda = 1.4,
		.surface = 1,
		.epaisseur = 0.15,
		.conductivite = 4.4,
	};
	structure door_bedroom = {
		.T = 15.0,
		.type = "door",
		.begining = {.i = 10, .j = 5},
		.ending = {.i = 10, .j = 5},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 1,
		.epaisseur = 0.1,
		.conductivite = 1.0,
	};
	structure radiateur_bedroom = {
		.T = 25.0,
		.type = "radiateur",
		.begining = {.i = 13, .j = 3},
		.ending = {.i = 13, .j = 3},
		.CTherVol = 3200000,
		.lambda = 55,
		.surface = 1,
		.epaisseur = 0.2,
		.conductivite = 1,
	};
    int nbrStructure = 8;
	structure init_structure[8] = {window1, window2, door, radiateur, wall1, wall2, door_bedroom, radiateur_bedroom};
	int w_top_i = 5;
    int w_bot_i = 7;
    int w_top_j = 2;
    int w_bot_j = 2;
	float temps[3] = {/* wall */ 20.0, /* AirInt */ 25.0, /* AirExt */ 15.0};
	float CTherVol[3] = {/* wall */ 2400000, /* AirInt */ 1256, /* AirExt */ 1256};
	float lambdas[3] = {/* wall */ 1.4 , /* AirInt */ 0.025 , /* AirExt */ 0.025};
	float surfaces[3] = {/* wall */ 1 , /* AirInt */ 1 , /* AirExt */ 1};
	float epaisseurs[3] = {/* wall */ 0.15 , /* AirInt */ 1 , /* AirExt */ 1};
	float conductivites[3] = {/* wall */ 4.4 , /* AirInt */ 1 , /* AirExt */ 1};
    int lapsTime = 86400;
    list* walls = create_list(height * width);
    list* intAirs = create_list(height * width);
    list* extAirs = create_list(height * width);
    list* structures = create_list(height * width);
	list* lists[4] = {walls, intAirs, extAirs, structures};
    cell** map = initialize(height, width, house, init_structure, nbrStructure, temps, CTherVol, lambdas, surfaces, epaisseurs, conductivites, lists);
	FILE* file = fopen("data.csv", "w");
    for (int t = 0; t <= lapsTime; t++)
    {
        fprintf(file, "%f,", map[house[0]+2][house[1]+1].T);
        nextStep(map, lists, height, width, temps[2]);
        if (t % 3600 == 0)
        {
            printf("%dh :\n", t / 3600);
            printMap(map, height, width);
            printf("\n");
        }
		if (t < 3600 && t % 60 == 0){
			printf("%dmin :\n", t / 60);
			printMap(map, height, width);
			printf("\n");
		}
        
    }
    return 0;
}
