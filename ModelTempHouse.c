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
				int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
				float total_conv_wall = 0;
				float total_conv = 0;
				for (int l = 0; l < 4; l++){
					if (i + toAdd[l][0] >= 0 && i + toAdd[l][0] < height && j + toAdd[l][1] >= 0 && j + toAdd[l][1] < width){
						cell c = map[(actCell.co.i + toAdd[l][0])*width + actCell.co.j + toAdd[l][1]];
						if (strcmp(actCell.type, "wall") == 0 || strcmp(actCell.type, "window") == 0 || strcmp(actCell.type, "door") == 0){
							total_conv_wall += (0.06+(c.epaisseur / c.lambda)+0.06)*(actCell.surface)*(actCell.T - c.T);
						}
						else{
							total_conv += c.T - actCell.T;
						}
					}else{
						total_conv = 0;
						total_conv_wall = 0;
						map[i*width + j].T = 15.0;
						break;
					}
				}
				map[i*width + j].T += total_conv_wall + (total_conv/2);
			}
			else if (strcmp(actCell.type, "airInt") == 0){
				int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
				float total_cond = 0;
				float total_conv_air = 0;
				float total_conv_wall = 0;
				int nbrCellAir = 0;
				for (int l = 0; l < 4; l++){
					cell c = map[(actCell.co.i + toAdd[l][0])*width + actCell.co.j + toAdd[l][1]];
					if (strcmp(actCell.type, "wall") == 0 || strcmp(actCell.type, "window") == 0 || strcmp(actCell.type, "door") == 0){
						total_cond += ((map[(actCell.co.i + 2*toAdd[l][0])*width + actCell.co.j + 2*toAdd[l][1]].T - actCell.T) * 1 * c.lambda * c.surface) / ((actCell.CTherVol * actCell.surface * actCell.epaisseur) * c.epaisseur);
						total_conv_wall += (0.06+(c.epaisseur / c.lambda)+0.06)*(actCell.surface)*(actCell.T - c.T);
					}
					else{
						total_cond += ((c.T - actCell.T) * 1 * c.lambda * c.surface) / ((actCell.CTherVol * actCell.surface * actCell.epaisseur) * c.epaisseur);
						total_conv_air += c.T - actCell.T;
						nbrCellAir++;
					}
				}
				if (actCell.T >= 18.0){
					map[i*width + j].T += total_cond + (total_conv_air/nbrCellAir) + total_conv_wall;
				}
				else{
					map[i*width + j].T += total_cond + (total_conv_air/nbrCellAir) + total_conv_wall + (80/(actCell.CTherVol * actCell.surface * actCell.epaisseur));
				}
			}
			else if (strcmp(actCell.type, "window") == 0){
				int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
				float total_cond = 0;
				float total_conv_window = 0;
				for (int l = 0; l < 4; l++){
					cell c = map[(actCell.co.i + toAdd[l][0])*width + actCell.co.j + toAdd[l][1]];
					if (strcmp(actCell.type, "window") == 0){
						total_conv_window += c.T - actCell.T;
					}
				}
				map[i*width + j].T += total_cond + (total_conv_window/2);
			}
			else if (strcmp(actCell.type, "wall") == 0){
				int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
				float total_conv_airExt = 0;
				float total_conv_wall = 0;
				for (int l = 0; l < 4; l++){
					cell c = map[(actCell.co.i + toAdd[l][0])*width + actCell.co.j + toAdd[l][1]];
					if (strcmp(actCell.type, "wall") == 0){
						total_conv_wall += c.T - actCell.T;
					}
				}
				map[i*width + j].T += total_conv_airExt + (total_conv_wall/2);
			}
			else if (strcmp(actCell.type, "door") == 0){
				int toAdd[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}; // Coordonnées à ajouter aux coordonnées de la cellule regardée pour parcourir les cellules adjacentes
				float total_conv = 0;
				for (int l = 0; l < 4; l++){
					cell c = map[(actCell.co.i + toAdd[l][0])*width + actCell.co.j + toAdd[l][1]];
					if (strcmp(actCell.type, "wall") == 0 || strcmp(actCell.type, "window") == 0 || strcmp(actCell.type, "door") == 0){
						total_conv += c.T - actCell.T;
					}
				}
				map[i*width + j].T += total_conv/2;
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
   	int height = 20;
   	int width = 20;
	// Define the house
	structure window1 = {
		.T = 20.0,
		.type = "window",
		.begining = {.i = 2, .j = 8},
		.ending = {.i = 2, .j = 10},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure window2 = {
		.T = 20.0,
		.type = "window",
		.begining = {.i = 9, .j = 2},
		.ending = {.i = 14, .j = 2},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure door = {
		.T = 20.0,
		.type = "door",
		.begining = {.i = 17, .j = 11},
		.ending = {.i = 17, .j = 11},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure wall_top = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 2, .j = 2},
		.ending = {.i = 2, .j = 17},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure wall_right = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 2, .j = 17},
		.ending = {.i = 17, .j = 17},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure wall_bottom = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 17, .j = 2},
		.ending = {.i = 17, .j = 17},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure wall_left = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 2, .j = 2},
		.ending = {.i = 17, .j = 2},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure intAirs = {
		.T = 25.0,
		.type = "airInt",
		.begining = {.i = 3, .j = 3},
		.ending = {.i = 16, .j = 16},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 2.5,
		.epaisseur = 1,
	};
	structure topExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 0, .j = 0},
		.ending = {.i = 1, .j = 19},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 2.5,
		.epaisseur = 5.0,
	};
	structure rightExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 0, .j = 18},
		.ending = {.i = 19, .j = 19},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 2.5,
		.epaisseur = 5.0,
	};
	structure bottomExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 18, .j = 0},
		.ending = {.i = 19, .j = 19},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 2.5,
		.epaisseur = 5.0,
	};
	structure leftExt = {
		.T = 15.0,
		.type = "airExt",
		.begining = {.i = 0, .j = 0},
		.ending = {.i = 19, .j = 1},
		.CTherVol = 1256,
		.lambda = 0.025,
		.surface = 2.5,
		.epaisseur = 5.0,
	};
	structure wall_bed_1 = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 2, .j = 13},
		.ending = {.i = 17, .j = 13},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure wall_bed_2 = {
		.T = 15.0,
		.type = "wall",
		.begining = {.i = 7, .j = 13},
		.ending = {.i = 7, .j = 17},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure window_bed = {
		.T = 15.0,
		.type = "window",
		.begining = {.i = 2, .j = 15},
		.ending = {.i = 2, .j = 15},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure door_bed = {
		.T = 20.0,
		.type = "door",
		.begining = {.i = 5, .j = 13},
		.ending = {.i = 5, .j = 13},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure door_bed_2 = {
		.T = 20.0,
		.type = "door",
		.begining = {.i = 8, .j = 13},
		.ending = {.i = 8, .j = 13},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure window_bed_2 = {
		.T = 20.0,
		.type = "window",
		.begining = {.i = 17, .j = 15},
		.ending = {.i = 17, .j = 15},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure window_bed_3 = {
		.T = 20.0,
		.type = "window",
		.begining = {.i = 10, .j = 17},
		.ending = {.i = 12, .j = 17},
		.CTherVol = 17000,
		.lambda = 1.0,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure wall_salon = {
		.T = 20.0,
		.type = "wall",
		.begining = {.i = 2, .j = 6},
		.ending = {.i = 17, .j = 6},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure open_door = {
		.T = 20.0,
		.type = "airInt",
		.begining = {.i = 7, .j = 6},
		.ending = {.i = 8, .j = 6},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
	structure wall_salon_2 = {
		.T = 20.0,
		.type = "wall",
		.begining = {.i = 11, .j = 6},
		.ending = {.i = 11, .j = 13},
		.CTherVol = 2400000,
		.lambda = 0.061,
		.surface = 2.5,
		.epaisseur = 0.394,
	};
	structure open_door_2 = {
		.T = 20.0,
		.type = "airInt",
		.begining = {.i = 11, .j = 9},
		.ending = {.i = 11, .j = 10},
		.CTherVol = 350000,
		.lambda = 0.12,
		.surface = 2.0,
		.epaisseur = 0.1,
	};
    int nbrStructure = 23;
	structure init_structure[23] = {topExt, rightExt, bottomExt, leftExt, wall_top, wall_right, wall_bottom, wall_left, intAirs, window1, window2, door, wall_bed_1, wall_bed_2, window_bed, door_bed, door_bed_2, window_bed_2, window_bed_3, wall_salon, open_door, wall_salon_2, open_door_2};
	cell* map = initialize(height, width, init_structure, nbrStructure);
	printMap(map, height, width);
	FILE* file = fopen("data.csv", "w");
	int lapsTime = 86400;
    for (int t = 0; t < lapsTime; t++)
    {
        fprintf(file, "%f,", map[(intAirs.begining.i + 1)*width + (intAirs.ending.j + 1)].T);
        nextStep(map, height, width);
        if (t % 3600 == 0 || (t <= 600 && t % 60 == 0))
        {
			if (t >= 3600){
            	//printf("%dh :\n", t/3600);
            	//printMap(map, height, width);
            	//printf("\n");
			} else {
				printf("%dmin :\n", t/60);
				printMap(map, height, width);
				printf("\n");
			}
        }
        
    }
	free(map);
    return 0;
}