#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>

/* Structures */


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
    float lambda_iso_ext;
    float lambda_iso_int;
    float epaisseur_iso_ext;
    float epaisseur_iso_int;
};
typedef struct structure structure;

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

void printMap(cell *map, int height, int width)
{
    for (int i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            if (strcmp(map[i*width + j].type, "AirExt") == 0)
            {
                printf("\x1b[46m");
            }
            else if (strcmp(map[i*width + j].type, "AirInt") == 0)
            {
                printf("\x1b[30;47m");
            }
            else if (strcmp(map[i*width + j].type, "Mur") == 0)
            {
                printf("\x1b[100m");
            }
            printf("%.1f ", map[i*width + j].T);
            printf("\x1b[0m");
        }
        printf("\n");
    }
}

cell* nextStep(cell* map, int height, int width, FILE* file){
    cell* newMap = (cell*)calloc(height*width, sizeof(cell));
    int toAdd[4][2] = {{0,1}, {0,-1}, {1,0}, {-1,0}};
    for (int i = 0; i < height; i++){
        for (int j = 0; j < width; j++){
            cell actCell = map[i*width + j];
            float rth = actCell.epaisseur/(actCell.lambda*actCell.surface);
            if (strcmp(actCell.type, "AirInt") == 0){
                float Q = 0;
                int nbrVoisinsAir = 0;
                for (int l = 0; l < 4; l++){
                    if (i+toAdd[l][0] >= 0 && i+toAdd[l][0] < height && j+toAdd[l][1] >= 0 && j+toAdd[l][1] < width){
                        cell c = map[(i+toAdd[l][0])*width + j+toAdd[l][1]];
                        if (strcmp(c.type, "AirInt") == 0 || strcmp(c.type, "AirExt") == 0){
                            Q += (c.T - actCell.T) * 1 / (rth * actCell.CTherVol * actCell.surface * actCell.epaisseur);
                            nbrVoisinsAir++;
                        }
                    }
                }
                if (newMap[i*width + j].type == NULL){
                    newMap[i*width + j] = actCell;
                }
                if (actCell.T < 20){
                    newMap[i*width + j].T += Q/nbrVoisinsAir + 80/(actCell.CTherVol * actCell.surface * actCell.epaisseur);
                } else {
                    newMap[i*width + j].T += Q/nbrVoisinsAir;
                }
                printf("Q : %f\n", Q/nbrVoisinsAir);
            } else if (strcmp(actCell.type, "AirExt") == 0){
                float Q = 0;
                int nbrVoisinsAir = 0;
                for (int l = 0; l < 4; l++){
                    if (i+toAdd[l][0] >= 0 && i+toAdd[l][0] < height && j+toAdd[l][1] >= 0 && j+toAdd[l][1] < width){
                        cell c = map[(i+toAdd[l][0])*width + j+toAdd[l][1]];
                        if (strcmp(c.type, "AirInt") == 0 || strcmp(c.type, "AirExt") == 0){
                            Q += (c.T - actCell.T) * 1 / (rth * actCell.CTherVol * actCell.surface * actCell.epaisseur);
                            nbrVoisinsAir++;
                        }
                    }
                }
                if (newMap[i*width + j].type == NULL){
                    newMap[i*width + j] = actCell;
                }
                newMap[i*width + j].T += Q/nbrVoisinsAir;
            } else {
                float Q1 = 0;
                float Q2 = 0;
                coord coor_int = {0, 0};
                coord coor_ext = {0, 0};
                for (int l = 0; l < 4; l++){
                    if (i+toAdd[l][0] >= 0 && i+toAdd[l][0] < height && j+toAdd[l][1] >= 0 && j+toAdd[l][1] < width){
                        cell c = map[(i+toAdd[l][0])*width + j+toAdd[l][1]];
                        if (strcmp(c.type, "AirInt") == 0){
                            Q1 += (c.T - actCell.T) * 1 / (rth * actCell.CTherVol * actCell.surface * actCell.epaisseur);
                            coord coor_int = {i+toAdd[l][0], j+toAdd[l][1]};
                        } else if (strcmp(c.type, "AirExt") == 0){
                            Q2 += (c.T - actCell.T) * 1 / (rth * actCell.CTherVol * actCell.surface * actCell.epaisseur);
                            coord coor_ext = {i+toAdd[l][0], j+toAdd[l][1]};
                        }
                    }
                }
                printf("Q1 + Q2 : %f\n", Q1 + Q2);
                if (newMap[i*width + j].type == NULL){
                    newMap[i*width + j] = actCell;
                }
                if (newMap[coor_int.i*width + coor_int.j].type == NULL){
                    newMap[coor_int.i*width + coor_int.j] = map[coor_int.i*width + coor_int.j];
                }
                if (newMap[coor_ext.i*width + coor_ext.j].type == NULL){
                    newMap[coor_ext.i*width + coor_ext.j] = map[coor_ext.i*width + coor_ext.j];
                }
                newMap[i*width + j].T += (Q1 + Q2);
                newMap[coor_int.i*width + coor_int.j].T += Q1;
                newMap[coor_ext.i*width + coor_ext.j].T -= Q2;
            }
        }
    }
    free(map);
    return newMap;
}

int main(){
    structure interieur = {
        .T = 10,
        .type = "AirInt",
        .begining = {0,0},
        .ending = {2,1},
        .CTherVol = 1256,
        .lambda = 0.025,
        .surface = 2.5,
        .epaisseur = 1
    };
    structure mur = {
        .T = 10,
        .type = "Mur",
        .begining = {0,2},
        .ending = {2,2},
        .CTherVol = 2400000,
        .lambda = 1.4,
        .surface = 2.5,
        .epaisseur = 0.1,
        .lambda_iso_ext = 0.04,
        .lambda_iso_int = 0.04,
        .epaisseur_iso_ext = 0.05,
        .epaisseur_iso_int = 0.05
    };
    structure exterieur = {
        .T = 10,
        .type = "AirExt",
        .begining = {0,3},
        .ending = {2,4},
        .CTherVol = 1256,
        .lambda = 0.025,
        .surface = 2.5,
        .epaisseur = 1
    };
    int height = 3;
    int width = 5;
    int nbrStructure = 3;
    structure structures[3] = {interieur, mur, exterieur};
    cell* map = initialize(height, width, structures, nbrStructure);
    FILE* file = fopen("test.csv", "w");
    int lapsTime = 200;
    for (int t = 0; t < lapsTime; t++){
        printMap(map, height, width);
        printf("\n");
        fprintf(file, "%f;%f;%f,", map[6].T, map[7].T, map[8].T);
        map = nextStep(map, height, width, file);
    }
    free(map);
    fclose(file);
    return 0;
}