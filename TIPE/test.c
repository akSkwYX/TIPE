#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

/* Structures particulière au modèle */

struct coordonates {
	int x;
	int y;
};

typedef struct coordonates coordonates;

// Cellule représentant une case de la carte

struct cell
{
    float temperature; // --------------- en degré Celsius
    char* type; // ------------ type de la cellule (airExt, airInt, wall, window, door)
	coordonates coords; // -------------- coordonnées de la cellule
	float volumetric_heat_capacity; // -------- Capacité thermique volumique en J.K-1.m-3
	float lambda; // ---------- Conductivité thermique en W.K-1.m-1
	float surface; // --------- Surface en m2
	float thickness; // ------- Epaisseur en m
	float outside_isolation_lambda; // -- Conductivité thermique de l'isolant extérieur en W.K-1.m-1
    float inside_isolation_lambda; // -- Conductivité thermique de l'isolant intérieur en W.K-1.m-1
    float outside_isolation_thickness; // Epaisseur de l'isolant extérieur en m
    float inside_isolation_thickness; // Epaisseur de l'isolant intérieur en m
};

typedef struct cell cell;

// Structure représentant une structure (un ensemble de cellule ayant les mêmes propriétés) de la maison

struct structure
{
	cell cell_composing_structure;
	coordonates begining;
	coordonates ending;
};

typedef struct structure structure;

structure* initialize_structure(){

	void* structures = malloc(sizeof(structure)*3 + sizeof(int));
	((int)*(structures)) = 3;

	cell isolated_wall = {
		.temperature = 20,
		.type = "isolated wall",
		.volumetric_heat_capacity = 2400000,
		.lambda = 1.4,
		.surface = 2.5,
		.thickness = 0.15,
		.outside_isolation_lambda = 0.04,
		.inside_isolation_lambda = 0.04,
		.outside_isolation_thickness = 0.07,
		.inside_isolation_thickness = 0.07
	};

	cell wall = {
		.temperature = 20,
		.type = "wall",
		.volumetric_heat_capacity = 2400000,
		.lambda = 1.4,
		.surface = 2.5,
		.thickness = 0.15
	};

	cell outside_air = {
		.temperature = 20,
		.type = "outside air",
		.volumetric_heat_capacity = 1256,
		.lambda = 0.025,
		.surface = 2.5,
		.thickness = 1
	};

	cell inside_air = {
		.temperature = 20,
		.type = "inside air",
		.volumetric_heat_capacity = 1256,
		.lambda = 0.025,
		.surface = 2.5,
		.thickness = 1
	};

	structure left_side = {
		.cell_composing_structure = inside_air,
		.begining = {0, 0},
		.ending = {9, 9},
	};

	structure middle_wall = {
		.cell_composing_structure = wall,
		.begining = {10, 0},
		.ending = {10, 9},
	};

	structure right_side = {
		.cell_composing_structure = outside_air,
		.begining = {11, 0},
		.ending = {20, 9},
	};

	(structure)*(structres + sizeof(int)) = left_side;
	structures[1] = middle_wall;
	structures[2] = right_side;

	return structures;

}

int main(){
	structure* structures = initialize_structure();
	int nbr_structures = *(structures + 3);
	printf("nbr_structures = %d\n", nbr_structures);
	printf("type = %s\n", structures[0].cell_composing_structure.type);
	return 0;
}