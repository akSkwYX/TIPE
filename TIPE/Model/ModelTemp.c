#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "structures.h"

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Fonctions de transition */

float conduction(cell c1, cell c2, float timestep){
	if (c1.outside_isolation_thickness == 0 || c2.outside_isolation_thickness == 0){
		return ((c1.temperature - c2.temperature) * timestep) / (c1.thickness / (c1.lambda * c1.surface));
	}
}

float update_inside_air(cell c, cell* voisins, float timestep){
	float Q = 0;
	float W = 0;
	for (int i=0; i<4; i++){
		Q += conduction(c, voisins[i], timestep);
	}
	return c.temperature + ((Q + W) / c.volumetric_heat_capacity);
}

float update_outside_air(cell c, cell* voisins, float timestep){
	return c.temperature;
}

float update_wall(cell c, cell* voisins, float timestep){
	float Q = 0;
	float W = 0;
	for (int i=0; i<4; i++){
		Q += conduction(c, voisins[i], timestep);
	}
	return c.temperature + ((Q + W) / c.volumetric_heat_capacity);
}

cell* update_map(cell* map, int height, int width, float timestep){
	cell* new_map = (cell*)malloc(sizeof(cell)*height*width);
	for (int y=1; y<height-1; y++){
		for (int x=1; x<width-1; x++){
			cell actCell = map[y*width + x];
			new_map[y*width + x] = actCell;
			cell voisins[4] = {
				map[(y-1)*width + x],
				map[(y+1)*width + x],
				map[y*width + x-1],
				map[y*width + x+1]
			};
			if (strcmp(actCell.type, "inside air") == 0){
				new_map[y*width + x].temperature = update_inside_air(actCell, voisins, timestep);
			}
			else if (strcmp(actCell.type, "outside air") == 0){
				new_map[y*width + x].temperature = update_outside_air(actCell, voisins, timestep);
			}
			else if (strcmp(actCell.type, "wall") == 0){
				new_map[y*width + x].temperature = update_wall(actCell, voisins, timestep);
			}
		}
	}
	for (int y=0; y<height; y++){
		new_map[y*width] = map[y*width];
	}
	for (int x=0; x<width; x++){
		new_map[x] = map[x];
	}
	for (int y=0; y<height; y++){
		new_map[y*width + width-1] = map[y*width + width-1];
	}
	for (int x=0; x<width; x++){
		new_map[(height-1)*width + x] = map[(height-1)*width + x];
	}
	return new_map;
}

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Fonction d'affichage de la carte */

void printMap(cell* map, int height, int width)
{
    for (int y = 0; y < height; y++)
    {
        for (int x = 0; x < width; x++)
        {
            if (strcmp(map[y*width + x].type, "outside air") == 0)
            {
                printf("\x1b[46m");
            }
            else if (strcmp(map[y*width + x].type, "inside air") == 0)
            {
                printf("\x1b[30;47m");
            }
			else if (strcmp(map[y*width + x].type, "isolated wall") == 0)
			{
				printf("\x1b[40;97m");
			}
			else if (strcmp(map[y*width + x].type, "wall") == 0)
			{
				printf("\x1b[100m");
			}
			else
			{
				printf("\x1b[41;31m");
			}
            printf("%.1f ", map[y*width + x].temperature);
            printf("\x1b[0m");
        }
        printf("\n");
    }
}

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Fonction de création de la carte */

cell* createMap(int height, int width, structure* structures, int nbr_structures){

	cell* map = (cell*)malloc(sizeof(cell) * height * width);

	for (int k=0; k<nbr_structures; k++){
		
		for (int l=0; l<structures[k].coord_list.length; l++){
			
			for (int y=structures[k].coord_list.list[2*l].y; y<=structures[k].coord_list.list[2*l+1].y; y++){
				for (int x=structures[k].coord_list.list[2*l].x; x<=structures[k].coord_list.list[2*l+1].x; x++){

					/* Initialize all attributes of the cell from the structures[k].cell_composing_structure cell */

					map[y*width + x].temperature = structures[k].cell_composing_structure.temperature;
					map[y*width + x].type = structures[k].cell_composing_structure.type;
					map[y*width + x].coords.x = x;
					map[y*width + x].coords.y = y;
					map[y*width + x].volumetric_heat_capacity = structures[k].cell_composing_structure.volumetric_heat_capacity;
					map[y*width + x].lambda = structures[k].cell_composing_structure.lambda;
					map[y*width + x].surface = structures[k].cell_composing_structure.surface;
					map[y*width + x].thickness = structures[k].cell_composing_structure.thickness;
					map[y*width + x].outside_isolation_lambda = structures[k].cell_composing_structure.outside_isolation_lambda;
					map[y*width + x].inside_isolation_lambda = structures[k].cell_composing_structure.inside_isolation_lambda;
					map[y*width + x].outside_isolation_thickness = structures[k].cell_composing_structure.outside_isolation_thickness;
					map[y*width + x].inside_isolation_thickness = structures[k].cell_composing_structure.inside_isolation_thickness;
				}
			}
		}

	}
	return map;
}

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Main */

int main(){
	int nbr_structures;
	int height;
	int width;
	float timestep = 1; // En secondes
	int	max_time = 10; // En secondes
	float time = 1;
	structure* structures = initialize_structure(&nbr_structures, &height, &width);
	cell* map = createMap(height, width, structures, nbr_structures);
	while (time <= max_time){
		/*if (time / 3600 == (int)(time / 3600)){
			printf("Time: %d\n", (int)(time / 3600));
			printMap(map, height, width);
			printf("\n");
		}
		*/
		printMap(map, height, width);
		printf("\n");
		map = update_map(map, height, width, timestep);
		time += timestep;
	}
	free(map);
	free(structures);
	return 0;
}