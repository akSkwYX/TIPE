#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "structures.h"

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

cell* createMap(int height, int width, structure_array* array_of_structures){

	structure* list_of_structures = array_of_structures->list_of_structures;
	int nbr_structures = array_of_structures->size;
	cell* map = (cell*)malloc(sizeof(cell) * height * width);
	
	for (int k=0; k<nbr_structures; k++){
		
		for (int x=list_of_structures[k].begining.x; x<list_of_structures[k].ending.x; x++){
			for (int y=list_of_structures[k].begining.y; y<list_of_structures[k].ending.y; y++){

				/* Initialize all attributes of the cell from the list_of_structures[k].cell_composing_structure cell */

				map[y*width + x].temperature = list_of_structures[k].cell_composing_structure.temperature;
				map[y*width + x].type = list_of_structures[k].cell_composing_structure.type;
				map[y*width + x].coords.x = x;
				map[y*width + x].coords.y = y;
				map[y*width + x].volumetric_heat_capacity = list_of_structures[k].cell_composing_structure.volumetric_heat_capacity;
				map[y*width + x].lambda = list_of_structures[k].cell_composing_structure.lambda;
				map[y*width + x].surface = list_of_structures[k].cell_composing_structure.surface;
				map[y*width + x].thickness = list_of_structures[k].cell_composing_structure.thickness;
				map[y*width + x].outside_isolation_lambda = list_of_structures[k].cell_composing_structure.outside_isolation_lambda;
				map[y*width + x].inside_isolation_lambda = list_of_structures[k].cell_composing_structure.inside_isolation_lambda;
				map[y*width + x].outside_isolation_thickness = list_of_structures[k].cell_composing_structure.outside_isolation_thickness;
				map[y*width + x].inside_isolation_thickness = list_of_structures[k].cell_composing_structure.inside_isolation_thickness;

			}
		}

	}
	return map;
}

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Main */

int main(){
	structure_array* array_of_structures = initialize_structure();
	cell* map = createMap(10, 11, array_of_structures);
	printMap(map, 10, 11);
	return 0;
}