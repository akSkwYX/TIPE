#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "structures.h"

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Fonctions de transition */

float conduction(cell* map, int width, int height, cell to_update_cell, cell other_cell, float timestep){
	if(other_cell.type != INNER_INSULATION && other_cell.type != OUTDOOR_INSULATION){
		float rth = other_cell.thickness / (other_cell.lambda * other_cell.surface);
		return ((other_cell.temperature - to_update_cell.temperature) * timestep) / rth;
	} /* else {
		int diff_x = other_cell.coords.x - to_update_cell.coords.x;
		int diff_y = other_cell.coords.y - to_update_cell.coords.y;
		cell other_other_cell = map[(other_cell.coords.y + diff_y)*width + other_cell.coords.x + diff_x];
		if (other_other_cell.type = INNER_INSULATION || other_other_cell.type == OUTDOOR_INSULATION){
			return to_update_cell.temperature;
		} else {
			float rth = other_cell.thickness / (other_cell.lambda * other_cell.surface);
			float rth_other = other_other_cell.thickness / (other_other_cell.lambda * other_other_cell.surface);
			return ((other_cell.temperature - to_update_cell.temperature) * timestep) / rth;
			return to_update_cell.temperature;
		}
	} */
}

float update_inside_air(cell* map, int width, int height, cell c, cell* voisins, float timestep){
	float Q = 0;
	float W = 0;
	for (int i=0; i<4; i++){
		Q += conduction(map, width, height, c, voisins[i], timestep);
	}
	//printf("heat capacity : %f\n", c.mass_heat_capacity * c.mass);
	return c.temperature + ((Q + W) / (c.mass_heat_capacity * c.mass));
}

float update_outside_air(cell* map, int width, int height, cell c, cell* voisins, float timestep){
	return c.temperature;
}

float update_wall(cell* map, int width, int height, cell c, cell* voisins, float timestep){
	float Q = 0;
	float W = 0;
	for (int i=0; i<4; i++){
		Q += conduction(map, width, height, c, voisins[i], timestep);
	}
	//printf("Q : %f  --  other : %f\n", Q, (Q + W) / (c.mass_heat_capacity * c.mass));
	return c.temperature + ((Q + W) / (c.mass_heat_capacity * c.mass));
}

float update_inner_insulation(cell* map, int width, int height, cell c, cell* voisins, float timestep){
	return c.temperature;
}

float update_outdoor_insulation(cell* map, int width, int height, cell c, cell* voisins, float timestep){
	return c.temperature;
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
			if (actCell.type == INSIDE_AIR){
				new_map[y*width + x].temperature = update_inside_air(map, width, height, actCell, voisins, timestep);
			}
			else if (actCell.type == OUTSIDE_AIR){
				new_map[y*width + x].temperature = update_outside_air(map, width, height, actCell, voisins, timestep);
			}
			else if (actCell.type == WALL){
				new_map[y*width + x].temperature = update_wall(map, width, height, actCell, voisins, timestep);
			}
			else if (actCell.type == INNER_INSULATION){
				new_map[y*width + x].temperature = update_inner_insulation(map, width, height, actCell, voisins, timestep);
			}
			else if (actCell.type == OUTDOOR_INSULATION){
				new_map[y*width + x].temperature = update_outdoor_insulation(map, width, height, actCell, voisins, timestep);
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
            if (map[y*width + x].type == OUTSIDE_AIR)
            {
                printf("\x1b[46m");
            }
            else if (map[y*width + x].type == INSIDE_AIR)
            {
                printf("\x1b[30;47m");
            }
			else if (map[y*width + x].type == INNER_INSULATION)
			{
				printf("\x1b[43m");
			}
			else if (map[y*width + x].type == OUTDOOR_INSULATION){
				printf("\x1b[43m");
			}
			else if (map[y*width + x].type == WALL)
			{
				printf("\x1b[100m");
			}
			else
			{
				printf("\x1b[41;31m");
			}
			if (map[y*width + x].type == INNER_INSULATION || map[y*width + x].type == OUTDOOR_INSULATION){
				printf("     ");
			} else {
				printf("%4.1f ", map[y*width + x].temperature);
			}
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
					map[y*width + x].mass_heat_capacity = structures[k].cell_composing_structure.mass_heat_capacity;
					map[y*width + x].lambda = structures[k].cell_composing_structure.lambda;
					map[y*width + x].height = structures[k].cell_composing_structure.height;
					map[y*width + x].length = structures[k].cell_composing_structure.length;
					map[y*width + x].surface = structures[k].cell_composing_structure.surface;
					map[y*width + x].thickness = structures[k].cell_composing_structure.thickness;
					map[y*width + x].volumetric_mass = structures[k].cell_composing_structure.volumetric_mass;
					map[y*width + x].mass = structures[k].cell_composing_structure.mass;
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
	int	max_time = 86400; // En secondes
	int time_print = 3600; // En secondes
	float time = 0;
	structure* structures = initialize_structure(&nbr_structures, &height, &width);
	cell* map = createMap(height, width, structures, nbr_structures);
	while (time <= max_time){
		if (time / time_print == (int)(time / time_print)){
			printf("Time: %dh\n", (int)(time / time_print));
			printMap(map, height, width);
			printf("\n");
		}
		map = update_map(map, height, width, timestep);
		time += timestep;
	}
	free(map);
	free(structures);
	return 0;
}