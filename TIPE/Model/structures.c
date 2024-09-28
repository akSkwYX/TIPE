#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "structures.h"

/* Liste chainée */

ListeChainee creerListeChainee() {
	return NULL;
}

bool LCestVide(ListeChainee liste) {
	return liste == NULL;
}

int LCobtenirElement(ListeChainee liste, char* cle){
	Link* current = liste;
	while (current != NULL && strcmp(current->cle, cle) != 0){
		current = current->next;
	}
	if (current != NULL){
		return current->value;
	} else {
		return -1;
	}
}

ListeChainee LCajouterElement(ListeChainee liste, char* cle, int valeur) {
	Link* nouveau = malloc(sizeof(Link));
	nouveau->cle = cle;
	nouveau->value = valeur;
	nouveau->next = liste;
	return nouveau;
}

ListeChainee LCsupprimerElement(ListeChainee liste, char* cle) {
	if (!LCestVide(liste)){
		Link* current = liste;
		if (strcmp(current->cle, cle) == 0){
			Link* tmp = current;
			Link* newListe = current->next;
			free(tmp);
			return newListe;
		} else {
			while (current->next != NULL && strcmp(current->next->cle, cle) != 0){
				current = current->next;
			}
			if (current->next != NULL){
				Link* tmp = current->next;
				current->next = current->next->next;
				free(tmp);
				return liste;
			}
		}
	}
}

void LCliberer(ListeChainee liste) {
	Link* current = liste;
	while (current != NULL){
		Link* tmp = current;
		current = current->next;
		free(tmp);
	}
}

void afficherListeChainee(ListeChainee liste) {
	Link* courant = liste;
	while (courant != NULL) {
		printf("%s : %d ", courant->cle, courant->value);
		courant = courant->next;
	}
	printf("\n");
}

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Table de hachage */

int hash(char* c, int m){
	int r = 0;
	int p = 1;
	int i = 0;
	while (c[i] != '\0'){
		r = (r + p*(c[i]-'a')) % m;
		p = (p*31) % m;
		i += 1;
	}
	return r;
}

dict* THcreer(int m){
	dict* d = (dict*)malloc(sizeof(dict));
	d->m = m;
	d->taille = 0;
	d->seaux = (ListeChainee*)calloc(m, sizeof(ListeChainee)*m);
	return d;
}

void THajouter(dict* d, char* c, int v){
	int indice = hash(c, d->m);
	d->seaux[indice] = LCajouterElement(d->seaux[indice], c, v);
	d->taille += 1;
}

int THobtenir(dict d, char c[]){
	int indice = hash(c, d.m);
	return LCobtenirElement(d.seaux[indice], c);
}

bool THestCle(dict d, char c[]){
	if (THobtenir(d, c) != -1){
		return true;
	}
	return false;
}

void THsupprimer(dict* d, char c[]){
	int indice = hash(c, d->m);
	d->seaux[indice] = LCsupprimerElement(d->seaux[indice], c);
	d->taille -= 1;
}

void THaffiche(dict* d){
	for (int i=0; i<d->m; i++){
		printf("|");
		afficherListeChainee(d->seaux[i]);
	}
	printf("\n");
}

void THliberer(dict* d){
	for (int i=0; i<d->m; i++){
		LCliberer(d->seaux[i]);
	}
	free(d->seaux);
	free(d);
}

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Composing the object to modelise */

float calculate_thermal_exchange_coefficient(cell c){
	if(c.convection_coefficient_1 != 0 && c.convection_coefficient_2 != 0){
		return 1 / (1 / c.convection_coefficient_1 + 1 / c.convection_coefficient_2 + c.thickness / c.lambda);
	} else if (c.convection_coefficient_1 == 0){
		return 1 / (1 / c.convection_coefficient_2 + c.thickness / c.lambda);
	} else if (c.convection_coefficient_2 == 0){
		return 1 / (1 / c.convection_coefficient_1 + c.thickness / c.lambda);
	}
}

structure* initialize_structure(int* nbr_structures, int* height, int* width){

	cell inner_insulation = {
		// Laine de roche
		.temperature = 0,
		.type = INNER_INSULATION,
		.mass_heat_capacity = 1030,
		.lambda = 0.04,
		.height = 2.5,
		.length = 0.01,
		//.thickness = 0.07,
		.thickness = 0.01,
		.surface = inner_insulation.height * inner_insulation.length,
		.volumetric_mass = 135,
		.mass = inner_insulation.volumetric_mass * inner_insulation.height * inner_insulation.length * inner_insulation.thickness,
		.convection_coefficient_1 = 0.17,
		.convection_coefficient_2 = 0,
		.thermal_exchange_coefficient = calculate_thermal_exchange_coefficient(inner_insulation)
	};

	cell outdoor_insulation = {
		// Laine de roche
		.temperature = 20,
		.type = OUTDOOR_INSULATION,
		.mass_heat_capacity = 1030,
		.lambda = 0.04,
		.height = 2.5,
		.length = 0.01,
		//.thickness = 0.07,
		.thickness = 0.01,
		.surface = outdoor_insulation.height * outdoor_insulation.length,
		.volumetric_mass = 135,
		.mass = outdoor_insulation.volumetric_mass * outdoor_insulation.height * outdoor_insulation.length * outdoor_insulation.thickness,
		.convection_coefficient_1 = 0.06,
		.convection_coefficient_2 = 0,
		.thermal_exchange_coefficient = calculate_thermal_exchange_coefficient(outdoor_insulation)
	};

	cell wall = {
		// Béton
		.temperature = 20,
		.type = WALL,
		.mass_heat_capacity = 880,
		.lambda = 1.4,
		.height = 2.5,
		.length = 0.01,
		//.thickness = 0.05,
		.thickness = 0.01,
		.surface = wall.height * wall.length,
		.volumetric_mass = 2400,
		.mass = wall.volumetric_mass * wall.surface * wall.thickness,
		.convection_coefficient_1 = 0.06,
		.convection_coefficient_2 = 0.17,
		.thermal_exchange_coefficient = calculate_thermal_exchange_coefficient(wall)
	};

	cell outside_air = {
		.temperature = 10,
		.type = OUTSIDE_AIR,
		.mass_heat_capacity = 1004,
		.lambda = 0.025,
		.height = 2.5,
		.length = 0.01,
		//.thickness = 1,
		.thickness = 0.01,
		.surface = outside_air.height * outside_air.length,
		.volumetric_mass = 1.293 * (273.15 / inner_insulation.temperature) * (101325 / 101325),
		.mass = outside_air.volumetric_mass * outside_air.height * outside_air.length * outside_air.thickness
	};

	cell inside_air = {
		.temperature = 20,
		.type = INSIDE_AIR,
		.mass_heat_capacity = 1004,
		.lambda = 0.025,
		.height = 2.5,
		.length = 0.01,
		//.thickness = 1,
		.thickness = 0.01,
		.surface = inside_air.height * inside_air.length,
		.volumetric_mass = 1.293 * (273.15 / inner_insulation.temperature) * (101325 / 101325),
		.mass = inside_air.volumetric_mass * inside_air.height * inside_air.length * inside_air.thickness
	};

	int length_outdoor_coords = 8;
	coordonates outdoor_coords_temp[8] = {
		{0, 0}, {1016, 4},
		{1012, 5}, {1016, 1016},
		{0, 5}, {4, 1016},
		{5, 1012}, {1012, 1016}
	};
	/* coordonates outdoor_coords_temp[2] = {
		{1, 1}, {1, 2}
	}; */
	coordonates* outdoor_coords = malloc(sizeof(coordonates)*length_outdoor_coords);
	for (int i=0; i<length_outdoor_coords; i++){ outdoor_coords[i] = outdoor_coords_temp[i]; }
	structure outdoor = {
		.cell_composing_structure = outside_air,
		.coord_list = 
		{
			.list = outdoor_coords,
			.length = length_outdoor_coords / 2
		}
	};

	int length_wall_coords = 8;
	coordonates wall_coords_temp[8] = {
		{5, 5}, {1011, 9},
		{1007, 10}, {1011, 1011},
		{5, 6}, {9, 1011},
		{10, 1007}, {1007, 1011}
	};
	/* coordonates wall_coords_temp[2] = {
		{2, 1}, {2, 2}
	}; */
	coordonates* wall_coords = malloc(sizeof(coordonates)*length_wall_coords);
	for (int i=0; i<length_wall_coords; i++){ wall_coords[i] = wall_coords_temp[i]; }
	structure walls = {
		.cell_composing_structure = wall,
		.coord_list = 
		{
			.list = wall_coords,
			.length = length_wall_coords / 2
		}
	};

	int length_inner_insulation_coords = 8;
	coordonates inner_insulation_coords_temp[8] = {
		{4, 4}, {4, 16},
		{5, 16}, {16, 16},
		{5, 4}, {16, 4},
		{16, 5}, {16, 15}
	};
	/* coordonates inner_insulation_coords_temp[8] = {
		{0, 0}, {4, 0},
		{4, 1}, {4, 3},
		{0, 1}, {0, 3},
		{1, 3}, {3, 3}
	}; */
	coordonates* inner_insulation_coords = malloc(sizeof(coordonates)*length_inner_insulation_coords);
	for (int i=0; i<length_inner_insulation_coords; i++){ inner_insulation_coords[i] = inner_insulation_coords_temp[i]; }
	structure inner_insulations = {
		.cell_composing_structure = inner_insulation,
		.coord_list = 
		{
			.list = inner_insulation_coords,
			.length = length_inner_insulation_coords / 2
		}
	};

	int length_inside_coords = 2;
	coordonates inside_coords_temp[2] = {
		{6, 6},
		{1010, 1010}
	};
	/* coordonates inside_coords_temp[2] = {
		{3, 1}, {3, 2}
	}; */
	coordonates* inside_coords = malloc(sizeof(coordonates)*length_inside_coords);
	for (int i=0; i<length_inside_coords; i++){ inside_coords[i] = inside_coords_temp[i]; }
	structure inside = {
		.cell_composing_structure = inside_air,
		.coord_list =
		{
			.list = inside_coords,
			.length = length_inside_coords / 2
		}
	};

	int local_nbr_structures = 3;
	structure* structures = (structure*)malloc(sizeof(structure)*local_nbr_structures);

	structures[0] = outdoor;
	structures[1] = walls;
	//structures[2] = inner_insulations;
	structures[2] = inside;
	*nbr_structures = local_nbr_structures;
	*height = 1017;
	*width = 1017;
	return structures;
}