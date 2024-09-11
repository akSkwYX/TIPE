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

structure* initialize_structure(int* nbr_structures, int* height, int* width){
	
	int local_nbr_structures = 3;
	structure* structures = (structure*)malloc(sizeof(structure)*local_nbr_structures);

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

	coordonates outdoor_coords_temp[8] = {
		{0, 0}, {20, 2},
		{18, 3}, {20, 20},
		{0, 3}, {2, 20},
		{3, 18}, {17, 20}
	};
	coordonates* outdoor_coords = malloc(sizeof(coordonates)*8);
	for (int i=0; i<8; i++){ outdoor_coords[i] = outdoor_coords_temp[i]; }
	structure outdoor = {
		.cell_composing_structure = outside_air,
		.coord_list = 
		{
			.list = outdoor_coords,
			.length = 4
		}
	};

	coordonates wall_coords_temp[8] = {
		{3, 3}, {3, 17},
		{4, 17}, {17, 17},
		{4, 3}, {17, 3},
		{17, 4}, {17, 16}
	};
	coordonates* wall_coords = malloc(sizeof(coordonates)*8);
	for (int i=0; i<8; i++){ wall_coords[i] = wall_coords_temp[i]; }
	structure walls = {
		.cell_composing_structure = wall,
		.coord_list = 
		{
			.list = wall_coords,
			.length = 4
		}
	};

	coordonates inside_coords_temp[2] = {
		{4, 4},
		{16, 16}
	};
	coordonates* inside_coords = malloc(sizeof(coordonates)*2);
	for (int i=0; i<2; i++){ inside_coords[i] = inside_coords_temp[i]; }
	structure inside = {
		.cell_composing_structure = inside_air,
		.coord_list =
		{
			.list = inside_coords,
			.length = 1
		}
	};

	structures[0] = outdoor;
	structures[1] = walls;
	structures[2] = inside;
	*nbr_structures = local_nbr_structures;
	*height = 21;
	*width = 21;
	return structures;
}