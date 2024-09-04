#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "listeChainee.h"

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