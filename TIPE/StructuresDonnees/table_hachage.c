#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include "table_hachage.h"

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
	if (obtenir(d, c) != -1){
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

int main(){
	dict* d = creer(10);
	ajouter(d, "a", 1);
	ajouter(d, "b", 2);
	ajouter(d, "c", 3);
	ajouter(d, "d", 4);
	ajouter(d, "e", 5);
	ajouter(d, "f", 6);
	ajouter(d, "g", 7);
	ajouter(d, "h", 8);
	ajouter(d, "i", 9);
	ajouter(d, "j", 10);
	affiche_table_hachage(d);
	supprimer(d, "e");
	affiche_table_hachage(d);
	THliberer(d);
}
