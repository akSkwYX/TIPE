#include "listeChainee.h"

struct dict {
  int m; //nombre de seaux de la table de hachage
  int taille; //nombre de couples clè-valeur dans la table
  ListeChainee* seaux; //tableaux de taille m contenant des liste chainée (les seaux)
};
typedef struct dict dict;


//Opérations générales sur les dictionnaires
dict* THcreer(int m); //crée un dictionnaire vide contenant m seaux
void THajouter(dict* d, char* c, int v); //ajoute le couple clé-valeur au dictionnaire
int THobtenir(dict d, char* c); //renvoie la valeur associée à la clé d (ou -1 si la clé n'existe pas)
bool THestCle(dict d, char* c); //renvoie vrai ssi c est une clé du dictionnaire
void THsupprimer(dict* d, char* c); //supprime le couple clé-valeur dont la clé est c
void THliberer(dict* d); //libere l'espace mémoire utilisé par le dictionnaire

//Opérations spécifiques aux tables de hachage
void THaffiche(dict* d); //affiche le contenue de chaque seau
