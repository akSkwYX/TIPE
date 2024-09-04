struct link {
	char* cle; // Clé
	int value; // Valeur
	struct link* next;
};
typedef struct link Link;
typedef Link* ListeChainee;

ListeChainee creerListeChainee(); // Crée une liste chaînée vide
bool LCestVide(ListeChainee liste); // Renvoie vrai si la liste chaînée est vide, faux sinon
int LCobtenirElement(ListeChainee liste, char* cle); // Renvoie la valeur associée à la clé cle dans la liste chaînée
ListeChainee LCajouterElement(ListeChainee liste, char* cle, int valeur); // Ajoute un élément en tête de la liste chaînée
ListeChainee LCsupprimerElement(ListeChainee liste, char* cle); // Supprime la première occurrence de valeur dans la liste chaînée
void LCliberer(ListeChainee liste); // Libère l'espace mémoire alloué à la liste chaînée
void afficherListeChainee(ListeChainee liste); // Affiche les éléments de la liste chaînée