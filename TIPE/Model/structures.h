/* Liste chainee */

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

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Table de hachage */

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

/*--------------------------------------------------------------------------------------------------------------------------------------------------------------------*/

/* Structures particulière au modèle */

struct coordonates {
	int x;
	int y;
};

typedef struct coordonates coordonates;

// Liste de coordonnées

struct coordonates_list {
	coordonates* list;
	int length;
};

typedef struct coordonates_list coordonates_list;

// Cellule représentant une case de la carte

enum cell_type { OUTDOOR_INSULATION, INNER_INSULATION,  WALL, OUTSIDE_AIR, INSIDE_AIR };

struct cell
{
    float temperature; // ----------------- Température en degré Celsius
    enum cell_type type; // --------------- Type de la cellule (airExt, airInt, wall, window, door)
	coordonates coords; // ---------------- Coordonnées de la cellule
	float mass_heat_capacity; // ---------- Capacité thermique massique en J.K-1.kg
	float lambda; // ---------------------- Conductivité thermique en W.K-1.m-1
	float height; // ---------------------- Hauteur en m
	float length; // ---------------------- Longueur en m
	float thickness; // ------------------- Epaisseur en m
	float surface; // --------------------- Surface en m2
	float volumetric_mass; // ------------- Masse volumique en kg.m-3
	float mass; // ------------------------ Masse en kg
};

typedef struct cell cell;

// Structure représentant une structure (un ensemble de cellule ayant les mêmes propriétés) de la maison

typedef struct structure
{
	cell cell_composing_structure;
	coordonates_list coord_list;
} structure;

structure* initialize_structure(int* nbr_structures, int* height, int* width); // Return a list of structures composing the house