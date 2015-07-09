/**
    spannin_tree.c
    Purpose: A simple implementation of spanning tree T of an undirected graph G 
	a subgraph that includes all of the vertices of G that is a tree. In general, 
	a graph may have several spanning trees, but a graph that is not connected 
	will not contain a spanning tree (but Spanning forests). If all of the edges
	of G are also edges of a spanning tree T of G, then G is a tree and is 
	identical to T (that is, a tree has a unique spanning tree and it is itself).
    
	--Using sparse array for registering edges--
	
    @author George Papageorgakis
    @version 1.0 11/2011
*/
#include <stdio.h>
#include <stdlib.h>
#define MAX_LENGTH 31
#define TRUE 1
#define FALSE 0

int lvl, root, nocycle = FALSE;

struct node{
	int i, j, level, father, active;
	struct node *previous, *next, *up, *down;
}*temp,V[MAX_LENGTH], H[MAX_LENGTH];


/**
    Inserts node in the horizontal indexing.	
*/
void insert_horizontal(struct node *aux, struct node *temp){
    	if (temp->j > aux->j){
		/* if next of aux is not null but there are other elements still to compare with temp
		the function takes the next structure that is being pointed by aux and the node that
		is already in temp.
		else if aux is the last element temp->previous points to aux, the NULL initialized
		*/
		if (aux->next != NULL)
			insert_horizontal(aux->next, temp);
		else{
			temp->previous	= aux;
			aux->next	= temp;
		}
	}
	/* else the temp is placed left of aux, the next of the previous struct 
	of aux must point to temp and aux->previous must show the new struct
	*/
    	else if (temp->j < aux->j){
		temp->previous	    = aux->previous;
		temp->next	    = aux;
		aux->previous->next = temp;			
		aux->previous	    = temp;
	}
}


/**
    Inserts node in the vertical indexing.	
*/
void insert_vertical(int *p_check, struct node *aux, struct node *temp){
	*p_check = 0;
	if (temp->i > aux->i){
		/* if down of aux is not null but there are other elements still to compare with temp
		the function takes the structure beneath that is being pointed by aux and the node that
		is already in temp.
		else if aux is the last element temp->up points to aux and temp->down persists as NULL 
		since its the last element
		*/
		if (aux->down != NULL)
			insert_vertical(p_check, aux->down, temp);
		else{
			temp->up  = aux;
			aux->down = temp;
		}
	}
	/* else if temp is placed above the aux, the temp->up must point to where aux
	was pointing to, temp->down must point to aux, the down of the upper structure 
	of aux must point to temp and the aux->up must point to the new struct.
	*/
	else if (temp->i < aux->i){
		temp->up	= aux->up;
		temp->down	= aux;
		aux->up->down	= temp;
		aux->up		= temp;
	}
	//avoid insertion of same element, also works as diagonal index checking
	else
        	*p_check = 1;
}


/**
    deletes a node if its safe
*/
void delete_node(struct node *aux){
	if (aux->next != NULL){
        aux->previous->next = aux->next;
        aux->next->previous = aux->previous;
    	}
	else{
		aux->previous->next =NULL;
	}
    	if (aux->down != NULL){
        aux->up->down = aux->down;
        aux->down->up = aux->up;
	}
	else{
		aux->up->down = NULL;
	}
	free(aux);
}

/**
    search for the node and delete it if it exists
*/
void search_delete(struct node *aux, int y){
	if (aux->j != y){
	  	//if not in the last element
		if ((aux->j < y) && (aux->next!=NULL))
			search_delete(aux->next, y);
		else{
			printf("Input Node does not exist.\n");fflush(stdout);
		}
	}
	//else the element exists and is to be deleted
	else{ 
		delete_node(aux);
		printf("Edge deleted.\n"); fflush(stdout);
	}
}

/**
    Print the sparse array
*/
void display(struct node *aux){
	if (aux != NULL){
		printf("(%d,%d)", aux->i, aux->j);
		display(aux->next);
	}
}

/**
    initialize the allocated indices of the matrix
*/
void initialize(void){
	int k;
	for (k=1; k < MAX_LENGTH; ++k){
		V[k].i		= k;
		V[k].j		= 0;
		V[k].level	= 0;
		V[k].active	= FALSE;
		V[k].next	= NULL;
		V[k].previous	= NULL;
		V[k].up		= NULL;
		V[k].down	= NULL;
		H[k].i		= 0;
		H[k].j		= k;
		H[k].level	= 0;
		H[k].active	= FALSE;
		H[k].next	= NULL;
		H[k].previous	= NULL;
		H[k].up		= NULL;
		H[k].down	= NULL;
	}
}


/**
    activate the symmetric edge
*/
void Activate_symmetric(struct node *aux1, int y){
	if (((aux1->j) < y) && ((aux1->next) != NULL))
		Activate_symmetric(aux1->next, y);
	else
		aux1->active = TRUE;
}


//--------------------------------------------------------------------------------------------------------------------//
/**
    Set the root and form the spanning tree
*/
void Spanning_tree(struct node *aux){
	int k, lvlprint;
	printf("Root:%d\n",root);
	for (lvl=1; lvl < MAX_LENGTH-1; ++lvl){
        	lvlprint = FALSE;
		//search the vertical array V
		for (k=1; k < MAX_LENGTH; ++k){
		    	if (V[k].level == lvl && V[k].active == FALSE){
			    	aux = &V[k];
			        while (aux->next != NULL){
			            	aux = aux->next;
					//include the node or not
			            	if (aux->active == FALSE && H[aux->j].active == FALSE){
						aux->active	 = TRUE;
						H[aux->j].active = TRUE;
						H[aux->j].father = aux->i;
						V[aux->j].level	 = lvl + 1;
						//for activation of the symmetric edge
						Activate_symmetric(&V[aux->j], aux->i);
						if (lvlprint == FALSE){
							printf("Level %d: ",lvl);
							lvlprint = TRUE;
						}
						printf("{%d}", aux->j);
						printf("(%d,%d)", aux->i, aux->j);
						printf("/(%d,%d),\t", aux->j, aux->i);
			            	}
			        }
			        V[k].active = TRUE;	//to avoid checking on the same row
			}
		}
		if (lvlprint == TRUE)
			printf("\n");
	}
	printf("\n");
}

/**
    Find cycles in the spanning tree
*/
//x=i and y=j of the current vertex
void find_cycle(int x, int y){
	int g = 0, m = 0, e = 0, spot = FALSE;
	int A[MAX_LENGTH-1], B[MAX_LENGTH-1];
	//initialize A, B
	for (m=0; m < MAX_LENGTH-1; ++m){
		if (A[m] != 0)
			A[m] = 0;
		if (B[m] != 0)
			B[m] = 0;
	}
	//fill A with the nodes while ascenting to the root
	m = 0;
	while (H[x].j != root){		// && (H[x].father!=root))
		A[m] = H[x].j;			
		x    = H[x].father;
		++m;
	}
	if (H[x].j == root)
		A[m] = root;
	
	//fill A with the nodes while ascenting to the root
	e = 0;
	while (H[y].j != root){
		B[e] = H[y].j;
		y    = H[y].father;
		++e;
	}
	if (H[y].j == root)
		B[e] = root;

	for (m=0;((m < MAX_LENGTH-1) && (A[m] != 0) && (spot == FALSE)); ++m){
		for (e=0; ((e < MAX_LENGTH-1) && (B[e] != 0) && (spot == FALSE)); ++e){
			if (A[m] == B[e]){
				spot	= TRUE;
				nocycle	= TRUE;
			}
		}
	}
	printf(" Cycle{");
	for (g=0; g < e; ++g)
		printf("%d,", B[g]);
	for (m=m-2; m >= 0; --m)
		printf("%d,", A[m]);
	printf("}\n");
}


void ST_cycles(struct node *aux){
	while (aux->next != NULL){
		aux = aux->next;
		//a cycle for all the inactive
		if (aux->active == FALSE){
			printf("(%d,%d):", aux->i, aux->j);
			find_cycle(aux->i, aux->j);
		}
	}
}


//--------------------------------------------------------------------------------------------------------------------//
int main (void){
	int k, x, y, check = 0, *p_check = &check;
	initialize();
    	char ch = '*';
	while (ch!='0'){
		fflush(stdin);
		x=0; y=0;
		printf("1.Insert Edges\t2.Print Edges\t3.Delete Edges\n4.Spanning Tree\t5.ST Cycles\t0.Quit\n");fflush(stdout);
		ch = getchar();
		if (ch == '1'){
			if ((temp = (struct node *) malloc(sizeof(struct node))) != NULL){
				temp->previous	= NULL;
				temp->next	= NULL;
				temp->up	= NULL;
				temp->down	= NULL;
				temp->active	= FALSE;
				temp->father	= 0;
				temp->level	= 0;
				temp->i		= 0;
				temp->j		= 0;
				printf("Input node(1-%d) i:\n", MAX_LENGTH-1);
				scanf("%d", &temp->i);
				printf("node j:\n");
				scanf("%d", &temp->j);
			        if (temp->i>0 && temp->j>0 && temp->i < MAX_LENGTH && temp->j < MAX_LENGTH){
			            	if (temp->i != temp->j){
						insert_horizontal(&V[temp->i],temp);   //start from row i
						insert_vertical(p_check, &H[temp->j], temp);
						x = temp->i;
						y = temp->j;
						printf("New default root: %d\n", x);
						root = x;
						V[x].level  = 1;
						H[x].father = x;
						H[x].active = TRUE;
						Spanning_tree(&V[x]);
					}
					else
						printf("Self cycle Edge not inserted\n");
	        		}
	        		else
	               			printf("Edge cordinates out of range\n");
			}
			else{
				printf("Memory is Full");
			}
			
			if (x != y){
				//forbids double insertion of diagonal nodes
				if (check == 0){
					if ((temp = (struct node *) malloc(sizeof(struct node))) != NULL){
						temp->previous	= NULL;
						temp->next	= NULL;
						temp->up	= NULL;
						temp->down	= NULL;
						temp->active	= FALSE;
						temp->father	= 0;
						temp->level	= 0;
						temp->i		= 0;
						temp->j		= 0;
						temp->i		= y;
						temp->j		= x;
						insert_horizontal(&V[temp->i], temp);
						insert_vertical(p_check, &H[temp->j], temp);
					}
					else 
						printf("Memory is Full");
				}
				else
					printf("Edge already registered\n");
			}
	
		}
		else if (ch == '2'){
	    		fflush(stdout);
			printf("______________________________________________________________________________\n\n");
			for (k=1; k < MAX_LENGTH; ++k){
				display(V[k].next);
				if (V[k].next != NULL)
					printf("\n");
			}
			printf("_______________________________________________________________________________\n");
		}
		else if (ch == '3'){
			printf("Which edge do you want to delete?\ni="); fflush(stdout);
			scanf("%d", &x);
			printf("j="); fflush(stdout);
			scanf("%d", &y);
			search_delete(&V[x], y);
			search_delete(&V[y], x);
		}
		else if (ch == '4'){
			printf("Insert Root Node:\n"); fflush(stdout);
			scanf("%d", &root);
			V[root].level  = 1;
			H[root].father = root;
			H[root].active = TRUE;
			Spanning_tree(&V[root]);
		}
		else if (ch == '5'){
			for (k=1; k < MAX_LENGTH; ++k)
				ST_cycles(&V[k]);
			if (nocycle == FALSE)
				printf("Tree has no cycles.\n");
		}
	}
    return 0;
}
