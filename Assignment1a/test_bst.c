#include "bst.h"
#include <stdio.h>
#include <stdlib.h>
int main()
{
	Node *root=NULL;
	
   	root=insertNode(root, 14);
	root=insertNode(root, 12);
	root=insertNode(root, 6);
	root=insertNode(root, 17);
	root=insertNode(root, 8);
   	printSubtree(root);
	printf("\n");
	root=deleteNode(root,14);
	root=deleteNode(root,8);
   	printSubtree(root);
	root=freeSubtree(root);
	printf("Number of nodes=%d\n", countNodes(root));

	return 0;
}
